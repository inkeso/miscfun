## based on https://github.com/gaborcsardi/prompt/ ##

.prompt_env <- new.env()

.prompt_env$error_hook <- function() {
    .prompt_env$update(expr = NA, value = NA, ok = FALSE, visible = NA)
    #orig <- .prompt_env$error
    #if (!is.null(orig) && is.function(orig)) orig()
    #if (!is.null(orig) && is.call(orig)) eval(orig)
}

.prompt_env$callback <- function(expr, value, ok, visible) {
    try(suppressWarnings(.prompt_env$update(expr, value, ok, visible)))
    TRUE
}

.prompt_env$update <- function(...) {
    mine <- .prompt_env$prompt
    if (is.function(mine)) mine <- mine(...)
    if (is.character(mine)) options(prompt = mine)
}

.cc <- function(...) paste(c(...),collapse="")

.prompt_env$getmem <- function() {
    obj <- ls(envir=globalenv())
    pstat <- readLines(sprintf("/proc/%s/status", Sys.getpid()))# .prompt_env$pid))
    mem <- as.numeric(sub(".*:\t([ 0-9]+) kB", "\\1", grep("VmSize", pstat, v=T)))  # Total
    sprintf("\033[33m%d Objects\033[90m   \033[96m%.1f MB", length(obj), mem/1024)
}

.prompt_env$boxen_topf <- function() {
    stuff <- .prompt_env$getmem()
    morestuff <- capture.output(lf(datafirst=.prompt_env$datafirst))
    if (length(morestuff) > 0) stuff <- c(stuff, "", sub(".*▔(▔+).*", "",morestuff))
    # OK, we got stuff to display. Now calculate its dimensions
    stuffdim <- c(max(nchar(gsub("\033\\[.*?m", "",stuff)))+2, length(stuff))
    
    # TODO: merge (like bello) to only draw once.
    # also, limit height if neccessary
    # draw a box of sufficient size and render content
    ansibox <- .cc("\033[48;5;17;36m┌", rep("─",stuffdim[1]), "┐")
    ansibox <- c(ansibox, rep(.cc("│", rep(" ",stuffdim[1]), "│"), stuffdim[2]))
    ansibox <- c(ansibox, .cc("└", rep("─",stuffdim[1]), "┘"))
    # and replace horizontal rules
    stuff[stuff==""] <- .cc("\033[36m\033[1D", rep("·",stuffdim[1]))
    # finally output
    # store cursor position
    cat("\0337")
    xpos <- getOption("width") - stuffdim[1] - 1
    for (i in seq_along(ansibox)) cat(sprintf("\033[%d;%dH%s", i+1, xpos, ansibox[i]))
    for (i in seq_along(stuff)) cat(sprintf("\033[%d;%dH%s", i+2, xpos+2, stuff[i]))
    # restore cursor position
    cat("\033[m\0338")
    invisible(stuffdim)
}

.prompt_env$boxen_file <- function(tofile) {
    stuff <- c(.prompt_env$getmem(),"", capture.output(lf(datafirst=.prompt_env$datafirst)))
    stuffdim <- c(max(nchar(gsub("\033\\[.*?m", "",stuff)))+2, length(stuff))
    cat(stuff, sep="\n", file=tofile)
    invisible(stuffdim)
}


.prompt_env$boxen_bello <- function() {
    stuff <- .prompt_env$getmem()
    morestuff <- capture.output(lf(concise=TRUE, datafirst=.prompt_env$datafirst))
    if (length(morestuff) > 0) stuff <- c(stuff, "", morestuff)
    # OK, we got stuff to display. Now calculate its dimensions
    stuffdim <- c(max(nchar(gsub("\033\\[.*?m", "",stuff)))+1, length(stuff))
    #stuffdim[1] <- min(stuffdim[1], .prompt_env$maxwidth)
    stuffdim[2] <- min(stuffdim[2], .prompt_env$maxheight)
    xpos <- getOption("width") - stuffdim[1]
    # padd/trim stuff (ignoring colors)
    stuff <- sapply(stuff, function(.) {
        asc <- gsub("\033\\[.*?m", "", .)
        if (asc == "") {
            r <- .cc("\033[36m", rep("·", stuffdim[1]))
        } else if (stuffdim[1] > nchar(asc)) {
            r <- .cc(" ", ., .cc(rep(" ", stuffdim[1] - nchar(asc)-1)))
        } else {
            # color-aware substr to limit width?
            r <- .cc(" ", .)
        }
        sprintf("\r\033[%dC\033[48;5;17;36m│%s\033[m ", xpos-1, r)
    }, USE.NAMES = FALSE)
    cat(sprintf("\r\033[%dC %s \n", xpos-1, .cc(rep(" ", stuffdim[1]))))
    cat(sprintf("\r\033[%dC\033[48;5;17;36m┌%s\033[m \n", xpos-1, .cc(rep("┈", stuffdim[1]))))
    cat(.cc(stuff[1:stuffdim[2]]))
    if(stuffdim[2] < .prompt_env$maxheight) {
        cat(sprintf("\r\033[%dC\033[48;5;17;36m└%s\033[m ", xpos-1, .cc(rep("┈", stuffdim[1]))))
        stuffdim[2] <- stuffdim[2] + 1
    }
    cat(sprintf("\033[%dA\r", stuffdim[2]+3))
    invisible(stuffdim)
}

.prompt_env$prompt <- function(expr, value, ok, visible) {
    prwidth <- system("tput cols", intern=T)
    prwidth <- if(nzchar(prwidth)) as.integer(prwidth) else 80

    # maybe update this on each call? because resize?
    rows <- system("tput lines", intern=T)
    rows <- if(nzchar(rows)) as.integer(rows) else 25
    .prompt_env$maxheight <- floor(rows / ifelse(.prompt_env$below, 2.3, 1))
    .prompt_env$maxwidth <- floor(prwidth / 3)

    diff <- proc.time() - .prompt_env$last
    val <- diff["elapsed"]
    assign("last", proc.time(), envir=.prompt_env)
    
    promptfile <- commandArgs(T)
    
    if (length(promptfile) > 0) {
        envbox <- .prompt_env$boxen_file(promptfile)
        envansi <- ''
    } else {
        options(width=prwidth)
        # box containing environment is completely part of the prompt.
        if (is.na(.prompt_env$below)) {
            envansi <- ''
        } else {
            if (.prompt_env$below) {
                envansi <- sprintf("\n%s",.cc(capture.output(envbox <- .prompt_env$boxen_bello())))
            } else {
                envansi <- .cc(capture.output(envbox <- .prompt_env$boxen_topf()))
            }
            options(width=prwidth - envbox[1] - 1)
        }
    }
    
    # try calculating execution time.
    if (!is.na(val) && val > 1) {
        res <- sprintf("%02.0f:%02.0f:%02.0f", (val %/% 3600) %% 24, (val %/% 60) %% 60, val %% 60)
        if (val > 86400) res <- sprintf("%.0f days, %s", val %/% 86400, res)
    } else {
        res <- ""
    }
    status <- if (ok) "\033[38;5;28mOK " else "\033[91mERR"
    if (startsWith(getwd(), Sys.getenv("HOME"))) {
        pwd <- sub(Sys.getenv("HOME"), "~", getwd(), fixed=T)
    } else {
        pwd <- getwd()
    }
    # if (nchar(pwd) > (prwidth / 2)) {
    #     pwd <- sub("^[^/]*", "…", substr(pwd, nchar(pwd)-(prwidth / 2)-2, nchar(pwd)))
    # }
    pwd_comp <- unlist(strsplit(pwd, "/"))
    if (length(pwd_comp) > 4) {
        pwd <- paste(c(pwd_comp[1], "…", rev(pwd_comp)[3:1]), collapse="/")
    }
    #prlen <- nchar(sprintf("--OK -%s-[%s]-[%d / %.1f MB]", res, pwd, obj, mem/1024))
    prlen <- nchar(sprintf("--OK -%s-[%s]", res, pwd))
    fillr <- if (prlen > 0) paste(rep("┈", prwidth - prlen), collapse="") else ""

    system(sprintf('[ -n "$TMUX" ] && tmux rename-window -t${TMUX_PANE} "R: %s"', pwd))

    row1 <- sprintf(
        #"\033[90m┌─%s\033[90m─\033[38;5;31m%s\033[90m─%s[\033[35m%s\033[90m]─[\033[33m%d\033[90m / \033[96m%.1f MB\033[90m]",
        "\033[90m┌─%s\033[90m─\033[38;5;31m%s\033[90m─%s[\033[35m%s\033[90m]",
        status, res, fillr, pwd)
    paste0(row1, envansi, "\033[0m\n▶ ")
    #paste0(row1, "\033[0m\n▶ ")
}

.prompt_env$task_id <- addTaskCallback(.prompt_env$callback)
.prompt_env$error <- getOption("error")
.prompt_env$last <- proc.time()
.prompt_env$last[] <- NA_real_
#.prompt_env$pid <- Sys.getpid()

# For displaying global environment in a box. 
# If --args <filename> is set, it will be written to file (for displaying it externally).
# Otherwise:

.prompt_env$below <- NA	    # Not shown
#.prompt_env$below <- TRUE   # show below prompt
#.prompt_env$below <- FALSE  # show on top right

# data, then functions when TRUE, functions then data when FALSE
.prompt_env$datafirst <- TRUE

options(prompt="▶ ", continue="▏ ", error=.prompt_env$error_hook)
.prompt_env$update(NULL, NULL, TRUE, FALSE)

