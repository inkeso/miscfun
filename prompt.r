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

.prompt_env$boxen <- function() {
    # well it DOES work and looks somewhat OK. But:
    # - pollutes scrolling
    # - reducing box size (by removing objects) is extremely ugly.
    # - when cursor is in top few rows, the wide cursor draws over the box
    # we can only work around those problems with a radically different approach.
    # I have no idea.

    # first, show memusage
    obj <- ls(envir=globalenv())
    pstat <- readLines(sprintf("/proc/%s/status", Sys.getpid()))# .prompt_env$pid))
    mem <- as.numeric(sub(".*:\t([ 0-9]+) kB", "\\1", grep("VmSize", pstat, v=T)))  # Total
    stuff <- sprintf("\033[33m%d Objects\033[90m   \033[96m%.1f MB", length(obj), mem/1024)
    morestuff <- capture.output(lf())
    if (length(morestuff) > 0) stuff <- c(stuff, "", sub(".*▔(▔+).*", "",morestuff))
    # OK, we got stuff to display. Now calculate its dimensions
    stuffdim <- c(max(nchar(gsub("\033\\[.*?m", "",stuff)))+2, length(stuff))
    # draw a box of sufficient size and render content
    ansibox <- .cc("\033[48;5;17;36m┌", rep("─",stuffdim[1]), "┐")
    ansibox <- c(ansibox, rep(.cc("│", rep(" ",stuffdim[1]), "│"), stuffdim[2]))
    ansibox <- c(ansibox, .cc("└", rep("─",stuffdim[1]), "┘"))
    # and replace horizontal rules
    stuff[stuff==""] <- .cc("\033[36m\033[1D", rep("·",stuffdim[1]))
    # finally output
    # store cursor position, send cursor to top and right, draw box, reposition cursor inside box
    cat("\0337\033[2H", ansibox, "\033[2H", sep=sprintf("\n\033[%dC", getOption("width")-stuffdim[1]-2))
    # draw stuff (inset), restore cursor position
    cat("", stuff, "\033[m\0338", sep=sprintf("\n\033[%dC", getOption("width")-stuffdim[1]))
    return(stuffdim)
}

.prompt_env$prompt <- function(expr, value, ok, visible) {
    diff <- proc.time() - .prompt_env$last
    val <- diff["elapsed"]
    assign("last", proc.time(), envir=.prompt_env)

    # this should ideally be done after the prompt is shown to avoid scrolling it out of view.
    # Don't now whether its possible at all.
    envbox <- .prompt_env$boxen()
    # reduce the prompt width? (looks weird)
    #prwidth <- getOption("width") - envbox[1] - 2
    prwidth <- getOption("width")

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
    paste0(row1, "\033[0m\n▶ ")
}

.prompt_env$task_id <- addTaskCallback(.prompt_env$callback)
.prompt_env$error <- getOption("error")
.prompt_env$last <- proc.time()
.prompt_env$last[] <- NA_real_
#.prompt_env$pid <- Sys.getpid()

options(prompt="▶ ", continue="▏ ", error=.prompt_env$error_hook)
.prompt_env$update(NULL, NULL, TRUE, FALSE)
