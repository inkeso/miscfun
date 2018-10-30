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


.prompt_env$prompt <- function(expr, value, ok, visible) {
    diff <- proc.time() - .prompt_env$last
    val <- diff["elapsed"]
    assign("last", proc.time(), envir=.prompt_env)
    
    pstat <- readLines(sprintf("/proc/%s/status", Sys.getpid()))# .prompt_env$pid))
    mem <- as.numeric(sub(".*:\t([ 0-9]+) kB", "\\1", grep("VmRSS", pstat, v=T)))
    
    if (!is.na(val) && val > 1) {
        res <- sprintf("%02.0f:%02.0f:%02.0f", (val %/% 3600) %% 24, (val %/% 60) %% 60, val %% 60)
        if (val > 86400) res <- sprintf("%.0f days, %s", val %/% 86400, res)
    } else {
        res <- ""
    }
    obj <- length(ls(envir=.GlobalEnv))
    status <- if (ok) "\033[38;5;28mOK " else "\033[91mERR"
    if (startsWith(getwd(), Sys.getenv("HOME"))) {
        pwd <- sub(Sys.getenv("HOME"), "~", getwd(), fixed=T)
    } else {
        pwd <- getwd()
    }
    if (nchar(pwd) > (getOption("width") / 2)) {
        pwd <- sub("^[^/]*", "…", substr(pwd, nchar(pwd)-(getOption("width") / 2)-2, nchar(pwd)))
    }
    prlen <- nchar(sprintf("--OK -%s-[%s]-[%d / %.1f MB]", res, pwd, obj, mem/1024))
    fillr <- if (prlen > 0) paste(rep("┈", getOption("width") - prlen), collapse="") else ""
    row1 <- sprintf(
        "\033[90m┌─%s\033[90m─\033[38;5;31m%s\033[90m─%s[\033[35m%s\033[90m]─[\033[33m%d\033[90m / \033[96m%.1f MB\033[90m]",
        status, res, fillr, pwd, obj, mem/1024)
    system(sprintf('[[ -n "$TMUX" ]] && tmux rename-window -t${TMUX_PANE} "R: %s"', pwd))
    paste0(row1, "\033[0m\n▶ ")
}

.prompt_env$task_id <- addTaskCallback(.prompt_env$callback)
.prompt_env$error <- NULL
.prompt_env$last <- proc.time()
.prompt_env$last[] <- NA_real_
#.prompt_env$pid <- Sys.getpid()

if (interactive()) {
    assign("error", getOption("error"), envir=.prompt_env)
    options(prompt="▶ ", continue="▏ ", error=.prompt_env$error_hook)
    assign("prompt", .prompt_env$prompt, envir=.prompt_env)
    .prompt_env$update(NULL, NULL, TRUE, FALSE)
}
