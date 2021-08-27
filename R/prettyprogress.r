#!/bin/Rscript

# Tools
.termWidth <- function(){
    twidth <- 80 # default
    try(twidth <- as.numeric(system("tput cols",T)))
    return(twidth)
}

.ctDiff <- function(val) {
    #res <- strftime(as.POSIXlt(val, origin="1970/01/01",tz="GMT"),"%H:%M:%S")
    #res[val > 24*60*60] <- strftime(as.POSIXlt(val[val > 24*60*60], origin="1970/01/01",tz="GMT"),"%d days, %H:%M:%S")
    val <- round(val)
    val[!is.finite(val)] <- 0
    res <- sprintf("%02d:%02d:%02d", (val %/% 3600) %% 24, (val %/% 60) %% 60, val %% 60)
    if (any(val > 86400)) res <- sprintf("%d days, %s", val %/% 86400, res)
    return(res)
}

.ctStat <- function(starttime, curr, maxv, info) {
    dpad <- nchar(sprintf("%0.0f", ceiling(max(maxv))))
    eta <- as.numeric(Sys.time()) - starttime
    eta[curr<maxv] <- (eta * (maxv - curr) / curr + .5)[curr<maxv]
    timestr <- ifelse(curr<maxv, "Time left:"," DONE IN: ")
    sprintf(paste(" %5.1f%% (%",dpad,".0f/%",dpad,".0f) [%s %s] %s", sep=""),
           (curr/maxv*100), floor(curr), maxv, timestr, .ctDiff(eta), info)
}

.getPlotFun <- function(type="txt") {
    themes <- list(
        # Terminal (ANSI) TODO: Was is mit Zeilenumbrüchen?!
        "txt"=list(
            init = function(maxval, width=80) {
                pstr <- paste(sprintf(" %5.1f%% (%d/%d) [Time left: ???]", 0, 0, maxval), collapse=" | ")
                cat(substr(pstr,0,width-1),"\033[K\r")
            },
            update = function(starttime, current, maxval, width=80, info="") {
                pstr <- paste(.ctStat(starttime, current, maxval, info), collapse=" | ")
                cat(substr(pstr,0,width-1),"\033[K\r")
            },
            finish = function(starttime) {
                #tota=(as.numeric(Sys.time()) - min(starttime))
                #cat(sprintf("DONE IN: %s \033[K\n", .ctDiff(tota)))
                cat("\n\033[K")
            }
        ),
        "simple"=list(
            init = function(maxval, width=80) {
                cstat <- .ctStat(as.numeric(Sys.time()), 0, maxval, "")
                cat(cstat, sep="\n")
            },
            update = function(starttime, current, maxval, width=80, info="") {
                fillchar <- "#" # "█"
                leftchar <- "-" # "░"
                cstat <- .ctStat(starttime, current, maxval, info)
                cat(sprintf("\033[%dA", length(current))) # geh hoch
                twi <- width - max(nchar(cstat)) - 4
                if (twi > 2) {
                    fill <- sapply(floor(current/maxval*twi), function(x) paste(rep(fillchar, x), collapse=""))
                    left <- sapply(ceiling((1-current/maxval)*twi), function(x) paste(rep(leftchar, x), collapse=""))
                    cat(sprintf(paste("%-",max(nchar(cstat)),"s [%s%s]", sep=""), cstat, fill, left), sep="\n")
                } else {
                    cat(substr(sprintf(paste("%-",max(nchar(cstat)),"s", sep=""), cstat), 0, width-1), sep="\n")
                }
            },
            finish = function(starttime) {}
        ),
        "small"=list(
            init = function(maxval, width=80) {
                cstat <- .ctStat(as.numeric(Sys.time()), 0, maxval, "")
                cat(cstat, sep="\n")
            },
            update = function(starttime, current, maxval, width=80, info="") {
                ppbc <- c("▏","▎","▍","▌","▋","▊","▉","█")
                colrs <- rep(c(38, 28, 130, 88, 92, 21), length.out=length(current)) # xterm256
                colbg <- rep(c(236, 237), length.out=length(current)) # xterm256
                ppbl <- length(ppbc)
                cstat <- strtrim(.ctStat(starttime, current, maxval, info), width - 3)
                twi <- width - max(nchar(cstat)) - 3
                perc <- (current * ppbl * twi) %/% maxval
                fcstat <- sub("(^.+% )" ,"\033[1;97m\\1\033[m", cstat, perl=T)
                fcstat <- sub("\\[(Time.+)\\]" ,"[\033[93m\\1\033[m]", fcstat, perl=T)
                fcstat <- sub("\\[( DONE.+)\\]" ,"[\033[92m\\1\033[m]", fcstat, perl=T)
                fcstat <- paste(fcstat, sapply(max(nchar(cstat))-nchar(cstat), function(x) paste(rep(" ",x), collapse="")), sep="")
                dash <- abs(twi - perc %/% ppbl - 1)
                cat(sprintf("\033[%dA", length(current))) # geh hoch
                if (twi > 2) {
                    v1 <- sapply(perc %/% ppbl, function(x) paste(rep(ppbc[ppbl], x), collapse=""))
                    v2 <- ifelse(maxval > current, paste(ppbc[(perc %% ppbl) + 1], sapply(dash, function(x) paste(rep(" ", x), collapse="")), sep=""), "")
                    cat(sprintf("%s\033[38;5;%dm▕\033[48;5;%dm%s%s\033[0;38;5;%dm▏\033[0m", fcstat, colrs, colbg, v1, v2, colrs), sep="\n")
                } else {
                    cat(substr(cstat, 0, width - 1), sep="\n")
                }
            },
            finish = function(starttime) {}
        ),
        "pretty"=list(
            init = function(maxval, width=80) {
                cstat <- .ctStat(as.numeric(Sys.time()), 0, maxval, "")
                for (mv in 1:length(maxval)) {
                    cat (" \033[38;5;242m", rep("▁", width-3), "\n", sep="")
                    cat ("▕", rep(" ", width-3), "\033[38;5;248m▏\n", sep="")
                    cat (" ", rep("▔", width-3), "\033[m\n", sep="")
                    cat (cstat[mv], "\n")
                }
            },
            update = function(starttime, current, maxval, width=80, info="") {
                ppbc <- c("▏","▎","▍","▌","▋","▊","▉","█")
                ppbl <- length(ppbc)
                steps <- ppbl * (width - 3)
                perc <- (current * steps) %/% maxval
                blen <- perc %/% ppbl
                cstat <- strtrim(.ctStat(starttime, current, maxval, info), width - 3)
                cat(sprintf("\033[%dA", length(current) * 4)) # geh hoch
                v1 <- sapply(blen, function(x) paste(rep(ppbc[ppbl], x), collapse=""))
                v2 <- ifelse(maxval > current, paste(ppbc[(perc %% ppbl) + 1], sapply(abs(width - 4 - blen), function(x) paste(rep(" ", x), collapse="")), sep=""), "")
                cat(paste("\033[1B\033[1C\033[m\033[38;5;25m", v1, v2, "\033[m\n\n", cstat, "\033[K\n", sep=""), sep="")
            },
            finish = function(starttime) {
                cat ("\033[32m ALL DONE IN: ", .ctDiff(as.numeric(Sys.time()) - min(starttime)),"\033[m\033[K\n\n",sep="")
            }
        ),
        # Graphical
        "plot"=list(
            init = function(maxval, width=NULL) {
                x11(width=(width/24), height=.7*length(maxval), title="Progress", type="nbcairo")
                par(bg="#333333", fg="#afafaf", mar=c(0.5,0.5,0.5,0.5))
                barplot(rep(0, length(maxval)), xlim=c(0, 100), horiz=T, axes=F)
            },
            update = function(starttime, current, maxval, width=NULL, info="") {
                prz <- current / maxval * 100
                stats <- .ctStat(starttime, current, maxval, "")
                barplot(rev(prz), xlim=c(0,100), horiz=T, axes=F, col="#2769BE")
                text(50, 1:length(current)*1.2-.25, rev(info), font=2, col="white") #bold
                text(50, 1:length(current)*1.2-.7, rev(stats), col="white")
            },
            finish = function(starttime) {
                cat("DONE IN:", .ctDiff(as.numeric(Sys.time()) - min(starttime)), "\n")
                dev.off()
            }
        ),
        "tk"=list(
            init = function(maxval, width=100) {
                require(tcltk)
                BG <- '#333333'
                FG <- '#dadada'
                tcl("ttk::style", "configure", "TProgressbar", background='#df6300', troughcolor='#555555', borderwidth=0)
                # TODO: it's a bit rude to litter GlobalEnv.
                # also independant concurrent progressbars are not possible this way
                .tkprogress <<- list(tkwin=tktoplevel(background=BG), killed=F)
                tkwm.geometry(.tkprogress$tkwin, sprintf("%dx%d", width*4, length(maxval)*64))
                tkwm.title(.tkprogress$tkwin, "Progress")
                tcl("wm", "attributes", .tkprogress$tkwin, topmost=T, type=T)

                fn <- tkfont.create(family = "helvetica", size = 10)
                .tkprogress[["tklab"]] <<- lapply(maxval, function(x) {
                    tklabel(.tkprogress$tkwin, text="", font=fn, pady=4, background=BG, foreground=FG)
                })
                .tkprogress[["tkval"]] <<- lapply(maxval, function(x) tclVar(0)) # enum?
                
                
                for (i in 1:length(maxval)) {
                    tkpack(tklabel(.tkprogress$tkwin, background=BG), side="top")
                    tkpack(ttkprogressbar(.tkprogress$tkwin, length=width*4-20, variable=.tkprogress$tkval[[i]]), side="top")
                    tkpack(.tkprogress$tklab[[i]], side="top")
                }
                kill <- function() {
                    if (!.tkprogress$killed) {
                        tkdestroy(.tkprogress$tkwin)
                        .tkprogress$killed <<- T
                    }
                }
                tkbind(.tkprogress$tkwin, "<Destroy>", kill)
            },
            update = function(starttime, current, maxval, width=NULL, info="") {
                if (!.tkprogress$killed) {
                    vals <- current / maxval * 100
                    stat <- .ctStat(starttime, current, maxval, info)
                    for (i in 1:length(current)) {
                        tclvalue(.tkprogress$tkval[[i]]) <- vals[i]
                        tkconfigure(.tkprogress$tklab[[i]], text=stat[i])
                    }
                } else {
                    stop("ABORTED")
                }
            },
            finish = function(starttime) {
                tkdestroy(.tkprogress$tkwin)
                cat("DONE IN:", .ctDiff(as.numeric(Sys.time()) - min(starttime)), "\n")
            }
        )
    )
    return(themes[[match.arg(type, names(themes))]])
}

#' ppbar
#'
#' Create one or many progressbar(s)
#' @param maxval
#'   a vector of max values for progressbars. number of items = number of progressbars.
#' @param width
#'   width of prgressbar(s). Default is terminal-width as determined by tput
#' @param type
#'   one of 'txt', 'simple', 'small', 'pretty', 'plot', 'tk'
#'
#' @export ppbar
#' @exportClass ppbar
ppbar <- setRefClass("ppbar",
    fields=c("maxval", "width", "type", "starttime", "current", "info", "plotfun", "lastupd"),
    # "maxval", "current" may be vectors

    methods=list(
        initialize = function(maxval, width=.termWidth(), type="txt") {
            options(digits.secs = 3)
            stopifnot(is.numeric(maxval) && all(maxval > 0))
            maxval <<- maxval
            width  <<- width
            type <<- type
            starttime <<- rep(as.numeric(Sys.time()), length(maxval))
            lastupd <<- as.numeric(Sys.time())
            current <<- rep(0, length(maxval))
            info <<- rep("", length.out=length(maxval))
            plotfun <<- .getPlotFun(type)
            plotfun$init(maxval, width)
        },

        update = function(i=NA, newmax=NULL, newinfo=NA, force=F) {
            if (is.null(i)) i <- current
            stopifnot(length(i) == length(maxval))
            if (!is.null(newmax)) {
                stopifnot(length(newmax) == length(maxval))
                maxval[!is.na(newmax)] <<- newmax[!is.na(newmax)]
            }
            if (!any(is.na(newinfo))) {
                info <<- rep(newinfo, length.out=length(i))
            }
            current <<- ifelse(is.na(i), current+1, i)
            current[current < 0] <<- 0
            starttime[current == 0] <<- as.numeric(Sys.time())
            current[current > maxval] <<- maxval[current > maxval]
            if (all(current == maxval)) {
                plotfun$update(starttime, current, maxval, width, info)
                plotfun$finish(starttime)
            } else if ((Sys.time() - lastupd) > 0.1 || force) {
                    plotfun$update(starttime, current, maxval, width, info)
                    lastupd <<- Sys.time()
            }
        }
    )
)

#' plapply
#'
#' lapply & sapply für multicor mit automatischer (pretty)progress-bar.
#' @export
plapply <- function(X, FUN, ..., mc.cores = getOption("mc.cores", 4L),
                       max.vector.size = getOption("max.vector.size", 1024L),
                       mc.progress="simple") {

    if (!is.vector(X) || is.object(X)) X <- as.list(X)
    stopifnot(length(X) > 0)
    
    # prepare progressbars
    if ("character" %in% is(mc.progress)) {
        mc.progress <- ppbar(length(X), type=mc.progress)
    } else if ("list" %in% is(mc.progress) && length(mc.progress)>=2) {
        mc.progress <- mc.progress
        mc.progress[[1]]$current[mc.progress[[2]]] <- 0
        mc.progress[[1]]$maxval[mc.progress[[2]]] <- length(X)
        mc.progress[[1]]$update(NULL)
    } else {
        stop("mc.progress must be character or list of length 2: c(ppbar, slot)")
    }
    
    updateprogress <- function(full, incr) {
        if (length(mc.progress) >= 2) {
            mc.progress[[1]]$current[mc.progress[[2]]] <- full + incr
            if ("add" %in% names(mc.progress)) {
                cadd <- mc.progress[[1]]$current[mc.progress[["add"]]] + incr
                mc.progress[[1]]$current[mc.progress[["add"]]] <- cadd
            }
            mc.progress[[1]]$update(NULL)
        } else {
            mc.progress$update(full + incr)
        }
        return(full + incr)
    }
    
    if (mc.cores > 1) { # real multicore
        require(parallel)
        require(future)
        # Set up maximun global size for the future package
        options(future.globals.maxSize=max.vector.size*1024^2)
        # Set up plan
        originalPlan <- plan("list")
        on.exit(plan(originalPlan))
        # plan(multiprocess)  # deprecated
        plan(multicore)

        progressFifo <- fifo(tempfile(), open="w+b", blocking=T)
        on.exit(close(progressFifo), add=T)
      
        progressMonitor <- futureCall(function(X, FUN, ..., mc.cores) {
            # Get results
            result <- mclapply(X, function(...) {
                res <- FUN(...)
                writeBin(1L, progressFifo)
                return(res)
            }, ..., mc.cores = mc.cores)
            # Check if any error was triggered
            if ("try-error" %in% sapply(result, class)) {
                # Warn the progress monitor if there's an error
                writeBin(-1L, progressFifo)
            }
            close(progressFifo)
            return(result)
        }, globals=list(progressFifo=progressFifo), args=list(X, FUN, ..., mc.cores=mc.cores))
        
        progress <- 0
        hasError <- F
        while (progress < length(X)) {
            progressUpdate <- readBin(progressFifo, "integer", n=100)
            # Check if any warning or error in the update
            if (any(progressUpdate == -1)) {
                hasError <- T
                break()
            }
            progress <- updateprogress(progress, sum(progressUpdate))
        }
        
        # Retrieve the result from the future
        results <- value(progressMonitor)
        if (hasError) warning("scheduled cores encountered error(s)")
    } else {
        # for single-core we don't spawn anything and just interate
        results <- list()
        items <- if(is.null(names(X))) 1:length(X) else names(X)
        for (i in 1:length(X)) {
            results[[items[i]]] <- FUN(X[[i]])
            updateprogress(i,0)
        }
    }
    return(results)
}

#' psapply
#'
#' lapply & sapply für multicor mit automatischer (pretty)progress-bar.
#' @export
psapply <- function (X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE, mc.cores=getOption("mc.cores", 4L),
                       max.vector.size = getOption("max.vector.size", 1024L),
                       mc.progress="simple") {
    FUN <- match.fun(FUN)
    answer <- plapply(X = X, FUN = FUN, ..., mc.cores=mc.cores, max.vector.size=max.vector.size, mc.progress=mc.progress)
    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
        names(answer) <- X
    if (!identical(simplify, FALSE) && length(answer)) 
        simplify2array(answer, higher = (simplify == "array"))
    else answer
}
