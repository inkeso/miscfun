#' captio
#'
#' display string as caption (underlined)
#' @export
caption <- function(txt, color="1;37", ulchar="▔", ulcolor="31") {
    txt <- gsub("(^\\s+)|(\\s+$)", "", txt)
    cat("\033[",color,"m",txt,"\033[0m\n",sep="")
    if (!(is.null(ulchar) || is.na(ulchar) || (nchar(ulchar) == 0)))
        cat("\033[",ulcolor,"m",rep(ulchar,nchar(txt)),"\033[0m\n",sep="")
}

#' lf
#'
#' List content of current workspace
#' @export
lf <- function() {
    #suppressPackageStartupMessages(require(gdata))
    dat <- gdata::ll(".GlobalEnv",digits=1, dim=T, sort=T)
    dat_fn <- dat[dat$Class=="function",]
    dat_dt <- dat[dat$Class!="function" & rownames(dat)!="ppbarenv",]
    if (nrow(dat_fn) > 0) {
        caption("Functions", ulcolor=32)
        for (item in rownames(dat_fn)) {
            if (is.function(get(item))) {
                para=gsub("^function \\(|\\)  $","",capture.output(str(get(item), give.attr=F)))
                cat("\033[32m",item,"\033[m(\033[33m",para,"\033[m)\n", sep="")
            }
        }
        cat("\n")
    }
    if (nrow(dat_dt) > 0) {
        caption("Data",ulcolor=32)
        dcl = capture.output(print(dat_dt[,c(1,3,2)]))
        # highlight!
        dcl <- sub("^([^ ]+) ", "@97m\\1@0m ", dcl) # all names
        dcl[1] <- paste("@97m",dcl[1],"@0m", sep="") # heading
        dcl <- sub(" (data.(frame|table)|matrix|array) ", " @36m\\1@0m ",dcl)
        dcl <- sub(" (list) ",                            " @34m\\1@0m ",dcl)
        dcl <- sub(" (environment) ",                     " @31m\\1@0m ",dcl)
        dcl <- sub(" (numeric|character|logical) ",       " @33m\\1@0m ",dcl)
        dcl <- sub(" (factor) ",                          " @1;33m\\1@0m ",dcl)
        cat(gsub("@","\033[",dcl),sep="\n")
    }
}

#' rc
#'
#' read from clipboard into vector (separated by line)
#' @export
rc <- function() {
    cbf = file("clipboard")
    targ <- readLines(cbf)
    close(cbf)
    return(targ)
}

.checkSysBin <- function(bin) {
    res <- attr(suppressWarnings(system(paste("which",bin), int=T, ignore.stdo=T, ignore.stde=T)),"status")
    return(length(res) == 0 || res[1] == 0)
}

#' read
#'
#' automagically read tables to data.frame or data.table.
#' Can read from textfiles (csv), even compressed (bz2, xz, gzip), excel-files.
#' Will read directly from http(s), ftp.
#' @export
read <- function(input, sheet=1, data.table=F, ...) {
    require(data.table)
    stopifnot(length(input) == 1)
    stopifnot(is(input, "character"))
    # list of known decompressors, must accept -d and output to stream.
    # fread is able to read gz and bz2 directly, but it decompresses to a tempfile.
    # using streams is way better.
    DECOMP <- c(gz="gzip", bz2="bzip2", xz="xz")
    # check for faster versions
    if (.checkSysBin("pigz")) DECOMP["gz"] <- "pigz"
    if (.checkSysBin("pixz")) DECOMP["xz"] <- "pixz"
    if (.checkSysBin("pbzip2")) DECOMP["bz2"] <- "pbzip2"
    extension <- last(unlist(strsplit(input, ".", fix=T)))

    iscmd <- FALSE
    remote <- FALSE
    if (substring(input, 1, 6) %in% c("http:/","https:","ftp://")) {
        input <- sprintf('wget -q --show-progress -O- "%s"', input)
        remote <- TRUE
        iscmd <- TRUE
    }

    if (extension %in% names(DECOMP)) {
        if (remote)
            input <- sprintf("%s | %s -d", input, DECOMP[extension])
        else
            input <- sprintf('%s -d <"%s"', DECOMP[extension], input)
        iscmd <- TRUE
    }

    if (toupper(extension) %in% c("XLS", "XLSX")) {
        require(openxlsx)
        if (remote) {
            tmp <- tempfile()
            system(sprintf("%s > \"%s\"", input, tmp))
            input <- tmp
        }
        res <- read.xlsx(input, sheet)
        if (data.table) {
            return(as.data.table(res))
        } else {
            return(res)
        }
    }
    if (iscmd) {
        return(fread(cmd=input, data.table=data.table, ...))
    } else {
        return(fread(input, data.table=data.table, ...))
    }
}

#' read.fasta
#'
#' read FASTA-File
#' @export
read.fasta <- function (file, to.upper=FALSE) {
    raw.fa <- scan(file, what=character(0), sep="\n", quiet=TRUE)
    ind <- grep(">", raw.fa)
    if (length(ind) == 0)
        stop("read.fasta: no '>' id lines found, check file format")

    if (to.upper) raw.fa[-ind] <- toupper(raw.fa[-ind])

    ind.s <- ind + 1
    ind.e <- c((ind - 1)[-1], length(raw.fa))

    seq.format <- function(x) {
        paste(raw.fa[(x[1]:x[2])], collapse="")
    }
    store.fa <- apply(cbind(ind.s, ind.e), 1, seq.format)
    names(store.fa) <- gsub("^>", "", raw.fa[ind])

    return(store.fa)
}

#' showxls
#'
#' export (and show) something as excel-file.
#' @param mat
#'   This may be a single object (anything that may be coerced to matrix). It will be exportet as a single-sheet-XLS.
#'   Or it may be an environment. In this case, every object will be exportet as a single sheet in the resulting XLS.
#' @param filename
#'   Write to this file. If 'NA', it will be a tempfile() and directly opend in localc.
#' @export
showxls <- function(mat, filename=NA) {
    fina <- ifelse(is.character(filename), filename, paste(tempfile(),".xlsx",sep=""))
    require(openxlsx)
    if (is.environment(mat)) {
        mat <- as.list(mat)
        # filter functions
        mat <- mat[!sapply(mat, function(x) any(c("refObject", "function") %in% is(x)))]
        mat <- lapply(mat, function(x) if(is.null(dim(x))) as.matrix(x) else x)
    } else {
        if (is.null(dim(mat))) mat <- as.matrix(mat)
    }
    #if (!is.list(mat)) mat <- as.data.frame(mat)
    write.xlsx(mat, fina, rowNames=TRUE, sep="|") # sep doesn't work?! [TODO]
    # show directly if no filename given
    if (is.na(filename)) system(paste ("localc" ,fina), ignore.stdout=T, ignore.stderr=T, wait=F)
}

#' env2xls
#'
#' export everything from environment. showxls can do that. This is depricated.
#' @export
env2xls <- function (envir, filename=NA) {
    warning("env2xls is deprecated, use showxls() instead.")
    showxls(envir, filename)
}

#' list2xls
#'
#' export a list of this as XLS with each item in a spereate sheet
#' @export
list2xls <- function (dflist, filename=NA) {
    stopifnot(is(dflist, "list"))
    fina <- ifelse(is.character(filename), filename, paste(tempfile(),".xlsx",sep=""))
    write.xlsx(dflist, fina, rowNames=TRUE, sep="|")
    if (is.na(filename)) void<-capture.output(system(paste ("localc" ,fina)))
}

#' list2table
#'
#' create a table from a list of vectors (even with different lengths)
#' @export
list2table <- function(x) {
    mali <- max(sapply(x,length))
    targ <- data.frame(row.names=1:mali)
    if (is.null(names(x))) names(x) <- paste0("L",1:length(x))
    for (l in 1:length(x)) {
        vec <- x[[l]]
        if (length(vec) < mali) vec <- c(vec, rep(NA, mali-length(vec)))
        targ[,names(x)[l]] <- vec
    }
    return(targ)
}

#' strcompare
#'
#' calculate the similarity of 2 strings
#' @export
strcompare <- function(x, y, motif=F) {
    x <- unlist(strsplit(x, "")) # compare this
    y <- unlist(strsplit(y, "")) # ...to that
#~     if (length(y) < length(x)) { # output will be length x
#~         tmp <- x; x <- y; y <- tmp
#~     }
    # pad second sequence
    y <- c(rep(" ", length(x)), y, rep(" ", length(x)))
    # calc all matches
    mtch <- sapply(1:(length(y)-length(x)), function(z) {
        sum(x==y[z:(length(x)-1+z)])
    })
    if (motif) {
        z <- which.max(mtch)
        m <- x==y[z:(length(x)-1+z)]
        x[!m] <- "."

        return (list(
            motif=paste(x, collapse=""),
            match_sequence=paste(y[z:(z+length(x)-1)], collapse=""),
            position=c(z-length(x)-1, z-1),
            matched=mtch[z] / length(x)
        ))
    } else {
        return(max(mtch) / length(x))
    }
}
