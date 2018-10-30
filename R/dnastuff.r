# Functions for DNA-Stuff (translating etc)

#' antisense
#'
#' create antisense-strand of given DNA
#' @export
antisense <- function(s) {
    if(!"Biostrings" %in% installed.packages())
        stop("Biostrings (from bioconductor) not installed.")
    # do not simply load Biostrings via require... it's way too much
    DNAString <- Biostrings::DNAString
    reverseComplement <- Biostrings::reverseComplement
    return(as.character(reverseComplement(DNAString(s))))
}

#' iupac2regex
#'
#' Convert an IUPAC-DNA-code zo a regexp matching this code
#' @export
iupac2regex <- function(x, mis=0) {
    if (mis <= 0) {
        x <- gsub("B", "[GTC]", x)   # not A (B comes after A)
        x <- gsub("D", "[GAT]", x)   # not C (D comes after C)
        x <- gsub("H", "[ACT]", x)   # not G (H comes after G)
        x <- gsub("K", "[GT]", x)    # keto
        x <- gsub("M", "[AC]", x)    # amino
        x <- gsub("N", "[AGCT]", x)  # any base (not a gap)
        x <- gsub("\\?", "[AGCT]", x)  # any base (not a gap)
        x <- gsub("R", "[GA]", x)    # purine
        x <- gsub("S", "[GC]", x)    # strong
        x <- gsub("V", "[GCA]", x)   # not T (V comes after T and U)
        x <- gsub("W", "[AT]", x)    # weak
        x <- gsub("-", "(.+?)", x)   # Gap
        x <- gsub("Y", "[TC]", x)    # pyrimidine
        return(x)
    } else if (mis <= 2) {
        #Fehlstellen!
        ux <- unlist(strsplit(x,""))
        gs <- apply(sapply(1:length(ux), function(i){grp <- ux; grp[i] <- "."; grp}),2,paste,collapse="")
        return (paste(sapply(gs, iupac2regex, mis=mis-1),collapse="|"))
    } else {
        stop("maybe you should not do this")
    }
}

#' translate_allframes
#'
#' Translate a DNA-sequence in all 6 reading-frames
#' @export
translate_allframes <- function(dna, mc.cores=6) {
    if(!"Biostrings" %in% installed.packages())
        stop("Biostrings (from bioconductor) not installed.")
    DNAString <- Biostrings::DNAString
    translate <- Biostrings::translate
    subseqs <- c(lapply(1:3, substring, text=dna), lapply(1:3, substring, text=antisense(dna)))
    trfun <- function(x) {
        suppressWarnings(translate(DNAString(x), if.fuzzy.codon="solve"))
    }
    if ("parallel" %in% installed.packages() && mc.cores > 1) {
        require(parallel)
        frames <- mclapply(subseqs, trfun, mc.cores=mc.cores)
    } else {
        frames <- lapply(subseqs, trfun)
    }
    frames <- sapply(frames, as.character)
    names(frames) <- c(paste0("53_F_", 1:3), paste0("35_F_", 1:3))
    return(frames)
}

#' ucsc
#'
#' Launch Browser with UCSC genome browser zoomed to given position
#' @export
ucsc <- function(chr, from, to) {
    browseURL(sprintf("http://genome-euro.ucsc.edu/cgi-bin/hgTracks?db=hg19&position=%s%%3A%d%%2D%d", chr, from, to))
}
