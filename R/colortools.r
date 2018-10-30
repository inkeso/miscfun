
#' LSDize
#'
#' Transform any vector into a color. The result can be varied with ii [1...27]
#' @export
LSDize <- function(x, ii=1) { 
    require(digest)
    resi <- unlist(lapply(x, function(x){
        paste("#",substr(digest(x,algo="sha1"),ii,ii+5),sep="")
    }))
    names(resi) <- as.character(x)
    return(resi)
}

#' LSDize.preview
#'
#' shows a map of al 27 possibilities for a given vektor
#' @export
LSDize.preview <- function(x) {
    slen=35 # crc32: 2; md5: 27; sha1: 35
    if (length(x) == 1) x = c(x,x)
    testkit <- x
    testmat <- matrix(1:(slen*length(testkit)), 
                      ncol=length(testkit), nrow=slen, byrow=T)
    colnames(testmat) <- testkit
    rownames(testmat) <- paste("ii",1:slen,sep="=")
    testcol <- unlist(lapply(1:slen, function(x){LSDize(testkit,ii=x)}))
    heatmap(testmat[slen:1,], col=testcol, scale="none", Colv=NA, Rowv=NA)
}

#' rg.colors
#'
#' Color palette from red to green (via black)
#' @export
rg.colors <- function (n, alpha = 1) {
    if ((n <- as.integer(n[1])) > 0) {
        even.n <- n%%2 == 0
        k <- n%/%2
        l1 <- k + 1 - even.n
        l2 <- n - k + even.n
        reds = seq.int(1, ifelse(even.n, 1/k, 0), length.out = l1)
        grns = seq.int(0, 1, length.out = l2)[-1]
        c(if (l1 > 0) rgb(r = reds, g = 0, b = 0, alpha = alpha), 
          if (l2 > 1) rgb(r = 0, g = grns, b = 0, alpha = alpha))
    }
    else character(0)
}

#' pheat.colors
#'
#' color platte also used by pheatmap
#' @export
pheat.colors <- function(n) {
    require(RColorBrewer)
    return (colorRampPalette(rev(brewer.pal(n=7, name="RdYlBu")))(n))
}

# see also: viridisLite, dichromat, GetTolColors, alle_farben.html
