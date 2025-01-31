#' dreamroc
#'
# 'Calculate ROC- & PR-Curve and AUC for a possibly incomplete prediction
# 'the same way, the DREAM-Committee does, according to the paper
# '
# 'Stolovitzky, G., Prill, R. J. and Califano, A. (2009),
# 'Lessons from the DREAM2 Challenges.
# 'Annals of the New York Academy of Sciences, 1158: 159–195.
# 'doi: 10.1111/j.1749-6632.2009.04497.x
# '
# 'providing an own plot-function for displaying both curves & areas.
#' @param Truth must be a named logical vector
#' @param Prediction must be a named numeric vector
#' @export
dreamroc <- function(Truth, Prediction) {
    # Input samples (Gold Standard):
    if (!is.logical(Truth))
        stop("Truth must be a logical vector")
    P <- sum(Truth)   # number of real positives
    N <- sum(!Truth)  # number of real negatives
    Tl <- length(Truth)

    # Input predictor
    if (!is.numeric(Prediction))
        stop ("Predictions must be numeric")
    if (length(intersect(names(Prediction),names(Truth)))==0)
        stop ("Nothing was predicted (no matching names)")

    Preds <- sort(Prediction, decreasing=TRUE)

    L <- length(Preds)

    # prepare result-vectors and scalars
    prec <- rep(NA,L)  # precision
    rec  <- rep(NA,L)  # recall, True positive rate, Sensitivity
    fpr  <- rep(NA,L)  # False positive rate

    FP <- 0       # False Positive
    TP <- 0       # True Positive
    TN <- 0       # True Negative
    AUPR <- 0     # Area under Precision/Recall curve.
    deltaAUC <- 0 # Area under unpredicted part of PR-curve
    AUROC <- 0    # Area under ROC curve

    # for each prediction
    for (k in 1:L) {
        if (Truth[names(Preds)[k]]) {
            TP <- TP + 1
            AUPR = AUPR + 1/P * (1 - FP * log( (k+1)/k ) )
        } else {
            FP <- FP + 1
        }
        sprintf("TP: %d, FP: %d\n", TP, FP)
        rec[k]  <- TP / P
        prec[k] <- TP / k
        fpr[k]  <- FP / N
    }
    # k==L
    TPl = TP
    if (TP < P) {
        rho = (P-TP)/(Tl-L)
        Lrho = L * rho
        rL = rec[L]
        AUPR <- AUPR +
            (rho*(1-rL) + rho*(rL - Lrho/P) * log((Lrho + P*(1-rL)) / Lrho))
        for (k in ((1:(P-TP))+L)) {
            TP=TP+1
            rk=TP/P
            rec = c(rec, rk)
            prec= c(prec, rho*P*rk / (P*(rk-rL) + Lrho))
            if (prec[k] != 0)
                FP = TP * (1 - prec[k])/prec[k]
            else
                FP = 1
            fpr = c(fpr, FP/N)
        }
    }

    #Correct calculation if equal predictions exist
    outsel = sort(Prediction,decreasing=T)
    outsel = c(outsel[2:length(outsel)] != outsel[1:(length(outsel)-1)],T)
    Tru  <- Truth[names(Preds)]
    rec  <- rec  [c(outsel,rep(TRUE,length(rec)   -length(outsel)))]
    prec <- prec [c(outsel,rep(TRUE,length(prec)  -length(outsel)))]
    fpr  <- fpr  [c(outsel,rep(TRUE,length(fpr)   -length(outsel)))]
    Tru  <- Tru  [c(outsel,rep(TRUE,length(Tru)   -length(outsel)))]
    Pre  <- Preds[c(outsel,rep(TRUE,length(Preds) -length(outsel)))]

    # integrate Area under ROC
    I <- 0
                 # L+P-TPl
    for (n in 1:(length(fpr)-1)) {
        I <- I + (fpr[n+1]+fpr[n]) * (rec[n+1]-rec[n]) / 2
    }
    AUROC <- 1 - I
    results <- list(fpr=fpr,rec=rec, prec=prec, Truth=Tru, Preds=Pre, AUROC=AUROC, AUPR=AUPR)
    class(results) <- "dreamroc"
    return(results)
}

#' @export
plot.dreamroc <- function(rocobj, main="", ...) {
    par(mfrow=c(1,2),omi=c(0,0,0.3,0))
    plot(0:1,0:1, xlab="False positive rate", ylab="Recall", type="n",
                  main="Receiver Operating Characteristic",
                  sub=sprintf("AUROC = %4.3f",rocobj$AUROC), ...)
    abline(0, 1, lty=2)
    lines(rocobj$rec ~ rocobj$fpr)


    plot(0:1,0:1, xlab="Recall", ylab="Precision", type="n",
                  main="Precision vs. Recall",
                  sub=sprintf("AUPR = %4.3f",rocobj$AUPR), ...)
    lines(rocobj$prec ~ rocobj$rec)

    mtext(main,outer=T,cex=2)
}

#' DEPRECATED
#' Create Points for a ROC-Curve. The result is an object with its own
#' print- and plot-functions. This is only provided for compatibility, use
#' dreamroc() instead.
#' @param noReduce is no longer used
#' @export
roc <- function(negatives, positives, noReduce=F) {
    preds <- c(positives, negatives)
    truth <- c(rep(TRUE, length(positives)), rep(FALSE,length(negatives)))
    names(preds) <- 1:length(preds) -> names(truth)
    dr <- dreamroc(truth, preds)
    results <- list(fpr=dr$fpr, tpr=dr$rec, precision=dr$prec, auc=dr$AUROC, aupr=dr$AUPR)
    results <- list(fpr=dr$fpr, tpr=dr$rec, precision=dr$prec, auc=dr$AUROC, aupr=dr$AUPR)
    class(results) <- "roc"
    return(results)
}

#' @export
print.roc <- function(x) {
    print (data.frame(x$alldata))
    cat (" AUC:",x$auc,"\n")
}

#' @export
plot.roc <- function(x, add=F, ...) {
    if (!add) {
        suppressWarnings(
            plot(c(0,1), c(0,1),
                 type="n",
                 xaxs="i",   yaxs="i",
                 xlab="False Positive Rate (= 1-Specificity)",
                 ylab="True Positive Rate (= Sensitivity)",
                 ...
            )
        )
        abline(0, 1, lty=2)
    }
    lines(c(1, x$fpr, 0), c(1, x$tpr, 0), ...)
}

#' plot_pr
#'
#' Plot Precision-Recall-Curve. x is a ROC-object.
#' @export
plot_pr <- function (x, add = F, ...) {
    if (!add) {
        suppressWarnings(plot(c(0, 1), c(0, 1), type = "n", xaxs = "i",
            yaxs = "i", xlab = "Recall (= Sensitivity)",
            ylab = "Precision", ...))
    }
    lines(x$tpr, x$precision, ...)
}

#' @export
roc.demo <- function() {
    neg_d=rnorm(10, mean=10, sd=5)
    pos_d=rnorm(10, mean=14, sd=6)
    roc_d=roc(neg_d,pos_d)
    print (roc_d)
    x11();
    plot(roc_d, main="ROC for some arbitrary data")
}

#' SUNBEAM
#'
#' "simple unredundant thus necessarily binary-based essential attribute
#' minimization"
#'
#' Algorithm for finding the best combination of specific
#' bound peptides, not bound in the controlgroup.
#' The aim is finding a combination of fewest possible peptides to tell
#' apart the sick from the healthy.
#' "Invented" during analysis of RA-ArrayIT (2009: JPT12[134]_..._1396 )
#' and SSc (2010: 01_Ssc)
#' For a given matrix of specificities ("specres") and two groups
#' (columns in specres) the best peptide-combination (rows in specres)
#' is returned. Has its own print- and plot-functions
#' @export
sunbeam <- function(specmat, grp_hi, grp_lo, verbose=F) {
    # Konsistenzprüfung
    if ((!is.logical(specmat)) || (length(dim(specmat))<2)) {
        stop("ERROR. First parameter must be a logical matrix.")
    }

    if (!(grp_hi %in% colnames(specmat) && grp_lo %in% colnames(specmat))) {
        stop ("ERROR. Element(s) from group(s) missing in logical matrix.")
    }

    if (length(intersect(grp_hi,grp_lo))>0) {
        stop ("ERROR. Groups overlapping.")
    }

    # Suche Peptide, die in grp_lo nicht spezifisch waren; die Ausgangsmenge
    ctr_spec = apply(specmat[,grp_lo, drop=F],1,sum)
    ctr_spec = names(ctr_spec[ctr_spec==0])
    if (length(ctr_spec) == 0) {
        warning ("NO RESULTS. No non-specific features in 'low' group found.")
        return (NA)
    }

    ill_spec = sort(apply(specmat[ctr_spec,grp_lo,drop=F],1,sum))
    if (length(ill_spec)==0) {
        warning ("NO RESULTS. No specific features left in 'high' group.")
        return (NA)
    }

    # convert binary to integer
    Sig2=specmat
    Sig2[specmat]=1

    # specificity-matrix for disease
    themat = Sig2[names(ill_spec),grp_hi, drop=F]
    resupep = c()  # result-peptide
    detpat = c()   # 'detected' patients
    # TODO: bei gleicher Anzahl verbleibender hits (=pro Peptid neu gewonnene
    #       Patienten) eine "intelligente" Auswahl treffen
    repeat {
        # peptid mit höchster Anz. hits
        hipi = sort(apply(themat,1,sum),decreasing=T)[1]
        if (hipi == 0) break() # keine specifischen übrig
        resupep <- append(resupep,hipi)
        # alle vom höchsten Pep. abgedeckte Patienten
        hipipat = themat[names(hipi),]
        rpat = names(hipipat)[hipipat==1]
        if (hipi == ncol(themat)) { # Keine Patienten übrig / alle erkannt
            detpat <- append(detpat,colnames(themat))
            break()
        }
        detpat <- append(detpat,rpat)
        # remove peptide & patients from the matrix
        themat = themat[setdiff(rownames(themat), names(hipi)),
                        setdiff(colnames(themat), rpat), drop=F]
    }
    ndetpat = setdiff(grp_hi,detpat)
    myset=c(detpat, ndetpat, grp_lo)
    mydat=Sig2[names(resupep),myset]

    thisresult = list(detected    = detpat,
                      notdetected = ndetpat,
                      peptides    = resupep,
                      resultmatrix= mydat)
    class(thisresult) <- "sunbeam"
    return (thisresult)
}

#' @export
print.sunbeam <- function(x) {
    cat("$detected:   ", length(x$detected),    "\n")
    cat("$notdetected:", length(x$notdetected), "\n")
    cat("$peptides:   ", length(x$peptides),    "\n")
}

#' @export
plot.sunbeam <- function(x, ...) {
    # remember:
    #  ColSideColors = rscol[c(x$detected, x$notdetected, grp_lo)]
    ndet = length(x$detected)
    nall = length(c(x$detected, x$notdetected))
    mtit=paste("Specific Peptides in ", ndet, " of ", nall,
               " (", round(ndet/nall*100, digits=1), "%) ",
                "Patients", sep="")

    pheatmap(x$resultmatrix, col=rev(gray.colors(3)),
              cluster_rows=F,cluster_cols=F, legend=F, main=mtit, ...)
}

#' logttest
#'
#' log2-t-Test for finding peptides, different in 2 groups.
#' `grp1` and `grp2` should be columns of `dat`.
#' Ex.: logttest(Signal[specificPeptide,], gCtr, gIll)
#' DEPRECATED. Use groupdiff instead
#' @export
logttest <- function (dat, grp1, grp2, p=0.001) {
    hib=groupdiff(dat, grp1, grp2, p, "ttest")
    names(c(sort(hib$higher1),sort(hib$higher2)))
}

#' groupdiff
#'
#' find differential peptides in 2 groupes.
#' @param dat data matrix or similar
#' @param grp1 and
#' @param grp2 should be columns of `dat`.
#' @param wtest following tests are available:
#'
#' "ttest" - log-2-t-test. `dat` should be signals
#'
#' "wilcox" - Wilcoxon rank sum test. `dat` should be signals too
#'
#' "fisher" - Exact fisher-test, `dat` should be T/F-table (specres)
#'
#' @return a list with a vector of peptides higher in group 1, a vector
#' of peptides higher in group 2 and, if fisher-test was done, a table
#' with the specific-counts (2x2-tables)
#' @export
groupdiff <- function(dat, grp1, grp2, p=0.001, wtest="ttest") {
    if(wtest == "ttestnolog"){
        calcPvalue <- function(x) {
            x1 <- x[grp1]
            x2 <- x[grp2]
            sign <- (mean(x1) > mean(x2))*2-1 # -1 if grp1 is lower
            return (sign * t.test(x1, x2)$p.value)
        }
        tmpP <- apply(dat, 1, calcPvalue)
    }
    # log(2) transformation & t-test to find differential peptides
    # `dat` should be Signals
    if(wtest == "ttest"){
        calcPvalue <- function(x) {
            x1 <- x[grp1]
            x2 <- x[grp2]
            sign <- (mean(x1) > mean(x2))*2-1 # -1 if grp1 is lower
            return (sign * t.test(log2(x1), log2(x2))$p.value)
        }
        tmpP <- apply(dat, 1, calcPvalue)
    }

    # wilcoxon rank sum test
    # `dat` should be Signals
    if(wtest == "wilcox") {
        calcPvalue <- function(x) {
            x1 <- x[grp1]
            x2 <- x[grp2]
            sign <- (mean(x1) > mean(x2))*2-1 # -1 if grp1 is lower
            return (sign * wilcox.test(x1, x2, exact=F, alternative="two.sided")$p.value)
        }
        tmpP <- apply(dat, 1, calcPvalue)
    }

    # use exact fisher-test to find differential peptides
    # `dat` should be specres
    if(wtest == "fisher") {
        # record the numbers
        fishtab <- matrix(ncol=4,nrow=0)
        colnames(fishtab) <- c("Pos1","Neg1","Pos2","Neg2")

        calcPvalue <- function(x) {
            x1 <- x[grp1]
            x2 <- x[grp2]
            fv = c(sum(x1, na.rm=T), sum(!x1, na.rm=T), sum(x2, na.rm=T), sum(!x2, na.rm=T))
            fishtab <<- rbind(fishtab,fv)
            f <- matrix(fv, nrow=2)
            if (sum(f[1,])>0 && sum(f[2,])>0) {
                sign <- (f[1,1]/f[2,1] > f[1,2]/f[2,2])*2-1 # -1 if grp1 is lower
                return (sign * fisher.test(f, alternative="two.sided")$p.value)
            } else {
                return (NA)
            }
        }
        tmpP <- apply(dat, 1, calcPvalue)
        rownames(fishtab) <- rownames(dat)
    }

    tmpP <- tmpP[!is.na(tmpP)]
    higher1 <- tmpP[tmpP >= 0 & tmpP <= p]
    higher2 <- tmpP[tmpP < 0 & tmpP >= -p]
    higher2 <- -higher2
    result <- list(higher1=sort(higher1),higher2=sort(higher2))
    if (wtest=="fisher") result$fishertable <- fishtab
    result
}
