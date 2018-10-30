#!/bin/Rscript
##### DEMOS #####

#TODO: Wie bastelt man das, daß das mit demo(LSDize) geht?

LSDize.demo <- function() {
    LSDize.preview(LETTERS)
}

ppbar.demo <- function(typen=c("txt", "simple", "small", "pretty", "tk", "plot")) {
    for (ttt in typen) {
        # simplest way
        caption(paste(ttt, "single bar"))
        bar <- ppbar(100, type=ttt) # only max-value: simple text-progress
        for (i in 1:100) {
            Sys.sleep(0.05)
            bar$update() # no i given: increment by one.
        }

        ## several bars. Here we misuse the third bar as a gauge.
        caption(paste(ttt, "three bars, main thread only"))
        bar2 <- ppbar(c(10, 1, 100, 100, 100, 100), type=ttt)
        for (i in 1:10) {
            # set any bar to i=0 to reset its starttime (for ETA-calculation)
            # you can also set a new maxval for any of the bars any time.
            tlen <- 15000
            bar2$update(c(i-1, 0, 0, 0, 0, 0), newmax=c(NA, tlen, NA, NA, NA, NA))
            for (j in 1:tlen) {
                xx <- mean(runif(j/500))*100
                nfo <- c(sprintf("Chunk %s", LETTERS[i]), "Calculate...", "Entropy", "alpha", "beta", "gamma")
                # oh. did i mention fractural updates?
                frac <- i-1 + j/tlen
                vals <- c(j, xx, sin(frac/10*pi)*50+50, cos(frac/10*pi)*50+50, frac*10)
                bar2$update(c((i-1) + (j/tlen), vals), newinfo=nfo) 
            }
        }
        # to finish up, all bars must reach 100%
        bar2$update(c(i, j, 100, 100, 100, 100))

        # simple multicore example using our plapply / psapply
        {
            caption(paste(ttt, "simple multithreading"))
            runrange <- function(x) {
                for (i in 1:20) log(sqrt(mean(range(runif(i^2))))) # superimportant
                range(runif(x %/% 5000 + 1))
            }
            res <- psapply(1:100000, runrange, mc.cores=4, mc.progress=ttt)
        }
        
        # fancy visu
        {
            if (ttt=="txt") {
                plot(sort(res[1,]), type="l", lwd=2)
                lines(sort(res[2,]), lwd=2)
            } else if (ttt=="simple") {
                plot(t(res), pch=".", col="#00707037")
            } else if (ttt=="pretty") {
                plot(sort(res[1,]), rev(res[2,]), pch=20,col="#00509010")
            } else if (ttt=="tk") {
                plot(sqrt(c(res)), pch=18, col="#af301010")
            } else if (ttt=="plot") {
                bv <- res[1,] * res[2,]
                bv <- bv + rev(bv)
                plot(bv, pch=18, col="#00509009") 
                points(max(bv)-bv, pch=18, col="#90500009")
                grad <- c("#00432D", paste(colorRampPalette(c("#007950", "#905090"))(15), sprintf("%X0", 14:0), sep=""))
                for (cx in 30:45) {
                    text(50000, mean(range(bv))+(cx-30)/200, "That's all folks!", font=2, cex=cx/10, col=grad[46-cx])
                }
            }
        }
        
        # several iterations of a multicore-apply
        {
            caption(paste(ttt, "several iterations of multithreading"))
            chunks <- LETTERS[1:6]
            # a 'normal' *apply can use ppbar. so first create one with 2 bars (slots)
            tbar <- ppbar(c(length(chunks), 1), type=ttt)
            res <- t(sapply(chunks, function(i) {
                # first we may set an info-string for first slot:
                
                # this will set it and also update the progressbar:
                #tbar$update(NULL, newinfo=c(sprintf("Iter %s", i), ""))
                
                # or just set the info and it will be displayed on next update:
                tbar$info[1] <- sprintf("Iter %s", i)
                
                # start multithreaded workload and tell psapply to use the second slot of our bar:
                tr <- psapply(1:20000, runrange, mc.cores=4, mc.progress=c(tbar, 2))
                
                # after this is done, increment first slot, update progressbar
                tbar$current[1] <- tbar$current[1]+1
                tbar$update(NULL)
                
                return(tr)
            }))
        }
        
        # smooth update of global progress with different sized workloads, if you know the total count
        {
            caption(paste(ttt, "several multithreading tasks of different sizes but known total"))
            genome <- c(chr1=145836, chr2=105836, chr3=87583, chr4=23654, chrA=47586)
            
            tbar <- ppbar(c(sum(genome), 1), type=ttt)
            res <- t(sapply(names(genome), function(ch) {
                tbar$info[1] <- ch
                # start multithreaded workload and tell psapply to use the second slot of our bar
                # also add/acumulate progress to first bar
                return(psapply(1:genome[[ch]], runrange, mc.cores=6, mc.progress=c(tbar, 2, add=1)))
            }))
        }
        cat("------------------\n\n")
    }
}

ppbar.compare <- function(typen=c("txt", "simple", "small", "pretty", "tk", "plot"), forz) {
    return(sapply(typen, function(ttt) {
        ## several bars. Here we misuse the third bar as a gauge.
        caption(paste(ttt, "three bars, main thread only"))
        st <- Sys.time()
        bar2 <- ppbar(c(10, 1, 100, 100, 100), type=ttt)
        for (i in 1:10) {
            # set any bar to i=0 to reset its starttime (for ETA-calculation)
            # you can also set a new maxval for any of the bars any time.
            tlen <- 10000
            bar2$update(c(i-1, 0, 0, 0, 0), newmax=c(NA, tlen, NA, NA, NA), force=forz)
            for (j in 1:tlen) {
                nfo <- c(sprintf("Chunk %s", LETTERS[i]), "Calculate...", "alpha", "beta", "gamma")
                bar2$update(c(i, j, 5,5,5), newinfo=nfo, force=forz) 
            }
        }
        # to finish up, all bars must reach 100%
        bar2$update(c(i, j, 100, 100, 100), force=forz)
        return(Sys.time()-st)
    }))
}

#' ppbar.demo()
#' caption("\nNO FORCE")
#' nf <- ppbar.compare(forz=F)
#' caption("\nFORCE")
#' ff <- ppbar.compare(forz=T)
