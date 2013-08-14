## sam
## 06Aug2013
##
## UPDATED on 14Aug2013:
## updated the name of the function 'instant.pkgs' to 'instantPkgs'
##
## This function is a wrapper for the SAM function of the 'samr'
## package that implements the Significant Analysis of Microarrays
## algorithm, described in Tusher, V., Tibshirani, R. and Chu,
## G. (2001): Significance of microarrays applied to the ionizing
## radiation response PNAS 2001 98:5116-5121, (Apr 24).  The
## expression data should be provided in the gct format.

sam <- function (gct,
                 class.id = NULL, # logical vector specifying the two
                                        # classes: TRUE for class one and FALSE
                                        # for class two
                 resp.type = c ("Quantitative", "Two class unpaired",
                     "Survival", "Multiclass", "One class", "Two class paired",
                     "Two class unpaired timecourse", "One class timecourse",
                     "Two class paired timecourse", "Pattern discovery"),
                 testStatistic = "standard",
                 nperms = 500,
                 logged2 = TRUE) {
    
    pkg.dfr <- data.frame (pkg = c ("samr",
                               "data.table"),
                           repos = rep ("cran", 2),
                           stringsAsFactors = FALSE)
    instantPkgs (pkg.dfr)
    
    ## removes samples with more than 80% missing values
    id80 <- apply (gct$data, 2, function (x) {
        sum (is.na (x)) / length (x) < .8
    })
    gct$data <- gct$data[, id80]
    
    if (is.null (class.id)) { 
            vec <- rep (1, ncol (gct$data))
        } else {
            vec <- ifelse (class.id, 2, 1)
        }

    delta <- 0 # delta chosen low, actual filtering based on q-values, see below
    samr.data <- list (x = gct$data,
                       y = vec,
                       geneid = rownames (gct$data),
                       genenames = gct$row.descriptions,
                       logged2 = logged2)

    samr.obj <- samr (samr.data,
                      resp.type = resp.type,
                      nperms = nperms,
                      testStatistic = "standard")

    delta.table <- samr.compute.delta.table (samr.obj)
    tab <- samr.compute.siggenes.table (samr.obj,
                                        delta,
                                        samr.data,
                                        delta.table,
                                        all.genes = TRUE)

    id.num <- c (1, 4:ncol (tab$genes.up)) # indices of columns to transform to 'numeric'
    tab$genes.up <- as.data.frame (tab$genes.up, stringsAsFactors = FALSE)
    tab$genes.up[, id.num] <- as.numeric (unlist (tab$genes.up[, id.num]))
    tab$genes.lo <- as.data.frame (tab$genes.lo, stringsAsFactors = FALSE)
    tab$genes.lo[, id.num] <- as.numeric (unlist (tab$genes.lo[, id.num]))

    formatColnames <- function (strg) {
        strg <- gsub ("[[:blank:]-]", ".", strg)
        strg <- gsub ("\\(\\%\\)", ".perc", strg)
        strg <- gsub ("\\(d\\)", ".d", strg)
        strg <- tolower (gsub ("\\(r\\)", ".r", strg))
        gsub ("\\(s\\+s0\\)", ".SplusS0", strg)
    }

    colnames (tab$genes.up) <- formatColnames (colnames (tab$genes.up))
    colnames (tab$genes.lo) <- formatColnames (colnames (tab$genes.lo))

    genes.up <- data.table (tab$genes.up)
    genes.up <- genes.up[order (q.value.perc, -score.d)]

    genes.dw <- data.table (tab$genes.lo)
    genes.dw <- genes.dw[order (q.value.perc, -score.d)]

    list (class.id = class.id,
          nperms = nperms,
          resp.type = resp.type,
          genes.up = genes.up,
          genes.dw = genes.dw)
}
