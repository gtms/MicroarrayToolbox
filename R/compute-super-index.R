## computeSuperIndex
## 30Jul2013
##
## given:
## a gct (gct);
## a character vector of features to subset the gct with (sig); and
## an logical specifying whether sig refers to affymetrix
## probesets or gene symbols (is.probeset),
## returns a numeric vector of the median expression of the features present in
## the expression matrix (should there be any, NA if not), sample-wise

computeSuperIndex <- function (gct,
                               sig, # character vector of features
                               is.probeset = FALSE) { # are the features
                                        # probesets? Defaults to gene symbols
    ## collapses rows of gct
    gct <- collapseRows (gct)

    ## selects which features to select from expression matrix
    if (is.probeset) {
        .id <- rownames (gct$data) %in% sig
    } else {
        .id <- gct$row.descriptions %in% sig
    }

    ## returns NA if no features from the signature are found in expression matrix
    if (sum (.id) == 0) return (NA)

    ## returns row of unique feature if only one feature from the signature is
    ## found in expression matrix
    if (sum (.id) == 1) return (gct$data[.id, ])

    ## otherwise proceeds to normalize matrix of expression and returns its
    ## median expression sample-wise
    mtx <- gct$data[.id, , drop = FALSE]
    mtx.median <- median (mtx, na.rm = TRUE)
    if (mtx.median == 0) mtx.median <- 1
    ## normalizes
    norm.mtx <- t (apply (mtx, 1, function (x) {
        md <- median (x, na.rm = TRUE)
        if (md == 0) {
            x * mtx.median
        } else {
            x * mtx.median / median (x, na.rm = TRUE)
        }
    }))
    ## returns median expression sample-wise
    apply (norm.mtx, 2, function (x) median (x, na.rm = TRUE))
}
