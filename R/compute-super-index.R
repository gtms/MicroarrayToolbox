computeSuperIndex <- function (gct,
                               sig,
                               is.probeset = FALSE) {
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
