## subsetGCTsamples
## 29Jul2013
##
## given a gct and a logical vector of the same length as the number of samples
## (logical.vec), returns a gct with samples subsetted by the logical vector

subsetGCTsamples <- function (gct,
                              logical.vec) { # logical vector with length
                                        # equal to the number of
                                        # samples in the gct
  list (row.descriptions = gct$row.descriptions,
        data = gct$data[, logical.vec])
}

