subsetGCTsamples <- function (gct,
                              logical.vec) {
  list (row.descriptions = gct$row.descriptions,
        data = gct$data[, logical.vec])
}

