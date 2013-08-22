pruneFeatures <- function (gct,
                           tol = .2) {
    idd <- apply (gct$data, 2, function (x) {
        sum (is.na (x)) / length (x) <= tol
    })
    list (data = gct$data[, idd],
          row.descriptions = gct$row.descriptions)
}
