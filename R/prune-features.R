pruneFeatures <- function (gct,
                           tol = .2) {
    idd <- apply (gct$data, 1, function (x) {
        sum (is.na (x)) / length (x) <= tol
    })
    list (data = gct$data[idd, ],
          row.descriptions = gct$row.descriptions[idd])
}
