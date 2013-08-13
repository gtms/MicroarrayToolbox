## pruneFeatures
## 29Jul2013
##
## given a gct and a value 0 <= tol < 1, returns the gct pruned from the
## features with more than 'tol' percent of absent calls (tol defaults to .2) 

pruneFeatures <- function (gct,
                           tol = .2) { # tolerated percentage of
                                        # absent calls
    idd <- apply (gct$data, 1, function (x) {
        sum (is.na (x)) / length (x) <= tol
    })
    list (data = gct$data[idd, ],
          row.descriptions = gct$row.descriptions[idd])
}
