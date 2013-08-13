## rmInvariantFeatures
## 29Jul2013
##
## given a gct, removes genes/probesets with null variance and returns
## it

rmInvariantFeatures <- function (gct) {
    sd.vec <- apply (gct$data, 1, sd)
    idd <- sd.vec != 0
    list (row.descriptions = gct$row.descriptions[idd],
          data = gct$data[idd, ])
}
