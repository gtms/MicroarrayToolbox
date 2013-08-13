## collapseRows
## 29Jul2013
##
## given a gct, uses the MaxMean method to collapse expression of features
## pointing to the same gene symbol and returns the collapsed gct
## implementation of the method described here:
## http://www.biomedcentral.com/1471-2105/12/322
collapseRows <- function (gct) {
    .sums <- rowSums (abs (gct$data), na.rm = TRUE)
    .id <- tapply (1:length (gct$row.descriptions),
                   gct$row.descriptions,
                   function (x) x[which.max (.sums[x])])
    list (row.descriptions = gct$row.descriptions[.id],
          data = gct$data[.id, ])
}

