collapseRows <- function (gct) {
    .sums <- rowSums (abs (gct$data), na.rm = TRUE)
    .id <- tapply (1:length (gct$row.descriptions),
                   gct$row.descriptions,
                   function (x) x[which.max (.sums[x])])
    list (row.descriptions = gct$row.descriptions[.id],
          data = gct$data[.id, ])
}

