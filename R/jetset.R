jetSet <- function (gct,
                    annotation.pkg) {
    require (jetset)
    dfr <- jscores (annotation.pkg)
    dtb <- data.table (dfr, keep.rownames = TRUE)
    setnames (dtb, "rn", "probeset")
    top.probesets <- dtb[, probeset[which.max (overall)], by = symbol]
    keep <- rownames (gct$data) %in% top.probesets[, V1]
    rd <- gct$row.descriptions[keep]
    dt <- gct$data[keep, ]
    na.id <- is.na (rd)
    list (row.descriptions = rd[!na.id],
          data = dt[!na.id, ])
}
