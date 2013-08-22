ggplotMDS <- function (gct,
                       labels = NULL,
                       colour = NULL,
                       title = "MDS",
                       path = NULL,
                       width = 10,
                       height = 7,
                       ...) {
    dst.mtx <- as.dist (1 - cor (gct$data,
                                 use = "complete.obs"))
    mds.lst <- isoMDS (dst.mtx,
                       k = 2,
                       maxit = 1000,
                       tol = 1e-20)
    if (is.null (labels)) labels <- rownames (mds.lst$points)
    mds.dfr <- data.frame (x = mds.lst$points[, 1],
                           y = mds.lst$points[, 2],
                           labels = labels,
                           row.names = rownames (mds.lst$points))
    if (!is.null (colour)) {
        mds.dfr$colour <- colour
        mds.plot <- ggplot (mds.dfr, aes (x, y, label = labels, colour = colour))
    } else {
        mds.plot <- ggplot (mds.dfr, aes (x, y, label = labels))
    }
    mds.plot <- mds.plot + geom_text (size = 4) +
        scale_x_continuous (name = "") + # removes x-axis label
            scale_y_continuous (name = "") + # removes y-axis label
                theme_set (theme_bw ()) +
                    ggtitle (title) +
                        theme (plot.title = element_text (face = "bold",
                                   size = 15))
    if (!is.null (path)) {
        mds.plot
        ggsave (filename = tolower (file.path (path,
                    paste (title = gsub (" ", "-", title), ".pdf", sep = ""))),
                width = width,
                height = height)
    } else {
        print (mds.plot)
    }
}
