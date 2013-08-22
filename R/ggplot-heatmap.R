ggHeat <- function (mtx,
                    do.reorder = TRUE,
                    title = "Heatmap",
                    path = NULL,
                    width = 10,
                    height = 7,
                    ...) {
    ## For melt () to work seamlessly, mtx has to be a matrix.
    if (!is.matrix (mtx)) stop ("Argument 'mtx' has to be of class 'matrix'")
    long.data <- melt (mtx)
    colnames (long.data)[1:2] <- c ("Var1", "Var2")

    ## Optionally, reorder both the row and column variables in any order
    ## Here, they are sorted by mean value
    if (do.reorder) {
        long.data$Var1 <- ordered (long.data$Var1,
                                   levels = names (sort (with (long.data,
                                       by (value, Var1, mean, na.rm = TRUE)))))
        long.data$Var2 <- ordered (long.data$Var2,
                                   levels = names (sort (with (long.data,
                                       by (value, Var2, mean, na.rm = TRUE)))))
    } else {
        long.data$Var1 <- ordered (long.data$Var1, levels = rownames (mtx))
        long.data$Var2 <- ordered (long.data$Var2, levels = colnames (mtx))
    }

    ## Define palette
    my.palette <- colorRampPalette (rev (brewer.pal (11, "Spectral")))

    zp1 <- ggplot (long.data,
                   aes (x = Var2, y = Var1, fill = value))
    zp1 <- zp1 + geom_tile ()
    zp1 <- zp1 + scale_fill_gradientn (colours = my.palette (100))
    zp1 <- zp1 + scale_x_discrete (expand = c (0, 0))
    zp1 <- zp1 + scale_y_discrete (expand = c (0, 0))
    zp1 <- zp1 + coord_equal ()
    zp1 <- zp1 + ggtitle (title)
    zp1 <- zp1 + theme_bw ()
    zp1 <- zp1 + theme (axis.title.x = element_blank (),
                        axis.title.y = element_blank (),
                        axis.text.x = element_text (angle = 45, hjust = 1),
                        legend.title = element_blank ())

    if (!is.null (path)) {
        zp1
        ggsave (file = tolower (file.path (path,
                    paste (title = gsub (" ", "-", title), ".pdf", sep = ""))),
                width = width,
                height = height)
    } else {
        print (zp1)
    }
}
