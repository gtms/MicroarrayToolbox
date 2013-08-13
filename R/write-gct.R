## writeGCT
## 06Aug2013
##
## This function is taken from the package GenePattern, available from
## http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/programmers/sections/gp_r
##
## given gct file and a file path, writes a gct file on disk
## see:
## http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/gct
## for more information

writeGCT <- function (gct,
                      filename,
                      check.file.extension = TRUE) {
    if (check.file.extension) {
        filename <- checkExtension (filename, ".gct")
    }
    f <- file (filename, "w")
    on.exit (close (f))
    cat ("#1.2", "\n", file = f, append = TRUE, sep = "")
    cat (dim (gct$data)[1], "\t", dim(gct$data)[2], "\n", file = f, 
         append = TRUE, sep = "")
    cat ("Name", "\t", file = f, append = TRUE, sep = "")
    cat ("Description", file = f, append = TRUE, sep = "")
    names <- colnames (gct$data)
    for (j in 1:length (names)) {
        cat ("\t", names[j], file = f, append = TRUE, sep = "")
    }
    cat ("\n", file = f, append = TRUE, sep = "")
    m <- matrix (nrow = dim (gct$data)[1], ncol = 2)
    m[, 1] <- row.names (gct$data)
    if (!is.null (gct$row.descriptions)) {
        m[, 2] <- gct$row.descriptions
    } else {
        m[, 2] <- ""
    }
    m <- cbind (m, gct$data)
    write.table (m, file = f, append = TRUE, quote = FALSE, sep = "\t", 
                 eol = "\n", col.names = FALSE, row.names = FALSE)
    filename
}
