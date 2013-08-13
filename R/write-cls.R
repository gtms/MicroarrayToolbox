## writeCLS
## 06Aug2013
##
## This function is taken from the package GenePattern, available from
## http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/programmers/sections/gp_r
##
## given cls file and a file path, writes a cls file on disk
## see:
## http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/cls
## for more information

writeCLS <- function (cls,
                      filename,
                      check.file.extension = TRUE) {
    if (check.file.extension) {
        filename <- checkExtension (filename, ".cls")
    }
    file <- file (filename, "w")
    on.exit (close (file))
    cat (file = file, length (cls$labels), length (levels (cls$labels)), 
         "1\n")
    if (length (cls$names) > 0) {
        cat (file = file, "# ")
        i <- 1
        while (i < length (cls$names)) {
            cat (file = file, cls$names[i])
            cat (file = file, " ")
            i <- i + 1
        }
        cat (file = file, cls$names[length (cls$names)])
        cat (file = file, "\n")
    }
    i <- 1
    while (i < length (cls$labels)) {
        cat (file = file, as.numeric (cls$labels[[i]]) - 1)
        cat (file = file, " ")
        i <- i + 1
    }
    cat (file = file, as.numeric (cls$labels[[length (cls$labels)]]) - 1)
    filename
}
