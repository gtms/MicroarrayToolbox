## readCLS
## 06Aug2013
##
## This function is taken from the package GenePattern, available from
## http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/programmers/sections/gp_r
##
## given a file path, reads a cls file into memory
## see:
## http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/cls
## for more information

readCLS <- function (file) {
    if (is.character (file)) 
        if (file == "") { 
            file <- stdin ()
        } else {
            file <- file (file, "r")
            on.exit (close (file))
        }
    if (!inherits (file, "connection")) 
        stop ("Argument `file' must be a character string or connection")
    line1 <- scan (file, nlines = 1, what = "character", quiet = TRUE)
    numberOfDataPoints <- as.integer (line1[1])
    numberOfClasses <- as.integer (line1[2])
    line2 <- scan (file, nlines = 1, what = "character", quiet = TRUE)
    classNames <- NULL
    if (line2[1] == "#") {
        classNames <- as.vector (line2[2:length (line2)])
        line3 <- scan (file, what = "character", nlines = 1, quiet = TRUE)
    } else {
        line3 <- line2
    }
    if (is.null (classNames)) {
        labels <- as.factor (line3)
        classNames <- levels (labels)
    } else {
        labels <- factor (line3, labels = classNames)
    }
    if (numberOfDataPoints != length(labels)) {
        stop ("Incorrect number of data points")
    }
    list (labels = labels,
          names = classNames)
}
