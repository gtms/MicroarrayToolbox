## readGCT
## 06Aug2013
##
## This function is taken from the package GenePattern, available from
## http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/programmers/sections/gp_r
##
## given a file path, reads a gct file into memory
## see:
## http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/gct
## for more information

readGCT <- function (file) {
    if (is.character (file)) 
        if (file == "") { 
            file <- stdin ()
        } else {
            file <- file(file, "r")
            on.exit (close (file))
        }
    if (!inherits (file, "connection")) 
        stop ("Argument `file' must be a character string or connection")
    version <- readLines (file, n = 1)
    dimensions <- scan (file,
                        what = list ("integer", "integer"), 
                        nmax = 1,
                        quiet = TRUE)
    rows <- dimensions[[1]]
    columns <- dimensions[[2]]
    column.names <- read.table (file,
                                header = FALSE,
                                nrows = 1, 
                                sep = "\t",
                                fill = FALSE)
    column.names <- column.names[3:length (column.names)]
    if (length (column.names) != columns) {
        stop (paste ("Number of sample names", length (column.names), 
                     "not equal to the number of columns", columns, "."))
    }
    colClasses <- c (rep (c ("character"), 2),
                     rep (c ("double"), columns))
    x <- read.table (file,
                     header = FALSE,
                     quote = "",
                     row.names = NULL, 
                     comment.char = "",
                     sep = "\t",
                     colClasses = colClasses, 
                     fill = FALSE)
    row.descriptions <- as.character(x[, 2])
    data <- as.matrix (x[seq (from = 3, to = dim (x)[2], by = 1)])
    column.names <- column.names[!is.na (column.names)]
    colnames (data) <- column.names
    row.names (data) <- x[, 1]
    list (row.descriptions = row.descriptions,
          data = data)
}
