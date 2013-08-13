## checkExtension
## 06Aug2013
##
## This function is taken from the package GenePattern, available from
## http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/programmers/sections/gp_r
##
## given a file.name and an extension, checks for proper use of the extension in the file.name

checkExtension <- function (file.name,
                             extension) {
  ext <- regexpr (paste (extension, "$", sep = ""),
                  tolower (file.name))
  if (ext[[1]] == -1) {
    file.name <- paste (file.name, extension, sep = "")
  }
  file.name
}
