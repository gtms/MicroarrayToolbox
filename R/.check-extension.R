checkExtension <- function (file.name,
                             extension) {
  ext <- regexpr (paste (extension, "$", sep = ""),
                  tolower (file.name))
  if (ext[[1]] == -1) {
    file.name <- paste (file.name, extension, sep = "")
  }
  file.name
}
