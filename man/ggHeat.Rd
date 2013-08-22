\name{ggHeat}
\alias{ggHeat}
\title{
ggHeat: draws an heatmap from a matrix
}
\description{
Given a matrix, plots a ggplot representing an heatmap of the matrix on a pdf
device.  Variable 'do.reorder' is a logical controlling whether the variables
are reordered according to their arithmetic means.
}
\usage{
ggHeat(mtx, do.reorder = TRUE, title = "Heatmap", path = NULL, width = 10, height = 7, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mtx}{
a matrix representing the data to plot, with observations as rows and variables
as columns
}
  \item{do.reorder}{
logical specifying whether the observations and variables should be reordered by
decreasing mean. Defaults to TRUE 
}
  \item{title}{
character vector specifying the title of the plot
}
  \item{path}{
character vector specifying the eventual path on disk where to write the output
pdf file (the complete file path is a concatenate of this variable and the title variable)
}
  \item{width}{
graphical parameter specifying the width of the output pdf file
}
  \item{height}{
graphical parameter specifying the height of the output pdf file
}
  \item{\dots}{
arguments to be passed to methods, such as graphical parameters
}
}
\author{
Gil Tomas
\email{gil.tomas@ulb.ac.be}
}