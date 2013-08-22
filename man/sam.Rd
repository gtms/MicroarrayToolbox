\name{sam}
\alias{sam}
\title{
sam: wrapper for the SAM function of the 'samr' package
}
\description{
This function is a wrapper for the SAM function of the 'samr' package
that implements the Significant Analysis of Microarrays algorithm,
described in Tusher, V., Tibshirani, R. and Chu, G. (2001):
Significance of microarrays applied to the ionizing radiation response
PNAS 2001 98:5116-5121, (Apr 24). The expression data should be
provided in the gct format.
}
\usage{
sam(gct, class.id = NULL, resp.type = c("Quantitative", "Two class unpaired",
"Survival", "Multiclass", "One class", "Two class paired", "Two class unpaired
timecourse", "One class timecourse", "Two class paired timecourse", "Pattern
discovery"), testStatistic = "standard", nperms = 500, logged2 = TRUE)
}
\arguments{
  \item{gct}{
a gct object, see details
}
  \item{class.id}{
logical vector of length equal to the number of samples in the gct,
specifying the eventual the classes for Two Class analysis: TRUE for
class one and FALSE for class two. Defaults to NULL
}
  \item{resp.type}{
problem type: "Quantitative" for a continuous parameter; "Two class unpaired";
          "Survival" for censored survival outcome; "Multiclass": more than 2
          groups; "One class" for a single group; "Two class paired" for two
          classes with paired observations; "Two class unpaired timecourse",
          "One class time course", "Two class.paired timecourse", "Pattern
          discovery". Refer to the 'samr' package for more details.
}
  \item{testStatistic}{
test statistic to use in two class unpaired case. Either "standard"
          (t-statistic) or "wilcoxon" (Two-sample wilcoxon or Mann-Whitney
          test). Only used for array data. Refer to the 'samr' package for more
          details
}
  \item{nperms}{
number of permutations used to estimate false discovery rates. Refer to the
	  'samr' package for more details. Defaults to 500
}
  \item{logged2}{
  has the data been transformed by log (base 2)? This information is
  used only for computing fold changes. Refer to the 'samr' package
  for more details. Defaults to TRUE
}
}
\details{
The samr package is authored by R. Tibshirani, G. Chu, Balasubramanian
Narasimhan and Jun Li

Maintainer: Rob Tibshirani <tibs at stanford.edu>

A gct object is a list composed of the following objects:
\itemize{
  \item{row.descriptions: }{character vector of length equal to the first
dimension of the data matrix, specifying the symbols of the genes targeted by
each feature in the array}
  \item{data: }{matrix containing the expression data. Each row corresponds to a
feature and each column to a sample. Rownames and colnames specify,
respectively, the unique manufacturer's feature-specific identifiers and the
unique sample identifiers}
}
}
\value{
A list composed by the following elements:
\itemize{
	\item{class.id: }{the class.id argument provided as input}
	\item{nperms: }{the nperms argument provided as input}
	\item{resp.type: }{the resp.type argument provided as input}
	\item{genes.up: }{a data table with a list of up-regulated genes
	ranked by increasing q-value and decreasing statistic score}
	\item{genes.dw: }{a data table with a list of down-regulated genes
	ranked by increasing q-value and decreasing statistic score}
}
}
\references{
Tusher, V., Tibshirani, R. and Chu, G. (2001): Significance of
microarrays applied to the ionizing radiation response PNAS 2001
98:5116-5121, (Apr 24)

More details about the gct format here:
\url{
http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/gct
}
}
\author{
Gil Tomas
\email{gil.tomas@ulb.ac.be}
}