\name{getGeneAnnotation}
\alias{getGeneAnnotation}
\title{
getGeneAnnotation: retrieves relevant gene annotation information
associated to a string of Affymetrix probesets identifiers
}
\description{
given a character vector of Affymetrix identifiers, returns a data table,
not keyed, with the following variables:
vendor.id: character vector of the provided Affymetrix identifiers
symbol: character vector of the corresponding gene symbols
gene.name: character vector of the corresponding gene names
entrez.id: numeric vector of the corresponding NCBI entrez gene IDs
ACCNUM: character vector of the corresponding GeneBank accesion numbers
map: character vector of the corresponding probesets' genomic locations
}
\usage{
getGeneAnnotation(affy.ids, annotation.pkg)
}
\arguments{
  \item{affy.ids}{
character vector of Affymetrix probesets identifiers
}
  \item{annotation.pkg}{
character vector specifying the Bioconductor annotation package to
retrieve the gene annotation data. See details for more information
}
}
\details{
Argument 'annotation.pkg' must specify the bioconductor annotation
package from which to extract the data, without the '.db' extension.
}
\value{
A data table with the following variables:
  \item{vendor.id}{character vector of the provided Affymetrix identifiers}
  \item{symbol}{character vector of the corresponding gene symbols}
  \item{gene.name}{character vector of the corresponding gene names}
  \item{entrez.id}{numeric vector of the corresponding NCBI entrez gene IDs}
  \item{ACCNUM}{character vector of the corresponding GeneBank
  accesion numbers}
  \item{map}{character vector of the corresponding probesets' genomic locations}
}
\references{
A list of bioconductor annotation packages can found here:
\url{
http://www.bioconductor.org/packages/release/data/annotation/
}
}