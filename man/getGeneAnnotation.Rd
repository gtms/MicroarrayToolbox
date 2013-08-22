\name{getGeneAnnotation}
\alias{getGeneAnnotation}
\title{
getGeneAnnotation: retrieves relevant gene annotation information
associated to a string of Affymetrix probesets identifiers
}
\description{
Given a character vector of Affymetrix identifiers, returns a data table with
the following variables: original Affymetrix identifiers followed by the
corresponding gene symbols, gene names, entrez IDs, Gene Bank accession numbers
(ACCNUM) and probesets' genomic locations.
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
\author{
Gil Tomas
\email{gil.tomas@ulb.ac.be}
}