\name{getGO}
\alias{getGO}
\title{
getGO: retrieves the Gene Ontology terms associated to a string of
gene symbols
}
\description{
Given a character vector of gene symbols OR a numeric vector of entrez
gene IDs, returns a data table with the following variables: entrez
gene IDs of queried genes; gene symbols queried; Gene Ontology
identifiers; Gene Ontology evidence codes; Gene Ontology domain terms
and Gene Ontology terms.
}
\usage{
getGO(vec, is.symbol = TRUE, path = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
character vector of gene symbols OR numeric vector of Entrez gene IDs
}
  \item{is.symbol}{
logical specifying whether the input 'vec' argument refers to gene
symbols or Entrez gene IDs. Deafults to TRUE
}
  \item{path}{
character vector specifying the eventual path on disk where to write
the csv file of the output table. Defaults to NULL
}
}
\details{
The argument 'path' should be a character string concatenating the
path and the name of the file to be written on disk. If 'path' is NULL,
no file will be written on disk.
}
\value{
A data table with the following variables:
  \item{gene.id}{character vector of the entrez gene IDs of the
  queried genes}
  \item{symbol}{character vector of the gene symbols of the queried
  genes}
  \item{go.id}{character vector of the retrieved Gene Ontology identifiers}
  \item{evidence}{character vector of the retrieved Gene Ontology evidence
  codes (\url{http://www.geneontology.org/GO.evidence.shtml})}
  \item{ontology}{character vector of the retrieved Gene Ontology domain
  terms (\url{http://www.geneontology.org/GO.ontology.structure.shtml})}
  \item{term}{character vector of the retrieved Gene Ontology terms}
}
\author{
Gil Tomas
\email{gil.tomas@ulb.ac.be}
}