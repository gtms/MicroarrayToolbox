\name{getGO}
\alias{getGO}
\title{
getGO: retrieves the Gene Ontology terms associated to a string of
gene symbols
}
\description{
Given a character vector of gene symbols OR a numeric vector of Entrez
IDs, returns a data table, with the following variables: genes
queried; Gene Ontology identifiers; Gene Ontology evidence codes; Gene
Ontology domain terms and Gene Ontology terms.
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
If path is NULL, no file will be written on disk.
}
\value{
A data table with the following variables:
  \item{gene}{factor of the queried genes symbols}
  \item{go}{character vector with each of the retrieved Gene Ontology identifiers}
  \item{go.evidence}{factor of the retrieved Gene Ontology evidence
  codes (cf. http://www.geneontology.org/GO.evidence.shtml)}
  \item{go.ontology}{factor of the retrieved Gene Ontology domains
  terms (cf. http://www.geneontology.org/GO.ontology.structure.shtml)}
  \item{term}{factor of the Gene Ontology terms}
}
\references{
See:
\url{
http://www.geneontology.org/
}
}