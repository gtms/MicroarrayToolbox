MicroarrayToolbox
=================


**MicroarrayToolbox: functions for routine microarray analysis**

MicroarrayToolbox is an R package containing functions that provide basic functionality to explore gene expression data. In order to streamline and standardize analyses, microarray data is expected to be provided in the 'gct' format, proposed by the Broad Institute.

## Installation

MicroarrayToolbox requires the prior installation of the following packages:
* data.table (CRAN)
* MASS (CRAN)
* ggplot2 (CRAN)
* annotate (Bioconductor)
* org.Hs.eg.db (Bioconductor)
* GO.db (Bioconductor)
* RColorBrewer (CRAN)
* samr (CRAN)

***

To install MicroarrayToolbox on your system, use the 'devtools' package:

    ## install.packages ("devtools")
    library (devtools)
    install_github ("MicroarrayToolbox", user = "gtms")
