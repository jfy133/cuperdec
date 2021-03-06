% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cuperdec-data.R
\docType{data}
\name{cuperdec_metadata_ex}
\alias{cuperdec_metadata_ex}
\title{Example metadata file input for cuperdec}
\format{
An TSV table loaded as a \code{tibble}.
}
\source{
\doi{10.5281/zenodo.3740492}
}
\usage{
data(cuperdec_metadata_ex)
}
\description{
Example metadata map file corresponding to samples
in example data "cuperdec_taxatable_ex". Includes
a grouping column corresponding to sample species.
}
\examples{

data(cuperdec_metadata_ex)
load_map(cuperdec_metadata_ex, sample_col = "#SampleID", source_col = "Env")
}
\keyword{datasets}
