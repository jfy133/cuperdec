% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculations.R
\name{calculate_curve}
\alias{calculate_curve}
\title{Calculate cumulative decay percent curve}
\usage{
calculate_curve(taxa_table, database)
}
\arguments{
\item{taxa_table}{An OTU table loaded with \code{\link{load_taxa_table}}.}

\item{database}{A database file loaded with \code{\link{load_database}}.}
}
\value{
An object in the form of a tibble with taxa of each given sample
  ordered by rank and the proportion of taxa up to that rank deriving
  from your target source.
}
\description{
Performs the initial decay curve based on percentage of 'target' isolation
source along a rank of most to least abundant taxa for a given sample.
}
\examples{
data(cuperdec_taxatable_ex)
data(cuperdec_database_ex)

taxa_table <- load_taxa_table(cuperdec_taxatable_ex)
iso_database <- load_database(cuperdec_database_ex, target = "oral")

calculate_curve(taxa_table, iso_database)
}
