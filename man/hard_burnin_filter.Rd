% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculations.R
\name{hard_burnin_filter}
\alias{hard_burnin_filter}
\title{Calculate hard burn-in retain/discard list}
\usage{
hard_burnin_filter(curves, percent_threshold, rank_burnin)
}
\arguments{
\item{curves}{A cuperdec curve table calculated with
\code{\link{calculate_curve}}.}

\item{percent_threshold}{A percentage of the target-source in a sample above
which a sample is considered 'retained'.}

\item{rank_burnin}{A number between 0 and 1 indicating the fraction of taxa
to ignore before applying the threshold.}
}
\value{
A tibble with each row showing each sample and whether it
  passed the specified filter.
}
\description{
Returns a table of whether each sample passes a given threshold, after
considering a 'burn-in', in the form of a fraction of the abundance ranks.
}
\examples{
data(cuperdec_taxatable_ex)
data(cuperdec_database_ex)

taxa_table <- load_taxa_table(cuperdec_taxatable_ex)
iso_database <- load_database(cuperdec_database_ex, target = "oral")

curve_results <- calculate_curve(taxa_table, iso_database)
hard_burnin_filter(curve_results, percent_threshold = 50, rank_burnin = 0.1)
}
