#' Example taxon table input for cuperdec
#'
#' Example taxon table used for input to cuperdec based
#' on data including shotgun-sequenced ancient calculus
#' samples aligned against the NCBI Nt database from Oct
#' 2017 using MALT. Samples are columns, rows are taxa
#' and counts are assigned reads.
#'
#' @docType data
#'
#' @usage data(cuperdec_taxatable_ex)
#'
#' @format An TSV table loaded as a \code{tibble}.
#'
#' @keywords datasets
#'
#' @source \doi{10.5281/zenodo.3740492}
#'
#' @examples
#'
#' data(cuperdec_taxatable_ex)
#' load_taxa_table(cuperdec_taxatable_ex)
"cuperdec_taxatable_ex"

#' Example isolation source database input for cuperdec
#'
#' Example isolation source database used for input to
#' cuperdec based. Species names are from a NCBI Nt database
#' and isolation sources gather from the Human Oral Microbiome
#' database, NCBI GenBank, and manual curation.
#'
#' @docType data
#'
#' @usage data(cuperdec_database_ex)
#'
#' @format An TSV table loaded as a \code{tibble}.
#'
#' @keywords datasets
#'
#' @source \doi{10.5281/zenodo.3740492}
#'
#' @examples
#'
#' data(cuperdec_database_ex)
#' load_database(cuperdec_database_ex, target = "oral")
"cuperdec_database_ex"

#' Example metadata file input for cuperdec
#'
#' Example metadata map file corresponding to samples
#' in example data "cuperdec_taxatable_ex". Includes
#' a grouping column corresponding to sample species.
#'
#' @docType data
#'
#' @usage data(cuperdec_metadata_ex)
#'
#' @format An TSV table loaded as a \code{tibble}.
#'
#' @keywords datasets
#'
#' @source \doi{10.5281/zenodo.3740492}
#'
#' @examples
#'
#' data(cuperdec_metadata_ex)
#' load_map(cuperdec_metadata_ex, sample_col = "#SampleID", source_col = "Env")
"cuperdec_metadata_ex"
