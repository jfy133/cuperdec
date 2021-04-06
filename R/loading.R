#' Load OTU table
#'
#' Loads a typical taxa table (Samples: columns; Taxa: rows) in TSV format
#' and standardises some columns, storing the table in the form of a tibble.
#'
#' @param x Path to a TSV file or tidy dataframe (e.g. tibble) consisting of an
#'   OTU table of samples as columns, except first column with taxon names.
#'
#' @return A tibble, formatted for use in downstream cuperdec functions.
#'
#' @examples
#' data(cuperdec_taxatable_ex)
#' taxa_table <- load_taxa_table(cuperdec_taxatable_ex)
#' @export
load_taxa_table <- function(x) {
  ## Validation
  if (is.data.frame(x)) {
    input_table <- x
  } else {
    input_table <- readr::read_tsv(x, col_types = readr::cols())
  }

  if (ncol(input_table) < 2) {
    stop(
      "[cuperdec] error: your taxa table requires a minimum of 2 columns only:
      Taxon, Sample_1"
    )
  }

  ## Processing
  input_table %>%
    dplyr::rename(Taxon = 1) %>%
    tidyr::pivot_longer(
      data = .,
      names_to = "Sample",
      values_to = "Count",
      cols = -dplyr::matches("Taxon")
    ) %>%
    dplyr::filter(.data$Count != 0)
}

#' Load database
#'
#' Loads a taxon/isolation source database file, i.e. first column is
#' a list of taxa, and the second column is a list of isolation sources, and
#' formats for downstream analysis.
#'
#' Taxon names should match that with the taxa table.
#'
#' @param x Path to a (minimum) two column TSV file or tidy dataframe (e.g.
#'  tibble), one column with taxon names and other indicating if from
#'  target isolation source.
#' @param target the string in the 'Isolation Source' (i.e. 2nd) column which
#'   is the expected target source of the samples
#'
#' @return A tibble, formatted for use in downstream cuperdec functions.
#'
#' @examples
#' data(cuperdec_database_ex)
#' iso_database <- load_database(cuperdec_database_ex, target = "oral")
#' @export
load_database <- function(x, target) {
  ## Validation
  if (is.data.frame(x)) {
    input_table <- x
  } else {
    input_table <- readr::read_tsv(x, col_types = readr::cols())
  }

  if (ncol(input_table) != 2) {
    stop(
      "[cuperdec] error: your isolation source database requires two columns
      only: Sample, Isolation Source."
    )
  }

  ## Processing
  result <- input_table %>%
    dplyr::rename(Taxon = 1, Isolation_Source = 2) %>%
    dplyr::mutate(
      Isolation_Source = dplyr::case_when(
        .data$Isolation_Source != target ~ FALSE,
        .data$Isolation_Source == target ~ TRUE,
        TRUE ~ FALSE
      )
    )

  if (!any(result$Isolation_Source)) {
    stop(
      "[cuperdec] error: your supplied target source was not found in your
      isolation source column!"
    )
  }

  return(result)
}

#' Load metadata table
#'
#' Loads a metadata table and reformats it for downstream analysis. This needs
#' to include at minimum two columns: sample name, and sample source.
#'
#' The two columns required need to include the following information:
#' \itemize{
#'   \item{Sample name - a unique identifier for each sample}
#'   \item{Sample source - a grouping ID indicating what 'source' the sample is
#'   from This is used for plotting to separate comparative 'sources' to your
#'   own samples.}
#' }
#'
#' @param x Path to a TSV file or tidy dataframe (e.g. tibble) with a column
#'   containing sample names and other grouping metadata columns.
#' @param sample_col A column name specifying which column should be used to
#'   specify sample names.
#' @param source_col A column name specifying which group or the source the
#'   sample is from.
#'
#' @return A tibble, formatted for use in downstream cuperdec functions.
#'
#' @examples
#' data(cuperdec_metadata_ex)
#' metadata_table <- load_map(cuperdec_metadata_ex,
#'   sample_col = "#SampleID",
#'   source_col = "Env"
#' )
#' @export
load_map <- function(x, sample_col, source_col) {
  ## Validation
  if (is.data.frame(x)) {
    input_table <- x
  } else {
    input_table <- readr::read_tsv(x, col_types = readr::cols())
  }

  if (ncol(input_table) < 2) {
    stop(
      "[cuperdec] error: your metadata database a minimum of two columns:
      Sample, Sample Source."
    )
  }

  if (!sample_col %in% colnames(input_table)) {
    stop("[cuperdec] error: your requested sample name column is not found in
         the dataframe!")
  }

  if (!source_col %in% colnames(input_table)) {
    stop("[cuperdec] error: your requested sample source column is not found in
         the dataframe!")
  }

  ## Processing
  input_table %>%
    dplyr::rename(Sample = sample_col, Sample_Source = source_col) %>%
    dplyr::select(.data$Sample, .data$Sample_Source)
}
