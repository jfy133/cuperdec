#' Load OTU table
#'
#' This loads a typical taxa table (Samples: columns; Taxa: rows) in TSV format
#' and standardises some columns, storing the table in the form of a tibble.
#'
#' @param x a TSV file or tidy data frame
#'
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
      "[cuperdec] error: your taxa table requires a minimum of 2 columns only: Taxon, Sample_1"
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
#' This loads a taxon / isolation source database file i.e. first column is
#' a list of taxa, and the second column is a list of isolation sources.
#'
#' Taxon names should match that with the taxa table.
#'
#' @param x a two column TSV file
#' @param target the string in the 'Isolation Source' (i.e. 2nd) column which is the expected target source of the samples
#'
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
      "[cuperdec] error: your isolation source database requires two columns only: Sample, Isolation Source."
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
      "[cuperdec] error: your supplied target source was not found in your isolation source column!"
    )
  }

  return(result)
}

#' Load metadata table
#'
#' This loads a metadata table. This needs to include at minimum two columns.
#' Note that the 'source' column should indicate
#'
#' 1. Sample name - a unique identifier for each sample
#' 2. Sample source - a grouping ID indicating what 'source' the sample is from. This is used for plotting to separate comparative 'sources' to your own samples.
#'
#' @param x a file
#' @param sample_col a column name specifying which column should be used to specify sample names
#' @param source_col a column name specifying which group or the source the sample is from
#'
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
      "[cuperdec] error: your metadata database a minimum of two columns: Sample, Sample Source."
    )
  }

  if (!sample_col %in% colnames(input_table)) {
    stop("[cuperdec] error: your requested sample name column is not found in the dataframe!")
  }

  if (!source_col %in% colnames(input_table)) {
    stop("[cuperdec] error: your requested sample source column is not found in the dataframe!")
  }

  ## Processing
  input_table %>%
    dplyr::rename(Sample = sample_col, Sample_Source = source_col) %>%
    dplyr::select(.data$Sample, .data$Sample_Source)
}
