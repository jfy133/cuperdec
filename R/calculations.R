#' Calculate curve
#'
#' Performs the initial decay curve based on percentage of 'target' isolation
#' source along a rank of most to least abundant taxa for a given sample.
#'
#' @param table a OTU table loaded with `load_taxa_table()`
#' @param database a database file loaded with `load_database()`
#'
#' @importFrom dplyr row_number
#'
#' @export
calculate_curve <- function(table, database){

  dplyr::left_join(table, database, by = c("Taxon")) %>%
    dplyr::arrange(.data$Sample, dplyr::desc(.data$Count)) %>%
    tidyr::replace_na(list(Isolation_Source = FALSE)) %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::mutate(Cumulative_Sum = cumsum(.data$Isolation_Source)) %>%
    dplyr::mutate(Fraction_Target = (.data$Cumulative_Sum / .data$Rank) * 100) %>%
    dplyr::select(.data$Sample, .data$Taxon, .data$Rank, .data$Fraction_Target)

}

#' goes_below
#'
#' detects whether after the first X fraction of elements in a vector, if a value
#'
#' @param value
#' @param vector
#'
#' @importFrom dplyr row_number
#'
#' @export
below <- function(table, database){



}

#' Calculate burnin filter
#'
#' @param table a tibble from `calculate_curve()`
#' @param threshold a percentage of the target-source in a sample above which a sample is considered 'retained'
#' @param burnin a number betwen 0 and 1 indicating the fraction of taxa to ignore before applying the threshold
#'
#' @export

apply_burnin <- function(table, threshold, burnin) {

  n_taxa <- table %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(N_Taxa = dplyr::n()) %>%
    dplyr::mutate(Start = .data$N_Taxa * burnin)

  ## TODO: Ugly as shouldn't need the duiplicated values for joining but will
  ## keep now until think of more elegent solution
  table %>%
    dplyr::left_join(n_taxa, by = "Sample") %>%
    dplyr::mutate(Pass = Start > Rank && Fraction_Target > threshold) %>%
    dplyr::summarise(Passed = any(.data$Pass))


}


#' Calculate adaptive burnin
#'
#'
#' @param x a TSV file
#'
#' @export
