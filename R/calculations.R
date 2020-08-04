#' Calculate curve
#'
#' Performs the initial decay curve based on percentage of 'target' isolation
#' source along a rank of most to least abundant taxa for a given sample.
#'
#' @param taxa_table a OTU table loaded with `load_taxa_table()`
#' @param database a database file loaded with `load_database()`
#'
#' @importFrom dplyr row_number
#'
#' @export
calculate_curve <- function(taxa_table, database) {

  if ( nrow(taxa_table) == 0 )
    stop("[cuperdec] error: your (long format-) taxa table has no rows! Has it been converted correctly?")

  if ( nrow(database) == 0 )
    stop("[cuperdec] error: your database table has no rows! Has it been converted correctly?")

  if ( ncol(taxa_table) < 3 || !any( c("Taxon", "Sample", "count") %in% colnames(taxa_table) ) )
    stop("[cuperdec] error: your (long format-) taxa table requires a minimum of 3 columns: Taxon, Sample, Count")

  if ( ncol(taxa_table) < 2 || !any( c("Taxon", "Isolation_Source") %in% colnames(taxa_table) ) )
    stop("[cuperdec] error: your database table requires a minimum of 2 columns: Taxon, Isolation_Source")

  if (database$Isolation_Source )

  dplyr::left_join(taxa_table, database, by = c("Taxon")) %>%
    dplyr::arrange(.data$Sample, dplyr::desc(.data$Count)) %>%
    tidyr::replace_na(list(Isolation_Source = FALSE)) %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::mutate(Cumulative_Sum = cumsum(.data$Isolation_Source)) %>%
    dplyr::mutate(Fraction_Target = (.data$Cumulative_Sum / .data$Rank) * 100) %>%
    dplyr::select(.data$Sample, .data$Taxon, .data$Rank, .data$Fraction_Target)

}

#' Apply simple filter
#'
#' Performs the initial decay curve based on percentage of 'target' isolation
#' source along a rank of most to least abundant taxa for a given sample.
#'
#' @param taxa_table a OTU table loaded with `load_taxa_table()`
#' @param percent_threshold a database file loaded with `load_database()`
#'
#' @importFrom dplyr row_number
#'
#' @export
simple_filter <- function(taxa_table, percent_threshold) {
  taxa_table %>%
    dplyr::mutate(Pass = .data$Fraction_Target > percent_threshold) %>%
    dplyr::summarise(Passed = any(.data$Pass))
}

#' Calculate hard burnin retain/discard list
#'
#' Returns a table of whether each sample passess a given threshold, after
#' considering a 'burn-in', in the form of a fraction of the abundance ranks
#'
#' @param taxa_table a tibble from `calculate_curve()`
#' @param percent_threshold a percentage of the target-source in a sample above which a sample is considered 'retained'
#' @param rank_burnin a number betwen 0 and 1 indicating the fraction of taxa to ignore before applying the threshold
#'
#' @export

hard_burnin_filter <- function(taxa_table, percent_threshold, rank_burnin) {

  n_taxa <- taxa_table %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(N_Taxa = dplyr::n()) %>%
    dplyr::mutate(Start = .data$N_Taxa * rank_burnin)

  ## TODO: Ugly as shouldn't need the duiplicated values for joining but will
  ## keep now until think of more elegent solution
  taxa_table %>%
    dplyr::left_join(n_taxa, by = "Sample") %>%
    dplyr::mutate(Pass = .data$Start > .data$Rank & .data$Fraction_Target > percent_threshold) %>%
    dplyr::summarise(Passed = any(.data$Pass))

}

#' Calculate adaptive burnin retain/discard list
#'
#' This function automates a selection of a per-sample 'burn in' based on the
#' nature of the sample's curve itself (rather than supplying a hard value) by
#' finding the point from which the 'fluctuation' of the curve doesn't exceed
#' the mean +- SD of the total curve.
#'
#' @param taxa_table a tibble from `calculate_curve()`
#' @param percent_threshold a percentage of the target-source in a sample above which a sample is considered 'retained'
#'
#' @importFrom stats sd
#' @export

adaptive_burnin_filter <- function(taxa_table, percent_threshold) {


  ## Find differences in percentage between each stepwise of rank
  table_fluc <- taxa_table %>%
    dplyr::mutate(Fluctuation = dplyr::lag(.data$Fraction_Target,
                                           1,
                                           default = 0) - .data$Fraction_Target)

  ## Calculate per-sample the mean +- SD of
  ## stepwise-rank-percentage-target-differences, with + as upper limit and -
  ## as lower limit
  limits <- table_fluc %>%
    dplyr::select(.data$Sample, .data$Fluctuation) %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(mean(.data$Fluctuation),
              sd(.data$Fluctuation),
              min(.data$Fluctuation),
              max(.data$Fluctuation)) %>%
    dplyr::mutate(Upper_Limit = .data$`mean(.data$Fluctuation)` + .data$`sd(.data$Fluctuation)`,
           Lower_Limit = .data$`mean(.data$Fluctuation)` - .data$`sd(.data$Fluctuation)`) %>%
    dplyr::select(.data$Sample, .data$Upper_Limit, .data$Lower_Limit)


  ## Find the position from which the difference stops exceeding the rank limits
   burnin_rank <- dplyr::left_join(table_fluc, limits, by = c("Sample")) %>%
     dplyr::mutate(Exceed_Limits = dplyr::if_else(.data$Fluctuation < .data$Upper_Limit &
                                             .data$Fluctuation > .data$Lower_Limit,
                                          F,
                                          T)) %>%
     dplyr::filter(.data$Exceed_Limits) %>%
     dplyr::slice(dplyr::n()) %>%
     dplyr::select(.data$Sample, .data$Rank) %>%
     dplyr::rename(Within_Limits = .data$Rank)

   ## Find whether sample exceeds the user specified percentage target source,
   ## after defined burn-in rank
   taxa_table %>%
     dplyr::left_join(burnin_rank, by = c("Sample")) %>%
     dplyr::mutate(Pass = .data$Rank > .data$Within_Limits + 1 & .data$Fraction_Target > percent_threshold) %>%
     dplyr::summarise(Passed = any(.data$Pass))
}
