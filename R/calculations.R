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

#' Apply simple filter
#'
#' Performs the initial decay curve based on percentage of 'target' isolation
#' source along a rank of most to least abundant taxa for a given sample.
#'
#' @param table a OTU table loaded with `load_taxa_table()`
#' @param threshold a database file loaded with `load_database()`
#'
#' @importFrom dplyr row_number
#'
#' @export
simple_filter <- function(table, threshold){
  table %>%
    dplyr::mutate(Pass = .data$Fraction_Target > threshold) %>%
    dplyr::summarise(Passed = any(.data$Pass))
}

#' Calculate hard burnin retain/discard list
#'
#' Returns a table of whether each sample passess a given threshold, after
#' considering a 'burn-in', in the form of a fraction of the abundance ranks
#'
#' @param table a tibble from `calculate_curve()`
#' @param threshold a percentage of the target-source in a sample above which a sample is considered 'retained'
#' @param burnin a number betwen 0 and 1 indicating the fraction of taxa to ignore before applying the threshold
#'
#' @export

hard_burnin_filter <- function(table, threshold, burnin) {

  n_taxa <- table %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(N_Taxa = dplyr::n()) %>%
    dplyr::mutate(Start = .data$N_Taxa * burnin)

  ## TODO: Ugly as shouldn't need the duiplicated values for joining but will
  ## keep now until think of more elegent solution
  table %>%
    dplyr::left_join(n_taxa, by = "Sample") %>%
    dplyr::mutate(Pass = .data$Start > .data$Rank && .data$Fraction_Target > threshold) %>%
    dplyr::summarise(Passed = any(.data$Pass))

}

#' Calculate adaptive burnin retain/discard list
#'
#' This function automates a selection of a per-sample 'burn in' based on the
#' nature of the sample's curve itself (rather than supplying a hard value) by
#' finding the point from which the 'fluctuation' of the curve doesn't exceed
#' the mean +- SD of the total curve.
#'
#' @param table a tibble from `calculate_curve()`
#' @param threshold a percentage of the target-source in a sample above which a sample is considered 'retained'
#'
#' @export

adaptive_burnin_filter <- function(table, threshold){

  ## Calculate per-sampler the mean +- SD of
  ## stepwise-rank-percentage-target-differences, with + as upper limit and -
  ## as lower limit
  fluctuations <- table %>%
    dplyr::mutate(Fluctuation = dplyr::lag(Fraction_Target,
                                           1,
                                           default = 0) - Fraction_Target) %>%
    dplyr::select(Sample, Fluctuation) %>%
    dplyr::group_by(Sample) %>%
    dplyr::summarise(mean(Fluctuation),
              sd(Fluctuation),
              min(Fluctuation),
              max(Fluctuation)) %>%
    dplyr::mutate(Upper_Limit = `mean(Fluctuation)` + `sd(Fluctuation)`,
           Lower_Limit = `mean(Fluctuation)` - `sd(Fluctuation)`) %>%
    dplyr::select(Sample, Upper_Limit, Lower_Limit)



   limits <- left_join(table, fluctuations) %>%
     dplyr::mutate(Exceed_Limits = if_else(Fluctuation < Upper_Limit &
                                            Fluctuation > Lower_Limit,
                                          F,
                                          T)) %>%
     dplyr::filter(Exceed_Limits) %>%
     dplyr::slice(dplyr::n()) %>%
     dplyr::select(Sample, Abundance_Rank) %>%
     dplyr::rename(Within_Limits = Abundance_Rank) %>%
     dplyr::filter(Within_Limits)

  ## TODO Finish calculations

}
