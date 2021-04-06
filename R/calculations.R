#' Calculate cumulative decay percent curve
#'
#' Performs the initial decay curve based on percentage of 'target' isolation
#' source along a rank of most to least abundant taxa for a given sample.
#'
#' @param taxa_table An OTU table loaded with \code{\link{load_taxa_table}}.
#' @param database A database file loaded with \code{\link{load_database}}.
#'
#' @return An object in the form of a tibble with taxa of each given sample
#'   ordered by rank and the proportion of taxa up to that rank deriving
#'   from your target source.
#'
#' @importFrom dplyr row_number
#'
#' @examples
#' data(cuperdec_taxatable_ex)
#' data(cuperdec_database_ex)
#'
#' taxa_table <- load_taxa_table(cuperdec_taxatable_ex)
#' iso_database <- load_database(cuperdec_database_ex, target = "oral")
#'
#' calculate_curve(taxa_table, iso_database)
#' @export
calculate_curve <- function(taxa_table, database) {
  ## Validation
  validate_taxatable(taxa_table)
  validate_database(database)

  ## Calculation
  dplyr::left_join(taxa_table, database, by = c("Taxon")) %>%
    dplyr::arrange(.data$Sample, dplyr::desc(.data$Count)) %>%
    tidyr::replace_na(list(Isolation_Source = FALSE)) %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::mutate(Cumulative_Sum = cumsum(.data$Isolation_Source)) %>%
    dplyr::mutate(Fraction_Target = (.data$Cumulative_Sum / .data$Rank) *
      100) %>%
    dplyr::select(
      .data$Sample,
      .data$Taxon,
      .data$Rank,
      .data$Fraction_Target
    )
}

#' Apply simple percentage filter
#'
#' Performs the initial decay curve based on percentage of 'target' isolation
#' source along a rank of most to least abundant taxa for a given sample.
#'
#' @param curves A cuperdec curve table calculated with
#'   \code{\link{calculate_curve}}.
#' @param percent_threshold A database file loaded with
#'   \code{\link{load_database}}.
#'
#' @return A tibble with each row showing each sample and whether it
#'   passed the specified filter.
#'
#' @importFrom dplyr row_number
#'
#' @examples
#' data(cuperdec_taxatable_ex)
#' data(cuperdec_database_ex)
#'
#' taxa_table <- load_taxa_table(cuperdec_taxatable_ex)
#' iso_database <- load_database(cuperdec_database_ex, target = "oral")
#'
#' curve_results <- calculate_curve(taxa_table, iso_database)
#' simple_filter(curve_results, percent_threshold = 50)
#' @export

simple_filter <- function(curves, percent_threshold) {
  ## Validation
  validate_curves(curves)

  if (!is.numeric(percent_threshold)) {
    stop("[cuperdec] error: percent_threshold must be numeric.")
  }

  ## Calculation
  curves %>%
    dplyr::mutate(Pass = .data$Fraction_Target > percent_threshold) %>%
    dplyr::summarise(Passed = any(.data$Pass))
}

#' Calculate hard burn-in retain/discard list
#'
#' Returns a table of whether each sample passes a given threshold, after
#' considering a 'burn-in', in the form of a fraction of the abundance ranks.
#'
#' @param curves A cuperdec curve table calculated with
#'   \code{\link{calculate_curve}}.
#' @param percent_threshold A percentage of the target-source in a sample above
#'   which a sample is considered 'retained'.
#' @param rank_burnin A number between 0 and 1 indicating the fraction of taxa
#'   to ignore before applying the threshold.
#'
#' @return A tibble with each row showing each sample and whether it
#'   passed the specified filter.
#'
#' @examples
#' data(cuperdec_taxatable_ex)
#' data(cuperdec_database_ex)
#'
#' taxa_table <- load_taxa_table(cuperdec_taxatable_ex)
#' iso_database <- load_database(cuperdec_database_ex, target = "oral")
#'
#' curve_results <- calculate_curve(taxa_table, iso_database)
#' hard_burnin_filter(curve_results, percent_threshold = 50, rank_burnin = 0.1)
#' @export

hard_burnin_filter <-
  function(curves, percent_threshold, rank_burnin) {
    ## Validation
    validate_curves(curves)

    if (!is.numeric(percent_threshold)) {
      stop("[cuperdec] error: percent_threshold must be numeric.")
    }

    if ((!is.numeric(rank_burnin) ||
      rank_burnin >= 1) || rank_burnin == 0) {
      stop("[cuperdec] error: rank_burnin must be a decimal number less than 1
           and more than 0")
    }

    ## Calculation
    n_taxa <- curves %>%
      dplyr::group_by(.data$Sample) %>%
      dplyr::summarise(N_Taxa = dplyr::n()) %>%
      dplyr::mutate(Start = .data$N_Taxa * rank_burnin)

    ## Note: This is ugly as shouldn't need the duplicated values for joining
    ## but will keep for now until think of more elegent solution
    curves %>%
      dplyr::left_join(n_taxa, by = "Sample") %>%
      dplyr::mutate(Pass = .data$Rank > .data$Start &
        .data$Fraction_Target > percent_threshold) %>%
      dplyr::summarise(Passed = any(.data$Pass))
  }

#' Calculate adaptive burn-in retain/discard list
#'
#' Automates a selection of a per-sample 'burn in' based on the
#' nature of the sample's curve itself (rather than supplying a hard value) by
#' finding the point from which the 'fluctuation' of the curve doesn't exceed
#' the mean +- SD of the total curve.
#'
#' @param curves A cuperdec curve table calculated with
#'   \code{\link{calculate_curve}}.
#' @param percent_threshold A percentage of the target-source in a sample above
#'   which a sample is considered 'retained'.
#'
#' @return A tibble with each row showing each sample and whether it
#'   passed the specified filter.
#'
#' @importFrom stats sd
#'
#' @examples
#' data(cuperdec_taxatable_ex)
#' data(cuperdec_database_ex)
#'
#' taxa_table <- load_taxa_table(cuperdec_taxatable_ex)
#' iso_database <- load_database(cuperdec_database_ex, target = "oral")
#'
#' curve_results <- calculate_curve(taxa_table, iso_database)
#' adaptive_burnin_filter(curve_results, percent_threshold = 0.1)
#' @export

adaptive_burnin_filter <- function(curves, percent_threshold) {
  ## Validation
  validate_curves(curves)

  if (!is.numeric(percent_threshold)) {
    stop("[cuperdec] error: percent_threshold must be numeric.")
  }

  ## Calculation

  ## Find differences in percentage between each stepwise of rank
  table_fluc <- curves %>%
    dplyr::mutate(Fluctuation = dplyr::lag(.data$Fraction_Target,
      1,
      default = 0
    ) - .data$Fraction_Target)

  ## Calculate per-sample the mean +- SD of
  ## stepwise-rank-percentage-target-differences, with + as upper limit and -
  ## as lower limit
  limits <- table_fluc %>%
    dplyr::select(.data$Sample, .data$Fluctuation) %>%
    dplyr::group_by(.data$Sample) %>%
    dplyr::summarise(
      mean(.data$Fluctuation),
      sd(.data$Fluctuation),
      min(.data$Fluctuation),
      max(.data$Fluctuation)
    ) %>%
    dplyr::mutate(
      Upper_Limit = .data$`mean(.data$Fluctuation)` +
        .data$`sd(.data$Fluctuation)`,
      Lower_Limit = .data$`mean(.data$Fluctuation)` -
        .data$`sd(.data$Fluctuation)`
    ) %>%
    dplyr::select(.data$Sample, .data$Upper_Limit, .data$Lower_Limit)


  ## Find the position from which the difference stops exceeding the rank limits
  burnin_rank <-
    dplyr::left_join(table_fluc, limits, by = c("Sample")) %>%
    dplyr::mutate(
      Exceed_Limits = dplyr::if_else(
        .data$Fluctuation < .data$Upper_Limit &
          .data$Fluctuation > .data$Lower_Limit,
        F,
        T
      )
    ) %>%
    dplyr::filter(.data$Exceed_Limits) %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::select(.data$Sample, .data$Rank) %>%
    dplyr::rename(Within_Limits = .data$Rank)

  ## Find whether sample exceeds the user specified percentage target source,
  ## after defined burn-in rank
  curves %>%
    dplyr::left_join(burnin_rank, by = c("Sample")) %>%
    dplyr::mutate(Pass = .data$Rank > .data$Within_Limits + 1 &
      .data$Fraction_Target > percent_threshold) %>%
    dplyr::summarise(Passed = any(.data$Pass))
}
