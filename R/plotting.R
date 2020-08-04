#' Plot cumulative percent decay curves
#'
#' Generate visual representation of curves, with optional separate plotting
#' of different groups, and also indication of individuals passing different
#' on types filters.
#'
#' @param cuperdec_table output tibble from `calculate_curves()`
#' @param metadata optional output from `load_map()`
#' @param burnin_result optional output from `apply_*_burnin()` functions
#' @param restrict_x optional restriction viewing of abundance rank to X number
#' of ranks (useful for closer inspection of curves)
#'
#' @export


plot_cuperdec <- function(cuperdec_table, metadata, burnin_result, restrict_x = 0) {

  if (restrict_x != 0) {
    cuperdec_table <- cuperdec_table %>% dplyr::filter(.data$Rank <= restrict_x)
  }

  if (missing(metadata) && missing(burnin_result)) {

     plot_simple(cuperdec_table)

    } else if (missing(metadata)) {

      plot_burnin(cuperdec_table, burnin_result)

    } else if (missing(burnin_result)) {

     plot_grouped(cuperdec_table, metadata)

    } else {

     plot_grouped_burnin(cuperdec_table, metadata, burnin_result)

  }

}

#' Plot curves with no metadata or burnin
#'
#' Informs `plot_cuperdec()`, plots curves with no additional formatting.
#'
#' @param cuperdec_table output tibble from `calculate_curves()`
#'
#' @noRd

plot_simple <- function(cuperdec_table) {

  if (any(!c("Sample", "Rank", "Fraction_Target") %in% colnames(cuperdec_table)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Rank, Fraction_Target. Is input from calculate_curve()?")

  ggplot2::ggplot(cuperdec_table, ggplot2::aes(.data$Rank,
                                      .data$Fraction_Target,
                                      group = .data$Sample)) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, 100) +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::theme_minimal()


}

#' Plot curves with burn-in filtering results
#'
#' Informs `plot_cuperdec()`, plots curves but with curve colouring based
#' on whether a sample passed a given filter.
#'
#' @param cuperdec_table output tibble from `calculate_curves()`
#' @param burnin_result optional output from `apply_*_burnin()` functions
#'
#' @noRd

plot_burnin <- function(cuperdec_table, burnin_result) {

  if (any(!c("Sample", "Passed") %in% colnames(burnin_result)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Rank, Fraction_Target. Is input from an apply_*_burnin() function?")

  if (!is.logical(burnin_result$Passed))
    stop("[cuperdec] error: filter 'Passed' column is not logical (i.e. TRUE/FALSE). Is input from an apply_*_burnin() function?")

  table_meta <- dplyr::left_join(cuperdec_table,
                                 burnin_result,
                                 by = c("Sample"))

  ## TODO - check if all samples in original dataframe is in map, given left join!

  ggplot2::ggplot(table_meta, ggplot2::aes(.data$Rank,
                                           .data$Fraction_Target,
                                           group = .data$Sample,
                                           colour = .data$Passed)
  ) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, 100) +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::theme_minimal()

}

#' Plot curves with group facets
#'
#' Informs `plot_cuperdec()`, separates curves based on a grouping category,
#' which places each group into a new facet.
#'
#' @param cuperdec_table output tibble from `calculate_curves()`
#' @param metadata optional output from `load_map()`
#'
#' @noRd

plot_grouped <- function(cuperdec_table, metadata) {

  if (any(!c("Sample", "Rank", "Fraction_Target") %in% colnames(cuperdec_table)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Rank, Fraction_Target. Is input from calculate_curve()?")

  if (any(!c("Sample", "Sample_Source") %in% colnames(metadata)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Sample_Source. Is input from load_map()?")

  if (any(!c("Sample", "Sample_Source") %in% colnames(metadata)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Sample_Source. Is input from load_map()?")

  table_meta <- dplyr::left_join(cuperdec_table, metadata, by = c("Sample"))

  if (any(is.na(table_meta$Sample_Source)))
    stop("[cuperdec] error: one or more of your samples did not have an associated sample source in the metadata table, or sample names did not match.")

  ## TODO - check if all samples in original dataframe is in map, given left join!

  ggplot2::ggplot(table_meta, ggplot2::aes(.data$Rank,
                                           .data$Fraction_Target,
                                           group = .data$Sample)
  ) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, 100) +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::facet_wrap(~ Sample_Source) +
    ggplot2::theme_minimal()

}

#' Plot curves with group facets and burn-in filtering results
#'
#' Informs `plot_cuperdec()`, plots curves but with curve colouring based
#' on whether a sample passed a given filter and separates curves of each
#' group into a different facet.
#'
#' @param cuperdec_table output tibble from `calculate_curves()`
#' @param metadata optional output from `load_map()`
#' @param burnin_result optional output from `apply_*_burnin()` functions
#'
#' @noRd

plot_grouped_burnin <- function(cuperdec_table, metadata, burnin_result) {

  ## TODO Add missing metadata check!

  if (any(!c("Sample", "Rank", "Fraction_Target") %in% colnames(cuperdec_table)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Rank, Fraction_Target. Is input from calculate_curve()?")

  if (any(!c("Sample", "Passed") %in% colnames(burnin_result)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Rank, Fraction_Target. Is input from calculate_curve()?")

  table_meta <- dplyr::left_join(cuperdec_table,
                                 metadata,
                                 by = c("Sample")) %>%
    dplyr::left_join(burnin_result, by = c("Sample"))

  if (any(is.na(table_meta$Sample_Source)))
    stop("[cuperdec] error: one or more of your samples did not have an associated sample source in the metadata table, or sample names did not match.")

  ggplot2::ggplot(table_meta, ggplot2::aes(.data$Rank,
                                           .data$Fraction_Target,
                                           group = .data$Sample,
                                           colour = .data$Passed)
  ) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, 100) +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::facet_wrap(~ Sample_Source) +
    ggplot2::theme_minimal()

}
