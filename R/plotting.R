#' Plot cumulative percent decay curves
#'
#' Generate visual representation of curves, with optional separate plotting
#' of different groups, and also indication of individuals passing different
#' on types filters.
#'
#' @param curves output tibble from `calculate_curves()`
#' @param metadata optional output from `load_map()`
#' @param burnin_result optional output from `apply_*_burnin()` functions
#' @param restrict_x optional restriction viewing of abundance rank to X number
#' of ranks (useful for closer inspection of curves)
#'
#' @export

plot_cuperdec <- function(curves, metadata, burnin_result, restrict_x = 0) {

  ## Validation
  validate_curves(curves)

  if (!missing(metadata)) {
    validate_map(metadata)
  }

  if (!missing(burnin_result)) {
    validate_filter(burnin_result)
  }

  if (!is.numeric(restrict_x)) {
    stop("[cuperdec] error: restrict_x must be numeric")
  }

  if (restrict_x != 0) {
    curves <- curves %>% dplyr::filter(.data$Rank <= restrict_x)
  }

  ## Call corresponding plotting function
  if (missing(metadata) && missing(burnin_result)) {
    plot_simple(curves)
  } else if (missing(metadata)) {
    plot_burnin(curves, burnin_result)
  } else if (missing(burnin_result)) {
    plot_grouped(curves, metadata)
  } else {
    plot_grouped_burnin(curves, metadata, burnin_result)
  }
}

#' Plot curves with no metadata or burnin
#'
#' Informs `plot_cuperdec()`, plots curves with no additional formatting.
#'
#' @param curves output tibble from `calculate_curves()`
#'
#' @noRd

plot_simple <- function(curves) {
  ggplot2::ggplot(curves, ggplot2::aes(.data$Rank,
    .data$Fraction_Target,
    group = .data$Sample
  )) +
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
#' @param curves output tibble from `calculate_curves()`
#' @param burnin_result optional output from `apply_*_burnin()` functions
#'
#' @noRd

plot_burnin <- function(curves, burnin_result) {

  ## Validation
  validate_samples(curves, burnin_result)

  ## Calculation
  table_meta <- dplyr::left_join(curves,
    burnin_result,
    by = c("Sample")
  )

  ggplot2::ggplot(table_meta, ggplot2::aes(.data$Rank,
    .data$Fraction_Target,
    group = .data$Sample,
    colour = .data$Passed
  )) +
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
#' @param curves output tibble from `calculate_curves()`
#' @param metadata optional output from `load_map()`
#'
#' @noRd

plot_grouped <- function(curves, metadata) {
  table_meta <- dplyr::left_join(curves, metadata, by = c("Sample"))

  ## Validation
  validate_samples(curves, metadata)
  validate_samplesource(table_meta)

  ## Plotting
  ggplot2::ggplot(table_meta, ggplot2::aes(.data$Rank,
    .data$Fraction_Target,
    group = .data$Sample
  )) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, 100) +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::facet_wrap(~Sample_Source) +
    ggplot2::theme_minimal()
}

#' Plot curves with group facets and burn-in filtering results
#'
#' Informs `plot_cuperdec()`, plots curves but with curve colouring based
#' on whether a sample passed a given filter and separates curves of each
#' group into a different facet.
#'
#' @param curves output tibble from `calculate_curves()`
#' @param metadata optional output from `load_map()`
#' @param burnin_result optional output from `apply_*_burnin()` functions
#'
#' @noRd

plot_grouped_burnin <- function(curves, metadata, burnin_result) {
  table_meta <- dplyr::left_join(curves,
    metadata,
    by = c("Sample")
  ) %>%
    dplyr::left_join(burnin_result, by = c("Sample"))

  ## Validation
  validate_samples(curves, metadata)
  validate_samplesource(table_meta)

  ggplot2::ggplot(table_meta, ggplot2::aes(.data$Rank,
    .data$Fraction_Target,
    group = .data$Sample,
    colour = .data$Passed
  )) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, 100) +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::facet_wrap(~Sample_Source) +
    ggplot2::theme_minimal()
}
