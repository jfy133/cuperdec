#' Plot cumulative percent decay curves
#'
#' Generates visual representation of curves, with optional separate plotting
#' of different groups, and also indication of individuals passing different
#' on types filters.
#'
#' @param curves Output tibble from \code{\link{calculate_curve}}.
#' @param metadata Output from \code{\link{load_map}}.
#' @param burnin_result Output from \code{apply_*_burnin}.
#'   functions (optional).
#' @param restrict_x Restrict viewing of abundance rank to X number
#'   of ranks (useful for closer inspection of curves) (optional).
#' @param facet_cols Custom number of columns for faceted plots (optional).
#'
#' @return A ggplot2 image object.
#'
#' @examples
#' data(cuperdec_taxatable_ex)
#' data(cuperdec_database_ex)
#' data(cuperdec_metadata_ex)
#'
#' taxa_table <- load_taxa_table(cuperdec_taxatable_ex)
#' iso_database <- load_database(cuperdec_database_ex, target = "oral")
#' metadata_table <- load_map(cuperdec_metadata_ex,
#'   sample_col = "#SampleID",
#'   source_col = "Env"
#' )
#'
#' curves <- calculate_curve(taxa_table, iso_database)
#' burnin_results <- adaptive_burnin_filter(curves, percent_threshold = 0.1)
#'
#' plot_cuperdec(curves, metadata_table, burnin_results)
#' @export

plot_cuperdec <-
  function(curves,
           metadata,
           burnin_result,
           restrict_x = 0,
           facet_cols = NULL) {
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
      plot_grouped(curves, metadata, facet_cols)
    } else {
      plot_grouped_burnin(curves, metadata, burnin_result, facet_cols)
    }
  }

#' Plot curves with no metadata or burnin
#'
#' Informs \code{\link{plot_cuperdec}}, plots curves with no additional
#' formatting.
#'
#' @param curves Output tibble from \code{\link{calculate_curve}}.
#'
#' @return A ggplot2 image object.
#'
#' @noRd

plot_simple <- function(curves) {
  ggplot2::ggplot(
    curves,
    ggplot2::aes(.data$Rank,
      .data$Fraction_Target,
      group = .data$Sample
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, 100) +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::theme_minimal()
}

#' Plot curves with burn-in filtering results
#'
#' Informs \code{\link{plot_cuperdec}}, plots curves but with curve colouring
#' based on whether a sample passed a given filter.
#'
#' @param curves Output tibble from \code{\link{calculate_curve}}.
#' @param burnin_result Optional output from \code{\link{apply_*_burnin}}
#'   functions.
#'
#' @return A ggplot2 image object.
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

  ggplot2::ggplot(
    table_meta,
    ggplot2::aes(
      .data$Rank,
      .data$Fraction_Target,
      group = .data$Sample,
      colour = .data$Passed
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, 100) +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::theme_minimal()
}

#' Plot curves with group facets
#'
#' Informs \code{\link{plot_cuperdec}}, separates curves based on a grouping
#' category, which places each group into a new facet.
#'
#' @param curves Output tibble from \code{\link{calculate_curve}}.
#' @param metadata Output from \code{\link{load_map}} (optional).
#' @param facet_cols Custom number of columns in faceted table (optional).
#'
#' @return A ggplot2 image object.
#'
#' @noRd

plot_grouped <- function(curves, metadata, facet_cols = NULL) {
  table_meta <- dplyr::left_join(curves, metadata, by = c("Sample"))

  ## Validation
  validate_samples(curves, metadata)
  validate_samplesource(table_meta)

  ## Plotting
  ggplot2::ggplot(
    table_meta,
    ggplot2::aes(.data$Rank,
      .data$Fraction_Target,
      group = .data$Sample
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, 100) +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::facet_wrap(~Sample_Source, ncol = facet_cols) +
    ggplot2::theme_minimal()
}

#' Plot curves with group facets and burn-in filtering results
#'
#' Informs \code{\link{plot_cuperdec}}, plots curves but with curve colouring
#'   based
#' on whether a sample passed a given filter and separates curves of each
#' group into a different facet.
#'
#' @param curves Output tibble from \code{\link{calculate_curve}}.
#' @param metadata Output from \code{\link{load_map}} (optional).
#' @param burnin_result Output from \code{\link{apply_*_burnin}}
#'   functions (optional).
#' @param facet_cols Custom number of columns in faceted table (optional).
#'
#' @return A ggplot2 image object.
#'
#' @noRd

plot_grouped_burnin <- function(curves,
                                metadata,
                                burnin_result,
                                facet_cols = NULL) {
  table_meta <- dplyr::left_join(curves,
    metadata,
    by = c("Sample")
  ) %>%
    dplyr::left_join(burnin_result, by = c("Sample"))

  ## Validation
  validate_samples(curves, metadata)
  validate_samplesource(table_meta)

  ggplot2::ggplot(
    table_meta,
    ggplot2::aes(
      .data$Rank,
      .data$Fraction_Target,
      group = .data$Sample,
      colour = .data$Passed
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, 100) +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::facet_wrap(~Sample_Source, ncol = facet_cols) +
    ggplot2::theme_minimal()
}
