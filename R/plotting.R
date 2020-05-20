#' Plot cumulative percent decay curves
#'
#' @param table output tibble from `calculate_curves()`
#' @param metadata optional output from `load_map()`
#' @param burnin_result optional output from `apply_*_burnin()` functions
#' @param restrict_x optional restriction viewing of abundance rank to X number of ranks (useful for closer inspection of curves)
#'
#' @export


plot_cuperdec <- function(table, metadata, burnin_result, restrict_x = 0){

  if (restrict_x != 0) {
    table <- table %>% dplyr::filter(.data$Rank <= restrict_x)
  }

  if (missing(metadata) && missing(burnin_result) ) {

     plot_simple(table)

    } else if (missing(metadata)) {

      plot_burnin(table, burnin_result)

    } else if (missing(burnin_result)) {

     plot_grouped(table, metadata)

    } else {

     plot_grouped_burnin(table, metadata, burnin_result)

  }

}

#' <Internal> Plot simple curves with no metadata or burnin
#'
#' Informs `plot_cuperdec()`
#'
#' @param table output tibble from `calculate_curves()`

plot_simple <- function(table) {

  if (any(!c("Sample", "Rank", "Fraction_Target") %in% colnames(table)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Rank, Fraction_Target. Is input from calculate_curve()?")

  ggplot2::ggplot(table, ggplot2::aes(.data$Rank,
                                      .data$Fraction_Target,
                                      group = .data$Sample)) +
    ggplot2::geom_line() +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::theme_minimal()


}

#' <Internal> Plot simple curves with no sample group separataion but with burnin colouring
#'
#' Informs `plot_cuperdec()`
#'
#' @param table output tibble from `calculate_curves()`
#' @param burnin_result optional output from `apply_*_burnin()` functions

plot_burnin <- function(table, burnin_result) {

  if (any(!c("Sample", "Passed") %in% colnames(burnin_result)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Rank, Fraction_Target. Is input from an apply_*_burnin() function?")

  if (!is.logical(burnin_result$Passed))
    stop("[cuperdec] error: filter 'Passed' column is not logical (i.e. TRUE/FALSE). Is input from an apply_*_burnin() function?")

  table_meta <- dplyr::left_join(table,
                                 burnin_result,
                                 by = c("Sample"))

  ## TODO - check if all samples in original dataframe is in map, given left join!

  ggplot2::ggplot(table_meta, ggplot2::aes(.data$Rank,
                                           .data$Fraction_Target,
                                           group = .data$Sample,
                                           colour = .data$Passed)
  ) +
    ggplot2::geom_line() +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::theme_minimal()

}

#' <Internal> Plot simple curves with sample group separation but no burnin colouring
#'
#' Informs `plot_cuperdec()`
#'
#' @param table output tibble from `calculate_curves()`
#' @param metadata optional output from `load_map()`

plot_grouped <- function(table, metadata) {

  if (any(!c("Sample", "Rank", "Fraction_Target") %in% colnames(table)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Rank, Fraction_Target. Is input from calculate_curve()?")

  if (any(!c("Sample", "Sample_Source") %in% colnames(metadata)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Sample_Source. Is input from load_map()?")

  if (any(!c("Sample", "Sample_Source") %in% colnames(metadata)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Sample_Source. Is input from load_map()?")

  table_meta <- dplyr::left_join(table, metadata, by = c("Sample"))

  if ( any(is.na(table_meta$Sample_Source)) )
    stop("[cuperdec] error: one or more of your samples did not have an associated sample source in the metadata table, or sample names did not match.")

  ## TODO - check if all samples in original dataframe is in map, given left join!

  ggplot2::ggplot(table_meta, ggplot2::aes(.data$Rank,
                                           .data$Fraction_Target,
                                           group = .data$Sample)
  ) +
    ggplot2::geom_line() +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::facet_wrap(~ Sample_Source) +
    ggplot2::theme_minimal()

}


#' <Internal> Plot simple curves with sample group separation and burnin colouring
#'
#' Informs `plot_cuperdec()`
#'
#' @param table output tibble from `calculate_curves()`
#' @param metadata optional output from `load_map()`
#' @param burnin_result optional output from `apply_*_burnin()` functions

plot_grouped_burnin <- function(table, metadata, burnin_result) {

  if (any(!c("Sample", "Rank", "Fraction_Target") %in% colnames(table)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Rank, Fraction_Target. Is input from calculate_curve()?")

  if (any(!c("Sample", "Passed") %in% colnames(burnin_result)))
    stop("[cuperdec] error: missing column in input table. Minimum required: Sample, Rank, Fraction_Target. Is input from calculate_curve()?")

  table_meta <- dplyr::left_join(table,
                                 metadata,
                                 by = c("Sample")) %>%
    dplyr::left_join(burnin_result, by = c("Sample"))

  if ( any(is.na(table_meta$Sample_Source)) )
    stop("[cuperdec] error: one or more of your samples did not have an associated sample source in the metadata table, or sample names did not match.")

  ggplot2::ggplot(table_meta, ggplot2::aes(.data$Rank,
                                           .data$Fraction_Target,
                                           group = .data$Sample,
                                           colour = .data$Passed)
  ) +
    ggplot2::geom_line() +
    ggplot2::xlab("Abundance Rank") +
    ggplot2::ylab("Percentage Target Source") +
    ggplot2::facet_wrap(~ Sample_Source) +
    ggplot2::theme_minimal()

}
