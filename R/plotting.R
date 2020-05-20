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

    ggplot2::ggplot(table, ggplot2::aes(.data$Rank,
                                        .data$Fraction_Target,
                                        group = .data$Sample)) +
      ggplot2::geom_line() +
      ggplot2::xlab("Abundance Rank") +
      ggplot2::ylab("Percentage Target Source") +
      ggplot2::theme_minimal()

    } else if (missing(burnin_result)) {

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


    } else {

      table_meta <- dplyr::left_join(table,
                                     metadata,
                                     by = c("Sample")) %>%
       dplyr::left_join(burnin_result, by = c("Sample"))

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


}
