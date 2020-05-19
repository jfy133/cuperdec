#' Plot cumulative percent decay curves
#'
#' @param table output tibble from `calculate_curves()`
#' @param metadata optional output from `load_map()`
#'
#' @export

## TODO Add consideration when threshold info is somehow supplied

plot_cuperdec <- function(table, metadata){

  if (missing(metadata)) {

    ggplot2::ggplot(table, ggplot2::aes(.data$Rank,
                                        .data$Fraction_Target,
                                        group = .data$Sample)) +
      ggplot2::geom_line() +
      ggplot2::xlab("Abundance Rank") +
      ggplot2::ylab("Percentage Target Source") +
      ggplot2::theme_minimal()

    } else {

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


}
