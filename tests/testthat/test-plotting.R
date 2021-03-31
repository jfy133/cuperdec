library(cuperdec)

raw_table <- system.file("extdata",
  "example_taxatable.tsv",
  package = "cuperdec"
)
raw_database <- system.file("extdata",
  "example_database.tsv",
  package = "cuperdec"
)
raw_metadata <- system.file("extdata",
  "example_metadata.tsv",
  package = "cuperdec"
)

## Test plotting!
taxatable <- cuperdec::load_taxa_table(raw_table)
database <- cuperdec::load_database(raw_database, "oral")
metadata <- load_map(raw_metadata, "#SampleID", "Env")

## calculate_curves
curve <- calculate_curve(taxatable, database)
curve_filtered <- cuperdec::hard_burnin_filter(curve, 50, 0.2)

## generate plots
curve_plot <- cuperdec::plot_cuperdec(curve)
curve_plot_restricted <-
  cuperdec::plot_cuperdec(curve, restrict_x = 50)
curve_metadata_plot <-
  cuperdec::plot_cuperdec(curve, metadata = metadata)
curve_filtered_plot <-
  cuperdec::plot_cuperdec(curve, burnin_result = curve_filtered)
curve_filtered_metadata_plot <-
  cuperdec::plot_cuperdec(curve,
    metadata = metadata,
    burnin_result = curve_filtered
  )

## plot tests
testthat::test_that("General plotting works", {
  testthat::expect_is(curve_plot, "ggplot")
})
testthat::test_that("Plot with metadata only works", {
  testthat::expect_is(curve_metadata_plot, "ggplot")
})
testthat::test_that("Plot with burnin only works", {
  testthat::expect_is(curve_filtered_plot, "ggplot")
})
testthat::test_that("Plot with burnin & metadata works", {
  testthat::expect_is(curve_filtered_metadata_plot, "ggplot")
})
testthat::test_that("Plot with burnin and cutoff", {
  testthat::expect_is(curve_plot_restricted, "ggplot")
})

## input validation tests
testthat::test_that("Check input parameters are right format", {
  testthat::expect_error(cuperdec::plot_cuperdec(curve, restrict_x = "50"))
})

## Note: could use library 'proto' to check expected ggplot objects
