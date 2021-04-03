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

taxatable <- cuperdec::load_taxa_table(raw_table)
database <- cuperdec::load_database(raw_database, "oral")
metadata <- load_map(raw_metadata, "#SampleID", "Env")

## calculate_curve
curve <- calculate_curve(taxatable, database)

testthat::test_that("Curves calculated as expected", {
  testthat::expect_named(curve, c("Sample", "Taxon", "Rank", "Fraction_Target"))
  testthat::expect_type(curve$Taxon, "character")
  testthat::expect_type(curve$Sample, "character")
  testthat::expect_type(curve$Rank, "integer")
  testthat::expect_type(curve$Fraction_Target, "double")
  testthat::expect_gt(length(unique(curve$Rank)), 1)
})

## simple_filter
filter_result <- simple_filter(curve, 50)

testthat::test_that("Simple filter works as expected", {
  testthat::expect_named(filter_result, c("Sample", "Passed"))
  testthat::expect_type(filter_result$Sample, "character")
  testthat::expect_type(filter_result$Passed, "logical")
})

testthat::test_that("Simple filter doesn't accept non-numeric threshold", {
  testthat::expect_error(simple_filter(curve, "50"))
})

## hard_burnin_filter
burnin_result <- hard_burnin_filter(curve, 50, 0.1)

testthat::test_that("Hard burnin filter works as expected", {
  testthat::expect_named(burnin_result, c("Sample", "Passed"))
  testthat::expect_type(burnin_result$Sample, "character")
  testthat::expect_type(burnin_result$Passed, "logical")
})

testthat::test_that("Simple filter inputs are valid", {
  testthat::expect_error(hard_burnin_filter(curve, "50", 0.1))
  ## rank_burnin between 0-1
  testthat::expect_error(hard_burnin_filter(curve, 50, 2))
  ## rank_burnin between 0-1
  testthat::expect_error(hard_burnin_filter(curve, 50, "0.1"))
})

## adaptive_burnin_filter
burnin_result <- adaptive_burnin_filter(curve, 50)

testthat::test_that("Adaptive burn-in works as expected", {
  testthat::expect_named(burnin_result, c("Sample", "Passed"))
  testthat::expect_type(burnin_result$Sample, "character")
  testthat::expect_type(burnin_result$Passed, "logical")
})

testthat::test_that("Adaptive filter inputs are valid", {
  testthat::expect_error(adaptive_burnin_filter(curve, "50"))
})
