library(cuperdec)

raw_table <- system.file("extdata", "example_taxatable.tsv", package = "cuperdec")
raw_database <- system.file("extdata", "example_database.tsv", package = "cuperdec")
raw_metadata <- system.file("extdata", "example_metadata.tsv", package = "cuperdec")

table <- cuperdec::load_taxa_table(raw_table)

testthat::test_that("Taxa table is correctly formatted for downstream",{
  testthat::expect_named(table, c("Taxon", "Sample", "Count"))
  testthat::expect_type(table$Taxon, "character")
  testthat::expect_type(table$Sample, "character")
  testthat::expect_type(table$Count, "double")
})

database <- cuperdec::load_database(raw_database, "oral")

testthat::test_that("Isolation source database table is correctly formatted for downstream",{
  testthat::expect_named(database, c("Taxon", "Isolation_Source"))
  testthat::expect_type(database$Taxon, "character")
  testthat::expect_type(database$Isolation_Source, "logical")
  testthat::expect_length(unique(database$Isolation_Source), 2)
})

metadata <- load_map(raw_metadata, "#SampleID", "Env")

testthat::test_that("Metadata map table is correctly formatted for downstream",{
  testthat::expect_named(metadata, c("Sample", "Sample_Source"))
  testthat::expect_type(metadata$Sample, "character")
  testthat::expect_type(metadata$Sample_Source, "character")
})
