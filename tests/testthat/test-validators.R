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

taxatable_broken <- taxatable %>% dplyr::filter(Count == -1)

testthat::test_that("Should error", {
  testthat::expect_error(cuperdec::validate_taxatable(taxatable_broken))
})
