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

## load_taxa_table
raw_table_path <- cuperdec::load_taxa_table(raw_table)
loaded_table <- readr::read_tsv(raw_table)
converted_table <- cuperdec::load_taxa_table(loaded_table)

testthat::test_that("Taxa table is correctly formatted for downstream", {
  testthat::expect_named(raw_table_path, c("Taxon", "Sample", "Count"))
  testthat::expect_type(raw_table_path$Taxon, "character")
  testthat::expect_type(raw_table_path$Sample, "character")
  testthat::expect_type(raw_table_path$Count, "double")
  testthat::expect_named(converted_table, c("Taxon", "Sample", "Count"))
})

table_fail <-
  readr::read_tsv(raw_table) %>% dplyr::select(`#Datasets`)

testthat::test_that("Fails when insufficent columns in OTU table", {
  testthat::expect_error(load_taxa_table(table_fail))
})

## load_database
database <- cuperdec::load_database(raw_database, "oral")
converted_database <- readr::read_tsv(raw_database) %>%
  cuperdec::load_database(x = ., target = "oral")

testthat::test_that("Isolation source database table is correctly
                    formatted for downstream", {
  testthat::expect_named(
    database,
    c("Taxon", "Isolation_Source")
  )
  testthat::expect_type(database$Taxon, "character")
  testthat::expect_type(
    database$Isolation_Source,
    "logical"
  )
  testthat::expect_length(
    unique(database$Isolation_Source),
    2
  )
  testthat::expect_named(
    converted_database,
    c("Taxon", "Isolation_Source")
  )
})


database_fail <-
  readr::read_tsv(raw_database) %>% dplyr::mutate(foo = "bar")
database_broken <-
  readr::read_tsv(raw_database) %>% dplyr::mutate(isolation_source = "bar")

testthat::test_that("Fails when failed or broken database", {
  testthat::expect_error(load_database(database_fail, "oral"))
  testthat::expect_error(load_database(database_broken, "oral"))
})

## load_map
metadata <- load_map(raw_metadata, "#SampleID", "Env")

converted_metadata <- readr::read_tsv(raw_metadata) %>%
  cuperdec::load_map(
    x = .,
    sample_col = "#SampleID",
    source_col = "Env"
  )


testthat::test_that("Metadata map table is correctly
                    formatted for downstream", {
  testthat::expect_named(metadata, c(
    "Sample",
    "Sample_Source"
  ))
  testthat::expect_type(metadata$Sample, "character")
  testthat::expect_type(metadata$Sample_Source, "character")
  testthat::expect_named(
    converted_metadata,
    c("Sample", "Sample_Source")
  )
})

map_fail_insufficientcols <-
  readr::read_tsv(raw_metadata) %>% dplyr::select(`#SampleID`)
map_fail_missingnamecol <-
  readr::read_tsv(raw_metadata) %>% dplyr::rename("Foo" = `#SampleID`)
map_fail_missingsourcecol <-
  readr::read_tsv(raw_metadata) %>% dplyr::rename("Foo" = "Env")

testthat::test_that("Fails when metadata file broken", {
  testthat::expect_error(load_map(
    map_fail_insufficientcols,
    "#SampleID",
    "Env"
  ))
  testthat::expect_error(load_map(
    map_fail_missingnamecol,
    "#SampleID",
    "Env"
  ))
  testthat::expect_error(load_map(
    map_fail_missingsourcecol,
    "#SampleID",
    "Env"
  ))
})
