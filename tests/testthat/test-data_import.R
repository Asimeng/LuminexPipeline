test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("data_import function throws an error if input directory is empty", {

  expect_error(data_import("test_empty_dir/"),
               "check input directory: misspelt or empty directory")
})

test_that("files without .txt extensions throw a warning message", {

  expect_warning(data_import("test_wrong_extension/"),
                 "some files may have incorrect format")

})

test_that("data output of data_import function has correct dimensions(column length)", {

  expect_equal(ncol(data_import("test_data_dir/")),
               39)
})

test_that("data_import function creates a filename column", {

  col_names <- colnames(data_import("test_data_dir/"))

  expect_equal("filename" %in% col_names,
               TRUE)
})

test_that("output of data_import function does not have a repeating header", {

  dat <- data_import("test_data_dir/")

  expect_equal(which(dat$Analyte == "Analyte"), integer(0))
})

test_that("data_import function writes 3 files to rds directory", {

  data_import("test_data_dir/")

  expect_match(list.files("rds"), "dta_import.rds", all = FALSE)
  expect_match(list.files("rds"), "rep_files.rds", all = FALSE)
  expect_match(list.files("rds"), "rep_files_invalid.rds", all = FALSE)

})
