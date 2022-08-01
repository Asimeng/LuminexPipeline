test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("column names transform to lower cases after applying colnames_clean
          function", {

  dta_separate <- readRDS("rds/dta_separate.rds")

  col_names <- names(colnames_clean(dta_separate))

  expect_equal(grep("^[[:upper:]]+$", col_names), integer(0)
)

})

test_that("colnames_clean function cleans white spaces in column names",{

  dta_separate <- readRDS("rds/dta_separate.rds")

  col_names <- names(colnames_clean(dta_separate))

  expect_equal(grep("\\s", col_names), integer(0))
})
