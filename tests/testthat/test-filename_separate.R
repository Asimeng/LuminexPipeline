# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that("filename_separate function joins columns: 'date, kit, instrument,
          plate, rerun' to output from preceding function (data_import)", {

            dta_import <- readRDS("rds/dta_import.rds")

            instrument_names <- c("bp", "mp")
            #test_colnames <- c("date", "kit", "instrument", "plate", "rerun")

            separated_filename <- filename_separate(dta_import, instrument_names)

            expect_match(colnames(separated_filename), "date", all = FALSE)
            expect_match(colnames(separated_filename), "kit", all = FALSE)
            expect_match(colnames(separated_filename), "instrument", all = FALSE)
            expect_match(colnames(separated_filename), "plate", all = FALSE)
            expect_match(colnames(separated_filename), "rerun", all = FALSE)

          })

test_that("parsed dates have the correct class", {

  dta_import <- readRDS("rds/dta_import.rds")

  instrument_names <- c("bp", "mp")

  separated_filename <- filename_separate(dta_import, instrument_names)

  expect_equal(class(separated_filename$date), c("POSIXct", "POSIXt"))

})


test_that("input data without a filename column stops with an error",{

  dta_import <- readRDS("rds/dta_test.rds")

  instrument_names <- c("bp", "mp")

  expect_error(filename_separate(dta_import, instrument_names),
               "check input data: input may not have a 'filename' column")
})

test_that("output of filename_separate function is of class tibble", {

  dta_import <- readRDS("rds/dta_import.rds")

  instrument_names <- c("bp", "mp")

  separated_filename <- filename_separate(dta_import, instrument_names)

  expect_equal(class(separated_filename), c("tbl_df","tbl","data.frame"))
})

test_that("filename_separate function writes processed data to rds files", {

  dta_import <- readRDS("rds/dta_import.rds")

  instrument_names <- c("bp", "mp")

  filename_separate(dta_import, instrument_names)

  expect_match(list.files("rds"), "dta_separate.rds", all = FALSE)
})
