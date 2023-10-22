test_that("the result of fars_read is a tibble", {
  expect_s3_class(fars_read(test_path("test_data", "accident_2013_test.csv")), c("tbl_df", "tbl"))
})




