test_that("browse_pr2 works", {
  expect_equal(browse_pr2("Ciliophora"), "subdivision")
  expect_equal(browse_pr2("abc"),NA)
})
