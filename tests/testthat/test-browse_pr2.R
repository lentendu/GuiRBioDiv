test_that("browse_pr2 works", {
  expect_equal(browse_pr2("Ciliophora"), "subdivision")
  expect_equal(browse_pr2("abcde"),NA)
  expect_equal(browse_pr2("Eukaryot",partial=F),NA)
})
