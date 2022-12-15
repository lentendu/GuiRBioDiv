test_that("main function works", {
  expect_equal(endemic(mat_exa, setNames( c( rep("A", 5), rep("B", 5) ), rownames(mat_exa) ) ),
               setNames(c(99,57,107,43,24,67,147,101,137,57),rownames(mat_exa)))
  expect_equal(endemic(mat_exa, setNames( c( rep("A", 5), rep("B", 5) ), rownames(mat_exa) ), singleocc = F ),
               setNames(c(40,38,53,15,13,38,82,61,67,32),rownames(mat_exa)))
})
test_that("error works", {
  expect_error(endemic(mat_exa, setNames( c( rep("A", 5), rep("B", 5) ), rownames(mat_exa) ), singleocc = "no" ),
               regex="unsupported value for singleocc option")
})
