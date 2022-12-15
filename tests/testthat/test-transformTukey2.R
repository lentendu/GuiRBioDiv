set.seed(2)
tmp_r<-rnorm(10)
test_that("Return same vector if already normal", {
  expect_equal(
    transformTukey2(tmp_r,onlynn = T)$val,
    tmp_r)
})
set.seed(2)
tmp<-runif(10,-2,2)
test_that("Return same transformation than expected", {
  expect_equal(
    transformTukey2(tmp)$trans,
    "x^-0.6")
})
set.seed(1)
tmp2<-runif(20,0,20)^.2
test_that("Return same transformation as expected for non-default lambda", {
  expect_equal(
    transformTukey2(tmp2,lambda=seq(-5,5,0.05))$trans,
    "x^4.6")
})

