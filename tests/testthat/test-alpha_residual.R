test_that("error works", {
  expect_error(alpha_residual(mat_exa,trans="not"), regexp="unknown trans option")
  expect_error(alpha_residual(mat_exa,index="not"), regexp="unknown index option")
  expect_error(alpha_residual(mat_exa,index="phylo"), regexp="need a tree to compute phylo index")
  expect_error(alpha_residual(mat_exa,setNames(rowSums(mat_exa),nm=paste0("s",1:10))),
               regexp="Mismatches between matrix rownames and count names")
})

tmp_hill<-hill_index(mat_exa) %>%
  tidyr::pivot_longer(starts_with("hill"),names_to="index",values_to="value") %>%
  dplyr::mutate(index=as.factor(index)) %>%
  dplyr::arrange(index,sample) %>%
  as.data.frame()
test_that("main function works", {
  expect_equal(alpha_residual(mat_exa),
               data.frame(tmp_hill,counts=rep(rowSums(mat_exa),3),
                          residual=c(scale(residuals(lm(tmp_hill[1:10,3] ~ rowSums(mat_exa))))[,1],
                                      scale(tmp_hill[11:20,3])[,1],
                                      scale(tmp_hill[21:30,3])[,1])))
  expect_equal(alpha_residual(mat_exa, trans="sqrt"),
               data.frame(tmp_hill,counts=c(sqrt(rowSums(mat_exa)),rep(rowSums(mat_exa),2)),
                          residual=c(scale(residuals(lm(tmp_hill[1:10,3] ~ sqrt(rowSums(mat_exa)))))[,1],
                                     scale(tmp_hill[11:20,3])[,1],
                                     scale(tmp_hill[21:30,3])[,1])))
})

test_that("information printed correctly", {
  expect_output(tmpo<-alpha_residual(mat_exa, inform=T),regexp="best linear correlation|not linearly correlated")
  expect_output(tmpo<-alpha_residual(mat_exa, trans="log", inform=T),regexp="best linear correlation|not linearly correlated")
})
