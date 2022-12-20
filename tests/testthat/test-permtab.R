set.seed(1)
tmp<-vegan::adonis2(mat_exa ~ group, data = data.frame(group=c("B","B","A","A","A","B","B","A","B","A")))
test_that("permtab works", {
  expect_equal(permtab(tmp)$R2, c(0.17,0.83,1))
  expect_equal(permtab(tmp)$pval, c("*","",""))
})
