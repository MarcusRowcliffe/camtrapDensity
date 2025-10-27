library(testthat)

context("seeding")

test_that("get_trap_rate is reproducible with seed", {
  tr <- data.frame(locationName=c('A','B','C'),
                   latitude=c(0,0,0), longitude=c(0,0,0),
                   n=c(3,2,5), effort=c(10,4.55,19.4), effort_unit='day')
  r1 <- get_trap_rate(tr, reps=100, seed=42)
  r2 <- get_trap_rate(tr, reps=100, seed=42)
  expect_equal(r1, r2)
})
