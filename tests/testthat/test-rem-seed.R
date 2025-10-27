library(testthat)

context("rem seed propagation")

test_that("get_parameter_table/get_trap_rate reproducible with seed", {
  traprate <- data.frame(locationName=c('A','B','C','D'),
                         latitude=c(0,0,0,0), longitude=c(0,0,0,0),
                         n=c(3,2,5,1), effort=c(10,4.55,19.4,7.2), effort_unit='day')
  strata <- data.frame(stratumID=c('s1','s2'), area=c(10,20))
  # create a simple fake radius/angle/speed/activity models with minimal structure expected
  radmod <- list(edd = matrix(c(5,0.3), nrow=1), data = data.frame(x=1), unit='m', proportion_used=1)
  colnames(radmod$edd) <- c("estimate", "se")
  anglemod <- list(edd = matrix(c(1,0.1), nrow=1), data = data.frame(x=1), unit='radian', proportion_used=1)
  colnames(anglemod$edd) <- c("estimate", "se")
  spdmod <- list(estimate = data.frame(est=1.0, se=0.1), data = data.frame(x=1), unit='m/second')
  # Define a minimal S4 class 'actmod' so '@' slot access in get_parameter_table works
  if (!methods::isClass('actmod')) {
    methods::setClass('actmod', representation(act='numeric', data='numeric'))
  }
  actmod <- methods::new('actmod', act=c(0.2, 0.05), data=1:2)

  p1 <- get_parameter_table(traprate, radmod, anglemod, spdmod, actmod, reps=100, seed=42)
  p2 <- get_parameter_table(traprate, radmod, anglemod, spdmod, actmod, reps=100, seed=42)
  expect_equal(p1, p2)
})
