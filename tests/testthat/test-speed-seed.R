library(sbd)

# Fixed, non-degenerate data exercise the parametric prediction path without
# relying on a random fixture or external NHMP files.
speed_seed_package <- function() {
  list(data = list(observations = data.frame(
    scientificName = "Vulpes vulpes",
    speed = exp(seq(log(0.05), log(0.8), length.out = 80))
  )))
}

test_that("parametric speed uncertainty is reproducible and preserves caller RNG", {
  package <- speed_seed_package()
  fit <- function() fit_speedmodel(package, "Vulpes vulpes", pdf = "l")$estimate
  first <- .with_seed(101, {
    before <- .Random.seed
    result <- fit()
    expect_true(identical(.Random.seed, before))
    result
  })
  second <- .with_seed(202, fit())
  expect_true(all(is.finite(as.matrix(first))))
  expect_gt(first$se, 0)
  expect_identical(first, second)
})

test_that("speed uncertainty honours explicit seeds and NULL opt-out", {
  package <- speed_seed_package()
  fit <- function(seed) {
    fit_speedmodel(package, "Vulpes vulpes", pdf = "l", seed = seed)$estimate
  }
  first <- fit(7)
  second <- fit(7)
  different <- fit(8)
  expect_identical(first, second)
  expect_identical(first$est, different$est)
  expect_false(identical(first$se, different$se))
  .with_seed(101, {
    before <- .Random.seed
    unseeded <- fit(NULL)
    expect_false(identical(.Random.seed, before))
    expect_false(identical(unseeded, fit(NULL)))
  })
})

test_that("rem_estimate forwards default, custom and NULL seeds to speed fitting", {
  seen <- list()
  local_mocked_bindings(fit_speedmodel = function(package, species, seed = NULL, ...) {
    seen[length(seen) + 1L] <<- list(seed)
    stop("speed seed captured")
  })
  fit <- function(...) rem_estimate(
    speed_seed_package(), species = "Vulpes vulpes", check_deployments = FALSE,
    radius_model = list(), angle_model = list(), ...
  )
  expect_error(fit(), "speed seed captured")
  expect_error(fit(seed = 7), "speed seed captured")
  expect_error(fit(seed = NULL), "speed seed captured")
  expect_identical(seen, list(42, 7, NULL))
})
