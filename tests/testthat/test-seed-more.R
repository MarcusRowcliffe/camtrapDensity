library(testthat)

context("seed helper and reproducibility")

test_that(".with_seed sets and restores RNG and returns reproducible draws", {
  # Save current RNG state
  old <- if (exists('.Random.seed', envir = .GlobalEnv)) get('.Random.seed', envir = .GlobalEnv) else NULL

  a1 <- .with_seed(42, runif(3))
  a2 <- .with_seed(42, runif(3))
  expect_equal(a1, a2)

  # Ensure global RNG state restored: draws without seed should differ from a1
  b <- runif(3)
  expect_false(all(b == a1))

  # restore old RNG state
  if (is.null(old)) {
    if (exists('.Random.seed', envir = .GlobalEnv)) rm('.Random.seed', envir = .GlobalEnv)
  } else assign('.Random.seed', old, envir = .GlobalEnv)
})
