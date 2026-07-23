## Helper to evaluate an expression with a temporary RNG seed
#' Evaluate expression with a temporary RNG seed
#'
#' Sets the random seed for the duration of evaluating `expr`, then
#' restores the previous RNG state. If `seed` is `NULL` the expression
#' is evaluated without changing the RNG state.
#'
#' @param seed Integer seed to set, or NULL to skip setting seed
#' @param expr An expression to evaluate
#' @return The result of evaluating `expr`
.with_seed <- function(seed, expr){
  expr <- substitute(expr)
  if(is.null(seed)){
    eval(expr, envir = parent.frame())
  } else{
    # Save RNG state (if present), set seed, eval, then restore
    has_seed <- exists('.Random.seed', envir = .GlobalEnv)
    old_seed <- if(has_seed) get('.Random.seed', envir = .GlobalEnv) else NULL
    on.exit({
      if(has_seed) assign('.Random.seed', old_seed, envir = .GlobalEnv) else rm('.Random.seed', envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(as.integer(seed))
    eval(expr, envir = parent.frame())
  }
}
