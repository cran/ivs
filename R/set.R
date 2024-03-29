#' Set operations
#'
#' @description
#' This family of functions treats ivs as sets. They always compute
#' the [minimal][iv_groups()] iv of each input and return a minimal iv.
#'
#' - `iv_set_complement()` takes the complement of the intervals in an iv. By
#'   default, the minimum and maximum of the inputs define the bounds to take
#'   the complement over, but this can be adjusted with `lower` and `upper`.
#'   Missing intervals are always dropped in the complement.
#'
#' - `iv_set_union()` answers the question, "Which intervals are in `x` or `y`?"
#'   It is equivalent to combining the two vectors together and then calling
#'   `iv_groups()`.
#'
#' - `iv_set_intersect()` answers the question, "Which intervals are in `x` and
#'   `y`?"
#'
#' - `iv_set_difference()` answers the question, "Which intervals are in `x` but
#'   not `y`?" Note that this is an asymmetrical difference.
#'
#' - `iv_set_symmetric_difference()` answers the question, "Which intervals are
#'   in `x` or `y` but not both?"
#'
#' @section Graphical Representation:
#'
#' Graphically, generating the complement looks like:
#'
#' ![](complement.png)
#'
#' If you were to set `upper = 20` with these intervals, then you'd get one more
#' interval in the complement.
#'
#' ![](complement-upper.png)
#'
#' Generating the intersection between two ivs looks like:
#'
#' ![](intersect.png)
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x `[iv]`
#'
#'   An interval vector.
#'
#' @param y `[iv]`
#'
#'   An interval vector.
#'
#' @param lower,upper `[vector(1) / NULL]`
#'
#'   Bounds for the universe over which to compute the complement. These should
#'   have the same type as the element type of the interval vector. It is
#'   often useful to expand the universe to, say, `-Inf` to `Inf`.
#'
#' @return
#' - For `iv_set_complement()`, a vector of the same type as `x` containing the
#' complement.
#'
#' - For all other set operations, a vector of the same type as the common type
#' of `x` and `y` containing the result.
#'
#' @seealso The _pairwise_ versions of these functions, such as
#' [iv_pairwise_set_union()].
#'
#' @name iv-sets
#'
#' @examples
#' x <- iv_pairs(
#'   c(10, 12),
#'   c(0, 5),
#'   c(NA, NA),
#'   c(3, 6),
#'   c(-5, -2),
#'   c(NA, NA)
#' )
#' x
#'
#' y <- iv_pairs(
#'   c(2, 7),
#'   c(NA, NA),
#'   c(-3, -1),
#'   c(14, 15)
#' )
#' y
#'
#' # Complement contains any values from `[-5, 12)` that aren't represented
#' # in these intervals. Missing intervals are dropped.
#' iv_set_complement(x)
#'
#' # Expand out the "universe" of possible values
#' iv_set_complement(x, lower = -Inf)
#' iv_set_complement(x, lower = -Inf, upper = Inf)
#'
#' # Which intervals are in x or y?
#' iv_set_union(x, y)
#'
#' # Which intervals are in x and y?
#' iv_set_intersect(x, y)
#'
#' # Which intervals are in x but not y?
#' iv_set_difference(x, y)
#'
#' # Which intervals are in y but not x?
#' iv_set_difference(y, x)
#'
#' # Missing intervals in x are kept if there aren't missing intervals in y
#' iv_set_difference(x, iv(1, 2))
#'
#' # Which intervals are in x or y but not both?
#' iv_set_symmetric_difference(x, y)
#'
#' # Missing intervals will be kept if they only appear on one side
#' iv_set_symmetric_difference(x, iv(1, 2))
#' iv_set_symmetric_difference(iv(1, 2), x)
NULL

#' @rdname iv-sets
#' @export
iv_set_complement <- function(x, ..., lower = NULL, upper = NULL) {
  proxy <- iv_proxy(x)
  check_iv(proxy, arg = "x")

  start <- field_start(proxy)
  end <- field_end(proxy)

  out <- vec_interval_complement(
    start = start,
    end = end,
    ...,
    lower = lower,
    upper = upper
  )

  out <- new_iv(out$start, out$end)
  out <- iv_restore(out, x)

  out
}

#' @rdname iv-sets
#' @export
iv_set_union <- function(x, y) {
  out <- vec_c(x, y)
  iv_groups(out)
}

#' @rdname iv-sets
#' @export
iv_set_intersect <- function(x, y) {
  args <- vec_cast_common(x = x, y = y)
  x <- args[[1]]
  y <- args[[2]]

  x_missing <- vec_detect_missing(x)
  y_missing <- vec_detect_missing(y)

  any_x_missing <- any(x_missing)
  any_y_missing <- any(y_missing)

  if (any_x_missing) {
    x <- vec_slice(x, !x_missing)
  }
  if (any_y_missing) {
    y <- vec_slice(y, !y_missing)
  }

  if (vec_size(x) == 0L || vec_size(y) == 0L) {
    out <- vec_ptype(x)
    out <- vec_ptype_finalise(out)

    if (any_x_missing && any_y_missing) {
      out <- vec_c(out, vec_init(out))
    }

    return(out)
  }

  x_proxy <- iv_proxy(x)
  y_proxy <- iv_proxy(y)

  check_iv(x_proxy, arg = "x")
  check_iv(y_proxy, arg = "y")

  x_span <- iv_span(x_proxy, missing = "error", empty = "error")
  y_span <- iv_span(y_proxy, missing = "error", empty = "error")

  span <- iv_span(vec_c(x_span, y_span), missing = "error", empty = "error")

  lower <- field_start(span)
  upper <- field_end(span)

  x_c <- iv_set_complement(x_proxy, lower = lower, upper = upper)
  y_c <- iv_set_complement(y_proxy, lower = lower, upper = upper)

  u <- iv_set_union(x_c, y_c)

  out <- iv_set_complement(u, lower = lower, upper = upper)

  if (any_x_missing && any_y_missing) {
    out <- vec_c(out, vec_init(out))
  }

  out <- iv_restore(out, x)

  out
}

#' @rdname iv-sets
#' @export
iv_set_difference <- function(x, y) {
  args <- vec_cast_common(x = x, y = y)
  x <- args[[1]]
  y <- args[[2]]

  x_missing <- vec_detect_missing(x)
  y_missing <- vec_detect_missing(y)

  any_x_missing <- any(x_missing)
  any_y_missing <- any(y_missing)

  if (any_x_missing) {
    x <- vec_slice(x, !x_missing)
  }
  if (any_y_missing) {
    y <- vec_slice(y, !y_missing)
  }

  if (vec_size(x) == 0L || vec_size(y) == 0L) {
    out <- iv_groups(x)

    if (any_x_missing && !any_y_missing) {
      out <- vec_c(out, vec_init(out))
    }

    return(out)
  }

  x_proxy <- iv_proxy(x)
  y_proxy <- iv_proxy(y)

  check_iv(x_proxy, arg = "x")
  check_iv(y_proxy, arg = "y")

  x_span <- iv_span(x_proxy, missing = "error", empty = "error")
  y_span <- iv_span(y_proxy, missing = "error", empty = "error")

  span <- iv_span(vec_c(x_span, y_span), missing = "error", empty = "error")

  lower <- field_start(span)
  upper <- field_end(span)

  x_c <- iv_set_complement(x_proxy, lower = lower, upper = upper)

  u <- iv_set_union(x_c, y_proxy)

  out <- iv_set_complement(u, lower = lower, upper = upper)

  if (any_x_missing && !any_y_missing) {
    out <- vec_c(out, vec_init(out))
  }

  out <- iv_restore(out, x)

  out
}

#' @rdname iv-sets
#' @export
iv_set_symmetric_difference <- function(x, y) {
  args <- vec_cast_common(x = x, y = y)
  x <- args[[1]]
  y <- args[[2]]

  x_missing <- vec_detect_missing(x)
  y_missing <- vec_detect_missing(y)

  any_x_missing <- any(x_missing)
  any_y_missing <- any(y_missing)

  if (any_x_missing) {
    x <- vec_slice(x, !x_missing)
  }
  if (any_y_missing) {
    y <- vec_slice(y, !y_missing)
  }

  if (vec_size(x) == 0L || vec_size(y) == 0L) {
    out <- iv_set_union(x, y)

    if (xor(any_x_missing, any_y_missing)) {
      out <- vec_c(out, vec_init(out))
    }

    return(out)
  }

  x_proxy <- iv_proxy(x)
  y_proxy <- iv_proxy(y)

  check_iv(x_proxy, arg = "x")
  check_iv(y_proxy, arg = "y")

  x_span <- iv_span(x_proxy, missing = "error", empty = "error")
  y_span <- iv_span(y_proxy, missing = "error", empty = "error")

  span <- iv_span(vec_c(x_span, y_span), missing = "error", empty = "error")

  lower <- field_start(span)
  upper <- field_end(span)

  x_c <- iv_set_complement(x_proxy, lower = lower, upper = upper)
  x_c_union_y <- iv_set_union(x_c, y_proxy)
  x_setdiff_y <- iv_set_complement(x_c_union_y, lower = lower, upper = upper)

  y_c <- iv_set_complement(y_proxy, lower = lower, upper = upper)
  y_c_union_x <- iv_set_union(y_c, x_proxy)
  y_setdiff_x <- iv_set_complement(y_c_union_x, lower = lower, upper = upper)

  out <- iv_set_union(x_setdiff_y, y_setdiff_x)

  if (xor(any_x_missing, any_y_missing)) {
    out <- vec_c(out, vec_init(out))
  }

  out <- iv_restore(out, x)

  out
}
