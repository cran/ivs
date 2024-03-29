# An internal helper class used for testing,
# but this lives here so we can register the S3 methods

nested_integer_iv <- function(start, end) {
  out <- iv(start, end, ptype = integer())
  new_nested_integer_iv(out)
}

nested_integer_iv_pairs <- function(...) {
  out <- iv_pairs(..., ptype = integer())
  new_nested_integer_iv(out)
}

new_nested_integer_iv <- function(iv, ..., class = character()) {
  if (!is_iv(iv)) {
    abort("`iv` must be an <iv>.")
  }
  if (!is_bare_integer(iv_start(iv))) {
    abort("`iv_start(iv)` must be a bare integer.")
  }
  if (!is_bare_integer(iv_end(iv))) {
    abort("`iv_end(iv)` must be a bare integer.")
  }

  fields <- list(iv = iv)

  new_rcrd(fields, ..., class = c(class, "ivs_nested_integer_iv"))
}

#' @export
vec_ptype_full.ivs_nested_integer_iv <- function(x, ...) {
  "nested_integer_iv"
}

#' @export
format.ivs_nested_integer_iv <- function(x, ...) {
  format(field(x, "iv"), ...)
}

#' @export
vec_ptype2.ivs_nested_integer_iv.ivs_nested_integer_iv <- function(x, y, ...) {
  iv <- new_iv(start = integer(), end = integer())
  new_nested_integer_iv(iv = iv)
}

#' @export
vec_cast.ivs_nested_integer_iv.ivs_nested_integer_iv <- function(x, to, ...) {
  x
}

#' @export
iv_proxy.ivs_nested_integer_iv <- function(x, ...) {
  field(x, "iv")
}

#' @export
iv_restore.ivs_nested_integer_iv <- function(x, to, ...) {
  new_nested_integer_iv(x)
}
