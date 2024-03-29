# Hardcoding the class isn't best practice. We are only doing this until
# `vctrs::new_vctr()` and potentially `vctrs::new_rcrd()` get rewritten in C.
# https://github.com/r-lib/vctrs/pull/1498
iv_classes <- c("ivs_iv", "vctrs_rcrd", "vctrs_vctr")

#' Construct a new iv
#'
#' `new_iv()` is a developer focused function for creating a new interval
#' vector. It does minimal checks on the inputs, for performance.
#'
#' @param start,end `[vector]`
#'
#'   A pair of vectors to represent the bounds of the intervals.
#'
#'   To be a valid interval vector, `start` must be strictly less than `end`,
#'   or both `start` and `end` must be a missing value.
#'
#' @param ... `[name-value pairs]`
#'
#'   Additional named attributes to attach to the result.
#'
#' @param class `[character]`
#'
#'   The name of the subclass to create.
#'
#' @return A new iv object.
#'
#' @export
#' @examples
#' new_iv(1, 2)
new_iv <- function(start, end, ..., class = character()) {
  out <- list(start = start, end = end)

  if (length(class)) {
    class <- c(class, iv_classes)
  } else {
    class <- iv_classes
  }

  n <- dots_n(...)

  if (n != 0L) {
    structure(out, ..., class = class)
  } else {
    class(out) <- class
    out
  }
}

new_bare_iv <- function(start, end) {
  out <- list(start = start, end = end)
  class(out) <- iv_classes
  out
}
new_bare_iv_from_fields <- function(x) {
  # Clear all other attributes
  attributes(x) <- list(names = names(x), class = iv_classes)
  x
}

# ------------------------------------------------------------------------------

#' Create an interval vector
#'
#' @description
#' - `iv()` creates an interval vector from `start` and `end` vectors. This
#'   is how you will typically create interval vectors, and is often used with
#'   columns in a data frame.
#'
#' - `iv_pairs()` creates an interval vector from _pairs_. This is often useful
#'   for interactive testing, as it provides a more intuitive interface for
#'   creating small interval vectors. It should generally not be used on a large
#'   scale because it can be slow.
#'
#' ## Intervals
#'
#' Interval vectors are _right-open_, i.e. `[start, end)`. This means that
#' `start < end` is a requirement to generate an interval vector. In particular,
#' empty intervals with `start == end` are not allowed.
#'
#' Right-open intervals tend to be the most practically useful. For example,
#' `[2019-01-01 00:00:00, 2019-01-02 00:00:00)` nicely encapsulates all times on
#' `2019-01-01`. With closed intervals, you'd have to attempt to specify this as
#' `2019-01-01 23:59:59`, which is inconvenient and inaccurate, as it doesn't
#' capture fractional seconds.
#'
#' Right-open intervals also have the extremely nice technical property that
#' they create a closed algebra. Concretely, the complement of a vector of
#' right-open intervals and the union, intersection, or difference of two
#' vectors of right-open intervals will always result in another vector of
#' right-open intervals.
#'
#' ## Missing intervals
#'
#' When creating interval vectors with `iv()`, if either bound is
#' [incomplete][vctrs::vec_detect_complete], then both bounds are set to
#' their missing value.
#'
#' @param start,end `[vector]`
#'
#'   A pair of vectors to represent the bounds of the intervals.
#'
#'   To be a valid interval vector, `start` must be strictly less than `end`.
#'
#'   If either `start` or `end` are incomplete / missing, then both bounds will
#'   be coerced to missing values.
#'
#'   `start` and `end` are recycled against each other and are cast to the same
#'   type.
#'
#' @param ptype `[vector(0) / NULL]`
#'
#'   A prototype to force for the inner type of the resulting iv. If `NULL`,
#'   this defaults to the common type of the inputs.
#'
#' @param size `[integer(1) / NULL]`
#'
#'   A size to force for the resulting iv. If `NULL`, this defaults to the
#'   common size of the inputs.
#'
#' @param ...
#'
#'   For `iv_pairs()`:
#'
#'   `[vector pairs]`
#'
#'   Vectors of size 2 representing intervals to include in the result.
#'
#'   All inputs will be cast to the same type.
#'
#'   For `iv()`:
#'
#'   These dots are for future extensions and must be empty.
#'
#' @return An iv.
#'
#' @name iv
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' set.seed(123)
#'
#' x <- tibble(
#'   start = as.Date("2019-01-01") + 1:5,
#'   end = start + sample(1:10, length(start), replace = TRUE)
#' )
#'
#' # Typically you'll use `iv()` with columns of a data frame
#' mutate(x, iv = iv(start, end), .keep = "unused")
#'
#' # `iv_pairs()` is useful for generating interval vectors interactively
#' iv_pairs(c(1, 5), c(2, 3), c(6, 10))
NULL

#' @rdname iv
#' @export
iv <- function(start, end, ..., ptype = NULL, size = NULL) {
  if (!vec_is(start)) {
    abort("`start` must be a vector.")
  }
  if (!vec_is(end)) {
    abort("`end` must be a vector.")
  }

  args <- list(start = start, end = end)
  args <- vec_cast_common(!!!args, .to = ptype)
  args <- vec_recycle_common(!!!args, .size = size)
  start <- args$start
  end <- args$end

  joint <- data_frame(start = start, end = end)
  complete <- vec_detect_complete(joint)

  if (!all(complete)) {
    incomplete <- !complete
    na <- vec_init(start)
    start <- vec_assign(start, incomplete, na)
    end <- vec_assign(end, incomplete, na)
  }

  compare <- vec_compare(start, end)
  greater_or_equal <- compare != -1L

  if (any(greater_or_equal, na.rm = TRUE)) {
    greater_or_equal <- which(greater_or_equal)
    greater_or_equal <- err_locs(greater_or_equal)
    message <- c(
      "`start` must be less than `end`.",
      i = glue(
        "`start` is not less than `end` at locations: {greater_or_equal}."
      )
    )
    abort(message)
  }

  new_iv(start, end)
}

#' @rdname iv
#' @export
iv_pairs <- function(..., ptype = NULL) {
  args <- list2(...)
  args <- unname(args)
  n_args <- length(args)

  if (n_args == 0L) {
    abort("Must supply at least one input.")
  }

  sizes <- list_sizes(args)
  not_pairs <- sizes != 2L
  if (any(not_pairs)) {
    loc <- which(not_pairs)[[1]]
    size <- sizes[[loc]]

    abort(c(
      "All inputs must be in pairs of size 2.",
      i = glue("Input {loc} is size {size}.")
    ))
  }

  start <- map(args, vec_slice, i = 1L)
  end <- map(args, vec_slice, i = 2L)

  start <- vec_c(!!!start, .error_call = current_env())
  end <- vec_c(!!!end, .error_call = current_env())

  iv(start, end, ptype = ptype)
}

# ------------------------------------------------------------------------------

#' Is `x` an iv?
#'
#' `is_iv()` tests if `x` is an iv object.
#'
#' @param x `[object]`
#'
#'   An object.
#'
#' @return A single `TRUE` or `FALSE`.
#'
#' @export
#' @examples
#' is_iv(1)
#' is_iv(new_iv(1, 2))
is_iv <- function(x) {
  inherits(x, "ivs_iv")
}

check_iv <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  if (!missing(x) && is_iv(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x = x,
    what = "an <iv>",
    arg = arg,
    call = call
  )
}

# ------------------------------------------------------------------------------

#' @export
vec_ptype.ivs_iv <- function(x, ...) {
  start <- unclass(x)[[1L]]
  ptype <- vec_ptype(start, ...)
  new_bare_iv(ptype, ptype)
}

#' @export
vec_ptype_finalise.ivs_iv <- function(x, ...) {
  start <- unclass(x)[[1L]]
  ptype <- vec_ptype_finalise(start, ...)
  new_bare_iv(ptype, ptype)
}

#' @export
vec_ptype2.ivs_iv.ivs_iv <- function(x, y, ...) {
  # If they are ivs, we can assume the structure is correct.
  # Going for absolute performance here.
  x <- unclass(x)[[1L]]
  y <- unclass(y)[[1L]]

  ptype <- vec_ptype2(x, y, ...)

  new_bare_iv(ptype, ptype)
}

#' @export
vec_cast.ivs_iv.ivs_iv <- function(x, to, ...) {
  # If they are ivs, we can assume the structure is correct.
  # Going for absolute performance here.
  to <- unclass(to)[[1L]]

  x <- unclass(x)
  start <- x[[1L]]
  end <- x[[2L]]

  start <- vec_cast(start, to, ...)
  end <- vec_cast(end, to, ...)

  new_bare_iv(start, end)
}

#' @export
vec_restore.ivs_iv <- function(x, to, ...) {
  new_bare_iv_from_fields(x)
}

# ------------------------------------------------------------------------------

#' Proxy and restore
#'
#' @description
#' - `iv_proxy()` is an S3 generic which allows you to write S3 methods for
#'   iv extension types to ensure that they are treated like iv objects. The
#'   input will be your iv extension object, `x`, and the return value should
#'   be an iv object.
#'
#' - `iv_restore()` is an S3 generic that dispatches off `to` that allows you
#'   to restore a proxied iv extension type back to its original type. The
#'   inputs will be a bare iv object, `x`, and your original iv extension
#'   object, `to`, and the return value should correspond to `x` restored to
#'   the type of `to`, if possible.
#'
#' You typically _don't_ need to create an `iv_proxy()` method if your class
#' directly extends iv through the `class` argument of [new_iv()]. You only
#' need to implement this if your class has a different structure than a
#' typical iv object. In particular, if `vctrs::field(x, "start")` and
#' `vctrs::field(x, "end")` don't return the `start` and `end` of the interval
#' vector respectively, then you probably need an `iv_proxy()` method.
#'
#' You typically _do_ need an `iv_restore()` method for custom iv extensions.
#' If your class is simple, then you can generally just call your constructor,
#' like `new_my_iv()`, to restore the class and any additional attributes that
#' might be required. If your class doesn't use [new_iv()], then an
#' `iv_restore()` method is mandatory, as this is one of the ways that ivs
#' detects that your class is compatible with ivs.
#'
#' This system allows you to use any `iv_*()` function on your iv extension
#' object without having to define S3 methods for all of them.
#'
#' Note that the default method for `iv_proxy()` returns its input unchanged,
#' even if it isn't an iv. Each `iv_*()` function does separate checking to
#' ensure that the proxy is a valid iv, or implements an alternate behavior if
#' no proxy method is implemented. In contrast, `iv_restore()` will error if a
#' method for `to` isn't registered.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x `[vector]`
#'
#'   A vector.
#'
#' @param to `[vector]`
#'
#'   The original vector to restore to.
#'
#' @return
#' - `iv_proxy()` should return an iv object for further manipulation.
#'
#' - `iv_restore()` should return an object of type `to`, if possible. In
#'   some cases, it may be required to fall back to returning an iv object.
#'
#' @name iv-genericity
#'
#' @examples
#' if (FALSE) {
#' # Registering S3 methods outside of a package doesn't always work quite
#' # right (like on the pkgdown site), so this code should only be run by a
#' # user reading the manual. If that is you, fear not! It should run just fine
#' # in your console.
#'
#' library(vctrs)
#'
#' new_nested_iv <- function(iv) {
#'   fields <- list(iv = iv)
#'   new_rcrd(fields, class = "nested_iv")
#' }
#'
#' format.nested_iv <- function(x, ...) {
#'   format(field(x, "iv"))
#' }
#'
#' iv_proxy.nested_iv <- function(x, ...) {
#'   field(x, "iv")
#' }
#'
#' iv_restore.nested_iv <- function(x, to, ...) {
#'   new_nested_iv(x)
#' }
#'
#' iv <- new_iv(c(1, 5), c(2, 7))
#'
#' x <- new_nested_iv(iv)
#' x
#'
#' # Proxies, then accesses the `start` field
#' iv_start(x)
#'
#' # Proxies, computes the complement to generate an iv,
#' # then restores to the original type
#' iv_set_complement(x)
#'
#' }
NULL

#' @rdname iv-genericity
#' @export
iv_proxy <- function(x, ...) {
  check_dots_empty0(...)
  UseMethod("iv_proxy")
}

#' @export
iv_proxy.default <- function(x, ...) {
  x
}


#' @rdname iv-genericity
#' @export
iv_restore <- function(x, to, ...) {
  check_dots_empty0(...)
  UseMethod("iv_restore", to)
}

#' @export
iv_restore.default <- function(x, to, ...) {
  class <- class(to)[[1L]]
  abort(glue("Object `to`, with type <{class}>, is not an <iv> and does not implement an `iv_restore()` method."))
}

#' @export
iv_restore.ivs_iv <- function(x, to, ...) {
  x
}

is_iv_extension <- function(x) {
  # If an `iv_restore()` method exists, then we assume that the object is
  # an iv extension that has a proxy that returns an iv. This is useful when
  # we aren't sure if the object is "iv-like" or not, like in the `missing`
  # and `empty` arguments of `iv_span()`.
  obj_s3_method_exists(x, "iv_restore")
}

# ------------------------------------------------------------------------------

#' @export
vec_ptype_abbr.ivs_iv <- function(x, ...) {
  proxy <- iv_proxy(x)
  check_iv(proxy, arg = "x")
  start <- field_start(proxy)
  inner <- vec_ptype_abbr(start)
  vec_paste0("iv<", inner, ">")
}

#' @export
vec_ptype_full.ivs_iv <- function(x, ...) {
  proxy <- iv_proxy(x)
  check_iv(proxy, arg = "x")
  start <- field_start(proxy)
  inner <- vec_ptype_full(start)
  vec_paste0("iv<", inner, ">")
}

# ------------------------------------------------------------------------------

#' Access the start or end of an interval vector
#'
#' @description
#' - `iv_start()` accesses the start of an interval vector.
#'
#' - `iv_end()` accesses the end of an interval vector.
#'
#' @param x `[iv]`
#'
#'   An interval vector.
#'
#' @return The start or end of `x`.
#'
#' @name iv-accessors
#'
#' @examples
#' x <- new_iv(1, 2)
#'
#' iv_start(x)
#' iv_end(x)
NULL

#' @rdname iv-accessors
#' @export
iv_start <- function(x) {
  x <- iv_proxy(x)
  check_iv(x)
  field_start(x)
}

#' @rdname iv-accessors
#' @export
iv_end <- function(x) {
  x <- iv_proxy(x)
  check_iv(x)
  field_end(x)
}

# ------------------------------------------------------------------------------

field_start <- function(x) {
  field(x, "start")
}
field_end <- function(x) {
  field(x, "end")
}
