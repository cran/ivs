#' Locate relationships between a vector and an iv
#'
#' @description
#' This family of functions locates different types of relationships between a
#' vector and an iv. It works similar to [base::match()], where `needles[i]`
#' checks for a match in all of `haystack`. Unlike `match()`, _all_ matches are
#' returned, rather than just the first.
#'
#' - `iv_locate_between()` locates where `needles`, a vector, falls between the
#'   bounds of `haystack`, an iv.
#'
#' - `iv_locate_includes()` locates where `needles`, an iv, includes the values
#'   of `haystack`, a vector.
#'
#' These functions return a two column data frame. The `needles` column is an
#' integer vector pointing to locations in `needles`. The `haystack` column is
#' an integer vector pointing to locations in `haystack` with a match.
#'
#' @inheritParams iv_locate_overlaps
#'
#' @param needles,haystack `[vector, iv]`
#'
#'   For `iv_*_between()`, `needles` should be a vector and `haystack` should be
#'   an iv.
#'
#'   For `iv_*_includes()`, `needles` should be an iv and `haystack` should be
#'   a vector.
#'
#'   - Each element of `needles` represents the value / interval to match.
#'
#'   - `haystack` represents the values / intervals to match against.
#'
#' @param missing `[integer(1) / "equals" / "drop" / "error"]`
#'
#'   Handling of missing values in `needles`.
#'
#'   - `"equals"` considers missing values in `needles` as exactly equal
#'     to missing values in `haystack` when determining if there is a
#'     matching relationship between them.
#'
#'   - `"drop"` drops missing values in `needles` from the result.
#'
#'   - `"error"` throws an error if any values in `needles` are missing.
#'
#'   - If a single integer is provided, this represents the value returned in
#'     the `haystack` column for values in `needles` that are missing.
#'
#' @return
#' A data frame containing two integer columns named `needles` and `haystack`.
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Detect relationships between a vector and an iv][vector-detect]
#'
#' [Pairwise detect relationships between a vector and an iv][vector-detect-pairwise]
#'
#' @name vector-locate
#' @examples
#' x <- as.Date(c("2019-01-05", "2019-01-10", "2019-01-07", "2019-01-20"))
#'
#' y <- iv_pairs(
#'   as.Date(c("2019-01-01", "2019-01-03")),
#'   as.Date(c("2019-01-04", "2019-01-08")),
#'   as.Date(c("2019-01-07", "2019-01-09")),
#'   as.Date(c("2019-01-10", "2019-01-20")),
#'   as.Date(c("2019-01-15", "2019-01-20"))
#' )
#'
#' x
#' y
#'
#' # Find any location where `x` is between the intervals in `y`
#' loc <- iv_locate_between(x, y)
#' loc
#'
#' iv_align(x, y, locations = loc)
#'
#' # Find any location where `y` includes the values in `x`
#' loc <- iv_locate_includes(y, x)
#' loc
#'
#' iv_align(y, x, locations = loc)
#'
#' # Drop values in `x` without a match
#' loc <- iv_locate_between(x, y, no_match = "drop")
#' loc
#'
#' iv_align(x, y, locations = loc)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- c(1, NA)
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # By default, missing values in `needles` are treated as being exactly
#' # equal to missing values in `haystack`, so the missing value in `a` is
#' # considered between the missing interval in `b`.
#' iv_locate_between(a, b)
#' iv_locate_includes(b, a)
#'
#' # If you'd like missing values in `needles` to always be considered
#' # unmatched, set `missing = NA`
#' iv_locate_between(a, b, missing = NA)
#' iv_locate_includes(b, a, missing = NA)
NULL

#' @rdname vector-locate
#' @export
iv_locate_between <- function(needles,
                              haystack,
                              ...,
                              missing = "equals",
                              no_match = NA_integer_,
                              remaining = "drop",
                              multiple = "all",
                              relationship = "none") {
  check_dots_empty0(...)

  iv_locate_vector(
    x = needles,
    y = haystack,
    x_arg = "needles",
    y_arg = "haystack",
    type = "between",
    missing = missing,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple,
    relationship = relationship
  )
}

#' @rdname vector-locate
#' @export
iv_locate_includes <- function(needles,
                               haystack,
                               ...,
                               missing = "equals",
                               no_match = NA_integer_,
                               remaining = "drop",
                               multiple = "all",
                               relationship = "none") {
  check_dots_empty0(...)

  iv_locate_vector(
    x = haystack,
    y = needles,
    x_arg = "haystack",
    y_arg = "needles",
    type = "includes",
    missing = missing,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple,
    relationship = relationship
  )
}

iv_locate_vector <- function(x,
                             y,
                             x_arg,
                             y_arg,
                             type,
                             missing,
                             no_match,
                             remaining,
                             multiple,
                             relationship,
                             ...,
                             error_call = caller_env()) {
  check_dots_empty0(...)

  y <- iv_proxy(y)
  check_iv(y, arg = y_arg, call = error_call)

  y_start <- field_start(y)
  y_end <- field_end(y)

  y_start_arg <- paste0("iv_start(", y_arg, ")")
  y_end_arg <- paste0("iv_end(", y_arg, ")")

  args <- list(x, y_start, y_end)
  names(args) <- c(x_arg, y_start_arg, y_end_arg)

  args <- vec_cast_common(!!!args, .call = error_call)
  x <- args[[1L]]
  y_start <- args[[2L]]
  y_end <- args[[3L]]

  incomplete <- check_locate_missing(missing, "match")

  if (type == "between") {
    condition <- c(">=", "<")
    needles <- data_frame(a = x, b = x)
    haystack <- data_frame(a = y_start, b = y_end)
    needles_arg <- x_arg
    haystack_arg <- y_arg
  } else if (type == "includes") {
    condition <- c("<=", ">")
    needles <- data_frame(a = y_start, b = y_end)
    haystack <- data_frame(a = x, b = x)
    needles_arg <- y_arg
    haystack_arg <- x_arg
  } else {
    abort("Unknown `type`.", .internal = TRUE)
  }

  vec_locate_matches(
    needles = needles,
    haystack = haystack,
    condition = condition,
    incomplete = incomplete,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple,
    relationship = relationship,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg,
    error_call = error_call
  )
}

#' Count relationships between a vector and an iv
#'
#' @description
#' This family of functions counts different types of relationships between a
#' vector and an iv. It works similar to [base::match()], where `needles[i]`
#' checks for a match in all of `haystack`.
#'
#' - `iv_count_between()` counts instances of when `needles`, a vector, falls
#'   between the bounds of `haystack`, an iv.
#'
#' - `iv_count_includes()` counts instances of when `needles`, an iv, includes
#'   the values of `haystack`, a vector.
#'
#' These functions return an integer vector the same size as `needles`
#' containing a count of the times where the `i`-th value of `needles` contained
#' a match in `haystack`.
#'
#' @inheritParams iv_locate_between
#'
#' @param missing `[integer(1) / "equals" / "error"]`
#'
#'   Handling of missing values in `needles`.
#'
#'   - `"equals"` considers missing values in `needles` as exactly equal
#'     to missing values in `haystack` when determining if there is a
#'     matching relationship between them.
#'
#'   - `"error"` throws an error if any values in `needles` are missing.
#'
#'   - If a single integer value is provided, this represents the count returned
#'     for a missing value in `needles`. Use `0L` to force missing values
#'     to never match.
#'
#' @param no_match `[integer(1) / "error"]`
#'
#'   Handling of `needles` without a match.
#'
#'   - `"error"` throws an error if any needles have zero matches.
#'
#'   - If a single integer is provided, this represents the count returned for
#'     a needle with zero matches. The default value gives unmatched needles
#'     a count of `0L`.
#'
#' @return An integer vector the same size as `needles`.
#'
#' @seealso
#' [Locating relationships between a vector and an iv][vector-locate]
#'
#' @name vector-count
#' @examples
#' x <- as.Date(c("2019-01-05", "2019-01-10", "2019-01-07", "2019-01-20"))
#'
#' y <- iv_pairs(
#'   as.Date(c("2019-01-01", "2019-01-03")),
#'   as.Date(c("2019-01-04", "2019-01-08")),
#'   as.Date(c("2019-01-07", "2019-01-09")),
#'   as.Date(c("2019-01-10", "2019-01-20")),
#'   as.Date(c("2019-01-15", "2019-01-20"))
#' )
#'
#' x
#' y
#'
#' # Count the number of times `x` is between the intervals in `y`
#' iv_count_between(x, y)
#'
#' # Count the number of times `y` includes a value from `x`
#' iv_count_includes(y, x)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- c(1, NA)
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # By default, missing values in `needles` are treated as being exactly
#' # equal to missing values in `haystack`, so the missing value in `a` is
#' # considered between the missing interval in `b`.
#' iv_count_between(a, b)
#' iv_count_includes(b, a)
#'
#' # If you'd like to propagate missing values, set `missing = NA`
#' iv_count_between(a, b, missing = NA)
#' iv_count_includes(b, a, missing = NA)
#'
#' # If you'd like missing values to be treated as unmatched, set
#' # `missing = 0L`
#' iv_count_between(a, b, missing = 0L)
#' iv_count_includes(b, a, missing = 0L)
NULL

#' @rdname vector-count
#' @export
iv_count_between <- function(needles,
                             haystack,
                             ...,
                             missing = "equals",
                             no_match = 0L) {
  check_dots_empty0(...)

  iv_count_vector(
    x = needles,
    y = haystack,
    x_arg = "needles",
    y_arg = "haystack",
    type = "between",
    missing = missing,
    no_match = no_match
  )
}

#' @rdname vector-count
#' @export
iv_count_includes <- function(needles,
                              haystack,
                              ...,
                              missing = "equals",
                              no_match = 0L) {
  check_dots_empty0(...)

  iv_count_vector(
    x = haystack,
    y = needles,
    x_arg = "haystack",
    y_arg = "needles",
    type = "includes",
    missing = missing,
    no_match = no_match
  )
}

iv_count_vector <- function(x,
                            y,
                            x_arg,
                            y_arg,
                            type,
                            missing,
                            no_match,
                            ...,
                            error_call = caller_env()) {
  check_dots_empty0(...)

  missing <- check_count_missing(missing, call = error_call)
  no_match <- check_count_no_match(no_match, call = error_call)

  # Never makes sense to keep remaining `y` rows
  remaining <- "drop"

  # Obviously we are trying to count all of the matches
  multiple <- "all"

  # None of the "count" functions currently expose `relationship`.
  # We could do this eventually if it seems useful.
  relationship <- "none"

  locations <- iv_locate_vector(
    x = x,
    y = y,
    x_arg = x_arg,
    y_arg = y_arg,
    type = type,
    missing = translate_count_missing(missing),
    no_match = translate_count_no_match(no_match),
    remaining = remaining,
    multiple = multiple,
    relationship = relationship,
    error_call = error_call
  )

  iv_count_locations(locations, missing, no_match)
}

#' Detect relationships between a vector and an iv
#'
#' @description
#' This family of functions detects different types of relationships between a
#' vector and an iv. It works similar to [base::%in%], where `needles[i]`
#' checks for a match in all of `haystack`.
#'
#' - `iv_between()` detects when `needles`, a vector, falls between the
#'   bounds in `haystack`, an iv.
#'
#' - `iv_includes()` detects when `needles`, an iv, includes the values
#'   of `haystack`, a vector.
#'
#' This function returns a logical vector the same size as `needles` containing
#' `TRUE` if the value in `needles` matches any value in `haystack` and `FALSE`
#' otherwise.
#'
#' @inheritParams iv_locate_between
#'
#' @param missing `[logical(1) / "equals" / "error"]`
#'
#'   Handling of missing values in `needles`.
#'
#'   - `"equals"` considers missing values in `needles` as exactly equal
#'     to missing values in `haystack` when determining if there is a
#'     matching relationship between them. Matched missing values in
#'     `needles` result in a `TRUE` value in the result, and unmatched missing
#'     values result in a `FALSE` value.
#'
#'   - `"error"` throws an error if any values in `needles` are missing.
#'
#'   - If a single logical value is provided, this represents the value returned
#'     in the result for values in `needles` that are missing. You can force
#'     missing values to be unmatched by setting this to `FALSE`, and you
#'     can force them to be propagated by setting this to `NA`.
#'
#' @return A logical vector the same size as `needles`.
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Locating relationships between a vector and an iv][vector-locate]
#'
#' [Pairwise detect relationships between a vector and an iv][vector-detect-pairwise]
#'
#' @name vector-detect
#' @examples
#' x <- as.Date(c("2019-01-05", "2019-01-10", "2019-01-07", "2019-01-20"))
#'
#' y <- iv_pairs(
#'   as.Date(c("2019-01-01", "2019-01-03")),
#'   as.Date(c("2019-01-04", "2019-01-08")),
#'   as.Date(c("2019-01-07", "2019-01-09")),
#'   as.Date(c("2019-01-10", "2019-01-20")),
#'   as.Date(c("2019-01-15", "2019-01-20"))
#' )
#'
#' x
#' y
#'
#' # Detect if the i-th location in `x` is between any intervals in `y`
#' iv_between(x, y)
#'
#' # Detect if the i-th location in `y` includes any value in `x`
#' iv_includes(y, x)
#'
#' # ---------------------------------------------------------------------------
#'
#' a <- c(1, NA)
#' b <- iv(c(NA, NA), c(NA, NA))
#'
#' # By default, missing values in `needles` are treated as being exactly
#' # equal to missing values in `haystack`, so the missing value in `a` is
#' # considered between the missing interval in `b`.
#' iv_between(a, b)
#' iv_includes(b, a)
#'
#' # If you'd like to propagate missing values, set `missing = NA`
#' iv_between(a, b, missing = NA)
#' iv_includes(b, a, missing = NA)
#'
#' # If you'd like missing values to be treated as unmatched, set
#' # `missing = FALSE`
#' iv_between(a, b, missing = FALSE)
#' iv_includes(b, a, missing = FALSE)
NULL

#' @rdname vector-detect
#' @export
iv_between <- function(needles,
                       haystack,
                       ...,
                       missing = "equals") {
  check_dots_empty0(...)

  iv_detect_vector(
    x = needles,
    y = haystack,
    x_arg = "needles",
    y_arg = "haystack",
    type = "between",
    missing = missing
  )
}

#' @rdname vector-detect
#' @export
iv_includes <- function(needles,
                        haystack,
                        ...,
                        missing = "equals") {
  check_dots_empty0(...)

  iv_detect_vector(
    x = haystack,
    y = needles,
    x_arg = "haystack",
    y_arg = "needles",
    type = "includes",
    missing = missing
  )
}

iv_detect_vector <- function(x,
                             y,
                             x_arg,
                             y_arg,
                             type,
                             missing,
                             ...,
                             error_call = caller_env()) {
  check_dots_empty0(...)

  y <- iv_proxy(y)
  check_iv(y, arg = y_arg, call = error_call)

  y_start <- field_start(y)
  y_end <- field_end(y)

  y_start_arg <- paste0("iv_start(", y_arg, ")")
  y_end_arg <- paste0("iv_end(", y_arg, ")")

  args <- list(x, y_start, y_end)
  names(args) <- c(x_arg, y_start_arg, y_end_arg)

  args <- vec_cast_common(!!!args, .call = error_call)
  x <- args[[1L]]
  y_start <- args[[2L]]
  y_end <- args[[3L]]

  incomplete <- check_detect_missing(missing, "match", call = error_call)

  if (type == "between") {
    condition <- c(">=", "<")
    needles <- data_frame(a = x, b = x)
    haystack <- data_frame(a = y_start, b = y_end)
    needles_arg <- x_arg
    haystack_arg <- y_arg
  } else if (type == "includes") {
    condition <- c("<=", ">")
    needles <- data_frame(a = y_start, b = y_end)
    haystack <- data_frame(a = x, b = x)
    needles_arg <- y_arg
    haystack_arg <- x_arg
  } else {
    abort("Unknown `type`.", .internal = TRUE)
  }

  matches <- vec_locate_matches(
    needles = needles,
    haystack = haystack,
    condition = condition,
    incomplete = incomplete,
    no_match = 0L,
    multiple = "any",
    needles_arg = needles_arg,
    haystack_arg = haystack_arg,
    error_call = error_call
  )

  # 0L -> FALSE
  # NA_integer -> NA
  # otherwise -> TRUE
  out <- as.logical(matches$haystack)

  out
}

#' Pairwise detect relationships between a vector and an iv
#'
#' @description
#' This family of functions detects different types of relationships between a
#' vector and an iv _pairwise_. where pairwise means that the i-th value of `x`
#' is compared against the i-th value of `y`. This is in contrast to
#' [iv_between()], which works more like [base::%in%].
#'
#' - `iv_pairwise_between()` detects if the i-th value of `x`, a vector, falls
#'   between the bounds of the i-th value of `y`, an iv.
#'
#' - `iv_pairwise_includes()` detects if the i-th value of `x`, an iv, includes
#'   the i-th value of `y`, a vector.
#'
#' These functions return a logical vector the same size as the common size of
#' `x` and `y`.
#'
#' @param x,y `[vector, iv]`
#'
#'   For `iv_pairwise_between()`, `x` must be a vector and `y` must be an iv.
#'
#'   For `iv_pairwise_includes()`, `x` must be an iv and `y` must be a vector.
#'
#'   `x` and `y` will be recycled against each other.
#'
#' @return A logical vector the same size as the common size of `x` and `y`.
#'
#' @seealso
#' [Locating relationships][relation-locate]
#'
#' [Locating relationships between a vector and an iv][vector-locate]
#'
#' [Detecting relationships between a vector and an iv][vector-detect]
#'
#' @name vector-detect-pairwise
#' @examples
#' x <- as.Date(c("2019-01-01", "2019-01-08", "2019-01-21"))
#'
#' y <- iv_pairs(
#'   as.Date(c("2019-01-01", "2019-01-03")),
#'   as.Date(c("2019-01-07", "2019-01-09")),
#'   as.Date(c("2019-01-18", "2019-01-21"))
#' )
#'
#' x
#' y
#'
#' # Does the i-th value of `x` fall between the i-th interval of `y`?
#' iv_pairwise_between(x, y)
#'
#' # Does the i-th interval of `y` include the i-th value of `x`?
#' iv_pairwise_includes(y, x)
#'
#' a <- c(1, NA, NA)
#' b <- iv_pairs(c(NA, NA), c(3, 4), c(NA, NA))
#'
#' # Missing intervals always propagate
#' iv_pairwise_between(a, b)
#' iv_pairwise_includes(b, a)
NULL

#' @rdname vector-detect-pairwise
#' @export
iv_pairwise_between <- function(x, y) {
  iv_detect_pairwise_vector(
    x = x,
    y = y,
    x_arg = "x",
    y_arg = "y"
  )
}

#' @rdname vector-detect-pairwise
#' @export
iv_pairwise_includes <- function(x, y) {
  iv_detect_pairwise_vector(
    x = y,
    y = x,
    x_arg = "y",
    y_arg = "x"
  )
}

iv_detect_pairwise_vector <- function(x,
                                      y,
                                      x_arg,
                                      y_arg,
                                      ...,
                                      error_call = caller_env()) {
  check_dots_empty0(...)

  y <- iv_proxy(y)
  check_iv(y, arg = y_arg, call = error_call)

  y_start <- field_start(y)
  y_end <- field_end(y)

  y_start_arg <- paste0("iv_start(", y_arg, ")")
  y_end_arg <- paste0("iv_end(", y_arg, ")")

  args <- list(x, y_start, y_end)
  names(args) <- c(x_arg, y_start_arg, y_end_arg)

  args <- vec_cast_common(!!!args, .call = error_call)
  args <- vec_recycle_common(!!!args, .call = error_call)
  x <- args[[1L]]
  y_start <- args[[2L]]
  y_end <- args[[3L]]

  after_start <- vec_compare(x, y_start) >= 0L
  before_end <- vec_compare(x, y_end) < 0L

  out <- after_start & before_end

  out
}
