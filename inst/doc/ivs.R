## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ivs)
library(clock)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
# Interval vector of integers
iv(1:5, 7:11)

# Interval vector of dates
starts <- as.Date("2019-01-01") + 0:2
ends <- starts + c(2, 5, 10)

iv(starts, ends)

## -----------------------------------------------------------------------------
start <- bit64::as.integer64("900000000000")
end <- start + 1234

iv(start, end)

## -----------------------------------------------------------------------------
start <- year_month_day(c(2019, 2020), c(1, 3))
end <- year_month_day(c(2020, 2020), c(2, 6))

iv(start, end)

## -----------------------------------------------------------------------------
x <- iv(1:3, 4:6)
x

## -----------------------------------------------------------------------------
iv_start(x)
iv_end(x)

## -----------------------------------------------------------------------------
tibble(x = x)

## -----------------------------------------------------------------------------
# iv_pairs() is a useful way to create small ivs from individual intervals
needles <- iv_pairs(c(1, 5), c(3, 7), c(10, 12))
needles

haystack <- iv_pairs(c(0, 6), c(13, 15), c(0, 2), c(7, 8), c(4, 5))
haystack

locations <- iv_locate_overlaps(needles, haystack)
locations

## -----------------------------------------------------------------------------
iv_align(needles, haystack, locations = locations)

## -----------------------------------------------------------------------------
iv_overlaps(needles, haystack)

## -----------------------------------------------------------------------------
locations <- iv_locate_overlaps(
  needles, 
  haystack, 
  type = "contains", 
  no_match = "drop"
)

iv_align(needles, haystack, locations = locations)

## -----------------------------------------------------------------------------
locations <- iv_locate_overlaps(
  needles, 
  haystack, 
  type = "within", 
  no_match = "drop"
)

iv_align(needles, haystack, locations = locations)

## -----------------------------------------------------------------------------
# Where does `needles` precede `haystack`?
locations <- iv_locate_precedes(needles, haystack)
locations

## -----------------------------------------------------------------------------
iv_align(needles, haystack, locations = locations)

## -----------------------------------------------------------------------------
# Where does `needles` follow `haystack`?
locations <- iv_locate_follows(needles, haystack)

iv_align(needles, haystack, locations = locations)

## -----------------------------------------------------------------------------
locations <- iv_locate_follows(
  needles = needles, 
  haystack = haystack, 
  closest = TRUE,
  no_match = "drop"
)

iv_align(needles, haystack, locations = locations)

## -----------------------------------------------------------------------------
sales <- as.Date(c("2019-01-01", "2020-05-10", "2020-06-10"))

commercial_starts <- as.Date(c(
  "2019-10-12", "2020-04-01", "2020-06-01", "2021-05-10"
))
commercial_ends <- commercial_starts + 90

commercials <- iv(commercial_starts, commercial_ends)

sales
commercials

## -----------------------------------------------------------------------------
tibble(sales = sales) %>%
  mutate(commercial_running = iv_between(sales, commercials))

## -----------------------------------------------------------------------------
iv_align(sales, commercials, locations = iv_locate_between(sales, commercials))

## -----------------------------------------------------------------------------
x <- c(1, 5, 10, 12)
x

y <- iv_pairs(c(0, 6), c(7, 9), c(10, 12), c(10, 12))
y

iv_pairwise_between(x, y)

## -----------------------------------------------------------------------------
enrollments <- tribble(
  ~name,      ~start,          ~end,
  "Amy",      "1, Jan, 2017",  "30, Jul, 2018",
  "Franklin", "1, Jan, 2017",  "19, Feb, 2017",
  "Franklin", "5, Jun, 2017",  "4, Feb, 2018",
  "Franklin", "21, Oct, 2018", "9, Mar, 2019",
  "Samir",    "1, Jan, 2017",  "4, Feb, 2017",
  "Samir",    "5, Apr, 2017",  "12, Jun, 2018"
)

# Parse these into "day" precision year-month-day objects
enrollments <- enrollments %>%
  mutate(
    start = year_month_day_parse(start, format = "%d, %b, %Y"),
    end = year_month_day_parse(end, format = "%d, %b, %Y"),
  )

enrollments

## -----------------------------------------------------------------------------
enrollments <- enrollments %>%
  mutate(
    start = calendar_narrow(start, "month"),
    end = calendar_narrow(end, "month") + 1L
  )

enrollments

enrollments <- enrollments %>%
  mutate(active = iv(start, end), .keep = "unused")

enrollments

## -----------------------------------------------------------------------------
bounds <- range(enrollments$active)
lower <- iv_start(bounds[[1]])
upper <- iv_end(bounds[[2]]) - 1L

months <- tibble(month = seq(lower, upper, by = 1))

months

## -----------------------------------------------------------------------------
months %>%
  mutate(count = iv_count_between(month, enrollments$active)) %>%
  print(n = Inf)

## -----------------------------------------------------------------------------
x <- iv_pairs(c(1, 5), c(5, 7), c(9, 11), c(10, 13), c(12, 13))
x

iv_groups(x)

## -----------------------------------------------------------------------------
iv_groups(x, abutting = FALSE)

## -----------------------------------------------------------------------------
costs <- tribble(
  ~user, ~system, ~from, ~to, ~cost,
  1L, "a", "2019-01-01", "2019-01-05", 200.5,
  1L, "a", "2019-01-12", "2019-01-13", 15.6,
  1L, "b", "2019-01-03", "2019-01-10", 500.3,
  2L, "a", "2019-01-02", "2019-01-03", 25.6,
  2L, "c", "2019-01-03", "2019-01-04", 30,
  2L, "c", "2019-01-05", "2019-01-07", 66.2
)

costs <- costs %>%
  mutate(
    from = as.Date(from),
    to = as.Date(to)
  ) %>%
  mutate(range = iv(from, to), .keep = "unused")

costs

## -----------------------------------------------------------------------------
costs %>%
  reframe(range = iv_groups(range), .by = user)

## -----------------------------------------------------------------------------
costs2 <- costs %>%
  mutate(range = iv_identify_group(range), .by = user)

# `range` has been updated with the corresponding group
costs2

# So now we can group on that to summarise the cost
costs2 %>%
  summarise(cost = sum(cost), .by = c(user, range))

## -----------------------------------------------------------------------------
x <- iv_pairs(c(1, 5), c(5, 7), c(9, 11), c(10, 13), c(12, 13))
x

## -----------------------------------------------------------------------------
iv_splits(x)

## -----------------------------------------------------------------------------
guests <- tibble(
  arrive = as.POSIXct(
    c("2008-05-20 19:30:00", "2008-05-20 20:10:00", "2008-05-20 22:15:00"),
    tz = "UTC"
  ),
  depart = as.POSIXct(
    c("2008-05-20 23:00:00", "2008-05-21 00:00:00", "2008-05-21 00:30:00"),
    tz = "UTC"
  ),
  name = list(
    c("Mary", "Harry"),
    c("Diana", "Susan"),
    "Peter"
  )
)

guests <- unnest(guests, name) %>%
  mutate(iv = iv(arrive, depart), .keep = "unused")

guests

## -----------------------------------------------------------------------------
iv_splits(guests$iv)

## -----------------------------------------------------------------------------
# Mary's arrival/departure times
guests$iv[[1]]

# The first start and last end correspond to Mary's original times,
# but we've also broken her stay up by the departures/arrivals of
# everyone else
iv_identify_splits(guests$iv)[[1]]

## -----------------------------------------------------------------------------
guests2 <- guests %>%
  mutate(iv = iv_identify_splits(iv)) %>%
  unnest(iv) %>%
  arrange(iv)

guests2

## -----------------------------------------------------------------------------
guests2 %>%
  summarise(n = n(), who = list(name), .by = iv)

## -----------------------------------------------------------------------------
x <- iv_pairs(c(1, 3), c(2, 5), c(10, 12), c(13, 15))
x

iv_set_complement(x)

## -----------------------------------------------------------------------------
iv_set_complement(x, lower = 0, upper = Inf)

## -----------------------------------------------------------------------------
y <- iv_pairs(c(-5, 0), c(1, 4), c(8, 10), c(15, 16))

x
y

iv_set_union(x, y)

## -----------------------------------------------------------------------------
iv_set_intersect(x, y)

## -----------------------------------------------------------------------------
iv_set_difference(x, y)

## -----------------------------------------------------------------------------
starts <- as.Date(c("2019-01-05", "2019-01-20", "2019-01-25", "2019-02-01"))
ends <- starts + c(5, 10, 3, 5)
x <- iv(starts, ends)

starts <- as.Date(c("2019-01-02", "2019-01-23"))
ends <- starts + c(5, 6)
y <- iv(starts, ends)

x
y

## -----------------------------------------------------------------------------
iv_set_intersect(x, y)

## -----------------------------------------------------------------------------
locations <- iv_locate_overlaps(x, y, no_match = "drop")
overlaps <- iv_align(x, y, locations = locations)

overlaps %>%
  mutate(intersect = iv_pairwise_set_intersect(needles, haystack))

## ---- error=TRUE--------------------------------------------------------------
iv_pairwise_set_intersect(iv(1, 5), iv(6, 9))

## -----------------------------------------------------------------------------
x <- iv_pairs(c(1, 5), c(3, NA), c(NA, 3))
x

## -----------------------------------------------------------------------------
y <- iv_pairs(c(NA, NA), c(0, 2))
y

## -----------------------------------------------------------------------------
# Match-like operations treat missing intervals as overlapping
iv_locate_overlaps(x, y)

iv_set_intersect(x, y)

## -----------------------------------------------------------------------------
# Pairwise operations treat missing intervals as infectious
z <- iv_pairs(c(1, 2), c(1, 4))

iv_pairwise_set_intersect(y, z)

