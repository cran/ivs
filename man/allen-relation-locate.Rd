% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relation.R
\name{allen-relation-locate}
\alias{allen-relation-locate}
\alias{iv_locate_relates}
\title{Locate relations from Allen's Interval Algebra}
\usage{
iv_locate_relates(
  needles,
  haystack,
  ...,
  type,
  missing = "equals",
  no_match = NA_integer_,
  remaining = "drop",
  multiple = "all",
  relationship = "none"
)
}
\arguments{
\item{needles, haystack}{\verb{[iv]}

Interval vectors used for relation matching.
\itemize{
\item Each element of \code{needles} represents the interval to search for.
\item \code{haystack} represents the intervals to search in.
}

Prior to comparison, \code{needles} and \code{haystack} are coerced to the same type.}

\item{...}{These dots are for future extensions and must be empty.}

\item{type}{\verb{[character(1)]}

The type of relationship to find. See the Allen's Interval Algebra section
for a complete description of each type. One of:
\itemize{
\item \code{"precedes"}
\item \code{"preceded-by"}
\item \code{"meets"}
\item \code{"met-by"}
\item \code{"overlaps"}
\item \code{"overlapped-by"}
\item \code{"starts"}
\item \code{"started-by"}
\item \code{"during"}
\item \code{"contains"}
\item \code{"finishes"}
\item \code{"finished-by"}
\item \code{"equals"}
}}

\item{missing}{\verb{[integer(1) / "equals" / "drop" / "error"]}

Handling of missing intervals in \code{needles}.
\itemize{
\item \code{"equals"} considers missing intervals in \code{needles} as exactly equal
to missing intervals in \code{haystack} when determining if there is a
matching relationship between them.
\item \code{"drop"} drops missing intervals in \code{needles} from the result.
\item \code{"error"} throws an error if any intervals in \code{needles} are missing.
\item If a single integer is provided, this represents the value returned in
the \code{haystack} column for intervals in \code{needles} that are missing.
}}

\item{no_match}{Handling of \code{needles} without a match.
\itemize{
\item \code{"drop"} drops \code{needles} with zero matches from the result.
\item \code{"error"} throws an error if any \code{needles} have zero matches.
\item If a single integer is provided, this represents the value returned in
the \code{haystack} column for values of \code{needles} that have zero matches. The
default represents an unmatched needle with \code{NA}.
}}

\item{remaining}{Handling of \code{haystack} values that \code{needles} never matched.
\itemize{
\item \code{"drop"} drops remaining \code{haystack} values from the result.
Typically, this is the desired behavior if you only care when \code{needles}
has a match.
\item \code{"error"} throws an error if there are any remaining \code{haystack}
values.
\item If a single integer is provided (often \code{NA}), this represents the value
returned in the \code{needles} column for the remaining \code{haystack} values
that \code{needles} never matched. Remaining \code{haystack} values are always
returned at the end of the result.
}}

\item{multiple}{Handling of \code{needles} with multiple matches. For each needle:
\itemize{
\item \code{"all"} returns all matches detected in \code{haystack}.
\item \code{"any"} returns any match detected in \code{haystack} with no guarantees on
which match will be returned. It is often faster than \code{"first"} and
\code{"last"} if you just need to detect if there is at least one match.
\item \code{"first"} returns the first match detected in \code{haystack}.
\item \code{"last"} returns the last match detected in \code{haystack}.
}}

\item{relationship}{Handling of the expected relationship between
\code{needles} and \code{haystack}. If the expectations chosen from the list below
are invalidated, an error is thrown.
\itemize{
\item \code{"none"} doesn't perform any relationship checks.
\item \code{"one-to-one"} expects:
\itemize{
\item Each value in \code{needles} matches at most 1 value in \code{haystack}.
\item Each value in \code{haystack} matches at most 1 value in \code{needles}.
}
\item \code{"one-to-many"} expects:
\itemize{
\item Each value in \code{needles} matches any number of values in \code{haystack}.
\item Each value in \code{haystack} matches at most 1 value in \code{needles}.
}
\item \code{"many-to-one"} expects:
\itemize{
\item Each value in \code{needles} matches at most 1 value in \code{haystack}.
\item Each value in \code{haystack} matches any number of values in \code{needles}.
}
\item \code{"many-to-many"} expects:
\itemize{
\item Each value in \code{needles} matches any number of values in \code{haystack}.
\item Each value in \code{haystack} matches any number of values in \code{needles}.
}

This performs no checks, and is identical to \code{"none"}, but is provided to
allow you to be explicit about this relationship if you know it exists.
\item \code{"warn-many-to-many"} doesn't assume there is any known relationship, but
will warn if \code{needles} and \code{haystack} have a many-to-many relationship
(which is typically unexpected), encouraging you to either take a closer
look at your inputs or make this relationship explicit by specifying
\code{"many-to-many"}.
}

\code{relationship} is applied after \code{filter} and \code{multiple} to allow potential
multiple matches to be filtered out first.

\code{relationship} doesn't handle cases where there are zero matches. For that,
see \code{no_match} and \code{remaining}.}
}
\value{
A data frame containing two integer columns named \code{needles} and \code{haystack}.
}
\description{
\code{iv_locate_relates()} is similar to \code{\link[=iv_locate_overlaps]{iv_locate_overlaps()}}, but it locates a
specific set of relations developed by James Allen in the paper:
\href{http://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf}{Maintaining Knowledge about Temporal Intervals}.
}
\section{Allen's Interval Algebra}{


The interval algebra developed by James Allen serves as a basis and
inspiration for \code{\link[=iv_locate_overlaps]{iv_locate_overlaps()}}, \code{\link[=iv_locate_precedes]{iv_locate_precedes()}}, and
\code{\link[=iv_locate_follows]{iv_locate_follows()}}. The original algebra is composed of 13 relations
which have the following properties:
\itemize{
\item Distinct: No pair of intervals can be related by more than one \code{type}.
\item Exhaustive: All pairs of intervals are described by one of the \code{type}s.
\item Qualitative: No numeric intervals are considered. The relationships are
computed by purely qualitative means.
}

Take the notation that \code{x} and \code{y} represent two intervals. Now assume that
\code{x} can be represented as \verb{[x_s, x_e)}, where \code{x_s} is the start of the
interval and \code{x_e} is the end of it. Additionally, assume that \code{x_s < x_e}.
With this notation, the 13 relations are as follows:
\itemize{
\item \emph{Precedes}:

\code{x_e < y_s}
\item \emph{Preceded-by}:

\code{x_s > y_e}
\item \emph{Meets}:

\code{x_e == y_s}
\item \emph{Met-by}:

\code{x_s == y_e}
\item \emph{Overlaps}:

\code{(x_s < y_s) & (x_e > y_s) & (x_e < y_e)}
\item \emph{Overlapped-by}:

\code{(x_e > y_e) & (x_s < y_e) & (x_s > y_s)}
\item \emph{Starts}:

\code{(x_s == y_s) & (x_e < y_e)}
\item \emph{Started-by}:

\code{(x_s == y_s) & (x_e > y_e)}
\item \emph{Finishes}:

\code{(x_s > y_s) & (x_e == y_e)}
\item \emph{Finished-by}:

\code{(x_s < y_s) & (x_e == y_e)}
\item \emph{During}:

\code{(x_s > y_s) & (x_e < y_e)}
\item \emph{Contains}:

\code{(x_s < y_s) & (x_e > y_e)}
\item \emph{Equals}:

\code{(x_s == y_s) & (x_e == y_e)}
}

Note that when \code{missing = "equals"}, missing intervals will only match
the \code{type = "equals"} relation. This ensures that the distinct property
of the algebra is maintained.
\subsection{Connection to other functions}{

Note that some of the above relations are fairly restrictive. For example,
\code{"overlaps"} only detects cases where \code{x} straddles \code{y_s}. It does not
consider the case where \code{x} and \code{y} are equal to be an overlap (as this
is \code{"equals"}) nor does it consider when \code{x} straddles \code{y_e} to be an
overlap (as this is \code{"overlapped-by"}). This makes the relations extremely
useful from a theoretical perspective, because they can be combined without
fear of duplicating relations, but they don't match our typical expectations
for what an "overlap" is.

\code{\link[=iv_locate_overlaps]{iv_locate_overlaps()}}, \code{\link[=iv_locate_precedes]{iv_locate_precedes()}}, and \code{\link[=iv_locate_follows]{iv_locate_follows()}} use
more intuitive \code{type}s that aren't distinct, but typically match your
expectations better. They can each be expressed in terms of Allen's
relations:
\itemize{
\item \code{iv_locate_overlaps()}:
\itemize{
\item \code{"any"}:

\code{overlaps | overlapped-by | starts | started-by | finishes | finished-by | during | contains | equals}
\item \code{"contains"}:

\code{contains | started-by | finished-by | equals}
\item \code{"within"}:

\code{during | starts | finishes | equals}
\item \code{"starts"}:

\code{starts | started-by | equals}
\item \code{"ends"}:

\code{finishes | finished-by | equals}
\item \code{"equals"}:

\code{equals}
}
\item \code{iv_locate_precedes()}:

\code{precedes | meets}
\item \code{iv_locate_follows()}:

\code{preceded-by | met-by}
}
}
}

\examples{
x <- iv(1, 3)
y <- iv(3, 4)

# `"precedes"` is strict, and doesn't let the endpoints match
iv_locate_relates(x, y, type = "precedes")

# Since that is what `"meets"` represents
iv_locate_relates(x, y, type = "meets")

# `"overlaps"` is a very specific type of overlap where an interval in
# `needles` straddles the start of an interval in `haystack`
x <- iv_pairs(c(1, 4), c(1, 3), c(0, 3), c(2, 5))
y <- iv(1, 4)

# It doesn't match equality, or when the starts match, or when the end
# of the interval in `haystack` is straddled instead
iv_locate_relates(x, y, type = "overlaps")
}
\references{
Allen, James F. (26 November 1983). "Maintaining knowledge about temporal
intervals". Communications of the ACM. 26 (11): 832–843.
}
\seealso{
\link[=relation-locate]{Locating relationships}

\link[=allen-relation-detect]{Detecting relations from Allen's Interval Algebra}

\link[=allen-relation-detect-pairwise]{Detecting relations from Allen's Interval Algebra pairwise}
}
