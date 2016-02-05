#' Mission Design Series (MDS)
#'
#' Various functions for working with MDS codes.
#'
#' @param x A character vector.
#' @param hyphen Logicial indicating whether or not to include a hyphen before
#'   the design number (if one is not already present). Only used in
#'   \code{standardize_mds}.
#' @param ... Additional optional arguments.
#' @rdname mds
#' @export
#' @examples
#' mds <- c("YEH-60B", "YEH-60B1", "AYEH-60B", "F-16A", "F16", "F016", "YRAH-66A")
#'
#' is_valid_mds(mds)
#'
#' mds %>%
#'   strip_leading_zeros %>%
#'   strip_hyphen
#'
#' mds %>%
#'   strip_leading_zeros %>%
#'   add_hyphen
is_valid_mds <- function(x, ...) {
  grepl(MDS_REGEX, x, ...)
}


#' @rdname mds
#' @export
is_invalid_mds <- function(x, ...) {
  !is_valid_mds(x, ...)
}


#' @rdname mds
#' @export
get_valid_mds <- function(x, ...) {
  grep(MDS_REGEX, x, value = TRUE, ...)
}


#' @rdname mds
#' @export
get_invalid_mds <- function(x, ...) {
  grep(paste0("!", MDS_REGEX), x, value = TRUE, ...)
}


#' @rdname mds
#' @export
add_hyphen <- function(x, ...) {
  gsub('^([A-Z]+)([0-9]+)', '\\1-\\2', x, ...)
}


#' @rdname mds
#' @export
strip_hyphen <- function(x, ...) {
  gsub("-?", replacement = "", x, ...)
}


#' @rdname mds
#' @export
strip_leading_zeros <- function(x, ...) {
  gsub("^([A-Z]*-?)(0*)", replacement = "\\1", x)
}


#' @rdname mds
#' @export
standardize_mds <- function(x, hyphen = TRUE, ...) {
  if (hyphen) {
    x %>%
      strip_leading_zeros(...) %>%
      add_hyphen(...)
  } else {
    x %>%
      strip_leading_zeros(...) %>%
      strip_hyphen(...)
  }
}
