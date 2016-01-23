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


#' Extract MDS Components
#'
#' Extract the six basic components from a given MDS.
#'
#' @param x A Character string.
#' @return A character vector with the following six components:
#'   \describe{
#'     \item{\code{"status_prefix"}}{status prefix;}
#'     \item{\code{"modified_mission"}}{modified mission;}
#'     \item{\code{"basic_mission"}}{basic mission;}
#'     \item{\code{"vehicle_type"}}{vehicle type;}
#'     \item{\code{"design_number"}}{design number;}
#'     \item{\code{"series_letter"}}{series letter.}
#'   }
#' @importFrom stringr str_extract
#' @export
#' @examples
#' # A single MDS
#' get_mds_components("F-017")
#'
#' # Multiple MDS
#' mds <- c("YEH-060B", "F17", "F-17", "F017", "F-017", "YRAH-066A")
#' plyr::ldply(mds, get_mds_components)
get_mds_components <- function(x) {
  x <- standardize_mds(x)
  design_number <- gsub("[A-Z]+-?", "", str_extract(x, "[A-Z]+-?[0-9]+"))
  series_letter <- str_extract(x, "[ABCDEFGHJKLMNPQRSTUVWXYZ]$")
  vehicle_type <- str_extract(x, "^[A-Z]+")
  c("status_prefix"    = NA,
    "modified_mission" = NA,
    "basic_mission"    = NA,
    "vehicle_type"     = vehicle_type,
    "design_number"    = design_number,
    "series_letter"     = series_letter)
}
