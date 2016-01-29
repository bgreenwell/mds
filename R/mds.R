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

  # Return vector of NAs for invalid MDS
  if (is_invalid_mds(x)) {
    return(c("status_prefix" = NA,
             "modified_mission" = NA,
             "basic_mission" = NA,
             "vehicle_type" = NA,
             "design_number" = NA,
             "series_letter" = NA))
  }

  # Standardize the MDS
  x <- standardize_mds(x, hyphen = TRUE)

  # Extract the design number and series letter (if present)
  design_number <- gsub("[A-Z]+-?", "", str_extract(x, "[A-Z]+-?[0-9]+"))
  series_letter <- gsub("-[0-9]+", "", str_extract(x, "-[0-9]+[ABCDEFGHJKLMNPQRSTUVWXYZ]$"))

  # Separate the letters to the left of the hyphen
  tokens <- strsplit(str_extract(x, "^[A-Z]+"))[[1]]

  # Trivial cases
  if (length(tokens) == 4) {
    status_prefix <- tokens[1]
    modified_mission <- tokens[2]
    basic_mission <- tokens[3]
    vehicle_type <- tokens[4]
  } else if (length(tokens) == 1) {
    status_prefix <- NA
    modified_mission <- NA
    basic_mission <- tokens
    vehicle_type <- NA
  } else if (length(tokens > 4)) {
    status_prefix <- NA
    modified_mission <- NA
    basic_mission <- NA
    vehicle_type <- NA
  } else {
    # Check the type of aircraft. If it is anything other than an airplane (e.g.
    # heavier than air, atmospheric craft) you will see one of the following
    # symbols immediately to the left of the hyphen. Otherwise, skip to the next
    # step.
    vehicle_type <- if (grepl(VEHICLE_TYPE, tokens[length(tokens)])) {
      tokens[length(tokens)]
    } else {
      NA
    }

    # Determine the basic mission. The letter immediately to the left of the dash
    # (when a type designation is not present) indicates the basic mission purpose
    # of that aircraft. Occasionally, the basic mission designation is left out if
    # the type and the modified mission (see next step) are included (e.g. MQ-9A).
    basic_mission <- if (is.na(vehicle_type) &&
                         grepl(BASIC_MISSION, tokens[length(tokens)])) {
      tokens[length(tokens)]
    } else if (!is.na(vehicle_type) &&
               length(tokens) == 3 &&
               grepl(BASIC_MISSION, tokens[length(tokens) - 1])) {
      tokens[length(tokens) - 1]
    } else if (!is.na(vehicle_type) &&
               length(tokens) == 2 &&
               grepl(BASIC_MISSION, tokens[length(tokens) - 1])) {
      NA
    } else {
      NA
    }

    # Find the modified mission. The letter left of the basic mission designation
    # indicates that a particular aircraft has been optionally modified for a
    # mission different than its original design purpose. There should only be one
    # letter for the modified mission designation, but there are a few exceptions
    # (e.g. EKA-3B). These symbols are similar to the basic mission symbols, but
    # contain a few extra descriptors.
    modified_mission <- if (!is.na(basic_mission) ) {

    } else {
      NA
    }

    # See if there is a status prefix. If this symbol is present, it will be all
    # the way to the left, and it is only needed if an aircraft is not in normal
    # operational service.
    status_prefix <- if (grepl(STATUS_PREFIX, tokens[1L])) {
      tokens[1L]
    } else {
      NA
    }
  }

  # Return named vector of results
  c("status_prefix"    = status_prefix,
    "modified_mission" = modified_mission,
    "basic_mission"    = basic_mission,
    "vehicle_type"     = vehicle_type,
    "design_number"    = design_number,
    "series_letter"     = series_letter)
}
