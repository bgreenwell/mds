#' Constants
#'
#' Constants defining various regular expressions related to the various
#' components of a mission design series (MDS).
#'
#' @rdname constants
#' @details
#' Coming soon!
#' @export
VEHICLE_TYPE <- "[DGHQSVZ]"


#' @rdname constants
#' @export
BASIC_MISSION <- "[ABCEFKLOPRSTUX]"


#' @rdname constants
#' @export
MODIFIED_MISSION <- "[ACDEFHKLMOPQRSTUVW]"


#' @rdname constants
#' @export
DESIGN_NUMBER <- "[0-9]"


#' @rdname constants
#' @export
SERIES_LETTER <- "[ABCDEFGHJKLMNPQRSTUVWXYZ]"  # omits I and O


#' @rdname constants
#' @export
STATUS_PREFIX <- "[GJNXYZ]"


#' @rdname constants
#' @export
MDS_REGEX <- paste0("^",
                      ###########################
                      # Status prefix is optional
                      ###########################
                      STATUS_PREFIX, "?",
                      ##############################
                      # Modified mission is optional
                      ##############################
                      MODIFIED_MISSION, "?",
                      ###################################################
                      # May contain basic mission OR vehicle type OR both
                      ###################################################
                      "(", BASIC_MISSION, "|", VEHICLE_TYPE, "|",
                      paste0(BASIC_MISSION, VEHICLE_TYPE), ")",
                      ####################
                      # Hyphen is optional
                      ####################
                      "-?",
                      #####################################################
                      # Should contain at least one digit for design number
                      #####################################################
                      DESIGN_NUMBER, "+",
                      ###########################
                      # Series letter is optional
                      ###########################
                      SERIES_LETTER, "?",
                      "$")
