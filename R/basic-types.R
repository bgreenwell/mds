#' Mission Design Series Basic Type
#' 
#' Abc.
#'
#' @param x A character vector.
#' @param ... Additional optional arguments to be passed onto \code{grepl}.
#'
#' @details
#' The \code{is_Y} functions return TRUE if \code{x} includes the basic mission 
#' symbol \code{Y}, and FALSE otherwise. 
#' 
#' The function \code{get_basic_mission} will extract and return the basic 
#' mission symbol, if any.
#'
#' @rdname basic-type
#' @export
is_ground_attack <- function(x, ...) {
  grepl("A[DGHQSVZ]?-?[0-9]{1,3}[ABCDEFGHJKLMNPQRSTUVWXYZ]*", x, ...)
}


#' @rdname basic-type
#' @export
is_bomber <- function(x, ...) {
  grepl("B[DGHQSVZ]?-?[0-9]{1,3}[ABCDEFGHJKLMNPQRSTUVWXYZ]*", x, ...)
}


#' @rdname basic-type
#' @export
is_transport <- function(x, ...) {
  grepl("C[DGHQSVZ]?-?[0-9]{1,3}[ABCDEFGHJKLMNPQRSTUVWXYZ]*", x, ...)
}


#' @rdname basic-type
#' @export
is_fighter <- function(x, ...) {
  grepl("F[DGHQSVZ]?-?[0-9]{1,3}[ABCDEFGHJKLMNPQRSTUVWXYZ]*", x, ...)
}


#' @rdname basic-type
#' @export
is_tanker <- function(x, ...) {
  grepl("K[DGHQSVZ]?-?[0-9]{1,3}[ABCDEFGHJKLMNPQRSTUVWXYZ]*", x, ...)
}


#' @rdname basic-type
#' @export
is_multi_mission <- function(x, ...) {
  grepl("M[DGHQSVZ]?-?[0-9]{1,3}[ABCDEFGHJKLMNPQRSTUVWXYZ]*", x, ...)
}


#' @rdname basic-type
#' @export
is_trainer <- function(x, ...) {
  grepl("T[DGHQSVZ]?-?[0-9]{1,3}[ABCDEFGHJKLMNPQRSTUVWXYZ]*", x, ...)
}


#' @rdname basic-type
#' @export
get_bassic_mission <- function(x, ...) {
  ifelse(is_ground_attack(x, ...), "A", 
  ifelse(is_bomber(x, ...), "B", 
  ifelse(is_transport(x, ...), "C", 
  ifelse(is_fighter(x, ...), "F",
  ifelse(is_tanker(x, ...), "K",
  ifelse(is_multi_mission(x, ...), "M",
  ifelse(is_trainer(x, ...), "T", "other")))))))
}
