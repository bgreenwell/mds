#' Mission Design Series Basic Type
#' 
#' Abc.
#'
#' @param x A character vector.
#' @param ... Additional optional arguments to be passed onto \code{grepl}.
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
