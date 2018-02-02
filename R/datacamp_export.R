#' @export
print.datacamp_export <- function(x, ...) {
  cat("A DataCamp Group Export containing the following course dataset:\n")
  print(x$courses)
}

#' Extract tidy data from DataCamp exports
#'
#' @param x An object of class \code{"datacamp_export"}.
#'
#' @export
#' @rdname datacamp_export
courses <- function(x) {
  UseMethod("courses")
}

#' @export
courses.datacamp_export <- function(x) {
  x$courses
}

#' @export
#' @rdname datacamp_export
users <- function(x) {
  UseMethod("users")
}

#' @export
users.datacamp_export <- function(x) {
  x$users
}

#' @export
#' @rdname datacamp_export
missing_courses <- function(x) {
  UseMethod("missing_courses")
}

#' @export
missing_courses.datacamp_export <- function(x) {
  x$courses %>%
    filter(is.na(course) | is.na(course_id)) %>%
    select(course, course_id)
}
