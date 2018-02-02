#' Read the contents of a DataCamp group export into R
#'
#' @param path Either the zip file downloaded from the website (must end in
#'   \code{.zip}), or the unzipped directory containing the exported files.
#'
#' @seealso \code{\link{courses}}, \code{\link{missing_courses}}, and
#'   \code{\link{users}} for extracting course and user information from the
#'   object returned by this function.
#'
#' @export
read_group_export <- function(path) {
  # If this is a .zip archive, extract its contents somewhere temporary.
  if (grepl(".zip", path)) {
    tmppath <- paste0(tempdir(), "/", gsub(".zip", "", path))
    message("unzipping in: ", tmppath)
    utils::unzip(path, overwrite = TRUE, exdir = tmppath)
    path <- tmppath
  }

  course_files <- grep("course_", dir(path), fixed = TRUE, value = TRUE)
  if (length(course_files) < 1) {
    stop("No DataCamp course data found")
  }

  overview_file <- grep("overview", dir(path), fixed = TRUE, value = TRUE)
  if (length(overview_file) != 1) {
    stop("An identifiable DataCamp overview file must be present")
  }

  raw_overview <- readr::read_csv(paste0(path, "/", overview_file),
                                  # Suppress column messages.
                                  col_types = readr::cols())

  user_ids <- generate_user_ids(raw_overview)

  course_metadata <- course_metadata()

  clean_overview <- raw_overview %>%
    dplyr::left_join(user_ids, by = c("user_name", "user_email")) %>%
    tidy_course_data(overview = TRUE) %>%
    filter(status != "Not Yet Started") %>%
    select(-course_id) %>%
    dplyr::left_join(course_metadata,
                     by = c(course = "course_overview_name")) %>%
    select(-course_excel_name)

  clean_courses <-
    tibble::tibble(filename = paste0(path, "/", course_files)) %>%
    mutate(course_id = as.integer(gsub(".*course_([0-9]+).csv", "\\1",
                                       filename)),
           course_data = purrr::map2(filename, course_id, function(f, id) {
             readr::read_csv(f, col_types = readr::cols()) %>%
               dplyr::left_join(user_ids, by = c("user_name",
                                                 "user_email")) %>%
               tidy_course_data(overview = FALSE, course_id = id) %>%
               filter(status != "Not Yet Started")
           })) %>%
    select(course_data) %>%
    tidyr::unnest(course_data) %>%
    select(-course) %>%
    dplyr::left_join(course_metadata, by = "course_id") %>%
    dplyr::rename(course = course_overview_name)

  # Check if there are any NAs in the course name <--> id mapping.
  if (anyNA(clean_overview$course_id) || anyNA(clean_courses$course)) {
    warning(paste("Some courses are missing names or IDs. Use",
                  "missing_courses() to diagnose which ones."))
  }

  clean_full <- dplyr::bind_rows(clean_overview, clean_courses) %>%
    select(user_id, course, course_id, chapter, free, status, start_date,
           completed_date, grade, completion)

  structure(list(courses = clean_full, users = user_ids),
            class = "datacamp_export")
}

# Generate a data frame mapping hashed user IDs to user names and emails from
# a raw overview data frame.
generate_user_ids <- function(.data, algo = "crc32") {
  .data %>%
    select(user_email, user_name) %>%
    group_by(user_email, user_name) %>%
    mutate(user_id = digest::digest(paste0(user_name, "|", user_email),
                                    algo = algo)) %>%
    ungroup() %>%
    dplyr::distinct() %>%
    arrange(user_email) %>%
    select(user_email, user_name, user_id)
}

# Parse the datacamp chapter/overview file format into a tidy data frame.
tidy_course_data <- function(.data, overview = FALSE, course_id = NA_integer_) {
  grades <- .data %>%
    select(user_id, dplyr::ends_with("_grade")) %>%
    tidyr::gather(course, grade, -user_id) %>%
    mutate(course = gsub("_grade", "", course))

  starts <- .data %>%
    select(user_id, dplyr::ends_with("_start")) %>%
    tidyr::gather(course, start, -user_id) %>%
    mutate(course = gsub("_start", "", course))

  completion <- .data %>%
    select(user_id, dplyr::ends_with("_completion")) %>%
    tidyr::gather(course, completion, -user_id) %>%
    mutate(course = gsub("_completion", "", course))

  completed <- .data %>%
    select(user_id, dplyr::ends_with("_completed")) %>%
    tidyr::gather(course, completed, -user_id) %>%
    mutate(course = gsub("_completed", "", course))

  clean <- .data %>%
    select(user_id) %>%
    dplyr::left_join(grades, by = "user_id") %>%
    dplyr::left_join(starts, by = c("user_id", "course")) %>%
    dplyr::left_join(completion, by = c("user_id", "course")) %>%
    dplyr::left_join(completed, by = c("user_id", "course")) %>%
    # Do some basic cleanup of the course/chapter names.
    mutate(course = gsub("__", " - ", course),
           course = gsub("_", " ", course),
           start_date = ifelse(start == "/", NA, start),
           completed_date = ifelse(completed == "/", NA, completed),
           # Grades for non-started courses should really be NA.
           grade = ifelse(is.na(start_date), NA, grade),
           status = dplyr::case_when(!is.na(completed_date) ~ "Completed",
                                     !is.na(start_date) ~ "Started",
                                     TRUE ~ "Not Yet Started") %>%
             factor(levels = c("Not Yet Started", "Started", "Completed")))

  # For the overview, courses are course names, and the chapter is "Overview".
  if (overview) {
    clean <- clean %>%
      mutate(chapter = "Overview", course_id = course_id)
  } else {
    clean <- clean %>%
      dplyr::rename(chapter = course) %>%
      mutate(course = NA_character_, course_id = course_id)
  }

  clean %>%
    select(user_id, course, course_id, chapter, start_date, completed_date,
           grade, completion, status)
}

# Load metadata from a system file, so that it can be edited by hand.
course_metadata <- function() {
  fname <- system.file("course_metadata.csv", package = "readcamp")
  readr::read_csv(fname, col_types = readr::cols())
}

# Silence visible binding errors.
utils::globalVariables(c("user_email", "user_name", "user_id", "status",
                         "course_id", "course_excel_name", "filename",
                         "course_data", "course", "course_overview_name",
                         "chapter", "free", "start_date", "completed_date",
                         "grade", "completion", "start"))

#' @importFrom dplyr %>% select mutate filter arrange group_by ungroup
NULL
