# Functions ----

read <- function(file_path, max_rows = Inf) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows,
    )
  return(data)
}

read_all <- function(filename, max_rows = Inf) {
  files <- .DATASET_DIR |>
    fs::dir_ls(regexp = filename, recurse = TRUE)
  data <- files |>
    purrr::map(\(file) read(file, max_rows = max_rows)) |>
    purrr::list_rbind(names_to = "file_path_id")
  return(data)
}


get_participant_id <- function(data) {
  data_with_id <- data |>
    dplyr::mutate(
      id = stringr::str_extract( # Extracts complete match from the argument below
        file_path_id,
        pattern = "(?<=/stress/)[:alnum:]{2}(?=/)" # (?<=) tells the str_extract to look for things preceeded by the following part inbetween //, the (?=/) tells the str_extract to look for the searched for items only if they match before a backlash.
      ),
      .before = file_path_id # Tells R to place the new column before the current column called file_path_id
    ) |>
    dplyr::select(!c(file_path_id))
  return(data_with_id)
}


summarise_by_datetime <- function(data, unit = "minute") {
  summarised_data <- data |>
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute"
      )
    ) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric),
        list(
          mean = mean,
          median = median,
          SD = sd
        )
      ),
      .by = c(id, collection_datetime) # Within summarise you'll keep track of a group_by approach by using .by. It further adds the listed functions as applied to each rounded minute and add those togehter.
    )
  return(summarised_data)
}


read_sensor_data <- function(filename, max_rows = 100, unit = "minute") {
  data <- read_all(filename, max_rows = max_rows) |>
    get_participant_id() |>
    summarise_by_datetime(unit = unit)
  return(data)
}


#' Tidy survey data dates
#'
#' @param data Uses survey data with unstructured time recording for start and end time of when survey was performed.
#'
#' @returns A tidid dataframe were the date-time format matches the quantitative data and further adding an additional column based on the start_time for surveying.
#' @export
#'
#' @examples
tidy_survey_dates <- function(data) {
  tidied <- data |>
    dplyr::mutate(
      date = lubridate::mdy(date),
      start_datetime = lubridate::as_datetime(paste(date, start_time)),
      end_datetime = lubridate::as_datetime(paste(date, end_time)),
      datetime_id = start_datetime,
      .before = start_time
    ) |>
    dplyr::select(-c(start_time, end_time, duration, date))
  return(tidied)
}

#' Pivot survey data columns to long format
#'
#' @param data Tidied survey data with reformatted date-time column.
#'
#' @returns A pivoted tibble where start_datetime and end_datetime is combined in one column, and previous value name removed. Further, added minutes.
survey_to_long <- function(data) {
  longer <- data |>
    dplyr::select(id, datetime_id, start_datetime, end_datetime) |>
    tidyr::pivot_longer(c(start_datetime, end_datetime), names_to = NULL, values_to = "collection_datetime") |> # We add a new column called 'name' were the previous column-names for the selected columns that were combined in one new column named value. Names_to = null removes the name column.
    dplyr::group_by(dplyr::pick(-collection_datetime)) |>
    tidyr::complete(collection_datetime = seq(min(collection_datetime),
      max(collection_datetime),
      by = 60
    )) |> # seq can create a vector of values between something. It will by default read a difference in time as seconds, why we had to define the by = 60 to make it a minut
    dplyr::ungroup()
  return(longer)
}
# Global variables ----
.DATASET_DIR <- here::here("data-raw/nurses-stress/")
