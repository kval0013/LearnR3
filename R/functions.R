# Functions ----

read <- function(file_path, max_rows = 100) {
  data <- file_path %>%
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows,
    )
  return(data)
}

read_all <- function(filename, max_rows = 100) {
  files <- .DATASET_DIR |>
    fs::dir_ls(regexp = filename, recurse = TRUE)
  data <- files |>
    purrr::map(\(file) read(file, max_rows = max_rows)) |>
    purrr::list_rbind(names_to = "file_path_id")
  return(data)


  get_participant_id <- function(data) {
    data_with_id <- data |>
      dplyr::mutate(
        ID = stringr::str_extract( # Extracts complete match from the argument below
          file_path_id,
          pattern = "(?<=/stress/)[:alnum:]{2}(?=/)" #(?<=) tells the str_extract to look for things preceeded by the following part inbetween //, the (?=/) tells the str_extract to look for the searched for items only if they match before a backlash.
        ),
        .before = file_path_id # Tells R to place the new column before the current column called file_path_id
      ) |>
      dplyr::select(!c(file_path_id))
    return(data_with_id)
  }



  # Global variables ----
  .DATASET_DIR <- here::here("data-raw/nurses-stress/")
}
