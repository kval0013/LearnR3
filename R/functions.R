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

  # Global variables ----
  .DATASET_DIR <- here::here("data-raw/nurses-stress/")
}
