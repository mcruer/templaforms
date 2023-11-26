
#' Add form version to file name
#'
#' This internal function appends a form version string to a given file name,
#' using a specific delimiter pattern.
#'
#' @param file_name The name of the file to which the version will be appended.
#' @param form_name_version The version string to append to the file name.
#' @return A string representing the new file name with the version appended.
#' @keywords internal
#' @examples
#' \dontrun{
#' add_form_to_string("report.xlsx", "cp.v2.0.0")
#' }
add_form_to_string <- function(file_name, form_name_version) {
  l_str <- "{(-"
  r_str <- "-)}"
  stringr::str_c(l_str, form_name_version, r_str, file_name)
}

#' Extract form version from file name
#'
#' This internal function extracts the form version string from a file name,
#' assuming it follows a specific delimiter pattern.
#'
#' @param file_name_with_form The file name from which to extract the version.
#' @return A string representing the extracted version, or NA if not found.
#' @keywords internal
#' @examples
#' \dontrun{
#' extract_form_from_string("{(-cp.v2.0.0-)}report.xlsx")
#' }
extract_form_from_string <- function (file_name_with_form){
  l_str_regex <- "\\{\\(-"
  r_str_regex <- "-\\)\\}"
  stringr::str_extract(file_name_with_form, stringr::str_c("(?<=", l_str_regex, ").*(?=", r_str_regex, ")"))
}

#' Extract the file path from a full file path and name
#'
#' This function extracts the path part from a full file path and name string.
#'
#' @param file_path_and_name The full file path and name.
#' @return A string representing the file path without the file name.
#' @export
#' @examples
#' \dontrun{
#' subset_file_path("/home/user/documents/report.xlsx")
#' }
subset_file_path <- function(file_path_and_name) {
  stringr::str_extract(file_path_and_name, ".*/")
}

#' Extract the file name from a full file path and name
#'
#' This function extracts the file name from a full file path and name string.
#'
#' @param file_path_and_name The full file path and name.
#' @return A string representing just the file name.
#' @export
#' @examples
#' \dontrun{
#' subset_file_name("/home/user/documents/report.xlsx")
#' }
subset_file_name <- function(file_path_and_name) {
  stringr::str_remove(file_path_and_name, ".*/")
}

#' Rename a file with an added form version in its name
#'
#' This function renames a file to include a form version within its name,
#' following a specific pattern for embedding the version.
#'
#' @param file_path_and_name The original full file path and name.
#' @param form_name_version The version string to embed in the file name.
#' @return TRUE if the file was successfully renamed, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' add_form_to_file_name("/home/user/documents/report.xlsx", "v2.0.0")
#' }
add_form_to_file_name <- function (file_path_and_name, form_name_version) {
  from <- file_path_and_name
  to <- stringr::str_c (subset_file_path(file_path_and_name),
               subset_file_name(file_path_and_name) %>%
                 add_form_to_string(form_name_version))
  file.rename(from, to)
}

#' Rename all files in a directory with an added form version in their names
#'
#' This function renames all files in a specified directory to include a
#' form version within their names, following a specific pattern for embedding
#' the version.
#'
#' @param folder_path The path to the directory containing the files.
#' @param form_name_version The version string to embed in the file names.
#' @return Invisible NULL. Function is used for side effects.
#' @export
#' @examples
#' \dontrun{
#' add_form_to_all_file_names("/home/user/documents", "v2.0.0")
#' }
add_form_to_all_file_names <- function (folder_path, form_name_version){
  file_tibble(folder_path) %>%
    dplyr::pull(path) %>%
    purrr::walk(add_form_to_file_name, form_name_version)
}
