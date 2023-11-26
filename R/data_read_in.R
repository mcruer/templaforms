#' Retrieve File Information for a Single Path
#'
#' This function returns a tibble with details about files in a given folder path.
#'
#' @param folder_path Character. The path of the folder from which files' information is to be extracted.
#' @param file_type Character. Type or extension of the files to be considered. Default is "." (all files).
#' @param recursive Logical. Should files from subdirectories also be included? Default is FALSE.
#' @param filter_out_tilda Logical. Should files with names containing a tilde (~) be excluded? Default is TRUE.
#'
#' @return A tibble containing columns: file (name of the file), path (complete path of the file),
#' size (file size), isdir (is it a directory?), and mtime (last modification time).
#'
#' @examples
#' \dontrun{
#' file_tibble_single_path(folder_path = "./my_directory")
#' }
#' #' @keywords internal
file_tibble_single_path <-
  function (folder_path,
            file_type = ".",
            recursive = FALSE,
            filter_out_tilda = TRUE) {

    if(stringr::str_sub(folder_path, start = -1L) != "/"){
      folder_path <- stringr::str_c(folder_path, "/")
    }

    #list.files(folder_path)
    tibble::tibble (file = list.files(folder_path, recursive = recursive),
                    path = stringr::str_c(folder_path, file),
                    info = purrr::map(path, ~.x %>%
                                        file.info () %>%
                                        tibble::tibble () %>%
                                        dplyr::select (size, isdir, mtime)
                    )) %>%
      tidyr::unnest(info) %>%
      gplyr::filter_in(file, stringr::str_c(file_type, "$")) %>%
      dplyr::filter(!(filter_out_tilda & stringr::str_detect(file, "~")))
  }

metadata_columns <- c(
  "folder_paths",
  "file_type",
  "recursive",
  "filter_out_tilda",
  "size",
  "isdir",
  "mtime",
  "update_needed_raw",
  "update_needed_form",
  "form_name_version",
  "sheets",
  "sheets_regex"
)

#' Re-Nest Metadata
#'
#' This function aggregates file information from multiple folder paths into a single tibble.
#'
#' @param df A dataframe containing columns to be contained within the metatdata column.
#'
#' @return A tibble with a nested metadata column.
#'
#' @export
nest_metadata <- function (df) {
  df %>%
    tidyr::nest(metadata = dplyr::any_of(metadata_columns))
}

#' Retrieve File Information for Multiple Paths
#'
#' This function aggregates file information from multiple folder paths into a single tibble.
#'
#' @param folder_paths Character vector. Paths of the folders from which files' information is to be extracted.
#' @param file_type Character. Type or extension of the files to be considered. Default is "." (all files).
#' @param recursive Logical. Should files from subdirectories also be included? Default is FALSE.
#' @param filter_out_tilda Logical. Should files with names containing a tilde (~) be excluded? Default is TRUE.
#'
#' @return A tibble with one row per unique file-path combination, with columns: folder_paths (list of all folder paths provided),
#' file_type (type of the file considered), recursive (were subdirectories included?), filter_out_tilda (were files with tildes excluded?),
#' file (name of the file), path (complete path of the file), and metadata (a list column containing details like size, isdir, and mtime).
#'
#' @examples
#' \dontrun{
#' file_tibble(folder_paths = c("./dir1", "./dir2"))
#' }
#' @export
file_tibble <-
  function (folder_paths,
            file_type = ".",
            recursive = FALSE,
            filter_out_tilda = TRUE) {

    #Loop over the folder_paths to create a single tibble.
    dplyr::bind_rows(
      purrr::map(folder_paths,
                 file_tibble_single_path,
                 file_type = file_type,
                 recursive = recursive,
                 filter_out_tilda = filter_out_tilda)
    ) %>%
      #create the contents of the metadata tibble and nest it to create a single
      #column.
      dplyr::mutate(folder_paths = list(folder_paths),
                    file_type = file_type,
                    recursive = recursive,
                    filter_out_tilda = filter_out_tilda,
                    update_needed_raw = TRUE,
                    update_needed_form = TRUE,
                    .before = 1) %>%
      nest_metadata()

  }





#' Internal Helper to Add Data Column from Excel Files
#'
#' The `add_data_column` function is an internal helper function that adds a column
#' to a tibble containing data extracted from Excel files.
#'
#' @param df A tibble.
#' @param column_name The name of the new column to be added.
#' @param sheets A character vector specifying which sheets to extract from the Excel files.
#' @param sheets_regex A regular expression pattern to match against sheet names in the
#'                     Excel files.
#'
#' @return A tibble with the original data and an additional column containing data from
#'         the specified Excel sheets.
#' @importFrom rlang :=
#' @keywords internal
add_data_column <- function (df, column_name, sheets, sheets_regex) {
  df %>%
    dplyr::mutate ({{column_name}} := purrr::map (path,
                                                  reformR::read_excel_all,
                                                  sheets = sheets,
                                                  sheets_regex = sheets_regex,
                                                  .progress = "text"))
}

add_form_name_version_to_metadata <- function (df) {

  get_form_name_version_from_raw <- function(raw_df) {
    gplyr::pull_cell(raw_df, 3, 1)
  }

  df %>%
    tidyr::unnest(metadata) %>%
    dplyr::mutate(
      form_name_version = extract_form_from_string(file),
      first_in_raw = purrr::map_chr(raw_df, get_form_name_version_from_raw),
      form_name_version = dplyr::if_else(is.na(form_name_version), first_in_raw, form_name_version)
    ) %>%
    dplyr::select(-first_in_raw) %>%
    nest_metadata()
}


#' Add Data Column to file_tibble
#'
#' The `get_raw_xlsx` function takes as input a tibble generated by the `file_tibble`
#' function and returns it with an added column `raw_df`. This column contains the data
#' from all sheets of the corresponding Excel files.
#'
#' @param df A tibble generated by the `file_tibble` function.
#' @param sheets A character vector specifying which sheets to extract from the Excel files.
#'               If NULL (default), all sheets are extracted.
#' @param sheets_regex A regular expression pattern to match against sheet names in the
#'                     Excel files. Defaults to ".*" to match all sheets.
#'
#' @return A tibble with the original data and an additional list column `raw_df` that
#'         contains the data from the Excel files.
#'
#' @examples
#' \dontrun{
#' # Assume `file_tibble` has been previously run to generate `original_tibble`
#' raw_data_tibble <- get_raw_xlsx(original_tibble)
#' }
#'
#' @export
get_raw_xlsx <- function (df, sheets = NULL, sheets_regex = ".*"){

  df %>%
    add_data_column("raw_df",
                    sheets = sheets,
                    sheets_regex = sheets_regex) %>%
    add_form_name_version_to_metadata() %>%
    tidyr::unnest(metadata) %>%
    dplyr::mutate(update_needed_raw = FALSE) %>%
    dplyr::mutate(sheets = ifelse(is.null(sheets), NA_character_, sheets),
                  sheets_regex = sheets_regex) %>%
    nest_metadata()
}

#' Add a form name/version to a file_tibble
#'
#' This is a helper function that is only necessary for forms that don't already have the form name/verison in the top left corner of the first sheet.
#'
#' @param df A tibble containing the data to be reformed.
#' @param new_form_name_version A string containing the name_version of a form template. Format is "name.v1.0.0".
#'
#' @return A tibble with a nested metadata column that contains the new_form_name_version
#'
#'
#' @export
add_form_name_version <- function (df, new_form_name_version) {
  df %>%
    tidyr::unnest(metadata) %>%
    dplyr::mutate (form_name_version = new_form_name_version) %>%
    nest_metadata()
}

#' Reform a Tibble Using Form Metadata
#'
#' This function uses metadata from a specified `column` to reform a tibble.
#' It utilizes `get_form_template` to extract specific data based on the form name and version,
#' and applies transformations to create a new tibble.
#'
#' @param df A tibble containing the data to be reformed.
#'
#' @return A tibble with a new column named `output` containing the reformed data.
#'
#' @examples
#' \dontrun{
#' reform_tib(my_data, "raw_form_column")
#' }
#'
#' @seealso
#' \code{\link{get_form_template}} for extracting form metadata.
#'
#' @export
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom tidyr unnest
reform_tib <- function (df) {

  get_form_reform <-    function (raw.form,
                                  form_name_version) {
    form_name <- stringr::str_remove(form_name_version, "\\.v\\d.*")
    form_version <- stringr::str_extract(form_name_version, "v\\d.*")

    reformR::reform(raw.form, get_form_template(form_name, form_version))

  }

  updated <- df %>%
    unnest(metadata) %>%
    dplyr::mutate (output = purrr::map2(raw_df, form_name_version, get_form_reform, .progress = "text"),
                   update_needed_form = FALSE) %>%
    tidyr::unnest(output) %>%
    nest_metadata()

}


#' Update Metadata Tibble for Files
#'
#' The `update_file_tibble` function takes as input a tibble that was originally generated
#' by the `file_tibble` function. This function checks for any changes in the file metadata
#' (e.g., additions, removals, or updates of files in the specified directories) and updates
#' the tibble accordingly.
#'
#' @param df A tibble generated by the `file_tibble` function containing file metadata and
#'           file paths.
#'
#' @return A tibble updated with the current metadata for the files. This includes any new files,
#'         removed files, or files that have seen metadata changes since the original tibble was
#'         generated. The tibble will also contain an 'update' column that is marked TRUE for
#'         files that have been added or changed.
#'
#' @details
#' The function first unpacks the metadata of the given `df`. It then retrieves the necessary
#' parameters like `folder_paths`, `file_type`, `recursive`, and `filter_out_tilda`. A new tibble
#' is generated using the `file_tibble` function with these parameters.
#'
#' The function identifies any new rows (i.e., files) that have been added since the original
#' tibble was generated. It also identifies rows that are present in both the old and new tibbles
#' but have different values for the `size`, `isdir`, or `mtime` metadata fields.
#'
#' Based on this comparison, the function updates the 'update_needed_raw' and 'update_needed_r'
#' columns in the tibble. Rows corresponding to newly added or changed files
#' are marked TRUE in both 'update_' columns.
#'
#' @references The function uses several functions from the 'tidyverse' and makes references to
#'             other custom packages. Ensure the necessary dependencies are installed and loaded.
#'
#' @examples
#' \dontrun{
#' # Assume `file_tibble` has been previously run to generate `original_tibble`
#' updated_tibble <- update_file_tibble(original_tibble)
#' }
#'
#' @export
update_file_tibble <- function(df) {
  old <- df %>%
    tidyr::unnest(metadata) %>%
    dplyr::select(-dplyr::all_of(c("update_needed_raw", "update_needed_form")))

  folder_paths <- old %>%
    gplyr::pull_cell(folder_paths)
  file_type <- old %>%
    gplyr::pull_cell(file_type)
  recursive <- old %>%
    gplyr::pull_cell(recursive)
  filter_out_tilda <- old %>%
    gplyr::pull_cell(filter_out_tilda)

  new <- file_tibble(folder_paths,
                     file_type = file_type,
                     recursive = recursive,
                     filter_out_tilda = filter_out_tilda
  ) %>%
    tidyr::unnest(metadata)

  if("raw_df" %in% names(old)) {
    new <- new %>%
      dplyr::left_join(old %>% select(-c(folder_paths, file_type, recursive,
                                         filter_out_tilda, size, isdir, mtime,
                                         file)))
  }

  # Identify rows in 'new' that are not in 'old'
  added_rows <- dplyr::anti_join(new, old, by = "path")

  # Identify rows in 'new' that are in 'old' but have different values for size, isdir, or mtime
  changed_rows <- dplyr::inner_join(new, old, by = "path", suffix = c(".new", ".old")) %>%
    dplyr::filter(
      size.new != size.old |
        isdir.new != isdir.old |
        mtime.new != mtime.old
    )

  # Mark the rows accordingly
  new <- new %>%
    dplyr::mutate(
      added = path %in% added_rows$path,
      changed = path %in% changed_rows$path
    ) %>%
    dplyr::mutate (update_needed_raw = added | changed,
                   update_needed_form = update_needed_raw) %>%
    dplyr::select(-added, - changed) %>%
    dplyr::group_by(file, path) %>%
    nest_metadata ()%>%
    dplyr::relocate(metadata, .before =1) %>%
    dplyr::ungroup()

  return(new)

}


#' Update Data from Excel Files Based on Metadata Changes
#'
#' The `update_raw_xlsx` function takes a tibble generated by `get_raw_xlsx` as input.
#' It checks if any files in the tibble have updated metadata, and if so, updates the
#' `raw_df` column with the latest data from those files.
#'
#' @param df A tibble generated by the `get_raw_xlsx` function.
#'
#' @return A tibble with the original data and updated `raw_df` column for any files with
#'         changed metadata.
#'
#' @details If the input tibble does not have a `raw_df` column, the function will raise an error.
#'
#' @examples
#' \dontrun{
#' # Assume `get_raw_xlsx` has been previously run to generate `raw_data_tibble`
#' updated_data_tibble <- udate_raw_xlsx(raw_data_tibble)
#' }
#'
#' @export
update_raw_xlsx <- function (df){
  #paths column name: "path"
  #data column name: "raw_df"
  #update column name: "update"
  #temp column name: "temp"
  #The arguments sheets and sheets_regex are stored as columns in the meta_data

  # replace_if_null <- function(old, new) {
  #   purrr::map2(old, new, ~ {if (is.null(.y)) .x else .y})
  # }

  if(!"raw_df" %in% names(df)){
    stop ("No raw_df column detected. Did you forget to use get_raw_df()?")
  }

  sheets <- gplyr::pull_cell(df %>% tidyr::unnest(metadata), sheets)
  if(is.na(sheets)) {sheets <- NULL}

  sheets_regex <- gplyr::pull_cell(df %>% tidyr::unnest(metadata), sheets_regex)

  df <- df %>%
    update_file_tibble() %>%
    tidyr::unnest(metadata) %>%
    gplyr::add_index()


  do_update <- df %>%
    dplyr::filter(update_needed_raw) %>%
    nest_metadata() %>%
    dplyr::select(index, file, path, metadata)

  if (nrow(do_update)>0){
    do_update <- do_update%>%
      get_raw_xlsx(sheets, sheets_regex)
  }

  dont_update <- df %>%
    dplyr::filter(!update_needed_raw) %>%
    nest_metadata()

  dplyr::bind_rows(do_update, dont_update) %>%
    dplyr::arrange (index) %>%
    dplyr::select (-index) %>%
    tidyr::unnest(metadata) %>%
    dplyr::mutate(update_needed_raw = FALSE) %>%
    nest_metadata() %>%
    add_form_name_version_to_metadata()
}

#' Update Form Metadata and Data
#'
#' The `update_forms` function modifies the given tibble that contains both raw data and metadata
#' to reflect changes in form structure or content as indicated by the metadata flags.
#'
#' @param df A tibble that contains a list of file paths, raw data frames, and metadata.
#'           This tibble is expected to have been processed by `get_raw_xlsx` and possibly `update_raw_xlsx`.
#'
#' @return A tibble with updated form metadata and associated data. Each item with `update_needed_form` set to TRUE
#'         will have its metadata and associated data updated.
#'
#' @details The function checks the `update_needed_form` flag for each item in the tibble. If the flag
#'          is TRUE, it performs the necessary updates based on the new form structure or content. The resulting
#'          tibble will have `update_needed_form` set to FALSE for all items.
#'
#' @examples
#' \dontrun{
#' # Assuming `df` is a tibble that has been flagged for updates
#' updated_forms <- update_forms(df)
#' }
#'
#' @export
update_forms <- function(df) {
  df <- df %>%
    tidyr::unnest(metadata) %>%
    gplyr::add_index()

  dont_update <- df %>%
    dplyr::filter(!update_needed_form)

  do_update <- df %>%
    dplyr::filter(update_needed_form) %>%
    dplyr::select(index, file, path, raw_df, dplyr::any_of(metadata_columns))

  if (nrow(do_update) > 0) {
    do_update <- do_update %>%
      nest_metadata() %>%
      reform_tib() %>%
      tidyr::unnest(metadata)
  }
  dplyr::bind_rows(dont_update, do_update) %>%
    dplyr::arrange(index) %>%
    dplyr::select(-index) %>%
    dplyr::mutate(update_needed_form = FALSE) %>%
    nest_metadata()
}


#' Update Entire Dataset Based on Raw Data and Form Metadata Changes
#'
#' The `update_all` function combines the functionalities of `update_raw_xlsx` and `update_forms`
#' to update an entire dataset based on changes in the file system or form structures.
#'
#' @param df A tibble that contains a list of file paths, raw data frames, and metadata, with possible
#'           updates pending in both raw data and form structure as indicated by respective flags.
#'
#' @return A fully updated tibble where both raw data and form metadata reflect the latest changes from
#'         the file system and form definitions.
#'
#' @details The function applies updates sequentially first to the raw data using `update_raw_xlsx`
#'          and then to the form metadata using `update_forms`. It ensures that the dataset reflects
#'          the current state of the file system and the latest form structures.
#'
#' @examples
#' \dontrun{
#' # Assuming `df` is the initial dataset tibble with updates pending
#' fully_updated_dataset <- update_all(df)
#' }
#'
#' @export
update_all <- function(df) {
  df %>%
    update_raw_xlsx() %>%
    update_forms()
}


