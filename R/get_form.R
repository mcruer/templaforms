utils::globalVariables(c("all_forms", ".data", "variable_info", "table_info", "column_info", "output", "paths", "files"))


#' Get Specific Form Information from `all_forms`
#'
#' This function retrieves specific information about a form based on its name and optionally, its version.
#' It returns relevant columns (`variable_info`, `table_info`, `column_info`) for the specified form.
#'
#' @param name The name of the form to be retrieved.
#' @param form.version The version of the form, if specific version is needed. Default is NULL.
#'
#' @return A tibble containing the `variable_info`, `table_info`, and `column_info` of the requested form.
#'
#' @examples
#' \dontrun{
#' get_form_template("example")
#' get_form_template("example", form.version = "v1.0.0")
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr filter arrange select slice
get_form_template <- function(name, form.version = NULL) {

  if (is.null(form.version)) {
    result <- all_forms %>%
      dplyr::filter(.data$form.name == name) %>%
      dplyr::arrange(dplyr::desc(version)) %>%
      dplyr::slice(1) %>%
      dplyr::select (variable_info, table_info, column_info)

  } else {
    result <- all_forms %>%
      dplyr::filter(.data$form.name == name) %>%
      dplyr::filter(.data$version == form.version) %>%
      dplyr::select (variable_info, table_info, column_info)
  }

  if (nrow(result) == 0) {
    stop("No matching form found.")
  }

  return(result)

}





