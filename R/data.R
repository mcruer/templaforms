#' Meta-Data Tags for Excel-Based Forms
#'
#' A dataset containing meta-data tags used to extract information
#' from excel-based forms within an organization. The dataset serves
#' as a central repository to manage changes in form structures and
#' versions.
#'
#' @format A tibble with 2 rows and 6 variables:
#' \describe{
#'   \item{\code{files}}{The file name of the template that contains the meta-data.}
#'   \item{\code{version}}{The version number of the form.}
#'   \item{\code{form.name}}{The name of the form.}
#'   \item{\code{variable_info}}{A list-column containing meta-data related to individual variables (not tables).}
#'   \item{\code{table_info}}{A list-column containing meta-data related to tables within the forms.}
#'   \item{\code{column_info}}{A list-column containing meta-data related to columns within each of the tables.}
#' }
#' @source Internal data collected and maintained for form management.
#' @seealso Other datasets and functions in the \pkg{templaforms} package can refer to this dataset for form management.
#' @keywords dataset
#' @name all_forms
#' @docType data
#' @usage data(all_forms)
"all_forms"
