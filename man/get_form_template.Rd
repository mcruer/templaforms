% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_form.R
\name{get_form_template}
\alias{get_form_template}
\title{Get Specific Form Information from \code{all_forms}}
\usage{
get_form_template(name, form.version = NULL)
}
\arguments{
\item{name}{The name of the form to be retrieved.}

\item{form.version}{The version of the form, if specific version is needed. Default is NULL.}
}
\value{
A tibble containing the \code{variable_info}, \code{table_info}, and \code{column_info} of the requested form.
}
\description{
This function retrieves specific information about a form based on its name and optionally, its version.
It returns relevant columns (\code{variable_info}, \code{table_info}, \code{column_info}) for the specified form.
}
\examples{
\dontrun{
get_form_template("example")
get_form_template("example", form.version = "v1.0.0")
}
}
