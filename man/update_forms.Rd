% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_read_in.R
\name{update_forms}
\alias{update_forms}
\title{Update Form Metadata and Data}
\usage{
update_forms(df)
}
\arguments{
\item{df}{A tibble that contains a list of file paths, raw data frames, and metadata.
This tibble is expected to have been processed by \code{get_raw_xlsx} and possibly \code{update_raw_xlsx}.}
}
\value{
A tibble with updated form metadata and associated data. Each item with \code{update_needed_form} set to TRUE
will have its metadata and associated data updated.
}
\description{
The \code{update_forms} function modifies the given tibble that contains both raw data and metadata
to reflect changes in form structure or content as indicated by the metadata flags.
}
\details{
The function checks the \code{update_needed_form} flag for each item in the tibble. If the flag
is TRUE, it performs the necessary updates based on the new form structure or content. The resulting
tibble will have \code{update_needed_form} set to FALSE for all items.
}
\examples{
\dontrun{
# Assuming `df` is a tibble that has been flagged for updates
updated_forms <- update_forms(df)
}

}
