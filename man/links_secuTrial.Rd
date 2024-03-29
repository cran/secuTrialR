% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/links_secuTrial.R
\name{links_secuTrial}
\alias{links_secuTrial}
\title{Show links between forms}
\usage{
links_secuTrial(
  object,
  forms = NULL,
  formcol = "#d8b365",
  varcol = "#e5f5f9",
  plot = TRUE
)
}
\arguments{
\item{object}{\code{secuTrialdata} object}

\item{forms}{a regular expression for which forms should be included}

\item{formcol}{color for form name circles}

\item{varcol}{color for variable name circles}

\item{plot}{boolean specifies if the plot should be shown}
}
\value{
a tcltk plot window.
}
\description{
secuTrial creates a large number of files and identifiers with which to link files together.
Understanding the links can be difficult. This function produces a map linking the forms
with common variables.
}
\details{
We recommend to resize the tcltk window and and click view/"fit to screen" to improve readability.
         Forms are colored dull orange, variables are colored light blue.
}
\note{
Note that where a form name is also a variable name, it is appended by \code{_form}
      (igraph requires uniquely named nodes).
}
\examples{
\donttest{
# ex. 1
# prepare path to example export
export_location <- system.file("extdata", "sT_exports", "BMD",
                               "s_export_CSV-xls_BMD_short_en_utf8.zip",
                               package = "secuTrialR")
# load all export data
sT_export <- read_secuTrial_raw(data_dir = export_location)
# plot links
links_secuTrial(sT_export)

# ex. 2
# prepare path to example export
export_location <- system.file("extdata", "sT_exports", "lnames",
                               "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
                               package = "secuTrialR")
# load all export data
sT_export <- read_secuTrial_raw(data_dir = export_location)
# plot links for form names starting with "ctu05"
links_secuTrial(sT_export, forms = "^ctu05")
}
}
