% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/completeness.R
\name{form_status_summary}
\alias{form_status_summary}
\title{A function to show summary statistics for form statuses}
\usage{
form_status_summary(object, ...)
}
\arguments{
\item{object}{\code{secuTrialdata} object}

\item{...}{further parameters}
}
\value{
data.frame summarizing the statuses for each form
}
\description{
This function warps form_status_counts and returns a
data.frame summarizing the statuses for each form.
Only saved forms are considered for the statistic.
}
\examples{
# prepare path to example export
export_location <- system.file("extdata","sT_exports", "snames",
                               "s_export_CSV-xls_CTU05_short_ref_miss_en_utf8.zip",
                               package = "secuTrialR")
# load all export data
sT_export <- read_secuTrial(data_dir = export_location)

# get form status
form_status_summary(sT_export)

}
\keyword{completeness}
\keyword{form}
\keyword{status}
