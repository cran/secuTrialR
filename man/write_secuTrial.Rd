% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_secuTrial.R
\name{write_secuTrial}
\alias{write_secuTrial}
\alias{write_secuTrial.secuTrialdata}
\alias{write_secuTrial.data.frame}
\title{Write secuTrial exports to other formats}
\usage{
write_secuTrial(object, ...)

\method{write_secuTrial}{secuTrialdata}(object, format = "dta", metadata = FALSE, ...)

\method{write_secuTrial}{data.frame}(object, filename, path = "", format = "dta", ...)
}
\arguments{
\item{object}{\code{secuTrialdata} object}

\item{...}{further parameters}

\item{format}{format in which to save the export (one of "dta", "sav", "xpt")}

\item{metadata}{if TRUE then metadate files will also be written}

\item{filename}{file name}

\item{path}{directory where the files should be saved}
}
\value{
a list of filenames
}
\description{
Convert the export prepared in R and export it to
             SPSS (sav), Stata (dta) or SAS (xpt version 8)
             using the haven package.
}
\details{
Due to variable naming limitations in other packages, date variables are
         appended with _d (rather than _date), datetime/POSIX variables are appended
         with _dt (rather than _datetime) and factors with _f (rather than _factor).
         Further variable names may be altered in the conversion process.
         For details please refer to the \code{haven} documentation.
}
\examples{
# prepare path to example export
export_location <- system.file("extdata", "sT_exports", "lnames",
                               "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
                               package = "secuTrialR")
# load all export data
sT_export <- read_secuTrial(data_dir = export_location)
tdir <- tempdir()
write_secuTrial(sT_export, format = "dta", path = tdir)
list.files(tdir)

}
