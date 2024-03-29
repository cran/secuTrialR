% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asdataframe.R
\name{as.data.frame.secuTrialdata}
\alias{as.data.frame.secuTrialdata}
\title{as.data.frame method for secuTrialdata objects
Make the data from the exports more easily accessible by placing them in
another environment (e.g. place them in the global environment
(\code{.GlobalEnv}) and you can reference them without referring to the
\code{secuTrialdata} object anymore. Ie. they become regular \code{data.frame}s).}
\usage{
\method{as.data.frame}{secuTrialdata}(
  x,
  ...,
  envir,
  data.frames = NULL,
  meta = FALSE,
  regex = NULL,
  rep = ""
)
}
\arguments{
\item{x}{\code{secuTrialdata} object}

\item{...}{further parameters}

\item{envir}{environment in which to put the data (e.g. \code{.GlobalEnv})}

\item{data.frames}{character vector of data.frame names to turn into data.frames.
If the vector is named,data.frames will be renamed into the non-empty names(data.frames)}

\item{meta}{logical should metadata be returned}

\item{regex}{regex syntax to remove from names}

\item{rep}{replacement for regex}
}
\value{
each \code{data.frame} in the \code{secuTrialdata} object is saved to it's
own \code{data.frame} in the designated environment
}
\description{
as.data.frame method for secuTrialdata objects
Make the data from the exports more easily accessible by placing them in
another environment (e.g. place them in the global environment
(\code{.GlobalEnv}) and you can reference them without referring to the
\code{secuTrialdata} object anymore. Ie. they become regular \code{data.frame}s).
}
\details{
\code{envir} must be specifically defined. For simplicity,
\code{.GlobalEnv} would probably be the easiest (assigning it to another
environment would still entail referring to that environment).
}
\examples{
# prepare path to example export
export_location <- system.file("extdata", "sT_exports", "lnames",
                               "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
                               package = "secuTrialR")
# load all export data
sT_export <- read_secuTrial_raw(data_dir = export_location)
# add files to a new environment called env1
env1 <- new.env()
as.data.frame(sT_export, envir = env1)
# add files to a new environment called env2, removing the project name from
# the file names
env2 <- new.env()
as.data.frame(sT_export, regex = "ctu05", envir = env2)
# add files to a new environment called env3, renaming files in data.frames
env3 <- new.env()
as.data.frame(sT_export, data.frames = c("allmedications" = "ctu05allmedi", "ctu05baseline"),
              envir = env3)
}
