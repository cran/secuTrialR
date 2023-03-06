## ---- include = FALSE---------------------------------------------------------
# needed so that the as.data.frame part of the vignette
# does not need a restart of the session everytime the
# vignette is built
#rm(list = ls()) # removed this at the request of the CRAN submission
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("secuTrialR", dependencies = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  # install
#  devtools::install_github("SwissClinicalTrialOrganisation/secuTrialR")

## -----------------------------------------------------------------------------
# load silently
suppressMessages(library(secuTrialR))
# show secuTrialR version
packageVersion("secuTrialR")

## -----------------------------------------------------------------------------
ctu05_data_location <- system.file("extdata", "sT_exports", "exp_opt",
                                   "s_export_CSV-xls_CTU05_all_info.zip",
                                   package = "secuTrialR")

## -----------------------------------------------------------------------------
ctu05_data <- read_secuTrial(data_dir = ctu05_data_location)

## -----------------------------------------------------------------------------
class(ctu05_data)

## -----------------------------------------------------------------------------
typeof(ctu05_data)

## -----------------------------------------------------------------------------
print(ctu05_data)

## -----------------------------------------------------------------------------
table(ctu05_data$treatment$rando_treatment)
table(ctu05_data$allmedi$med_product)

## -----------------------------------------------------------------------------
# raw gender data
ctu05_data$baseline$gender

# transformed gender data
ctu05_data$baseline$gender.factor

# raw more meds
ctu05_data$allmedi$no_more_meds

# transformed more meds
ctu05_data$allmedi$no_more_meds.factor

## -----------------------------------------------------------------------------
label(ctu05_data$allmedi$no_more_meds.factor)
label(ctu05_data$baseline$gender.factor)
label(ctu05_data$esurgeries$surgery_organ.factor)

## -----------------------------------------------------------------------------
# raw
ctu05_data$baseline$visit_date

# processed
ctu05_data$baseline$visit_date.date

# raw only head
head(ctu05_data$baseline$hiv_date)

# processed only head
head(ctu05_data$baseline$hiv_date.datetime)

# classes
class(ctu05_data$baseline$visit_date)
class(ctu05_data$baseline$visit_date.date)
class(ctu05_data$baseline$hiv_date)
class(ctu05_data$baseline$hiv_date.datetime)


## -----------------------------------------------------------------------------
ctu05_data$export_options

## -----------------------------------------------------------------------------
ctu05_data$export_options$project_name
ctu05_data$export_options$encoding

## -----------------------------------------------------------------------------
names(ctu05_data$export_options)

## -----------------------------------------------------------------------------
get_participants(ctu05_data)

## -----------------------------------------------------------------------------
annual_recruitment(ctu05_data)

## -----------------------------------------------------------------------------
annual_recruitment(ctu05_data, rm_regex = "\\(.*\\)$")

## ---- fig.height = 3.6, fig.width = 8-----------------------------------------
plot_recruitment(ctu05_data, cex = 1.2, rm_regex = "\\(.*\\)$")

## ---- fig.height = 3.9, fig.width = 3.9---------------------------------------
vs <- visit_structure(ctu05_data)
plot(vs)

## -----------------------------------------------------------------------------
fss <- form_status_summary(ctu05_data)
tail(fss, n = 5)

## -----------------------------------------------------------------------------
fsc <- form_status_counts(ctu05_data)
# show the top
head(fsc)

## ---- eval = FALSE------------------------------------------------------------
#  links_secuTrial(ctu05_data)

## -----------------------------------------------------------------------------
# randomly retrieve at least 25 percent of participants recorded after March 18th 2019
# from the centres "Inselspital Bern" and "Charité Berlin"
return_random_participants(ctu05_data,
                           percent = 0.25,
                           seed = 1337,
                           date = "2019-03-18",
                           centres = c("Inselspital Bern (RPACK)",
                                       "Charité Berlin (RPACK)"))

## -----------------------------------------------------------------------------
return_scores(ctu05_data)

## -----------------------------------------------------------------------------
return_hidden_items(ctu05_data)

## -----------------------------------------------------------------------------
ctu06_v1 <- read_secuTrial(system.file("extdata", "sT_exports", "change_tracking",
                                       "s_export_CSV-xls_CTU06_version1.zip",
                                       package = "secuTrialR"))

ctu06_v2 <- read_secuTrial(system.file("extdata", "sT_exports", "change_tracking",
                                       "s_export_CSV-xls_CTU06_version2.zip",
                                       package = "secuTrialR"))

diff_secuTrial(ctu06_v1, ctu06_v2)


## ---- eval = FALSE------------------------------------------------------------
#  # retrieve path to a temporary directory
#  tdir <- tempdir()
#  # write spss
#  write_secuTrial(ctu05_data, format = "sav", path = tdir)

## -----------------------------------------------------------------------------
# initialize some subset identifiers
participants <- c("RPACK-INS-011", "RPACK-INS-014", "RPACK-INS-015")
centres <- c("Inselspital Bern (RPACK)", "Universitätsspital Basel (RPACK)")

# exclude Bern and Basel
ctu05_data_berlin <- subset_secuTrial(ctu05_data, centre = centres, exclude = TRUE)
# exclude Berlin
ctu05_data_bern_basel <- subset_secuTrial(ctu05_data, centre = centres)
# keep only subset of participants
ctu05_data_pids <- subset_secuTrial(ctu05_data, participant = participants)

class(ctu05_data_berlin)
class(ctu05_data_bern_basel)
class(ctu05_data_pids)

## -----------------------------------------------------------------------------
# only Berlin remains
ctu05_data_berlin$ctr

# all centres remain even though all three participant ids are from Bern
ctu05_data_pids$ctr

## ---- fig.height = 3.8, fig.width = 8-----------------------------------------
# keep only Bern
ctu05_data_bern <- subset_secuTrial(ctu05_data, centre = "Inselspital Bern (RPACK)")
# plot
plot_recruitment(ctu05_data_bern)

## ---- fig.height = 3.8, fig.width = 8-----------------------------------------
# keep only Bern and Berlin
ctu05_data_bern_berlin <- subset_secuTrial(ctu05_data,
                                           centre = c("Inselspital Bern (RPACK)",
                                                      "Charité Berlin (RPACK)"))
# plot
plot_recruitment(ctu05_data_bern_berlin)

## -----------------------------------------------------------------------------
head(ctu05_data$treatment$mnpdocid)
head(ctu05_data$baseline$mnpdocid)

## -----------------------------------------------------------------------------
server <- "server.secutrial.com"
instance <- "ST21-setup-DataCapture"
customer <- "TES"
project <- "7036"

# make three links with the first three baseline docids
bl_docids <- head(ctu05_data$baseline$mnpdocid, n = 3)
links <- build_secuTrial_url(server, instance, customer,
                             project, bl_docids)

## -----------------------------------------------------------------------------
env <- new.env()
ls(env)

## -----------------------------------------------------------------------------
# add files to env
as.data.frame(ctu05_data, envir = env)

## -----------------------------------------------------------------------------
ls(env)

## ---- echo = FALSE, results = TRUE--------------------------------------------

# incomplete dates
warning(
"In dates_secuTrial.data.frame(tmp, datevars, timevars, dateformat,  :
Not all dates were converted for
variable: 'a_variable_name'
in form: 'a_form_name'
This is likely due to incomplete date entries."
)


## ---- echo = FALSE, results = TRUE--------------------------------------------
# duplicate factors
warning(
"In factorize_secuTrial.data.frame(curr_form_data, cl = object$cl,  :
Duplicate values found during factorization of a_variable_name")

## ---- echo = FALSE, results = TRUE--------------------------------------------
# duplicate labels
warning(
"In label_secuTrial.secuTrialdata(d) :
The labels attribute may be longer than 1 for the following variables and forms.
Likely the label was changed from its original state in the secuTrial project setup.
variables: a_variable_name
forms: a_form_name"
)

## -----------------------------------------------------------------------------
treatment_shrink <- ctu05_data$treatment[, c("mnpcvpid", "rando_treatment")]

## -----------------------------------------------------------------------------
bl_treat <- merge(x = ctu05_data$baseline, y = treatment_shrink,
                  by = "mnpcvpid", all.x = TRUE)
# check dimensions
dim(ctu05_data$baseline)
dim(bl_treat)

## -----------------------------------------------------------------------------
bl_surg <- merge(x = ctu05_data$baseline, y = ctu05_data$esurgeries, by = "mnpdocid")

## -----------------------------------------------------------------------------
table(ctu05_data$esurgeries$mnpdocid)

## -----------------------------------------------------------------------------
# before merge
table(ctu05_data$baseline$height)

# after merge
table(bl_surg$height)

## -----------------------------------------------------------------------------
# write a temporary object
surg <- ctu05_data$esurgeries[, c("mnpdocid", "surgery_organ.factor")]
# only retain non NA rows
surg <- surg[which(! is.na(surg$surgery_organ.factor)), ]
# show it
surg

## -----------------------------------------------------------------------------
library(tidyr) # pivot_wider
# add a count
surg$count <- 1
# show the data
surg

# make it wide
surg_wide <- pivot_wider(surg, names_from = surgery_organ.factor, values_from = count)
# show the wide data
surg_wide


## -----------------------------------------------------------------------------
# merge
bl_surg_no_dup <- merge(x = ctu05_data$baseline, y = surg_wide,
                        by = "mnpdocid", all.x = TRUE)

# compare dimensions
dim(bl_surg_no_dup)
dim(ctu05_data$baseline)

# check the height variable
table(bl_surg_no_dup$height)

## -----------------------------------------------------------------------------
sessionInfo()

