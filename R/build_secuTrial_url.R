#' Compose a secuTrial URL
#'
#' Given a secuTrial server URL, and optionally instance, customer, project id and document id,
#' this function composes a URL to a specific secuTrial instance, customer or form.
#'
#' To find the server and instance of a secuTrial database, simply extract the information from the URL that
#' you usually use to log in. For example in:
#'
#' \emph{https://server.secutrial.com/apps/WebObjects/ST21-setup-DataCapture.woa/wa/choose?customer=TES}
#'
#' \itemize{
#' \item server id is: \strong{server.secutrial.com}
#' \item instance id is: \strong{ST21-setup-DataCapture}
#' \item url prefix is: \strong{apps/WebObjects/}
#' \item you can find the customer id at the end of the link i.e \strong{TES}
#'
#'       Alternatively it can be found in the
#'       CustomerAdminTool -> click "Kunden" -> Value in DB column of table on login page.
#' \item you can find the project id in the AdminTool -> Projects -> left side ID column in the table.
#' \item you can find docids in secuTrial exports as values in the "mnpdocid" column.
#'
#'       Alternatively they are noted in the form footers of the DataCapture.
#' }
#'
#' Also note that only the server address has to be provided, the other arguments are optional.
#' Thus, there are different scenarios:
#' \itemize{
#' \item if only server address is provided, the output will point to the secuTrial server
#' \item if secuTrial server and instance are provided, the output will point to the secuTrial instance.
#' \item if secutrial server, instance and customer id are provided, the output will point to the customer page.
#' \item if secuTrial server, instance, customer, project and document id are provided,
#'       the output will point to a specific secuTrial form.
#' }
#'
#' @param server string containing a server URL
#' @param instance (optional) string containing secuTrial instance name
#' @param customer (optional) string containing secuTrial customer label
#' @param projid (optional) string containing secuTrial project identifier
#' @param docid (optional) secuTrial document/form identifer
#' @param prefix (optional) string containing the URL prefix to the WebObjects application (default: "apps/WebObjects/")
#' @return string containing a URL to desired secuTrial page. Currently we provide no
#'         guarantee that the returned URL is valid.
#' @export
#'
#' @examples
#'
#' # This example, builds pseudo-urls that do not point to an active secuTrial instance.
#'
#' server <- "server.secutrial.com"
#' instance <- "ST21-setup-DataCapture"
#' customer <- "TES"
#' project <- "7036"
#' docid <- "181"
#'
#' build_secuTrial_url(server)
#' build_secuTrial_url(server, instance)
#' build_secuTrial_url(server, instance, customer)
#' build_secuTrial_url(server, instance, customer, project)
#' build_secuTrial_url(server, instance, customer, project, docid)
#'
#' # examples of docids (mnpdocid)
#' path <- system.file("extdata", "sT_exports", "lnames",
#'                     "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                     package = "secuTrialR")
#' sT_export <- read_secuTrial(path)
#'
#' # return docids
#' docids <- sT_export$ctu05baseline$mnpdocid
#'
#' # make several links with all docids
#' build_secuTrial_url(server, instance, customer, project, docids)
#'
build_secuTrial_url <- function(server, instance = NA, customer = NA, projid = NA, docid = NA, prefix = "apps/WebObjects/") {
  if (all(!is.na(docid)) & (is.na(projid) | is.na(customer) | is.na(instance))) {
    warning("'projid', 'customer' and 'instance' must all be provided with 'docid'")
  } else if (!is.na(projid) & (is.na(customer) | is.na(instance))) {
    warning("'customer' and 'instance' must all be provided with 'projid'")
  } else if (!is.na(customer) & is.na(instance)) {
    warning("'instance' must be provided with 'customer'")
  }
  # check completeness of server url
  if (!grepl("^https://", server)) {
    server <- paste0("https://", server)
  }
  if (!grepl("/$", server)) {
    server <- paste0(server, "/")
  }
  # start building the secuTrial url
  sT_url <- server
  # check what info is availalbe and compose with what you have
  if (!is.na(instance)) {
    sT_url <- paste0(sT_url, prefix, instance)
    if (!is.na(customer)) {
      sT_url <- paste0(sT_url, ".woa/wa/choose?customer=", customer)
      if (!is.na(projid)) {
        sT_url <- paste0(sT_url, "&projectid=", projid)
        if (all(!is.na(docid))) {
          sT_url <- paste0(sT_url, "&docid=", docid)
        }
      }
    }
  }
  return(sT_url)
}
