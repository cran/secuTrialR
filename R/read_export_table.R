# Load an individual file/table from a secuTrial export archive
#
# @description
# This function can load individual tables from a secuTrial data
# export. If id translations for data tables shall be performed, the casenodes/cn,
# centres/ctr and visitplan/vp meta tables need to be loaded with this function first.
# Also export options need to be generated with read_export_options beforehand.
#
# This is an internal function which is wrapped by read_secuTrial_raw
#
# @param data_dir string The data_dir specifies the path to the secuTrial data export.
# @param file_name string The file_name specifies which file to load.
# @param export_options list This can be generated with read_export_options.
# @param add_pat_id boolean If TRUE this will add the pat_id to the table.
# @param add_centre boolean If TRUE this will add the centre name to the table.
# @param add_visitname boolean If TRUE this will add visit names to the table.
# @param casenodes_table data.frame This data.frame must be created with this function
#                      before any secuTrial data tables are read.
#                      Is obsolete if is_meta_table == FALSE. (see examples)
# @param centre_table data.frame This data.frame must be created with this function
#                      before any other secuTrial data tables are read.
#                      Is obsolete if is_meta_table == FALSE. (see examples)
# @param visitplan_table data.frame This data.frame must be created with this function
#                      before any other secuTrial data tables are read.
#                      Is obsolete if is_meta_table == FALSE. (see examples)
# @param is_meta_table boolean If TRUE the table is treated as a meta data table (e.g. id translations are ignored).
# @param ... for passing other options to read functions
#
# @return The function returns a data.frame for the data in file_name.
#
# @seealso read_export_options
#
# @examples
# \donttest{
# nolint start
# # load casenodes (casenodes/cn) table
# casenodes <- secuTrialR:::read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
#                                                                  "s_export_CSV-xls_BMD_short_en_utf8.zip",
#                                                                  package = "secuTrialR"),
#                                           file_name = "cn.xls",
#                                           export_options = export_options,
#                                           is_meta_table = TRUE)
#
# # load centre (centres/ctr) table
# centre <- secuTrialR:::read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
#                                                                 "s_export_CSV-xls_BMD_short_en_utf8.zip",
#                                                                 package = "secuTrialR"),
#                                          file_name = "ctr.xls",
#                                          export_options = export_options,
#                                          is_meta_table = TRUE)
#
# # load visitplan (visitplan/vp) table
# visitplan <- secuTrialR:::read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
#                                                                    "s_export_CSV-xls_BMD_short_en_utf8.zip",
#                                                                    package = "secuTrialR"),
#                                             file_name = "vp.xls",
#                                             export_options = export_options,
#                                             is_meta_table = TRUE)
#
# # load bone mineral denisty form data
# bmd <- secuTrialR:::read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
#                                                              "s_export_CSV-xls_BMD_short_en_utf8.zip",
#                                                              package = "secuTrialR"),
#                                       file_name = "bmd.xls",
#                                       export_options = export_options,
#                                       casenodes_table = casenodes,
#                                       centre_table = centre,
#                                       visitplan_table = visitplan)
# }
# nolint end
#
#' @importFrom utils type.convert
read_export_table <- function(data_dir, file_name, export_options,
                              add_pat_id = TRUE, add_centre = TRUE, add_visitname = TRUE,
                              casenodes_table, centre_table, visitplan_table,
                              is_meta_table = FALSE,
                              sep = export_options$sep, quote = export_options$quote,
                              ...) {
  ops <- options()
  on.exit(ops)
  options(stringsAsFactors = FALSE)

  # ISO encoding must be names "latin1"
  curr_encoding <- export_options$encoding
  if (curr_encoding == "ISO-8859-1" | curr_encoding == "ISO-8859-15") {
    curr_encoding <- "latin1"
  }

  if (export_options$is_zip) {
    archive_con <- unz(data_dir, file_name)
    table_lines <- readr::read_lines(archive_con, locale = readr::locale(encoding = curr_encoding))
  } else if (export_options$is_zip == FALSE) {
    table_lines <- readr::read_lines(paste0(data_dir, "/", file_name), locale = readr::locale(encoding = curr_encoding))
  } else {
    stop(paste0("Could not load table ", file_name))
  }

  # Some tables may be non-rectangular due to additional 
  # empty cells at the end of some rows
  # In such a case, fill rows to make table rectangular
  # (analogous to fill = TRUE in read.table())
  nsep <- sapply(table_lines, function(x) stringr::str_count(x, paste0(quote,sep,quote)), USE.NAMES = FALSE)
  if (any(nsep) != max(nsep)) {
    table_lines[nsep != max(nsep)] <- paste0(table_lines[nsep != max(nsep)], sep, quote, quote)
  }

  loaded_table <- readr::read_delim(file = I(table_lines),
                                    na = export_options$na.strings,
                                    delim = sep,
                                    quote = quote,
                                    # do not attempt to change the names of any last/empty column
                                    name_repair = "minimal",
                                    # do not convert columns
                                    col_types = cols(.default = "c"),
                                    # escape special characters with backslash
                                    escape_backslash = TRUE,
                                    escape_double = FALSE,
                                    ...)
  
  # Remove any last empty column (columns without name)
  loaded_table <- loaded_table[, names(loaded_table) != ""]
  
  # Convert table to data.frame, convert column types with read.table() default type.convert()
  loaded_table <- as.data.frame(lapply(loaded_table, function(x) type.convert(x, as.is = TRUE)))

  # do not manipulate meta tables
  if (is_meta_table) {
    return(loaded_table)
  }

  # adding pat_id (only possible if Add-ID was exported in ExportSearchTool)
  if (add_pat_id & ("mnppid" %in% names(loaded_table))) {
    if (export_options$add_id) {
      if("pat_id" %in% names(loaded_table)){
        warning("variable 'pat_id' replaced with the additional id")
      }
      loaded_table <- add_pat_id_col(table = loaded_table,
                                     id = "pat_id",
                                     casenodes_table = casenodes_table)
    }
  }

  # adding centres (only possible if centre information was exported in ExportSearchTool)
  if (add_centre & "mnppid" %in% names(loaded_table) & export_options$centre_info) {
    loaded_table <- add_centre_col(table = loaded_table, id = "centre",
                                   remove_ctag = FALSE, casenodes_table = casenodes_table,
                                   centre_table = centre_table)
  }

  # adding visit names (only possible if Project Setup was exported in ExportSearchTool)
  if (add_visitname & "mnpvisid" %in% names(loaded_table) & export_options$proj_setup)  {
    loaded_table <- add_visitname_col(table = loaded_table, id = "visit_name",
                                      visitplan_table = visitplan_table)
  }

  return(loaded_table)
}
