#' Read Imaging Data from ABCDS Directory
#'
#' Reads imaging scan data (amyloid, tau, MRI, or FDG) for participants and/or
#' controls from a specified directory. Optionally merges demographic information.
#'
#' @param directory Character string specifying the path to the ABCDS data
#'   directory. If \code{NULL}, uses a default directory checked by
#'   \code{check_abcds_directory()}.
#' @param person Character vector specifying which groups to include. Options
#'   are \code{"participants"}, \code{"controls"}, or both. Default is
#'   \code{c("participants", "controls")}.
#' @param include_demographics Logical indicating whether to merge demographic
#'   data with the imaging data. Default is \code{FALSE}.
#' @param scan Character string specifying the type of imaging scan to read.
#'   Must be one of:
#'   \itemize{
#'     \item \code{"amy"} - Amyloid PET scans
#'     \item \code{"tau"} - Tau PET scans
#'     \item \code{"mri"} - MRI scans
#'     \item \code{"fdg"} - FDG PET scans
#'   }
#'
#' @return Returns imaging data as a tibble or list depending on the
#'   \code{person} argument:
#'   \itemize{
#'     \item If both participants and controls are requested and found, returns
#'       a named list with elements \code{participants} and \code{controls}.
#'     \item If only one group is requested or found, returns a single tibble
#'       for that group.
#'     \item Throws an error if no imaging files are found.
#'   }
#'
#' @details
#' The function searches for CSV files in the specified directory that match
#' the pattern "Scan" and the requested scan type. Files are identified as
#' belonging to participants or controls based on the filename (files containing
#' "Controls" are classified as control data).
#'
#' The \code{update_stamp} column is automatically removed if present in the data.
#'
#' When \code{include_demographics = TRUE}, demographic data is merged using
#' the following key columns: \code{subject_label}, \code{event_sequence},
#' \code{language_code}, and \code{language_label}.
#'
#' @examples
#' \dontrun{
#' # Read tau PET scans for participants only
#' tau_data <- read_imaging(scan = "tau", person = "participants")
#'
#' # Read amyloid scans for both groups with demographics
#' amy_data <- read_imaging(
#'   scan = "amy",
#'   person = c("participants", "controls"),
#'   include_demographics = TRUE
#' )
#'
#' # Access separate datasets
#' participants_amy <- amy_data$participants
#' controls_amy <- amy_data$controls
#' }
#'
#' @seealso \code{\link{read_demographics}}, \code{\link{check_abcds_directory}}
#'
#' @export

read_imaging <- function(
  directory = NULL,
  person = c("participants", "controls"),
  include_demographics = FALSE,
  scan = c("amy", "tau", "mri", "fdg")
) {
  directory <- check_abcds_directory(directory)
  scan <- match.arg(scan)

  if (length(person) == 1) {
    person <- match.arg(person)
  }

  scan_files <- list.files(directory, pattern = "Scan", full.names = TRUE)
  files <- scan_files[grepl(scan, scan_files, ignore.case = TRUE)]
  person_types <- unname(sapply(files, .detect_person))

  if ("participants" %in% person & "participant" %in% person_types) {
    participants <- utils::read.csv(files[!grepl("Controls", files)])
    if ("update_stamp" %in% colnames(participants)) {
      participants$update_stamp <- NULL
    }
    if (include_demographics) {
      participants <- merge(
        read_demographics(
          directory,
          person = "participants"
        ),
        participants,
        by = c(
          "subject_label",
          "event_sequence",
          "language_code",
          "language_label"
        )
      )
    }
    class(participants) <- c("tbl_df", "tbl", "data.frame")
  }

  if ("controls" %in% person & "control" %in% person_types) {
    controls <- utils::read.csv(files[grepl("Controls", files)])
    if ("update_stamp" %in% colnames(controls)) {
      controls$update_stamp <- NULL
    }
    if (include_demographics) {
      controls <- merge(
        read_demographics(
          directory,
          person = "controls"
        ),
        controls,
        by = c(
          "subject_label",
          "event_sequence",
          "language_code",
          "language_label"
        )
      )
    }
    class(controls) <- c("tbl_df", "tbl", "data.frame")
  }

  if (
    exists("participants", inherits = FALSE) &
      exists("controls", inherits = FALSE)
  ) {
    return(list(participants = participants, controls = controls))
  } else if (exists("participants", inherits = FALSE)) {
    return(participants)
  } else if (exists("controls", inherits = FALSE)) {
    return(controls)
  } else {
    stop("Did not find any health history files")
  }
}
