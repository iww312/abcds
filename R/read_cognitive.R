#' @title read_cognitive
#' @description Reads the files containing the cognitive function results from the directory
#'   for the participants and controls. The cognitive function assessments are identified as those
#'   starting with `cog` using the `dd_crf_name` variable in the data dictionary.
#' @param directory A path name containing the downloaded ABC-DS data from the
#'   University of South Carolina Laboratory of Neuro Imaging's (LONI) Image
#'   and Data Archive
#' @param person The specific group for which to read in the data, Default: c("participants", "controls")
#' @param include_demographics An optional parameter to merge demographic information
#'   before returning the data, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details Reads the files containing the cognitive function results from the
#'   directory for the participants and controls. The cognitive function
#'   assessments are identified as those starting with `cog` using the
#'   `dd_crf_name` variable in the data dictionary. End users also have an
#'   option to include the demographics with the `include_demographics` argument.
#' @rdname read_cognitive
#' @export

read_cognitive <- function(
  directory,
  person = c("participants", "controls"),
  include_demographics = FALSE
) {
  if (length(person) == 1) {
    person <- match.arg(person)
  }

  load(system.file("extdata/data_dictionary.RData", package = "abcds"))

  cog_labels <- gsub(
    "\u2013| |:|/",
    "_",
    unique(data_dictionary[
      startsWith(data_dictionary$dd_crf_name, "cog"),
      "dd_crf_label"
    ])
  )

  if ("participants" %in% person) {
    participants <- data.frame()
    for (i in cog_labels[!grepl("Montreal", cog_labels)]) {
      file <- list.files(directory, pattern = i, full.names = TRUE)
      if (length(file) != 0) {
        temp_df <- utils::read.csv(file)
      }
      if ("update_stamp" %in% colnames(temp_df)) {
        temp_df$update_stamp <- NULL
      }
      if (all(dim(participants) == 0)) {
        participants <- temp_df
      } else {
        participants <- merge(
          participants,
          temp_df,
          by = c(
            "subject_label",
            "event_sequence",
            "language_code",
            "language_label"
          ),
          all = TRUE
        )
      }
    }
    if (include_demographics) {
      participants <- merge(
        read_demographics(directory),
        participants,
        by = c(
          "subject_label",
          "event_sequence",
          "language_code",
          "language_label"
        )
      )
    }
    participants <- add_latency(
      directory,
      data = participants,
      person = "participants",
      visit = "clinical"
    )
    participants <- participants[
      order(participants$subject_label, participants$event_sequence),
    ]
    class(participants) <- c("tbl_df", "tbl", "data.frame")
  }

  if ("controls" %in% person) {
    controls <- utils::read.csv(
      list.files(
        directory,
        pattern = cog_labels[grepl("Montreal", cog_labels)],
        full.names = TRUE
      )
    )
    if ("update_stamp" %in% colnames(controls)) {
      controls$update_stamp <- NULL
    }
    if (include_demographics) {
      controls <- merge(
        read_demographics(
          directory,
          controls = TRUE
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
    controls <- add_latency(
      directory,
      data = controls,
      person = "controls",
      visit = "clinical"
    )
    controls <- controls[
      order(controls$subject_label, controls$event_sequence),
    ]
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
    stop("Did not find any cognitive files")
  }
}
