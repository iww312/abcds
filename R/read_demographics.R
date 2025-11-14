#' @title read_demographics
#' @description Reads the file containing the demographics from the directory
#'   for the participants and controls.
#' @param directory A path name containing the downloaded ABC-DS data from the
#'   University of South Carolina Laboratory of Neuro Imaging's (LONI) Image
#'   and Data Archive
#' @param person The specific group for which to read in the data, Default: c("participants", "controls")
#' @param event_sequence Harmonized event sequence referring to the data collection time point, Default: NULL
#' @param simplify Reduce the demographic variables to only include age,
#'   gender, race, and ethnicity, Default: FALSE
#' @return A data frame of the demographics of the participants or controls or
#'   a list containing the demographics of the participants and controls.
#' @details Reads the file containing the demographics from the directory
#'   for the participants and controls. End users also have an option to simplify
#'   the demographics included using the `simplify` argument or return the
#'   demographics at a specific time point using `event_sequence`. Currently, the
#'   `simplify` argument returns the `age_at_visit`, `de_gender`, `de_race`, and
#'   `de_ethnicity` variables.
#' @rdname read_demographics
#' @importFrom readr read_csv
#' @export

read_demographics <- function(
  directory = NULL,
  person = c("participants", "controls"),
  event_sequence = NULL,
  simplify = FALSE
) {
  directory <- check_abcds_directory(directory)

  if (length(person) == 1) {
    person <- match.arg(person)
  }

  demographic_files <- list.files(
    directory,
    pattern = "Demographics",
    full.names = TRUE
  )

  age_and_latency_files <- list.files(
    directory,
    pattern = "Age_at_Event",
    full.names = TRUE
  )

  person_types <- unname(sapply(demographic_files, .detect_person))

  simplified_demographics <- c(
    "age_at_visit",
    "de_gender",
    "de_race",
    "de_ethnicity"
  )

  if ("participants" %in% person & "participant" %in% person_types) {
    participants <- readr::read_csv(
      demographic_files[
        !grepl("Controls", demographic_files)
      ],
      show_col_types = FALSE
    )
    if ("update_stamp" %in% colnames(participants)) {
      participants$update_stamp <- NULL
    }
    participants_age <- readr::read_csv(
      age_and_latency_files[
        !grepl("Controls", age_and_latency_files)
      ],
      show_col_types = FALSE
    )
    participants <- merge(
      participants,
      participants_age[, c("subject_label", "event_sequence", "age_at_visit")],
      by = c("subject_label", "event_sequence")
    )
    if (is.null(event_sequence)) {
      event_sequence <- max(participants$event_sequence)
    }
    participants <- participants[
      participants$event_sequence <= event_sequence,
    ]
    participants <- participants[
      order(participants$subject_label, participants$event_sequence),
    ]
    class(participants) <- c("tbl_df", "tbl", "data.frame")
    if (simplify) {
      participants <- participants[, c(
        "subject_label",
        "event_sequence",
        simplified_demographics
      )]
    }
  }

  if ("controls" %in% person & "control" %in% person_types) {
    controls <- readr::read_csv(
      demographic_files[grepl(
        "Controls",
        demographic_files
      )],
      show_col_types = FALSE
    )
    if ("update_stamp" %in% colnames(controls)) {
      controls$update_stamp <- NULL
    }
    controls_age <- readr::read_csv(
      age_and_latency_files[grepl(
        "Controls",
        age_and_latency_files
      )],
      show_col_types = FALSE
    )
    controls <- merge(
      controls,
      controls_age[, c("subject_label", "event_sequence", "age_at_visit")],
      by = c("subject_label", "event_sequence")
    )
    if (is.null(event_sequence)) {
      event_sequence <- max(controls$event_sequence)
    }
    controls <- controls[controls$event_sequence <= event_sequence, ]
    controls <- controls[
      order(controls$subject_label, controls$event_sequence),
    ]
    class(controls) <- c("tbl_df", "tbl", "data.frame")
    if (simplify) {
      controls <- controls[, c(
        "subject_label",
        "event_sequence",
        simplified_demographics
      )]
    }
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
    stop("Did not find any demographic files")
  }
}
