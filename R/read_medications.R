#' @title read_medications_old
#' @description Reads the file containing the medication and health history data
#'   from the directory for the participants and controls.
#' @param directory A path name containing the downloaded ABC-DS data from the
#'   University of South Carolina Laboratory of Neuro Imaging's (LONI) Image
#'   and Data Archive
#' @param person The specific group for which to read in the data, Default: c("participants", "controls")
#' @param event_sequence Harmonized event sequence referring to the data collection time point, Default: NULL
#' @param categorize Add medication category counts (e.g., number of medications per category), Default: FALSE
#' @return A data frame of the medication data of the participants or controls or
#'   a list containing the medication data of the participants and controls.
#' @details Reads the file containing the medication and health history worksheet
#'   from the directory for the participants and controls. End users have an option
#'   to add medication category counts using the `categorize` argument or return the
#'   medication data at a specific time point using `event_sequence`. When `categorize = TRUE`,
#'   the function adds columns counting the number of medications in each category.
#'   Multiple medications may be pipe-delimited (e.g., "6|9") in a single cell.
#' @rdname read_medications_old
#' @export

read_medications_old <- function(
  directory = NULL,
  person = c("participants", "controls"),
  event_sequence = NULL,
  categorize = FALSE
) {
  directory <- check_abcds_directory(directory)

  if (length(person) == 1) {
    person <- match.arg(person)
  }

  medication_files <- list.files(
    directory,
    pattern = "Medications_Health_History_Worksheet",
    full.names = TRUE
  )

  person_types <- unname(sapply(medication_files, .detect_person))

  med_categories <- c(
    "cmalz" = "alzheimers",
    "cmdep" = "depression",
    "cmanx" = "anxiety",
    "cmpsy" = "psychosis",
    "cmmood" = "mood",
    "cmepi" = "epilepsy",
    "cmhyp" = "hypertension",
    "cmstn" = "statins",
    "cmthy" = "thyroid",
    "cmhrt" = "heart",
    "cmppi" = "proton pump inhibitor",
    "cmah" = "antihistamine",
    "cmchol" = "cholesterol"
  )

  if ("participants" %in% person & "participant" %in% person_types) {
    participants <- utils::read.csv(medication_files[
      !grepl("Controls", medication_files)
    ])
    if ("update_stamp" %in% colnames(participants)) {
      participants$update_stamp <- NULL
    }

    if (!is.null(event_sequence)) {
      participants <- participants[
        participants$event_sequence <= event_sequence,
      ]
    }

    if (categorize) {
      for (subj in unique(participants$subject_label)) {
        subj_data <- participants[participants$subject_label == subj, ]

        for (event in unique(subj_data$event_sequence)) {
          event_data <- subj_data[subj_data$event_sequence == event, ]
          for (var in names(med_categories)) {
            count_col <- paste0("n_", med_categories[var])
            ex_col <- paste0(var, "ex")

            if (ex_col %in% colnames(event_data)) {
              n_meds <- 0
              for (i in 1:nrow(event_data)) {
                val <- as.character(event_data[[ex_col]][i])
                if (!is.na(val) && val != "" && val != "0") {
                  split_vals <- strsplit(val, "\\|")[[1]]
                  split_vals <- split_vals[split_vals != "" & split_vals != "0"]
                  n_meds <- n_meds + length(split_vals)
                }
              }
            } else {
              n_meds <- ifelse(any(event_data[[var]] == 1, na.rm = TRUE), 1, 0)
            }

            participants[
              participants$subject_label == subj &
                participants$event_sequence == event,
              count_col
            ] <- n_meds
          }

          count_cols <- paste0("n_", med_categories)
          total <- sum(
            participants[
              participants$subject_label == subj &
                participants$event_sequence == event,
              count_cols
            ][1, ],
            na.rm = TRUE
          )
          participants[
            participants$subject_label == subj &
              participants$event_sequence == event,
            "total_medications"
          ] <- total
        }
      }
    }

    participants <- participants[
      order(participants$subject_label, participants$event_sequence),
    ]
    class(participants) <- c("tbl_df", "tbl", "data.frame")
  }

  if ("controls" %in% person & "control" %in% person_types) {
    controls <- utils::read.csv(medication_files[grepl(
      "Controls",
      medication_files
    )])
    if ("update_stamp" %in% colnames(controls)) {
      controls$update_stamp <- NULL
    }

    if (!is.null(event_sequence)) {
      controls <- controls[controls$event_sequence <= event_sequence, ]
    }

    if (categorize) {
      for (subj in unique(controls$subject_label)) {
        subj_data <- controls[controls$subject_label == subj, ]

        for (event in unique(subj_data$event_sequence)) {
          event_data <- subj_data[subj_data$event_sequence == event, ]

          for (var in names(med_categories)) {
            count_col <- paste0("n_", med_categories[var])
            ex_col <- paste0(var, "ex")

            if (ex_col %in% colnames(event_data)) {
              n_meds <- 0
              for (i in 1:nrow(event_data)) {
                val <- as.character(event_data[[ex_col]][i])
                if (!is.na(val) && val != "" && val != "0") {
                  split_vals <- strsplit(val, "\\|")[[1]]
                  split_vals <- split_vals[split_vals != "" & split_vals != "0"]
                  n_meds <- n_meds + length(split_vals)
                }
              }
            } else {
              n_meds <- ifelse(any(event_data[[var]] == 1, na.rm = TRUE), 1, 0)
            }

            controls[
              controls$subject_label == subj &
                controls$event_sequence == event,
              count_col
            ] <- n_meds
          }

          count_cols <- paste0("n_", med_categories)
          total <- sum(
            controls[
              controls$subject_label == subj &
                controls$event_sequence == event,
              count_cols
            ][1, ],
            na.rm = TRUE
          )
          controls[
            controls$subject_label == subj &
              controls$event_sequence == event,
            "total_medications"
          ] <- total
        }
      }
    }

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
    stop("Did not find any medication files in the directory")
  }
}
