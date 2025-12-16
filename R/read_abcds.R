read_abcds <- function(directory,
                       participant = TRUE,
                       control = FALSE,
                       add_demographics = c(""),
                       entire_data = FALSE,
                       ...) # ... is variable names

if(length(person) == 1) person <- match.arg(person)

  choice <- menu(c("ApoE", "Health_History"), title = "Choose file type")
  pattern <- c("ApoE", "Health_History")[choice]
  files <- list.files(directory, pattern = pattern, full.names = TRUE)

  person_types <- unname(sapply(files, .detect_person))

  if("participants" %in% person & "participant" %in% person_types){
    participants <- utils::read.csv(files[!grepl("Controls", files)])
    if("update_stamp" %in% colnames(participants)) participants$update_stamp <- NULL
    if(include_demographics){
      participants <- merge(
        read_demographics(
          directory,
          person = "participants",
          event_sequence = 1),
        participants, by = "subject_label")
    }
    class(participants) <- c("tbl_df", "tbl", "data.frame")
  }

  if("controls" %in% person & "control" %in% person_types){
    controls <- utils::read.csv(files[grepl("Controls", files)])
    if("update_stamp" %in% colnames(controls)) controls$update_stamp <- NULL
    if(include_demographics){
      controls <- merge(
        read_demographics(
          directory,
          person = "controls",
          event_sequence = 1),
        controls, by = "subject_label"
      )
    }
    class(controls) <- c("tbl_df", "tbl", "data.frame")
  }

  if(exists("participants", inherits = FALSE) & exists("controls", inherits = FALSE)){
    return(list(participants = participants, controls = controls))
  } else if(exists("participants", inherits = FALSE)){
    return(participants)
  } else if(exists("controls", inherits = FALSE)){
    return(controls)
  } else{
    stop("Did not find any ApoE files")
  }
}
