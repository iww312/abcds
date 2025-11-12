data <- read.csv(file.choose(), stringsAsFactors = FALSE)
required_cols <- c("subject_label", "age_at_visit")
missing_cols <- setdiff(required_cols, names(data))
if (length(missing_cols) > 0) {
  stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
}
data$subject_label <- trimws(as.character(data$subject_label))
data$age_at_visit <- suppressWarnings(as.numeric(as.character(data$age_at_visit)))
first_age_by_subject <- aggregate(age_at_visit ~ subject_label, data = data, FUN = function(x) min(x, na.rm = TRUE))
first_age_by_subject$age_at_visit[is.infinite(first_age_by_subject$age_at_visit)] <- NA
cat("Summary table created with", nrow(first_age_by_subject), "subjects.\n\n")
repeat {
  patient_id <- readline(prompt = "Enter subject_label (or 'exit' to quit): ")
 
  if (tolower(patient_id) == "exit") {
    cat("Exiting lookup.\n")
    break
  }
  
  patient_id <- trimws(as.character(patient_id))
  match_row <- first_age_by_subject[first_age_by_subject$subject_label == patient_id, ]
  
  if (nrow(match_row) == 0) {
    cat("Subject", patient_id, "not found.\n\n")
  } else {
    cat("Age at FIRST visit for", patient_id, ":", match_row$age_at_visit, "\n\n")
  }
}
