explore_columns <- function(data, menu_func) {
  col_names <- colnames(data)
  col_choice <- menu_func(
    choices = col_names,
    title = "Which column would you like to explore?"
  )

  if (col_choice > 0) {
    selected_col <- col_names[col_choice]
    cat("\n")
    cli::cat_line(cli::col_magenta(paste0("Exploring: ", selected_col)))
    cat("\n")
    cat("Unique values: ", length(unique(data[[selected_col]])), "\n")
    cat("Missing values: ", sum(is.na(data[selected_col])), "\n")

    if (
      is.character(data[[selected_col]]) ||
        is.factor(data[[selected_col]]) ||
        length(unique(data[[selected_col]])) <= 20
    ) {
      cat("Value frequencies:\n")
      print(table(data[[selected_col]], useNA = "ifany"))
    } else {
      cat("Summary statistics:\n")
      print(summary(data[[selected_col]]))
    }
    readline(prompt = "\nPress [Enter] to continue...")
  }
}

filter_data <- function(data, menu_func) {
  col_names <- names(data)
  col_choice <- menu_func(
    choices = col_names,
    title = "Which column would you like to filter by?"
  )

  if (col_choice > 0) {
    selected_col <- col_names[col_choice]

    # Show available values
    unique_vals <- unique(data[[selected_col]])
    cat("\n")
    cli::cat_line(paste0("Available values in '", selected_col, "':"))
    print(utils::head(unique_vals, 20))
    if (length(unique_vals) > 20) {
      cat("... and", length(unique_vals) - 20, "more\n")
    }
    cat("\n")

    # Step 2: Choose operator
    operator_choices <- c("==", "!=", ">", "<", ">=", "<=", "is.na", "!is.na")
    operator_choice <- menu_func(
      choices = operator_choices,
      title = "Select comparison operator:"
    )

    if (operator_choice > 0) {
      operator <- operator_choices[operator_choice]

      # Step 3: Handle is.na operators (no value needed)
      if (operator %in% c("is.na", "!is.na")) {
        if (operator == "is.na") {
          filtered <- data[is.na(data[[selected_col]]), ]
        } else {
          filtered <- data[!is.na(data[[selected_col]]), ]
        }

        if (nrow(filtered) > 0) {
          data <- filtered
          cli::cat_line(cli::col_green(paste0(
            "Filtered to ",
            nrow(data),
            " rows."
          )))
        } else {
          cli::cat_line(cli::col_red("No rows match that filter."))
        }
      } else {
        # Step 4: Get comparison value
        filter_value <- readline(
          prompt = paste0(
            "Enter value for '",
            selected_col,
            " ",
            operator,
            " ?' (or press Enter to cancel): "
          )
        )

        if (nchar(filter_value) > 0) {
          # Convert to appropriate type
          if (is.numeric(data[[selected_col]])) {
            filter_value <- as.numeric(filter_value)
          } else if (is.logical(data[[selected_col]])) {
            filter_value <- as.logical(filter_value)
          }

          # Apply filter using tidy evaluation
          tryCatch(
            {
              # Build the expression: selected_col > filter_value
              filter_expr <- rlang::call2(
                operator,
                rlang::sym(selected_col),
                filter_value
              )
              filtered <- dplyr::filter(data, !!filter_expr)

              if (nrow(filtered) > 0) {
                data <- filtered
                cli::cat_line(cli::col_green(paste0(
                  "Filtered to ",
                  nrow(data),
                  " rows where ",
                  selected_col,
                  " ",
                  operator,
                  " ",
                  filter_value
                )))
              } else {
                cli::cat_line(cli::col_red("No rows match that filter."))
              }
            },
            error = function(e) {
              cli::cat_line(cli::col_red(paste0(
                "Error applying filter: ",
                e$message
              )))
            }
          )
        }
      }
    }
  }
  return(data)
}

horizontal_menu <- function(choices, title = NULL) {
  if (!is.null(title)) {
    cli::cat_line(cli::col_cyan(title))
    cat("\n")
  }

  # Display choices horizontally with numbers
  choice_text <- paste0(
    sapply(seq_along(choices), function(i) {
      paste0("[", i, "] ", choices[i])
    }),
    collapse = "  |  "
  )

  cli::cat_line(choice_text)
  cat("\n")

  # Get user input
  repeat {
    user_input <- readline(prompt = "Enter choice number: ")
    choice_num <- suppressWarnings(as.integer(user_input))

    if (
      !is.na(choice_num) && choice_num >= 1 && choice_num <= length(choices)
    ) {
      return(choice_num)
    } else if (user_input == "") {
      return(0) # User pressed Enter without input
    } else {
      cli::cat_line(cli::col_red("Invalid choice. Please try again."))
    }
  }
}

launch_interactive_console <- function(data, horizontal_menus = TRUE) {
  menu_func <- if (horizontal_menus) horizontal_menu else utils::menu
  # Display Welcome Message
  cli::cat_boxx(
    c(
      cli::col_green(
        "Entering an interactive mode for exploring the ABC-DS Data."
      ),
      "Select 'Q' to Quit or 'R' to reset the data to its original state"
    )
  )
  original_data <- current_data <- data
  repeat {
    cat("\n")
    cli::cat_line(cli::col_cyan("Current data preview:"))
    print(utils::head(current_data))
    cat("\n")

    main_choices <- c(
      "Explore by column",
      "Filter Data",
      "Summary statistics",
      "Reset data",
      "Quit"
    )

    main_choice <- menu_func(
      choices = main_choices,
      title = "What would you like to do?"
    )

    if (main_choice == 1) {
      explore_columns(current_data, menu_func)
    }

    if (main_choice == 2) {
      current_data <- filter_data(current_data, menu_func)
    }

    if (main_choice == 3) {
      cat("\n")
      cli::cat_line(cli::col_cyan("Summary Statistics:"))
      print(summary(current_data))
      readline(prompt = "\n Press [Enter] to continue...")
    }

    if (main_choice == 4) {
      current_data <- original_data
      cli::cat_line(cli::col_yellow("Data reset to original state."))
      next
    }

    if (main_choice == 5 || main_choice == 0) {
      cli::cat_line(cli::col_green("Exiting interactive console. Goodbye!"))
      break
    }
  }
  invisible(current_data)
}
