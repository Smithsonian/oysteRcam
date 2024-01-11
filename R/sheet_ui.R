#' sheet_ui
#'
#' @description
#' The comprehensive user interface function that utilizes other functions in
#' oysteRcam to have the user answers prompted questions that will produce a
#' datasheet. This datasheet will then be used as the scoring sheet.
#'
#' Functions utilized from oysteRcam: waterbody_ui(), sampling_date_ui(),
#' numofsides(), camside_ui(), sites_ui(), missing_sites_ui(),
#' metadata_builder(), and datasheet().
#'
#' @return An excel file.

################################################################################

########################
### File Description ###
########################

# Filename: sheet_ui.R
# Author: Sam McNeely
# Contributors:
# Date Created: 11/27/2023
# Date Modified: 01/11/2024

# Place in the data collection process:
# This R script is meant to be run after images have been extracted from the
# GoPro videos and before the images are qualitatively scored.

################################################################################

###########################
### Function Definition ###
###########################
#
# Notes:
#
# Do NOT modify the function definitions. Simply run through each function to
# create it (Ctrl + Enter).

# * Create a function that takes user input to define which camera is being
# * evaluated first to create the spreadsheet variables.
# Name the function sheet_ui().
sheet_ui <- function() {

  # Call waterbody_ui() to set the sampling site location.
  location <- waterbody_ui()

  # Call sampling_date_ui() to set the date sampling occurred.
  date <- sampling_date_ui()

  # Call numofsides_ui() to set how many camera sides need to be analyzed in the
  # following iterative loop.
  numsides <- numofsides_ui()

  # * while loop 2 [Variable assignment]
  # * The i counter variable and its corresponding while loop are in place to
  # * ensure that all camera sides that need to be analyzed have variables for
  # * them.

  # create a counter variable, i, initialized at 0
  i <- 0

  # Use a while loop so that the user is prompted all sides have been accounted
  # for.
  while(i != numsides) {

    # Call camside_ui() to determine which side is being prepared for analysis.
    side <- camside_ui()

    # Call sites_ui() to gather the site number information from this specific
    # side.
    site <- sites_ui()

    # * Define parameters for this specific camera side.
    # Parameters: Date_a/b, Location_a/b, Random_Assignment_a/b, and Notes_a/b

    # create an if-statement for if side equals "A" or "B" to define the variable
    # names
    if(side == "A") {
      Date_a <- rep(date, length(site))
      Location_a <- rep(water_body, length(site))
      Site_a <- site
      Random_Assignment_a <- sample(1:length(site), length(site))
      Notes_a <- rep("", length(site))
    } else if(side == "B") {
      Date_b <- rep(date, length(site))
      Location_b <- rep(water_body, length(site))
      Site_b <- site
      Random_Assignment_b <- sample(1:length(sites), length(site))
      Notes_b <- rep("", length(site))
    }

    # * Determine if Habitat Score should be added to the datasheet
    # using an if-statement, determine whether hab_answer exists
    if(exists("hab_answer")) {

      # if hab_answer does exist, determine whether hab_answer = "Y" or "N"
      if(hab_answer == "Y") {

        # if side == "A", define Habscore_a as an empty vector
        if(side == "A") {
          Habscore_a <- rep(NA, length(site))

          # if side == "B", define Habscore_b as an empty vector
        } else if(side == "B") {
          Habscore_b <- rep(NA, length(site))
        }
      } # If hab_answer = "N", nothing needs to be done

      # if hab_answer doesn't exist, create it by prompting the user
    } else {

      # * while loop 2.1 [Habitat Score addition and verification]
      # * Have the user answer if Habitat Score will be evaluated.
      # stop_2.1 is the flag for when the user entry is valid, exit the current
      # while loop
      stop_2.1 <- FALSE

      # use a while loop to prompt for user input until a specific entry "Y" or
      # "N" is entered
      while(stop_2.1 == FALSE) {

        # ask the user if Habitat Score will be evaluated
        hab_answer <- readline(prompt = cat("\n\nAre you evaluating Habitat Score? [Y/N]\n"))

        # assign hab_answer to the Global Environment
        assign(x = 'hab_answer', value = hab_answer, envir = .GlobalEnv)

        # if the entry is "N", set stop_2.1 = TRUE to exit the current while loop
        # without adding in Habitat Score variables
        if(hab_answer == "N") {
          stop_2.1 <- TRUE
          cat('\n\nHabitat Score will not be added to the datasheets for evaluation.\n')

          # if the entry is "Y", set stop_2.1 = TRUE to exit the current while loop
          # and create Habitat Score variables according to each side
        } else if(hab_answer == "Y") {
          stop_2.1 <- TRUE

          # if side == "A", define Habscore_a as an empty vector
          if(side == "A") {
            Habscore_a <- rep(NA, length(site))

            # if side == "B", define Habscore_b as an empty vector
          } else if(side == "B") {
            Habscore_b <- rep(NA, length(site))
          }

          # if the entry is not "Y" nor "N", then print an error message and
          # reprompt.
        } else {
          cat('\n\nError: invalid entry.\nEnter "Y" for Yes or "N" for No.\n')
        }
      }
    }

    # * Determine if Percent Cover should be added to the datasheet
    # using an if-statement, determine whether pc_answer exists
    if(exists("pc_answer")) {

      # if pc_answer does exist, determine whether pc_answer = "Y" or "N"
      if(pc_answer == "Y") {

        # if side == "A", define Percent_Cover_a as an empty vector
        if(side == "A") {
          Percent_Cover_a <- rep(NA, length(site))

          # if side == "B", define Percent_Cover_b as an empty vector
        } else if(side == "B") {
          Percent_Cover_b <- rep(NA, length(site))
        }
      } # If pc_answer = "N", nothing needs to be done

      # if pc_answer doesn't exist, create it by prompting the user
    } else {

      # * while loop 2.2 [Percent Cover addition and verification]
      # * Have the user answer if Percent Cover will be evaluated.
      # stop_2.2 is the flag for when the user entry is valid, exit the current
      # while loop
      stop_2.2 <- FALSE

      # use a while loop to prompt for user input until a specific entry "Y" or
      # "N" is entered
      while(stop_2.2 == FALSE) {

        # ask the user if Percent Cover will be evaluated
        pc_answer <- readline(prompt = cat("\n\nAre you evaluating Percent Cover? [Y/N]\n"))

        # assign pc_answer to the Global Environment
        assign(x = 'pc_answer', value = pc_answer, envir = .GlobalEnv)

        # if the entry is "N", set stop_2.2 = TRUE to exit the current while loop
        # without adding in Percent Cover variables
        if(pc_answer == "N") {
          stop_2.2 <- TRUE
          cat('\n\nPercent Cover will not be added to the datasheets for evaluation.\n')

          # if the entry is "Y", set stop_2.2 = TRUE to exit the current while loop
          # and create Percent Cover variables according to each side
        } else if(pc_answer == "Y") {
          stop_2.2 <- TRUE

          # if side == "A", define Percent_Cover_a as an empty vector
          if(side == "A") {
            Percent_Cover_a <- rep(NA, length(site))

            # If side == "B", define Percent_Cover_b as an empty vector
          } else if(side == "B") {
            Percent_Cover_b <- rep(NA, length(site))
          }

          # if the entry is not "Y" nor "N", then print an error message and
          # reprompt.
        } else {
          cat('\n\nError: invalid entry.\nEnter "Y" for Yes or "N" for No.\n')
        }
      }
    }

    # * Create the dataframe for the specific side
    # # create an if-statement for if side equals "A" or "B" to define the
    # dataframe's name
    if(side == "A") {
      df_a <- data.frame(Date_a, Location_a, Site_a, Random_Assignment_a,
                         Notes_a)
    } else if(side == "B") {
      df_b <- data.frame(Date_b, Location_b, Site_b, Random_Assignment_b,
                         Notes_b)
    }

    # * If Habitat Score is being evaluated, add the respective columns to the
    # * dataframes
    # Side A
    if((hab_answer == 'Y') & (side == 'A')) {
      df_a$Habscore_a <- Habscore_a

      # reorder the columns so that Habscore_a comes before Notes_a
      df_a <- df_a[, c(1:4, 6, 5)]

      # Assign df_a to the Global Environment
      assign(x = 'df_a', value = df_a, envir = .GlobalEnv)

      # Side B
    } else if((hab_answer == 'Y') & (side == 'B')) {
      df_b$Habscore_b <- Habscore_b

      # reorder the columns so that Habscore_b comes before Notes_b
      df_b <- df_b[, c(1:4, 6, 5)]

      # Assign df_b to the Global Environment
      assign(x = 'df_b', value = df_b, envir = .GlobalEnv)
    }

    # * If Percent Cover is being evaluated, add the respective columns to the
    # * dataframes
    # Side A
    if((pc_answer == 'Y') & (side == 'A')) {
      df_a$Percent_Cover_a <- Percent_Cover_a

      # reorder the columns so that Percent_Cover_a comes before Notes_a
      df_a <- df_a[, c(1:5, 7, 6)]

      # Assign df_a to the Global Environment
      assign(x = 'df_a', value = df_a, envir = .GlobalEnv)

      # Side B
    } else if((pc_answer == 'Y') & (side == 'B')) {
      df_b$Percent_Cover_b <- Percent_Cover_b

      # reorder the columns so that Percent_Cover_b comes before Notes_b
      df_b <- df_b[, c(1:5, 7, 6)]

      # Assign df_b to the Global Environment
      assign(x = 'df_b', value = df_b, envir = .GlobalEnv)
    }

    # * Add in rows for the sites with missing images.
    # Call missing_sites_ui() to gather the site number information from this
    # specific side in which images were not extracted for said site.
    missing_sites <- missing_sites_ui()

    # append the dataframe with rows for missing images
    if(length(missing_sites) >= 1) {

      # append the correct dataframe: df_a or df_b
      if(side == "A") {
        for(i in 1:length(missing_sites)) {
          df_a <- df_a %>%
            add_row(Date_a = Date, Location_a = water_body,
                    Site_a = missing_sites[i], Habscore_a = NA,
                    Notes_a = "Missing image")
        }

        # Assign df_a to the Global Environment
        assign(x = 'df_a', value = df_a, envir = .GlobalEnv)

      } else if(side == "B") {
        for(i in 1:length(missing_sites)) {
          df_b <- df_b %>%
            add_row(Date_b = Date, Location_b = water_body,
                    Site_b = missing_sites[i], Habscore_b = NA,
                    Notes_b = "Missing image")
        }

        # Assign df_b to the Global Environment
        assign(x = 'df_b', value = df_b, envir = .GlobalEnv)
      }
    }

    cat("The sheet for side ", side, " has been created.", sep = "")

    # increment i by 1
    i <- i + 1
  }

  # Remove ms, side, and site from the Global Environment
  rm(missing_sites, side, site, envir = .GlobalEnv)

  # * Create the metadata sheet
  metadata_builder()

  # * Build the datasheet by calling datasheet()
  datasheet()
}

################################################################################
