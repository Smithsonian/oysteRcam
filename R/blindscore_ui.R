#' blindscore_ui
#'
#' @description
#' A user interface function that creates a copy of the original image folder,
#' renames the folder './Side A (Blind Scoring)' or './Side A (Blind Scoring)',
#' and renames the images according to their designated random assignment
#' number.
#'
#' @param cdir The full path to the directory that contains the existing GoPro images that were previously extracted from the videos. This path should end with either '/Side A' or '/Side B'.
#' @param sheet The filename of the .xlsx file created using sheet_ui(). Example: 'BroadCreek_2023308_NoMaxScores.xlsx'
#'
#' @returns A copied folder of the original GoPro images for the site renamed
#' for blind scoring and the images renamed according to their designated random
#' assignment number.

################################################################################

#######################
### blindscore_ui() ###
#######################

################################################################################

########################
### File Description ###
########################

# Filename: blindscore_ui.R
# Author: Sam McNeely
# Contributors:
# Date Created: 12/11/2023
# Date Modified: 01/07/2024

# Purpose:
# The purpose of this script is to copy the folder of GoPro images from one side
# and then rename each image according to its corresponding random assignment
# number.

# Place in the data collection process:
# This R script is meant to be run after the excel file has been created and
# before the images are qualitatively scored.

################################################################################

###########################
### Function Definition ###
###########################
#
# Notes:
#
# Do NOT modify the function definitions. Simply run through the function to
# create it (Ctrl + Enter).

# * Create a function that copies the folder of GoPro images and renames each
# * image to its corresponding randomly assigned number.
# Name the function blindscore_ui()
blindscore_ui <- function(cdir, sheet) {

  # * while loop 1 [Path entry and verification]
  # * The stop_1 variable and its corresponding while loop are reassurances that
  # * the user input the correct entry. This variable is the flag that tells the
  # * while loop to either continue or stop running.
  stop_1 <- FALSE

  while(stop_1 == FALSE) {

    cdir <- readline(prompt = cat("\n\nPlease paste the path to the directory containing the images from one of the camera sides:\n"))

    # * while loop 1.1 [Path entry]
    # * Have the user input which camera side is being entered with the only
    # * options being "A" or "B"
    # stop_1.1 is the flag for when the user entry is valid, exit the current
    # while loop
    stop_1.1 <- FALSE

    # use a while loop to prompt for user input until a specific entry "A" or
    # "B" is entered
    while(stop_1.1 == FALSE) {
      cdir <- readline(prompt = cat("\n\nPlease paste the path to the directory containing the images from one of the camera sides:\n"))

      # if side equals "A" or "B", break the while loop
      if(cdir == "A" | side == "B") {
        stop_1.1 <- TRUE

        # if side does NOT equal "A" or "B", print an error message and prompt
        # for user input again
      } else {
        cat('\n\nError: invalid entry.\nEnter "A" or "B".\n')
      }
    }

    # * while loop 1.2 [Camera side verification]
    stop_1.2 <- FALSE

    while(stop_1.2 == FALSE) {
      # Prompt the user to check that their input is correct
      answer <- readline(prompt = cat("\n\nYou are evaluating camera side ", side, ". Is this correct? [Y/N]\n", sep = ""))

      # If user input is correct, break the main while loop
      if(answer == "Y") {
        stop_1.2 <- TRUE
        stop_1 <- TRUE

        # Use assign() to assign side to the global environment
        assign(x = 'side', value = side, envir = .GlobalEnv)

        # If user input is incorrect, reprompt the user
      } else if(answer == "N") {
        cat('\n\nCamera side is incorrect. Reprompting...\n')
        stop_1.2 <- TRUE
      } else {
        cat('\n\nError: invalid entry.\nEnter "Y" or "N".\n')
      }
    }
  }

  camdir()

  # * Copy and paste the image folder.
  # Create the string that will be the new copied folder.
  pastepath <- paste0(cdir, ' (Blind Scoring)')

  # The folder that will be copy and pasted to must be created before it is copy
  # and pasted.
  dir.create(pastepath)

  # Copy the folder over to the new folder.
  file.copy(from = list.files(path = cdir, full.names = TRUE),
            to = pastepath,
            recursive = TRUE)

  # * Pattern building for renaming the files
  # Extract the name of the first file in cdir that will be used as the
  # template.
  firstfile <- list.files(cdir)[1]

  # Extract the site abbreviation for the files from firstfile.
  siteabbrev <- str_split_i(string = firstfile, pattern = '[0-9]+_', 1)

  # Build the pattern for recognition.
  patt <- paste0(siteab, '[0-9]+.*')

  # * Rename the files within the pastepath folder.
  # Determine which side is being evaluated to appropriately choose the sheet.
  if(grepl(pattern = '\\Side A', x = cdir) | grepl(pattern = '/Side A', x = cdir)) {

    # * Data loading
    # Read in the excel file that contains the random assignments from side A.
    data <- read.xlsx(xlsxFile = paste0('./Data/NoMaxScores/RAP/', sheet), sheet = 'Side A')

    # * Data Cleaning
    # Assign the Random_Assigntment_a column to an object
    randassign_a <- data$Random_Assignment_a

    # Rename the images within the new folder, pastepath, by using randassign_a
    for(i in 1:length(list.files(pastepath))) {
      file.rename(from = paste0(pastepath, '/', list.files(path = pastepath)[i]),
                  to = paste0(pastepath, '/', randassign_a[i], '.png'))
    }

  } else if(grepl(pattern = '\\Side B', x = cdir) | grepl(pattern = '/Side B', x = cdir)) {

    # Read in the excel file that contains the random assignments from side B.
    data <- read.xlsx(xlsxFile = paste0('./Data/NoMaxScores/RAP/', sheet), sheet = 'Side B')

    # Assign the Random_Assigntment_b column to an object
    randassign_b <- data$Random_Assignment_b

    # Rename the images within the new folder, pastepath, by using randassign_b
    for(i in 1:length(list.files(pastepath))) {
      file.rename(from = paste0(pastepath, '/', list.files(path = pastepath)[i]),
                  to = paste0(pastepath, '/', randassign_b[i], '.png'))
    }
  }
}

################################################################################
