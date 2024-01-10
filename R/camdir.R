#' camdir
#'
#' @description
#' A function that generates the path to the folder containing the GoPro images
#' to be analyzed in a format that R can read. For optimal efficiency, copy the
#' path to this folder prior to running the function to utilize the default
#' setting.
#'
#' @param path The path to the folder containing the GoPro images
#' to be analyzed. For the default method: path = "clipboard" attempts to use
#' most recent item copied to convert the toward an R readable path.
#'
#' @return The path to the folder containing the GoPro images.
#'
#' @examples
#' ## Pass a directory path copied from the File Explorer that uses escape keys as the standard folder-subfolder separation.
#' camdir(path = "C:\Desktop\GitHub\Oysters\RAP\Data")

################################################################################

########################
### File Description ###
########################

# Filename: camdir.R
# Author: Sam McNeely
# Contributors:
# Date Created: 12/20/2023
# Date Modified: 01/09/2024

# Purpose:
# The purpose of this script is to score the images in R and then it will output
# a CSV with the scored data.

# Place in the data collection process:
# This R script is meant to be run after images have been extracted from the
# GoPro videos.

################################################################################

###########################
### Function Definition ###
###########################
#
# Notes:
#
# Do NOT modify the function definitions. Simply run through each function to
# create it (Ctrl + Enter).

# * Create a function that takes the copied path or user input and returns a
# * character vector that R can read without an 'Escape key' error.
# Name the function camdir()
# path: the path to the directory containing images to be scored. Default to
# "clipboard".
camdir <- function(path = "clipboard") {

  # * while loop 1 [Camera directory path entry and verification]
  # * The stop_1 variable and its corresponding while loop are reassurances that
  # * the user input the correct entry. This variable is the flag that tells the
  # * while loop to either continue or stop running.
  stop_1 <- FALSE

  # Set answer == "Y" to allow entry into the first if-statement
  answer <- 'Y'

  while(stop_1 == FALSE) {

    # If path == "clipboard", assign camdir to the copied path which is stored
    # on the MS Windows Clipboard.
    camdir <- if(path == "clipboard" & answer == 'Y') {

      # Check which OS is being used: Windows, Mac
      # Read the clipboard if using Windows
      if(Sys.info()['sysname'] == 'Windows') {
        readClipboard()

        # Read the clipboard if using macOS
      } else if(Sys.info()['sysname'] == 'Darwin') {
        pipe('pbpaste') # Could be pipe('pbcopy', 'w')
      }

      # If path != "clipboard", prompt the user to paste the path.
    } else if(answer == 'N') {
      readline(prompt = cat("\n\nPlease paste the path to the directory containing the images from one of the camera sides:\n"))
    }

    # Use chartr() to replace "\\" with "/" so that R does not assume escape keys
    camdir <- chartr("\\", "/", camdir)

    # * while loop 1.1 [Camera directory path verification]
    # * Have the user input which camera side is being entered with the only
    # * options being "A" or "B"
    # stop_2 is the flag for when the user entry is valid, exit the current
    # while loop
    stop_2 <- FALSE

    # use a while loop to prompt for user input until a specific entry "A" or
    # "B" is entered
    while(stop_2 == FALSE) {
      # Prompt the user to check that their input is correct
      answer <- readline(prompt = cat("\n\nThe path to the folder of images is:\n\n", camdir, "\n\nIs this correct? [Y/N]\n", sep = ""))

      # If user input is correct, break the while loops 1 and 1.1 and return
      # camdir
      if(answer == "Y") {
        stop_1 <- TRUE
        stop_2 <- TRUE

        # Use assign() to assign camdir to the global environment
        assign(x = 'camdir', value = camdir, envir = .GlobalEnv)

        # If user input is incorrect, reprompt the user
      } else if(answer == "N") {
        cat('\n\nFolder path is incorrect. Reprompting...\n')
        stop_2 <- TRUE

        # If neither a "Y" nor an "N" were entered, present an error message and
        # reprompt the user.
      } else {
        cat('\n\nError: invalid entry.\nEnter "Y" or "N".\n')
      }
    }
  }
}

################################################################################
