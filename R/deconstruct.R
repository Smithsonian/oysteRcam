#' deconstruct
#'
#' @description
#' The function that gathers all the necessary metadata from a GoPro image's
#' filename.
#'
#' Functions utilized from oysteRcam: camdir(), waterbody(), and fromjulian().
#'
#' @return An excel file.

################################################################################

########################
### File Description ###
########################

# Filename: deconstruct.R
# Author: Sam McNeely
# Contributors:
# Date Created: 12/21/2023
# Date Modified: 01/09/2023

# Place in the data collection process:
# This R script is meant to be run after camdir(), which assigns the directory
# path of the folder of images to be scored to a variable.

################################################################################

###########################
### Function Definition ###
###########################
#
# Notes:
#
# Do NOT modify the function definitions. Simply run through each function to
# create it (Ctrl + Enter).

# * Create a function that takes the first file in the designated image folder
# * assigned by camdir() and extracts location, date, and camera side.
# Name the function deconstruct()
deconstruct <- function() {

  # Extract the name of the first file in the image folder.
  firstfile <- list.files(camdir)[1]

  ## First Extraction: Location
  # From firstfile, extract the location abbreviation.
  locabbrev <- str_split_i(string = firstfile, pattern = '[0-9]+_', 1)

  # Use waterbody() to set the location of the samples.
  waterbody()

  ## Second Extraction: Date
  # From firstfile, extract the year and Julian date.
  datext <- str_split_i(string = firstfile, pattern = '_', 2) # 7 numbers

  # From datext, pull out the year.
  year <- str_extract(datext, '^[0-9]{4}')

  # From datext, pull out the Julian date.
  jdate <- str_extract(datext, '[0-9]{3}$')

  # Use fromjulian() to convert the Julian date into MM/DD/YYYY.
  fromjulian(jdate = jdate, year = year)

  ## Third Extraction: Camera Side
  # From firstfile, extract the camera side.
  side <- str_split_i(string = firstfile, pattern = '_', 3) # a or b

}

################################################################################
