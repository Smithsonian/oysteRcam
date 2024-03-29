#' fromjulian
#'
#' @description
#' A function that takes a Julian date and a year as arguments and converts it
#' into the date format MM/DD/YYYY.
#'
#' @param jdate Julian date as three numbers: ###.
#' @param year Year as four numbers: ####.
#'
#' @return The date in MM/DD/YYYY format converted from the Julian Date.
#'
#' @examples
#' ## Pass a Julian date from a year that is NOT a leap year.
#' fromjulian(jdate = 289, year = 2023)
#'
#' ## Pass the same Julian date from a year that IS a leap year.
#' fromjulian(jdate = 289, year = 2020)
#'
#' ## Now pass a Julian date from a year that is NOT a leap year and before to March 1.
#' fromjulian(jdate = 43, year = 2021)
#'
#' ## Pass the same Julian date from a year that IS a leap year and before February 29.
#' fromjulian(jdate = 43, year = 2016)

################################################################################

########################
### File Description ###
########################

# Filename: fromjulian.R
# Author: Sam McNeely
# Contributors:
# Date Created: 01/07/2024
# Date Modified: 01/07/2024

# Purpose:
# The purpose of this script is to define a function that converts a Julian date
# into the MM/DD/YYYY format.

# Place in the data collection process:
# This function is meant to gather the sample collection date.

################################################################################

###########################
### Function Definition ###
###########################
#
# Notes:
#
# Do NOT modify the function definitions. Simply run through the function to
# create it (Ctrl + Enter).

# * Create a function that converts Julian date into MM/DD/YYYY date format.
# Name the function fromjulian()
fromjulian <- function(jdate, year) {

  # * Create a calendar as a dataframe to assist in extracting the date from the
  # * Julian date.

  # * Julian dates are dependent upon the year. If the year is a leap year
  # * (February is 29 days long, rather than 28), then the Julian date will be
  # * different than if the same date was referenced in a non-leap year year.
  # * Leap years occur every 4th year.

  # If the year is divisible by 4 with a remainder of 0, then the year is a leap
  # year and the Julian date needs to account for that.
  if(year %% 4 == 0) {
    leapyear()
    day <- calendar$day_leap[jdate]
    month <- calendar$num_leap[jdate]
    sample_date <- paste0(month, '/', day, '/', year)

  # If the year is not divisible by 4 with a remainder of 0, then the year is
  # NOT a leap year.
  } else {
    regyear()
    day <- calendar$day_reg[jdate]
    month <- calendar$num_reg[jdate]
    sample_date <- paste0(month, '/', day, '/', year)
  }

  # Remove calendar from the global environment.
  rm(calendar, envir = .GlobalEnv)

  # Use assign() to assign sample_date to the global environment
  assign(x = 'sample_date', value = sample_date, envir = .GlobalEnv)
}

################################################################################
