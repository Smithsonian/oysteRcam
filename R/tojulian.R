#' tojulian
#'
#' @description
#' A function that takes a month, day, and year as arguments and converts it
#' into a Julian date.
#'
#' @param month Month as two numbers: ##.
#' @param day Day as one or two numbers: # or ##.
#' @param year Year as four numbers: ####.
#'
#' @return The date in MM/DD/YYYY format converted from the Julian Date.
#'
#' @examples
#' ## Pass a date from a year that is NOT a leap year.
#' tojulian(month = 10, day = 14, year = 2023)
#'
#' ## Pass the same date from a year that IS a leap year.
#' tojulian(month = 10, day = 14, year = 2020)
#'
#' ## Now pass a date from a year that is NOT a leap year and before to March 1.
#' tojulian(month = 02, day = 05, year = 2021)
#'
#' ## Pass the same Julian date from a year that IS a leap year and before February 29.
#' tojulian(month = 02, day = 05, year = 2016)

################################################################################

########################
### File Description ###
########################

# Filename: tojulian.R
# Author: Sam McNeely
# Contributors:
# Date Created: 01/08/2024
# Date Modified: 01/08/2024

################################################################################

###########################
### Function Definition ###
###########################
#
# Notes:
#
# Do NOT modify the function definitions. Simply run through the function to
# create it (Ctrl + Enter).

# * Create a function that outputs a Julian date based on the MM/DD/YYYY date
# * format.
# Name the function tojulian()
tojulian <- function(month, day, year) {

  # * Create a calendar as a dataframe to assist in extracting the date from the
  # * Julian date.

  # * If month and day are single digit numbers, add a 0 in front and convert it
  # * to a string.
  # Month
  if(length(month) == 1) {
    month <- paste0('0', month)
  }

  # Day
  if(length(day) == 1) {
    day <- paste0('0', day)
  }

  # * Julian dates are dependent upon the year. If the year is a leap year
  # * (February is 29 days long, rather than 28), then the Julian date will be
  # * different than if the same date was referenced in a non-leap year year.
  # * Leap years occur every 4th year.

  # If the year is divisible by 4 with a remainder of 0, then the year is a leap
  # year and the Julian date needs to account for that.
  if(year %% 4 == 0) {
    leapyear()
    jdate <- calendar$jdate_leap[calendar$num_leap == month & calendar$day_leap == day]

  # If the year is not divisible by 4 with a remainder of 0, then the year is
  # NOT a leap year.
  } else {
    regyear()
    jdate <- calendar$jdate_reg[calendar$num_reg == month & calendar$day_reg == day]
  }

  # Remove calendar from the global environment.
  rm(calendar, envir = .GlobalEnv)

  # Use assign() to assign jdate to the global environment
  assign(x = 'julian_date', value = jdate, envir = .GlobalEnv)
}

################################################################################
