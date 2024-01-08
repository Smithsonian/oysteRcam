#' fromjulian
#'
#' @description
#' A function that takes a Julian date and a year as arguments and converts it
#' into the date format MM/DD/YYYY.
#'
#' @param jdate Julian date as three numbers: ###.
#' @param year Year as four numbers: YYYY.
#'
#' @return The date in MM/DD/YYYY format converted from the Julian Date.
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
#'

####################
### fromjulian() ###
####################

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

# * Create a function that defines the water body.
# Name the function waterbody()
fromjulian <- function(jdate, year) {

  # * Create a calendar as a dataframe to assist in extracting the date from the
  # * Julian date.
  # Define the days of each month.
  day_jan <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
               11:31)
  day_feb_nonlp <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
                     11:28)
  day_feb_lp <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
                  11:29)
  day_mar <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
               11:31)
  day_apr <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
               11:30)
  day_may <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
               11:31)
  day_jun <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
               11:30)
  day_jul <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
               11:31)
  day_aug <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
               11:31)
  day_sep <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
               11:30)
  day_oct <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
               11:31)
  day_nov <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
               11:30)
  day_dec <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
               11:31)

  # Define the month name corresponding to its length.
  mon_jan <- c(rep('January', length(day_jan)))
  mon_feb_nonlp <- c(rep('February', length(day_feb_nonlp)))
  mon_feb_lp <- c(rep('February', length(day_feb_lp)))
  mon_mar <- c(rep('March', length(day_mar)))
  mon_apr <- c(rep('April', length(day_apr)))
  mon_may <- c(rep('May', length(day_may)))
  mon_jun <- c(rep('June', length(day_jun)))
  mon_jul <- c(rep('July', length(day_jul)))
  mon_aug <- c(rep('August', length(day_aug)))
  mon_sep <- c(rep('September', length(day_sep)))
  mon_oct <- c(rep('October', length(day_oct)))
  mon_nov <- c(rep('November', length(day_nov)))
  mon_dec <- c(rep('December', length(day_dec)))

  # Define the month number corresponding to its length.
  num_jan <- c(rep('01', length(day_jan)))
  num_feb_nonlp <- c(rep('02', length(day_feb_nonlp)))
  num_feb_lp <- c(rep('02', length(day_feb_lp)))
  num_mar <- c(rep('03', length(day_mar)))
  num_apr <- c(rep('04', length(day_apr)))
  num_may <- c(rep('05', length(day_may)))
  num_jun <- c(rep('06', length(day_jun)))
  num_jul <- c(rep('07', length(day_jul)))
  num_aug <- c(rep('08', length(day_aug)))
  num_sep <- c(rep('09', length(day_sep)))
  num_oct <- c(rep('10', length(day_oct)))
  num_nov <- c(rep('11', length(day_nov)))
  num_dec <- c(rep('12', length(day_dec)))

  # * Non-leap year.
  # Combine month lengths into one column, day_nonlp.
  day_nonlp <- c(day_jan, day_feb_nonlp, day_mar, day_apr, day_may, day_jun,
                 day_jul, day_aug, day_sep, day_oct, day_nov, day_dec)

  # Combine months into one column, mon_nonlp.
  mon_nonlp <- c(mon_jan, mon_feb_nonlp, mon_mar, mon_apr, mon_may, mon_jun,
                 mon_jul, mon_aug, mon_sep, mon_oct, mon_nov, mon_dec)

  # Combine numths into one column, num_nonlp.
  num_nonlp <- c(num_jan, num_feb_nonlp, num_mar, num_apr, num_may, num_jun,
                 num_jul, num_aug, num_sep, num_oct, num_nov, num_dec)

  # Combine the day and month columns into a dataframe, regyear.
  regyear <- data.frame(num_nonlp, mon_nonlp, day_nonlp)

  # * Leap year.
  # Combine month lengths into one column, day_lp.
  day_lp <- c(day_jan, day_feb_lp, day_mar, day_apr, day_may, day_jun,
                 day_jul, day_aug, day_sep, day_oct, day_nov, day_dec)

  # Combine months into one column, mon_lp.
  mon_lp <- c(mon_jan, mon_feb_lp, mon_mar, mon_apr, mon_may, mon_jun,
                 mon_jul, mon_aug, mon_sep, mon_oct, mon_nov, mon_dec)

  # Combine numths into one column, num_lp.
  num_lp <- c(num_jan, num_feb_lp, num_mar, num_apr, num_may, num_jun,
              num_jul, num_aug, num_sep, num_oct, num_nov, num_dec)

  # Combine the day and month columns into a dataframe, regyear.
  leapyear <- data.frame(num_lp, mon_lp, day_lp)

  # * Julian dates are dependent upon the year. If the year is a leap year
  # * (February is 29 days long, rather than 28), then the Julian date will be
  # * different than if the same date was referenced in a non-leap year year.
  # * Leap years occur every 4th year.

  # If the year is divisible by 4 with a remainder of 0, then the year is a leap
  # year and the Julian date needs to account for that.
  if(year %% 4 == 0) {
    day <- leapyear$day_lp[jdate]
    month <- leapyear$num_lp[jdate]
    sample_date <- paste0(month, '/', day, '/', year)

  # If the year is not divisible by 4 with a remainder of 0, then the year is
  # NOT a leap year.
  } else {
    day <- regyear$day_nonlp[jdate]
    month <- regyear$num_nonlp[jdate]
    sample_date <- paste0(month, '/', day, '/', year)
  }

  # Use assign() to assign sample_date to the global environment
  assign(x = 'sample_date', value = sample_date, envir = .GlobalEnv)
}

################################################################################
