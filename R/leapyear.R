#' leapyear
#'
#' @description
#' A function that creates a dataframe for a leap year with the following
#' variables:
#'
#' num_leap: The month's two digit sequential number. Ex: January = 01.
#'
#' mon_leap: The name of the month. Ex: October.
#'
#' day_leap: The day in the month. Ex: July 23 = 23.
#'
#' jdate_leap: The Julian date corresponding to a leap year.
#'
#' @return A dataframe for a leap year. The dataframe will be stored as
#' "calendar' and assigned to the global environment.

################################################################################

########################
### File Description ###
########################

# Filename: leapyear.R
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

# * Create a function that that builds a calendar for a leap year.
# Name the function leapyear()
leapyear <- function() {

  # * Create a calendar as a dataframe to assist in extracting the date from the
  # * Julian date.
  # Define the days of each month.
  day_jan <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:31)
  day_feb <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:29)
  day_mar <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:31)
  day_apr <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:30)
  day_may <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:31)
  day_jun <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:30)
  day_jul <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:31)
  day_aug <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:31)
  day_sep <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:30)
  day_oct <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:31)
  day_nov <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:30)
  day_dec <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', 11:31)

  # Combine into one vector: day_leap.
  day_leap <- c(day_jan, day_feb, day_mar, day_apr, day_may, day_jun, day_jul,
                day_aug, day_sep, day_oct, day_nov, day_dec)

  # Define the month number corresponding to its length.
  num_jan <- c(rep('01', length(day_jan)))
  num_feb <- c(rep('02', length(day_feb)))
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

  # Combine into one vector: num_leap.
  num_leap <- c(num_jan, num_feb, num_mar, num_apr, num_may, num_jun, num_jul,
               num_aug, num_sep, num_oct, num_nov, num_dec)

  # Define the month name corresponding to its length.
  mon_jan <- c(rep('January', length(day_jan)))
  mon_feb <- c(rep('February', length(day_feb)))
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

  # Combine into one vector: mon_leap.
  mon_leap <- c(mon_jan, mon_feb, mon_mar, mon_apr, mon_may, mon_jun, mon_jul,
               mon_aug, mon_sep, mon_oct, mon_nov, mon_dec)

  # Define the Julian dates.
  jdate_leap <- c('001', '002', '003', '004', '005', '006', '007', '008', '009', '010',
                  '011', '012', '013', '014', '015', '016', '017', '018', '019', '010',
                  '021', '022', '023', '024', '025', '026', '027', '028', '029', '010',
                  '031', '032', '033', '034', '035', '036', '037', '038', '039', '010',
                  '041', '042', '043', '044', '045', '046', '047', '048', '049', '010',
                  '051', '052', '053', '054', '055', '056', '057', '058', '059', '010',
                  '061', '062', '063', '064', '065', '066', '067', '068', '069', '010',
                  '071', '072', '073', '074', '075', '076', '077', '078', '079', '010',
                  '081', '082', '083', '084', '085', '086', '087', '088', '089', '010',
                  '091', '092', '093', '094', '095', '096', '097', '098', '099', 100:366)

  # Combine the four variables into a dataframe, leapyear.
  leapyear <- data.frame(num_leap, mon_leap, day_leap, jdate_leap)

  # Use assign() to assign leapyear as calendar to the global environment.
  assign(x = 'calendar', value = leapyear, envir = .GlobalEnv)

}

################################################################################
