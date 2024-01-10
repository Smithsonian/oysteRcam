#' datasheet
#'
#' @description
#' This function creates an excel spreadsheet from the dataframes created in
#' previous functions. The spreadsheet produced is used as a scoring sheet to
#' score GoPro images.
#'
#' This function will not work on its own because it uses objects that are
#' created in other functions. However, it will work in sheet_ui() in
#' conjunction with other preceding functions.
#'
#' Functions utilized from oysteRcam: tojulian().
#'
#' @return An excel file.

################################################################################

########################
### File Description ###
########################

# Filename: datasheet.R
# Author: Sam McNeely
# Contributors:
# Date Created: 11/28/2023
# Date Modified: 01/09/2024

# Difference from PhotoRandomizer.R:
# This script outputs the excel spreadsheet, which is the final step in the
# PhotoRandomizer.R script.

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
# Do NOT modify the function definitions. Simply run through the function to
# create it (Ctrl + Enter).

# * Create a function that builds an excel spreadsheet.
# Name the function datasheet()
datasheet <- function() {

  # if there are spaces in the name of the water body, remove them
  if(grepl(" ", water_body)) {
    waterbody_nospaces <- gsub(" ", "", water_body)

    # Remove water_body from the Global Environment
    rm(water_body, envir = .GlobalEnv)

  } else {
    waterbody_nospaces <- water_body

    # Remove water_body from the Global Environment
    rm(water_body, envir = .GlobalEnv)
  }

  # * Combine dataframes into a list
  # create the list sheets that contains the sheets that will make up the Excel file
  sheets <- list("Metadata" = mdat, "Side A" = df_a, "Side B" = df_b)

  # Remove mdat from the Global Environment
  rm(mdat, df_a, df_b, envir = .GlobalEnv)

  # * Create path for files to be stored
  # name the paths without changing the current working directory
  wd <- "./Data"

  # setting up the sub-directory NoMaxScores
  sub_dir_1 <- "NoMaxScores"

  # * Write an if-statement that assigns sub_dir_2 to RAP, PC, or RAP_PC
  # if hab_answer = "Y", ask if pc_answer = "Y"
  if(hab_answer == "Y") {

    # Remove hab_answer from the Global Environment
    rm(hab_answer, envir = .GlobalEnv)

    # if both hab_answer and pc_answer = "Y", set sub_dir_2 = "RAP_PC"
    if(pc_answer == "Y") {
      sub_dir_2 <- 'RAP_PC'

      # Remove pc_answer from the Global Environment
      rm(pc_answer, envir = .GlobalEnv)

      # if hab_answer = "Y" and pc_answer = "N", set sub_dir_2 = "RAP"
    } else {
      sub_dir_2 <- 'RAP'

      # Remove pc_answer from the Global Environment
      rm(pc_answer, envir = .GlobalEnv)

    }
  } else {
    # Remove hab_answer from the Global Environment
    rm(hab_answer, envir = .GlobalEnv)

    # if hab_answer = "N" and pc_answer = "Y", set sub_dir_2 = "PC"
    if(pc_answer == "Y") {
      sub_dir_2 <- 'PC'

      # Remove pc_answer from the Global Environment
      rm(pc_answer, envir = .GlobalEnv)

    } else {
      # Remove pc_answer from the Global Environment
      rm(pc_answer, envir = .GlobalEnv)
    }
  }

  # check if directory ./Data exists
  if(file.exists(wd)) {

    # check if sub-directory NoMaxScores exists
    if(file.exists(file.path(wd, sub_dir_1))) {

      # check if sub-directory RAP_only exists
      if(file.exists(file.path(wd, sub_dir_1, sub_dir_2))) {
        # nothing needs to be done if all directories exist

        # if sub_dir_2 RAP_only doesn't exist, create it
      } else {
        dir.create(file.path(wd, sub_dir_1, sub_dir_2))
      }

      # if sub_dir_1 NoMaxScores doesn't exist, create it along with sub_dir_2
      # RAP_only
    } else {
      dir.create(file.path(wd, sub_dir_1, sub_dir_2))
    }

    # if wd doesn't exist, create it along with sub_dir_1 NoMaxScores and
    # sub_dir_2 RAP_only
  } else {
    dir.create(file.path(wd, sub_dir_1, sub_dir_2))
  }

  # Define year for filename
  year <- str_split_i(sample_date, '/', 3)

  # Convert date into Julian date using tojulian().
  jdate <- tojulian(month = str_split_i(sample_date, '/', 1),
                    day = str_split_i(sample_date, '/', 2),
                    year = year) # Defined above.

  # create the file name and location
  filename <- paste0(waterbody_nospaces, '_', year, jdate, '_NoMaxScores.xlsx') # Siteabbrev_YearJuliandate_NoMaxScores.xlsx # Ex: BroadCreek_2023229_NoMaxScores.xlsx

  # Remove date from the Global Environment
  rm(date, envir = .GlobalEnv)

  # Export list to excel
  write_xlsx(sheets, file.path(wd, sub_dir_1, sub_dir_2, filename))

}

################################################################################
