library(data.table)
library(dplyr)

#' @title Function for importing, cleaning, and selecting the data
#'
#' @description Calls impDat(), clnDat(), selDat() and saves data to a the 
#' specified output folder. Takes the arguments of the three functions above.
#'
#' @param raw_name Name of the file to import.
#' @param in_folder Location of the folder containing the raw data.
#' @param out_folder Location to save the cleaned data.
#' @param veg_index Vegetation index to select. "NDVI", "NDRE", "CIRE"
#' @return Data imported, cleaned, subsetted, and saved into specified folder.
impClnSel <- function(raw_name, in_folder, out_folder, veg_index) {
  stopifnot(
    is.character(raw_name),
    is.character(veg_index),
    is.character(in_folder),
    is.character(out_folder),
    grepl("NDVI|NDRE|CIRE", veg_index)
  )
  dat <- impDat(raw_name, in_folder) %>%
    clnDat() %>%
    selDat(veg_index) 
  data.table::fwrite(dat, paste0(out_folder, "/", raw_name))
  return()
}

#' @title Function for importing raw data.
#'
#' @description Uses the data.table package function 'fread' to bring in
#' .csv data.
#'
#' @param raw_name Name of the file to import.
#' @param in_folder Location of the folder containing the raw data.
#' @return Imported data table.
impDat <- function(raw_name, in_folder) {
  dat <- data.table::fread(paste0(in_folder, "/", raw_name))
  return(dat)
}

#' @title Function for cleaning raw data.
#'
#' @description Takes a data table and converts any character "NULL" values 
#' to NA. Then removes rows in which the response variable (yield or protein)
#' is NA (missing observations). 
#'
#' @param dat Data table or data frame to clean.
#' @return Clean data table.
clnDat <- function(dat) {
  dat <- as.data.frame(dat)
  stopifnot(
    is.data.frame(dat)
  )
  for (j in 1:ncol(dat)) {
    col_name <- names(dat)[j]
    dat[dat[, col_name] == "NULL", col_name] <- NA
  }
  # only select non NA observations
  resp_col <- grep("^yld$|^pro$", names(dat))
  dat <- dat[!is.na(dat[, resp_col]), ] %>% 
    data.table::as.data.table()
  return(dat)
}

#' @title Function for selecting variables from the raw data.
#'
#' @description Subsets out columns to use for analysis from the raw data. 
#' This includes removing unneeded columns (i.e. geometry, grid_cell, etc.)
#' and aggregating other columns from various sources (i.e. precipitation 
#' from Daymet and Gridmet). 
#'
#' @param dat Data table to select data from.
#' @param veg_index Vegetation index to select. "NDVI", "NDRE", "CIRE"
#' @return Data table with variables used for analysis.
selDat <- function(dat, veg_index) {
  stopifnot(
    is.data.frame(dat),
    is.character(veg_index)
  )
  veg_index <- tolower(veg_index)
  dat <- dplyr::select(dat, -c("geometry", "cell_id", "grid", "ssm_cy",
                               "ssm_py", "susm_cy", "susm_py", "musym"))
  df <- matrix(NA, nrow = nrow(dat), ncol = 7) %>%
    as.data.frame()
  names(df) <- c("prec_cy","prec_py","gdd_cy","gdd_py",
                 "veg_cy","veg_py","veg_2py")
  df$prec_cy <- ifelse(!is.na(dat$prec_cy_d),
                       dat$prec_cy_d,
                       ifelse(!is.na(dat$prec_cy_g),
                              dat$prec_cy_g,
                              NA)) 
  # rnorm(nrow(dat),
  #       mean(Prc[Prc$Year==YEAR]$winter.pr),
  #       3)
  df$prec_py <- ifelse(!is.na(dat$prec_py_d),
                       dat$prec_py_d,
                       ifelse(!is.na(dat$prec_py_g),
                              dat$prec_py_g,
                              NA))
  # rnorm(nrow(dat),
  #       mean(Prc[Prc$Year==YEAR]$ante.pr + 
  #              Prc[Prc$Year==YEAR]$growing.pr),
  #       3)
  df$gdd_cy <- ifelse(!is.na(dat$gdd_cy_d),
                      dat$gdd_cy_d,
                      ifelse(!is.na(dat$gdd_cy_g),
                             dat$gdd_cy_g,
                             NA))
  # rnorm(nrow(dat),
  #       mean(Prc[Prc$Year==YEAR]$march.gdd),
  #       3)
  df$gdd_py <- ifelse(!is.na(dat$gdd_py_d),
                      dat$gdd_py_d,
                      ifelse(!is.na(dat$gdd_py_g),
                             dat$gdd_py_g,
                             NA))
  # rnorm(nrow(dat),
  #       mean(Prc[Prc$Year==YEAR]$july.gdd),
  #       3)
  if(veg_index == "ndvi"){
    # select veg index data
    df$veg_cy <- ifelse(!is.na(dat$ndvi_cy_s),
                        dat$ndvi_cy_s,
                        dat$ndvi_cy_l)
    df$veg_py <- ifelse(!is.na(dat$ndvi_py_s),
                        dat$ndvi_py_s,
                        dat$ndvi_py_l)
    df$veg_2py <- ifelse(!is.na(dat$ndvi_2py_s),
                         dat$ndvi_2py_s,
                         dat$ndvi_2py_l)
  }
  if(veg_index=="ndre"){
    df$veg_cy <- dat$ndre_cy
    df$veg_py <- dat$ndre_py
    df$veg_2py <- dat$ndre_2py
  }
  if(veg_index == "cire"){
    df$veg_cy <- dat$cire_cy
    df$veg_py <- dat$cire_py
    df$veg_2py <- dat$cire_2py
  }
  trimCols <- c("prec_cy_d","prec_py_d","gdd_cy_d","gdd_py_d",
                "prec_cy_g","prec_py_g","gdd_cy_g","gdd_py_g",
                "ndvi_cy_s","ndvi_py_s","ndvi_2py_s",
                "ndvi_cy_l","ndvi_py_l","ndvi_2py_l",
                "ndre_cy","ndre_py","ndre_2py",
                "cire_cy","cire_py","cire_2py")
  dat <- dat[,!..trimCols]
  dat <- cbind(dat,df)
  return(dat)
}



