library(data.table)

#' @title Wrapper function for calculating and adding a net-return column to data 
#' for analysis.
#'
#' @description Imports the specified filename, calculates net-return based off of 
#' yield and protein, adds it as a column and resaves to the folder the data was 
#' found int.
#'
#' @param file_name Filename with data containing yield and protein.
#' @param folder Location of the folder containing the data to import and the location
#' to save the data.
#' @param econ_list List of economic parameters needed for NRcalcFun().
#' @return Data imported, net-return calculated, and re-saved 
#' into specified folder with prepared data.
NRfunWrapper <- function(filename, folder, econ_list) {
  dat <- impDat(filename, folder)
  dat <- NRcalcFun(dat, econ_list)
  data.table::fwrite(dat, paste0(folder, "/", filename))
  return(invisible())
}

#' @title Function for calculating and adding a net-return column to data 
#' for analysis.
#'
#' @description Takes a data.table or data.frame and calculates net-return based off of 
#' yield and protein and a named list of economic paramters. These include;
#' Bp = Base price received per bushel, B0pd - B2pd = Protein premium/dockage model 
#' coefficients, CN = Cost of nitrogen per lbs, FC = Other fixed costs per acre.
#'
#' @param dat Filename with data containing yield and protein.
#' @param econ_list List of economic paramters for calculating a net-return in $/acre.
#' @return Data table with a new column for net-return.
NRcalcFun <- function(dat, econ_list) {
  Bp <- econ_list$Bp
  B0pd <- econ_list$B0pd
  B1pd <- econ_list$B1pd
  B2pd <- econ_list$B2pd
  CN <- econ_list$CN
  FC <- econ_list$FC
  
  prosq <- dat$pro^2
  P <- Bp + (B0pd + B1pd * dat$pro + B2pd * prosq)
  dat$NR <- dat$yld * P - CN * dat$aa_n - FC
  return(dat)
}
