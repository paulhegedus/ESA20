#' @title Wrapper function for interpolating protein.
#'
#' @description Calls interpolatePro() and saves prepared yield data to a 
#' user specified output folder. 
#'
#' @param year_list List with filenames for data for a field from the most 
#' recent and previous years.
#' @param in_folder Location of the folder containing the data to import.
#' @param out_folder Location to save the interpolated data.
#' @param years Vector of possible years found in the datasets.
#' @return Data imported, protein interpolated to yield locations, and saved 
#' into specified folder with prepared data.
interpolateProWrapper <- function(year_list, in_folder, out_folder, years) {
  output <- lapply(year_list, interpolatePro, in_folder, years)
  lapply(output, saveDat, out_folder)
  return(invisible())
}

#' @title Function for interpolating protein to yield data.
#'
#' @description Takes a vector with protein and yield data, and uses
#' universal kriging to interpolate protein data to the locations of the
#' yield data, consolidating the two response variables into the dataset 
#' with the more spatially dense data.
#'
#' @param dat_vec Vector of filenames with protein and yield data for a 
#' single year.
#' @param in_folder Location of the folder containing the data to import.
#' @return Yield data with protein interpolated to observed locations.
interpolatePro <- function(dat_vec, in_folder, years) {
  output <- list()
  field_name <- stringr::str_locate(dat_vec[1], "_")[1]
  output$field_name <- stringr::str_sub(dat_vec[1], 1, field_name - 1)
  year <- stringr::str_locate(dat_vec[1], paste(years, collapse = "|"))
  output$year <- stringr::str_sub(dat_vec[1], year[1], year[2])
  
  dat_list <- list(pro = dat_vec[grep("pro", dat_vec)],
                   yld = dat_vec[grep("yld", dat_vec)])
  dat_list <- lapply(dat_list, impDat, in_folder) %>%
    lapply(as.data.frame) %>%
    lapply(function(x) {x$X <- x$x; x$Y <- x$y; sp::coordinates(x) <- ~X+Y; return(x)})
  ## fit variogram
  dpVgm <- gstat::variogram(pro~x+y,dat_list$pro)
  dpVgmFit <- suppressWarnings(
    gstat::fit.variogram(dpVgm,
                         gstat::vgm(c("Cir","Sph","Pen","Mat","Nug","Exp","Gau",
                                      "Exc","Ste","Lin", "Bes", "Per","Wav",
                                      "Hol","Log","Spl")))
  )
  #plot(dpVgm, dpVgmFit)
  krigVal <- gstat::krige(pro~x+y, dat_list$pro, dat_list$yld, dpVgmFit) 
  dat_list$yld$pro <- krigVal$var1.pred 
  output$dat <- as.data.frame(dat_list$yld)
  
  png(paste0("results/", output$field_name, "/", output$field_name, "_", 
             output$year, "_krigedPro.png"),
      width = 8, height = 6, units = "in", res = 300)
    par(mfrow = c(1,2))
    plot(dat_list$pro$x, dat_list$pro$y, col = dat_list$pro$pro,
         main = paste0("Observed Protein: ",output$field_name, " ", output$year),
         xlab = "Longitude - UTM Zone 12", ylab = "Latitude - UTM Zone 12")
    plot(dat_list$yld$x, dat_list$yld$y, col = dat_list$yld$pro,
         main = paste0("Kriged Protein: ",output$field_name, " ", output$year),
         xlab = "Longitude - UTM Zone 12", ylab = "Latitude - UTM Zone 12")
    par(mfrow = c(1,1))
  dev.off() %>% invisible()
  return(output)
}

#' @description Saves data from a list containing fields for a field name, year data
#' collected, and the data itself. Saves to a specified outputs folder with a naming
#' convention of 'fieldname_year.csv'.
#'
#' @param out_list List with a field_name, year, and data.
#' @param out_folder Location of the folder to save data to.
#' @return Data saved to specified folder.
saveDat <- function(out_list, out_folder) {
  data.table::fwrite(out_list$dat, 
                     paste0(out_folder, "/", 
                            out_list$field_name, "_",
                            out_list$year, ".csv"))
}