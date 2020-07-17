#' @title Function for generating exploratory figures.
#' 
#' @description Import a prepared file, make scatterplots of each variable against
#' each response (yield, protein, net-return) and maps each response variable and 
#' the as-applied nitrogen.
#'
#' @param file_name Filename with data containing yield and protein.
#' @param in_folder Location of the prepared data for exploring.
#' @param out_Folder Location to save exploratory plots.
#' @return Exploratory figures in the specified folder.
explore <- function(file_name, in_folder, out_folder, years) {
  field_name <- stringr::str_locate(file_name, "_")[1]
  field_name <- stringr::str_sub(file_name, 1, field_name - 1)
  year <- stringr::str_locate(file_name, paste(years, collapse = "|"))
  year <- stringr::str_sub(file_name, year[1], year[2])
  
  cwd <- paste0(out_folder, "/", field_name, "/explanatory") # outputs working directory
  if (!file.exists(cwd)) { 
    dir.create(cwd)
  }
  
  dat <- impDat(file_name, "prepped")
  exploreScatters(dat, field_name, out_folder)
  exploreMaps(dat, field_name, out_folder)
  return(invisible())
}
 
#' @title Function for generating exploratory scatterplots of responses 
#' vs explanatory variables.
#' 
#' @description Creates scatterplots of responses vs explanatory variables 
#' for protein, yield, and net-return. Saves to specified field's folder 
#' inside of the specified output folder.
#'
#' @param dat Data table with prepared data containing protein, yield, and 
#' net-return.
#' @param field_name Field name used for saving to correct output folder and 
#' for labeling plots.
#' @param out_Folder Location to save exploratory plots.
#' @return Exploratory scatterplots in the specified folder.
exploreScatters <- function(dat, field_name, out_folder) {
  if (any(grepl("yld", names(dat)))) {
    for (i in 1:ncol(dat)) {
      p <- ggplot(data = dat, aes_string(x = names(dat)[i], y = "yld")) +
        geom_point(aes(color = year, shape = field)) +
        labs(x = colnames(dat)[i], y = "yld") +
        theme_classic() +
        ggtitle(paste0(field_name, " ", unique(dat$year), " Yield"))
      ggsave(p, file = paste0(out_folder, "/", field_name, "/explanatory/", field_name, 
                            "_yld_vs_", colnames(dat)[i],".png"),
             width = 7.5, height = 7.5, units = "in")
    }
  }
  if (any(grepl("pro", names(dat)))) {
    for (i in 1:ncol(dat)) {
      p <- ggplot(data = dat, aes_string(x = names(dat)[i], y = "pro")) +
        geom_point(aes(color = year, shape = field)) +
        labs(x = colnames(dat)[i], y = "pro") +
        theme_classic() +
        ggtitle(paste0(field_name, " ", unique(dat$year), " Protein"))
      ggsave(p, file = paste0(out_folder, "/", field_name, "/explanatory/", field_name, 
                              "_pro_vs_", colnames(dat)[i],".png"),
             width = 7.5, height = 7.5, units = "in")
    }
  }
  if (any(grepl("NR", names(dat)))) {
    for (i in 1:ncol(dat)) {
      p <- ggplot(data = dat, aes_string(x = names(dat)[i], y = "NR")) +
        geom_point(aes(color = year, shape = field)) +
        labs(x = colnames(dat)[i], y = "NR") +
        theme_classic() +
        ggtitle(paste0(field_name, " ", unique(dat$year), " Net-Return"))
      ggsave(p, file = paste0(out_folder, "/", field_name, "/explanatory/", field_name, 
                              "_NR_vs_", colnames(dat)[i],".png"),
             width = 7.5, height = 7.5, units = "in")
    }
  }
  return(invisible())
}
 
#' @title Function for creating maps of response variables and as-applied nitrogen.
#' 
#' @description Creates maps of observed yield, interpolated protein, calculated 
#' net-return, and as-applied nitrogen. Saves to specified field's folder 
#' inside of the specified output folder.
#'
#' @param dat Data table with prepared data containing protein, yield, and 
#' net-return.
#' @param field_name Field name used for saving to correct output folder and 
#' for labeling plots.
#' @param out_Folder Location to save exploratory plots.
#' @return Exploratory maps in the specified folder.
exploreMaps <- function(dat, field_name, out_folder) {
  if (any(grepl("yld", names(dat)))) {
    fig <- suppressWarnings(
      suppressMessages(
        plotMaps(dat,
                 "yld",
                 "Yield (bu/ac)",
                 "Rasterized Observed Yield ",
                 farmername = "Farmer",
                 FIELD = field_name)
      )
    ) 
    ggsave(fig, 
           file = paste0(out_folder, "/", field_name, "/explanatory/", field_name, 
                         "_obsYld_", unique(dat$year), ".png"),
           width = 7.5, height = 7.5, units = "in")
  }
  if (any(grepl("pro", names(dat)))) {
    fig <- suppressWarnings(
      suppressMessages(
        plotMaps(dat,
                 "pro",
                 "Protein Percent",
                 "Rasterized Grain Protein ",
                 farmername = "Farmer",
                 FIELD = field_name)
      )
    ) 
    ggsave(fig, 
           file = paste0(out_folder, "/", field_name, "/explanatory/", field_name, 
                            "_obsPro_", unique(dat$year), ".png"),
           width = 7.5, height = 7.5, units = "in")
    
  }
  if (any(grepl("NR", names(dat)))) {
    fig <- suppressWarnings(
      suppressMessages(
        plotMaps(dat,
                 "NR",
                 "Net-Return ($/ac)",
                 "Rasterized Net-Return ",
                 farmername = "Farmer",
                 FIELD = field_name)
      )
    ) 
    ggsave(fig, 
           file = paste0(out_folder, "/", field_name, "/explanatory/", field_name, 
                         "_obsNR_", unique(dat$year), ".png"),
           width = 7.5, height = 7.5, units = "in")
  } 
  return(invisible())
} 
 
#' @title Creats a map of specified variables.
#' 
#' @description Creates maps of specified data and labels and returns the map.
#'
#' @param df Data table or data frame with prepared data containing specified 
#' variable.
#' @param varSelect Variable to map.
#' @param varLabel Label for the legend.
#' @param mainLabel Label for the plot.
#' @param FIELD Field name to use for labeling.
#' @param farmername Name of the farmer to use for labeling.
#' @return Exploratory map of specified variable
plotMaps <- function(df, varSelect, varLabel, mainLabel="", FIELD=NULL, farmername) {
  utmZone <- ifelse(farmername=="loewen",
                    14,
                    12)
  df <- as.data.frame(df)
  dfOG <- df # lapply on varSelect?
  for(i in 1:length(varSelect)){
    df <- dfOG
    if(length(which(is.na(df[,varSelect[i]])))>0){
      df <- df[-which(is.na(df[,varSelect[i]])),] ## subset rows with variable of interest
    }
    sp <- sp::SpatialPoints(coords=df[,c("x","y")])
    utm <- sp::SpatialPoints(sp, proj4string=sp::CRS(paste0("+proj=utm +zone=",utmZone)))
    longlat <- sp::spTransform(utm,sp::CRS("+proj=longlat +datum=WGS84"))
    llc <- as(longlat, "SpatialPointsDataFrame")
    llc <- as.data.frame(llc@coords)
    sp <- sp::SpatialPoints(coords=llc[,c("x","y")])
    e <- raster::extent(llc[,c("x","y")])
    rast <- raster::raster(ext=e, resolution=0.00015)
    rastVar <- raster::rasterize(sp, rast, df[,varSelect[i]], fun=mean,na.rm=TRUE)
    #map <- get_map(location = c(lon = mean(coordinates(longlat)[,1]), lat = mean(coordinates(longlat)[,2])), 
    #               zoom = 14,
    #               maptype = "satellite", source = "google")
    rSpdf <- as(rastVar, "SpatialPixelsDataFrame") #sp::SpatialPixelsDataFrame(rastVar) # 
    rDf <- as.data.frame(rSpdf)
    if(any(grepl("pro",varSelect[i],ignore.case = T))){
      color <- RColorBrewer::brewer.pal(15, "RdYlBu") #rev(topo.colors(15)) # rev(colorRamps::cyan2yellow(15))
    }else{
      if(any(grepl("yld",varSelect[i],ignore.case = T))){
        color <- RColorBrewer::brewer.pal(15, "RdYlGn") # rev(colorRamps::green2red(15)) #color <- rev(grDevices::heat.colors(15))
      }else{
        if(any(grepl("NR",varSelect[i]))){
          color <- RColorBrewer::brewer.pal(15, "Spectral") # rev(colorRamps::green2red(15)) #rev(topo.colors(15))
        }else{
          if(any(grepl("aa_n",varSelect[i]))|
             any(grepl("ndist",varSelect[i]))|
             any(grepl("aa_sr",varSelect[i]))){
            color <- rev(colorRamps::matlab.like2(15))
          }
        }
      }
    }
    colnames(rDf)[1] <- varLabel[1]
    main <- mainLabel[1] 
    if(grepl("prev",varSelect[1])){
      subMain <- df[1,"prev_yr"]
    }else{
      subMain <- df[1,"year"]
    }
    if(is.null(FIELD)){
      df$field <- as.character(df$field)
      FIELD <- ifelse(unique(df$field)>1,paste(unique(df$field),collapse=" & "),df$field[1])
    }
    #png(paste0(cwd,"/",fieldSelect,"/",yearSelect,"/ObservedMaps/",fieldSelect,"_",varSelect[i],".png"), width=10, height=10, units='in', res=100)
    varMap <- suppressMessages( ggplot()+ # ggmap(map,extent = "panel")
                                  geom_tile(data=rDf,aes(x=x,y=y,fill = rDf[,1])) + 
                                  scale_fill_gradientn(limits=c(floor(min(rDf[,1])),ceiling(max(rDf[,1]))),
                                                       colours=color,
                                                       breaks=seq(as.integer(floor(min(rDf[,1]))),as.integer(ceiling(max(rDf[,1]))),by=(ceiling(max(rDf[,1]))-floor(min(rDf[,1])))/5),
                                                       name=colnames(rDf)[1]) +
                                  scale_x_continuous(limits = c(e@xmin-0.001, e@xmax+0.001), expand = c(0, 0),breaks=c(e@xmin-0.001, e@xmax+0.001)) +
                                  scale_y_continuous(limits = c(e@ymin-0.001, e@ymax+0.001), expand = c(0, 0)) +
                                  labs(title =  paste0(main), subtitle = paste0(FIELD,
                                                                                " ",
                                                                                subMain), x="", y="") +
                                  theme_bw() +
                                  theme(axis.text.x=element_blank(),
                                        axis.text.y=element_blank())) #+
                                  # legendMap::scale_bar(lon=e@xmin-0.0005,lat=e@ymin-0.00075,distance_lon = 0.2, distance_lat = .01, distance_legend = -.01, 
                                  #           dist_unit = "km", orientation = TRUE, arrow_length = .05, arrow_distance =  .02))  
    #suppressWarnings(print(varMap))
    #dev.off()
    return(varMap)
  }
  #setwd(cwd)
} 
    
      