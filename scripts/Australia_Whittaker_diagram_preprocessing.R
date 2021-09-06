Australia_Whittaker_diagram_preprocessing <- function (sourceDir,
                                                       destDir,
                                                       awap,
                                                       remove.after.processing=T) {
 
    ### download the zip file first
    ### downloading this dataset takes ages!
    cloud_get(path = paste0(sourceDir, "/NVIS_V6.zip"),
              dest = paste0(destDir, "/NVIS_V6.zip"),
              open_file = F)
    
    ### unzip the file
    system(paste0("unzip ", destDir, "/NVIS_V6.zip -d ", destDir))
    
    ### read in the data - raster format
    file.path <- paste0(destDir, "/NVIS_V6/GRID_NVIS6_0_AUST_EXT_MVG/aus6_0e_mvg/")
    
    ### raster
    aus.raster <- raster(paste0(file.path, "w001000.adf"))
    
    ### aggregate to obtain large file
    #aus.raster.agg <- aggregate(aus.raster, fact=5, fun="modal")
    
    ### crs
    crs.aus <- crs(aus.raster)
    e <- extent(aus.raster)
    
    ### split into 20 sub rasters
    n.lyr.x <- 20
    #n.lyr.y <- 2
    x.range <- seq(e[1], e[2], (e[2]-e[1])/n.lyr.x)
    #y.range <- seq(e[3], e[4], (e[4]-e[3])/n.lyr.y)
    
    ### reproject
    #crs.test <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    rasterOptions(memfrac=.3)
    
    for (i in 1:n.lyr.x) {
        #for (j in 1:n.lyr.y) {
            ### i = 7, -685000  -484500 -4841000 -1004000
            #e.sub <- c(x.range[i], x.range[i+1], 
            #           y.range[j], y.range[j+1])
            
            ### create the subset extent
            e.sub <- c(x.range[i], x.range[i+1], 
                       e[3], e[4])
            
            ### crop
            sub.raster <- crop(aus.raster, e.sub)
            
            ### re-project
            #sub.reproj.raster <- projectRaster(sub.raster, crs=crs.test,
            #                                   method="ngb")
            
            ### convert into xy
            sub.points <- rasterToPoints(sub.raster)
            
            ### convert to dataframe
            sub.points <- as.data.frame(sub.points)
            
            ### convert from AEA to lonlat
            sub.points$Lon0 <- (13200000+sub.points$x) / (1e+5)
            sub.points$Lat0 <- sub.points$y / (1e+5)
            
            ### assign the nearest 0.5 degree
            sub.points$Lon1 <- as.numeric(gsub("\\..*", "", sub.points$Lon0))
            sub.points$Lon2 <- as.numeric(paste0("0.", gsub(".*\\.", "", sub.points$Lon0)))
            
            sub.points$Lat1 <- as.numeric(gsub("\\..*", "", sub.points$Lat0))
            sub.points$Lat2 <- as.numeric(paste0("0.", gsub(".*\\.", "", sub.points$Lat0)))
            
            sub.points$Lon <- ifelse(sub.points$Lon2<0.475, sub.points$Lon1-1+0.975,
                                     ifelse(sub.points$Lon2>=0.975, sub.points$Lon1+0.975,
                                            sub.points$Lon1+0.475))
            
            sub.points$Lat <- ifelse(sub.points$Lat2<0.475, sub.points$Lat1+0.025,
                                     ifelse(sub.points$Lat2>=0.975, sub.points$Lat1-0.975,
                                            sub.points$Lat1-0.475))
            
            sub.points$Lon1 <- NULL
            sub.points$Lon2 <- NULL
            sub.points$Lat1 <- NULL
            sub.points$Lat2 <- NULL
            sub.points$Lon0 <- NULL
            sub.points$Lat0 <- NULL
            
            ### merge with awapDF
            outDF <- merge(sub.points, awap, by.x=c("Lon", "Lat"), 
                           by.y=c("lon", "lat"),
                           all.x=T)
            
            ### save
            saveRDS(outDF, paste0("output/NVIS_v6_AWAP_df", i, #"_", j, 
                                  ".rds"))
            
            rm(outDF, sub.points, sub.raster)
        #}
        
    }
    
    
    ### now loop through individual rds to get the information needed
    extract_climate_per_NVIS_vegetation_type(n.lyr.x)
    
    
    
    
    
 
    ### whether to delete the downloaded data or not
    if (remove.after.processing==T) {
        system(paste0("rm ", destDir, "/NVIS_V6.zip"))
        system(paste0("rm -r ", destDir, "/NVIS_V6"))
        system(paste0("rm -r ", destDir, "/__MACOSX"))
        
    }
    
}