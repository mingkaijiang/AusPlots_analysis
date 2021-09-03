Australia_Whittaker_diagram <- function (sourceDir,
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
    
    ### crs
    crs.aus <- crs(aus.raster)
    e <- extent(aus.raster)
    
    ### split into 20 sub rasters
    n.lyr <- 20
    x.range <- seq(e[1], e[2], (e[2]-e[1])/n.lyr)

    ### reproject
    crs.test <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    
    for (i in 1:n.lyr) {
        
        ### create the subset extent
        e.sub <- c(x.range[i], x.range[i+1], 
                   e[3], e[4])
        
        ### crop
        sub.raster <- crop(aus.raster, e.sub)
        
        ### re-project
        sub.reproj.raster <- projectRaster(sub.raster, crs=crs.test)
        
        ### convert into xy
        sub.points <- rasterToPoints(sub.reproj.raster)
        
        ### convert to dataframe
        sub.points <- as.data.frame(sub.points)
        
        ### assign the nearest 0.5 degree
        sub.points$Lon1 <- as.numeric(gsub("\\..*", "", sub.points$x))
        sub.points$Lon2 <- as.numeric(paste0("0.", gsub(".*\\.", "", sub.points$x)))
        
        sub.points$Lat1 <- as.numeric(gsub("\\..*", "", sub.points$y))
        sub.points$Lat2 <- as.numeric(paste0("0.", gsub(".*\\.", "", sub.points$y)))
        
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
        
        ### merge with awapDF
        outDF <- merge(sub.points, awap, by.x=c("Lon", "Lat"), 
                       by.y=c("lon", "lat"),
                       all.x=T)
        
        ### save
        saveRDS(outDF, paste0("output/NVIS_v6_AWAP_df", i, ".rds"))
        
        
    }
    
 
    ### whether to delete the downloaded data or not
    if (remove.after.processing==T) {
        system(paste0("rm ", destDir, "/NVIS_V6.zip"))
        system(paste0("rm -r ", destDir, "/NVIS_V6"))
        system(paste0("rm -r ", destDir, "/__MACOSX"))
        
    }
    
}