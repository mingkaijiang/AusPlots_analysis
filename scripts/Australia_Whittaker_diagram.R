Australia_Whittaker_diagram <- function (sourceDir,
                                         destDir,
                                         awap,
                                         to.plot=T,
                                         remove.after.processing=T) {
 
    ### download the zip file first
    cloud_get(path = paste0(sourceDir, "/NVIS_V6.zip"),
              dest = paste0(destDir, "/NVIS_V6.zip"),
              open_file = F)
    
    ### unzip the file
    system(paste0("unzip ", destDir, "/NVIS_V6.zip -d ", destDir))
    
    ### read in the data - raster format
    file.path <- paste0("data/NVIS_V6/GRID_NVIS6_0_AUST_EXT_MVG/aus6_0e_mvg/")
    
    aus.raster <- raster(paste0(file.path, "w001000.adf"))
    #myDF2 <- raster(paste0(file.path, "w001001.adf"))
    
    ### process awap data 
    mapDF <- awap[,c("lon", "lat", "MAP")]
    matDF <- awap[,c("lon", "lat", "Tmn")]
    
    map.raster <- rasterFromXYZ(mapDF)
    mat.raster <- rasterFromXYZ(matDF)
    
    
    ### reproject
    crs.test <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    test <- projectRaster(aus.raster, crs=crs.test)
    
    ### after reprojection, resample
    test <- resample(map.raster, aus.raster, method="bilinear")
    
    ### after resampling, split the raster into smaller ones
    
    ### loop through each smaller raster to extract vegetation and climate information
    
    
    ### the raster is at 100 m resolution, too small, aggregate it
    #test <- aggregate(myDF1, fact=50, fun=max, na.rm=T)
    
    ### the file is too large to process in one go, split it
    #destDir <- "output"
    #y0 <- splitRaster(myDF1, 10, 5, path = file.path(destDir, "y0")) #
    
    test1 <- as.data.frame(test, xy=T)
    
    
    
    ### whether to delete the downloaded data or not
    if (remove.after.processing==T) {
        system(paste0("rm ", destDir, "/NVIS_V6.zip"))
        system(paste0("rm -r ", destDir, "/NVIS_V6"))
        system(paste0("rm -r ", destDir, "/__MACOSX"))
        
    }
    
}