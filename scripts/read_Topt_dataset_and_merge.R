read_Topt_dataset_and_merge <- function(inDF) {
    
    mgDF <- readRDS("output/Topt/merged_temperature_parameters.rds")
    
    ### read in lonlat information
    lonlatDF <- read.csv("output/Topt/lonlat.csv")
    
    ### prepare lonlatDF
    lonlatDF$T_mean <- as.vector(mgDF[,,1])
    lonlatDF$T_sd <- as.vector(mgDF[,,2])
    lonlatDF$T_opt <- as.vector(mgDF[,,3])
    lonlatDF$T_param <- as.vector(mgDF[,,4])
    
    ### convert into raster
    myDF1 <- lonlatDF[,c("lon", "lat", "T_sd")]
    myDF2 <- lonlatDF[,c("lon", "lat", "T_opt")]
    myDF3 <- lonlatDF[,c("lon", "lat", "T_param")]
    
    r1 <- rasterFromXYZ(myDF1)
    r2 <- rasterFromXYZ(myDF2)
    r3 <- rasterFromXYZ(myDF3)
    
    
    ### inDF get spatial points
    spDF <- inDF[,c("Lon", "Lat")]
    coordinates(spDF) <- ~Lon+Lat
 
    ### extract
    spDF1 <- extract(r1, spDF)
    spDF2 <- extract(r2, spDF)
    spDF3 <- extract(r3, spDF)
    
    ### inDF
    outDF <- inDF
    outDF$Tsd <- spDF1
    outDF$Topt <- spDF2
    outDF$TSM <- spDF3
    
    return(outDF)
    
}



