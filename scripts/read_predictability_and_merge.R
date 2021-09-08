read_predictability_and_merge <- function(inDF) {
    
    lonlatDF <- read.csv("output/Topt/biome_temp_prec_full_1991_2012.csv")
    
    
    ### convert into raster
    myDF1 <- lonlatDF[,c("lon", "lat", "tempP")]
    myDF2 <- lonlatDF[,c("lon", "lat", "tempC")]
    myDF3 <- lonlatDF[,c("lon", "lat", "tempM")]
    
    myDF4 <- lonlatDF[,c("lon", "lat", "precP")]
    myDF5 <- lonlatDF[,c("lon", "lat", "precC")]
    myDF6 <- lonlatDF[,c("lon", "lat", "precM")]
    
    r1 <- rasterFromXYZ(myDF1)
    r2 <- rasterFromXYZ(myDF2)
    r3 <- rasterFromXYZ(myDF3)
    
    r4 <- rasterFromXYZ(myDF4)
    r5 <- rasterFromXYZ(myDF5)
    r6 <- rasterFromXYZ(myDF6)
    
    
    ### inDF get spatial points
    spDF <- inDF[,c("Lon", "Lat")]
    coordinates(spDF) <- ~Lon+Lat
 
    ### extract
    spDF1 <- extract(r1, spDF)
    spDF2 <- extract(r2, spDF)
    spDF3 <- extract(r3, spDF)
    
    spDF4 <- extract(r4, spDF)
    spDF5 <- extract(r5, spDF)
    spDF6 <- extract(r6, spDF)
    
    ### inDF
    outDF <- inDF
    outDF$tempP <- spDF1
    outDF$tempC <- spDF2
    outDF$tempM <- spDF3
    
    outDF$precP <- spDF4
    outDF$precC <- spDF5
    outDF$precM <- spDF6
    
    return(outDF)
    
}



