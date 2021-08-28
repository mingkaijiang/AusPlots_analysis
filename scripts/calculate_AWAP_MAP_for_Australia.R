calculate_AWAP_MAP_for_Australia <- function(sourceDir, destDir) {
    
    #### Create output folder
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### prepare year DFs
    yr.list <- c(1971:2000)
    n.yr <- length(yr.list)
    
    ### grid information
    lat.id <- c(1:691)
    lat.lab <- paste0("lat", lat.id)
    lon.length <- max(lat.id)
    
    lon.id <- c(1:886)
    lon.lab <- paste0("lon", lon.id)
    lat.length <- max(lon.id)
    
    lon <- seq(111.975, 111.975 + (0.05 * 885), by=0.05)
    lat <- seq(-10.025, -10.025 + (-0.05 * 690), by=-0.05)
    
    ### create lon lat DF for future plotting
    latlonDF <- data.frame(rep(lat.id, each = max(lon.id)),
                           rep(lon.id, max(lat.id)), 
                           rep(lat, each = max(lon.id)),
                           rep(lon, max(lat.id)))
    colnames(latlonDF) <- c("latID", "lonID", "lat", "lon")
    
    ### out DF
    out <- array(NA, c(lon.length, lat.length, n.yr))
    
    for (i in 1:n.yr) {
        
        ### list files in a directory
        sub.files <- list.files(path = paste0(sourceDir, yr.list[i]),
                                pattern = ".grid")
        
        ### n.files
        n.files <- length(sub.files)
        
        ### prepare tmp storage
        tmp <- array(NA, c(lon.length, lat.length, n.files))

        
        ### loop through all files in this year
        for (j in 1:n.files) {
            inName <- paste0(sourceDir, yr.list[i], "/", sub.files[j])
            
            myDF <- read.ascii.grid(inName)
            
            ### save data
            tmp[, , j] <- myDF$data
            
            ## checking script
            #test <- myDF$data
            #test <- tmp[,,1]
            #r <- raster(test)
            #plot(r)
            
        } ## j
        
        out[,,i] <- rowSums(tmp, dims = 2)

        #test <- out[,,1]
        #r <- raster(test)
        #plot(r)

    } ### i loop
    
    ### calculate 30-yr means
    outDF <- array(NA, c(lon.length, lat.length))
    outDF <- rowMeans(out, dims=2)
    #r <- raster(outDF)
    #plot(r)
    
    test <- melt(outDF)
    colnames(test) <- c("latID", "lonID", "MAP")
    
    ### assign values to latlonDF
    latlonDF <- merge(latlonDF, test, by=c("latID", "lonID"))
    
    #test <- latlonDF[,c("lon", "lat", "MAP")]
    #test2 <- rasterFromXYZ(test)
    #plot(test2)
    
    ### save output
    saveRDS(latlonDF, file=paste0(destDir, "/Australia_30-yr_MAP.rds"))
    
    
}   # function loop
