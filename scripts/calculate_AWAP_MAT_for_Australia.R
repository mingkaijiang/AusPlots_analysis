calculate_AWAP_MAT_for_Australia <- function(sourceDir, destDir) {
    
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
    out1 <- out2 <- array(NA, c(lon.length, lat.length, n.yr))
    
    ### loop
    for (i in 1:n.yr) {
        
        ### list files in a directory
        sub.files1 <- list.files(path = paste0(sourceDir, "tmax/", yr.list[i]),
                                pattern = ".grid")
        
        sub.files2 <- list.files(path = paste0(sourceDir, "tmin/", yr.list[i]),
                                 pattern = ".grid")
        
        ### n.files
        n.files <- length(sub.files1)
        
        ### prepare tmp storage
        tmp1 <- tmp2 <- array(NA, c(lon.length, lat.length, n.files))

        
        ### loop through all files in this year
        for (j in 1:n.files) {
            inName1 <- paste0(sourceDir, "tmax/", yr.list[i], "/", sub.files1[j])
            inName2 <- paste0(sourceDir, "tmin/", yr.list[i], "/", sub.files2[j])
            
            myDF1 <- read.ascii.grid(inName1)
            myDF2 <- read.ascii.grid(inName2)
            
            ### save data
            tmp1[, , j] <- myDF1$data
            tmp2[, , j] <- myDF2$data
            
            ## checking script
            #test <- myDF$data
            #test <- tmp[,,1]
            #r <- raster(test)
            #plot(r)
            
        } ## j
        
        out1[,,i] <- rowMeans(tmp1, dims = 2)
        out2[,,i] <- rowMeans(tmp2, dims = 2)
        
        #test <- out[,,1]
        #r <- raster(test)
        #plot(r)

    } ### i loop
    
    ### calculate 30-yr means
    outDF1 <- array(NA, c(lon.length, lat.length))
    outDF1 <- rowMeans(out1, dims=2)
    
    outDF2 <- array(NA, c(lon.length, lat.length))
    outDF2 <- rowMeans(out2, dims=2)
    
    #r <- raster(outDF2)
    #plot(r)
    
    test1 <- melt(outDF1)
    colnames(test1) <- c("latID", "lonID", "Tmax")
    
    test2 <- melt(outDF2)
    colnames(test2) <- c("latID", "lonID", "Tmin")
    
    test <- merge(test1, test2, by=c("latID", "lonID"))
    
    test$Tmn <- with(test, (Tmax+Tmin)/2)
    
    test <- test[,c("latID", "lonID", "Tmn")]
    
    ### assign values to latlonDF
    latlonDF <- merge(latlonDF, test, by=c("latID", "lonID"))
    
    #test <- latlonDF[,c("lon", "lat", "Tmn")]
    #test2 <- rasterFromXYZ(test)
    #plot(test2)
    
    ### save output
    saveRDS(latlonDF, file=paste0(destDir, "/Australia_30-yr_MAT.rds"))
    
    
}   # function loop
