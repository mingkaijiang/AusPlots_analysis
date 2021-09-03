Australia_Whittaker_diagram <- function (sourceDir,
                                         destDir,
                                         awap,
                                         to.plot=T,
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
    
    aus.raster <- raster(paste0(file.path, "w001000.adf"))

    ### process awap data 
    mapDF <- awap[,c("lon", "lat", "MAP")]
    matDF <- awap[,c("lon", "lat", "Tmn")]
    
    map.raster <- rasterFromXYZ(mapDF)
    mat.raster <- rasterFromXYZ(matDF)
    
    
    ### reproject
    crs.test <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    crs.aus <- crs(aus.raster)
    
    ### add crs to map and mat
    crs(map.raster) <- crs.test
    crs(mat.raster) <- crs.test
    
    map.raster.proj <- projectRaster(map.raster, crs=crs.aus)
    mat.raster.proj <- projectRaster(mat.raster, crs=crs.aus)
    
    ### the rasters must have the same extent
    e1 <- extent(map.raster.proj)
    e2 <- extent(aus.raster)
    
    
    ## extract a smaller region
    map.sub.raster <- crop(map.raster.proj, e2)
    mat.sub.raster <- crop(mat.raster.proj, e2)
    
    ## disaggregate
    map.dis.raster <- disaggregate(map.sub.raster, c(49,56))
    mat.dis.raster <- disaggregate(mat.sub.raster, c(49,56))
    
    
    #fastRandomPoints <- function(r, n) {
    #    if(raster::nlayers(r) > 1) r <- r[[1]]
    #    v <- raster::getValues(r)
    #    v.notNA <- which(!is.na(v))
    #    x <- sample(v.notNA, n)
    #    pts <- raster::xyFromCell(r, x)
    #    return(pts)
    #}
    
    ### given the large dataset, randomly sampling the raster
    t1.raster <- sample(aus.raster, 100000, replace=F)
    t2.raster <- which(!is.na(t1.raster))
    t3.coord <- xyFromCell(aus.raster, t2.raster)
    
    ### subset
    sub.raster <- crop(aus.raster, c(1000000,1050000,-2000000,-1005000))
    sub.test <- oceanmap::raster2matrix(aus.raster)
    test <- which(sub.test == 1, arr.ind=T)
    test2 <- xyFromCell(sub.raster, test)
    
    ### now we know the location of the data points, get their climate
    #test <- extract(map.dis.raster, t3.coord, method="bilinear")
    
    
    test.raster <- aus.raster == 1
    out.raster <- mask(sub.raster, test.raster, maskvalue=1)
    test <- rasterToPoints(aus.raster)
    
    plot(test.raster)
    
    
    
    
    
    
    ### whether to delete the downloaded data or not
    if (remove.after.processing==T) {
        system(paste0("rm ", destDir, "/NVIS_V6.zip"))
        system(paste0("rm -r ", destDir, "/NVIS_V6"))
        system(paste0("rm -r ", destDir, "/__MACOSX"))
        
    }
    
}