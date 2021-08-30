download_AusTrait_from_CloudStor <- function(sourceDir,
                                             destDir,
                                             awap,
                                             to.plot=T,
                                             remove.after.processing=T) {
    
    ### download the zip file first
    cloud_get(path = paste0(sourceDir, "/AusTrait.zip"),
              dest = paste0(destDir, "/AusTrait.zip"),
              open_file = F)
    
    ### unzip the file
    system(paste0("unzip ", destDir, "/AusTrait.zip -d ", destDir))
    
    ### read the data
    myDF <- readRDS(paste0(destDir, "/AusTrait/austraits-3.0.2.rds"))
    
    ### get site information
    austrait.sites <- myDF$sites
    
    ### get site ID
    tmpDF1 <- austrait.sites[austrait.sites$site_property=="longitude (deg)",]
    tmpDF2 <- austrait.sites[austrait.sites$site_property=="latitude (deg)",]
    
    names(tmpDF1)[names(tmpDF1)=="value"] <- "longitude"
    names(tmpDF2)[names(tmpDF2)=="value"] <- "latitude"
    
    tmpDF1$site_property <- NULL
    tmpDF2$site_property <- NULL
    
    tmpDF <- merge(tmpDF1, tmpDF2, by=c("dataset_id", "site_name"))
    tmpDF$longitude <- as.numeric(tmpDF$longitude)
    tmpDF$latitude <- as.numeric(tmpDF$latitude)
    
    tmpDF <- tmpDF[complete.cases(tmpDF$latitude),]
    tmpDF <- tmpDF[complete.cases(tmpDF$longitude),]
    
    ### create a new outDF
    outDF <- tmpDF
    
    ### overlap with awap data to get MAT and MAP
    coordinates(tmpDF) = ~longitude+latitude
    
    ### get awap raster
    matDF <- awap[,c("lon", "lat", "Tmn")]
    mapDF <- awap[,c("lon", "lat", "MAP")]
    
    mat.raster <- rasterFromXYZ(matDF)
    map.raster <- rasterFromXYZ(mapDF)
    
    outDF$MAT <- extract(mat.raster, tmpDF)
    outDF$MAP <- extract(map.raster, tmpDF)
    
    ### plotting script
    if (to.plot == T) {
        ### australia polygon
        aus.poly <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
        
        pdf(paste0("output/AusTrait_spatial_coverage.pdf"), width=10, height=6)
        p1 <- ggplot(data=aus.poly)+
            geom_point(outDF, 
                       mapping=aes(x=longitude, y=latitude),
                       pch=21)+
            geom_sf(fill=NA)+
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_text(size=12),
                  axis.title.x=element_text(size=14),
                  axis.text.y=element_text(size=12),
                  axis.title.y=element_text(size=14),
                  legend.text=element_text(size=12),
                  legend.title=element_text(size=14),
                  panel.grid.major=element_blank(),
                  legend.position="bottom",
                  legend.box = 'vertical',
                  legend.box.just = 'left')+
            ylim(-45, -10)+
            xlim(100,160)
        
        plot(p1)
        dev.off()
        
        
        pdf(paste0("output/global_whittaker_diagram_with_AusTrait.pdf"), width=10, height=6)
        p1 <- whittaker_base_plot(color_palette = NULL)
        
        #plot(p1)
        
        p2 <- p1 + 
            geom_point(outDF, 
                       mapping=aes(x=MAT, y=MAP/10),
                       pch=19)+
            theme(legend.position = c(0.15, 0.65),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(gray(0.7)),
                  panel.border = element_rect(fill = NA))
        
        plot(p2)
        dev.off()
    }
    
    
    
    ### whether to delete the downloaded data or not
    if (remove.after.processing==T) {
        system(paste0("rm ", destDir, "/AusTrait.zip"))
        system(paste0("rm -r ", destDir, "/AusTrait"))
        system(paste0("rm -r ", destDir, "/__MACOSX"))
        
    }
    
    
    return(outDF)
    
}