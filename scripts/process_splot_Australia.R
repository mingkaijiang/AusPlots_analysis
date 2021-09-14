process_splot_Australia <- function(sourceDir,
                                    destDir,
                                    awap,
                                    to.plot=T,
                                    remove.after.processing=T) {
    
    ### download the zip file first
    cloud_get(path = paste0(sourceDir, "/sPlot_Australia.zip"),
              dest = paste0(destDir, "/sPlot_Australia.zip"),
              open_file = F)
    
    ### unzip the file
    system(paste0("unzip ", destDir, "/sPlot_Australia.zip -d ", destDir))
    
    
    ### obtain site information
    myDF1 <- fread(paste0(destDir, "/sPlot_Australia/AEKOSExtractionForSplot/aekosDownload/aekosDownload1.csv"))

    myDF2 <- fread(paste0(destDir, "/sPlot_Australia/AEKOSExtractionForSplot/aekosDownload/aekosDownload2.csv"))

    myDF3 <- fread(paste0(destDir, "/sPlot_Australia/AEKOSExtractionForSplot/aekosDownload/aekosDownload3.csv"))

    myDF4 <- fread(paste0(destDir, "/sPlot_Australia/AEKOSExtractionForSplot/aekosDownload/aekosDownload4.csv"))
    colnames(myDF4) <- colnames(myDF3)
    
    myDF <- rbind(rbind(myDF1, myDF2), rbind(myDF3, myDF4))
    sites <- unique(myDF$AEKOSLink)
    
    
    ### single site entry
    tmpDF <- myDF[,c("AEKOSLink", "Long", "Lat")]
    tmpDF <- tmpDF[!duplicated(tmpDF$AEKOSLink),]
    
    outDF <- tmpDF
    
    ### overlap with awap data to get MAT and MAP
    coordinates(tmpDF) = ~Long+Lat
    
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
        aus.poly <- ne_countries(scale = "medium", 
                                 country = "Australia", 
                                 returnclass = "sf")
        
        
        ### plot map
        p1 <- ggplot(data=aus.poly)+
            geom_point(outDF, 
                       mapping=aes(x=Long, y=Lat),
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
        
        pdf(paste0("output/sPlot_site.pdf"))
        plot(p1)
        dev.off()
        
        pdf(paste0("output/global_whittaker_diagram_with_splot.pdf"), 
            width=10, height=6)
        p1 <- whittaker_base_plot(color_palette = NULL)
        
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
    
    
    
    ########################
    ### read other datasets
    inDF1 <- fread(paste0(destDir, "/sPlot_Australia/AEKOSExtractionForSplot/r_site_r_speciesobservations.csv/r_sites.csv"))
    #inDF2 <- fread(paste0(destDir, "/sPlot_Australia/AEKOSExtractionForSplot/r_site_r_speciesobservations.csv/r_speciesobservations.csv"))
    
    
    ### single site entry
    tmpDF <- inDF1[,c("aekosid", "longitude", "latitude")]
    tmpDF <- tmpDF[!duplicated(tmpDF$aekosid),]
    
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
        aus.poly <- ne_countries(scale = "medium", 
                                 country = "Australia", 
                                 returnclass = "sf")
        
        
        ### plot map
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
        
        pdf(paste0("output/sPlot_site2.pdf"))
        plot(p1)
        dev.off()
        
        
    }
    
    
    
    
    
    
    
    ### This dataset is messy.
    ### We will need to focus on tracking a particular patch over time.
    ### But possible data quality issues include:
    ### inconsistent number of sample size (due to incomplete assessment of plant demography within each patch);
    ### incomplete match of data variables (a lot missing data, for different variables).
    ### We will need to figure out a way to process the data.
    
    ### whether to delete the downloaded data or not
    if (remove.after.processing==T) {
        system(paste0("rm ", destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017.zip"))
        system(paste0("rm -r ", destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017"))
        system(paste0("rm -r ", destDir, "/__MACOSX"))
        
    }
    
    return(outDF)
        
    # end

}
