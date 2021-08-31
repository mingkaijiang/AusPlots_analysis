process_AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017 <- function(sourceDir,
                                                                           destDir,
                                                                           awap,
                                                                           to.plot=T,
                                                                           remove.after.processing=T) {
    
    ### download the zip file first
    cloud_get(path = paste0(sourceDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017.zip"),
              dest = paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017.zip"),
              open_file = F)
    
    ### unzip the file
    system(paste0("unzip ", destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017.zip -d ", destDir))
    
    
    ### obtain site information
    siteDF1 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/site-details/site-details_1.csv"))
    siteDF1 <- siteDF1[-1,]
    
    siteDF2 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/site-details/site-details_2.csv"))
    siteDF2 <- siteDF2[-1,]
    
    siteDF <- rbind(siteDF1, siteDF2)
    
    
    ### rename
    colnames(siteDF) <- c("SiteID", "SiteVisitID", "CustodianSiteID", "VisitDate", "CustodianSurveyName",
                          "Lon", "Lat", "CoordinateReliability", "MethodName", "SiteComments")
    
    ### remove un-necessary columns
    siteDF$CustodianSurveyName <- NULL
    siteDF$MethodName <- NULL
    siteDF$SiteComments <- NULL
    
    ### convert lon lat to numeric
    siteDF$Lon <- as.numeric(siteDF$Lon)
    siteDF$Lat <- as.numeric(siteDF$Lat)
    
    ### create visit frequency of each coordinate
    #freqDF <- count(siteDF, c("Lon", "Lat"))
    freqDF <- count(siteDF, "SiteID")
    
    tmpDF <- siteDF[,c("SiteID", "Lon", "Lat")]
    tmpDF <- tmpDF[!duplicated(tmpDF$SiteID),]
    freqDF <- merge(freqDF, tmpDF, by="SiteID")
    
    
    ### create a new outDF
    outDF <- freqDF

    ### overlap with awap data to get MAT and MAP
    coordinates(freqDF) = ~Lon+Lat
    
    ### get awap raster
    matDF <- awap[,c("lon", "lat", "Tmn")]
    mapDF <- awap[,c("lon", "lat", "MAP")]
    
    mat.raster <- rasterFromXYZ(matDF)
    map.raster <- rasterFromXYZ(mapDF)
    
    outDF$MAT <- extract(mat.raster, freqDF)
    outDF$MAP <- extract(map.raster, freqDF)
    
    ### plotting script
    if (to.plot == T) {
        ### australia polygon
        aus.poly <- ne_countries(scale = "medium", 
                                 country = "Australia", 
                                 returnclass = "sf")
        
        
        ### plot map
        p1 <- ggplot(data=aus.poly)+
            geom_point(outDF, 
                       mapping=aes(x=Lon, y=Lat, 
                                   fill=as.character(freq)),
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
        
        pdf(paste0("output/AEKOS_2007_site.pdf"))
        plot(p1)
        dev.off()
        
        pdf(paste0("output/global_whittaker_diagram_with_AEKOS.pdf"), width=10, height=6)
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
    
    ### read individual plant data
    indDF1 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Individual-Plants/Individual-Plants_1.csv"))
    indDF2 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Individual-Plants/Individual-Plants_2.csv"))
    indDF3 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Individual-Plants/Individual-Plants_3.csv"))
    indDF4 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Individual-Plants/Individual-Plants_4.csv"))
    indDF5 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Individual-Plants/Individual-Plants_5.csv"))
    indDF6 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Individual-Plants/Individual-Plants_6.csv"))
    
    indDF1 <- indDF1[-1,]
    indDF2 <- indDF2[-1,]
    indDF3 <- indDF3[-1,]
    indDF4 <- indDF4[-1,]
    indDF5 <- indDF5[-1,]
    indDF6 <- indDF6[-1,]

    indDF <- rbind(indDF1, rbind(indDF2, rbind(indDF3, rbind(indDF4, rbind(indDF5, indDF6)))))
    
    ### rename columns
    colnames(indDF) <- c("IndividualPlantID", "SiteID", "SiteVisitID", "CustodianSiteID",
                         "CustodianSurveyName","VisitDate", "Lon", "Lat", "CoordinateReliability",
                         "PlotSamplingUnit","IndividualOrganismID", "TaxonName", "TaxonNameComments",
                         "TaxonIdentityMethod","TrasectObservationInterval", "TransectObservationIntervalUnits",
                         "Stratum", "StratumComment", "CrownClass", "CrownClassComment", "CanopyDiameterNorthSouthValue",
                         "CanopyDiameterNorthSouthUnits", "CanopyDiameterEastWestValue","CanopyDiameterEastWestUnits", 
                         "SkyinCrownCategory", "GirthValue", "GirthValueUnits", "HeightValue", "HeightValueUnits",
                         "DBHValue", "DBHUnits", "PointofMeasurementValue", "PointofMeasurementUnits",
                         "BoleHeightValue", "BoleHeightUnits", "BrokenStemHeightValue", "BrokenStemHeightUnit",
                         "LifeStageCategory", "LifeStageComment", "Dominance", "SpeciesDominanceRanking", "SpeciesDominanceRankComment",
                         "DominantGrowthForm", "DominantGrowthFormComment", "AliveDead", "CauseofDeath", "CauseofDeathComment",
                         "ThreatPressureName", "ThreatPressureSymptomCategory", "ThreatPressureSymptomCategoryComment",
                         "ThreatPressureSeverityCategory", "ThreatPressureSeverityCategoryComment", "IndividualOccurrence",
                         "IndividualOccurrenceComment", "Size", "SexCategory", "SexComment", "MethodName", "IndividualOrganismComment")
    
    ### drop individual files to save space
    rm(indDF1)
    rm(indDF2)
    rm(indDF3)
    rm(indDF4)
    rm(indDF5)
    rm(indDF6)
    
    ### remove all the comments
    indDF$CoordinateReliability <- NULL
    indDF$TaxonNameComments <- NULL
    indDF$TaxonIdentityMethod <- NULL
    indDF$StratumComment <- NULL
    indDF$CrownClassComment <- NULL
    indDF$LifeStageComment <- NULL
    indDF$SpeciesDominanceRankComment <- NULL
    indDF$DominantGrowthFormComment <- NULL
    indDF$CauseofDeathComment <- NULL
    indDF$ThreatPressureSeverityCategoryComment <- NULL
    indDF$ThreatPressureSymptomCategoryComment <- NULL
    indDF$IndividualOccurrenceComment <- NULL
    indDF$IndividualOrganismComment <- NULL
    indDF$SexComment <- NULL
    indDF$MethodName <- NULL
    indDF$SexCategory <- NULL
    indDF$IndividualOccurrence <- NULL
    indDF$Size <- NULL
    indDF$ThreatPressureSeverityCategory <- NULL
    indDF$ThreatPressureSymptomCategory <- NULL
    indDF$ThreatPressureName <- NULL
    indDF$CauseofDeath <- NULL
    indDF$AliveDead <- NULL
    indDF$DominantGrowthForm <- NULL
    indDF$SpeciesDominanceRanking <- NULL
    indDF$Dominance <- NULL
    indDF$LifeStageCategory <- NULL
    indDF$IndividualOrganismID <- NULL
    indDF$CrownClass <- NULL
    indDF$Stratum <- NULL
    indDF$SkyinCrownCategory <- NULL
    
    ### add unit information in the comment to remove unit columns
    ## convert all height into meter
    indDF$HeightValue <- as.numeric(indDF$HeightValue)
    indDF$HeightValue <- ifelse(indDF$HeightValueUnits == "metres", 
                                indDF$HeightValue, 
                                indDF$HeightValue  / 100)
    #indDF$HeightValueUnits <- NULL
    
    ## all DBH in unit of cm
    indDF$DBHUnits <- NULL
    
    ## all point of measurements in unit of meter
    indDF$PointofMeasurementUnits <- NULL
    
    ## girth value unit in cm
    indDF$GirthValueUnits <- NULL
    
    ## Bole height unit in meter
    indDF$BoleHeightUnits <- NULL
    
    ## Broken stem height unit in 
    indDF$BrokenStemHeightUnit <- NULL
    indDF$BrokenStemHeightValue <- NULL
    
    ### canopy diameter east west in cm
    indDF$CanopyDiameterEastWestUnits <- NULL
    indDF$CanopyDiameterNorthSouthUnits <- NULL
    
    ## transect interval unit in meter
    indDF$TransectObservationIntervalUnits <- NULL
    
    ## convert data into numeric
    indDF$Lon <- as.numeric(indDF$Lon)
    indDF$Lat <- as.numeric(indDF$Lat)
    indDF$TrasectObservationInterval <- as.numeric(indDF$TrasectObservationInterval)
    indDF$CanopyDiameterNorthSouthValue <- as.numeric(indDF$CanopyDiameterNorthSouthValue)
    indDF$CanopyDiameterEastWestValue <- as.numeric(indDF$CanopyDiameterEastWestValue)
    indDF$GirthValue <- as.numeric(indDF$GirthValue)
    indDF$DBHValue <- as.numeric(indDF$DBHValue)
    indDF$PointofMeasurementValue <- as.numeric(indDF$PointofMeasurementValue)
    indDF$BoleHeightValue <- as.numeric(indDF$BoleHeightValue)
    
    ## as date
    indDF$VisitDate <- as.Date(as.character(indDF$VisitDate), format = "%d-%m-%Y")

    ## dbh vs. height
    p1 <- ggplot() +
        geom_hex(indDF, mapping = aes(DBHValue, HeightValue), bins = 100)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab("DBH (cm)")+
        ylab("Height (m)")
    
    p2 <- ggplot() +
        geom_hex(indDF, mapping = aes(DBHValue, BoleHeightValue), bins = 100)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab("DBH (cm)")+
        ylab("Bole Height (m)")
    
    p3 <- ggplot() +
        geom_density(indDF, mapping = aes(DBHValue))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab("DBH (cm)")
    
    p4 <- ggplot() +
        geom_density(indDF, mapping = aes(HeightValue))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab("Height (m)")
    
    p5 <- ggplot() +
        geom_density(indDF, mapping = aes(CanopyDiameterNorthSouthValue))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab("Canopy Diameter North-South (cm)")
    
    
    p6 <- ggplot() +
        geom_density(indDF, mapping = aes(CanopyDiameterEastWestValue))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab("Canopy Diameter East-West (cm)")
    
    
    
    combined_plot <- plot_grid(p1, p2, p3, p4, p5, p6, 
                               ncol=2, align="vh", axis = "l")
    
    save_plot(paste0("output/AEKOS_2007_individual_DBH_height.pdf"),
              combined_plot, base_width=10, base_height = 12)
    
    
    ### calculate mean DBH and height for each site
    sumDF <- summaryBy(CanopyDiameterEastWestValue+CanopyDiameterNorthSouthValue+HeightValue+DBHValue+GirthValue+PointofMeasurementValue+BoleHeightValue~CustodianSiteID+VisitDate+Lon+Lat,
                       FUN=c(mean, sd), data=indDF, na.rm=T, keep.names=T)
    
    ### coun number of stems within each site
    freqDF <- count(indDF, c("CustodianSiteID", "VisitDate"))
    
    ### merge 
    mgDF <- merge(sumDF, freqDF, by=c("CustodianSiteID", "VisitDate"))
    
    ### plot self thinning
    p1 <- ggplot() +
        geom_point(mgDF, mapping = aes(DBHValue.mean, freq))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        xlab("Mean DBH (cm)")+
        ylab("Number of plants per site")
    
    pdf(paste0("output/AEKOS_2007_plot_site_DBH_stem_density.pdf"))
    plot(p1)
    dev.off()
    
    
    ### plot time series
    #test <- subset(mgDF, CustodianSiteID == "1")
    #
    #test <- subset(mgDF, CustodianSiteID %in% c(1:100))
    #
    #test <- subset(mgDF, HeightValue.mean >= 3 & HeightValue.mean <= 20)
    #
    #p1 <- ggplot() +
    #    geom_line(test, mapping = aes(VisitDate, HeightValue.mean, col=CustodianSiteID))+
    #    #geom_errorbar(mgDF, mapping = aes(x=VisitDate, ymin=HeightValue.mean-HeightValue.sd,
    #    #                                  ymax=HeightValue.mean+HeightValue.sd, col=CustodianSiteID))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=12), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=12),
    #          legend.text=element_text(size=10),
    #          legend.title=element_text(size=12),
    #          panel.grid.major=element_blank(),
    #          legend.position="none",
    #          legend.text.align=0)+
    #    ylab("Mean height (m)")+
    #    xlab("year")
    #
    #plot(p1)
    #
    #
    #### check individual data to see why soem plots are so high
    #test <- subset(indDF, CustodianSiteID == "1" & VisitDate == "1970-02-24")
    
    ### comments:
    ### currently, there is un-believable results on height (i.e. > 100 m),
    ### which may be a data quality issue. 
    ### so suspend results here. 
    
    
    ### read in population datasets
    popDF1 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_1.csv"))
    popDF2 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_2.csv"))
    popDF3 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_3.csv"))
    popDF4 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_4.csv"))
    popDF5 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_5.csv"))
    popDF6 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_6.csv"))
    popDF7 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_7.csv"))
    popDF8 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_8.csv"))
    popDF9 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_9.csv"))
    popDF10 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_10.csv"))
    popDF11 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_11.csv"))
    popDF12 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_12.csv"))
    popDF13 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_13.csv"))
    popDF14 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_14.csv"))
    popDF15 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_15.csv"))
    popDF16 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_16.csv"))
    popDF17 <- fread(paste0(destDir, "/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/Vegetation-Populations/Vegetation-Populations_17.csv"))
    
    ### remove comments
    popDF1 <- popDF1[-1,]
    popDF2 <- popDF2[-1,]
    popDF3 <- popDF3[-1,]
    popDF4 <- popDF4[-1,]
    popDF5 <- popDF5[-1,]
    popDF6 <- popDF6[-1,]
    popDF7 <- popDF7[-1,]
    popDF8 <- popDF8[-1,]
    popDF9 <- popDF9[-1,]
    popDF10 <- popDF10[-1,]
    popDF11 <- popDF11[-1,]
    popDF12 <- popDF12[-1,]
    popDF13 <- popDF13[-1,]
    popDF14 <- popDF14[-1,]
    popDF15 <- popDF15[-1,]
    popDF16 <- popDF16[-1,]
    popDF17 <- popDF17[-1,]

    ### combine
    popDF <- rbind(popDF1, 
                   rbind(popDF2, 
                         rbind(popDF3, 
                               rbind(popDF4, 
                                     rbind(popDF5, 
                                           rbind(popDF6, 
                                                 rbind(popDF7, 
                                                       rbind(popDF8,
                                                             rbind(popDF9, 
                                                                   rbind(popDF10, 
                                                                         rbind(popDF11, 
                                                                               rbind(popDF12, 
                                                                                     rbind(popDF13, 
                                                                                           rbind(popDF14, 
                                                                                                 rbind(popDF15,
                                                                                                       rbind(popDF16, popDF17))))))))))))))))
    
    
    ### delete individual files
    rm(popDF1)
    rm(popDF2)
    rm(popDF3)
    rm(popDF4)
    rm(popDF5)
    rm(popDF6)
    rm(popDF7)
    rm(popDF8)
    rm(popDF9)
    rm(popDF10)
    rm(popDF11)
    rm(popDF12)
    rm(popDF13)
    rm(popDF14)
    rm(popDF15)
    rm(popDF16)
    rm(popDF17)
 
    ### column names
    colnames(popDF) <- c("VegetationPopulationID", "SiteID", "SiteVisitID", "CustodianSiteID",
                         "CustodianSurveyName","VisitDate", "Lon", "Lat", "CoordinateReliability",
                         "PlotSamplingUnit","TaxonName", "TaxonNameComments",
                         "TaxonIdentityMethod","AbundanceValue", "AbundanceRange",
                         "AbundanceUnits", "AbundanceComment", "SamplingUnitSize", "SamplingUnitSizeUnits", "CanopyDiameterNorthSouthValue",
                         "CanopyDiameterNorthSouthUnits", "CanopyDiameterEastWestValue","CanopyDiameterEastWestUnits", 
                         "HeightValue", "HeightRange", "HeightUnits", "HeightCategory", "HeightComment",
                         "AverageHeightValue", "AverageHeightUnits", "LifeStageComment", "PostFireRegenerationStrategyCategory",
                         "DominatesComment", "DominanceRankComment", "MethodName", "VegetationPopulationComment")
    
    ### delete empty columns
    popDF$CoordinateReliability <- NULL
    popDF$TaxonNameComments <- NULL
    popDF$TaxonIdentityMethod <- NULL
    popDF$AbundanceComment <- NULL
    popDF$MethodName <- NULL
    popDF$VegetationPopulationComment <- NULL
    popDF$HeightCategory <- NULL
    popDF$HeightComment <- NULL
    
    ### add missing information
    popDF$SamplingUnitSizeUnits <- ifelse(popDF$SamplingUnitSizeUnits%in%c("square metres", "hectares"), 
                                          popDF$SamplingUnitSizeUnits, "unknown")
    
    
    
    popDF$HeightUnits <- ifelse(popDF$HeightUnits%in%c("centimetres", "metres"),
                                popDF$HeightUnits, "unknown")
    
    
    popDF$LifeStageComment <- ifelse(popDF$LifeStageComment%in%c("Adult", "Juvenile", 
                                                                 "Alive", "Dead"),
                                     popDF$LifeStageComment, "Unknown")
    
    popDF$PostFireRegenerationStrategyCategory <- ifelse(popDF$PostFireRegenerationStrategyCategory%in%c("Resprouter", "Seeder"),
                                                         popDF$PostFireRegenerationStrategyCategory, "Unknown")
    
    
    popDF$DominatesComment <- ifelse(popDF$DominatesComment=="Midstorey Vegetation",
                                     "MidstoreyVegetation", 
                                     ifelse(popDF$DominatesComment=="Groundstorey Vegetation", 
                                            "GroundstoreyVegetation", 
                                            ifelse(popDF$DominatesComment%in%c("Overstorey Vegetation",
                                                                               "overstorey: species that dominate the tallest stratum with a canopy cover OF greater than or equal to 5%"),
                                                   "OverstoreyVegetation", 
                                                   ifelse(popDF$DominatesComment=="understorey: species that occur in strata below the overstorey",
                                                          "UnderstoreyVegetation",
                                                          ifelse(popDF$DominatesComment == "emergent: species that emerges above the overstorey and occupies a stratum that has a canopy cover less than 5%",
                                                                 "EmergentVegetation", "Unknown")))))
    
    popDF$DominanceRankComment <- ifelse(popDF$DominanceRankComment=="Most dominant taxon",
                                         "MostDominantTaxon", 
                                         ifelse(popDF$DominanceRankComment%in%c("Second most dominant taxon",
                                                                                "2nd most dominant taxon"),
                                                "SecondMostDominantTaxon",
                                                ifelse(popDF$DominanceRankComment%in%c("Third most dominant taxon",
                                                                                       "3rd most dominant taxon"),
                                                       "ThirdMostDominantTaxon",
                                                       ifelse(popDF$DominanceRankComment=="1st major alien taxon",
                                                              "FirstMajorAlienTaxon",
                                                              ifelse(popDF$DominanceRankComment=="2nd major alien taxon",
                                                                     "SecondMajorAlienTaxon",
                                                                     ifelse(popDF$DominanceRankComment=="3rd major alien taxon",
                                                                            "ThirdMajorAlienTaxon",
                                                                            ifelse(popDF$DominanceRankComment=="4th major alien taxon",
                                                                                   "FourthMajorAlienTaxon", 
                                                                                   ifelse(popDF$DominanceRankComment=="5th major alien taxon",
                                                                                          "FifthMajorAlienTaxon", 
                                                                                          ifelse(popDF$DominanceRankComment=="Dominant species in emergent stratum",
                                                                                                 "DominantSpeciesEmergentStratum", "unknown")))))))))
    
    
    ### convert unit
    popDF$VisitDate <- as.Date(as.character(popDF$VisitDate), format = "%d-%m-%Y")
    
    popDF$Lon <- as.numeric(popDF$Lon)
    popDF$Lat <- as.numeric(popDF$Lat)
    
    popDF$AbundanceValue <- as.numeric(popDF$AbundanceValue)
    
    ## convert all into per hectare
    popDF$SamplingUnitSize <- as.numeric(popDF$SamplingUnitSize)
    popDF$SamplingUnitSize_ha <- ifelse(popDF$SamplingUnitSizeUnits == "square metres", 
                                        popDF$SamplingUnitSize / 10000.0,
                                        popDF$SamplingUnitSize)
    
    ## cm
    popDF$CanopyDiameterEastWestValue <- as.numeric(popDF$CanopyDiameterEastWestValue)
    popDF$CanopyDiameterNorthSouthValue <- as.numeric(popDF$CanopyDiameterNorthSouthValue)
    popDF$CanopyDiameterEastWestUnits <- NULL
    popDF$CanopyDiameterNorthSouthUnits <- NULL
    
    # height
    popDF$HeightValue <- as.numeric(popDF$HeightValue)
    popDF$HeightValue_m <- ifelse(popDF$HeightUnits == "centimetres", 
                                        popDF$HeightValue / 100.0,
                                        popDF$HeightValue)
    
    popDF$AverageHeightValue <- as.numeric(popDF$AverageHeightValue)
    popDF$AverageHeightUnits <- NULL
    
    ### add missing information

    ### now all data are cleaned and reprocessed, we can investigate.
    
    ### plot PDF of abundance (percent) and height (m), split into different groups
    subDF1 <- popDF[popDF$AbundanceUnits == "percent",]
    subDF2 <- popDF[popDF$AbundanceUnits == "individuals",]
    

    p1 <- ggplot(subDF1[subDF1$DominanceRankComment!="unknown",],
                 aes(AbundanceValue, col = DominanceRankComment)) +
        geom_density()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab("Abundance (%)")
    
    
    p2 <- ggplot() +
        geom_density(subDF2[subDF2$LifeStageComment!="Unkown",],
                     mapping = aes(AbundanceValue, col = LifeStageComment))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        xlab("Abundance (individual)")
    
    
    ### make summary statistics - split by abundance
    #sumDF1 <- summaryBy(AbundanceValue+CanopyDiameterNorthSouthValue+CanopyDiameterEastWestValue+HeightValue_m+AverageHeightValue~DominatesComment,
    #                    data=subDF1, FUN=c(mean, sd), na.rm=T, keep.names=T)
    #
    #
    #sumDF2 <- summaryBy(AbundanceValue+CanopyDiameterNorthSouthValue+CanopyDiameterEastWestValue+HeightValue_m+AverageHeightValue~DominanceRankComment,
    #                    data=subDF1, FUN=c(mean, sd), na.rm=T, keep.names=T)
    #
    #
    #sumDF3 <- summaryBy(AbundanceValue+CanopyDiameterNorthSouthValue+CanopyDiameterEastWestValue+HeightValue_m+AverageHeightValue~LifeStageComment,
    #                    data=subDF2, FUN=c(mean, sd), na.rm=T, keep.names=T)
    #
    
    p1 <- ggplot() +
        geom_boxplot(subDF1, mapping = aes(DominatesComment, AbundanceValue, col = DominatesComment))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Abundance (%)")+
        xlab("Dominance")
    
    
    p2 <- ggplot() +
        geom_boxplot(subDF1, mapping = aes(DominanceRankComment, AbundanceValue, col = DominanceRankComment))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Abundance (%)")+
        xlab("Dominance")
    
    
    p3 <- ggplot() +
        geom_boxplot(subDF2, mapping = aes(LifeStageComment, AbundanceValue, col = LifeStageComment))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Abundance (individual)")+
        xlab("Life cycle stage")
    
    #plot(p3)

    
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
