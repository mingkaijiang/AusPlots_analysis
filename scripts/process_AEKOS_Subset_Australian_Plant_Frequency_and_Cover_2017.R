process_AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017 <- function(sourceDir,
                                                                           outDir) {
    
    ### prepare outdir
    if(!dir.exists(outDir)) {
        dir.create(outDir, showWarnings = FALSE)
    }
    
    
    ### obtain site information
    siteDF1 <- fread(paste0(sourceDir, "site-details/site-details_1.csv"))
    siteDF1 <- siteDF1[-1,]
    
    siteDF2 <- fread(paste0(sourceDir, "site-details/site-details_2.csv"))
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
    freqDF <- count(siteDF, c("Lon", "Lat"))
    
    ### plot map
    p1 <- ggplot() + 
        geom_point(data=freqDF, aes(y=Lat, x=Lon, col=freq)) +
        coord_quickmap(xlim=range(freqDF$Lon), ylim=range(freqDF$Lat))+
        borders("world", col="grey", lwd=0.2) +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.box = 'none',
              legend.box.just = 'vertical',
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="white"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ggtitle("AEKOS site")
    
    pdf(paste0(outDir, "AEKOS_site.pdf"))
    plot(p1)
    dev.off()
    
    ### read individual plant data
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # end

}
