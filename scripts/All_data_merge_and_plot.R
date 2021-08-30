All_data_merge_and_plot <- function (awap) {
    
    ### Steps:
    ### 1. Pass in AWAP MAT and MAP data across Australia
    ### 2. Get all Ausplot coordinates,
    ### 3. Extract site-specific AWAP MAT and MAP data
    ### 4. Make Whittaker diagram
    ### 5. Project Australia MAT and MAP point onto Whittaker diagram
    ### 6. Project Ausplot MAT and MAP point onto Whittaker diagram
    ### 7. Repeat for AusTraits - same as Falster et al. 2021.
    ### 8. Add ozflux sites
    ### 9. Add AEKOS sites
    ### 10. Add other network sites
    ### 11. Check spatial coverage gaps

    ##########################################################################
    ### read in Ausplot data
    my.ausplots.data <- try(get_ausplots(bounding_box = c(110, 155, -50, -10)))
    
    ausplot.sites <- my.ausplots.data$site.info
    
    ### potential multiple site visits highlight
    ausplot.sites$visit_repeat <- ifelse(ausplot.sites$visit_start_date==ausplot.sites$visit_end_date, "1", "2")

    ### extract lon lat info
    ausplot.lonlat <- ausplot.sites[,c("longitude", "latitude", "site_unique")]
    
    coordinates(ausplot.lonlat) = ~longitude+latitude
    
    ### get awap raster
    matDF <- awap[,c("lon", "lat", "Tmn")]
    mapDF <- awap[,c("lon", "lat", "MAP")]
    
    mat.raster <- rasterFromXYZ(matDF)
    map.raster <- rasterFromXYZ(mapDF)
    
    ausplot.sites$MAT <- extract(mat.raster, ausplot.lonlat)
    ausplot.sites$MAP <- extract(map.raster, ausplot.lonlat)

    
    austrait.site <- download_AusTrait_from_CloudStor(sourceDir="DAVE_data/Raw_data",
                                                      destDir="data/Raw_data",
                                                      awap=awap,
                                                      to.plot=F,
                                                      remove.after.processing=T)
    
    
    ##########################################################################
    ### Merge all data of different source together
    ## prepare ausplot
    tmpDF1 <- ausplot.sites[,c("site_unique", "longitude", "latitude",
                               "MAT", "MAP")]
    tmpDF1$Dataset <- "AusPlot"
    names(tmpDF1)[names(tmpDF1)=="site_unique"] <- "DataID"
    
    ## prepare austrait
    tmpDF2 <- austrait.site[,c("dataset_id", "longitude", "latitude",
                               "MAT", "MAP")]
    tmpDF2$Dataset <- "AusTrait"
    names(tmpDF2)[names(tmpDF2)=="dataset_id"] <- "DataID"
    
    
    allDF <- rbind(tmpDF1, tmpDF2)
    
    
    ### plotting
    ### australia polygon
    aus.poly <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
    
    ### Spatial map
    pdf(paste0("output/All_data_spatial_coverage_visit.pdf"), width=10, height=6)
    p1 <- ggplot(data=aus.poly)+
        geom_point(allDF, 
                   mapping=aes(x=longitude, y=latitude, 
                               fill=Dataset),
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
        scale_fill_manual(name="Dataset",
                          values=c("AusPlot" = "red3", "AusTrait" = "yellow"),
                          labels=c("AusPlot" = "AusPlot", "AusTrait" = "AusTrait"))+
        ylim(-45, -10)+
        xlim(100,160)
    
    plot(p1)
    dev.off()
    
    ### Whittaker global
    ### basic Whittaker diagram
    pdf(paste0("output/global_whittaker_diagram_with_all_datasets.pdf"), width=10, height=6)
    p1 <- whittaker_base_plot(color_palette = NULL)
    
    p2 <- p1 + 
        geom_point(allDF, 
                   mapping=aes(x=MAT, y=MAP/10, col=Dataset),
                   pch=19)+
        scale_color_manual(name="Visit",
                           values=c("AusPlot" = "red3", "AusTrait" = "yellow"),
                           labels=c("AusPlot" = "AusPlot", "AusTrait" = "AusTrait"))+
        theme(legend.position = c(0.15, 0.65),
              panel.background = element_blank(),
              panel.grid.major = element_line(gray(0.7)),
              panel.border = element_rect(fill = NA))
    
    plot(p2)
    dev.off()
    

}