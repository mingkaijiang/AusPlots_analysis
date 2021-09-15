Ausplot_on_Whittaker_diagram <- function (awap) {
    
    ##########################################################################
    ### read in Ausplot data
    my.ausplots.data <- try(get_ausplots(bounding_box = c(110, 155, -50, -10)))
    
    ausplot.sites <- my.ausplots.data$site.info
    
    site.name <- unique(ausplot.sites$site_location_name)
    site.table <- table(ausplot.sites$site_location_name)
    rep.sites <- site.table[site.table>1]
    rep.sites.names <- names(site.table[site.table>1])
    repDF <- data.frame(rep.sites)
    colnames(repDF) <- c("site_location_name", "visit_repeat")
    
    rep.site.data <- ausplot.sites[ausplot.sites$site_location_name%in%rep.sites.names,]
    
    plotDF <- ausplot.sites[!duplicated(ausplot.sites$site_location_name),]
    plotDF <- merge(plotDF, repDF, by=c("site_location_name"),
                    all.x=T)
    plotDF$visit_repeat <- ifelse(is.na(plotDF$visit_repeat), 1, plotDF$visit_repeat)
    plotDF$visit_repeat <- as.character(plotDF$visit_repeat)
    
    #sites.list <- unique(my.ausplots.data$veg.vouch$site_location_name)
    
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
    
    
    ### plotting script for Ausplot only
    ### australia polygon
    aus.poly <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
    
    
    ## AusPlots spatial coverage
    pdf(paste0("output/AusPlots_spatial_coverage.pdf"), width=10, height=6)
    map_ausplots(my.ausplots.data)
    dev.off()
    
    pdf(paste0("output/AusPlots_spatial_coverage_visit.pdf"), width=10, height=6)
    p1 <- ggplot(data=aus.poly)+
        geom_point(plotDF, 
                   mapping=aes(x=longitude, y=latitude, 
                               fill=visit_repeat),
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
        scale_fill_manual(name="Visit",
                          values=c("1" = "yellow", "2" = "orange", "3" = "red"),
                          labels=c("1" = "Single", "2" = "2", "3" = "3"))+
        ylim(-45, -10)
    
    plot(p1)
    dev.off()
    
    ### Whittaker global
    ### basic Whittaker diagram
    pdf(paste0("output/global_whittaker_diagram_with_Ausplot.pdf"), width=10, height=6)
    p1 <- whittaker_base_plot(color_palette = NULL)
    
    #plot(p1)
    
    p2 <- p1 + 
        geom_point(ausplot.sites, 
                   mapping=aes(x=MAT, y=MAP/10, col=visit_repeat),
                   pch=19)+
        scale_color_manual(name="Visit",
                           values=c("1" = "yellow", "2" = "black"),
                           labels=c("1" = "Single", "2" = "Multiple"))+
        #scale_size_manual(name="Visit",
        #                  values=c("1" = 1, "2" = 3),
        #                  labels=c("1" = "single", "2" = "multiple"))+
        theme(legend.position = c(0.15, 0.65),
              panel.background = element_blank(),
              panel.grid.major = element_line(gray(0.7)),
              panel.border = element_rect(fill = NA))
    
    plot(p2)
    dev.off()
    
  
}