process_CSIRO_North_Queesland_rainforest_permanent_plots <- function(sourceDir,
                                                                     outDir) {
    ### We present repeated stem measurement data from 20, 0.5 ha (100 m x 50 m) 
    ### permanent rainforest plots in northern Queensland, Australia from 1971 to 2013.  
    ### The plots have a rainfall range of 1200 to 3500 mm, 
    ### represent eleven vegetation types, six parent materials, 
    ### and range from 15 m to 1200 m above sea level. 
    ### Except for minor disturbances associated with selective logging on two plots, 
    ### the plots were established in old growth forest and all plots have thereafter been protected. 
    ### Plots were regularly censused and at each census 
    ### the diameter at breast height (DBH) of all stems ≥10 cm DBH were recorded.  
    ### Data is presented for 10998 individual stems with plot stem densities 
    ### at establishment ranging from 476 to 1104 stems ha-1.  
    ### Due to the wide geographical range of the plots, 
    ### no species dominate, although the families Lauraceae, 
    ### Rutaceae and Myrtaceae contribute a large number of species. 
    ### Basal area values at establishment ranged from 28.6 to 63.3 m2 ha-1 
    ### and showed no trend of increasing or decreasing over time 
    ### due mainly to regular disturbance and recovery from natural events such as cyclones.  
    ### In addition to stems ≥10 cm DBH data, we present height data, 
    ### floristic data from understory stems (≥50 cm height to <10 cm DBH), 
    ### an auxiliary species list (including vines, epiphytes, ferns, 
    ### grasses, herbs and other life forms), 
    ### and a list of voucher specimens lodged in herbaria. 
    ### The data collected from the 20 plots provides an insight into the floristics, 
    ### structure and long term forest dynamics of Australian tropical rainforests 
    ### and allows direct comparisons to be made with long-term monitoring plots at a global scale.
    
    ### prepare outdir
    if(!dir.exists(outDir)) {
        dir.create(outDir, showWarnings = FALSE)
    }
    
    ### read input
    myDF1 <- read.csv(paste0(sourceDir, "CSIRO_PermanentPlots_TreeMeasurementData.csv"))
    myDF2 <- read.csv(paste0(sourceDir, "CSIRO_PermanentPlots_AuxiliaryData.csv"))
    myDF3 <- read.csv(paste0(sourceDir, "CSIRO_PermanentPlots_UnderstoryData.csv"))
    myDF4 <- read.csv(paste0(sourceDir, "CSIRO_PermanentPlots_VoucherData.csv"))
    
    
    ### add information about site
    metaDF1 <- read.csv(paste0(sourceDir, "metadataTable1.csv"))
    metaDF2 <- read.csv(paste0(sourceDir, "metadataTable2.csv"))
    metaDF <- merge(metaDF1, metaDF2, by=c("epNumber", "PlotName"))
    
    myDF1 <- merge(myDF1, metaDF, by="epNumber")
    
    ### calculate number of stems per plot and per hectare
    stemDF <- count(myDF1, c("epNumber", "year"))
    names(stemDF)[3] <- "NumberStems_ha"
    stemDF$NumberStems_ha <- stemDF$NumberStems_ha * 2
    myDF1 <- merge(myDF1, stemDF, by=c("epNumber", "year"))

    
    ### create a dataset that contains tree mortality information
    tmpDF1 <- setDT(myDF1[myDF1$status=="Dead",])
    tmpDF1 <- tmpDF1[,c("epNumber", "stemNumber", "year")]
    names(tmpDF1)[3] <- "deathYear"
    tmpDF2 <- setDT(myDF1[myDF1$status=="Alive",])
    
    tmpDF3 <- merge(tmpDF1, tmpDF2, by=c("epNumber", "stemNumber"),
                    all.x=T)
    
    myDF5 <- tmpDF3 %>% 
        group_by(epNumber, stemNumber) %>% 
        filter(year==max(year))
    
    ### calculate mortality rate
    m.rate <- round(dim(tmpDF1)[1]/dim(tmpDF2)[1], 3) * 100
    
    ### 
    p1 <- ggplot(myDF5, aes(x=year, y = dbh_centimetres, color=establishmentHeight_metres)) +
        geom_point()+
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
        xlab("Last year of measurement before mortality")+
        ylab("DBH before mortality (cm)")+
        scale_color_viridis(name="Tree height at plot establishment (m)")+
        annotate("text", x=1980, y = 200, 
                 label = paste0("Mortality rate = ", m.rate, "%"))+
        geom_smooth(mapping=aes(x=year, y=dbh_centimetres), method="loess", span=0.3)
    
    
    p2 <- ggplot(myDF5, aes(x=MAT, y = dbh_centimetres, fill=as.factor(MAT))) +
        geom_boxplot()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        xlab("MAT")+
        ylab("DBH before mortality (cm)")
    
    p3 <- ggplot(myDF5, aes(x=MAP, y = dbh_centimetres, fill=as.factor(MAP))) +
        geom_boxplot()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        xlab("MAP")+
        ylab("DBH before mortality (cm)")
    
    
    p4 <- ggplot(myDF5, aes(x=Altitude_m_asl, y = dbh_centimetres, fill=as.factor(Altitude_m_asl))) +
        geom_boxplot()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        xlab("Altitude")+
        ylab("DBH before mortality (cm)")
    
    p5 <- ggplot(myDF5, aes(x=as.character(wetTropicsEndemic), y = dbh_centimetres, fill=as.character(wetTropicsEndemic))) +
        geom_boxplot()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_x_discrete("wet tropics endemic", labels = c("0" = "No", "1" = "yes"))+
        xlab("Altitude")+
        ylab("DBH before mortality (cm)")
    
    
    p6 <- ggplot(myDF5, aes(x=epNumber, y = dbh_centimetres, fill=epNumber)) +
        geom_boxplot()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        xlab("EP plot number")+
        ylab("DBH before mortality (cm)")
    
    
    combined_plot <- plot_grid(p1, p2, p3, p4, p5, p6,
                               ncol=2, align="vh", axis = "l")
    
    save_plot(paste0(outDir, "mortality_summary.pdf"),
              combined_plot, base_width=10, base_height = 12)
    
    
    
    ### plot density plot
    myDF5$Dataset <- "Mortality"
    myDF5$deathYear <- NULL
    myDF5 <- setDT(myDF5)
    tmpDF2$Dataset <- "Alive"
    myDF6 <- rbind(myDF5, tmpDF2)
    
    ### plot animation
    p2 <- ggplot() +
            geom_density(myDF6, mapping=aes(x=dbh_centimetres, col=Dataset))+
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
            xlab("Individual DBH (cm)")+
            transition_time(year)+
            labs(title = "Year: {frame_time}")+
            shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    ## save animation
    animate(p2, fps = 10, width = 750, height = 450,renderer = gifski_renderer())
    anim_save(paste0("animated_individual_DBH_mortality_alive.gif"), 
              animation=last_animation(), path=outDir)
    
    
    ##################################### plot self thinning ##################################### 
    sumDF <- summaryBy(dbh_centimetres+NumberStems_ha~epNumber+year, data=myDF1, FUN=c(mean, sd),
                       keep.names=T, na.rm=T)
    
    p1 <- ggplot() +
        geom_point(sumDF, mapping = aes(dbh_centimetres.mean, NumberStems_ha.mean, col=year))+
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
        xlab("Mean DBH (cm)")+
        ylab("Number of stems (ha-1)")+
        transition_time(year)+
        labs(title = "year: {frame_time}")+
        shadow_wake(wake_length = 0.1, alpha = FALSE)
    
    
    ## save animation
    animate(p1, fps = 10, width = 750, height = 450, renderer = gifski_renderer())
    anim_save("animated_thinning_with_year.gif", 
              animation=last_animation(), path=outDir)
    
    ### use height as a proxy for age
    p1 <- ggplot() +
        geom_point(sumDF, mapping = aes(dbh_centimetres.mean, NumberStems_ha.mean, col=year))+
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
        xlab("Mean DBH (cm)")+
        ylab("Number of stems (ha-1)")
    
    pdf(paste0(outDir, "plot_DBH_stem_density.pdf"))
    plot(p1)
    dev.off()
    
    
    ##################################### disturbance events ##################################### 
    ### select drought sites
    dDF <- myDF1[myDF1$epNumber%in%c("ep33", "ep40"),]
    
    ### plotting
    p1 <- ggplot() +
        geom_point(dDF, mapping = aes(year, NumberStems_ha, col=epNumber))+
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
        xlab("Year")+
        ylab("Number of stems (ha-1)")+
        scale_color_manual("site", 
                             breaks=c("ep33", "ep40"),
                            values = c("blue2", "red3"))+
        geom_vline(xintercept = 1995, lty = 1, col = "blue2")+
        geom_vline(xintercept = 2006, lty = 1, col = "red3")
    
    
    p2 <- ggplot() +
        geom_point(dDF, mapping = aes(year, dbh_centimetres, col=epNumber))+
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
        xlab("Year")+
        ylab("DBH (cm)")+
        scale_color_manual("site", 
                           breaks=c("ep33", "ep40"),
                           values = c("blue2", "red3"))+
        geom_vline(xintercept = 1995, lty = 1, col = "blue2")+
        geom_vline(xintercept = 2006, lty = 1, col = "red3")
    
    combined_plot <- plot_grid(p1, p2, 
                               ncol=1, align="vh", axis = "l")
    
    save_plot(paste0(outDir, "mortality_due_to_drought.pdf"),
              combined_plot, base_width=6, base_height = 8)

    
    
    # end
}