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
        annotate("text", x=1975, y = 200, 
                 label = paste0("Mortality rate = ", m.rate, "%"))+
        geom_smooth(mapping=aes(x=year, y=dbh_centimetres), method="loess", span=0.3)
    
    
    pdf(paste0(outDir, "DBH_at_mortality.pdf"))
    plot(p1)
    dev.off()
    
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
    
    
    
    # end
}