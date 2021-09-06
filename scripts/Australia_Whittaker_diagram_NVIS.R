Australia_Whittaker_diagram_NVIS <- function () {
 
    dir.create("output/NVIS")
    
    ### The vegetation group 
    #• MVG 1 - Rainforests and vine thickets
    #• MVG 2 - Eucalypt tall open forests
    #• MVG 3 - Eucalypt open forests
    #• MVG 4 - Eucalypt low open forests
    #• MVG 5 - Eucalypt woodlands
    #• MVG 6 - Acacia forests and woodlands
    #• MVG 7 - Callitris forests and woodlands
    #• MVG 8 - Casuarina forests and woodlands
    #• MVG 9 - Melaleuca forests and woodlands
    #• MVG 10 - Other forests and woodlands
    #• MVG 11 - Eucalypt open woodlands
    #• MVG 12 - Tropical eucalypt woodlands/grasslands
    #• MVG 13 - Acacia open woodlands
    #• MVG 14 - Mallee woodlands and shrublands
    #• MVG 15 - Low closed forests and tall closed shrublands
    #• MVG 16 - Acacia shrublands
    #• MVG 17 - Other shrublands
    #• MVG 18 - Heathlands
    #• MVG 19 - Tussock grasslands
    #• MVG 20 - Hummock grasslands
    #• MVG 21 - Other grasslands, herblands, sedgelands and rushlands
    #• MVG 22 - Chenopod shrublands, samphire shrublands and forblands
    #• MVG 23 - Mangroves
    #• MVG 24 – Inland aquatic: freshwater, salt lakes, lagoons
    #• MVG 25 - Cleared, non-native vegetation, buildings
    #• MVG 26 – Unclassified native vegetation
    #• MVG 27 – Naturally bare: sand, rock, claypan, mudflat
    #• MVG 28 – Sea and estuaries
    #• MVG 29 – Regrowth, modified native vegetation
    #• MVG 30 – Unclassified forest
    #• MVG 31 – Other open woodlands
    #• MVG 32 – Mallee open woodlands and sparse mallee shrublands 
    #• MVG 99 – Unknown/no data
    
    veg.list <- c(1:23, 31, 32) 
    
    pdf("output/NVIS/vegetation_space_first_look.pdf")
    for (i in veg.list) {
        inDF <- readRDS(paste0("output/NVIS/NVIS_v6_AWAP_veg", i, ".rds"))
        
        p1 <- ggplot(inDF, aes(x=Tmn, y=MAP) ) +
            stat_density_2d(aes(fill = ..level..), 
                            geom = "polygon", colour="white")+
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_text(size=12),
                  axis.title.x=element_text(size=14),
                  axis.text.y=element_text(size=12),
                  axis.title.y=element_text(size=14),
                  legend.text=element_text(size=12),
                  legend.title=element_text(size=14),
                  panel.grid.major=element_blank(),
                  legend.position="right",
                  legend.box = 'vertical',
                  legend.box.just = 'left')+
            ggtitle(paste0("NVIS veg ", i))
        
        plot(p1)
    
    }
    dev.off()
    
    
}


