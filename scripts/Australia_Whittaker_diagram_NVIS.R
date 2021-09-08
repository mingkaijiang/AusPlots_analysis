Australia_Whittaker_diagram_NVIS <- function () {
 
    
    dir.create("output/NVIS")
    dir.create("output/NVIS/cleaned")
    
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
    
    veg.list <- c(1:23, 
                  31, 32) 
    vegDF <- data.frame("vegID" = veg.list,
                        "vegName" = c("Rainforests & vine thickets",                            #1
                                      "Euc tall open forests",                                  #2
                                      "Euc open forests",                                       #3
                                      "Euc low open forests",                                   #4
                                      "Euc woodlands",                                          #5
                                      "Acacia f&w",                                             #6
                                      "Callitris f&w",                                          #7
                                      "Casuarina f&w",                                          #8
                                      "Melaleuca f&w",                                          #9
                                      "Other f&w",                                              #10
                                      "Eucalypt open woodlands",                                #11
                                      "Tropical eucalypt woodlands/grasslands",                 #12
                                      "Acacia open woodlands",                                  #13
                                      "Mallee woodlands & shrublands",                          #14 
                                      "Low closed forests and tall closed shrublands",          #15
                                      "Acacia shrublands",                                      #16
                                      "Other shrublands",                                       #17
                                      "Heathlands",                                             #18
                                      "Tussock grasslands",                                     #19 
                                      "Hummock grasslands",                                     #20 
                                      "Other grasslands, herblands, sedgelands & rushlands",    #21
                                      "Chenopod shrublands, samphire shrublands and forblands", #22
                                      "Mangroves",                                              #23
                                      "Other open woodlands",                                   #31
                                      "Mallee open woodlands & sparse mallee shrublands"))       #32
    
    

    ### plot
    for (i in veg.list) {
        
        ### read input
        inDF <- readRDS(paste0("output/NVIS/NVIS_v6_AWAP_veg", i, ".rds"))
        
        ### remove duplicated
        df_dups <- inDF[c("Lon", "Lat")]
        plotDF <- inDF[!duplicated(df_dups),]
        
        saveRDS(plotDF, paste0("output/NVIS/cleaned/NVIS_v6_AWAP_veg", 
                               i, "_cleaned.rds"))
        
    }
    
    
    #### read in all the input again and merge the dataframe
    plotDF <- c()
    
    for (i in veg.list) {
        
        tmpDF <- readRDS(paste0("output/NVIS/cleaned/NVIS_v6_AWAP_veg", 
                                i, "_cleaned.rds"))
        
        plotDF <- rbind(plotDF, tmpDF)
        
    }
        
    
    ### read in the Topt dataset and merge with the plot DF
    ### not a good predictor, because Topt depends linearly on Tmean,
    ### and TSM is a universal constant.
    #plotDF <- read_Topt_dataset_and_merge(inDF=plotDF)
    
    ### read in predictability scores
    plotDF <- read_predictability_and_merge(inDF=plotDF)
    
    ### make plotting
    pdf(paste0("output/NVIS/cleaned/vegetation_space_veg_individual.pdf"))
    
    for (i in veg.list) {
        p1 <- ggplot(plotDF[plotDF$w001000==i,], 
                     aes(x=Tmn, y=MAP) ) +
            #stat_density_2d(aes(fill = ..level..), 
            #                geom = "polygon", colour="white")+
            geom_density_2d_filled(alpha=0.5) + 
            geom_density_2d(size = 0.25, colour = "black")+
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
            #xlim(c(5, 35))+
            #ylim(c(0, 3000))+
            ggtitle(paste0(vegDF[vegDF$vegID==i, "vegName"]));p1
        
        plot(p1)

    }
    
    dev.off()
    
    
    ### all veg type together
    p2 <- ggplot(plotDF,
                 aes(x=Tmn, y=MAP) ) +
        geom_density_2d_filled() + 
        geom_density_2d(size = 0.25, colour = "white")+
        facet_wrap(vars(w001000))+
        xlim(c(5, 35))+
        ylim(c(0, 3000))+
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
              legend.box.just = 'left')
    
    pdf(paste0("output/NVIS/cleaned/vegetation_space_veg_all.pdf"))
    plot(p2)
    dev.off()
    
    #### more detailed cleaning and transforming
    
    ### remove type 21 because it is occupying too small a region
    veg.list.rev <- veg.list[!veg.list==21]
    vegDF.rev <- vegDF[!vegDF$vegID==21,]
    plotDF.rev <- plotDF[!plotDF$w001000==21,]
    
    plotDF.rev <- plotDF.rev[complete.cases(plotDF.rev$MAP),]
    plotDF.rev <- plotDF.rev[complete.cases(plotDF.rev$Tmn),]
    

    ### prepare two outDF to store the confidence interval values and the 
    ### calculated KDE values
    clDF <- c()
    kdeDF <- c()
    
    ### for kde calculation
    require(ks)
    
    ### loop through the revised list of vegetation groups
    for (i in veg.list.rev) {
        ### create a two column data frame for kde calculations
        DF <- data.frame("MAT" = plotDF.rev[plotDF.rev$w001000==i, "Tmn"],
                         "MAP" = plotDF.rev[plotDF.rev$w001000==i, "MAP"])
        #DF <- DF[order(-DF$MAP, -DF$MAT),]
        
        l <- nrow(DF)
        
        if(l <= 1000) {
            H <- Hpi(x=DF)      # optimal bandwidth estimation
            est<- kde(x=DF, H=H, compute.cont=TRUE)     # kernel density estimation
        } else {
            DF.sub <- DF[sample(nrow(DF), 1000),]
            # kernel density estimation
            H <- Hpi(x=DF.sub)      # optimal bandwidth estimation
            est<- kde(x=DF.sub, H=H, compute.cont=TRUE)     # kernel density estimation
        }
        
        # set contour probabilities for drawing contour levels
        cl<-contourLevels(est, prob=c(0.9, 0.8, 0.7, 0.6, 
                                      0.5, 0.25, 0.2, 0.1, 0.01), approx=TRUE)
        
        cl.out <- data.frame("VegID" = i, 
                             "CI10" = cl[1],
                             "CI20" = cl[2],
                             "CI30" = cl[3],
                             "CI40" = cl[4],
                             "CI50" = cl[5],
                             "CI75" = cl[6],
                             "CI80" = cl[7],
                             "CI90" = cl[8],
                             "CI99" = cl[9])
        
        clDF <- rbind(clDF, cl.out)
        
        #plot(est, cont=seq(1, 100, by=1), display="filled.contour2", add=FALSE, 
        #     ylab="MAP", xlab="MAT", #main=biome[i],
        #     ylim=c(0,4000), xlim=c(-10,40),las=1,
        #     cex.axis=2.0, cex.main = 3.5, cex.lab = 3) 
        #plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
        #plot(est,abs.cont=cl[2], labels=c(0.75),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
        #plot(est,abs.cont=cl[4], labels=c(0.9),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
        
        tmp <- data.frame("MAT" = rep(est$eval.points[[1]], 
                                      length(est$eval.points[[2]])),
                          "MAP" = rep(est$eval.points[[2]], 
                                      each = length(est$eval.points[[1]])),
                          "KDE" = as.vector(est$estimate),
                          "VegID" = i)
        
        kdeDF <- rbind(kdeDF, tmp)
        
        ### checking script
        #p2 <- ggplot(tmp[tmp$KDE>cl[2],],
        #             aes(x=MAT, y=MAP)) +
        #    geom_point(aes(fill=KDE, col=KDE)); p2 
        
    }
    
    
    
    ### now we obtain the CIs and the gridded KDE for each veg group
    ### we can convert to do some plotting
    plotDF <- c()
    
    for (i in veg.list.rev) {
        tmp <- kdeDF[kdeDF$VegID == i & kdeDF$KDE > clDF$CI90[clDF$VegID==i],]
        plotDF <- rbind(plotDF, tmp)
            
    }

    ### subset forests only
    plotDF1 <- plotDF[plotDF$VegID%in%c(1,     # Rainforests & vine thickets
                                        2,     # Euc tall open forests
                                        10,    # Other f&w
                                        11,    # Eucalypt open woodlands
                                        12,    # Tropical eucalypt woodlands/grasslands
                                        13),]  # Acacia open woodlands
    plotDF1$VegID <- as.character(plotDF1$VegID)
    
    #plotDF2 <- plotDF[plotDF$VegID%in%c(11:23),]
    #plotDF2$VegID <- as.character(plotDF2$VegID)
    
    col.list1 <- brewer.pal("Set3", n=6)
    #col.list2 <- brewer.pal("Set3", n=12)
    
    ### plot forests only
    p2 <- ggplot(data=plotDF1, aes(x=MAT, y=MAP)) +
        geom_point(aes(col=VegID), pch=19)+
        theme_linedraw() +
        #scale_color_manual(name="NVIS",
        #                   values = c(col.list1),
        #                   label = c(vegDF.rev$vegName[vegDF.rev$vegID%in%c(1,2,10,12)]))+
        xlim(c(5, 35))+
        ylim(c(0, 3000))+
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
              legend.box.just = 'left'); p2
    

    p3 <- ggplot(data=plotDF2, aes(x=MAT, y=MAP)) +
        geom_point(aes(col=VegID), pch=19)+
        theme_linedraw() +
        scale_color_manual(name="NVIS",
                           values = c(col.list2),
                           label = c(vegDF.rev$vegName[vegDF.rev$vegID%in%c(11:23)]))+
        xlim(c(5, 35))+
        ylim(c(0, 3000))+
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
              legend.box.just = 'left'); p3
    
    
    
    
    plotDF3 <- kdeDF[kdeDF$KDE > 0.0001,]
    plotDF3$VegID <- as.character(plotDF3$VegID)
    
    p4 <- ggplot(data=plotDF3, aes(x=MAT, y=MAP)) +
        geom_point(aes(col=VegID), pch=19)+
        theme_linedraw() +
        scale_color_manual(name="NVIS",
                           values = rainbow(n=length(vegDF.rev$vegName)),
                           label = c(vegDF.rev$vegName))+
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
              legend.box.just = 'left'); p4
    
    
    
    
    
    ### reclassify the vegetation groups
    #### read in all the input again and merge the dataframe
    plotDF <- c()
    
    for (i in veg.list) {
        
        tmpDF <- readRDS(paste0("output/NVIS/cleaned/NVIS_v6_AWAP_veg", 
                                i, "_cleaned.rds"))
        
        plotDF <- rbind(plotDF, tmpDF)
        
    }
    
    plotDF$veg2 <- ifelse()
    
    
    
    
}


