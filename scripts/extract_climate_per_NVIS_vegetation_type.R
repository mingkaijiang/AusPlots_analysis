extract_climate_per_NVIS_vegetation_type <- function (n.lyr.x) {
    
    
    ### the vegetation group is labelled as 0 - 99
    
        ### loop through each region DF
    for (j in 2:99) {
        
        ### create empty outDF
        outDF <- c()
        
        for (i in 1:n.lyr.x) {

            ### region DF
            myDF <- readRDS(paste0("output/NVIS_v6_AWAP_df", i, ".rds"))
            
            ### extract by vegetation groups
            tmpDF <- subset(myDF, w001000==j)
            
            ### rbind
            outDF <- rbind(outDF, tmpDF)
        }
        
        ### save
        saveRDS(outDF, paste0("output/NVIS_v6_AWAP_veg", j, ".rds"))
    }
    
    
    
    
}
