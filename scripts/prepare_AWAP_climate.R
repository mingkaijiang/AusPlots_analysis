prepare_AWAP_climate <- function () {
    
    
    ### steps:
    ### 1. Calculate annual average T and total P for 30 yrs, for each grid
    ### 2. Merge the MAT and MAP data and save as rds
    
    ### calculate MAP and MAT, 30 yr averages
    #calculate_AWAP_MAP_for_Australia(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
    #                                 destDir = "output")
    
    #calculate_AWAP_MAT_for_Australia(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/", 
    #                                 destDir = "output")
    
    if (file.exists("data/Processed/Australia_30-yr_MAP.rds")) {
        
        mapDF <- readRDS("data/Processed/Australia_30-yr_MAP.rds")
        matDF <- readRDS("data/Processed/Australia_30-yr_MAT.rds")
        
    } else {
        ### read in MAT and MAP
        mapDF <- cloud_get(path = "DAVE_data/Processed/Australia_30-yr_MAP.rds",
                           dest = "data/Processed/Australia_30-yr_MAP.rds",
                           open_file = T)
        
        matDF <- cloud_get(path = "DAVE_data/Processed/Australia_30-yr_MAT.rds",
                           dest = "data/Processed/Australia_30-yr_MAT.rds",
                           open_file = T)
        
    }
    
    outDF <- merge(mapDF, matDF, by=c("latID", "lonID", "lat", "lon"))
    
    return(outDF)
    
}