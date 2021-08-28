prepare_AWAP_climate <- function () {
    
    
    ### steps:
    ### 1. Calculate annual average T and total P for 30 yrs, for each grid
    ### 2. Merge the MAT and MAP data and save as rds
    
    ### calculate MAP and MAT, 30 yr averages
    #calculate_AWAP_MAP_for_Australia(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/rain/", 
    #                                 destDir = "output")
    
    #calculate_AWAP_MAT_for_Australia(sourceDir = "/Volumes/TOSHIBAEXT/AWAP/", 
    #                                 destDir = "output")

    
    ### read in MAT and MAP
    mapDF <- readRDS("output/Australia_30-yr_MAP.rds")
    matDF <- readRDS("output/Australia_30-yr_MAT.rds")
    
    
    outDF <- merge(mapDF, matDF, by=c("latID", "lonID", "lat", "lon"))
    
    return(outDF)
    
}