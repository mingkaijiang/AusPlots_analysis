download_AusTrait_from_CloudStor <- function(sourceDir,
                                             destDir,
                                             remove.after.processing=T) {
    
    ### download the zip file first
    cloud_get(path = paste0(sourceDir, "/AusTrait.zip"),
              dest = paste0(destDir, "/AusTrait.zip"),
              open_file = F)
    
    ### unzip the file
    system(paste0("unzip ", destDir, "/AusTrait.zip -d ", destDir))
    
    ### read the data
    myDF <- readRDS(paste0(destDir, "/AusTrait/austraits-3.0.2.rds"))
    
    ### get site information
    austrait.sites <- myDF$sites
    
    ### get site ID
    site.id <- unique()
    
    tmpDF <- reshape::cast(austrait.sites, dataset_id ~ site_property)
    siteDF <- tmpDF[,c("longitude (deg)", "latitude (deg)", "dataset_id")]
    
}