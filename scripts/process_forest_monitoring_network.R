process_forest_monitoring_network <- function (sourceDir,
                                               outDir) {
    
    
    ### prepare outdir
    if(!dir.exists(outDir)) {
        dir.create(outDir, showWarnings = FALSE)
    }
    
    
    ### read input
    myDF1 <- fread(paste0(sourceDir, "data_large_tree_survey.csv"))
    
    
    # end
}
