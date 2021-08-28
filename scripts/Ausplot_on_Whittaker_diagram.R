Ausplot_on_Whittaker_diagram <- function (awap) {
    
    ### Steps:
    ### 1. Pass in AWAP MAT and MAP data across Australia
    ### 2. Get all Ausplot coordinates,
    ### 3. Extract site-specific AWAP MAT and MAP data
    ### 4. Make Whittaker diagram
    ### 5. Project Australia MAT and MAP point onto Whittaker diagram
    ### 6. Project Ausplot MAT and MAP point onto Whittaker diagram
    ### 7. Repeat for AusTraits - same as Falster et al. 2021.
    ### 8. Add ozflux sites
    ### 9. Add AEKOS sites
    ### 10. Add other network sites
    ### 11. Check spatial coverage gaps

    my.ausplots.data <- try(get_ausplots(bounding_box = c(110, 155, -50, -10)))
    
    ausplot.sites <- my.ausplots.data$site.info
    

    #### plotting script
    ## AusPlots
    pdf(paste0("output/AusPlots.pdf"), width=10, height=6)
    map_ausplots(my.ausplots.data)
    dev.off()
    
    
    ## Whittaker global
    pdf(paste0("output/whittaker_diagram.pdf"), width=10, height=6)
    whittaker_base_plot(color_palette = NULL)+
        theme(legend.position = c(0.15, 0.75),
              panel.background = element_blank(),
              panel.grid.major = element_line(gray(0.7)),
              panel.border = element_rect(fill = NA))
    dev.off()
    
}