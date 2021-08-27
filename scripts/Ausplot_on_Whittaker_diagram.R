Ausplot_on_Whittaker_diagram <- function () {
    
    ### Steps:
    ### 1. Get all Ausplot coordinates,
    ### 2. Calculate AWAP MAT and MAP data across Australia
    ### 3. Extract site-specific AWAP MAT and MAP data
    ### 4. Make Whittaker diagram
    ### 5. Project Australia MAT and MAP point onto Whittaker diagram
    ### 6. Project Ausplot MAT and MAP point onto Whittaker diagram
    ### 7. Repeat for AusTraits - same as Falster et al. 2021.
    ### 8. Check spatial coverage gaps
    
    
    
    
    
    #### plotting script
    pdf(paste0("output/whittaker_diagram.pdf"), width=10, height=6)
    whittaker_base_plot(color_palette = NULL)+
        theme(legend.position = c(0.15, 0.75),
              panel.background = element_blank(),
              panel.grid.major = element_line(gray(0.7)),
              panel.border = element_rect(fill = NA))
    dev.off()
    
}