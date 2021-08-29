#### Main script to process AusPlots data for DGVM analysis
#### Mingkai Jiang (m.jiang@westernsydney.edu.au)
####
####


##########################################################################
#### Prepare basic set-up
### clear wk space
rm(list=ls(all=TRUE))

### prepare
source("prepare.R")

##########################################################################
### prepare AWAP data
awapDF <- prepare_AWAP_climate()

prepare_NVIS_Australia_vegetation_classification(awap = awapDF,
                                                 destDir="output")

prepare_forest_of_Australia_2013()


##########################################################################
### Ausplot site projected to Whittaker diagram

Ausplot_on_Whittaker_diagram(awap = awapDF)


##########################################################################
#### CSIRO Permanent rainforest plot North Queensland 
process_CSIRO_North_Queesland_rainforest_permanent_plots(sourceDir="data/CSIRO_Permanent_rainforest_plot_North_Queensland/data/",
                                                         outDir="output/CSIRO_Permanent_rainforest_plot_North_Queensland/")



##########################################################################
#### AEKOS subset Australian vegetation frequency and cover 2017
#process_AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017(sourceDir="data/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/data/",
#                                                               outDir="output/AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017/")



##########################################################################
#### Forest Monitoring network
process_forest_monitoring_network(sourceDir="data/Forest_Monitoring_Network/data/",
                                  outDir="output/Forest_Monitoring_Network/")





### end.
