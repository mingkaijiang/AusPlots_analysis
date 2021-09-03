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

### set cloudstor authentication for the first time
### First time user needs to use web interface for set-up
### 1. Log on to Cloudstor,
### 2. Go to Settings
### 3. Set-up App passwords
### 4. Copy and paste the App user name and password
### 5. Run the following line, with first.cloudstor set as T
### 6. Enter user name, and password when prompted
### 7. Now you can download and upload data to Cloudstor
first.cloudstor <- "F"
if (first.cloudstor == "T") {
    cloud_auth()
} else {
    print("no need to run cloud_auth")
}

##########################################################################
### prepare AWAP data
awapDF <- prepare_AWAP_climate()

##########################################################################
Australia_Whittaker_diagram(sourceDir="DAVE_data/Raw_data",
                            destDir="data/Raw_data",
                            awap=awapDF,
                            remove.after.processing=T)

##########################################################################
### Ausplot site projected to Whittaker diagram
Ausplot_on_Whittaker_diagram(awap = awapDF)

##########################################################################
### AusTrait dataset
austrait.site <- download_AusTrait_from_CloudStor(sourceDir="DAVE_data/Raw_data",
                                                  destDir="data/Raw_data",
                                                  awap=awapDF,
                                                  to.plot=T,
                                                  remove.after.processing=T)

##########################################################################
#### CSIRO Permanent rainforest plot North Queensland 
QLD.rainforest.site <- process_CSIRO_North_Queesland_rainforest_permanent_plots(sourceDir="DAVE_data/Raw_data",
                                                                                destDir="data/Raw_data",
                                                                                awap=awapDF,
                                                                                to.plot=T,
                                                                                remove.after.processing=T)


##########################################################################
#### AEKOS subset Australian vegetation frequency and cover 2017
### a messy dataset
aekos.site <- process_AEKOS_Subset_Australian_Plant_Frequency_and_Cover_2017(sourceDir="DAVE_data/Raw_data",
                                                                             destDir="data/Raw_data",
                                                                             awap=awapDF,
                                                                             to.plot=T,
                                                                             remove.after.processing=T)





##########################################################################
### merge all known datasets and make basic plots
All_data_merge_and_plot(sourceDir="DAVE_data/Raw_data",
                        destDir="data/Raw_data",
                        awap=awapDF,
                        austrait.site=austrait.site,
                        QLD.rainforest.site=QLD.rainforest.site,
                        aekos.site=aekos.site)



<<<<<<< HEAD

=======
>>>>>>> cdb27e7068132023ee4da67acb9891921a14d371
##########################################################################
prepare_forest_of_Australia_2013()




##########################################################################
#### Forest Monitoring network
process_forest_monitoring_network(sourceDir="data/Forest_Monitoring_Network/data/",
                                  outDir="output/Forest_Monitoring_Network/")





### end.
