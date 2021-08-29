#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

### plot biomes
remotes::install_github("valentinitnelav/plotbiomes")

remotes::install_github("swish-climate-impact-assessment/awaptools")

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, 
               plyr,
               doBy, 
               lubridate,
               ggplot2,
               viridis,
               sciplot,
               scales,
               data.table,
               cowplot,
               gridExtra,
               ggthemes,
               RColorBrewer,
               gganimate,
               gifski,
               ausplotsR,
               plotbiomes,
               awaptools,
               RSAGA,
               cloudstoR,
               SpaDES)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z1 in sourcefiles)source(z1)


### End