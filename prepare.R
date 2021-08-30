#### Create output folder
if(!dir.exists("data")) {
    dir.create("data", showWarnings = FALSE)
}


if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

sub.folder <- c("Raw_data", "Temporary", "Processed")

for (i in sub.folder) {
    if(!dir.exists(paste0("data/", i))) {
        dir.create(paste0("data/", i), showWarnings = FALSE)
    }
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
               SpaDES,
               rnaturalearth)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z1 in sourcefiles)source(z1)


### End