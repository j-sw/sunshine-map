library(tidyverse)
library(magrittr)

# https://www.fao.org/land-water/databases-and-software/climwat-for-cropwat/en/
# CLIMWAT database of the Food and Agricultural Organization of the United Nations

f_pen <- list.files("C:/vm/data/solar radiation/My_CLIMWAT_Files","pen$",full.names = TRUE)
f_cli <- list.files("C:/vm/data/solar radiation/My_CLIMWAT_Files","cli$",full.names = TRUE)
f_rds <- gsub("pen$","rds",f_pen)

sum(!file.exists(f_rds))

# (1:length(f_pen))[!file.exists(f_rds)]

for (i in (1:length(f_pen))[!file.exists(f_rds)]) {
  clim_i <- read_table(f_pen[i],skip=1,col_names=c("maxtemp","mintemp","humidity","wind","sunshine","radiation","eto"))
  meta_i <- read.table(f_pen[i],nrows=1,sep=",",col.names=c("loc","nom","elev","lat","unk1","long","unk2"))
  rain_i <- read_table(f_cli[i],skip=1,col_names=c("eto2","rain","effrain"))
  
  dat_i <- meta_i %>%
    select(elev,lat,long) %>%
    data.frame(month=0:11) %>%
    bind_cols(clim_i,rain_i) %>%
    select(-eto2)
  
  saveRDS(dat_i,f_rds[i])
  print(i)
}

fl <- list.files("C:/vm/data/solar radiation/My_CLIMWAT_Files","rds$",full.names = TRUE)
chartonum <- function(x) {
  gsub("([0-9])\\.([0-9]{3,3})","\\1\\2",x) %>%
    as.numeric
}

dat <- map(fl,~ readRDS(.x)) %>%
  map(~ mutate(.x,across(where(is.character),chartonum))) %>%
  bind_rows



saveRDS(dat,"data/climatedata.rds")
  



    