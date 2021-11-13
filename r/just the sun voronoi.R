library(tidyverse)
library(magrittr)
library(ggvoronoi)
library(sf)
library(ggplot2)
library(ragg)
library(av)

# Sunshine data
dat <- readRDS("data/climatedata.rds") %>%
  select(long,lat,month,sunshine)

xl <- c(-20,26.7)
yl <- c(35,65)

datf <- dat %>%
  filter(long>=xl[1]-10,long<=xl[2]+10,lat>yl[1]-10,lat<yl[2]+10)

# Bright spots
bs <- datf %>%
  filter(month==10,sunshine>6,lat>50)

bsr <- datf %>%
  filter(month%in%c(9,11)) %>%
  right_join(bs %>% select(long,lat)) %>%
  group_by(long,lat) %>%
  summarise(sunshine2=mean(sunshine)) %>%
  right_join(bs %>% select(-sunshine))

# Dark spot
ds <- datf %>%
  filter(month%in%4:6,sunshine==0)

dsr <- datf %>%
  filter(month%in%c(3,7)) %>%
  right_join(ds %>% select(long,lat)) %>%
  group_by(long,lat) %>%
  summarise(sunshine2=mean(sunshine)) %>%
  right_join(ds %>% select(-sunshine))

dat_fixed <- dat %>%
  left_join(bind_rows(bsr,dsr)) %>%
  mutate(sunshine=ifelse(!is.na(sunshine2),sunshine2,sunshine)) %>%
  select(-sunshine2)

# Ocean data
# wat <- st_read("C:/vm/data/water polygons/ne10m",stringsAsFactors=FALSE)


# Voronoi polygon pre-calc
vf <- dat_fixed %>%
  filter(long>=xl[1]-10,long<=xl[2]+10,lat>yl[1]-10,lat<yl[2]+10) %>%
  arrange(month,long,lat)
vc <- vf %>%
  filter(month==0) %>%
  select(-month) %>%
  mutate(sunshine=1:length(sunshine))
vp <- voronoi_polygon(vc,x="long",y="lat") %>% fortify_voronoi

# Data source etc
labnotes <- data.frame(
  textx=c(3.35),
  texty=c(34.5),
  lab="Data source: CLIMWAT database of the Food and Agricultural Organization of the United Nations\nMap data made with Natural Earth")

# Clear any previous attempts
file.remove(list.files("europe","png$",full.names=T))

jn <- 13

for (i in 0:11) {for (j in seq(0,1,length.out=jn)[-jn]) {
  # Voronoi value update
  sold <- filter(vf,month==(i-1)%%12)$sunshine
  snew <- filter(vf,month==i)$sunshine
  vi <- vp
  vi$sunshine <- (sold*(1-j)+snew*j)[vi$sunshine]
  
  # labels
  labdat <- data.frame(
    tj = c(.9,1-j,j),
    textx=c(3.35,-19,-19),
    texty=c(65,50+c(j*3,-3+j*3)),
    monlab=c("Sunshine Hours",month.abb[(i+c(-1,0))%%12+1])) %>%
    mutate(cl=grey(.5+.5*tj))
  
  agg_png(paste0("europe/sunshine",formatC(c(12,1:11)[i+1],flag="0",width=2),"_",formatC(as.integer(j*100),flag="0",width=2),".png"),background=grey(.5),width=800,height=800)
    # Main plot
  gg <- wat %>%
    ggplot() +
    geom_polygon(data=vi,aes(x=x,y=y,group=id,fill=sunshine,color=sunshine)) +
    geom_sf(fill=grey(.5),colour=grey(.5)) +
    coord_sf(xlim=xl,ylim=yl) +
    theme_void() +
    theme(panel.background=element_rect(fill=grey(.5),color="white",size=0)) +
    theme(legend.position=c(.071,.225),legend.key.size=unit(1,'cm'),legend.title=element_text(size=16,colour=grey(.8)),legend.text=element_text(size=12,colour=grey(.8))) +
    scale_fill_gradientn(colours=c(hsv(.8,.9,.3),hsv(0,.7,.6),hsv(.1,.9,.8),hsv(0.15,.7,.9),hsv(0.1,0,1)),limits=c(0,14)) +
    scale_color_gradientn(colours=c(hsv(.8,.9,.3),hsv(0,.7,.6),hsv(.1,.9,.8),hsv(0.15,.7,.9),hsv(0.1,0,1)),limits=c(0,14)) +
    # scale_color_gradient(low=grey(.5),high=grey(1),limits=c(0,1)) +
    geom_text(data=labdat,aes(x=textx,y=texty,label=monlab,fill=NULL),color=labdat$cl,size=16,show.legend=FALSE) +
    geom_text(data=labnotes,aes(x=textx,y=texty,label=lab,fill=NULL),colour=grey(.2),size=4,show.legend=FALSE,fontface="italic") +
    guides(color="none") +
    labs(fill="Sunshine")
  print(gg)
  dev.off()
  print(paste0(i,": ",j))
  
  
}} # ij

vid_frames <- list.files("europe","png$",full.names=T)
av_encode_video(vid_frames,"europe/europesunshine.mp4",framerate=12)
av_encode_video(rep(vid_frames,3),"europe/europesunshine3.mp4",framerate=12)

