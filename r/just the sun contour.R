library(tidyverse)
library(magrittr)
library(ggvoronoi)
library(sf)
library(ggplot2)
library(ragg)
library(av)
library(reshape2)

# Sunshine data
dat <- readRDS("data/climatedata.rds") %>%
  select(long,lat,month,sunshine)

xl <- c(-20,26.7)
yl <- c(35,65)

ns = 400
xs <- seq(xl[1]-10,xl[2]+10,length.out=ns)
ys <- seq(yl[1]-10,yl[2]+10,length.out=ns)

hourbreaks <- c(0,2,4,5,6,8,10,14)
hbf <- paste0("(",hourbreaks[-length(hourbreaks)],", ",hourbreaks[-1],"]")
hbl <- paste0(hourbreaks[-length(hourbreaks)],"-",hourbreaks[-1])
bcl <- c(hsv(.8,.9,.3),hsv(.9,.8,.45),hsv(0,.7,.6),hsv(.05,.8,.7),hsv(.1,.9,.8),hsv(0.15,.7,.9),hsv(0.1,0,1))

dex <- 10
datf <- dat %>%
  filter(long>=xl[1]-dex,long<=xl[2]+dex,lat>yl[1]-dex,lat<yl[2]+dex)

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

dat_fixed <- datf %>%
  left_join(bind_rows(bsr,dsr)) %>%
  mutate(sunshine=ifelse(!is.na(sunshine2),sunshine2,sunshine)) %>%
  select(-sunshine2) %>%
  arrange(month,long,lat)

dm <- dat_fixed %>%
  filter(month==0) %>%
  select(long,lat)

# Ocean data
# https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-ocean/
wat <- st_read("C:/vm/data/water polygons/ne10m",stringsAsFactors=FALSE)

# Data source etc
labnotes <- data.frame(
  textx=c(3.35),
  texty=c(34.5),
  lab="Data source: CLIMWAT database of the Food and Agricultural Organization of the United Nations\nMap data made with Natural Earth")

# Clear any previous attempts
file.remove(list.files("europe2","png$",full.names=T))

jn <- 13

for (i in c(1:11,0)) {for (j in seq(0,1,length.out=jn)[-jn]) {
  # Contour value update
  sold <- filter(dat_fixed,month==(i-1)%%12)$sunshine
  snew <- filter(dat_fixed,month==i)$sunshine
  di <- dm
  di$sunshine <- (sold*(1-j)+snew*j)
  
  smoo <- loess(sunshine~long*lat,data=di,span=.05)
  zs <- expand.grid(long=xs,lat=ys) %>%
    predict(smoo,newdata=.)
  cd <- melt(zs,id.vars=c("long","lat"),value.name="Sunshine") %>%
    mutate(x=rep(xs,ns),y=rep(ys,each=ns)) %>%
    mutate(Sunshine=pmax(0.01,pmin(13.99,Sunshine)))

  # labels
  labdat <- data.frame(
    tj = c(.9,1-j,j),
    textx=c(3.35,-19,-19),
    texty=c(65,50+c(j*3,-3+j*3)),
    monlab=c("Sunshine Hours",month.abb[(i+c(-1,0))%%12+1])) %>%
    mutate(cl=grey(.5+.5*tj))
  
  agg_png(paste0("europe2/sunshine",formatC(c(12,1:11)[i+1],flag="0",width=2),"_",formatC(as.integer(j*100),flag="0",width=2),".png"),background=grey(.5),width=800,height=800)
    # Main plot
  gg <- wat %>%
    ggplot() +
    geom_contour_filled(data=cd,aes(x=x,y=y,z=Sunshine,fill=factor(..level..,levels=hbf,labels=hbl)),breaks=hourbreaks) +
    geom_sf(fill=grey(.5),colour=grey(.5)) +
    coord_sf(xlim=xl,ylim=yl) +
    theme_void() +
    theme(panel.background=element_rect(fill=grey(.5),color="white",size=0)) +
    theme(legend.position=c(.071,.225),legend.key.size=unit(1,'cm'),legend.title=element_text(size=16,colour=grey(.8)),legend.text=element_text(size=12,colour=grey(.8))) +
    geom_text(data=labdat,aes(x=textx,y=texty,label=monlab,fill=NULL),color=labdat$cl,size=16,show.legend=FALSE) +
    geom_text(data=labnotes,aes(x=textx,y=texty,label=lab,fill=NULL),colour=grey(.2),size=4,show.legend=FALSE,fontface="italic") +
    guides(color="none") +
    scale_fill_manual(values=bcl,drop=FALSE,na.value=bcl[1]) +
    labs(fill="Sunshine")
  print(gg)
  dev.off()
  print(paste0(i,": ",j))
  
  
}} # ij

vid_frames <- list.files("europe2","png$",full.names=T)
av_encode_video(vid_frames,"europe2/europe2sunshine.mp4",framerate=12)
av_encode_video(rep(vid_frames,3),"europe2/europe2sunshine3.mp4",framerate=12)

