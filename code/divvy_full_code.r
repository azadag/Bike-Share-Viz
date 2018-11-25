
### Azad Amir-Ghassemi April 2013
### Exercise in programmatic mapping... uses start/stop station ride location data from
### Chicago divvy bikes to create pseudo best-ride routes using OSM
### creates a heat-map of routes split into regular subscribers / 
### one-off users.
### This is a full process file; data cleanup, mapping, routing, 
### routino linux program must to be configured along with OSM maps
### for city of choice.  alternative routing using leaflet possible
### mapping builds on Code supplied by james cheshire Feb 2012
###
### See output here : http://1drv.ms/1pSI6uK (large jpg) 
###

library(ggmap)
library(ggplot2)
library(proto)
library(dplyr)

setwd("C:\\Users\\Azad\\Downloads\\@Do\\divvy\\")

# data cleanup, uses dplyr for data munging

## https://www.divvybikes.com/assets/images/Divvy_Stations_Trips_2013.zip

stations <- read.csv("data\\Divvy_Stations_2013.csv")
trips <- read.csv("data\\Divvy_Trips_2013.csv")

trips_df <- tbl_df(trips) %>% select(from_station_id, to_station_id, usertype, gender, birthday, tripduration)

trips_df$routes <- as.factor(paste(trips_df$from_station_id,trips_df$to_station_id))

trips_df$from_station_id <- as.factor(trips_df$from_station_id)
trips_df$to_station_id <- as.factor(trips_df$to_station_id)

tripsRoute <- trips_df %>% group_by( routes) %>% summarise( 
                   count = n() ) %>%
                   #dist = mean(Distance, na.rm = TRUE), 
                   #delay = mean(ArrDelay, na.rm = TRUE))
                   filter( count > 25 )

#summary(table(tripsRoute2))  ## quick check of routes / histogram
#qplot(tripsRoute2$count,ylim=c(0,10))


rou <- reshape2::colsplit(as.character(tripsRoute$routes), pattern =' ', c("from_id","to_id"))

tripsRoute3 <- cbind(tripsRoute, rou)

stations2 <- tbl_df(stations) %>% select( id, latitude, longitude, online.date)

tripsRoute4 <- merge( tripsRoute3, stations2, by.x="from_id",by.y="id" )
tripsRoute4 <- merge( tripsRoute4, stations2, by.x="to_id",by.y="id" )
tripsRoute5 <- tbl_df( tripsRoute4) %>% select( to_id : longitude.y )
names(tripsRoute5) <- c("to_id","from_id","routes","count","lat_fr","long_fr","online.date","lat_to","lon_to")

write.csv(tripsRoute5, file="commonroutes.csv")

################################################
## redo but split by usertype

trips_df_user <- tbl_df( trips ) %>% select( from_station_id, to_station_id, usertype, gender, birthday, tripduration )
trips_df_user$routes <- as.factor( paste(trips_df_user$usertype, trips_df_user$from_station_id, trips_df_user$to_station_id) )

trips_df_user$from_station_id <- as.factor(trips_df_user$from_station_id)
trips_df_user$to_station_id <- as.factor(trips_df2$to_station_id)


tripsRoute <- trips_df_user %>% group_by(routes) %>% summarise( count = n() ) %>% filter( count > 25 )
rou <- reshape2::colsplit(as.character(trips_df_user$routes), pattern =' ', c("type","from_id","to_id"))

trips_df_user_r <- cbind( trips_df_user, rou )

stations2 <- tbl_df( stations ) %>% select( id, latitude, longitude, online.date )

#create all combinations of trip / stations with 2 merges
tripsRoute4 <- merge(trips_df_user_r, stations2, by.x="from_id",by.y="id")
tripsRoute4 <- merge(trips_df_user_r, stations2, by.x="to_id",by.y="id")
tripsRoute5 <- tbl_df(tripsRoute4) %>% select( to_id : longitude.y )
names(tripsRoute5) <- c("to_id","from_id","routes","count","type","lat_fr","long_fr","online.date","lat_to","lon_to")

## intermediary write to csv
write.csv( tripsRoute5, file="commonroutes_by_type.csv" )

### calls out to linux Routino program which uses OSM to determine best
### routing path
###
## make system call to routino-router note : 
##  To set up routino, you need to download the OSM map extract http://wiki.openstreetmap.org/wiki/Planet.osm#Regional_extract_sources 
## for your area, and convert the local database using the planetsplitter
## http://www.routino.org/documentation/usage.html
## 
## the router will emphasize bike routes and non-highway roads and output each
## trip into a separate gpx file.

# this function writes out out the combinations of origins and destinations 
routes <- tripsRoute5

for (i in 1: nrow(routes)) { ### length of routes file)
  num <- i
  lat_f <- routes[[6]][i]
  lon_f <- routes[[7]][i]
  lat_t <- routes[[9]][i]
  lon_t <- routes[[10]][i]


  router <- paste("routino-router --dir=data --transport=bicycle --shortest  --lon1=", lon_f, 
            " --lat1=", lat_f, " --lon2=", lon_t, " --lat2=", lat_t, " --output-text-all --output-gpx-track", sep = "")
  
  system( router, wait = TRUE)
  system( paste( " mv shortest-all.txt shortest-all_",i,".txt", sep=""), wait = TRUE )
  system( paste(" mv shortest-track.gpx shortest-track_",i,".gpx", sep=""), wait = TRUE )  
  
}    

#### Pulls back pull gpx back in...

library(mapproj)
library(rgdal)

##### 
#repeats 3x to make data sets a full set of riders, subscribed customers, and one-time users
# setwd("C:\\Users\\Azad\\Downloads\\@Do\\divvy\\data\\routes\\")
## set wd to folder where routino wrote gpx output to
setwd()

files <- dir(pattern = "\\.gpx")

# }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
reads <- lapply (files,  function(x) {readOGR(dsn=x, layer="track_points")})

do <- function(file) {
  out <- tryCatch(
      {
      message(paste( "'try' part ", file))      
      readOGR(file, layer="track_points",drop_unsupported_fields=T, 
        dropNULLGeometries=T, verbose = F)
      },
      error = function(cond){
        message(paste("something happened ",file))
        message(cond)
        #return(NULL)
      },
      warning = function(code) {
        message(paste("file caused a warning:", file))
        message("here's the original warning:")
        message(code)
        #
        #return(NULL)
      }
    
    )
  return(out)
}

y <- lapply(files,do)
x <- y[!sapply(y, is.null)]

a <- sapply(x,FUN=coordinates)
b <- reshape2::melt(a)
c <- reshape(b, idvar = c("L1","Var1"), timevar="Var2", direction = "wide")

c2 <- c %.% group_by(L1) %.% mutate( index2 = max(Var1))

c2$end1 <-  unlist(
  tapply(c2$value.coords.x1, c2$L1, function(x) c(x[-1],NA) )
	)
c2$end2 <-  unlist(
  tapply(c2$value.coords.x2, c2$L1, function(x) c(x[-1],NA) )
	)

c3 <- c2[complete.cases(c2), ]
c3$c <- as.factor(paste(c3$end1,c3$end2))

names(c3) <- c("idx","riderid","stlon","stlat","len","elon","elat","c")

c4 <- tbl_df(c3)
c4 <- group_by(c4, c)
c5 <- summarise(c4, count = n() )

c3a <- merge(c3, c5, by.x="c",by.y="c")
c3a <- arrange(c3a, riderid, idx)
c3a$riderid <- as.factor(c3a$riderid)

c3a -> TOTALRIDERS_DF

###############

files <- dir(pattern = "data\\routes\\subs\\subs\\.gpx")

y<- lapply(files,do)
x <- y[!sapply(y, is.null)]
a <- sapply(x,FUN=coordinates)
b <- reshape2::melt(a)
c <- reshape(b, idvar = c("L1","Var1"), timevar="Var2", direction = "wide")
c2 <- c %.% group_by(L1) %.% mutate( index2 = max(Var1))

c2$end1 <-  unlist(
  tapply(c2$value.coords.x1, c2$L1, function(x) c(x[-1],NA) )
)
c2$end2 <-  unlist(
  tapply(c2$value.coords.x2, c2$L1, function(x) c(x[-1],NA) )
)

c3 <- c2[complete.cases(c2), ]
c3$c <- as.factor(paste(c3$end1,c3$end2))
names(c3) <- c("idx","riderid","stlon","stlat","len","elon","elat","c")

c4 <- tbl_df(c3)
c4 <- group_by(c4, c)
c5 <- summarise(c4, count = n() )

c3a <- merge(c3, c5, by.x="c",by.y="c")
c3a <- arrange(c3a, riderid, idx)
c3a$riderid <- as.factor(c3a$riderid)

c3a -> SUBS_RIDERS_DF
write.csv(SUBS_RIDERS_DF, "subs.csv")

#########

files <- dir(pattern = "\\data\\routes\\cust\\cust\\.gpx")

reads <- lapply (files,  function(x) {readOGR(dsn=x, layer="track_points")})

y<- lapply(files,do)
x <- y[!sapply(y, is.null)]
a <- sapply(x,FUN=coordinates)
b <- reshape2::melt(a)
c <- reshape(b, idvar = c("L1","Var1"), timevar="Var2", direction = "wide")
c2 <- c %.% group_by(L1) %.% mutate( index2 = max(Var1))


c2$end1 <-  unlist(
  tapply(c2$value.coords.x1, c2$L1, function(x) c(x[-1],NA) )
	)
c2$end2 <-  unlist(
  tapply(c2$value.coords.x2, c2$L1, function(x) c(x[-1],NA) )
	)

c3 <- c2[complete.cases(c2), ]
c3$c <- as.factor(paste(c3$end1,c3$end2))
names(c3) <- c("idx","riderid","stlon","stlat","len","elon","elat","c")

c4 <- tbl_df(c3)
c4 <- group_by(c4, c)
c5 <- summarise(c4, count = n() )

c3a <- merge(c3, c5, by.x="c",by.y="c")
c3a <- arrange(c3a, riderid, idx)
c3a$riderid <- as.factor(c3a$riderid)

c3a -> CUST_RIDERS_DF

write.csv(CUST_RIDERS_DF, "cust.csv")


##########################
#### Simple  Heat Map ####
##########################


ylim1 <- range( c3a$stlon )
ylim1 <- range( c3a$stlat )


p <- ggplot()
p<- p + geom_segment(data = c3a, mapping = aes(x=stlon, y=stlat, xend= elon, yend = elat))
p + geom_point(data=c3a, mapping=aes(x=stlon, y=stlat), size=4, shape=21, fill="white")

plon_m <- ggplot(c3a, aes(x=elat, y=elong, xend = elong, yend = elat))

plon2 <- plon_m + geom_segment2(aes(xend=elong, yend=elat, size= count, colour=count))+scale_size(range=c(0.06, 1.8))+scale_colour_gradient(low="#FFFFFF", high="#FFFF33", space="rgb")+coord_equal(ratio=1/cos(c3a$elat[1]*pi/180)) + quiet + theme(panel.background=element_rect(fill="#404040")) ## took out quiet
plon2

###### Full heat mapping and output ####
#builds on Code supplied by james cheshire Feb 2012 to clean map elements
#load packages and enter development mode

library('devtools')
dev_mode()
library(ggplot2)
library(proto)
library(maps)
library(rgdal)
library(maptools)
gpclibPermit()

## create GeomSegment2 function that is now build into ggmaps. this version
## has more flexibility in terms of mapping
GeomSegment2 <- proto(ggplot2:::GeomSegment, {
  objname <- "geom_segment2"
  draw <- function(., data, scales, coordinates, arrow=NULL, ...) {
    if (is.linear(coordinates)) {
      return(with(coord_transform(coordinates, data, scales),
                  segmentsGrob(x, y, xend, yend, default.units="native",
                               gp = gpar(col=alpha(colour, alpha), lwd=size * .pt,
                                         lty=linetype, lineend = "round"),
                               arrow = arrow)
      ))
    }
  }}


geom_segment2 <- function(mapping = NULL, data = NULL, stat =
                            "identity", position = "identity", arrow = NULL, ...) {
  GeomSegment2$new(mapping = mapping, data = data, stat = stat,
                   position = position, arrow = arrow, ...)

#### Load external shapefile data
#### load parcel data. You need to fortify if loaded as a shapefile
# for chicago's parcel map this takes 4-6 hours.
built2 <- readShapePoly(".\\data\\layers\\built\\built.shp")
buil3 <- fortify(built2,region="FID")

#### read bike lanes, water files
bikeroutes <- fortify(readShapeLines(".\\data\\bikelanes\\lanes_clip.shp"))

water<- fortify(readShapePoly(".\\data\\layers\\hydro_clip.shp"))
# built<- fortify(readShapePoly("C:\\Users\\Azad\\Downloads\\@Do\\divvy\\data\\layers\\build_clip.shp"))


#####
pbuilt<-c(geom_polygon(data=buil3, aes(x=long, y=lat, group=group), colour= "#4B4B4B", fill="#4F4F4F", lwd=0.001))
pwater<-c(geom_polygon(data=water, aes(x=long, y=lat, group=group), colour= "#708090", fill="#708090"))
pbike <- c(geom_path(data=bikeroutes, aes(x=long, y=lat, group=group), colour= "#41ab5d", fill="#41ab5d", alpha =.5, lwd=.001))

#This step removes the axes labels etc when called in the plot.
xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

## test mapping
#ggplot()  + pwater + pbike+ quiet + 
#  theme(panel.background=element_rect(fill="#404040")) + theme(legend.position="none") +
#  coord_equal(ratio=.8/cos(c3a$elat[1]*pi/180)) ### approximates a decent coordinate system

###############

plon_m <- ggplot(c3a, aes(x=stlon, y=stlat))

plon2<- plon_m + pwater + pbuilt+ pbike + geom_segment2(aes(xend=elon, yend=elat, size= count, colour=count), alpha =.3 ) + 
  scale_size(range=c(0.06, .2))+scale_colour_gradient(low="#FFFFFF", high="#FFFF33", space="rgb") + quiet + 
  theme(panel.background=element_rect(fill="#404040")) + theme(legend.position="none") +
  coord_equal(ratio=.8/cos(c3a$elat[1]*pi/180))

ggsave("out\\plon2.pdf")


#######################
#### Full routes

plon_T <- ggplot(TOTALRIDERS_DF, aes(x=stlon, y=stlat))

plon3 <- plon_T + pwater + pbuilt+ pbike + geom_segment2(aes(xend=elon, yend=elat, size= count, colour=count), alpha =.3 ) + 
  scale_size(range=c(0.06, .2))+scale_colour_gradient(low="#FFFFFF", high="#FFFF33", space="rgb") + quiet + 
  theme(panel.background=element_rect(fill="#404040")) + theme(legend.position="none") +
  coord_equal(ratio=.8/cos(c3a$elat[1]*pi/180))

ggsave("out\\plonT.pdf")

########################
plon_C <- ggplot(CUST_RIDERS_DF, aes(x=stlon, y=stlat))

plon_3 <- plon_C + pwater + pbuilt+ pbike + geom_segment2(aes(xend=elon, yend=elat, size= count, colour=count), alpha =.3 ) + 
  scale_size(range=c(0.06, .2))+scale_colour_gradient(low="#FFFFFF", high="#FFFF33", space="rgb") + quiet + 
  theme(panel.background=element_rect(fill="#404040")) + theme(legend.position="none") +
  coord_equal(ratio=.8/cos(c3a$elat[1]*pi/180))

ggsave(plon_3, file="out\\plonC.pdf")

#########################
plon_S <- ggplot(SUBS_RIDERS_DF, aes(x=stlon, y=stlat))

plon_4 <- plon_S + pwater + pbuilt+ pbike + geom_segment2(aes(xend=elon, yend=elat, size= count, colour=count), alpha =.3 ) + 
  scale_size(range=c(0.06, .2))+scale_colour_gradient(low="#FFFFFF", high="#FFFF33", space="rgb") + quiet + 
  theme(panel.background=element_rect(fill="#404040")) + theme(legend.position="none") +
  coord_equal(ratio=.8/cos(c3a$elat[1]*pi/180))

ggsave(plon_4, file="out\\plonS.pdf")

#########3
plon_SC <- ggplot(total, aes(x=stlon, y=stlat))
plon_6 <- plon_SC + pwater + pbuilt+ pbike + geom_segment2(aes(xend=elon, yend=elat, size= count, colour=type, linetype= type), alpha =.2, data=subset(total)) + 
  scale_size(range=c(0.06, .5)) + scale_color_manual( values = c(names="type", "sub"= "#FFFF33", "cust" = "#2c7fb8")) +
  quiet + 
  theme(panel.background=element_rect(fill="#404040")) + theme(legend.position="none") +
  coord_equal(ratio=.8/cos(c3a$elat[1]*pi/180))

ggsave(plon_6, file="out\\plonSCa.pdf")
ggsave(plon_6, file="out\\plonSCa.svg")


#################   test
plon_SC <- ggplot(total, aes(x=stlon, y=stlat))
plon_6a <- plon_SC + pwater + pbuilt+ pbike + geom_segment2(aes(xend=elon, yend=elat, size= count, colour=type), alpha =.2, data=subset(total )) + 
#       geom_segment2(aes(xend=elon, yend=elat, size= count, colour=type), alpha =.2, data=subset(total, type="sub" )) +
       scale_size(range=c(0.06, .3)) + scale_color_manual( values = c(names="type", "sub"= "#FFFF33", "cust" = "#2c7fb8")) +
       quiet + geom_line(position=position_jitter(w=0.02, h=.02)) +
       theme(panel.background=element_rect(fill="#404040")) + theme(legend.position="none") +
       coord_equal(ratio=.8/cos(c3a$elat[1]*pi/180))
     
     ggsave(plon_6, file="out\\plonSCb.pdf")  

sc <- continuous_scale("type", "", palette = function(x) {ifelse(x = "sub", "#FFFF33", "#2c7fb8")})


plon_SC <- ggplot(total, aes(x=stlon, y=stlat))

plon_6_2 <- plon_SC + pwater + pbuilt+ pbike + geom_segment2(aes(xend=elon, yend=elat, size= count, color=factor(type)), alpha =.2, data=subset(total )) +           
           scale_size(range=c(0.06, .3)) +
           scale_color_gradientn(colours = topo.colors(10)) +
  quiet + 
  theme(panel.background=element_rect(fill="#404040")) + theme(legend.position="none") +
  coord_equal(ratio=.8/cos(c3a$elat[1]*pi/180))
                       
ggsave(plon_6_2, file="out\\plonSC_2.pdf")     