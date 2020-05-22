# # ===================================================
# write a function that plots crimes 
#              incidence in Baltimore city
# Data: Baltimore crime data
# Source: https://data.baltimorecity.gov/

# clear everything
rm(list = ls()) 
t0 <- proc.time()
# libraries 
#   need to install.packages() these
#   let me know if installation does not work
library(maps)
library(maptools)

# download, unzip and read the shape file
url_zip <- 'https://dl.dropboxusercontent.com/s/chyvmlrkkk4jcgb/school_distr.zip'
if(!file.exists('school_distr.zip')) download.file(url_zip, 'school_distr.zip')     # download file as zip
unzip('school_distr.zip')   # unzip in the default folder
schdstr_shp <- readShapePoly('school.shp')  # read shape file
xlim <- schdstr_shp@bbox[1,]
ylim <- schdstr_shp@bbox[2,]

# example of how to use the shape file
#   if there are no error code reading the above, you can directly plot the map of Baltimore (lines within are school districts)
#   we'll be overlaying our plots of crime incidents on this map:
plot(schdstr_shp, axes = T)     # axes = T gives x and y axes


# ======= now let's follow instructions in the pdf file ======

# download and load the crime csv data
#   link is https://dl.dropboxusercontent.com/s/4hg5ffdds9n2nx3/baltimore_crime.csv
url <- 'https://dl.dropboxusercontent.com/s/4hg5ffdds9n2nx3/baltimore_crime.csv'
if (!file.exists('baltimore_crime.csv')) {     # check whether data exists in local folder (prevents downloading every time)
  download.file(url, 'baltimore_crime.csv')
}
df.raw <- read.csv('baltimore_crime.csv',stringsAsFactors = F)# load data

# transform dates and time variables depending on what you need
time_1<-paste(df.raw$CrimeDate,df.raw$CrimeTime,sep=" ")
time_2<-as.POSIXct(time_1,format="%m/%d/%Y %H:%M:%S")
time_2

year<-as.numeric(format(time_2,"%Y"))
month<-as.numeric(format(time_2,"%m"))
day<-as.numeric(format(time_2,"%d"))
time_0<-as.numeric(format(time_2,"%H"))+as.numeric(format(time_2,"%M"))/60

# split coordinates into longitude and latitude, both as numeric
# note: no for/while/repeat loop

sp <- function(x,n) { 
  ul <- unlist(strsplit(x,','))[n]
  gsub('[^0-9\\.\\-]','', ul)
}

latitude <- as.numeric(sapply(df.raw$Location1, function(x) sp(x,1) ))
longitude <- as.numeric(sapply(df.raw$Location1, function(x) sp(x,2) ))

df<-data.frame(df.raw$Location,df.raw$District,df.raw$CrimeDate,year,month,day,df.raw$CrimeTime,time_0,latitude,longitude,df.raw$Description)
names(df)<-c("Location","District","CrimeDate","year","month","day","CrimeTime","time","latitude","longitude","Description")

#summary geographic patterns of crimes
points(df$longitude[df$Description=="LARCENY"&df$year>=2013&df$year<=2015&df$month>=6&df$month<=8],
       df$latitude[df$Description=="LARCENY"&df$year>=2013&df$year<=2015&df$month>=6&df$month<=8],col=rgb(1,0,0,0.3),cex=.3)

# generate geographic and time patterns for crimes with keyword "ASSAULT"
# note: no copy and paste of the same/similar command many times
t1 <- proc.time()
t2 <- proc.time()
par(mfrow=c(2,2))
for(i in 0:3){
  plot(schdstr_shp, axes = T,main=paste("hour:",6*i,"-",6*i+6))
  points(df$longitude[grepl("ASSAULT", as.character(df$Description))&df$time>6*i&df$time<=6*i+6],
         df$latitude[grepl("ASSAULT", as.character(df$Description))&df$time>6*i&df$time<=6*i+6],
         col=rgb(1,0,0,0.05),cex=.1)
}


t3 <- proc.time()

t1 - t0
t3 - t2


