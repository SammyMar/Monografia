library(sf)
library(maptools)
library(spatstat)
library(rgdal)

setwd("~/JORGE/VALENCIA/shape files ")

w_vlc <- readOGR("valencia_outline.shp")
w_vlc <- st_as_sf(w_vlc)
w_vlc <- st_transform(w_vlc, crs = 3857)
w_vlc <- st_sf(geom=w_vlc)
winn_vlc <- as.owin(as_Spatial(w_vlc))
 plot(winn_vlc)

net_vlc <- read_sf("valencia_road.shp")
net_vlc <- st_transform(net_vlc, crs = 3857)
net_vlc <- as.linnet.SpatialLines(as_Spatial(net_vlc))
Window(net_vlc) <- winn_vlc
net_vlc <- as.linnet(net_vlc, W=winn_vlc)
 plot(net_vlc,box=T)

 setwd("~/JORGE/VALENCIA ")

dados <- read.csv("crime-data-valencia.csv",header = TRUE, sep =",",dec = ",")
aux <- which(dados[[4]]=="Sustraccion")
dados=dados[aux,]

tempos <- paste(dados[[2]], dados[[3]], sep=" ")
tempos = strptime(tempos, format = "%m/%d/%Y %H:%M:%S")
aux = which(is.na(tempos) == "TRUE")


#dados=dados[-aux,]
#tempos = tempos[-aux]

aux = order(tempos)
tempos = tempos[aux]
dados = dados[aux,]

aux = c(7,8,9,10,11)
for(i in 1:length(aux))
    dados[,aux[1]] = as.factor(dados[,aux[1]])

T = length(tempos)
I = 10000

loc = dados[I:T,c(13,14,11,15:25)]
times = tempos[I:T]


covariates = function(times,loc,knn){
  
  COV = (matrix(0,length(times),24))
  for(i in (knn+1):length(times)){
    t_cov = difftime(times[i], times[(i-knn):(i-1)],units="hours")
    d_cov = sqrt((as.numeric(loc[i,1]) - as.numeric(loc[(i-knn):(i-1),1])) ^2 + 
                   (as.numeric(loc[i,2]) - as.numeric(loc[(i-knn):(i-1),2])) ^2)
    aux = which(d_cov < 0.015)
    d_cov = d_cov[aux]
    t_cov = t_cov[aux]
    # COV[i-50,] = c(summary(t_cov),summary(d_cov),t_cov[50:45],d_cov[50:45])
    COV[i,] = c(summary(as.numeric(t_cov)),summary(d_cov),length(d_cov),as.numeric(loc[i,4:14]))
  }
  return(COV)
}

library(e1071)

knn = 3000

COV = covariates(times[I:T],loc[I:T,],knn)



size = 10
pred = matrix(0,length(times)-(knn),size)

for(i in 1:size) {
  
  swap = sample(1:length(times),length(times))
  loc_sim = loc[swap,]
  COV_sim = covariates(times,loc_sim,knn)
  model <- svm(COV_sim[(knn+1):length(times),],type='one-classification',nu=0.01,kernel = "radial")
  pred[,i] = predict(model,COV[(knn+1):length(times),])
  cat(i)
  
}

xlim = as.numeric(range(loc[,1]))
ylim = as.numeric(range(loc[,2]))

alarm = as.numeric(which(p==F))

for(i in 2:length(p)){
  plot(loc[(knn+alarm[i]-15):(knn+alarm[i]),1:2],xlim=xlim,ylim=ylim,pch=19,cex=.8,col=p[(alarm[i]-15):alarm[i]]+1)
  Sys.sleep(3)
}









n = 5500

COV = covariates(times,loc,knn)

library(e1071)
n = sum(times<=1)
T = length(times)
model <- svm(COV[(knn+1):n,],type='one-classification',nu=0.01,kernel = "radial")

pred = predict(model,COV[(n+1):T,])

plot(loc[(n+1):(T),1],loc[(n+1):(T),2],pch=19,col = pred+2,cex=.2)

table(pred,cluster[(n+1):T])

##################################################################################
##################################### simulação cluster ##########################
##################################################################################




cp_vlc_year <- cp_vlc$year
cp_vlc_points <- SpatialPointsDataFrame(coords = data.frame(x=as.numeric(cp_vlc$crime_lon),y=as.numeric(cp_vlc$crime_lat)),data = data.frame(rep(0,nrow(cp_vlc))))
proj4string(cp_vlc_points) = CRS("+proj=longlat")
cp_vlc_points <- st_as_sfc(cp_vlc_points)
class(cp_vlc_points)
cp_vlc_points <- st_as_sf(cp_vlc_points)
cp_vlc_points <- st_transform(cp_vlc_points, crs = 3857)

X_valencia <- lpp(st_coordinates(cp_vlc_points)[cp_vlc_year==2020,],L=net_vlc)
X_valencia <- unique(X_valencia)


cp_vlc <- read.csv("crime-data-valencia.csv",header = TRUE, sep =",",dec = ",")
cp_vlc_year <- cp_vlc$year
cp_vlc_points <- SpatialPointsDataFrame(coords = data.frame(x=as.numeric(cp_vlc$crime_lon),y=as.numeric(cp_vlc$crime_lat)),data = data.frame(rep(0,nrow(cp_vlc))))
proj4string(cp_vlc_points) = CRS("+proj=longlat")
cp_vlc_points <- st_as_sfc(cp_vlc_points)
class(cp_vlc_points)
cp_vlc_points <- st_as_sf(cp_vlc_points)
cp_vlc_points <- st_transform(cp_vlc_points, crs = 3857)

X_valencia <- lpp(st_coordinates(cp_vlc_points)[cp_vlc_year==2020,],L=net_vlc)
X_valencia <- unique(X_valencia)

