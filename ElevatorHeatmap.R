library(jsonlite)
library(dplyr)
library(stringr)
library(httr)
library(leaflet)
library(leaflet.extras)

source('bd09.R')
bdtowgs=function(lnglat_char){
  spl=str_split_fixed(lnglat_char,',',2)
  bd_lng=as.numeric(spl[,1])
  bd_lat=as.numeric(spl[,2])
  lnglat=bd09togcj02(bd_lng,bd_lat)
  lnglat=gcj02towgs84(lnglat[1],lnglat[2])
  return(lnglat)
}

url='http://ghzyj.gz.gov.cn/sofpro/bmyyqt/gzlpc/gfgs/map/mapdata.jsp'
r=GET(url,query = list(searchText = "电梯",
                       gsdz = "",
                       opeClass= "",
                       pageSize= "1000",
                       ghgstype= "1", #1批后，2批前
                       currentPage= "1"))
json=fromJSON(rawToChar(r$content))
df=json$result
#df=df[1:100,]
cl=lapply(df$lnglat,bdtowgs)
data=as.data.frame(t(do.call(cbind,cl)))
colnames(data)=c('lng','lat')
m<-leaflet(data=data) %>% 
  setView(113.5,23.1,11) %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addHeatmap(~lng,~lat,radius = 5,intensity = 1)
m

