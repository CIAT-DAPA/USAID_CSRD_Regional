# Created by: Lizeth Llanos
# This script make charts for NA data
# April 2017

library(raster)
library(grid)
library(ggplot2)
library(rgeos)
library(reshape)

# Define variable
 variable = "tmax"
 variable = "tmin"
 variable = "prec"


# Define input and ouput path
inDir = "S:/observed/weather_station/mex-smn/new_2018/daily_processed/"
outDir = paste0("S:/observed/weather_station/mex-smn/new_2018/daily_processed/quality_control/",variable,"/")


# Load data base with all raw stations catalog with lat and long
data_station.ini = read.csv(paste0(inDir,variable,"_daily_qc.csv"),header = T)

catalog = read.csv("S:/observed/weather_station/mex-smn/new_2018/catalog_tmin.csv",header = T)

# Define period from data
dates=seq(as.Date("1980/1/1"), as.Date("2017/12/31"), "days") 

data_station = data_station.ini[which(data_station.ini$year %in% as.numeric(unique(format(dates,"%Y")))),]

# Extract stations names
nomb = substring(names(data_station[,-1:-3]),2,nchar(names(data_station[,-1:-3])))
#nomb_s = do.call("rbind",strsplit(nomb,"_"))
name_st = nomb

# Summary function
summary_st = cbind(apply(data_station[,-1:-3],2,min,na.rm=T),apply(data_station[,-1:-3],2,max,na.rm=T),apply(data_station[,-1:-3],2,function(x) sum(is.na(x))/length(x)))
summary_st = as.data.frame(cbind(name_st, summary_st))
names(summary_st) = c("name_st","min","max","datos_faltantes")
#pos = which(as.character(summary_st$cod) %in% as.character(catalog$national_code))
summary_st$lat =catalog$LATITUD
  summary_st$long =catalog$LONGITUD

rownames(summary_st) = NULL
write.csv(summary_st,paste0(outDir,"summary_all_st_",variable,"_qc.csv"),row.names = F,quote = F)

summary_st$datos_faltantes = as.numeric(as.character(summary_st$datos_faltantes))*100

#########################
# Map missing values
#########################

map_na = function(summary_st,years,variable,outDir,shape_dir){
  if(variable=="prec"){
    variable_n = "Precipitación"
    low <- "cyan"; mid <- "green"; high <- "blue"
  }
  if(variable=="tmax"){
    variable_n = "Temperatura máxima"
    low <- "green"; mid <- "yellow"; high <- "red"
  }
  if(variable=="tmin"){
    variable_n = "Temperatura mínima"
    low <- "green"; mid <- "yellow"; high <- "red"
  }
  
  # honduras = shapefile(shape_dir) #Modificar ruta de la ubicación del shapefile
  # hnd=extent(-84.7,-89.2,12.9,16)
  # honduras=crop(honduras,hnd)
  honduras<-getData('GADM', country='MEX', level=1)
  name ="% NA" ; uplimit <- 0; dwlimit <- 60; step <- 5; uplimit_size <- 0; dwlimit_size <- 1; step_size <- 0.1; size = 1.2
  
  
  honduras@data$id <- rownames(honduras@data)
  #honduras@data$id_dpto <-rep(0,nrow(honduras@data))
  #honduras@data$id_dpto[c(4,9,11,13,14,15)] <- 1
  honduras2 <- fortify(honduras, region="id")
  #honduras2<- fortify(honduras, region="id_dpto")
  p <- ggplot(honduras2, aes(x=long,y=lat))
  p <- p + geom_polygon(aes(fill=hole,group=group),fill="grey 90")
  p <- p + scale_fill_manual(values=c("grey 90","grey 90"))
  p <- p + geom_path(aes(long,lat,group=group),color="black",size=0.3)
  
  p <- p + geom_point(data=summary_st, aes(x=long, y=lat,col=datos_faltantes),size=0.7)#+
    #geom_point(data=summary_st,aes(x=long, y=lat),shape = 1,size = 0.7,colour = "black") 
  
  p <- p +scale_color_gradient2(name=name, low = low, mid = mid, high = high,
                                limits=c(uplimit,dwlimit), guide="colourbar",
                                breaks=seq(uplimit,dwlimit,by=step), labels=paste(seq(uplimit,dwlimit,by=step)))
  
  p <- p + coord_equal()
  p <- p + theme(legend.key.height=unit(1.7,"cm"),legend.key.width=unit(1,"cm"),
                 legend.text=element_text(size=10),
                 panel.background=element_rect(fill="white",colour="black"),
                 axis.text=element_text(colour="black",size=10),
                 axis.title=element_text(colour="black",size=12,face="bold"))
  
  p <- p +labs(title = paste0("Distribución de datos faltantes para la variable de ",variable_n," en el período ",years), x = "Longitud", y = "Latitud") 
  
  tiff(paste(outDir,"map_faltantes_",variable,"_",years,".tif",sep=""), height=1500,width=2100,res=200,
       pointsize=1.5,compression="lzw")
  print(p)
  dev.off()
}

shape_dir = "Z:/Water_Planning_System/03_geodata/HND_adm/HND_adm1.shp"
years = paste0(min(unique(format(dates,"%Y"))),"-",max(unique(format(dates,"%Y"))))

map_na(summary_st=summary_st[summary_st$datos_faltantes<60,],years,variable,outDir,shape_dir)

