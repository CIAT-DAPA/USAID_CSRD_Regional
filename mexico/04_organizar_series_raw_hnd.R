#######################################################
## Organize data frame with all stations in a specific time period
## Created by: Lizeth Llanos
## March 2017
#######################################################


###################################
#---------Daily data--------------#
###################################
organize_data = function(inDir,outDir,rutCat,variable,time_period){
 
  rutOrigen = paste0(inDir,variable,"-per-station/")
    
  files <-list.files(rutOrigen,pattern="\\.txt$")
  nom.files<-substring(files,1,nchar(files)-13)
  
  idstation = read.csv(rutCat,header=T) #Cargar base con código y nombre de la estación
  names(idstation) = tolower(names(idstation))
  #idstation = idstation[which(idstation$variable==variable),]
 
   
  cod=as.character(idstation$estacion)
  # not_f = which(is.na(cod))
  # cod = cod[-not_f]
  
  where <- match(cod,nom.files)
  station_find=nom.files[where[which(!is.na(where))]]
  station_find_n=paste0(cod[which(!is.na(where))],"_",idstation[which(!is.na(where)),"nombre"])
  
   #Definir periodo que se desea analizar
  
  fechas=format(time_period,"%Y%m%d")
  fechas=cbind.data.frame("Date"=fechas,"NA")
  
  cat("Leyendo datos para cada estación de ",variable,"... \n")
  Datos <- lapply(paste(rutOrigen,cod[which(!is.na(where))],"_raw_",variable,".txt",sep=""),function(x){read.table(x,header=T,sep="\t")})
  
  datos_to=as.data.frame(matrix(NA,nrow(fechas),length(Datos)))
  
  
  cat("Organizando todos los datos en un solo archivo... \n")
  for(j in 1:length(Datos)) {  
    
    old=na.omit(Datos[[j]])
    if(nrow(old)!=0){
      combnew=old[!duplicated(old[,1]),]
    }else{
      combnew = old
    }
    
    
    final=merge(fechas,combnew,by="Date",all.x=T)
    #if(nrow(final)==nrow(datosprecip)){
    datos_to[,j]=final[,3]
    #}
    
  }
  
  year=as.numeric(substr(fechas[,1],1,4))
  month=as.numeric(substr(fechas[,1],5,6))
  day=as.numeric(substr(fechas[,1],7,8))
  
  datos_fin=cbind(day,month,year,datos_to)
  
  names(datos_fin)=c("day","month","year",as.character(station_find_n))
  
  cat("Escribiendo datos en .csv... \n")
  #Se guardan los archivos en formato .csv con la info organizada
  write.csv(datos_fin,paste(outDir,"/daily_processed/",variable,"_daily_raw.csv",sep=""),row.names=F)
  cat("Proceso finalizado! \n")
}


##Run para datos
variable = c("evap","tmax","tmin")
#variable = "prec"

inDir = "S:/observed/weather_station/mex-smn/new_2018/daily-raw/"
outDir ="S:/observed/weather_station/mex-smn/new_2018" 
rutCat = "S:/observed/weather_station/mex-smn/new_2018/daily-raw/catalog_daily.csv"

dir.create(paste0(outDir,"/daily_processed"),showWarnings = F)

time_period=seq(as.Date("1980/1/1"), as.Date("2017/12/31"), "days")


for(j in 1:length(variable)) organize_data(inDir,outDir,rutCat,variable[j],time_period)

