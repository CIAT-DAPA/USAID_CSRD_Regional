library(rjson)
library(dplyr)

catalog <- read.table("clipboard", header=T)
files <- paste0("http://104.239.241.61:8080/climex/?idst=",catalog[,1],"&yr=",1970:2018)

read_cimmyt <- function(files, outDir){

  json_data <- fromJSON(file=files)
  st_name <- substring(files, 41,nchar(files)-8)
  if(length(json_data)>0){
    data_all <- matrix(NA,length(json_data),7 ) %>% as.data.frame
    
    for(j in 1:7) data_all[,j] <- lapply(json_data, '[[', j) %>% unlist
    
    names(data_all) <- c("year", "month", "day", "tmax", "tmin", "prec", "hr")
    Date <- paste0(data_all[,1], sprintf("%02d", as.numeric(data_all[,2])),sprintf("%02d", as.numeric(data_all[,3])))
    
    for(i in 1:4){
      
      var <- names(data_all)[i+3]
      oDirVar <- paste0(outDir,"/",var,"-per-station")
      dir.create(oDirVar)
      data_f <- cbind.data.frame(Date, Value = as.numeric(data_all[,names(data_all)[i+3]]))
      write.table(data_f, paste0(oDirVar, "/", st_name, "_raw_", var, ".txt"), quote = F, row.names = F
                  , append = TRUE, col.names=!file.exists(paste0(oDirVar, st_name, "_raw_", var, ".txt")))
      
    }
    
  }
  cat(" - write whtst output file " , files, "\n")
}

outDir <- "S:/observed/weather_station/mex-smn/daily-raw"
lapply(1:length(files),function(k) read_cimmyt(files[k], outDir))
