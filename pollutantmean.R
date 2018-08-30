pollutantmean<- function(directory, pollutant, id){
      data<-NULL
      selectedPollutantValues<-NULL
      allFilestoRead<- getFilesToRead(directory,id)

            for (file in allFilestoRead){
            values <<- read.csv(file, header = TRUE)
            
            if (is.null(data))
                  data<- values
            else
                  data<- rbind(data, values) 
      }
      

      if (pollutant %in% c("sulfate","Sulfate", "SULFATE"))
            selectedPollutantValues<-as.numeric(data$sulfate)
      if (pollutant %in% c("nitrate","Nitrate", "NITRATE"))
            selectedPollutantValues<-as.numeric(data$nitrate)
      
      myMean<-mean(selectedPollutantValues, na.rm = TRUE)
      myMean
}


getFilesToRead<- function(directory, id){
      filesToRead<-NULL
      allFiles<- list.files(directory, full.names = TRUE)
      for (file in allFiles){
            Zeile <- read.csv(file,nrows = 1)
            if (Zeile[4] %in% id){
                  if (is.null(filesToRead))
                        filesToRead<- file
                  else{
                        filesToRead<- c(filesToRead, file)
                  }
            }
      }
      filesToRead
}


##################Wertvolle befehle###################
#print(file.path(directory,paste("00",as.character(id[i]),".csv",sep = "")))
# write.csv(x = allData, file = "test.csv")
