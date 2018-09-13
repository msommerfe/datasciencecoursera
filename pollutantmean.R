
#setwd("C:/devsbb/Git/datasciencecoursera")
#source("pollutantmean.R")

pollutantmean<- function(directory, pollutant, id){
      data<-NULL
      selectedPollutantValues<-NULL
      allFilestoRead<- getFilesToReadFast(directory,id)

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

complete<- function(directory, id){
      allFilestoRead<- getFilesToReadFast(directory,id)
      columnId<-NULL
      columnNobs<- NULL
      
      for (file in allFilestoRead){
            values <<- read.csv(file, header = TRUE)
            if (is.null(columnId)){
                  Zeile <- read.csv(file,nrows = 1)
                  columnId<- rbind(Zeile[4])
                  columnNobs<- rbind(table(complete.cases(values))["TRUE"])

            }
            else{
                  Zeile <- read.csv(file,nrows = 1)
                  columnId<- rbind(columnId, Zeile[4]) 
                  columnNobs<- rbind(columnNobs,table(complete.cases(values))["TRUE"])
                  
            }
      }
      
      myComplete<-data.frame(columnId,columnNobs)
      colnames(myComplete)<-c("id","nobs")
      
      myComplete
}
      

getFilesToReadFast<-function(directory,ids){

      myIndex <- 0
      
      filesToRead <- vector(length = length(ids))
      vId         <- c(length = length(allFiles))
      vFile       <- c(length = length(allFiles))
      
      allFiles    <- list.files(directory, full.names = TRUE)
      
      for (file in allFiles){
            myIndex<- myIndex+1
            zeile <- read.csv(file,nrows = 1)
            vFile[myIndex]<- file
            vId[myIndex]<- as.integer(zeile[4])
      }

      mappingList <-list(vId,vFile)

      myIndex<-0
      for (i in ids){
            myIndex<-myIndex+1
            indexFile<- match(i,mappingList[[1]])
            filesToRead[myIndex]<-mappingList[[2]][indexFile]
            }

      filesToRead
      }


getFilesToRead<- function(directory, id){
      filesToRead <- c(length = length(id))
      allFiles <- list.files(directory, full.names = TRUE)
      fileIds  <- c(length = length(allFiles))
      fileNames <- c(length = length(allFiles))
      for (file in allFiles){
            Zeile <- read.csv(file,nrows = 1)
            fileIds<- c(fileIds, as.integer(Zeile[4]))

            fileNames<- c(fileNames, file)
      }
      
      for (i in id){
            index<- match(i,fileIds)
            print(index)
            print(fileNames[index])
            filesToRead<-c(filesToRead,fileNames[index])
      }

      #      print(paste("filestoRead",filesToRead))

      filesToRead
}


##################Wertvolle befehle###################
#print(file.path(directory,paste("00",as.character(id[i]),".csv",sep = "")))
# write.csv(x = allData, file = "test.csv")
