#Urls for datasets (1:3 means it only gets the past 3 years)
rtt_link <- GetLinks(rtt_url,'statistical-work-areas/rtt-waiting-times/rtt-data-')
rtt_files <- GetLinks(rtt_link,'Full-CSV-data')

#Read all the data (currently from 2016, if you want last 4 years change the GETLINKS URL)
rtt_data <- sapply(rtt_files,
                   function(x){
                     UnzipCSV(x)
                   })

#Apply function to all .zip links
rtt_data2 <- lapply(rtt_data,
                   function(x){
                     if(ncol(x) == 71){
                       names(x) <- as.character(x[3,])
                       x <- x[-(1:3),]
                       return(x)
                     } else {
                       return(x)
                     }
                   })
