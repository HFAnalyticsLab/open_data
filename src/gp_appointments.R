#Read in all gp links
gp_links <- paste0('https://digital.nhs.uk/',GetLinks(gp_url,'statistical'))
gp_files <- GetLinks(gp_links,'Appointments_GP_Daily')

#read in data
rtt_data <- sapply(gp_files,
                   function(x){
                     UnzipCSV(x)
                   })
