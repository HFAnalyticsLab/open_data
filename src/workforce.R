#read links
workforce_links <- GetLinks(workforce_url,'nhs-workforce-statistics')
workforce_links2 <- GetLinks(paste0('https://digital.nhs.uk/',workforce_links),'.zip')

#read data

#Read all csvs from urls; unz for zips
UnzipCSV <- function(files){
  #creates temp file to read in the data
  temp <- tempfile()
  download.file(files,temp,extdir=tempdir())
  #unlink the temp file, important to do
  unlink(temp)
  files_names}

#There are embedded nuls in the data. have to clean. pls fix nhs digital
workforce_data <- lapply(workforce_links2,
                         function(x){
                           UnzipCSV(x)
                         })
