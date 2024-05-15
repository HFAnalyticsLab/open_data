#functions for common usage

#Function to clean files and remove nuks
CleanFiles<-function(file,newfile){
  writeLines(iconv(readLines(file,skipNul = TRUE)),
             newfile)
}

#Gets links from any single url; string matches
GetLinks <- function(url_name,string){
  files <- c()
  #this is inefficient and bad practice but it's a small vector.
  for(i in seq_along(url_name)){
    pg <- rvest::read_html(url_name[i])
    pg<-(rvest::html_attr(rvest::html_nodes(pg, "a"), "href"))
    files <- c(files,pg[grepl(string,pg,ignore.case = T)])
    files <- files %>% unique()
  }
  return(files)
}

#Read all csvs from urls; unz for zips
UnzipCSV <- function(files,zip_grep){
  
  if(missing(zip_grep) == T){
    zip_grep <- ''
  } else {
    zip_grep
  }
  
  #creates temp file to read in the data
  temp <- tempfile()
  download.file(files,temp,extdir=tempdir())
  #This is needed because a zip file may have multiple files
  file_names <- unzip(temp,list=T)$Name
  files_names <- file_names[grepl('.csv',file_names)]
  file_names <- file_names[grepl(pattern=zip_grep,x=file_names)]
  data <- lapply(file_names,
                function(x){
                  dirty_data <- unzip(temp,x,exdir=tempdir())
                  CleanFiles(dirty_data,dirty_data)
                  cleaned_data <- data.table::fread(dirty_data,encoding = "UTF-8")
                  #janitor to clean unruly names
                  names(cleaned_data) <- names(cleaned_data) %>% 
                    janitor::make_clean_names()  
                  return(cleaned_data)
                })
  names(data) <- file_names
  #unlink the temp file, important to do
  unlink(temp)
  data}

ReadExcelSheets <- function(filename, sheet_names, tibble = T) {
  #default args
  if(missing(sheet_names)){
    sheets <- readxl::excel_sheets(filename)
  } else {
    sheets <- sheet_names
  }
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}

ReadODSSheets <- function(filename, tibble = T) {
  sheets <- readODS::list_ods_sheets(filename)
  x <- lapply(sheets, function(X) readODS::read_ods(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}

#Download and read excel
ReadExcel <- function(files,sheets){
  #creates temp file to read in the data
  temp <- tempfile()
  download.file(files,temp)
  data <- ReadExcelSheets(filename = temp,sheet_names = sheets)
  #unlink the temp file, important to do
  unlink(temp)
  data}

#Download and read excel
ReadODS <- function(files){
  #creates temp file to read in the data
  temp <- tempfile()
  download.file(files,temp)
  #This is needed because a zip file may have multiple files
  data <- ReadODSSheets(temp)
  #unlink the temp file, important to do
  unlink(temp)
  data}

