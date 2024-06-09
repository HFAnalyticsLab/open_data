library(httr)
library(jsonlite)
library(tidyverse)

base <- 'https://opendata.nhsbsa.net/api/3/action/'
datastore <- 'datastore_search_sql?resource_id='
GetQuery <- function(dataset) {
  paste0("SELECT SUM(TOTAL_QUANITY_IN_VMP_UNIT) AS quantity, AVG(INDICATIVE_COST) as cost, VMP_SNOMED_CODE, VMP_PRODUCT_NAME, YEAR_MONTH FROM `",dataset,"` GROUP BY YEAR_MONTH, VMP_SNOMED_CODE, VMP_PRODUCT_NAME")
}

list_of_names <- jsonlite::fromJSON(('https://opendata.nhsbsa.net/api/3/action/package_show?id=secondary-care-medicines-data-indicative-price'))
datasets <- list_of_names[["result"]][["resources"]][["bq_table_name"]]

data<-lapply(
  datasets,
  function(x){
    resource_id <- paste0(x,'&sql=')
    query <- GetQuery(x)
    url <- URLencode(paste0(base,datastore,resource_id,query))
    data <- jsonlite::fromJSON(url)
    return(data[["result"]][["result"]][["records"]])
  }
)

scmd_data <- data %>%
  data.table::rbindlist()