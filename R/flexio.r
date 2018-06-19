# FlexioR


flexioPaginationLength <- 100

#' Returns a dataset containing resources records from Flexio
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param resourceName name of the flexio resource
#' @param auth flexio authentification token
#' @param header additional header (you don't need to add 'range' and 'Authorization', they have been put automaticaly for you)
#' @param fields name of the fields you want to get in your dataset (leave it empty if you want all the fields)
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
getFlexioResource <- function(flexioURL, account, resourceName, auth, header=NULL, fields=c() ,verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',resourceName, sep = "", collapse = NULL)
  rangeFrom <- 0
  dataset <- NULL #Empty dataset

  dots <- c('.   ','..  ','... ')
  doti <- 1

  repeat{
    if(!verbose){cat('\r')}
    range <- sprintf("%i-%i", rangeFrom, rangeFrom + flexioPaginationLength-1)
    cat(sprintf("Getting records [%i %i] from Flexio  %s", rangeFrom, rangeFrom + flexioPaginationLength, dots[doti]))
    doti <- ifelse(doti == 3, 1, doti + 1)

    if(verbose)
    {
      req <- GET(requestURL, add_headers(Authorization=auth, range=range, header), verbose())
    }
    else{
      req <- GET(requestURL, add_headers(Authorization=auth, range=range, header))

    }

    if(! req$status_code %in% c(200,206)){cat('\r'); print(http_status(req)$message); return(NULL)}

    resp <- fromJSON(content(req, "text"))
    new_dataset <- data.frame(resp, stringsAsFactors=FALSE, check.names=FALSE)
    dataset <- rbind(dataset, new_dataset)

    rangeFrom <- rangeFrom + flexioPaginationLength
    if(req$status_code == 200){break}
  }
  if(!verbose){
    cat('\r')
  }

  # Delete all the columns which are not fields of the resource
  schema <- getFlexioResourceFieldsTypes(
    flexioURL=flexioURL,
    account=account,
    resourceName=resourceName,
    auth=auth
  )
  

  # Convert each column to its right type
  for(name in names(dataset)){
    if(! is.null(schema[[name]]))
    {
      switch(schema[[name]],
        DATETIME= {dataset <- castDATETIMEToTime(dataset, name)},
        TIME=     {dataset <- castTIMEToTime(dataset, name)},
        DATE=     {dataset <- castDATEToTime(dataset, name)}
      )
    }
  }
  
  dataset$RecordID <- dataset[,'_id']
  dataset<- subset(dataset, select=c('RecordID',names(schema)))

  if (length(fields) != 0){
    dataset <- subset(dataset, select=fields)
  }
  return(dataset)
}


#' Returns the raw schema of a flexio resource
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param resourceName name of the flexio resource
#' @param auth flexio authentification token
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
getFlexioResourceSchema <- function(flexioURL, account, resourceName, auth, verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',resourceName,'/schema', sep = "", collapse = NULL)
  if(verbose)
  {
    req <- GET(requestURL, add_headers(Authorization=auth), verbose())
  }
  else{
    req <- GET(requestURL, add_headers(Authorization=auth))
  }
  if(! req$status_code %in% c(200)){print(http_status(req)$message); return(NULL)}

  return(content(req))
}

#' Returns a list containing the Flexio resource fields names and their types
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param resourceName name of the flexio resource
#' @param auth flexio authentification token
#' @family Flexio Interaction
#' @export
getFlexioResourceFieldsTypes <- function(flexioURL, account, resourceName, auth) {
  schema <- getFlexioResourceSchema(
    flexioURL=flexioURL,
    account=account,
    resourceName=resourceName,
    auth=auth,
    verbose=FALSE
  )

  types <- list()

  for (field in schema$properties) {
    types[field$name] <- field$'data-type'
  }
  return(types)
}

#' Sends a resource to Flexio. Adds each entry of the given dataset to the Flexio resource, Returns the a vector with the recordIDs
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param resourceName name of the flexio resource
#' @param auth flexio authentification token
#' @param data the data you want to send to Flexio
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
postFlexioResource <- function(flexioURL, account, resourceName, auth, data, verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',resourceName, sep = "", collapse = NULL)

  n <- nrow(data)

  dots <- c('.   ','..  ','... ')
  doti <- 1

  records <- c()

  for (entry in 1:n)
  {
    if (!verbose){cat('\r')}
    jason <- toJSON(data[entry,])
    body <- substr(jason, 2, nchar(jason)-1)

    cat(sprintf("Posting record #%i of %i to Flexio  %s", entry, n + flexioPaginationLength, dots[doti]))
    doti <- ifelse(doti == 3, 1, doti + 1)

    if(verbose){
      req <- POST(requestURL, body=body, add_headers(Authorization=auth, 'Content-type'='application/json'), verbose())
    }
    else{
      req <- POST(requestURL, body=body, add_headers(Authorization=auth, 'Content-type'='application/json'))
    }

    records <- c(records, req$header['X-Entity-Id'])

    if(! req$status_code %in% c(201)){print(http_status(req)$message); return(FALSE)}
  }

  if(!verbose){
    cat('\r')
  }

  return(unlist(records, use.names=FALSE))
}

#' Returns a 1 entry dataset containing a single resource record from Flexio
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param resourceName name of the flexio resource
#' @param auth flexio authentification token
#' @param recordID : the ID of the record you want
#' @param fields name of the fields you want to get in your dataset (leave it empty if you want all the fields)
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
getFlexioRecord <- function(flexioURL, account, resourceName, auth, recordID, fields=c(), verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',resourceName,'/',recordID , sep = "", collapse = NULL)

  if(verbose){
    req <- GET(requestURL, add_headers(Authorization=auth), verbose())
  }
  else{
    req <- GET(requestURL, add_headers(Authorization=auth))
  }

  if(! req$status_code %in% c(200)){print(http_status(req)$message); return(NULL)}

  resp <- fromJSON(content(req, "text"))
  for (e in 1:length(resp)) {
    if(is.null(unlist(resp[e]))){
      resp[e] <- 0
    }
  }
  record <- as.data.frame(resp, check.names=FALSE)
  
  record$RecordID <- recordID
  if (length(fields) != 0){
    record <- subset(record, select=fields)
  }

  return(record)
}

#' Updates a resource record from Flexio (all the fields)
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param resourceName name of the flexio resource
#' @param auth flexio authentification token
#' @param recordID : the ID of the record you want
#' @param data : the data you want to use
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
putFlexioRecord <- function(flexioURL, account, resourceName, auth, recordID, data, verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',resourceName,'/',recordID , sep = "", collapse = NULL)

  body <- toJSON(data)
  body <- substring(body,2,nchar(body)-1)

  if(verbose){
    req <- PUT(requestURL, add_headers(Authorization=auth, 'Content-type'='application/json'), body=body, verbose())
  }
  else
  {
    req <- PUT(requestURL, add_headers(Authorization=auth, 'Content-type'='application/json'), body=body)
  }
  if(! req$status_code %in% c(200)){print(http_status(req)$message); return(FALSE)}
  return(TRUE)
}


#' Updates a resource record from Flexio (only the given fields)
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param resourceName name of the flexio resource
#' @param auth flexio authentification token
#' @param recordID : the ID of the record you want
#' @param data : the data you want to use
#' @param fields : the fields you want to update (leave it empty to update all the fields)
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
patchFlexioRecord <- function(flexioURL, account, resourceName, auth, recordID, data, fields=c(), verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',resourceName,'/',recordID , sep = "", collapse = NULL)

  if(length(fields) > 0){
    data <- subset(data, select = fields) #FIXME PATCH doesn't work with missing fields
  }

  body <- toJSON(data)
  body <- substring(body,2,nchar(body)-1)

  if(verbose){
    req <- PATCH(requestURL, add_headers(Authorization=auth, 'Content-type'='application/json'), body=body, verbose())
  }
  else{
    req <- PATCH(requestURL, add_headers(Authorization=auth, 'Content-type'='application/json'), body=body)
  }
  if(! req$status_code %in% c(200)){print(http_status(req)$message); return(FALSE)}
  return(TRUE)
}



#' Deletes a resource record from Flexio
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param resourceName name of the flexio resource
#' @param auth flexio authentification token
#' @param recordID : the ID of the record you want
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
deleteFlexioRecord <- function(flexioURL, account, resourceName, recordID, auth, verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',resourceName,'/',recordID , sep = "", collapse = NULL)

  if(verbose){
    req <- DELETE(requestURL, add_headers(Authorization=auth), verbose())
  }
  else{
    req <- DELETE(requestURL, add_headers(Authorization=auth))
  }
  if(! req$status_code %in% c(204)){print(http_status(req)$message); return(FALSE)}
  return(TRUE)
}

#' @family Flexio Interaction
#' @export
downloadFlexioFile <- function(targetFile, flexioResourceURL, flexioStorageURL, account, resourceName, auth, recordID, field){
  record <- getFlexioRecord(
    flexioURL = flexioResourceURL, 
    account = account,
    resourceName = resourceName,
    auth = auth, 
    recordID = recordID, 
    fields = field
  )
  file_url <- sprintf("%s/%s",flexioStorageURL, as.character(record[1,1]))
  download.file(file_url, destfile = targetFile, method = 'curl', extra=list('-k'))
  
  return(targetFile)
  
}

#' Convert a dataset's columns from string to numeric
#' @param dataset the dataset you want to convert
#' @param colnames the names of the columns you want to convert
#' @family Data formatting
#' @export
castStringToNum <- function(dataset, colnames) {
  for(c in colnames){
    dataset[,c] <- as.numeric(gsub(',','.',dataset[,c]))
  }
  return(dataset)
}

#' Convert a dataset's columns from DATETIME string format to numeric
#' @param dataset the dataset you want to convert
#' @param colnames the names of the columns you want to convert
#' @family Data formatting
#' @export
castDATETIMEToTime <- function(dataset, colnames) {
  for(c in colnames){
    dataset[,c] <- as.numeric(as.POSIXct(dataset[,c],format="%Y-%m-%dT%H:%M:%S"))
  }
  return(dataset)
}

#' Convert a dataset's columns from DATE string format to numeric
#' @param dataset the dataset you want to convert
#' @param colnames the names of the columns you want to convert
#' @family Data formatting
#' @export
castDATEToTime <- function(dataset, colnames) {
  for(c in colnames){
    dataset[,c] <- as.numeric(as.POSIXct(dataset[,c],format="%Y-%m-%dT"))
  }
  return(dataset)
}

#' Converts a dataset's columns from TIME string format to numeric
#' @param dataset the dataset you want to convert
#' @param colnames the names of the columns you want to convert
#' @family Data formatting
#' @export
castTIMEToTime <- function(dataset, colnames) {
  for(c in colnames){
    dataset[,c] <- as.numeric(as.POSIXct(dataset[,c],format="%H:%M:%S"))
  }
  return(dataset)
}

#' Replaces the fields names with the given ones
#' @param dataset the dataset you want to update names
#' @param names list of the old and new names formatted like that : list('old_name1'='new_name1', 'old_name2'='new_name2', ...)
#' @family Data formatting
#' @export
setFieldNames <- function(dataset, names){
  for (i in 1:length(names)) {
    if(names(names[i]) %in% names(dataset)){
      dataset[,names[[i]]] <- dataset[,names(names[i])]
      dataset[,names(names[i])] <- NULL
    }
  }
  return(dataset)
}

#' Genrerates a training and a validation dataset with a given separation
#' @param dataset the dataset you want to split
#' @param separation the repartition you want. Must be between 0 and 1
#' @family Data preparation
#' @export
splitDataset <- function(dataset, separation) {
  if(separation <= 0 || 1 <= separation){
    sprintf("Error : %s is not in ]0,1[", separation);
    return(FALSE)
  }
  ind <- sample(2, nrow(dataset), replace=TRUE, prob=c(separation, 1-separation))
  dataset.training <- dataset[ind==1,]
  dataset.validation <- dataset[ind==2,]

  return(list(training=dataset.training, validation=dataset.validation))
}

#' Clean a dataset. Remove all rows containing missing values
#' @param dataset the dataset you want to clean
#' @family Data preparation
#' @export
cleanDataset <- function(dataset){
  dataset <- na.omit(dataset)
  return(dataset)
}

#' Saves a dataset in a .csv file
#' @param the dataset you want to save
#' @param the file you want to save the dataset in. Leave if empty to use the default file (tmp.csv)
#' @family others
#' @export
saveDatasetToFile <- function(dataset, file="tmp.csv") {
  write.csv(dataset, file=file, row.names=FALSE)
}
