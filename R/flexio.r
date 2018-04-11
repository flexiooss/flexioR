# FlexioR


flexioPaginationLength <- 100

#' Returns a dataset containing ressources reccords from Flexio
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param ressourceName name of the flexio ressource
#' @param auth flexio authentification token
#' @param header additional header (you don't need to add 'range' and 'Authorization', they have been put automaticaly for you)
#' @param fields name of the fields you want to get in your dataset (leave it empty if you want all the fields)
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
getFlexioRessource <- function(flexioURL, account, ressourceName, auth, header=NULL, fields=c() ,verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',ressourceName, sep = "", collapse = NULL)
  rangeFrom <- 0
  dataset <- NULL #Empty dataset

  dots <- c('.   ','..  ','... ')
  doti <- 1

  repeat{
    range <- sprintf("%i-%i", rangeFrom, rangeFrom + flexioPaginationLength)
    cat('\r')
    cat(sprintf("Getting reccords [%i %i] from Flexio  %s", rangeFrom, rangeFrom + flexioPaginationLength, dots[doti]))
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
  cat('\r')

  # Delete all the columns which are not fields of the ressource
  schema <- getFlexioRessourceFieldsTypes(
    flexioURL=flexioURL,
    account=account,
    ressourceName=ressourceName,
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

  dataset<- subset(dataset, select=names(schema))

  if (length(fields) != 0){
    dataset <- subset(dataset, select=fields)
  }
  return(dataset)
}


#' Returns the raw schema of a flexio ressource
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param ressourceName name of the flexio ressource
#' @param auth flexio authentification token
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
getFlexioRessourceSchema <- function(flexioURL, account, ressourceName, auth, verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',ressourceName,'/schema', sep = "", collapse = NULL)
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

#' Returns a list containing the Flexio ressource fields names and their types
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param ressourceName name of the flexio ressource
#' @param auth flexio authentification token
#' @family Flexio Interaction
#' @export
getFlexioRessourceFieldsTypes <- function(flexioURL, account, ressourceName, auth) {
  schema <- getFlexioRessourceSchema(
    flexioURL=flexioURL,
    account=account,
    ressourceName=ressourceName,
    auth=auth,
    verbose=FALSE
  )

  types <- list()

  for (field in schema$properties) {
    types[field$name] <- field$'data-type'
  }
  return(types)
}

#' Sends a ressource to Flexio. Adds each entry of the given dataset to the Flexio ressource
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param ressourceName name of the flexio ressource
#' @param auth flexio authentification token
#' @param data the data you want to send to Flexio
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
postFlexioRessource <- function(flexioURL, account, ressourceName, auth, data, verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',ressourceName, sep = "", collapse = NULL)

  n <- nrow(data)
  for (entry in 1:n)
  {
    jason <- toJSON(data[entry,])
    body <- substr(jason, 2, nchar(jason)-1)

    if(verbose){
      req <- POST(requestURL, body=body, add_headers(Authorization=auth, 'Content-type'='application/json'), verbose())
    }
    else{
      req <- POST(requestURL, body=body, add_headers(Authorization=auth, 'Content-type'='application/json'))
    }

    if(! req$status_code %in% c(201)){print(http_status(req)$message); return(FALSE)}
  }
  return(TRUE)
}

#' Returns a 1 entry dataset containing a single ressource reccord from Flexio
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param ressourceName name of the flexio ressource
#' @param auth flexio authentification token
#' @param reccordID : the ID of the reccord you want
#' @param fields name of the fields you want to get in your dataset (leave it empty if you want all the fields)
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
getFlexioReccord <- function(flexioURL, account, ressourceName, auth, reccordID, fields=c(), verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',ressourceName,'/',reccordID , sep = "", collapse = NULL)

  if(verbose){
    req <- GET(requestURL, add_headers(Authorization=auth), verbose())
  }
  else{
    req <- GET(requestURL, add_headers(Authorization=auth))
  }

  if(! req$status_code %in% c(200)){print(http_status(req)$message); return(NULL)}

  resp <- fromJSON(content(req, "text"))
  resp=resp[-(which(sapply(resp,is.null),arr.ind=TRUE))] #Remove the NULL fields

  reccord <- as.data.frame(resp, check.names=FALSE)
  if (length(fields) != 0){
    reccord <- subset(reccord, select=fields)
  }

  return(reccord)
}

#' Updates a ressource reccord from Flexio (all the fields)
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param ressourceName name of the flexio ressource
#' @param auth flexio authentification token
#' @param reccordID : the ID of the reccord you want
#' @param data : the data you want to use
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
putFlexioReccord <- function(flexioURL, account, ressourceName, auth, reccordID, data, verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',ressourceName,'/',reccordID , sep = "", collapse = NULL)

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


#' Updates a ressource reccord from Flexio (only the given fields)
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param ressourceName name of the flexio ressource
#' @param auth flexio authentification token
#' @param reccordID : the ID of the reccord you want
#' @param data : the data you want to use
#' @param fields : the fields you want to update (leave it empty to update all the fields)
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
patchFlexioReccord <- function(flexioURL, account, ressourceName, auth, reccordID, data, fields=c(), verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',ressourceName,'/',reccordID , sep = "", collapse = NULL)

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



#' Deletes a ressource reccord from Flexio
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param ressourceName name of the flexio ressource
#' @param auth flexio authentification token
#' @param reccordID : the ID of the reccord you want
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
deleteFlexioReccord <- function(flexioURL, account, ressourceName, reccordID, auth, verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',ressourceName,'/',reccordID , sep = "", collapse = NULL)

  if(verbose){
    req <- DELETE(requestURL, add_headers(Authorization=auth), verbose())
  }
  else{
    req <- DELETE(requestURL, add_headers(Authorization=auth))
  }
  if(! req$status_code %in% c(204)){print(http_status(req)$message); return(FALSE)}
  return(TRUE)
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

#' Convert a dataset's columns from TIME string format to numeric
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

setFieldNames <- function(dataset, names){
  for (i in 1:length(names)) {
    if(names(names[i]) %in% names(dataset)){
      dataset[,names[[i]]] <- dataset[,names(names[i])]
      dataset[,names(names[i])] <- NA
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
