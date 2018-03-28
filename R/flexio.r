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

  repeat{
    range <- sprintf("%i-%i", rangeFrom, rangeFrom + flexioPaginationLength)

    if(verbose)
    {
      req <- GET(requestURL, add_headers(Authorization=auth, range=range, header), verbose())
    }
    else{
      req <- GET(requestURL, add_headers(Authorization=auth, range=range, header))
    }

    if(! req$status_code %in% c(200,206)){print(http_status(req)$message); return(NULL)}

    resp <- fromJSON(content(req, "text"))
    new_dataset <- data.frame(resp, stringsAsFactors=FALSE)
    dataset <- rbind(dataset, new_dataset)


    rangeFrom <- rangeFrom + flexioPaginationLength
    if(req$status_code == 200){break}
  }

  if (length(fields) != 0){
    dataset <- subset(dataset, select=fields)
  }

  return(dataset)
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
    if(length(data) == 1){
      body <- sprintf('{\"%s\" : \"%s\"}', colnames(data),data[entry,]) #Manual JSON formatting
    }
    else{
      body <- toJSON(data)
    }
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
    # data <- subset(data, select = fields) FIXME PATCH doesn't work with missing fields
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
