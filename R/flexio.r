#' Prints Hello World
#' @family Random stuff
#' @export
helloworld <- function() {print("Array index starts from 1")}

#' Max range authorized by Flexio API
#' @export
flexioPaginationLength <- 100


#' Returns a dataset containing ressources from Flexio
#' @param fields name of the fields you want to get in your dataset (leave it empty if you want all the fields)
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param ressourceName name of the flexio ressource
#' @param auth flexio authentification token
#' @param header additional header (you don't need to add 'range' and 'Authorization', they have been put automaticaly for you)
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
getFlexioRessource <- function(flexioURL, account, ressourceName, auth, header=NULL, fields=c() ,verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',ressourceName, sep = "", collapse = NULL)
  rangeFrom <- 0
  dataset <- NULL

  repeat{
    range <- sprintf("%i-%i", rangeFrom, rangeFrom + flexioPaginationLength)

    req <- GET(requestURL, add_headers(Authorization=auth, range=range, header))

    if(! req$status_code %in% c(200,206)){quit()}

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

#' Sends a ressource to Flexio
#' @param fields name of the fields you want to get in your dataset (leave it empty if you want all the fields)
#' @param flexioURL URL of Flexio's API
#' @param account flexio account
#' @param ressourceName name of the flexio ressource
#' @param auth flexio authentification token
#' @param data the data you want to send to Flexio
#' @param verbose set it to TRUE to print the request details
#' @family Flexio Interaction
#' @export
postFlexioRessource <- function(flexioURL, account, ressourceName, auth, verbose=FALSE, data) {
  requestURL <- paste(flexioURL,'/',account,'/',ressourceName, sep = "", collapse = NULL)

  print(data)

  n <- nrow(data)
  for (entry in 1:n)
  {
    if(length(dataset) == 1){
      # body <- paste("'{",'"',colnames(data),'" : "', data[entry,], '"', "'}'")
      body <- sprintf('{\"%s\" : \"%s\"}', colnames(data),data[entry,])
    }
    else{
    }
    print(body)
    if(verbose){
      req <- POST(
        requestURL, body=body, add_headers(Authorization=auth, 'Content-type'='application/json'), verbose())
    }
    else{
      req <- POST(
        requestURL, body=body, add_headers(Authorization=auth, 'Content-type'='application/json'))
    }


    #TODO Verifier retour
  }
}
