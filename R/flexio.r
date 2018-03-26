library(httr)
library(jsonlite)

#' Prints Hello World
#' @export
helloworld <- function() {print(c("Hello","World"))}

#' Max range authorized by Flexio API
#' @export
flexioPaginationLength <- 100


#' Gets a ressource from Flexio
#' @param flexioURL URL of Flexio's API
#' @export
getFlexioRessource <- function(flexioURL, account, ressourceName, auth, header=NULL, fields=c() ,verbose=FALSE) {
  requestURL <- paste(flexioURL,'/',account,'/',ressourceName, sep = "", collapse = NULL)
  rangeFrom <- 0
  dataset <- NULL

  repeat{
    range <- sprintf("%i-%i", rangeFrom, rangeFrom + flexioPaginationLength)

    if(verbose){
      req <- GET(requestURL, add_headers(Authorization=auth, range=range, header), verbose()) #TODO Retirer verbose
    }
    else{
      req <- GET(requestURL, add_headers(Authorization=auth, range=range, header))
    }

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
