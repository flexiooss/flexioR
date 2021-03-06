% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flexio.r
\name{downloadFlexioFile}
\alias{downloadFlexioFile}
\title{Download the file which is in the given Flexio record and stores it in the target file}
\usage{
downloadFlexioFile(targetFile, flexioResourceURL, flexioStorageURL, account,
  resourceName, auth, recordID, field)
}
\arguments{
\item{targetFile}{local file in which you want to store the downloaded file}

\item{flexioResourceURL}{URL of Flexio Resources API}

\item{flexioStorageURL}{URL of Flexio Object Storage API}

\item{account}{flexio account}

\item{resourceName}{name of the flexio resource in which the file is stored}

\item{auth}{Flexio authentification token}

\item{recordID}{the ID of the record in which the file is stored}

\item{field}{field in which the file is stored}
}
\description{
Download the file which is in the given Flexio record and stores it in the target file
}
\seealso{
Other Flexio Interaction: \code{\link{deleteFlexioRecord}},
  \code{\link{getFlexioRecord}},
  \code{\link{getFlexioResourceFieldsTypes}},
  \code{\link{getFlexioResourceSchema}},
  \code{\link{getFlexioResource}},
  \code{\link{patchFlexioRecord}},
  \code{\link{postFlexioResource}},
  \code{\link{putFlexioRecord}}
}
