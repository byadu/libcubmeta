% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{opendb}
\alias{opendb}
\title{opendb}
\usage{
opendb(
  db = NULL,
  host = "localhost",
  user = "cubot",
  password = "",
  port = 3306
)
}
\arguments{
\item{db}{is the database name}

\item{host}{is the database host/server}

\item{user}{is the database user id}

\item{password}{is the database password for the user}

\item{port}{is the database port}
}
\description{
Open a 'MySQL' connection
}
\details{
Returns a 'MySQL' connection
}
