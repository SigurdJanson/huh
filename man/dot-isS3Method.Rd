% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functiontype.R
\name{.isS3Method}
\alias{.isS3Method}
\title{.isS3Method}
\usage{
.isS3Method(method, env = parent.frame())
}
\arguments{
\item{env}{Base environment in which to look for function definition.}

\item{name}{Function or name of function.}
}
\value{
TRUE/FALSE
}
\description{
Checks if \code{method} is the name of a valid / registered S3 method.
}
\keyword{internal}
