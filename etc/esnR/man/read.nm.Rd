\name{read.nm} 
\alias{read.nm} 
\title{Read NONMEM tables into R Data frames}
\description{ 
  Read NONMEM tables into NONMEM.
} 
\usage{
read.nm(file,na.strings=c("NA","."),...)
} 
\arguments{ 
  \item{file}{file name of the table to import} 
  \item{na.strings}{Missing strings to import.  By default they are "NA"
    and "."} 
  \item{...}{Other arguments to pass to read.table function} 
} 

\details{
  Read data into NONMEM data frame.  If the NONMEM data is in PLT tools
  directory structure, read the pre-converted CSV instead.
}

\author{
  Matthew L. Fidler <matthew.filder@gmail.com>
  Maintainer: Matthew L. Fidler <matthew.fidler@gmail.com>
}
\examples{ 
  # Read in NONMEM table
  \dontrun{
    d <- read.nm("table.tbl")
  }
}
\keyword{nonmem tables}

