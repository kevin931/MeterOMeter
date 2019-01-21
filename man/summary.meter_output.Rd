\name{summary.meter_output}
\alias{summary.meter_output}

\title{
"Summary for Meter_Score output"
}
\description{
This function is a method of summary on the object returned from meter_score
}
\usage{
summary.meter_output(x)
}

\arguments{
  \item{x}{
  An S3 object returned from meter_score
}
}
\details{

}
\value{

The values returned are printed summaries.
}
\references{

}
\author{
Kevin Wang
}
\note{

}


\seealso{

}
\examples{
require(MeterOMeter)

output <- meter_score("010101010111", "iambic", "pentameter")

summary(output)
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
