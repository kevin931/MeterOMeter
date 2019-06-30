\name{meter_yaml}
\alias{meter_yaml}

\title{

Contert Meter-O-Meter YAML Output to Data Frames

}
\description{
This function converts the specific YAML file from www.meter-o-meter.com to data frames. While working with meter-o-meter YAML files, this function lays the ground work for other functions in this package and in R.
}
\usage{
meter_yaml(file)
}

\arguments{
  \item{file}{
  A local file path for the YAML file.
}
}
\details{

}
\value{
  A dataframe with
}
\references{

}
\author{
Kevin Wang
}
\note{
www.meter-o-meter.com is currently not open source. The function has work in conjunction with its outputs that are currently not available to the public. Efforts will be made in conjunction with www.meter-o-meter.com.
}


\seealso{

}
\examples{

meter_yaml("./data/moby_dick.yaml")

}
