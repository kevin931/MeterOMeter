\name{meter_score}
\alias{meter_score}

\title{

  Metrical Density

}
\description{
This function calculates the mertical density of given text in the form of binary stresses. It supports most common meters, but texts have to be pre-processed so that "0" represents an unstressed syllable and "1" represents a stressed syllable.
}
\usage{
function(x,
        meter_type=c("iambic",
                     "trochaic",
                     "spondaic",
                     "anapestic",
                     "dactylic",
                     "pyrrhic"),
        meter_length=c("dimeter",
                       "trimeter",
                       "tetrameter",
                       "pentameter",
                       "hexameter",
                       "heptameter",
                       "octameter"),
        secondary_stress=T,
        meterometer.com=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A dataframe from the output of meter_yaml or a vector or a string of binary inputs.
}
  \item{meter_type}{

  A string indicating one of the six type of stress mattern to look for
  }
  \item{meter_length}{

  A string indicating one of the six supported meter length
  }
  \item{secondary_stress}{

  A logical vector indicating whether there are encoded "2" for optional stresses.
  }
}
\details{
}
\value{
  \item{metrical_score}{
  A single numeric value which is the percentage of matched instances
  }
  \item{metrical_structure}{
  A vector indicating which possible instances have exact matches.
  }
}
\references{
}
\author{
Kevin Wang
}
\note{
This function is in essence a pattern matching algorithm with a targeted audience of literary scholars. Ideally, this function should be combined with a program or algorithm that parses raw text data.
}


\seealso{

}
\examples{
meter_score("0101010100010101010101", "iambic", "dimeter", F, F)

meter_score("0101020122010101010101", "iambic", "dimeter", T, F)

}

