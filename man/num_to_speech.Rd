\name{num_to_speech}
\alias{num_to_speech}

\title{

Convert Numbers to English
}
\description{

This function converts numbers to English in how people actually speak it instead of converting it digit by digit. It currents supports integers between 0 and 99999 and decimals between 0 and 1 with up to ten digits.

}
\usage{
num_to_speech(number, zero.digit=F, round.digit=3, zero.append=F)
}

\arguments{
  \item{number}{

  A vector of numbers.

}

  \item{zero.digits}{
  A logical vector indicating whether the zero before the decimal point should be pronunced.
  }

  \item{round.digit}{

  A numeric vector indicating how many digits after the decimal point to round.
  }

  \item{zero.append}{
A logical vector indicating whether zeroes will be appended if the round.digit is larger than the actual number of digits after the decimal point.
  }
}
\details{

}
\value{
A vector of numbers in English
}
\references{

}
\author{
Kevin Wang
}
\note{
Since this function attempts to replicate how humans actually say the numbers instead of saying all the digits, it now only supports a limited range of numbers.
}

\seealso{
}
\examples{
num_to_speech(c(100, 296, 0.1), T, 1, F)
}

