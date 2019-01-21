\name{text_to_syl}
\alias{text_to_syl}
\title{

Converting Words to Syllables

}
\description{

This function implements the function syl_split by providing support for vector or string input. It also has the option for multi-threading for better performance on a modern CPU.
}
\usage{
text_to_syl(x,
            type=c("vector", "string"),
            multi_core=F,
            core_used=NA)
}

\arguments{
  \item{x}{
    A vector or a single string containing one or more words.
}
  \item{type}{

    "vector" for vector input and "string" input
  }

  \item{multi_core}{

  a logical indicating whether to use multiple cores with parallel package
  }

  \item{core_used}{

  a numeric vector that specifies how many cores to use if "multi_core" is TRUE.
  }
}
\details{

This function is dependent upon the syl_split function and for better performance, the parallel package is required. In the future, the type argument may no longer be required as it will be handled by the function itself.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

}
\author{
Kevin Wang
}
\note{
Future support for Windows;
May merge with syl_split in the future versions.
}


\seealso{

}
\examples{

require(MeterOMeter)

text_to_syllable(c("I", "am", "a", "developer"), "vector")

text_to_syllable("I am a developer", "string")

text_to_syllable("I am a developer", "string", T, 2)

}

