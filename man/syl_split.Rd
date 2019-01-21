\name{syl_split}
\alias{syl_split}

\title{
Split Words into Syllables
}
\description{
This function splits words into syllables by keeping only functional syllables, meaning that some of the vowels and consonents will be dropped. It is most useful when trying to count the number of syllables in words and dealing with words that are uncommon, misspelled, or even invented.
}
\usage{
syl_split(input)
}

\arguments{
  \item{input}{
  A string of words
}
}
\details{

}
\value{
A list of syllables of each word in the string.
}
\references{

}
\author{
Kevin Wang
}
\note{
For words that are totally unusual and/or acronyms, this function may not work as expected.
}


\seealso{

}
\examples{
syl_split("The table in beside the chair.")
}
