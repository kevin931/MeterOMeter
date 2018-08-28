##################

# This script is used to convert numeric numbers to English language.

#Number to speech: Up to 5 digit integers and 10-digit decimals between 0 and 1

### FIX APPEND ZERO ARGUMENT

##################

num.to.speech<- function(x, zero.digit=F, round.digit=3, zero.append=F) {

  len<-length(x)

  single.digit<-c("zero", "one", "two", "three", "four",
                  "five", "six", "seven", "eight", "nine")

  teen<-c("ten", "eleven", "twelve", "thirteen", "fourteen",
          "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")

  ty<-c("twenty", "thirty", "forty", "fifty", "sixty",
        "seventy", "eighty", "ninety")

  if (len==1) {

    if (!is.numeric(x)) {

      if(grepl("[[:digit:]]",x) ) {

        warning("Non-numeric input. \nWith mixed characters and digits, results may not be accurate!")

        ### Insert Code Here

        x<-as.numeric(x)

        if (is.na(x)) {

          stop("Unable to convert 'x' to numeric.")

        }

      } else {

        stop("Non-numeric 'x' input.")

      }
    }

    dig.len<-nchar(x)

    if (round.digit>10) {

      x.1<-round(x, 10)

      warning("'round.digit' exceeding the maximum allowed.\n'round.digit' is set to 10")

    } else {

      x.1<-round(x, round.digit)

    }

    x.1<-as.character(x.1)

    if(grepl("[[:punct:]]", x)) {

      if (grepl("^0", x) & grepl("[[:punct:]]", x)) {

        x.1<-unlist(strsplit(x.1, split=".", fixed=T))

        x.1<-x.1[2]

        x.1<-unlist(strsplit(x.1, split=""))

        decimal.len<-length(x.1)

        if (round.digit>decimal.len) {

          if (zero.append) {

            x.2<-rep("zero", round.digit-decimal.len)

            for (i in 1:decimal.len) {

              x.1[i]<-single.digit[as.numeric(x.1[i])+1]

            }

            x.1<-append(x.1, x.2)

            warning("Too many digits to round. Zeros appended.")

          } else {

            for (i in 1:decimal.len) {

              x.1[i]<-single.digit[as.numeric(x.1[i])+1]

            }

            warning("Too many digits to round.")
          }



          if (zero.digit) {

            return(paste0("zero point ", paste(x.1, collapse = " ")))

          } else {

            return(paste0("point ", paste(x.1, collapse = " ")))
          }

        } else {

          x.1<-x.1[1:round.digit]

          for (i in 1:round.digit) {

            x.1[i]<-single.digit[as.numeric(x.1[i])+1]

          }

          if (zero.digit) {

            return(paste0("zero point ", paste(x.1, collapse = " ")))

          } else {

            return(paste0("point ", paste(x.1, collapse = " ")))
          }

        }

      } else {

        ##Code Needed Here
      }


    } else if (dig.len==1) {

      return(single.digit[x+1])

    } else if (dig.len==2) {

      x.1<-unlist(strsplit(x.1, split =""))

      if (x.1[1]=="1") {

        return(teen[as.numeric(x.1[2])+1])

      } else if (x.1[2]=="0") {

        return(paste(ty[as.numeric(x.1[1])-1]))

      } else {

        x.1<-unlist(strsplit(x.1, split=""))

        return(paste(ty[as.numeric(x.1[1])-1],single.digit[as.numeric(x.1[2])+1]))

      }

    } else if (dig.len==3) {

      x.1<-unlist(strsplit(x.1, split =""))

      if (x.1[2]=="1") {

        x.2<-teen[as.numeric(x.1[3])+1]

      } else if (x.1[2]=="0" & x.1[3]!="0") {

        x.2<-single.digit[as.numeric(x.1[3])+1]

      } else if (x.1[2]=="0" & x.1[3]=="0") {

        return(paste(single.digit[as.numeric(x.1[1])+1],"hundred"))

      } else if (x.1[2]!="0" & x.1[3]=="0") {

        x.2<-ty[as.numeric(x.1[2])-1]


      } else {

        x.2<-paste(ty[as.numeric(x.1[2])-1],
                   single.digit[as.numeric(x.1[3])+1],
                   sep="-")
      }

      return(paste(single.digit[as.numeric(x.1[1])+1]," hundred and ",x.2, sep="" ))

    } else if (dig.len==4) {

      x.1<-unlist(strsplit(x.1, split =""))

      if (x.1[2]=="0" & x.1[3]=="0" & x.1[4]=="0") {

        return(paste(single.digit[as.numeric(x.1[1])+1],"thousand"))

      } else if (x.1[2]=="0" & x.1[3]=="0" & x.1[4]!="0") {

        return(paste(single.digit[as.numeric(x.1[1])+1],
                     "thousand and",
                     single.digit[as.numeric(x.1[4])+1]
        )
        )

      } else if (x.1[2]!="0" & x.1[3]=="1") {

        x.2<-teen[as.numeric(x.1[4])+1]

      } else if (x.1[2]!="0" & x.1[3]=="0" & x.1[4]!="0") {

        x.2<-single.digit[as.numeric(x.1[4])+1]

      } else if (x.1[2]!="0" & x.1[3]!="0" & x.1[4]=="0") {

        x.2<-ty[as.numeric(x.1[3])-1]


      } else if (x.1[2]=="0" & x.1[3]=="1") {

        return(paste(single.digit[as.numeric(x.1[1])+1],
                     "thousand and",
                     teen[as.numeric(x.1[4])+1]
        )
        )

      } else if (x.1[2]=="0" & x.1[3]!="0" & x.1[4]=="0") {

        return(paste(single.digit[as.numeric(x.1[1])+1],
                     "thousand and",
                     ty[as.numeric(x.1[3])-1]
        )
        )

      } else if (x.1[2]=="0" & x.1[3]!="0" & x.1[4]!="0") {

        return(paste(single.digit[as.numeric(x.1[1])+1],
                     "thousand and",
                     ty[as.numeric(x.1[3])-1],
                     "-",
                     single.digit[as.numeric(x.1[4])+1]
        )
        )

      } else {

        x.2<-paste(ty[as.numeric(x.1[3])-1],
                   single.digit[as.numeric(x.1[4])+1],
                   sep="-")
      }

      x.3<-paste(single.digit[as.numeric(x.1[2])+1]," hundred and ",x.2, sep="" )

      return(paste(single.digit[as.numeric(x.1[1])+1],
                   " thousand ",
                   x.3,
                   sep=""))

    } else if (dig.len==5) {

      x.1<-unlist(strsplit(x.1, split =""))

      if (x.1[1]=="1") {

        if (x.1[3]=="0" & x.1[4]=="0" & x.1[5]=="0") {

          return(paste(teen[as.numeric(x.1[2])+1],"thousand"))

        } else if (x.1[3]=="0" & x.1[4]=="0" & x.1[5]!="0") {

          return(paste(teen[as.numeric(x.1[2])+1],
                       "thousand and",
                       single.digit[as.numeric(x.1[5])+1]
          )
          )

        } else if (x.1[3]!="0" & x.1[4]=="1") {

          return(paste(teen[as.numeric(x.1[2])+1],
                       "thousand",
                       single.digit[as.numeric(x.1[3])+1],
                       "hundred and",
                       teen[as.numeric(x.1[5])+1]
          )
          )


        } else if (x.1[3]!="0" & x.1[4]=="0" & x.1[5]!="0") {

          return(paste(teen[as.numeric(x.1[2])+1],
                       "thousand",
                       single.digit[as.numeric(x.1[3])+1],
                       "hundred and",
                       single.digit[as.numeric(x.1[5])+1]
          )
          )

        } else if (x.1[3]!="0" & x.1[4]!="0" & x.1[5]=="0") {

          return(paste(teen[as.numeric(x.1[2])+1],
                       "thousand",
                       single.digit[as.numeric(x.1[3])+1],
                       "hundred and",
                       ty[as.numeric(x.1[4])-1]
          )
          )


        } else if (x.1[3]=="0" & x.1[4]=="1") {

          return(paste(teen[as.numeric(x.1[2])+1],
                       "thousand and",
                       teen[as.numeric(x.1[5])+1]
          )
          )

        } else if (x.1[3]=="0" & x.1[4]!="0" & x.1[5]=="0") {

          return(paste(teen[as.numeric(x.1[2])+1],
                       "thousand and",
                       ty[as.numeric(x.1[4])-1]
          )
          )

        } else if(x.1[3]=="0" & x.1[4]!="0" & x.1[5]!="0") {

          return(paste(teen[as.numeric(x.1[2])+1],
                       " thousand and ",
                       ty[as.numeric(x.1[4])-1],
                       "-",
                       single.digit[as.numeric(x.1[5])+1],
                       sep=""
          )
          )

        } else {

          x.2<-paste(ty[as.numeric(x.1[4])-1],
                     single.digit[as.numeric(x.1[5])+1],
                     sep="-")

          x.3<-paste(single.digit[as.numeric(x.1[3])+1]," hundred and ",x.2, sep="" )

          return(paste(teen[as.numeric(x.1[2])+1],
                       " thousand ",
                       x.3,
                       sep=""))
        }


      } else if (!any(grepl("0",x.1))) {

        if (x.1[4]=="1") {

          return(paste(ty[as.numeric(x.1[1])-1],
                       "-",
                       single.digit[as.numeric(x.1[2])+1],
                       " thousand ",
                       single.digit[as.numeric(x.1[3])+1],
                       " hundred and ",
                       teen[as.numeric(x.1[5])+1],
                       sep=""))

        } else {

          x.2<-paste(ty[as.numeric(x.1[4])-1],
                     single.digit[as.numeric(x.1[5])+1],
                     sep="-")

          x.3<-paste(single.digit[as.numeric(x.1[3])+1]," hundred and ",x.2, sep="" )

          return(paste(ty[as.numeric(x.1[1])-1],
                       "-",
                       single.digit[as.numeric(x.1[2])+1],
                       " thousand ",
                       x.3,
                       sep=""))
        }

      } else {

        x.2<-paste(ty[as.numeric(x.1[1])-1],
                   single.digit[as.numeric(x.1[2])+1],
                   sep="-")

        if (x.1[3]=="0" & x.1[4]=="0" & x.1[5]=="0") {

          return(paste(x.2,"thousand"))

        } else if (x.1[3]=="0" & x.1[4]=="0" & x.1[5]!="0") {

          return(paste(x.2,
                       "thousand and",
                       single.digit[as.numeric(x.1[5])+1]
          )
          )

        } else if (x.1[3]=="0" & x.1[4]=="1") {

          return(paste(x.2,
                       "thousand and",
                       teen[as.numeric(x.1[5])+1]
          ))

        } else if (x.1[3]!="0" & x.1[4]=="0" & x.1[5]!="0") {

          return(paste(x.2,
                       "thousand",
                       single.digit[as.numeric(x.1[3])+1],
                       "hundred and",
                       single.digit[as.numeric(x.1[5])+1]
          ))

        } else if (x.1[3]!="0" & x.1[4]!="0" & x.1[5]=="0") {

          return(paste(x.2,
                       "thousand",
                       single.digit[as.numeric(x.1[3])+1],
                       "hundred and",
                       ty[as.numeric(x.1[4])-1]
          ))

        } else if (x.1[3]=="0" & x.1[4]!="0" & x.1[5]=="0") {

          return(paste(x.2,
                       "thousand and",
                       ty[as.numeric(x.1[4])-1]
          )
          )

        } else if(x.1[3]=="0" & x.1[4]!="0" & x.1[5]!="0") {

          return(paste(x.2,
                       " thousand and ",
                       ty[as.numeric(x.1[4])-1],
                       "-",
                       single.digit[as.numeric(x.1[5])+1],
                       sep=""
          )
          )

        } else if (x.1[3]!="0" & x.1[4]=="0" & x.1[5]=="0") {

          return(paste(x.2,
                       "thousand and",
                       single.digit[as.numeric(x.1[3])+1],
                       "hundred"
          ))
        }

      }



    }



  } else {

    stop("Vector length greater than 1. Using lapply/sapply recommended.")
  }



}
