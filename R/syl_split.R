#######

# This function splits words into syllables.
# It now has the accuracy of 85.9% if using the CMU dictionary as a guide.
# This function is basis of text_to_phoneme function
# which uses multiple cores and takes different types of intakes.

#########


syl_split<-function(word) {

  require(tm)

  if (word=="a.m." | word=="a.m") {

    return(c("a", "m"))
  }

  if (grepl("ment$", word)){

    word<-unlist(strsplit(word, ""))

    word<-word[1:(length(word)-4)]

    word<-paste0(word, collapse="")

    word.1<-"me"

  } else if(grepl("less$", word)) {

    word<-unlist(strsplit(word, ""))

    word<-word[1:(length(word)-4)]

    word<-paste0(word, collapse="")

    word.1<-"le"

  } else if (grepl("lessness$", word)) {

    word<-unlist(strsplit(word, ""))

    word<-word[1:(length(word)-8)]

    word<-paste0(word, collapse="")

    word.1<-c("le", "ne")

  } else if (grepl("fulness$", word)) {

    word<-unlist(strsplit(word, ""))

    word<-word[1:(length(word)-8)]

    word<-paste0(word, collapse="")

    word.1<-c("fu", "ne")

  } else if (grepl("ful$", word)) {

    word<-unlist(strsplit(word, ""))

    word<-word[1:(length(word)-3)]

    word<-paste0(word, collapse="")

    word.1<-c("fu")

  } else if (grepl("ology$", word)) {

    word<-unlist(strsplit(word, ""))

    word<-word[1:(length(word)-5)]

    word<-paste0(word, collapse="")

    word.1<-c("o", "lo", "gy")

  } else {

    word.1<-NA
  }

  vowels<-"[aeiouy]"

  word<-removePunctuation(word)

  character.string<-strsplit(word, "")

  character.string<-unlist(character.string)

  character.string.reserve<-character.string

  vowel.pos<-grep(vowels,character.string, perl=T)

  cons.pos<-grep("[^aeiouy]", character.string, perl=T)



  if (length(character.string)==1) {

    return(character.string)

  } else if (length(vowel.pos)==0) {

    if (word=="st" | word=="pm") {

      return(word)

    } else {

      return(character.string)

    }
  } else if (length(cons.pos)==0 & !any(grepl("[y]", character.string))) {

    if (length(character.string)==2 | length(character.string)==3 ) {

      return(word)

    } else {

      return(character.string)

    }

  } else {

    character.string.sub<-vector()

    if (length(vowel.pos) >1) {

      for (i in 1:(length(vowel.pos)-1)) {

        if ((vowel.pos[i+1]-vowel.pos[i])==1) {

          character.string.sub[i]<-paste(character.string[vowel.pos[i]],
                                         character.string[vowel.pos[i+1]],
                                         sep="")

        } else {

          character.string.sub[i]<-NA
        }
      }

      character.string.new<-vector()

      if (any(!is.na(character.string.sub))) {

        j<-1

        while (j <=  length(character.string.sub)) {

          if (!is.na(character.string.sub[j])) {

            character.string.new[j]<-paste(character.string[vowel.pos[j]],
                                           character.string[vowel.pos[j+1]],
                                           sep="")
            j<-j+2

          } else {

            character.string.new[j]<-character.string[vowel.pos[j]]

            j<-j+1

          }
        }
        character.string.new[j]<-character.string[vowel.pos[j]]

        for (k in 1:length(vowel.pos)) {

          character.string[vowel.pos[k]]<-character.string.new[k]

        }

        character.string<-character.string[-which(is.na(character.string))]
      }

      rm(character.string.new, character.string.sub)

      vowel.pos<-grep(vowels,character.string, perl=T)

      if (any(diff(vowel.pos)==1)) {

        time.1<-length(which(diff(vowel.pos)==1))

        for (rep.1 in 1:time.1) {

          character.string.sub<-vector()

          if (length(vowel.pos) >1) {

            for (i in 1:(length(vowel.pos)-1)) {

              if ((vowel.pos[i+1]-vowel.pos[i])==1) {

                character.string.sub[i]<-paste(character.string[vowel.pos[i]],
                                               character.string[vowel.pos[i+1]],
                                               sep="")

              } else {

                character.string.sub[i]<-NA
              }
            }

            character.string.new<-vector()

            if (any(!is.na(character.string.sub))) {

              j<-1

              while (j <=  length(character.string.sub)) {

                if (!is.na(character.string.sub[j])) {

                  character.string.new[j]<-paste(character.string[vowel.pos[j]],
                                                 character.string[vowel.pos[j+1]],
                                                 sep="")
                  j<-j+2

                } else {

                  character.string.new[j]<-character.string[vowel.pos[j]]

                  j<-j+1

                }
              }
              character.string.new[j]<-character.string[vowel.pos[j]]

              for (k in 1:length(vowel.pos)) {

                character.string[vowel.pos[k]]<-character.string.new[k]

              }

              character.string<-character.string[-which(is.na(character.string))]
            }
          }
        }


      }


    }

    character.string<-unlist(strsplit(character.string, "NA"))

    if (any(character.string=="")) {

      character.string<-character.string[-which(character.string=="")]

    }

    vowel.pos<-grep(vowels,character.string, perl=T)

    character.string.new<-vector()

    character.string.sub<-vector()

    if (length(vowel.pos)>1) {

      for (k.1 in 2:length(vowel.pos)) {

        if ((vowel.pos[k.1]-vowel.pos[k.1-1])==3 &
            (character.string[vowel.pos[k.1-1]+1]==character.string[vowel.pos[k.1]-1])) {

          character.string.new[k.1-1]<-"Yes"

        } else {

          character.string.new[k.1-1]<-NA
        }

      }

      if (any(!is.na(character.string.new))) {

        index.1<-which(character.string.new=="Yes")

        for (k.2 in index.1) {

          character.string.new[index.1]<- paste(character.string[vowel.pos[k.2]+1],
                                                character.string[vowel.pos[k.2]+1],
                                                sep="")

        }

        for (k.3 in index.1) {

          character.string[vowel.pos[k.3]+1]<-character.string.new[k.3]

          character.string[vowel.pos[k.3]+2]<-NA
        }

        character.string<-character.string[-which(is.na(character.string))]
      }
    }

    vowel.pos<-grep(vowels,character.string, perl=T)

    split.word<- vector()

    if (any(nchar(character.string)>1)) {

      multi.index<-which(nchar(character.string)>1)

      names(character.string)<-c(1:length(character.string))

      for (k.4 in 1:length(vowel.pos)) {

        if (vowel.pos[k.4]==1) {

          split.word[k.4]<- paste0(character.string[vowel.pos[k.4]])

          character.string[vowel.pos[k.4]]<-NA

        } else {

          split.word[k.4]<- paste0(character.string[vowel.pos[k.4]-1],
                                   character.string[vowel.pos[k.4]])

          character.string[vowel.pos[k.4]-1]<-NA
          character.string[vowel.pos[k.4]]<-NA
        }
      }

      names(split.word)<-c(vowel.pos)

      split.word<-append(split.word, character.string[multi.index])

    } else {

      for (k.4 in 1:length(vowel.pos)) {

        if (vowel.pos[k.4]==1) {

          split.word[k.4]<- paste0(character.string[vowel.pos[k.4]])

        } else {

          split.word[k.4]<- paste0(character.string[vowel.pos[k.4]-1],
                                   character.string[vowel.pos[k.4]])
        }
      }
    }

    names(split.word)<-NULL

    if (any(is.na(split.word))) {

      split.word<-split.word[-which(is.na(split.word))]
    }

    if (grepl("e$", word)) {

      if (length(split.word)==1) {

        split.word<-split.word

      } else if (grepl("[^aeiouyr][aeiou]e$", word)) {

        split.word<-split.word

      } else if (grepl("[^aeiouyr]le$", word)) {

        split.word<-split.word

      } else {

        split.word[length(split.word)]<-NA
      }
    }

    if (grepl("es$", word)) {

      if (length(split.word)==1) {

        split.word<-split.word

      } else if (grepl("[^aeiouyr][aeiou]e$", word)) {

        split.word<-split.word

      } else if(grepl("ges$|les$", word)) {

        split.word<-split.word

      } else {

        split.word[length(split.word)]<-NA
      }

    }

    if(grepl("[ae]nce$", word)) {

      split.word[length(split.word)]<-NA

    }

    if (any(is.na(split.word))) {

      split.word<-split.word[-which(is.na(split.word))]
    }

    if (grepl("dnt$", word)) {

      split.word<-append(split.word, "nt")

    }

    if (any(grepl("y[aeiou]$|y[aeiou][aeiou]$|y[aeiou][aeiou][aeiou]$|y[aeiou][aeiou][aeiou][aeiou]$|y[aeiou][aeiou][aeiou][aeiou][aeiou]",
                  split.word))

    ) {

      y.index<-which(grepl("y[aeiou]$|y[aeiou][aeiou]$|y[aeiou][aeiou][aeiou]$|y[aeiou][aeiou][aeiou][aeiou]$|y[aeiou][aeiou][aeiou][aeiou][aeiou]",
                           split.word))

      y.split<-strsplit(split.word[y.index], "")

      for (y.split.index in 1:length(y.split)) {

        y.placement<-which(y.split[[y.split.index]]=="y")

        if (length(y.placement)==1) {


          y.split[[y.split.index]]<-c(paste0(y.split[[y.split.index]][1:(y.placement-1)],
                                             collapse = ""),
                                      paste0(y.split[[y.split.index]][y.placement:(length(y.split[[y.split.index]]))],
                                             collapse= ""))

        } else {

          y.split[[y.split.index]]<-c(paste(y.split[[y.split.index]][1:(y.placement[length(y.placement)]-1)],
                                            collapse = ""),
                                      paste(y.split[[y.split.index]][y.placement[length(y.placement)]:length(y.split[[y.split.index]])],
                                            collapse = ""))

        }



        split.word<-append(split.word,
                           y.split[[y.split.index]],
                           after = (y.index[y.split.index])+2*y.split.index-2)

        split.word[(y.index[y.split.index])+2*y.split.index-2]<-NA

      }

      if (any(is.na(split.word))) {

        split.word<-split.word[-which(is.na(split.word))]
      }

    }

    if (length(split.word)>1) {

      if (split.word[length(split.word)]=="ye" &
          grepl("[aeiou]$", split.word[length(split.word)-1])) {

        split.word[length(split.word)-1] <- paste0(split.word[length(split.word)-1],
                                                   split.word[length(split.word)])

        split.word<-split.word[1:(length(split.word)-1)]

      }
    }


    if (!is.na(word.1)) {

      split.word<-append(split.word, word.1)

    }

    if (is.na(word.1) & grepl("ed$", word)) {

      if (length(split.word)==1) {

        split.word<-split.word

      } else if (grepl("[^aeiouyr][aeiou]ed$", word)) {

        split.word<-split.word

      } else if (grepl("[tdl]ed$", word)) {

        split.word<-split.word

      } else {

        split.word[length(split.word)]<-NA
      }

    }

    if (any(is.na(split.word))) {

      split.word<-split.word[-which(is.na(split.word))]
    }


    return(split.word)


  }

}
