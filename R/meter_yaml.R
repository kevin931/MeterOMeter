meter_yaml<-function(file) {

  temp<-readLines(file)

  temp<-temp[6:length(temp)]

  a<-grep("^- boundaries", temp)

  list.1<-list()

  for (i in 1:length(a)) {

    if (i==length(a)) {

      list.1[[i]]<-temp[a[i]:length(temp)]

    } else {

      list.1[[i]]<-temp[a[i]:(a[i+1]-1)]

    }
  }

  rm(temp, a)

  word <- vector()

  pronunciation.1<-vector()

  source.1<-vector()

  score.1<-vector()

  pronunciation.2<-vector()

  source.2<-vector()

  score.2<-vector()

  pronunciation.3<-vector()

  source.3<-vector()

  score.3<-vector()

  pronunciation.4<-vector()

  source.4<-vector()

  score.4<-vector()

  for (i in 1:length(list.1)) {

    sub_list<-list.1[[i]]

    words<-grep("word:", sub_list)

    word[i]<-unlist(strsplit(sub_list[words[1]], ": "))[2]

    pronunciations<-grep("^\\s{4}\\-\\sbinary", sub_list)

    sources<-grep("source:", sub_list)

    scores<-grep("\\s{6}score", sub_list)

    if(length(pronunciations)==1) {

      pronunciation.1[i]<-unlist(strsplit(sub_list[pronunciations], ": "))[2]

      source.1[i]<-unlist(strsplit(sub_list[sources], ": "))[2]

      score.1[i]<-unlist(strsplit(sub_list[scores], ": "))[2]

      pronunciation.2[i]<-NA

      pronunciation.3[i]<-NA

      pronunciation.4[i]<-NA

      source.2[i]<-NA

      source.3[i]<-NA

      source.4[i]<-NA

      score.2[i]<-NA

      score.2[i]<-NA

      score.3[i]<-NA

      score.4[i]<-NA

    } else if (length(pronunciations)==2) {

      pronunciation.1[i]<-unlist(strsplit(sub_list[pronunciations[1]], ": "))[2]

      source.1[i]<-unlist(strsplit(sub_list[sources[1]], ": "))[2]

      score.1[i]<-unlist(strsplit(sub_list[scores[1]], ": "))[2]

      pronunciation.2[i]<-unlist(strsplit(sub_list[pronunciations[2]], ": "))[2]

      source.2[i]<-unlist(strsplit(sub_list[sources[2]], ": "))[2]

      score.2[i]<-unlist(strsplit(sub_list[scores[2]], ": "))[2]

      pronunciation.3[i]<-NA

      pronunciation.4[i]<-NA

      source.3[i]<-NA

      source.4[i]<-NA

      score.3[i]<-NA

      score.4[i]<-NA

    } else if (length(pronunciations)==3) {

      pronunciation.1[i]<-unlist(strsplit(sub_list[pronunciations[1]], ": "))[2]

      source.1[i]<-unlist(strsplit(sub_list[sources[1]], ": "))[2]

      score.1[i]<-unlist(strsplit(sub_list[scores[1]], ": "))[2]

      pronunciation.2[i]<-unlist(strsplit(sub_list[pronunciations[2]], ": "))[2]

      source.2[i]<-unlist(strsplit(sub_list[sources[2]], ": "))[2]

      score.2[i]<-unlist(strsplit(sub_list[scores[2]], ": "))[2]

      pronunciation.3[i]<-unlist(strsplit(sub_list[pronunciations[3]], ": "))[2]

      source.3[i]<-unlist(strsplit(sub_list[sources[3]], ": "))[2]

      score.3[i]<-unlist(strsplit(sub_list[scores[3]], ": "))[2]

      pronunciation.4[i]<-NA

      source.4[i]<-NA

      score.4[i]<-NA

    } else if (length(pronunciations)==4) {

      pronunciation.1[i]<-unlist(strsplit(sub_list[pronunciations[1]], ": "))[2]

      source.1[i]<-unlist(strsplit(sub_list[sources[1]], ": "))[2]

      score.1[i]<-unlist(strsplit(sub_list[scores[1]], ": "))[2]

      pronunciation.2[i]<-unlist(strsplit(sub_list[pronunciations[2]], ": "))[2]

      source.2[i]<-unlist(strsplit(sub_list[sources[2]], ": "))[2]

      score.2[i]<-unlist(strsplit(sub_list[scores[2]], ": "))[2]

      pronunciation.3[i]<-unlist(strsplit(sub_list[pronunciations[3]], ": "))[2]

      source.3[i]<-unlist(strsplit(sub_list[sources[3]], ": "))[2]

      score.3[i]<-unlist(strsplit(sub_list[scores[3]], ": "))[2]

      pronunciation.4[i]<-unlist(strsplit(sub_list[pronunciations[4]], ": "))[2]

      source.4[i]<-unlist(strsplit(sub_list[sources[4]], ": "))[2]

      score.4[i]<-unlist(strsplit(sub_list[scores[4]], ": "))[2]


    } else {

      warning("This function now only supports 4 pronunciations.\n
              Future versions will add better support.")

      pronunciation.1[i]<-unlist(strsplit(sub_list[pronunciations[1]], ": "))[2]

      source.1[i]<-unlist(strsplit(sub_list[sources[1]], ": "))[2]

      score.1[i]<-unlist(strsplit(sub_list[scores[1]], ": "))[2]

      pronunciation.2[i]<-unlist(strsplit(sub_list[pronunciations[2]], ": "))[2]

      source.2[i]<-unlist(strsplit(sub_list[sources[2]], ": "))[2]

      score.2[i]<-unlist(strsplit(sub_list[scores[2]], ": "))[2]

      pronunciation.3[i]<-unlist(strsplit(sub_list[pronunciations[3]], ": "))[2]

      source.3[i]<-unlist(strsplit(sub_list[sources[3]], ": "))[2]

      score.3[i]<-unlist(strsplit(sub_list[scores[3]], ": "))[2]

      pronunciation.4[i]<-unlist(strsplit(sub_list[pronunciations[4]], ": "))[2]

      source.4[i]<-unlist(strsplit(sub_list[sources[4]], ": "))[2]

      score.4[i]<-unlist(strsplit(sub_list[scores[4]], ": "))[2]

    }
  }

  rm(i, sub_list, words, pronunciations, scores, sources)



  #### Hot fixing an encoding issue


  pronunciation.1 <- strsplit(pronunciation.1, split="")

  pronunciation.1.fix <- vector()

  for (i in 1:length(pronunciation.1)) {

    word_pro <- pronunciation.1[[i]]

    num_index <- which(word_pro == "0" | word_pro=="1")

    pronunciation.1.fix[i] <- paste(word_pro[num_index], collapse = "")


  }

  pronunciation.1 <- pronunciation.1.fix

  pronunciation.2 <- strsplit(pronunciation.2, split="")

  pronunciation.2.fix <- vector()

  for (i in 1:length(pronunciation.2)) {

    word_pro <- pronunciation.2[[i]]

    num_index <- which(word_pro == "0" | word_pro=="1")

    pronunciation.2.fix[i] <- paste(word_pro[num_index], collapse = "")


  }

  pronunciation.2 <- pronunciation.2.fix

  pronunciation.3 <- strsplit(pronunciation.3, split="")

  pronunciation.3.fix <- vector()

  for (i in 1:length(pronunciation.3)) {

    word_pro <- pronunciation.3[[i]]

    num_index <- which(word_pro == "0" | word_pro=="1")

    pronunciation.3.fix[i] <- paste(word_pro[num_index], collapse = "")


  }

  pronunciation.3 <- pronunciation.3.fix

  pronunciation.4 <- strsplit(pronunciation.4, split="")

  pronunciation.4.fix <- vector()

  for (i in 1:length(pronunciation.4)) {

    word_pro <- pronunciation.4[[i]]

    num_index <- which(word_pro == "0" | word_pro=="1")

    pronunciation.4.fix[i] <- paste(word_pro[num_index], collapse = "")


  }

  pronunciation.4 <- pronunciation.4.fix

  rm(pronunciation.4.fix, pronunciation.3.fix, pronunciation.2.fix,
     pronunciation.1.fix, word_pro, nun_index)

  syllables.1<-nchar(pronunciation.1)

  syllables.2<-nchar(pronunciation.2)

  syllables.3<-nchar(pronunciation.3)

  syllables.4<-nchar(pronunciation.4)

  data<-data.frame(word,
                   pronunciation.1,
                   syllables.1,
                   source.1,
                   score.1,
                   pronunciation.2,
                   syllables.2,
                   source.2,
                   score.2,
                   pronunciation.3,
                   syllables.3,
                   source.3,
                   score.3,
                   pronunciation.4,
                   syllables.4,
                   source.4,
                   score.4)

  return(data)

}
