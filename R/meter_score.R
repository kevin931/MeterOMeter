meter_score<-function(x,
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

                      word_boundary=F,

                      meterometer.com=F
                       )
{
  if (meter_length=="dimeter") {

    meter_length<-2

  } else if (meter_length=="trimeter") {

    meter_length<-3

  } else if (meter_length=="tetrameter") {

    meter_length<-4

  } else if (meter_length=="pentameter") {

    meter_length<-5

  } else if (meter_length=="hexameter") {

    meter_length<-6

  } else if (meter_length=="heptameter") {

    meter_length<-7

  } else {

    meter_length<-8
  }

  if (meter_type=="iambic") {

    meter_type<-"01"

  } else if (meter_type=="trochaic") {

    meter_type<-"10"

  } else if (meter_type=="spondaic") {

    meter_type<-"11"

  } else if (meter_type=="anapestic") {

    meter_type<-"001"

  } else if (meter_type=="dactylic") {

    meter_type<-"100"
  } else if (meter_type=="pyrrhic") {

    meter_type<-"00"
  }

  pattern<-rep(meter_type, meter_length)

  pattern<-paste(pattern, collapse = "")

  pattern_length<-nchar(pattern)

  rm(meter_length, meter_type)

  if (meterometer.com) {

    pronunciation.1<-as.character(x$pronunciation.1)

    pronunciation.2<-as.character(x$pronunciation.2)

    pronunciation.3<-as.character(x$pronunciation.3)

    pronunciation.4<-as.character(x$pronunciation.4)

    pronunciation.2[which(is.na(pronunciation.2))]<-pronunciation.1[which(is.na(pronunciation.2))]

    pronunciation.3[which(is.na(pronunciation.3))]<-pronunciation.1[which(is.na(pronunciation.3))]

    pronunciation.4[which(is.na(pronunciation.4))]<-pronunciation.1[which(is.na(pronunciation.4))]

    pronunciation.1<-paste(pronunciation.1, collapse = "")

    pronunciation.1<-unlist(strsplit(pronunciation.1, ""))

    pronunciation.2<-paste(pronunciation.2, collapse = "")

    pronunciation.2<-unlist(strsplit(pronunciation.2, ""))

    pronunciation.3<-paste(pronunciation.3, collapse = "")

    pronunciation.3<-unlist(strsplit(pronunciation.3, ""))

    pronunciation.4<-paste(pronunciation.4, collapse = "")

    pronunciation.4<-unlist(strsplit(pronunciation.4, ""))

    index.1<-vector()

    index.2<-vector()

    index.3<-vector()

    index.4<-vector()

    for (i in 1:(length(pronunciation.1)-pattern_length+1)) {

      portion.1<-paste(pronunciation.1[i:(i+pattern_length-1)], collapse = "")

      portion.2<-paste(pronunciation.2[i:(i+pattern_length-1)], collapse = "")

      portion.3<-paste(pronunciation.3[i:(i+pattern_length-1)], collapse = "")

      portion.4<-paste(pronunciation.4[i:(i+pattern_length-1)], collapse = "")

      if (portion.1==pattern) {

        index.1[i]<-1
      } else {

        index.1[i]<-0
      }

      if (portion.2==pattern) {

        index.2[i]<-1
      } else {

        index.2[i]<-0
      }

      if (portion.3==pattern) {

        index.3[i]<-1
      } else {

        index.3[i]<-0
      }

      if (portion.4==pattern) {

        index.4[i]<-1
      } else {

        index.4[i]<-0
      }

    }

    index<-index.1+index.2+index.3+index.4

    for (i in 1:length(index)) {

      if (index[i]==0) {

        index[i]<-0

      }  else {

        index[i]<-1
      }

    }

    rm(portion.1, portion.2, portion.3, portion.4,
       pronunciation.1, pronunciation.2, pronunciation.3,
       pronunciation.4)

    if (word_boundary) {

      warning("word_boundary not yet available.
              Stay tuned for future updates.")

      if (1==0) {

        word.index<-rep(0, nrow(x))

        syl.start.1<-vector()

        syl.end.1<-vector()

        syllables.1<-x$syllables.1

        for (i in 1:nrow(x)) {

          if (i==1) {

            syl.end.1[i]<-syllables.1[i]

            syl.start.1[i]<- 1

          }  else {

            syl.end.1[i]<-sum(syllables.1[i], syl.end.1[i-1])

            syl.start.1[i]<- syl.end.1[i]-syllables.1[i]+1

          }

        }


        index.1.syl<-index.1.syl[which(index.1==1)]

        length.syl.start<-length(syl.start.1)

        length.syl.end<-length(syl.end.1)

        for (i in 1:length(index.1.syl)) {

          print(i)

          instance<-unlist(strsplit(index.1.syl[i], split = "_"))

          if (length(setdiff(syl.start.1, instance[1]))<length.syl.start &
              length(setdiff(syl.end.1, instance[2]))<length.syl.end ) {

            word.index[which(syl.start.1==instance[1]):which(syl.end.1==instance[2])]<-1

          }

        }

      }


      }


    metrical_score<-sum(index)/length(index)

    metrical_structure<-index



  } else {

    x<-paste(x, collapse="")

    x<-unlist(strsplit(x, ""))

    if (secondary_stress) {

      x[which(x=="2")]<-"1"

    } else {

      x[which(x=="2")]<-"0"

    }

    index<-vector()

    for (i in 1:(length(x)-pattern_length+1)) {

      portion<-paste(x[i:(i+pattern_length-1)], collapse = "")

      if (portion==pattern) {

        index[i]<-1
      } else {

        index[i]<-0
      }
    }

    metrical_score<-sum(index)/length(index)

    metrical_structure<-index
  }

  output<-list(metrical_score=metrical_score, metrical_structure=metrical_structure)

  class(output)<-c("meter_output", "list")

  return(output)

}
