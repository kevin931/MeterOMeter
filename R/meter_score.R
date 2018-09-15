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
                       secondary_stress=T
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

  return(metrical_score)

}
