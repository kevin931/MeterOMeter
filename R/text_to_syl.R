############

# This function implements syl_split function.
# Advantages of using this function over syl_split:
#   Support for multiple cores under unix-systemn.
#   Support for input as string or vector.

########################




text_to_syllables<-function(x, type=c("vector", "string"),
                            multi_core=F, core_used=NA) {

  require(tm)

  x<-tolower(x)

  if (multi_core) {

    require(parallel)

    if (type=="vector") {

      return(mclapply(x, syl_split))

    } else {

      x<-unlist(strsplit(x, " "))

      return(mclapply(x, syl_split))

    }

  } else {

    if (type=="vector") {

      return(lapply(x, syl_split))

    } else {

      x<-unlist(strsplit(x, " "))

      return(lapply(x, syl_split))

    }
  }
}
