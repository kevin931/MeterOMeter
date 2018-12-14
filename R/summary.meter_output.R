summary.meter_output<-function(x){

  meter_length<-length(x$metrical_structure)

  meterical_ins<-length(which(x$metrical_structure==1))

  writeLines("\nThe metrical score is:\n")

  print(x$metrical_score)

  writeLines(paste("\nTotal number of instances is ", meter_length, ".\n", sep = ""))

  writeLines(paste("Number of metrical instances is ", meterical_ins, ".", sep=""))

}
