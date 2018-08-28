############

# This script is to tag POS, a functionality used to train various models.
# This script is found on stack overflow, citation needed.
# https://stackoverflow.com/questions/28764056/could-not-find-function-tagpos

############



tagPOS <-  function(x, ...) {

  require(NLP)

  require(openNLP)

  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)

}


