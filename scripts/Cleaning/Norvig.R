





library(spellcheckr)  #  <- More information at https://www.rdocumentation.org/packages/spellcheckr/versions/0.1.2 
# ^^ this library provides an implementation of Peter Norvig's spell check algorithm (more info at: https://www.norvig.com/spell-correct.html)

# Norvig's algorithm is a bit slow, and can be pointless if it faces a 
# mispelling it has no hope of fixing. 
# for this reason, I made this function to speed it up for my data cleaning.

# takes as input d - a vector of strings containing the allowable words.
norvig <- function(x,d){
  # We first check if the word is in our dictionary, if it is then we dont bother checking it.

  if (x %in% d){return(x)}
  # Check the Hamming distance between each item in the dictionary and the word,
  # if it's more than 4 then Norvig's algorithm probably wont work anyway. 
  # if it's less than 4 give norvig's algorithm a chance to fix it
  
  return(correct(wd=x,maxedit = 3,dictionary=d))
}
