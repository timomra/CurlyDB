library(stringdist)


fix_insertions <-  function(incorrect_string,allowed_strings){
  # Given most of the fields have insertions and not deletions, I wrote this 
  # Custom function which uses Levenshtein distance to identify closest possible 
  # allowed word.
  
  # However, this is ultimately a heuristic solution, and while it seems to 
  # perform well on this data, it's unclear how good it would be in future.
  
  
  # As this is slower, I did it after removing all the characters I know shouldn't 
  # be in the strings (Like special ones)
  
  # First get the Levenshtein distances between all words and our incorrect word.
  # I customised the function to account for what the mistakes look like.
  levdists <- unlist(lapply(allowed_strings, stringdist, method="lv",b=incorrect_string, 
                weight=c(
                  0.5, # <- given it's usually inserted characters, I made it less costly to delete characters than to add them,
                  1, # <- addition
                  1, # <- substitution
                  1  # <- transposition
                )))
  
  
  # Identify which allowed word has the lowest Levenshtein distance to our incorrect word
  min_dist <- levdists[which.min(levdists)]
  
  # Get the number of values which have this "lowest" Levenshtein distance. 
  # If there any more than 1, just return back the original value as we cant be sure which was intended.
  no_best_matches <- sum(levdists == min_dist)  
  # if the minimum distance is too large, i.e., 3 or more, then just return the incorrect word.
  if (min_dist >=3){
    return(incorrect_string)
  }
  else if (no_best_matches == 1){
    return(allowed_strings[which.min(levdists)])
  }
  else{
    return(incorrect_string)
  }
  
}


# Example of the function in action:
# This returns a correction as it has a pretty obvious intended  spelling.
fix_insertions("E1.8.5",allowed_lifestage)


# These have no obvious intention... therefore we just return them
fix_insertions("",allowed_lifestage)
fix_insertions("E1",allowed_lifestage)


