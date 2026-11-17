library("dplyr")
library("spellcheckr")

setwd("~/OneDrive/__MScBioinformatics/5_DataCleaning/_Assignment/")


# =================================== # 
# ====== Parse Data into a DF ======= #

# A list of the columns
params <- read.csv(list.files("Data/group13_data/Test",full.names = TRUE)[1],header = FALSE)[,1]

# Create an empty dataframe
df <- data.frame(row.names = params)


for (file in list.files("Data/group13_data/",full.names = TRUE)){
  
  record <- read.csv(file,header=FALSE,row.names = 1)
  df<- cbind(df,record)
}




## FINALLY we can start to actually do shit.

vals <- trimws(tolower(unlist(df[rownames(df)=="lacz_eye",])))
  
vals


for (val in vals){
  
  for (word in strsplit(val, split = " ")){
    correct(word, maxedit = 1,)
    
  }
}

vals_corrected <- correct(i, maxedit = 1,)


vals_corrected



clean <- trimws(tolower(vals))

# Split every value into words
words_list <- strsplit(clean, "\\s+")

# Correct each distinct word only once
uniq <- unique(unlist(words_list))
fixed <- vapply(uniq, function(w) correct(w, maxedit = 1), character(1))

# Map the corrections back and rejoin each value
corrected <- vapply(words_list,
                    function(w) paste(fixed[w], collapse = " "),
                    character(1))

sort(table(corrected), decreasing = TRUE)








