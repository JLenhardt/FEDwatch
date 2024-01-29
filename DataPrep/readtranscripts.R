
library(readtext)
library(dplyr)



## function remove pattern
rem_pat <- function(x, pat) {
  i = 1
  for( i in 1:length(pat) ){
    x <- gsub(pat[i], "", x) 
    
  }
  x
}

## create dataframe 

path_dir = "./selectedtranscripts" 
files_path <- choose.dir(default = path_dir , caption = "Choose files") 
fomclist_path = list.files(path = files_path, full.names = TRUE)
text_df <- readtext(fomclist_path, docvarsfrom = "filepaths")
transc_label <- data.frame(end_date = as.Date(gsub("\\D", "", text_df$doc_id), "%Y%m%d"),  type = rem_pat(text_df$doc_id, c("\\d", "FOMC", ".pdf")) ) # labeling with date and transcript type 
transcript_df <- cbind(text_df, transc_label) %>% rename(filepath = docvar1)
saveRDS(transcript_df, "transcriptdf.rds")



