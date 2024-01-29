
#libraries
c("ggplot2","quanteda","tidyverse", "readtext")
library(ggplot2)
library(quanteda)
library(quanteda.textstats)
library(tidyverse)
library(dplyr)
library(readtext)
library("quanteda.textstats")


#read selected pdfs
selection_pdf <- readtext("./selectedtranscripts/*.pdf", docvarsfrom = "filenames")
saveRDS(selection_pdf, file = "selection_pdf.rds")
selection_pdf <- readRDS("selection_pdf.rds")


#read all sample pdfs
all_pdf <- readtext("./sampletranscripts/*.pdf", docvarsfrom = "filenames")
colnames(all_pdf)
corp_ex_text <- corpus(all_pdf, text_field = "text")

#create tokens
toks <- tokens(corp_ex_text, remove_punct = TRUE, remove_numbers = TRUE)
toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove")
saveRDS(toks_nostop, file = "sampletokensnostp.rds")

#unigram
toks_nostop <- readRDS("Sampletokensnostp.rds")
toks_ngram <- tokens_ngrams(toks_nostop, n=1)
remove(toks_nostop)
unigram_df <- dfm(toks_ngram)
remove(toks_ngram)
top_50_ngram <- textstat_frequency(unigram_df, n = 50)
remove(unigram_df)
saveRDS(top_50_ngram, file = "fd_unigram.rds")
#remove()

#bigram 
toks_nostop <- readRDS("Sampletokensnostp.rds")
toks_ngram <- tokens_ngrams(toks_nostop, n=2)
remove(toks_nostop)
bigram_df <- dfm(toks_ngram)
remove(toks_ngram)
top_50_ngram <- textstat_frequency(bigram_df, n = 50)
saveRDS(top_50_ngram, file = "fd_bigram.rds")
remove(bigram_df)

#trigram
toks_nostop <- readRDS("Sampletokensnostp.rds")
toks_ngram <- tokens_ngrams(toks_nostop, n=3)
remove(toks_nostop)
trigram_df <- dfm(toks_ngram)
remove(toks_ngram)
top_50_ngram <- textstat_frequency(trigram_df, n = 50)
saveRDS(top_50_ngram, file = "fd_trigram.rds")
remove(trigram_df)


### Creating aggregate n-gram df 
unigram <- readRDS("fd_unigram.rds")
bigram <- readRDS("fd_bigram.rds")
trigram <- readRDS("fd_trigram.rds")
ngram_df<- data.frame(rank = unigram$rank, unigram$feature, bigram$feature, trigram$feature)
saveRDS(ngram_df,"ngram50.rds")


##inflation##
infl_t <- c("inflation", "price*", "expenditures")

toks_inside <- tokens_keep(toks_nostop, pattern = infl_t, window = 10)
toks_inside <- tokens_remove(toks_inside, pattern = infl_t) # remove the keywords
toks_outside <- tokens_remove(toks_nostop, pattern = infl_t, window = 10)

dfmat_inside <- dfm(toks_inside)
dfmat_outside <- dfm(toks_outside)

tstat_key_inside <- textstat_keyness(rbind(dfmat_inside, dfmat_outside), 
                                     target = seq_len(ndoc(dfmat_inside)), measure = "lr")
infl_cont <- data.frame(tstat_key_inside[1:20,])



###unemployment##
employment_t <- c("employment", "unemployment", "job*")
toks_inside <- tokens_keep(toks_nostop, pattern = employment_t, window = 10)
toks_inside <- tokens_remove(toks_inside, pattern = employment_t) # remove the keywords
toks_outside <- tokens_remove(toks_nostop, pattern = employment_t, window = 10)
dfmat_inside <- dfm(toks_inside)
dfmat_outside <- dfm(toks_outside)
tstat_key_inside <- textstat_keyness(rbind(dfmat_inside, dfmat_outside), 
                                     target = seq_len(ndoc(dfmat_inside)), measure = "lr")
unem_cont <- data.frame(tstat_key_inside[1:20,])


###interest rate##
interest_t <- c("federal", "funds", "rate")
toks_inside <- tokens_keep(toks_nostop, pattern = interest_t, window = 10)
toks_inside <- tokens_remove(toks_inside, pattern = interest_t) # remove the keywords
toks_outside <- tokens_remove(toks_nostop, pattern = interest_t, window = 10)
dfmat_inside <- dfm(toks_inside)
dfmat_outside <- dfm(toks_outside)
tstat_key_inside <- textstat_keyness(rbind(dfmat_inside, dfmat_outside), 
                                     target = seq_len(ndoc(dfmat_inside)), measure = "lr")
ffr_cont <- data.frame(tstat_key_inside[1:20,])

list_objectives <- list(inflation = infl_cont, unemployment = unem_cont, ffr = ffr_cont)
saveRDS(list_objectives, "list_objectives.rds")


