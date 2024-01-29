

#libraries  
library(haven)
library(dplyr)
library(readxl) #for reading excel data
library(pracma)


#load data 
stata <- read_dta("FinalAnalysisData.dta")
full_data <- stata %>% zap_formats() %>% zap_label()   #Remove labels and stata formats 
saveRDS(full_data, "full_data.rds")

### Data for chapter 2 ##################################################################

context_wide = full_data %>% filter(type == "meeting" & year >= 2000 ) %>% select(date,grgdpf0, gppcef0, urate, ffr ) %>% dplyr::distinct()
saveRDS(context_wide, "context_wide.rds")




#### Data for chapter 6.1 ################################################################

#List of variables for chapter
var_list_6_1 <- c("type", "date", "transcript", "time", "t_Neg", "t_Pos", "t_total", "t_LMe", "gppcexf0", "gppcexf01", "gppcexf01_SQ", "urate", "ugapF0", "ugapF01", "unempf0", "nairuF0", "ugapF01_SQ", "grgdpf01", "grgdpf01_SQ") 

#Desc: remove observations without LM scores, limit to FOMC meetings, aggregate on transcript (identical to grouping by time if both filtered for meeting)
chapter6_1_dat <- workset %>%   filter(!is.na(LM_Neg_Count_econ | LM_Pos_Count_econ | total_count_econ ) & type == "meeting") %>% group_by(transcript) %>% 
  summarise( t_Neg = sum(LM_Neg_Count_econ), t_Pos = sum(LM_Pos_Count_econ), t_total = sum(total_count_econ)) %>% 
  mutate(t_LMe = ((t_Neg- t_Pos)/t_total)*100 ) %>% 
  left_join(workset, by="transcript", multiple= "all") %>%
#Desc:  select relevant data, remove duplicates
select(var_list_6_1 ) %>% dplyr::distinct()
  
saveRDS(chapter6_1_dat, "c61_data.rds")

#### FRED Employment data ################################################################

fred_empl <- read_excel("./FRED/employment_ind.xlsx")
saveRDS(fred_empl, "fred_empl.rds")





#### Chapter methods ###########

mat_dat <- full_data %>% select(date, speakerstr,time, type, Flag_ChairGovPres, LM_NegPos_Lex_econ, gppcexf01, gppcexf01_SQ) %>%
  filter(type == "meeting" & time >= 481 & time <= 623 & Flag_ChairGovPres ==1)  %>%
  rename(LMe = LM_NegPos_Lex_econ) %>% fastDummies::dummy_cols(. , select_columns = "speakerstr")

#X <- mat_dat %>% select(-type, -date, -time, -speakerstr, -Flag_ChairGovPres, -LMe) %>% data.matrix(.) 
#Y <- mat_dat$LMe
#betas <- inv(t(X) %*% X) %*% t(X) %*% Y
#betas <- round(betas, 3)
#cond(X)

saveRDS(mat_dat, "matdat.rds")

