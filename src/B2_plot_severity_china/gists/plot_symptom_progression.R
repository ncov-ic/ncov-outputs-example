#plot_symptom_progression

library(dplyr)
library(magrittr)

library(ggplot2)
library(viridis)
library(ggpubr)
library(googlesheets4)
library(lubridate)

googlesheets4::sheets_deauth()
df_raw <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1jBbIdYObHrIWz4xaQRDXVFEQE-NbcqiL7DOjdSR39-o/edit?usp=sharing",
  sheet = "country_case_studies",
  col_types = "c", ## read everything as character, easier than having to guess excel's date format
  na = c("", "NA") ## sometimes people actually write NA rather than leaving a cell blank.
)

df <- janitor::clean_names(df_raw) %>% as.data.frame()

df %<>% mutate(over_60 = ifelse(age>59, 1,0) )

#----------------------------------------------------------
# fix dates etc.
date_col_indices <- grep("date", names(df))
for(x in date_col_indices){
  df[,x] <- lubridate::dmy(df[,x], quiet = TRUE)
}

#----------------------------------------------------------
#plot times from date_onset to date_symp_prog1
df %>% 
  mutate(time_from_onset_progression = date_symp_prog1-date_onset) %>%
  filter(time_from_onset_progression>0) %>%
  ggplot() + 
  geom_density(aes(time_from_onset_progression))+
  ggpubr::theme_pubr() +
  xlim(0, 14)

sum(!is.na(df %>% 
  mutate(time_from_onset_progression = date_symp_prog1-date_onset) %>%
  filter(time_from_onset_progression>0) %>% select(time_from_onset_progression)))

#----------------------------------------------------------
# look at symptoms
symptom_col_indices <- grep("^symptoms_at_onset|^symp_prog1|^symp_prog2|^symp_prog3|^symp_prog4",
                            names(df))

df %<>% mutate_at(names(df)[symptom_col_indices],
                  strsplit, split = ";")


out <- get_symptoms(df)

#----------------------------------------------------------
# symptoms for just older/ younger
out <- get_symptoms(df %>% filter(over_60==1))

out$g

out <- get_symptoms(df %>% filter(over_60==0))

out$g

#----------------------------------------------------------
# time to first pneumonia
get_first_pneumonia <- function(dat){
  
  ind <- grep("\\bpneumonia\\b", dat)[1]
  names(dat)[ind]
}

df$name_first_pneumonia <- apply(df, 1, get_first_pneumonia)

df %<>% mutate(name_first_pneumonia = ifelse(is.na(name_first_pneumonia),
                                             NA,
                                             paste0("date_", name_first_pneumonia)))

df$date_first_pneumonia <- NA
for(i in 1:nrow(df)){
  if(df$name_first_pneumonia[i] != "date_NA" & 
     df$name_first_pneumonia[i] != "date_symptoms_full" &
     df$name_first_pneumonia[i] != "date_symptoms_at_onset"){
    df$date_first_pneumonia[i] <- df[i,df$name_first_pneumonia[i]]
  } else if(df$name_first_pneumonia[i] == "date_symptoms_at_onset"){
    df$date_first_pneumonia[i] <- df$date_onset[i]
  }
}

df %<>% mutate(time_first_pneumonia = dmy(date_first_pneumonia) - dmy(date_onset) )

ggplot(df) + geom_histogram(aes(time_first_pneumonia), fill = "cyan") + 
  xlim(0, 20) +
  theme_pubr()+
  xlab("Time from onset of symptoms to pneumonia") 

sum(df$time_first_pneumonia>0 & df$time_first_pneumonia<20, na.rm = T)


#----------------------------------------------------------
# what proportion of all pneumonia at is at onset
df_pneumonia <- df %>% 
  filter(!is.na(name_first_pneumonia)) %>% 
  filter(name_first_pneumonia != "date_symptoms_full")

table(df_pneumonia$name_first_pneumonia)

nrow(df_pneumonia)

### ------------------------------------------------------
# number reporting any symptoms
df %<>% mutate(symptom_reported = NA)

for(i in 1:nrow(df)){
  df$symptom_reported[i] = all(is.na(df[i, symptom_col_indices]))
}

sum(df$symptom_reported)
