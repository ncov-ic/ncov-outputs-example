# code gist

library(dplyr)
library(magrittr)

library(ggplot2)
library(viridis)
library(ggpubr)
library(googlesheets4)


googlesheets4::sheets_deauth()
df_raw <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1jBbIdYObHrIWz4xaQRDXVFEQE-NbcqiL7DOjdSR39-o/edit?usp=sharing",
  sheet = "china_papers_preprints",
  col_types = "c", ## read everything as character, easier than having to guess excel's date format
  na = c("", "NA") ## sometimes people actually write NA rather than leaving a cell blank.
)

df <- janitor::clean_names(df_raw) %>% as.data.frame()

numeric_col_indices <- grep("numerator|denominator|size", names(df))
df[,numeric_col_indices] <-  sapply(numeric_col_indices, 
       FUN = function(x){
         df[,x] <- as.numeric(df[,x])
       })


# only want clinical papers
df %<>% dplyr::filter(manuscript_type=="CLINICAL")

#-----------------------------------------------------------------------------
# cleaning symptoms
# first we are interested in those reporting particular symptoms
df_symptom <- df %>% dplyr::filter(!is.na(symptom))

# start grouping things and get rid of capitals
df_symptom %<>% mutate(symptom = tolower(symptom),
                       comorbidity = tolower(comorbidity))

# ignore no-pneumonia and all for now 
df_symptom %<>% filter(symptom != "no pneumonia")

# now satrt grouping symptoms
symptom_meta <- read.csv("symptom_meta.csv", stringsAsFactors = FALSE)

df_symptom$grouped_symptoms <- symptom_meta$symptom_group[match(df_symptom$symptom, symptom_meta$symptom)]

comorbidity_meta <- read.csv("comorbidity_meta.csv", stringsAsFactors = FALSE)

df_symptom$grouped_comorbidity <- comorbidity_meta$comorbidity_group[match(df_symptom$comorbidity, 
                                                                    comorbidity_meta$comorbidity)]

#-----------------------------------------------------------------------------
# let the plot begin
df_symptom %>%
  filter(symptom != "all") %>%
  group_by(grouped_symptoms) %>%
  summarise(numerator = sum(symptom_numerator, na.rm = TRUE), 
            denominator = sum(denominator, na.rm = TRUE)) %>%
  mutate(proportion_symptom= numerator/sum(denominator))%>%

ggplot() + 
  geom_col(aes(x = reorder(grouped_symptoms, -proportion_symptom), 
                                  y = proportion_symptom,
                                  fill = proportion_symptom)) +
  theme_pubr() +
  scale_fill_viridis_c()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.position = "none") +
  labs(x = "Symptoms", y = "Proportion of all reported symptoms")

#-----------------------------------------------------------------------------
# plotting symptom occurrence by severity
df_symptom %>%
  filter(symptom != "all") %>%
  filter(severity_severe_non_severe %in% c("severe", "non-severe", "all"))%>%
  group_by(grouped_symptoms, severity_severe_non_severe) %>%
  summarise(numerator = sum(symptom_numerator, na.rm = TRUE), 
            denominator = sum(denominator, na.rm = TRUE)) %>%
  mutate(proportion_symptom= numerator/sum(denominator))%>%
  
  ggplot() + 
  geom_col(aes(x = reorder(grouped_symptoms, -proportion_symptom), 
               y = proportion_symptom,
               fill = grouped_symptoms)) +
  theme_pubr() +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.position = "none") +
  labs(x = "Symptoms", y = "Proportion of all reported symptoms") +
  facet_wrap(severity_severe_non_severe~.)

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# comorbidity
df_symptom %>%
  group_by(grouped_comorbidity) %>%
  summarise(numerator = sum(comorbidity_numerator, na.rm = TRUE), 
            denominator = sum(cohort_size, na.rm = TRUE)) %>%
  mutate(proportion_comorbidity= numerator/sum(denominator))%>%
  
  ggplot() + 
  geom_col(aes(x = reorder(grouped_comorbidity, -proportion_comorbidity), 
               y = proportion_comorbidity,
               fill = proportion_comorbidity)) +
  theme_pubr() +
  scale_fill_viridis_c()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.position = "none") +
  labs(x = "Comorbidity", y = "Proportion of all reported comorbidity")
