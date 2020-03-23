
get_symptoms <- function(df){
  
  symptoms_freq <- unlist(df$symptoms_at_onset, use.names = FALSE)
  symptoms_freq <- as.data.frame(table(symptoms_freq))[, 1:2]
  names(symptoms_freq) <- c("symptom", "freq_at_onset")
  
  tmp <- unlist(df$symp_prog1, use.names = FALSE)
  tmp <- as.data.frame(table(tmp))
  names(tmp) <- c("symptom", "freq_at_prog1")
  symptoms_freq %<>% left_join(tmp)
  
  tmp <- unlist(df$symp_prog2, use.names = FALSE)
  tmp <- as.data.frame(table(tmp))
  names(tmp) <- c("symptom", "freq_at_prog2")
  symptoms_freq %<>% full_join(tmp)
  
  tmp <- unlist(df$symp_prog3, use.names = FALSE)
  tmp <- as.data.frame(table(tmp))
  names(tmp) <- c("symptom", "freq_at_prog3")
  symptoms_freq %<>% full_join(tmp)
  
  tmp <- unlist(df$symp_prog4, use.names = FALSE)
  tmp <- as.data.frame(table(tmp))
  names(tmp) <- c("symptom", "freq_at_prog4")
  symptoms_freq %<>% full_join(tmp)
  
  
  ### RECODE SYMPTOMS ###
  unique(symptoms_freq$symptom)
  symptoms_freq$symptom[grep("mild pneumonia|^pneumonia|image positive|positive lung image", 
                             symptoms_freq$symptom)] = "pneumonia"
  symptoms_freq$symptom[grep("chill", symptoms_freq$symptom)] = "chill"
  symptoms_freq$symptom[grep("breathing difficulties", symptoms_freq$symptom)] = "breathing difficulties"
  symptoms_freq$symptom[grep("sore throat", symptoms_freq$symptom)] = "sore throat"
  symptoms_freq$symptom[grep("cough", symptoms_freq$symptom)] = "cough"
  symptoms_freq$symptom[grep("fatigue", symptoms_freq$symptom)] = "fatigue"
  #######################
  
  symptoms_freq %<>% 
    group_by(symptom) %>%
    summarise(freq_at_onset = sum(freq_at_onset, na.rm = T),
              freq_at_prog1 = sum(freq_at_prog1, na.rm = T),
              freq_at_prog2 = sum(freq_at_prog2, na.rm = T),
              freq_at_prog3 = sum(freq_at_prog3, na.rm = T),
              freq_at_prog4 = sum(freq_at_prog4, na.rm = T))
  
  symptoms_freq %<>% filter(!symptom %in% 
                              c("stable", 
                                "severe", 
                                "no symptoms", 
                                "no pneumonia", 
                                "mild",
                                "worsening prognosis") )
  
  
  symptoms_freq <- rbind(symptoms_freq, data.frame(symptom = "Total reported",
                                                   freq_at_onset = NA,
                                                   freq_at_prog1 = NA,
                                                   freq_at_prog2 = NA,
                                                   freq_at_prog3 = NA,
                                                   freq_at_prog4 = NA))
  for(i in 2:ncol(symptoms_freq)){
    total_symp<- sum(symptoms_freq[,i], na.rm = T)
    symptoms_freq[,i] <- symptoms_freq[,i]/total_symp
    symptoms_freq[nrow(symptoms_freq), i] <- total_symp
  }
  
  
  symptoms_df <- symptoms_freq
  
  symptoms_freq %<>% tidyr::gather(Progression, Frequency, -symptom)
  
  g <- symptoms_freq %>% 
    filter(symptom != "Total reported") %>%
    filter(!is.na(Frequency)) %>%
    mutate(Frequency = as.numeric(Frequency)) %>%
    ggplot() + 
    geom_col(aes(x=reorder(symptom, -Frequency), y = Frequency, fill = symptom))+
    theme_pubr()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), legend.position = "none")+
    facet_wrap(Progression~.)+
    ylab("Proportion") +
    xlab("Symptom")
  
  return(list(g = g, 
              symtoms_df = symptoms_df, 
              symptoms_freq = symptoms_freq))
  
}