#in future set symptoms as arguments
# add age arrow

make_progressionmap <- function(df){
  df_times <- df
  
  #get symptoms
  df_times %<>% add_first_symptom("pneumonia") %>% mutate(time_Pneumonia = time_first)
  df_times %<>% add_first_symptom("fever") %>% mutate(time_Fever = time_first)
  df_times %<>% add_first_symptom("respiratory symptoms") %>% 
    mutate(time_Respiratory = time_first)
  df_times %<>% add_first_symptom("diarrhea") %>% 
    mutate(time_Diarrhea = time_first)
  df_times %<>% add_first_symptom("myalgia or fatigue") %>% 
    mutate(time_Fatigue = time_first)
  
  #add times to usual events
  df_times %<>% mutate(time_Hospitalised = date_hospitalised - date_onset,
                       time_Recovered = date_recovered - date_onset,
                       time_Death = date_death - date_onset)
  
  #select only times
  df_times %<>% select(c(starts_with("time_"), new_id, age))
  
  #remove the last time_first
  df_times %<>% select(-time_first)
  
  #colours
  cols = c("#fb5600",
           "#00e87e",
           "#a803ce",
           "#d2c100",
           "#017bcd",
           "#ff6e75",
           "#81ffed",
           "#710025",
           "#101900")
  
  # old chaincheckcer code
  g = ggplot(df_times, aes(text = paste0("ID: ",new_id))) +   
    geom_rect(aes(xmin = 0,
                  xmax = time_Recovered,
                  ymin = reorder(new_id, -as.numeric(age)), 
                  ymax = reorder(new_id, -as.numeric(age)),
                  color = "Recovered"),
              size = 1.1) +
    geom_rect(aes(xmin = 0,
                  xmax = time_Death,
                  ymin = reorder(new_id, -as.numeric(age)), 
                  ymax = reorder(new_id, -as.numeric(age)),
                  color = "Death"),
              size = 1.1) +
    
    geom_point(aes(x = time_Recovered,
                   y = reorder(new_id, -as.numeric(age)),
                   color = "Recovered"),
               size = 2, shape = "square", stroke = 2)+
    
    geom_point(aes(x = time_Death,
                   y = reorder(new_id, -as.numeric(age)),
                   color = "Death"),
               size = 2, shape = "square", stroke = 2)+
    
    
    geom_point(aes(x = time_Hospitalised,
                   y = reorder(new_id, -as.numeric(age)),
                   color = "Hospitalised"),
               size = 2, shape = 3, stroke = 2)+
    
    geom_point(aes(x = time_Pneumonia,
                   y = reorder(new_id, -as.numeric(age)),
                   color = "Pneumonia"),
               size = 3) +
    geom_point(aes(x = time_Fever,
                   y = reorder(new_id, -as.numeric(age)),
                   color = "Fever"),
               size = 3) +
    geom_point(aes(x = time_Respiratory,
                   y = reorder(new_id, -as.numeric(age)),
                   color = "Respiratory symptoms"),
               size = 3) +
    geom_point(aes(x = time_Diarrhea,
                   y = reorder(new_id, -as.numeric(age)),
                   color = "Diarrhea"),
               size = 3) +
    geom_point(aes(x = time_Fatigue,
                   y = reorder(new_id, -as.numeric(age)),
                   color = "Fatigue"),
               size = 3) +
    
    labs(color = "Event",
         shape = NULL,
         stroke = NULL,
         size = NULL, 
         fill = NULL)+
    theme_pubr(legend = "bottom")+
    grids(linetype = "dashed") +
    guides(colour = guide_legend(nrow=3))+
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = 20))+
    ylab("")+xlab("Time from onset to event") + xlim(0, NA) +
    scale_colour_manual(values = cols)
  
  return(g)
}