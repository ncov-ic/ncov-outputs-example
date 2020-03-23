get_symptoms <- function(df, symptom_meta){
  
  if("symptoms_at_onset" %in% names(df) &
     "symp_prog1" %in% names(df) &
     "symp_prog2" %in% names(df) &
     "symp_prog3" %in% names(df) &
     "symp_prog4" %in% names(df) &
     "any_symptom" %in% names(df)){
    
    #table all symptoms in each progression
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
    
    tmp <- unlist(df$any_symptom, use.names = FALSE)
    tmp <- as.data.frame(table(tmp))
    if(nrow(tmp)>0){
      names(tmp) <- c("symptom", "freq_at_any")
    } else {
      tmp <- data.frame(symptom = as.character(NA),
                        freq_at_any = as.numeric(0))
    }
    symptoms_freq %<>% full_join(tmp) 
    

    return(symptoms_freq)
    
  } else {
    stop("There are symptom names missing that get_symptoms expects.")
  }
}