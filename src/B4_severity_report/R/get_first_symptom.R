get_first_symptom <- function(dat, symptom){
  
  ind <- grep(paste0("\\b",symptom,"\\b"), dat)[1]
  names(dat)[ind]
}

#--------------------------------------------------------------------------------#
add_first_symptom <- function(df, symptom){
  df$name_first <- apply(df, 1, FUN = function(x)get_first_symptom(x, symptom ))
  
  df %<>% mutate(name_first = ifelse(is.na(name_first),
                                     NA,
                                     paste0("date_", name_first)))
  
  df$date_first <- ymd(NA)
  
  for(i in 1:nrow(df)){
    if(!is.na(df$name_first[i]) & 
       df$name_first[i] != "date_symptoms_full" &
       df$name_first[i] != "date_symptoms_at_onset"){
      
      df$date_first[i] <- df[i,df$name_first[i]]
      
    } else if(is.na(df$name_first[i])){
      
    } else if(df$name_first[i] == "date_symptoms_at_onset"){
      df$date_first[i] <- df$date_onset[i]
    }
  }
  
  df %<>% mutate(time_first = ymd(date_first) - ymd(date_onset) )
  
  return(df)
}

#--------------------------------------------------------------------------------#
get_mean_onset_to_symptom <- function(df, symptom){
  
  df <- add_first_symptom(df, symptom)
  
  return(list(mean_out = mean(df$time_first, na.rm = TRUE),
              sd_out = sd(df$time_first, na.rm = TRUE)))
}
