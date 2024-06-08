hw_definitions<-function(data){
  pct95_maxtemp = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct95_maxtemp = quantile(maxtemp, probs = 0.95))
  
  data <- left_join(data, pct95_maxtemp, by = ("patzip"))
  data$maxhw95 <- ifelse(data$maxtemp >= data$pct95_maxtemp, 1, 0)
  
  pct975_maxtemp = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct975_maxtemp = quantile(maxtemp, probs = 0.975))
  
  data <- left_join(data, pct975_maxtemp, by = ("patzip"))
  data$maxhw975 <- ifelse(data$maxtemp >= data$pct975_maxtemp, 1, 0)
  
  pct99_maxtemp = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct99_maxtemp = quantile(maxtemp, probs = 0.99))
  
  data <- left_join(data, pct99_maxtemp, by = ("patzip"))
  data$maxhw99 <- ifelse(data$maxtemp >= data$pct99_maxtemp, 1, 0)
  
  library(Hmisc)
  data <- data %>% 
    dplyr::group_by(patzip) %>% 
    mutate(lag.value = dplyr::lag(maxtemp, n = 1, default = NA))
  
  library(Hmisc)
  data <- data %>% 
    dplyr::group_by(patzip) %>% 
    mutate(lag.value_2 = dplyr::lag(maxtemp, n = 2, default = NA))
  
  #Heat Wave 99 
  data$max_hw99_2 = ifelse(data$maxtemp > data$pct99_maxtemp & data$lag.value >  data$pct99_maxtemp, 1, 0)
  data$max_hw975_2 = ifelse(data$maxtemp > data$pct975_maxtemp & data$lag.value >  data$pct975_maxtemp, 1, 0)
  data$max_hw95_2 = ifelse(data$maxtemp > data$pct95_maxtemp & data$lag.value >  data$pct95_maxtemp, 1, 0)
  
  
  data$max_hw99_3 = ifelse(data$maxtemp > data$pct99_maxtemp & data$lag.value >  data$pct99_maxtemp & data$lag.value_2 > data$pct99_maxtemp, 1, 0)
  data$max_hw975_3 = ifelse(data$maxtemp > data$pct975_maxtemp & data$lag.value >  data$pct975_maxtemp & data$lag.value_2 > data$pct975_maxtemp, 1, 0)
  data$max_hw95_3 = ifelse(data$maxtemp > data$pct95_maxtemp & data$lag.value >  data$pct95_maxtemp & data$lag.value_2 > data$pct95_maxtemp, 1, 0)
  
  
  #MIn Temp 
  pct95_mintemp = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct95_mintemp = quantile(mintemp, probs = 0.95))
  
  data <- left_join(data, pct95_mintemp, by = ("patzip"))
  data$minhw95 <- ifelse(data$mintemp >= data$pct95_mintemp, 1, 0)
  
  pct975_mintemp = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct975_mintemp = quantile(mintemp, probs = 0.975))
  
  data <- left_join(data, pct975_mintemp, by = ("patzip"))
  data$minhw975 <- ifelse(data$mintemp >= data$pct975_mintemp, 1, 0)
  
  pct99_mintemp = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct99_mintemp = quantile(mintemp, probs = 0.99))
  
  data <- left_join(data, pct99_mintemp, by = ("patzip"))
  data$minhw99 <- ifelse(data$mintemp >= data$pct99_mintemp, 1, 0)
  
  library(Hmisc)
  data <- data %>% 
    dplyr::group_by(patzip) %>% 
    mutate(lag.value = dplyr::lag(mintemp, n = 1, default = NA))
  
  library(Hmisc)
  data <- data %>% 
    dplyr::group_by(patzip) %>% 
    mutate(lag.value_2 = dplyr::lag(mintemp, n = 2, default = NA))
  
  #Heat Wave 99 
  data$min_hw99_2 = ifelse(data$mintemp > data$pct99_mintemp & data$lag.value >  data$pct99_mintemp, 1, 0)
  data$min_hw975_2 = ifelse(data$mintemp > data$pct975_mintemp & data$lag.value >  data$pct975_mintemp, 1, 0)
  data$min_hw95_2 = ifelse(data$mintemp > data$pct95_mintemp & data$lag.value >  data$pct95_mintemp, 1, 0)
  
  
  data$min_hw99_3 = ifelse(data$mintemp > data$pct99_mintemp & data$lag.value >  data$pct99_mintemp & data$lag.value_2 > data$pct99_mintemp, 1, 0)
  data$min_hw975_3 = ifelse(data$mintemp > data$pct975_mintemp & data$lag.value >  data$pct975_mintemp & data$lag.value_2 > data$pct975_mintemp, 1, 0)
  data$min_hw95_3 = ifelse(data$mintemp > data$pct95_mintemp & data$lag.value >  data$pct95_mintemp & data$lag.value_2 > data$pct95_mintemp, 1, 0)
  
  #Difference between max and min temp 
  data$diff_temp = data$maxtemp- data$mintemp
  pct05_difftemp = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct05_difftemp = quantile(diff_temp, probs = 0.05))
  
  data <- left_join(data, pct05_difftemp, by = ("patzip"))
  data$diff_hw05 <- ifelse(data$diff_temp <= data$pct05_difftemp, 1, 0)
  
  pct025_difftemp = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct025_difftemp = quantile(diff_temp, probs = 0.025))
  
  data <- left_join(data, pct025_difftemp, by = ("patzip"))
  data$diff_hw025 <- ifelse(data$diff_temp <= data$pct025_difftemp, 1, 0)
  
  pct01_difftemp = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct01_difftemp = quantile(diff_temp, probs = 0.01))
  
  data <- left_join(data, pct01_difftemp, by = ("patzip"))
  data$diff_hw01 <- ifelse(data$diff_temp <= data$pct01_difftemp, 1, 0)
  
  library(Hmisc)
  data <- data %>% 
    dplyr::group_by(patzip) %>% 
    mutate(lag.value = dplyr::lag(diff_temp, n = 1, default = NA))
  
  library(Hmisc)
  data <- data %>% 
    dplyr::group_by(patzip) %>% 
    mutate(lag.value_2 = dplyr::lag(diff_temp, n = 2, default = NA))
  
  #Heat Wave 99 
  data$diff_hw01_2 = ifelse(data$diff_temp < data$pct01_difftemp & data$lag.value <  data$pct01_difftemp, 1, 0)
  data$diff_hw025_2 = ifelse(data$diff_temp < data$pct025_difftemp & data$lag.value <  data$pct025_difftemp, 1, 0)
  data$diff_hw05_2 = ifelse(data$diff_temp < data$pct05_difftemp & data$lag.value <  data$pct05_difftemp, 1, 0)
  
  
  data$diff_hw01_3 = ifelse(data$diff_temp < data$pct01_difftemp & data$lag.value <  data$pct01_difftemp & data$lag.value_2 < data$pct01_difftemp, 1, 0)
  data$diff_hw025_3 = ifelse(data$diff_temp < data$pct025_difftemp & data$lag.value <  data$pct025_difftemp & data$lag.value_2 < data$pct025_difftemp, 1, 0)
  data$diff_hw05_3 = ifelse(data$diff_temp < data$pct05_difftemp & data$lag.value <  data$pct05_difftemp & data$lag.value_2 < data$pct05_difftemp, 1, 0)
  
  #Heat Index
  pct95_hi = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct95_hi = quantile(data$heatIndex, probs = 0.95))
  
  data <- left_join(data, pct95_hi, by = ("patzip"))
  data$hi95 <- ifelse(data$heatIndex >= data$pct95_hi, 1, 0)
  
  pct975_hi = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct975_hi = quantile(maxtemp, probs = 0.975))
  
  data <- left_join(data, pct975_hi, by = ("patzip"))
  data$hi975 <- ifelse(data$heatIndex >= data$pct975_hi, 1, 0)
  
  pct99_hi = data %>% 
    dplyr::group_by(patzip) %>% 
    dplyr::summarize(pct99_hi = quantile(maxtemp, probs = 0.99))
  
  data <- left_join(data, pct99_hi, by = ("patzip"))
  data$hi99 <- ifelse(data$heatIndex >= data$pct99_hi, 1, 0)
  
  return(data)
}
  
  
