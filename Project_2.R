##Part 1A
exponential <- function(x,k){
  sum <- 0
  while(k>=1){
    sum <- sum + (x)^k/factorial(k)
    k <- k - 1
  }
  return(sum+1)
}

exponential2 <- function(x,k){
  sum <- 0
  for(i in 1:k){
    sum <- sum + (x)^k/factorial(k)
    k <- k - 1
  }
  return(sum+1)
}


## Part 1B: Mean

sample_mean<-function(x){
  mean_c = numeric(0)
  for( i in 1:x){
    s = sum(x)
    l = length(x)
    mean_c[i] = s/l
  }
  return (mean_c)
}  
  

mean_fun<-function(x){
  mean_c = numeric(0)
  for( i in 1:nrow(x)){
    s = sum(x[,i], na.rm=TRUE)
    l = length(x[,i][is.na(x[,i]) == FALSE])
    mean_c[i] = s/l
  }
  return (mean_c)
}


x<- as.numeric(c(1:10))  
  
##Part1B: SD                                                    #ask this
sample_sd<- function(x){
  m<- sample_mean(x)
  for(i in 1:x){
    s2= sum((x[i] - m)^2)
    l2= length(x)-1
    sd2= sqrt(s2/l2)
  }
  return (sd2)
}


##Part1C: Confidence interval 

calculate_CI<- function(x, conf=k){
  N <- length(x)
  m<- sample_mean(x)
  degrees_freedom = N - 1
  alpha <- 1 - k
  t_score = qt(p=alpha/2, df=degrees_freedom, lower.tail=FALSE)
  CI= t_score*var(x)/N
  UB= (m+CI)
  LB= (m-CI)
  return= c(LB, UB)
}


confinterval <- function(x,conf=k){
  alpha = 1-k
  df = length(x)-1
  pos_confinterval = sample_mean(x) + qt((1 - alpha/2),df)*(var(x)/length(x))
  neg_confinterval = mean(x) - qt((1 - alpha/2),df)*(var(x)/length(x))
  
  cut_points <- c(neg_confinterval,pos_confinterval)
  return(cut_points)
}

    
##Part 2 
#1
new_rainfall <- na.omit(rainfall)                            

#2
new_rainfall$date <- paste(new_rainfall$year, new_rainfall$month, new_rainfall$day, sep="-") %>% ymd() %>% as.Date()

new_rainfall = select(new_rainfall, -month, -day)

#3
new_rainfall$city_name<- toupper(new_rainfall$city_name)

#4
df1 = merge(new_rainfall, temperature, on ='date')


####Part 4A
df2<- filter(df1, year >= 2014)


For(i in df2$city_name){
  for(i in df2$year){
    sm<- sample_mean(df2$rainfall)
    sd<- sample_sd(df2$rainfall)
    ci<- calculate_CI(df2$rainfall, 0.95)
    df2$lower_bound <- ci[[1]]
    df2$upper_bound <- ci[[2]]
    df2$mean<- sm
    df2$sd<- sd
  } 
}

df2$sm <-sample_mean(rainfall) %>%
  df2$sd <-sample_sd(rainfall) %>%
  df2$ci <-calculate_CI(rainfall, 0.95) 


##Part3A
df2<- filter(df1, year >= 2014)
q<- df2 %>% group_by(city_name, year) %>%
  summarise (max_temp = max(temperature, na.rm= TRUE), min_temp = min(temperature, na.rm = TRUE))

y <- cbind(q$max_temp, q$min_temp)  
ggplot(aes(x=q$year, y= y, group=1)) + geom_line(color="blue", "red")  + geom_point() + facet_grid(. ~ city_name) 


ggplot(aes(df2$year, max(df2$temperature)))
q+ geom_point()+ geom_line() + facet_grid(.~ city_name) 



%>% ggplot2::aes(x= df2$temp_type, y= df2$year, ~df2$city_name )


#Part3A (Alternative)

q1 <- filter(df1, year >= 2014) %>% group_by(city_name, date) %>%
  summarise (max_temp = max(temperature, na.rm= TRUE), min_temp = min(temperature, na.rm = TRUE))



q1 %>% ggplot (aes(x=date, y= max_temp, color= "Maximum temperature", group=1)) + geom_point() + geom_line(color="blue")  + geom_line(aes(y= min_temp, color= "Minimum temperature"))+ geom_point() + facet_grid(. ~ city_name)  



###Part 3B 

###Part 3B

hist_rainfall<- function(cy){
  for(cy in df1$city_name) {
  df1 %>% 
    filter(city_name == "cy")
      ggplot(aes(log(rainfall))) + 
      geom_histogram()
  }
}
hist_rainfall(PERTH)

```{r}
hist_rainfall<- function(yr){
  df1 %>% 
    filter(year== yr)
  summarise(rainfall, city_name, year)
}
    
hist_rainfall(SYDNEY, 2000)




hr<- function(cy,yr){
  
}




hist_rainfall<- function(city, year){
      print(city)
      print(year)
  df1 %>% 
        filter(city_name == city & year == year) %>% 
        ggplot(aes(log(rainfall))) + 
        geom_histogram()
    }
hist_rainfall("SYDNEY", "2019")