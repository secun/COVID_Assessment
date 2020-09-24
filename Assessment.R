#Analysis on US Data on COVID
#data definitions: https://covidtracking.com/about-data/data-definitions

#####Libraries
library(tidyverse) #contains readr
library(ggplot2)

#####Data load
setwd("C:/DATA/R model/00_Learning/00_COVID_US")
data.covid<-as.data.frame (read_csv('national-history.csv'))

#####Data transformation

#####Quick data visualization
skimr::skim(data.covid)
#inIcuCumulative  and hospitalizedCumulative have missing data which represents a 
#high percentage of the total observations. Imputation might be the 
#best solution, however, for this qualitative analysis won't move the needle

#####Question #1
#####Data analysis
#Set  reference date
key_date <- as.Date("2020-09-07")
slot <- 7

#Go backwards
b<-data.covid %>%
  filter(date ==  (key_date - as.difftime(slot, unit="days"))) %>%
  select(positive)

#Go forward
f<-data.covid %>%
  filter(date ==  (key_date + as.difftime(slot, unit="days"))) %>%
  select(positive)


paste("Taking the following date as reference ", key_date, "and going back and forward 7 days", 
      "the number of positives (confirmed plus probable) were respectively ",
        b, " and ", f)

#####Question #2
data.plot <- data.covid %>%
  select (date,positiveIncrease,hospitalizedIncrease,deathIncrease) %>%
  filter(date > as.Date("2020-03-16"))  %>%
  pivot_longer(names_to="type",
               cols=c("positiveIncrease",
                      "hospitalizedIncrease",
                      "deathIncrease"
                      )
               ) 
#
#
data.plot$type_g = factor(data.plot$type, levels=unique(data.plot$type))

ggplot(data = data.plot, mapping = aes(x = date,y=value)) +  
  geom_area(fill="blue")+
  facet_grid(type_g~.,scales="free_y",switch = "y")+
  scale_x_date(date_breaks="2 weeks")+
  theme(axis.text.x = element_text(angle = -45))+
  labs(y = "Indicators", 
       x ="Time ", 
       title =paste("Indicators of COVID pandemic in the US. in",Sys.Date()),
       caption = "Data source: https://covidtracking.com/data/download")


#####Question #3
#####Calculate deltas for UCI and onVentilator patients
data.plot <-data.covid
data.plot$inIcuIncrease <- data.covid$inIcuCumulative - lead (data.covid$inIcuCumulative,1)
data.plot$onVentilatorIncrease <- data.covid$onVentilatorCumulative - lead (data.covid$onVentilatorCumulative,1)
  
data.plot <- data.plot %>%
  select (date,
          positiveIncrease,
          hospitalizedIncrease,
          inIcuIncrease,
          onVentilatorIncrease,
          deathIncrease
          ) %>%
  filter(date > as.Date("2020-03-16"))  %>%
  pivot_longer(names_to="type",
               cols=c("positiveIncrease",
                      "hospitalizedIncrease",
                      "inIcuIncrease",
                      "onVentilatorIncrease",
                      "deathIncrease"
                      )
               ) 
#
#
data.plot$type_g = factor(data.plot$type, levels=unique(data.plot$type))

ggplot(data = data.plot, mapping = aes(x = date,y=value)) +  
  geom_area(fill="blue")+
  facet_grid(type_g~.,scales="free_y",switch = "y")+
  scale_x_date(date_breaks="2 weeks")+
  theme(axis.text.x = element_text(angle = -45))+
  labs(y = "Indicators", 
       x ="Time ", 
       title =paste("Indicators of COVID pandemic in the US. in",Sys.Date()),
       caption = "Data source: https://covidtracking.com/data/download")

