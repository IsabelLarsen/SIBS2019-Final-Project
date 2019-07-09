library(dplyr)
library(ggplot2)
library(gridExtra)
library(gmodels)

dc <- drugConsumptionUKUS


# Plots by Country % ------------------------------------------------------
#Alcohol
  #USA
    usaAlc <- dc %>% 
      filter(Country=="USA") %>%    
      ggplot(aes(Alcohol, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
             geom_bar(aes(y=(..count..)/sum(..count..))) +
      labs(title="USA Alcohol Consumption", x="Last Alcohol Consumption Date", y="Percent of Responses") +
      geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
      scale_y_continuous(labels = scales::percent)
  #UK
    ukAlc <- dc %>% 
      filter(Country=="UK") %>%    
      ggplot(aes(Alcohol, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
      geom_bar(aes(y=(..count..)/sum(..count..))) +
      labs(title="UK Alcohol Consumption", x="Last Alcohol Consumption Date", y="Percent of Responses") +
      geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
      scale_y_continuous(labels = scales::percent)
  grid.arrange(usaAlc, ukAlc, nrow=1, top="Alcohol Consumption: USA v.s UK")
  
  CrossTable(dc$Alcohol, dc$Country)
  
#Nicotine
  #USA
    usaNic <- dc %>% 
      filter(Country=="USA") %>%    
      ggplot(aes(Nicotine, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
      geom_bar(aes(y=(..count..)/sum(..count..))) +
      labs(title="USA Nicotine Consumption", x="Last Nicotine Consumption Date", y="Percent of Responses") +
      geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
      scale_y_continuous(labels = scales::percent)
  #UK
    ukNic <- dc %>% 
      filter(Country=="UK") %>%    
      ggplot(aes(Nicotine, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
      geom_bar(aes(y=(..count..)/sum(..count..))) +
      labs(title="UK Nicotine Consumption", x="Last Nicotine Consumption Date", y="Percent of Responses") +
      geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
      scale_y_continuous(labels = scales::percent)
  grid.arrange(usaNic, ukNic, nrow=1, top="Nicotine Consumption: USA v.s UK")
  
  CrossTable(dc$Nicotine, dc$Country)
  
#Cannabis
  #USA
    usaCan <- dc %>% 
      filter(Country=="USA") %>%    
      ggplot(aes(Cannabis, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
      geom_bar(aes(y=(..count..)/sum(..count..))) +
      labs(title="USA Cannabis Consumption", x="Last Cannabis Consumption Date", y="Percent of Responses") +
      geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
      scale_y_continuous(labels = scales::percent)
  #UK
    ukCan <- dc %>% 
      filter(Country=="UK") %>%    
      ggplot(aes(Cannabis, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
      geom_bar(aes(y=(..count..)/sum(..count..))) +
      labs(title="UK Cannabis Consumption", x="Last Cannabis Consumption Date", y="Percent of Responses") +
      geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
       scale_y_continuous(labels = scales::percent)
  grid.arrange(usaCan, ukCan, nrow=1, top="Cannabis Consumption: USA v.s UK")
  
  CrossTable(dc$Cannabis, dc$Country)
  
  