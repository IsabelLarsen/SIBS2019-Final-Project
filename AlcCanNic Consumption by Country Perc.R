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
  
  #With Gender Distributions
  usaAlcGen <- dc %>% 
    filter(Country=="USA") %>%    
    ggplot(aes(Alcohol, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Gender) +
    labs(title="USA Alcohol Consumption", x="Last Alcohol Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  ukAlcGen <- dc %>% 
    filter(Country=="UK") %>%    
    ggplot(aes(Alcohol, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Gender) +
    labs(title="UK Alcohol Consumption", x="Last Alcohol Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  grid.arrange(usaAlcGen, ukAlcGen, nrow=2, top="Alcohol Consumption: USA v.s UK by Gender")
  
  #With Gender distributions
  usaAlcAge <- dc %>% 
    filter(Country=="USA") %>%    
    ggplot(aes(Alcohol, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Age) +
    labs(title="USA Alcohol Consumption", x="Last Alcohol Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  #UK
  ukAlcAge <- dc %>% 
    filter(Country=="UK") %>%    
    ggplot(aes(Alcohol, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Age) +
    labs(title="UK Alcohol Consumption", x="Last Alcohol Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  grid.arrange(usaAlcAge, ukAlcAge, nrow=2, top="Alcohol Consumption: USA v.s UK by Age")
  
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
  
  #Gender Distributions
  #USA
  usaNicGen <- dc %>% 
    filter(Country=="USA") %>%    
    ggplot(aes(Nicotine, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Gender) +
    labs(title="USA Nicotine Consumption", x="Last Nicotine Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  #UK
  ukNicGen <- dc %>% 
    filter(Country=="UK") %>%    
    ggplot(aes(Nicotine, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Gender) +
    labs(title="UK Nicotine Consumption", x="Last Nicotine Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  grid.arrange(usaNicGen, ukNicGen, nrow=2, top="Nicotine Consumption: USA v.s UK by Gender")
  
  #Agw Distributions
  #USA
  usaNicAge <- dc %>% 
    filter(Country=="USA") %>%    
    ggplot(aes(Nicotine, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Age) +
    labs(title="USA Nicotine Consumption", x="Last Nicotine Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  #UK
  ukNicAge <- dc %>% 
    filter(Country=="UK") %>%    
    ggplot(aes(Nicotine, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Age) +
    labs(title="UK Nicotine Consumption", x="Last Nicotine Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  grid.arrange(usaNicAge, ukNicAge, nrow=2, top="Nicotine Consumption: USA v.s UK by Age")
  
  
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
  
  #Gender distributions
  #USA
  usaCanGen <- dc %>% 
    filter(Country=="USA") %>%    
    ggplot(aes(Cannabis, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Gender) +
    labs(title="USA Cannabis Consumption", x="Last Cannabis Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  #UK
  ukCanGen <- dc %>% 
    filter(Country=="UK") %>%    
    ggplot(aes(Cannabis, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Gender) +
    labs(title="UK Cannabis Consumption", x="Last Cannabis Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  grid.arrange(usaCanGen, ukCanGen, nrow=2, top="Cannabis Consumption: USA v.s UK by Gender")
  
  #Age distributions
  #USA
  usaCanAge <- dc %>% 
    filter(Country=="USA") %>%    
    ggplot(aes(Cannabis, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Age) +
    labs(title="USA Cannabis Consumption", x="Last Cannabis Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  #UK
  ukCanAge <- dc %>% 
    filter(Country=="UK") %>%    
    ggplot(aes(Cannabis, prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
    geom_bar(aes(y=(..count..)/sum(..count..))) +
    facet_grid(.~Age) +
    labs(title="UK Cannabis Consumption", x="Last Cannabis Consumption Date", y="Percent of Responses") +
    geom_text(stat = 'count', position=position_dodge(width=0.9), vjust=-0.5, size = 5) +
    scale_y_continuous(labels = scales::percent)
  grid.arrange(usaCanAge, ukCanAge, nrow=2, top="Cannabis Consumption: USA v.s UK by Gender")
  
  CrossTable(dc$Cannabis, dc$Country)
  
  