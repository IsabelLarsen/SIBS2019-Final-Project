# Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(gmodels)
library(readr)


# Basic Tables & Plots ----------------------------------------------------

table(drugConsumption$Age)
plot(drugConsumption$Age)

table(drugConsumption$Gender)
plot(drugConsumption$Gender)

table(drugConsumption$Education)
plot(drugConsumption$Education)

table(drugConsumption$Country)
plot(drugConsumption$Country)

table(drugConsumption$Ethnicity)
plot(drugConsumption$Ethnicity)

table(drugConsumption$Neuroticism)
plot(drugConsumption$Neuroticism)

table(drugConsumption$Extraversion)
plot(drugConsumption$Extraversion)

table(drugConsumption$OpennessToExperience)
plot(drugConsumption$OpennessToExperience)

table(drugConsumption$Agreeableness)
plot(drugConsumption$Agreeableness)

table(drugConsumption$Conscientiousness)
plot(drugConsumption$Conscientiousness)

table(drugConsumption$Alcohol)
plot(drugConsumption$Alcohol)

table(drugConsumption$Amphetamines)
plot(drugConsumption$Amphetamines)

table(drugConsumption$AmylNitrite)
plot(drugConsumption$AmylNitrite)

table(drugConsumption$Benzodiazepine)
plot(drugConsumption$Benzodiazepine)

table(drugConsumption$Caffeine)
plot(drugConsumption$Caffeine)

table(drugConsumption$Cannabis)
plot(drugConsumption$Cannabis)

table(drugConsumption$Chocolate)
plot(drugConsumption$Chocolate)

table(drugConsumption$Cocaine)
plot(drugConsumption$Cocaine)

table(drugConsumption$Crack)
plot(drugConsumption$Crack)

table(drugConsumption$Ectasy)
plot(drugConsumption$Ectasy)

table(drugConsumption$Heroin)
plot(drugConsumption$Heroin)

table(drugConsumption$Ketamine)
plot(drugConsumption$Ketamine)

table(drugConsumption$LegalHighs)
plot(drugConsumption$LegalHighs)

table(drugConsumption$LSD)
plot(drugConsumption$LSD)

table(drugConsumption$Methadone)
plot(drugConsumption$Methadone)

table(drugConsumption$MagicMushrooms)
plot(drugConsumption$MagicMushrooms)

table(drugConsumption$Nicotine)
plot(drugConsumption$Nicotine)

table(drugConsumption$FakeSemeron)
plot(drugConsumption$FakeSemeron)

table(drugConsumption$VolatileSubstanceAbuse)
plot(drugConsumption$VolatileSubstanceAbuse)



# Graphical Comparisons ---------------------------------------------------
  
#Alcohol

  #Alcohol consumption ~ gender
  ftable(xtabs(~Alcohol+Gender, data=drugConsumption))  
  CrossTable(drugConsumption$Alcohol, drugConsumption$Gender)
  
    ggplot(drugConsumption, aes(Alcohol, color=Gender, fill=Gender)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge") +
      labs(title="Alcohol Consumption by Gender", x="Latest Alcohol Consumption", y="Percent", fill="Gender") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Alcohol)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position = "dodge") +
      facet_grid(. ~ Gender) +
      labs(title="Alcohol Consumption by Gender", x="Latest Alcohol Consumption", y="Percent") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
  #Alcohol consumption ~ age
    ftable(xtabs(~Alcohol+Age, data=drugConsumption))
    CrossTable(drugConsumption$Alcohol, drugConsumption$Age)
    
    ggplot(drugConsumption, aes(Alcohol, fill=Age)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position = "dodge") +
      labs(title="Alcohol Consumption by Age", x="Latest Alcohol Consumption", y="Percent", fill="Age Group") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal")
    
    ggplot(drugConsumption, aes(Alcohol)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position = "dodge") +
      facet_grid(. ~ Age) +
      labs(title="Alcohol Consumption by Age", x="Latest Alcohol Consumption", y="Percent") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom",
            legend.box = "horizontal") 
    
  #Alcohol consumption ~ country
    ftable(xtabs(~Alcohol+Country, data=drugConsumption))  
    CrossTable(drugConsumption$Alcohol, drugConsumption$Country)
    
    ggplot(drugConsumption, aes(Alcohol, fill=Country)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position = "dodge") +
      labs(title="Alcohol Consumption by Country", x="Latest Alcohol Consumption", y="Percent", fill="Country") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal")
    
    ggplot(drugConsumption, aes(Alcohol)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position = "dodge") +
      facet_grid(. ~ Country) +
      labs(title="Alcohol Consumption by Country", x="Latest Alcohol Consumption", y="Percent") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal")

#Cannabis
    
  #Cannabis consumption ~ gender
    ftable(xtabs(~Cannabis+Gender, data=drugConsumption))  
    CrossTable(drugConsumption$Cannabis, drugConsumption$Gender)
    
    ggplot(drugConsumption, aes(Cannabis, fill=Gender)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge") +
      labs(title="Cannabis Consumption by Gender", x="Latest Cannabis Consumption", y="Percent", fill="Gender") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Cannabis)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position = "dodge") +
      facet_grid(. ~ Gender) +
      labs(title="Cannabis Consumption by Gender", x="Latest Cannabis Consumption", y="Percent") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
  #Cannabis consumption ~ age
    ftable(xtabs(~Cannabis+Age, data=drugConsumption))  
    CrossTable(drugConsumption$Cannabis, drugConsumption$Age)
    
    ggplot(drugConsumption, aes(Cannabis, fill=Age)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge") +
      labs(title="Cannabis Consumption by Age", x="Latest Cannabis Consumption", y="Percent", fill="Age Group") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Cannabis)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position = "dodge") +
      facet_grid(. ~ Age) +
      labs(title="Cannabis Consumption by Age", x="Latest Cannabis Consumption", y="Percent") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
  #Cannabis consumption ~ country
    ftable(xtabs(~Cannabis+Country, data=drugConsumption))  
    CrossTable(drugConsumption$Cannabis, drugConsumption$Country)
    
    ggplot(drugConsumption, aes(Cannabis, fill=Country)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge") +
      labs(title="Cannabis Consumption by Country", x="Latest Cannabis Consumption", y="Percent", fill="Age Group") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Cannabis)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position = "dodge") +
      facet_grid(. ~ Country) +
      labs(title="Cannabis Consumption by Country", x="Latest Cannabis Consumption", y="Percent") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
#Nicotine
    
  #Nicotine consumption ~ gender
    ftable(xtabs(~Nicotine+Gender, data=drugConsumption))  
    CrossTable(drugConsumption$Nicotine, drugConsumption$Gender)
    
    ggplot(drugConsumption, aes(Nicotine, fill=Gender)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge") +
      labs(title="Nicotine Consumption by Gender", x="Latest Nicotine Consumption", y="Percent", fill="Gender") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Nicotine)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position = "dodge") +
      facet_grid(. ~ Gender) +
      labs(title="Nicotine Consumption by Gender", x="Latest Nicotine Consumption", y="Percent") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
  #Nicotine consumption ~ age
    ftable(xtabs(~Nicotine+Age, data=drugConsumption))  
    CrossTable(drugConsumption$Nicotine, drugConsumption$Age)
    
    ggplot(drugConsumption, aes(Nicotine, fill=Age)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position="dodge") +
      labs(title="Nicotine Consumption by Age", x="Latest Nicotine Consumption", y="Percent", fill="Age Group") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Nicotine)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position = "dodge") +
      facet_grid(. ~ Age) +
      labs(title="Nicotine Consumption by Age", x="Latest Nicotine Consumption", y="Percent") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
  #Nicotine consumption ~ country
    ftable(xtabs(~Nicotine+Country, data=drugConsumption))  
    CrossTable(drugConsumption$Nicotine, drugConsumption$Country)
    
    ggplot(drugConsumption, aes(Nicotine, fill=Country)) +
      geom_bar(aes(y=(..count..)/sum(..count..), position="dodge")) +
      labs(title="Nicotine Consumption by Country", x="Latest Nicotine Consumption", y="Percent", fill="Age Group") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Nicotine)) +
      geom_bar(aes(y=(..count..)/sum(..count..)), position = "dodge") +
      facet_grid(. ~ Country) +
      labs(title="Nicotine Consumption by Country", x="Latest Nicotine Consumption", y="Percent") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    


# Personality Summaries: Determining Categories ---------------------------
    
drugConsump <- read_csv("C:/Users/izzyl/Dropbox/SIBS/Hackathon/SIBS2019-Final-Project/drug_consumption.data", col_names = FALSE)
names(drugConsump) <- c("ID", "Age", "Gender", "Education", "Country", "Ethnicity", "Neuroticism", "Extraversion", 
                            "OpennessToExperience", "Agreeableness", "Conscientiousness", "Impulsiveness", "SensationSeeking", 
                            "Alcohol", "Amphetamines", "AmylNitrite", "Benzodiazepine", "Caffeine", "Cannabis", "Chocolate", 
                            "Cocaine", "Crack", "Ectasy", "Heroin", "Ketamine", "LegalHighs", "LSD", "Methadone", "MagicMushrooms", 
                            "Nicotine", "FakeSemeron", "VolatileSubstanceAbuse")
drugConsump$Country <- factor(drugConsump$Country,
                                  levels = c(-0.09765, 0.24923, -0.46841, -0.28519, 0.21128, 0.96082, -0.57009),
                                  labels = c("Australia", "Canada", "NewZeland", "Other", "Ireland", "UK", "USA"))
drugConsump$Ethnicity <- factor(drugConsump$Ethnicity,
                                    levels = c(-0.50212, -1.10702, 1.90725, 0.12600, -0.22166, 0.11440, -0.31685),
                                    labels = c("Asian", "Black", "Black/Asian", "White/Asian", "White/Black", "Other", "White"))
drugConsumpUKUS <- drugConsump[((drugConsump$Country == "UK" | drugConsump$Country == "USA") & 
                                          (drugConsump$Ethnicity == "White")),]

drugConsumpUKUS
#Neuroticism
  summary(drugConsumpUKUS$Neuroticism)
  #1st q = -0.678250 = <30
  #3rd q = 0.629670 = >42
#Extraversion
  summary(drugConsumpUKUS$Extraversion)
  #1st q = -0.695090 = <36
  #3rd q = 0.637790 = >44
#Openness to Experience
  summary(drugConsumpUKUS$OpennessToExperience)
  #1st q = -0.717270 = <42
  #3rd q = 0.723300 = >51
#Agreeableness
  summary(drugConsumpUKUS$Agreeableness)
  #1st q = -0.606330 = <40
  #3rd q = 0.760960 = >48
#Connscientiousness
  summary(drugConsumpUKUS$Conscientiousness)
  #1st q = -0.65253 = <37
  #3rd q = 0.75830 = >47
  
#Add category values to drugConsumption
  drugConsumptionUKUS <- mutate(drugConsumptionUKUS, NeuroCategory = ifelse(Neuroticism %in% 0:29, "Low",
                                                                              ifelse(Neuroticism %in% 30:42, "Avg", 
                                                                                      "High")))
  drugConsumptionUKUS <- mutate(drugConsumptionUKUS, ExtraCategory = ifelse(Extraversion %in% 0:35, "Low",
                                                                              ifelse(Extraversion %in% 36:44, "Avg", 
                                                                                      "High")))
  drugConsumptionUKUS <- mutate(drugConsumptionUKUS, OpenCategory = ifelse(OpennessToExperience %in% 0:41, "Low",
                                                                              ifelse(OpennessToExperience %in% 42:51, "Avg",
                                                                                      "High")))
  drugConsumptionUKUS <- mutate(drugConsumptionUKUS, AgreeCategory = ifelse(Agreeableness %in% 0:39, "Low",
                                                                              ifelse(Agreeableness %in% 40:48, "Avg",
                                                                                   "High")))
  drugConsumptionUKUS <- mutate(drugConsumptionUKUS, ConscCategory = ifelse(Conscientiousness %in% 0:37, "Low",
                                                                            ifelse(Conscientiousness %in% 37:47, "Avg",
                                                                                    "High")))
                                                                                   
#Check values
  table(drugConsumptionUKUS$NeuroCategory)
  table(drugConsumptionUKUS$ExtraCategory)
  table(drugConsumptionUKUS$OpenCategory)
  table(drugConsumptionUKUS$AgreeCategory)
  table(drugConsumptionUKUS$ConscCategory)
  