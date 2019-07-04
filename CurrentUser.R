#Create currentUser dataframe
currentUser <- drugConsumption[(drugConsumption$Amphetamines == "LastDay" | drugConsumption$Amphetamines == "lastWeek" | 
                                drugConsumption$Amphetamines == "lastMonth") | (drugConsumption$AmylNitrite == "LastDay" | 
                                drugConsumption$AmylNitrite == "lastWeek" | drugConsumption$AmylNitrite == "lastMonth") | 
                                (drugConsumption$Benzodiazepine == "LastDay" | drugConsumption$Benzodiazepine == "lastWeek" | 
                                drugConsumption$Benzodiazepine == "lastMonth") |  (drugConsumption$Cannabis == "LastDay" | 
                                drugConsumption$Cannabis == "lastWeek" | drugConsumption$Cannabis == "lastMonth") |
                                (drugConsumption$Cocaine == "LastDay" | drugConsumption$Cocaine == "lastWeek" | 
                                drugConsumption$Cocaine == "lastMonth") | (drugConsumption$Crack == "LastDay" | 
                                drugConsumption$Crack == "lastWeek" | drugConsumption$Crack == "lastMonth") | 
                                (drugConsumption$Ectasy == "LastDay" | drugConsumption$Ectasy == "lastWeek" |
                                drugConsumption$Ectasy == "lastMonth") | (drugConsumption$Heroin == "LastDay" | 
                                drugConsumption$Heroin == "lastWeek" | drugConsumption$Heroin == "lastMonth") |
                                (drugConsumption$Ketamine == "LastDay" | drugConsumption$Ketamine == "lastWeek" | 
                                drugConsumption$Ketamine == "lastMonth") | (drugConsumption$LegalHighs == "LastDay" | 
                                drugConsumption$LegalHighs == "lastWeek" | drugConsumption$LegalHighs == "lastMonth") |
                                (drugConsumption$LSD == "LastDay" | drugConsumption$LSD == "lastWeek" | 
                                drugConsumption$LSD == "lastMonth") | (drugConsumption$Methadone == "LastDay" | 
                                drugConsumption$Methadone == "lastWeek" | drugConsumption$Methadone == "lastMonth") | 
                                (drugConsumption$MagicMushrooms == "LastDay" | drugConsumption$MagicMushrooms == "lastWeek" | 
                                drugConsumption$MagicMushrooms == "lastMonth") | (drugConsumption$VolatileSubstanceAbuse == "LastDay" | 
                                drugConsumption$VolatileSubstanceAbuse == "lastWeek" | 
                                drugConsumption$VolatileSubstanceAbuse == "lastMonth"),]

#Look at basic plots
plot(currentUser$Amphetamines)
plot(currentUser$AmylNitrite)
plot(currentUser$Benzodiazepine)
plot(currentUser$Cannabis)
plot(currentUser$Cocaine)
plot(currentUser$Crack)
plot(currentUser$Ectasy)
plot(currentUser$Heroin)
plot(currentUser$Ketamine)
plot(currentUser$LegalHighs)
plot(currentUser$LSD)
plot(currentUser$Methadone)
plot(currentUser$VolatileSubstanceAbuse)
