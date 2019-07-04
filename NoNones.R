#Create noNone dataframe 
noNone <- drugConsumption[!(drugConsumption$Amphetamines == "Never" & drugConsumption$AmylNitrite == 
                              "Never" & drugConsumption$Benzodiazepine == "Never" & drugConsumption$Cocaine == "Never" & 
                              drugConsumption$Ectasy == "Never" & drugConsumption$Heroin == "Never" & 
                              drugConsumption$Ketamine == "Never" & drugConsumption$LegalHighs == "Never" & drugConsumption$LSD == "Never"
                              & drugConsumption$Methadone == "Never" &drugConsumption$ MagicMushrooms == "Never" & 
                              drugConsumption$VolatileSubstanceAbuse == "Never"),]

#Look at barplots
plot(noNone$Amphetamines)
plot(noNone$AmylNitrite)
plot(noNone$Benzodiazepine)
plot(noNone$Cannabis)
plot(noNone$Cocaine)
plot(noNone$Crack)
plot(noNone$Ectasy)
plot(noNone$Heroin)
plot(noNone$Ketamine)
plot(noNone$LegalHighs)
plot(noNone$LSD)
plot(noNone$Methadone)
plot(noNone$VolatileSubstanceAbuse)