# Installing packges
#tidyverse 
#here
#janitor
#skimr
#reshape2
#plm

# load packges 
library(tidyverse)
library(here)
library(janitor)
library(skimr)
library(reshape2)
library(plm)
library(marginaleffects)
library(fixest)


#read in data
ESG_scores_G <- read.csv(here("Data ESG","ESG scores- Governance.csv"), sep=";", dec=",")
ESG_scores_O <- read.csv(here("Data ESG","ESG scores- Overall .csv"), sep=";", dec=",")
ESG_scores_E <- read.csv(here("Data ESG","ESG scores-Environmental.csv"), sep=";", dec=",")
ESG_scores_S <- read.csv(here("Data ESG","ESG scores-Social.csv"), sep=";", dec=",")
Data_Orbis <- read.csv(here("Data Orbis","Orbis-Eikon-New-fixed.csv"), sep=";" , dec=",", na.strings = "n.a.")
Data_Orbis$Profit.margin.....2012<-sub("n.s.", Data_Orbis$Profit.margin.....2012, NA)
str(Data_Orbis)
#viewing data 

View(ESG_scores_G)
View(ESG_scores_O)
View(ESG_scores_E)
View(ESG_scores_S)
View(Data_Orbis)

# 1. get complete data (inputs)
# 2. make sure everzthing is properly imported
# 3. merge bz company ID and year (using either merge() or join())


#New material 
#Overall ESG melting------------------------------------------------------  
MELT_O_ESG <- melt (ESG_scores_O, id.vars = "ISIN.code")
MELT_O_ESG $variable <- as.numeric(substr(MELT_O_ESG $variable,3,6)) 

colnames(MELT_O_ESG)[3]<-"ESG_O"
colnames(MELT_O_ESG)[2]<-"year"
View(MELT_O_ESG)

#Envirioment ESG melt
MELT_E_ESG <- melt (ESG_scores_E, id.vars = "ISIN.code")
MELT_E_ESG $variable <- as.numeric(substr(MELT_E_ESG $variable,3,6)) 

colnames(MELT_E_ESG)[3]<-"ESG_E"
colnames(MELT_E_ESG)[2]<-"year"
View(MELT_E_ESG)

#Social ESG melt 
MELT_S_ESG <- melt (ESG_scores_S, id.vars = "ISIN.code")
MELT_S_ESG $variable <- as.numeric(substr(MELT_S_ESG $variable,3,6)) 

colnames(MELT_S_ESG)[3]<-"ESG_S"
colnames(MELT_S_ESG)[2]<-"year"
View(MELT_S_ESG)

#Governance ESG melt 
MELT_G_ESG <- melt (ESG_scores_G, id.vars = "ISIN.code")
MELT_G_ESG $variable <- as.numeric(substr(MELT_G_ESG $variable,3,6)) 

colnames(MELT_G_ESG)[3]<-"ESG_G"
colnames(MELT_G_ESG)[2]<-"year"
View(MELT_G_ESG)

#Orbis melting --------------------------------------------------------

#Assets
MELT_Assets <- Data_Orbis [,-c(2)]
MELT_Assets <-melt(MELT_Assets [,1:10], id.vars = c("ISIN.code"))
MELT_Assets $variable <- as.numeric(substr(MELT_Assets $variable,8,11)) 
colnames(MELT_Assets)[3]<-"A"
colnames(MELT_Assets)[2]<-"year"
View(MELT_Assets)
#names 

MELT_names <- Data_Orbis [,-c(2:128)]


#Profit margin
Data_Orbis <- read.csv(here("Data Orbis","Orbis-Eikon-New-fixed.csv"), sep=";" , dec=",", na.strings = "n.a.")
Data_Orbis$Profit.margin.....2012<-sub("n.s.", Data_Orbis$Profit.margin.....2012, NA)
str(Data_Orbis)

MELT_PM <- Data_Orbis [,-c(2:20)]
MELT_PM <-melt(MELT_PM [,1:10], id.vars = c("ISIN.code"))
MELT_PM $variable <- as.numeric(substr(MELT_PM $variable,15,18)) 
colnames(MELT_PM)[3]<-"PM"
colnames(MELT_PM)[2]<-"year"
View(MELT_PM)

#Current ratio
MELT_Current <- Data_Orbis [,-c(2:11)]
MELT_Current <-melt(MELT_Current [,1:10], id.vars = c("ISIN.code"))
MELT_Current $variable <- as.numeric(substr(MELT_Current $variable,15,18)) 
colnames(MELT_Current)[3]<-"CR"
colnames(MELT_Current)[2]<-"year"
View(MELT_Current)

#ROE
MELT_ROE <- Data_Orbis [,-c(2:119)]
MELT_ROE <-melt(MELT_ROE [,1:10], id.vars = c("ISIN.code"))
MELT_ROE $variable <- as.numeric(substr(MELT_ROE $variable,5,8)) 
colnames(MELT_ROE)[3]<-"ROE"
colnames(MELT_ROE)[2]<-"year"
View(MELT_ROE)

#solvency ratio 

MELT_Solvency <- Data_Orbis [,-c(2:29)]
MELT_Solvency <-melt(MELT_Solvency [,1:10], id.vars = c("ISIN.code"))
MELT_Solvency $variable <- as.numeric(substr(MELT_Solvency $variable,16,19)) 

colnames(MELT_Solvency)[3]<-"SR"
colnames(MELT_Solvency)[2]<-"year"
View(MELT_Solvency)
#Employees

MELT_Employees <- Data_Orbis [,-c(2:38)]
MELT_Employees <-melt(MELT_Employees [,1:10], id.vars = c("ISIN.code"))
MELT_Employees $variable <- as.numeric(substr(MELT_Employees $variable,11,14)) 

colnames(MELT_Employees)[3]<-"EMP"
colnames(MELT_Employees)[2]<-"year"
View(MELT_Employees)
#Equity

MELT_Equity <- Data_Orbis [,-c(2:47)]
MELT_Equity <-melt(MELT_Equity [,1:10], id.vars = c("ISIN.code"))
MELT_Equity $variable <- as.numeric(substr(MELT_Equity $variable,8,11)) 

colnames(MELT_Equity)[3]<-"EQ"
colnames(MELT_Equity)[2]<-"year"
View(MELT_Equity)
#P/L
MELT_P.L <- Data_Orbis [,-c(2:56)]
MELT_P.L <-melt(MELT_P.L [,1:10], id.vars = c("ISIN.code"))
MELT_P.L $variable <- as.numeric(substr(MELT_P.L $variable,5,8)) 

colnames(MELT_P.L)[3]<-"P/L"
colnames(MELT_P.L)[2]<-"year"
View(MELT_P.L)

#CGS
MELT_CGS <- Data_Orbis [,-c(2:65)]
MELT_CGS <-melt(MELT_CGS [,1:10], id.vars = c("ISIN.code"))
MELT_CGS $variable <- as.numeric(substr(MELT_CGS $variable,5,8)) 

colnames(MELT_CGS)[3]<-"CGS"
colnames(MELT_CGS)[2]<-"year"
View(MELT_CGS)
#COE
MELT_COE <- Data_Orbis [,-c(2:74)]
MELT_COE <-melt(MELT_COE [,1:10], id.vars = c("ISIN.code"))
MELT_COE $variable <- as.numeric(substr(MELT_COE $variable,5,8)) 

colnames(MELT_COE)[3]<-"COE"
colnames(MELT_COE)[2]<-"year"
View(MELT_COE)
#MC
MELT_MC <- Data_Orbis [,-c(2:83)]
MELT_MC <-melt(MELT_MC [,1:10], id.vars = c("ISIN.code"))
MELT_MC $variable <- as.numeric(substr(MELT_MC $variable,4,7)) 

colnames(MELT_MC)[3]<-"MC"
colnames(MELT_MC)[2]<-"year"
View(MELT_MC)

#Time
MELT_Time <- Data_Orbis [,-c(2:92)]
MELT_Time <-melt(MELT_Time [,1:10], id.vars = c("ISIN.code"))
MELT_Time $variable <- as.numeric(substr(MELT_Time $variable,5,8)) 
colnames(MELT_Time)[3]<-"AGE"
colnames(MELT_Time)[2]<-"year"
View(MELT_Time)

#Research and Development 

MELT_RD <- Data_Orbis [,-c(2:101)]
MELT_RD <-melt(MELT_RD [,1:10], id.vars = c("ISIN.code"))
MELT_RD $variable <- as.numeric(substr(MELT_RD $variable,4,7)) 
colnames(MELT_RD)[3]<-"RD"
colnames(MELT_RD)[2]<-"year"
View(MELT_RD)

#dividends 

MELT_D <- Data_Orbis [,-c(2:110)]
MELT_D <-melt(MELT_D [,1:10], id.vars = c("ISIN.code"))
MELT_D $variable <- as.numeric(substr(MELT_D $variable,3,6)) 
colnames(MELT_D)[3]<-"D"
colnames(MELT_D)[2]<-"year"
View(MELT_D)

#Mergin---------------------------------------------------------------
#step1
Merge1 <- merge(MELT_O_ESG,MELT_E_ESG, all = TRUE)
Merge2 <- merge(MELT_S_ESG,MELT_G_ESG, all = TRUE)
Merge3 <- merge(MELT_PM,MELT_Current, all = TRUE)
Merge4 <- merge(MELT_Solvency,MELT_Employees, all = TRUE)
Merge5 <- merge(MELT_Equity,MELT_P.L, all = TRUE)
Merge6 <- merge(MELT_CGS,MELT_COE, all = TRUE)
Merge7 <- merge(MELT_Assets,MELT_Time, all = TRUE)
Merge8 <- merge(MELT_RD,MELT_D, all = TRUE)
Merge9 <- merge(Merge8,MELT_ROE, all = TRUE)
Merge10 <- merge(Merge9,MELT_names, all = TRUE)

#Step 2

Merge12 <- merge(Merge1,Merge2, all = TRUE)
Merge34 <- merge(Merge3,Merge4, all = TRUE)
Merge56 <- merge(Merge5,Merge6, all = TRUE)
Merge567 <- merge(Merge56,Merge7, all = TRUE)
Merge56789 <- merge(Merge567,Merge9, all = TRUE)
Merge5678910 <- merge(Merge56789,Merge10, all = TRUE)

#Step 3

Merge345678910 <- merge(Merge34,Merge5678910, all = TRUE)
#34567 is financial data and 12 is ESG data 

MergeFINAL <- merge(Merge12,Merge345678910)
MergeFINAL $CGS <-MergeFINAL $CGS * -1
MergeFINAL $D <-MergeFINAL $D * -1
view(MergeFINAL)
#Complete data --- Only cells with values in it - only companies with all values available. 
final_complete_all <-MergeFINAL[complete.cases(MergeFINAL[,c("PM","ESG_G","Name","CGS","ESG_S","ESG_E","COE","ESG_O","SR","ROE","CR","RD","A","ISIN.code","year")]),c("PM","ESG_G","Name","ESG_S","ESG_E","ESG_O","COE", "CGS", "ROE","SR","CR","RD","A","ISIN.code","year")]
final_complete_all$COE<-as.numeric (final_complete_all$COE)
view(final_complete_all)
summary(final_complete_all)
str(final_complete_all)

Cloud <- final_complete_all$ISIN.code
view(Cloud)
#Model ---------------------------------------------------------

#Model Profit Margin mean - using feols. 

MODEL_G_PM <- feols(PM ~ ESG_G + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_G_PM)
MODEL_S_PM <- feols(PM ~ ESG_S + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_S_PM)
MODEL_E_PM <- feols(PM ~ ESG_E + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_E_PM)
MODEL_O_PM <- feols(PM ~ ESG_O + + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_O_PM)

#disregard 
MODEL_PM_W <- feols(PM ~ CGS + COE + CR + RD + SR + A + E + I(CGS*CGS) + I(COE*COE)+ RD:COE + RD:CGS|year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_PM_W)

#marginal effect 
marginaleffects(MODEL_G_PM, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_G_PM))

marginaleffects(MODEL_S_PM, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_S_PM))

marginaleffects(MODEL_E_PM, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_E_PM))

marginaleffects(MODEL_O_PM, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_O_PM))


#semivarience --------------------------------------
#residuals **** did not work ******
final_complete_all$residual_G <- resid(MODEL_G_PM)
View(final_complete_all)

final_complete_all$residual_E <- resid(MODEL_E_PM)
View(final_complete_all)

final_complete_all$residual_S <- resid(MODEL_S_PM)
View(final_complete_all)

final_complete_all$residual_O <- resid(MODEL_O_PM)
View(final_complete_all)

final_complete_all$MODEL_PM_W <- resid(MODEL_PM_W)
View(final_complete_all)

#squared residuals

final_complete_all$squaredresidual_G <- resid(MODEL_G_PM)*resid(MODEL_G_PM)
View(final_complete_all)

final_complete_all$squaredresidual_E <- resid(MODEL_E_PM)*resid(MODEL_E_PM)
View(final_complete_all)

final_complete_all$squaredresidual_S <- resid(MODEL_S_PM)*resid(MODEL_S_PM)
View(final_complete_all)

final_complete_all$squaredresidual_O <- resid(MODEL_O_PM)*resid(MODEL_O_PM)
View(final_complete_all)

final_complete_all$squaredresidual_W <- resid(MODEL_PM_W)*resid(MODEL_PM_W)
View(final_complete_all)

#model for Semi-varience.#### it worked on tuesday. gives me an error today. 

MODEL_G_SV <- feols(squaredresidual_G ~ ESG_G + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_G<0),])
summary (MODEL_G_SV)
# i mistakenly deleted the code you gave me to fix the SV. Can i have it again? 
MODEL_S_SV <- feols(squaredresidual_S ~ ESG_S + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_S<0),])
summary (MODEL_S_SV)
MODEL_E_SV <- feols(squaredresidual_E ~ ESG_E + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_E<0),])
summary (MODEL_E_SV)
MODEL_O_SV <- feols(squaredresidual_O ~ ESG_O + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_O<0),])
summary (MODEL_O_SV)

#Marginal effects SV 
marginaleffects(MODEL_G_SV, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_G_SV))

marginaleffects(MODEL_S_SV, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_S_SV))

marginaleffects(MODEL_E_SV, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_E_SV))

marginaleffects(MODEL_O_SV, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_O_SV))


#robustness check ROE

MODEL_G_ROE <- feols(ROE ~ ESG_G + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_G_ROE)
MODEL_S_ROE <- feols(ROE ~ ESG_S + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_S_ROE)
MODEL_E_ROE <- feols(ROE ~ ESG_E + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_E_ROE)
MODEL_O_ROE <- feols(ROE ~ ESG_O + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_O_ROE)



#semivarienece for ROE rubustness check 
#residuas 
final_complete_all$residual_G_R <- resid(MODEL_G_ROE)
View(final_complete_all)

final_complete_all$residual_S_R <- resid(MODEL_S_ROE)
View(final_complete_all)

final_complete_all$residual_E_R <- resid(MODEL_E_ROE)
View(final_complete_all)

final_complete_all$residual_O_R <- resid(MODEL_O_ROE)
View(final_complete_all)
# squared residuals ROE

final_complete_all$squaredresidual_G_R <- resid(MODEL_G_ROE)*resid(MODEL_G_ROE)
View(final_complete_all)

final_complete_all$squaredresidual_E_R <- resid(MODEL_E_ROE)*resid(MODEL_E_ROE)
View(final_complete_all)

final_complete_all$squaredresidual_S_R <- resid(MODEL_S_ROE)*resid(MODEL_S_ROE)
View(final_complete_all)

final_complete_all$squaredresidual_O_R <- resid(MODEL_O_ROE)*resid(MODEL_O_ROE)
View(final_complete_all)

#SV ROE 
MODEL_G_SV_R <- feols(squaredresidual_G_R ~ ESG_G + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_G<0),])
summary (MODEL_G_SV_R)
MODEL_S_SV_R <- feols(squaredresidual_S_R ~ ESG_S + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_S<0),])
summary (MODEL_S_SV_R)
MODEL_E_SV_R <- feols(squaredresidual_E_R ~ ESG_E + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_E<0),])
summary (MODEL_E_SV_R)
MODEL_O_SV_R <- feols(squaredresidual_O_R ~ ESG_O + COE + CGS + CR + RD + SR + A + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_O<0),])
summary (MODEL_O_SV_R)

#Marginal effects Robustness check 
marginaleffects(MODEL_G_ROE, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_G_ROE))

marginaleffects(MODEL_S_ROE, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_S_ROE))

marginaleffects(MODEL_E_ROE, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_E_ROE))

marginaleffects(MODEL_O_ROE, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_O_ROE))

#Marginal effects ROE - SV

marginaleffects(MODEL_G_SV_R, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_G_SV_R))

marginaleffects(MODEL_S_SV_R, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_S_SV_R))

marginaleffects(MODEL_E_SV_R, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_E_SV_R))

marginaleffects(MODEL_O_SV_R, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_O_SV_R))

#summary statistics
#ESG
summary(final_complete_all$ESG_G)
summary(final_complete_all$ESG_S)
summary(final_complete_all$ESG_E)
summary(final_complete_all$ESG_O)
#financials 
summary(final_complete_all$PM)
summary(final_complete_all$ROE)
summary(final_complete_all$SR)
summary(final_complete_all$CR)

summary(final_complete_all$COE)
summary(final_complete_all$RD)
    
#SD 
sd(final_complete_all$ESG_G)
sd(final_complete_all$ESG_S)
sd(final_complete_all$ESG_E)
sd(final_complete_all$ESG_O)
#financials 
sd(final_complete_all$PM)
sd(final_complete_all$ROE)
sd(final_complete_all$SR)
sd(final_complete_all$CR)

sd(final_complete_all$COE)
sd(final_complete_all$RD)


#word cloud 
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("tm")
library(tm)


write.table(MODEL_G_PM, 'MODEL_G_PM_.csv', row.names = FALSE)

#checking if model value chage if we dont take ESG scores into account. 
MODEL_PM_W <- feols(PM ~ CGS + COE + CR + RD + SR + A + CGS:COE + I(CGS*CGS) + I(COE*COE)+ RD:COE + RD:CGS |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_G_PM_W)
MODEL_SV_W <- feols(squaredresidual_W ~ CGS + COE + CR + RD + SR + A + CGS:COE + I(CGS*CGS) + I(COE*COE)+ RD:COE + RD:CGS|ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_G<0),])
summary (MODEL_SV_W)

marginaleffects(MODEL_PM_W, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_PM_W))

marginaleffects(MODEL_SV_W, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_SV_W))

#ESG summary analisys all ESG listed companies sample
summary(ESG_scores_G)
summary(ESG_scores_E)
summary(ESG_scores_S)
summary(ESG_scores_O)
#70 Aniaml protein/ dairy 

summary(MergeFINAL$ESG_G)
summary(MergeFINAL$ESG_S)
summary(MergeFINAL$ESG_E)
summary(MergeFINAL$ESG_O)


#Test perhaps use for the beverages paper. in the model below the cost variable costs of employees (COE) was taken out. 

MODEL_PM_G_T <- feols(PM ~ ESG_G + CGS + CR + RD + SR + A + I(CGS*CGS) + RD:CGS|year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_PM_G_T)
