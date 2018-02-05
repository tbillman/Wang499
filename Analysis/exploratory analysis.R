#####Analysis of NPV results#####
library("tidyverse")
library("randomForest")
data = read_csv(file = "C:/Users/Thomas/Desktop/Data/OrgNPVs.csv")
#Issue with spaces in Variable Names
mod = lm(NPV ~ CreditScore + `MSA Code` + `MI Percentage` +
           DTI + UPB + CLTV + LTV + `Interest Rate`  + `Original Term`, data)
mod1 = lm(NPV ~ CreditScore + DTI + UPB + CLTV + LTV + State, data)
cleandat = cbind(data$CreditScore,data$`MSA Code`, data$`MI Percentage`,
                 data$`Number of Units`, data$`Occupancy Status`, data$CLTV,
                 data$DTI, data$UPB, data$LTV, data$`Interest Rate`, 
                 data$Product, data$State, data$`Postal Code`)
rf1 = randomForest(cleandat[complete.cases(cleandat),], y= data$NPV[complete.cases(cleandat)])
#####Using Sample Data instead#####
datas = read_csv(file = "C:/Users/Thomas/Documents/Github/Wang499/OrgSampleNPVs.csv")
mod1 = lm(NPV ~ CreditScore + DTI + UPB + CLTV + LTV + State, datas)
cleandats = cbind.data.frame(datas$CreditScore,datas$`MSA Code`, datas$`MI Percentage`,
                 datas$`Number of Units`, datas$`Occupancy Status`, datas$CLTV,
                 datas$DTI, datas$UPB, datas$LTV, datas$`Interest Rate`, 
                 datas$Product, datas$State, datas$`Postal Code`)
keep = which(complete.cases(cleandats) == T)
rf1 = randomForest(cleandats[keep,], y= datas$NPV[keep])
