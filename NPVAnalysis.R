library("randomForest")
library("MASS")
library("parallel")
dat = read.table(file = "C:/Users/Thomas/Desktop/Data/Freddie1999/subsetNPV.csv", sep = "|")
model = lm(NPV ~ CreditScore + MSA.Code + MI.Percentage + Occupancy.Status + CLTV + DTI + UPB +
             Property.Type + Loan.Purpose, data = dat)
summary(model)
pairs(formula = NPV ~ CreditScore + MSA.Code + MI.Percentage + Occupancy.Status + CLTV + DTI + UPB +
        Property.Type + Loan.Purpose, data = dat)
plot(dat$NPV,model$residuals)
