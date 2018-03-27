#####Analysis of NPV results#####
####Have Shell Script go to Honors Directory#####
library("tidyverse")
library("randomForest")
library("car")
library("Rborist");#install.packages('Rborist')
setwd("C:/Users/Thomas/Documents/Github/Wang499")
datas= read.csv(file = "OrgNPVs.csv")
# #Issue with spaces in Variable Names
# mod = lm(NPV ~ CreditScore + `MSA Code` + `MI Percentage` +
#            DTI + UPB + CLTV + LTV + `Interest Rate`  + `Original Term`, data)
# mod1 = lm(NPV ~ CreditScore + DTI + UPB + CLTV + LTV + State, data)
# cleandat = cbind(data$CreditScore,data$`MSA Code`, data$`MI Percentage`,
#                  data$`Number of Units`, data$`Occupancy Status`, data$CLTV,
#                  data$DTI, data$UPB, data$LTV, data$`Interest Rate`, 
#                  data$Product, data$State, data$`Postal Code`)
# rf1 = randomForest(cleandat[complete.cases(cleandat),], y= data$NPV[complete.cases(cleandat)])
#####Using Sample Data instead#####
#datas = read_csv(file = "C:/Users/Thomas/Documents/Github/Wang499/OrgSampleNPVs.csv")
datasu = sapply(1:length(colnames(datas)), function(x){
  length(unique(datas[,x]))
})
useless = which(datasu == 1)
datas = datas[,-useless]
keep = which(complete.cases(datas) == T)
datasc = datas[keep,]
modc = lm(NPV ~ CreditScore + MSA.Code + MI.Percentage +
            DTI + UPB + CLTV + LTV + Interest.Rate  + Original.Term, datasc)
outlc = which(cooks.distance(modc) >= (4/dim(datasc)[1]))
datascc = datasc[-outlc,]
#####Randomforest Exploratory#####
badvars <- c(5,16,18,19,22)
rfdat = datascc[,-badvars]
names(rfdat) = make.names(names(rfdat))
rfdat$FirstTimeHomebuyer = as.factor(rfdat$FirstTimeHomebuyer)
rfdat$Occupancy.Status = as.factor(rfdat$Occupancy.Status)
rfdat$Channel = as.factor(rfdat$Channel)
rfdat$PPM = as.factor(rfdat$PPM)
rfdat$Property.Type = as.factor(rfdat$Property.Type)
rfdat$Loan.Purpose = as.factor(rfdat$Loan.Purpose)
rfdat$Seller.Name = as.factor(rfdat$Seller.Name)
rfdat$Servicer.Name = as.factor(rfdat$Servicer.Name)

n<-dim(rfdat)[1]; m=dim(rfdat)[2];
cv.size<-5;rfdat.gp<-sample(1:cv.size,n,T) # table(dat.gp)

cv.rf.Rbo<-function(x){
  fidx<-which(rfdat.gp==x);fold.size<-length(fidx)
  train.rfdat<-rfdat[-fidx,];
  test.rfdat<-rfdat[fidx,];
  pt <- PreFormat(train.rfdat[,-m])
  f.rf.mod<-Rborist(pt, train.rfdat[,m] ,nTree=100);
  f.pred<-predict(f.rf.mod,test.rfdat[,-m])$predictions;
  accu<-sum(test.rfdat$NPV==f.pred)/fold.size;
  return(c(accu,fold.size))
}
# added the sort so the order of your df is maintained
res.rf<-lapply(c(1:cv.size), FUN=cv.rf.Rbo)
res.rf