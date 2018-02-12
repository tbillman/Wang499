#####Analysis of NPV results#####
library("tidyverse")
library("randomForest")
library("car")
datas= read_csv(file = "C:/Users/Thomas/Desktop/Data/OrgNPVs.csv")
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
  dim(unique(datas[,x]))[1]
})
useless = which(datasu == 1)
datas = datas[,-useless]
keep = which(complete.cases(datas) == T)
datasc = datas[keep,]
#Visualizing the NPVs
hist(datasc$NPV,breaks = 100, main = "NPV Distribution", xlab = "NPVs")
# For some reason this doesn't work vector too big error
#modc = lm(NPV ~ ., datasc)
modc = lm(NPV ~ CreditScore + `MSA Code` + `MI Percentage` +
            DTI + UPB + CLTV + LTV + `Interest Rate`  + `Original Term`, datasc)
plot(cooks.distance(modc))
outlc = which(cooks.distance(modc) >= (4/dim(datasc)[1]))
datascc = datasc[-outlc,]
modc1 = lm(NPV ~ CreditScore + `MSA Code` + `MI Percentage` +
           DTI + UPB + CLTV + LTV + `Interest Rate`  + `Original Term`, datascc)
vif(modc1)
#It appears that there is high multicolinearity with CLTV and LTV, let's eliminate CLTV
modc2 = lm(NPV ~ CreditScore + DTI + UPB  + 
             LTV + `Interest Rate`  + `Original Term`, datascc)
vif(modc2)
summary(modc2)
AIC(modc2);BIC(modc2)
shapiro.test(datascc$NPV[1:5000])
hist(datascc$NPV,breaks = 50, main = "NPV Distribution", xlab = "NPVs")
npv  = datascc$NPV
n = length(datascc$NPV)
npv.ecdf<-ecdf(datascc$NPV)
acper<-npv.ecdf(npv)-1/2/n
avg.npv<-mean(npv);sd.npv<-sd(npv)
npv.tran<-qnorm(acper,avg.npv,sd.npv)
head(cbind(npv,acper,npv.tran))
shapiro.test(sample(npv.tran,5000))
par(mfrow = c(2,1))
hist(npv.tran, breaks = 50)
hist(npv, breaks = 50)

trandat = datascc
trandat$NPV = npv.tran
modc3 = lm(NPV ~ CreditScore + DTI + UPB  + 
             LTV + `Interest Rate`  + `Original Term`, trandat)
vif(modc3)
summary(modc3)
AIC(modc3);BIC(modc3)

mods = lm(NPV/UPB ~ CreditScore + DTI   + 
             LTV + `Interest Rate`  + `Original Term`, datascc)
summary(mods)
hist(datascc$NPV/datascc$UPB, breaks = 100)

mods = lm(NPV/UPB ~ CreditScore + DTI   + 
            LTV + `Interest Rate`  + `Original Term`, trandat)
summary(mods)
hist(trandat$NPV/trandat$UPB, breaks = 100)

#####Randomforest Exploratory#####
badvars <- c(5,16,18,19,22)
rfdat = datascc[,-badvars]
rfdat$FirstTimeHomebuyer = as.factor(rfdat$FirstTimeHomebuyer)
rfdat$Occupancy.Status = as.factor(rfdat$Occupancy.Status)
rfdat$Channel = as.factor(rfdat$Channel)
rfdat$PPM = as.factor(rfdat$PPM)
rfdat$Property.Type = as.factor(rfdat$Property.Type)
rfdat$Loan.Purpose = as.factor(rfdat$Loan.Purpose)
rfdat$Seller.Name = as.factor(rfdat$Seller.Name)
rfdat$Servicer.Name = as.factor(rfdat$Servicer.Name)
names(rfdat) = make.names(names(rfdat))
train = sample(1:nrow(rfdat),5000)
train.rf = randomForest(NPV ~.,
                        data = rfdat, subset = train)
train.rf
plot(train.rf)

oob.err=double(6)
test.err=double(6)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:6) 
{
  rf=randomForest(NPV ~ . , data = rfdat , subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,rfdat[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(rfdat[-train,], mean( (NPV - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}
test.err
oob.err
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
