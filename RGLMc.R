#####Analysis of NPV results#####
####Have Shell Script go to Honors Directory#####
library("randomForest")
library("ranger")
library("randomGLM")
datas= read.csv(file = "OrgNPVs2.csv")
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
datasu = sapply(1:ncol(datas), function(x){
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
set.seed(12345)
n<-dim(rfdat)[1]; m=dim(rfdat)[2];
cv.size<-5;rfdat.gp<-sample(1:cv.size,n,T) # table(dat.gp)

fact <- sapply(1:m, function(x){
  is.factor(rfdat[,x])
})
rglm.dat <- rfdat[,which(fact == 0)]
rglm.dat$NPV <- cut(rglm.dat$NPV, c(-Inf, 0, Inf), labels = c(0,1))

m <- dim(rglm.dat)[2]

cv.rglm <- function(x){
  fidx <- which(rfdat.gp == x)
  fold.size <- length(fidx)
  train.rgdat <- rglm.dat[-fidx,]
  test.rgdat <- rglm.dat[fidx,]
  rglm.mod <- randomGLM(x = train.rgdat[,-m], y = train.rgdat[,m],
                        nCandidateCovariates = m-1, nThreads =1)
  preds <-predict.randomGLM(rglm.mod , test.rgdat[,-m])
  preds <- sapply(1:dim(preds)[1], function(x){
    if (preds[x,1] > preds[x,2]){
      return(0)
    } else return(1)
  })
  accu <- length(which(preds == test.rgdat[,m]))/fold.size
  return(c(accu,fold.size))
}
res.rglm <- lapply(1:cv.size, FUN=cv.rglm)
res.rglm
