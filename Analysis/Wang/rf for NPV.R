#fn<-file.choose();
library(randomForest); #install.packages('randomForest');
library(ranger);
library(Rborist);#install.packages('Rborist')
library(randomGLM)
fn<-"/Users/Thomas/Documents/Github/Wang499/OrgNPVs2.csv"
raw<-read.csv(fn, header=TRUE);
rfdat<-subset.data.frame(raw, select=-c(FirstPmt,Maturity.Date,MSA.Code, Loan.Sequence.Number,Seller.Name,Servicer.Name,Super.Conforming))
rfdat<-rfdat[complete.cases(rfdat),]
rfdat<-rfdat[1:1000,];
n<-dim(rfdat)[1]; m=dim(rfdat)[2];

set.seed(12345)

cv.size<-5;rfdat.gp<-sample(1:cv.size,n,T) # table(dat.gp)

cv.rf.ranger<-function(x)
{
  print(x)
  fidx<-which(rfdat.gp==x);fold.size<-length(fidx)
  train.rfdat<-rfdat[-fidx,];
  test.rfdat<-rfdat[fidx,];
  f.rf.mod<-ranger(NPV~., data=train.rfdat,num.trees=500);
  r2 <- f.rf.mod$r.squared
  return(c(r2,fold.size))
  
}
# added the sort so the order of your df is maintained
res.rf<-lapply(c(1:cv.size), FUN=cv.rf.ranger)
res.rf

fact <- sapply(1:(m),function(x){
  is.factor(rfdat[,x])
})
rglm.dat <- rfdat[,which(fact == 0)]
m <- dim(rglm.dat)[2]

cv.rglm<-function(x)
{
  print(x)
  fidx<-which(rfdat.gp==x);fold.size<-length(fidx)
  train.rgdat<-rglm.dat[-fidx,];
  test.rgdat<-rglm.dat[fidx,];
  rglm.mod<-randomGLM(x = train.rgdat[,-m], y = train.rgdat[,m], 
                      xtest = test.rgdat[,-m], nCandidateCovariates = m-1);
  r2 <- summary(lm(test.rgdat[,m] ~ rglm.mod$predictedTest))$r.squared
  return(c(r2,fold.size))
  
}
res.rglm <- lapply(c(1:cv.size), FUN=cv.rglm)
res.rglm
