#fn<-file.choose();
start <- Sys.time()
library(randomForest); #install.packages('randomForest');
library(ranger);
library(Rborist);#install.packages('Rborist')
library(randomGLM)
fn<-"OrgNPVs2.csv"
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
rglm.dat$NPV <- cut(rglm.dat$NPV, c(-Inf,0,Inf), labels = c(0,1))
m <- dim(rglm.dat)[2]

cv.rglm<-function(x)
{
  print(x)
  fidx<-which(rfdat.gp==x);fold.size<-length(fidx)
  train.rgdat<-rglm.dat[-fidx,];
  test.rgdat<-rglm.dat[fidx,];
  rglm.mod<-randomGLM(x = train.rgdat[,-m], y = train.rgdat[,m], classify = T
                      , nCandidateCovariates = m-1, keepModels = T);
  preds <- predict.randomGLM(rglm.mod , test.rgdat[,-m])
  preds <- sapply(1:dim(preds)[1],function(x){
    if (preds[x,1] > preds[x,2]){
      return(0)
    }else return(1)
  })
  accu <- length(which(test.rgdat[,m] == preds))/fold.size
  return(c(accu,fold.size))
  
}
res.rglm <- lapply(c(1:cv.size), FUN=cv.rglm)
res.rglm
Sys.time() - start
