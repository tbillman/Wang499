#fn<-file.choose();
library(randomForest); #install.packages('randomForest');
library(ranger);
library(Rborist);#install.packages('Rborist')
fn<-"/Users/yishiwang/Documents/Research Since 2013/Research 2018/Project with Thomas Billman/OrgNPVs.csv"
raw<-read.csv(fn, header=TRUE);
rfdat<-subset.rfdata.frame(raw, select=-c(FirstPmt,Maturity.rfdate,MSA.Code, Loan.Sequence.Number,Seller.Name,Servicer.Name,Super.Conforming))
rfdat<-rfdat[complete.cases(rfdat),]
 rfdat<-rfdat[1:1000,];
n<-dim(rfdat)[1]; m=dim(rfdat)[2];
# dat$NPV<-as.factor(dat$NPV>0);
rfdat$NPV<-cut(rfdat$NPV, c(-Inf, 0,10^4,3*10^4,Inf),lables=c(1:4));
table(rfdat$NPV)


set.seed(12345)

cv.size<-5;rfdat.gp<-sample(1:cv.size,n,T) # table(dat.gp)

cv.rf.ranger<-function(x)
{
  fidx<-which(rfdat.gp==x);fold.size<-length(fidx)
  train.rfdat<-rfdat[-fidx,];
  test.rfdat<-rfdat[fidx,];
  f.rf.mod<-ranger(NPV~., rfdata=train.rfdat,num.trees=500);
  f.pred<-predict(f.rf.mod,test.rfdat)$predictions;
  accu<-sum(test.rfdat$NPV==f.pred)/fold.size;
  return(c(accu,fold.size))
  
}

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