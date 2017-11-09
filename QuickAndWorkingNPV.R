#####Packages and Functions#####
library("randomForest")
library("MASS")
library("parallel")
i = (1.0293)^(1/12) - 1
starttime = Sys.time()
date.read <- function(yyyymm){
  as.Date(paste0(as.character(yyyymm), '01'), format='%Y%m%d')
}

nmonths <- function(end, start) {
  ed <- as.POSIXlt(end)
  sd <- as.POSIXlt(start)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon) + 1
}

set.grab <- function(orig, perfo){
  sets = as.list(NULL)
  a = as.character(orig$`Loan Sequence Number`)
  b = as.character(perfo$`Sequence Number`)
  info.matrix = outer(a,b,"==")
  coor = which(info.matrix==TRUE, arr.ind = TRUE)
  sets = lapply(1:length(a),function(x){
    set = NULL
    app = coor[which(coor[,1] == x, arr.ind = TRUE),2]
    set = rbind(perfo[app,])
    sets[[x]] = set
  })
  return(sets)
}


classify <- function(set){
  if(is.null(set[dim(set)[1],]$`Zero Balance`) | is.na(set[dim(set)[1],]$`Zero Balance`)){
    return("Current")
  }else{
    if(set[dim(set)[1],]$`Zero Balance` == 1){
      return("Prepaid")
    } else
      if((set[dim(set)[1],]$`Zero Balance`== 3) |(set[dim(set)[1],]$`Zero Balance`== 9) ){
        return("Default")
      } 
  }
}

prepaid.npv <- function(set,i){
  return(sum((set$`Current UPB` * set$`Current Interest Rate`/1200)[-dim(set)[1]],
             (set$`Current UPB`[-dim(set)[1]] - set$`Current UPB`[-1]) * (1 + i)^(-1 * set$`Loan Age`[-1])) - set$`Current UPB`[1])
}

default.npv <- function(set,i){
  PMT = sum((set$`Current UPB` * set$`Current Interest Rate`/1200)[-dim(set)[1]],
            (set$`Current UPB`[-dim(set)[1]] - set$`Current UPB`[-1]) * (1 + i)^(-1 * set$`Loan Age`[-1])) 
  nreal = as.numeric(set[dim(set)[1]-1,]$`Loan Age`)
  ti = nreal + nmonths(end = date.read(set[dim(set)[1],]$`Zero Balance Date`), start = date.read(set[dim(set)[1],]$`Last Paid Installment`)) - 1
  vit = (1 + i)^(-ti)
  OUPB = set[1,]$`Current UPB`
  CUPB = set[dim(set)[1]-1,]$`Current UPB`
  AL = set[dim(set)[1],]$`Actual Loss`
  NPV = PMT - OUPB + vit * (CUPB + AL)
  return(NPV)
}
npv = function(set, i){
  set = as.data.frame(set)
  status = classify(set)
  if (length(set) == 1){
    return("NA")
  }
  if (status == "Prepaid" | status == "Current"){
    NPV = prepaid.npv(set,i)
  }else{
    NPV = default.npv(set,i)
  }
  return(NPV)
}


#####Data Entry#####
#####Using Sample Data Instead#####
org <- read.table(file ="C:/Users/Thomas/Desktop/Data/Freddie1999/sample_orig_1999.txt",header = FALSE, sep = "|")
names = c("CreditScore",
          "FirstPmt",
          "FirstTimeHomebuyer",
          "Maturity Date",
          "MSA Code",
          "MI Percentage",
          "Number of Units",
          "Occupancy Status",
          "CLTV",
          "DTI",
          "UPB",
          "LTV",
          "Interest Rate",
          "Channel",
          "PPM",
          "Product",
          "State",
          "Property Type",
          "Postal Code",
          "Loan Sequence Number",
          "Loan Purpose",
          "Original Term",
          "Borrower Num",
          "Seller Name",
          "Servicer Name",
          "Super Conforming")
colnames(org) <- names

perf <- read.table(file ="C:/Users/Thomas/Desktop/Data/Freddie1999/sample_svcg_1999.txt",header = FALSE, sep = "|")
names = c("Sequence Number",
          "Period",
          "Current UPB",
          "Delinquincy Status",
          "Loan Age",
          "Months to Maturity",
          "Repurchased",
          "Modification",
          "Zero Balance",
          "Zero Balance Date",
          "Current Interest Rate",
          "Current Deferred UPB",
          "Last Paid Installment",
          "MI Recoveries",
          "Net Sales Proceeds",
          "Non MI Recoveries",
          "Expenses",
          "Legal Costs",
          "Maintainence and Preservation Costs",
          "Tax and Insurance",
          "Misc",
          "Actual Loss",
          "Modification Cost")
colnames(perf) = names

org1 = org[1:1000,]
which(as.character(perf$`Sequence Number`) == as.character(org[2000,]$`Loan Sequence Number`))
perf1 = perf[1:60000,]
sets1 = set.grab(orig = org1, perfo = perf1)
min = unlist(lapply(sets1, function(X){
  min(X$`Loan Age`)
}))
sets1 = sets1[which(min<2)] ; org1 = org1[which(min<2),]
npvs = lapply(sets1, function(x){
  return(npv(set = x, i = i))
})
org1 = cbind(org1,unlist(npvs))
colnames(org1)[27] = "NPV"
head(org1)
write.table(org1, file = "C:/Users/Thomas/Desktop/Data/Freddie1999/subsetNPV.csv", sep = "|")
endtime = Sys.time()
time.taken = endtime - starttime

int = cbind(org1$NPV, org1$CreditScore, org1$`MI Percentage`, org1$`Occupancy Status`, org1$CLTV, org1$DTI, org1$UPB, org1$`Interest Rate`)
pairs(int)
org1 = org1[,-26]
lm(NPV~., data = org1)
