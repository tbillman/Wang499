#####New Set Grab Idea#####
start.time = Sys.time()
library("tidyverse")
library("MASS")
library("parallel")
i = (1.0293)^(1/12) - 1
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
  if (dim(set)[1] == 1 | (!is.na(set[dim(set)[1],]$`Zero Balance`) & set[dim(set)[1],]$`Zero Balance` == 6)){
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
org <- read.delim(file ="C:/Users/Thomas/Desktop/Data/Freddie1999/sample_orig_1999.txt",header = FALSE, sep = "|", allowEscapes = T)
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

perf <- read.delim(file ="C:/Users/Thomas/Desktop/Data/Freddie1999/sample_svcg_1999.txt",header = FALSE, sep = "|", allowEscapes = T)
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
sq = as.numeric(perf$`Sequence Number`)
q = sq[-1] - sq[-length(sq)]
q = q*1:length(q)
q =c(0, q[q>0]); q = q + 1
sets = lapply(2:length(q), function(x){
  print(x)
  return(perf[q[x-1]:(q[x]-1),])
})
npvs = lapply(sets, function(x){
  print(rownames(x)[1])
  return(c(as.character(x$`Sequence Number`[1]),npv(x,i)))
})
keep<-c();j=1;
for(i in 1:dim(org)[1]){
  while(org[i,]$`Loan Sequence Number`!=npvs[[j]][1]) j=j+1;
  print(j)
  keep<-c(keep,j);
}
realnpvs = as.numeric(unlist(lapply(1:length(npvs), function(x){
  print(x)
  return(npvs[[x]][2])
})))
orgs = org[-dim(org)[1],]
#Checking on some what the NPV distribution looks like
#hist(realnpvs[which(!is.na(realnpvs))])
orgf = cbind(orgs,realnpvs[keep])
colnames(orgf) = c(colnames(org),"NPV")
write.table(orgf,file = "C:/Users/Thomas/Documents/GitHub/Wang499/OrgSampleNPVs.csv",
            row.names = F, sep = ",")
Sys.time-start.time