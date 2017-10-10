#####with real data#####
library("randomForest")
library("MASS")
library("parallel")
i = (1.0293)^(1/12) - 1
###############Data Entry#################
#reading in Freddie Mac Origination Data
org <-read.table(file ="C:/Users/Thomas/Desktop/Data/FreddieQ12012/FredOrig2012Q1.txt",header = FALSE, sep = "|")
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

per <- read.table(file ="C:/Users/Thomas/Desktop/Data/FreddieQ12012/Perf.txt",header = FALSE, sep = "|")
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
colnames(per) = names
#####As Lists#####
org.list = split(org, seq(nrow(org)))
org = as.list(org.list)
per.list = split(per, seq(nrow(per)))
per = as.list(per.list)
#####Functions#####
date.read <- function(yyyymm){
  as.Date(paste0(as.character(yyyymm), '01'), format='%Y%m%d')
}

nmonths <- function(end, start) {
  ed <- as.POSIXlt(end)
  sd <- as.POSIXlt(start)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon) + 1
}

pmt.calc <- function(L, r, n){
  vrn = (1+r)^(-n)
  return(L * r / (1-vrn))
}

classify <- function(set){
  if(is.na(set[[length(set)]]$`Zero Balance`)){
    return("Current")
  }else{
    if(set[[length(set)]]$`Zero Balance` == 1){
      return("Prepaid")
    } else
      if((set[[length(set)]]$`Zero Balance`== 3) |(set[[length(set)]]$`Zero Balance`== 9) ){
        return("Default")
      } 
  }
}

grab = function(orig.element ,perf.list ){
  newl = lapply(perf.list, function(x){
    a = NULL
    if(as.character(orig.element$`Loan Sequence Number`) == as.character(x$`Sequence Number`)){
      a = c(a,x)
    }
  }
  )
  return(newl[which(as.character(newl) != "NULL")])
}

prepaid.npv <- function(set,i){
  NPV = 0
  PV = 0
  for(x in 2:length(set)){
    PMT = ((set[[x-1]]$`Current UPB` * (set[[x-1]]$`Current Interest Rate`/1200) + set[[x-1]]$`Current UPB` - set[[x]]$`Current UPB`) * ((1+i)^(-1 * set[[x]]$`Loan Age`)))
    PV <- PV + PMT
  }
  NPV = PV - set[[1]]$`Current UPB` + set[[length(set)]]$`Current UPB` * (1+i)^(-length(set))
  return(NPV)
}

default.npv <- function(set,i){
  nplan = set[[length(set)-1]]$`Loan Age` + set[[length(set)-1]]$`Months to Maturity`
  nreal = as.numeric(set[[length(set)-1]]$`Loan Age`)
  t = nreal + nmonths(end = date.read(set[[length(set)]]$`Zero Balance Date`), start = date.read(set[[length(set)]]$`Last Paid Installment`)) - 1
  r =set[[length(set)-1]]$`Current Interest Rate` / 1200
  vinreal = (1 + i)^(-nreal)
  vit = (1 + i)^(-t)
  OUPB = set[[1]]$`Current UPB`
  CUPB = set[[length(set)-1]]$`Current UPB`
  pmt = pmt.calc(L = OUPB, r = r, n = nplan)
  AL = set[[length(set)]]$`Actual Loss`
  NPV = pmt * ((1-vinreal) / i) - OUPB + vit * (CUPB + AL)
  return(NPV)
}

npv = function(orig.element, perf.list, i){
  set = grab(orig.element = orig.element, perf.list = perf.list)
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

#####NPV calc#####
npvs = lapply(org, function(x){
  a = NULL
  a = c(a,npv(orig.element = x, perf.list = per, i = i))
  return(a)
}
)
j = 1
names = names = c("CreditScore",
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
                  "Super Conforming",
                  "NPV")
for (x in 1:length(npvs)){
  org[[x]] = c(org[[x]],npvs[x])
  names(org[[x]]) = names
}
orgdf  <-  as.data.frame(matrix(unlist(org), byrow = TRUE,ncol=27))
names(orgdf) = names
#####output#####
write.csv(orgdf, file = "ORGandNPV.txt", sep = "|")