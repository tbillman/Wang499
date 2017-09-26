#functions for analysis

#reading in dates
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

set.grab <- function(index){
set = NULL
for (x in 1:dim(fred.p)[1]) {
  if (as.character(fred.p[x,1]) == as.character(fred.o[index,20])){
    set = rbind(set,fred.p[x,])
    if(as.character(fred.p[x+1,1]) != as.character(fred.o[index,20])){
      return(set)
      break
    }
    }
}
return(set)
}

prepaid.npv <- function(set,i){
  NPV = 0
  PV = 0
  for(x in 2:dim(set)[1]){
    PMT = ((set$`Current UPB`[x-1] * (set$`Current Interest Rate`[x-1]/1200) + set$`Current UPB`[x-1] - set$`Current UPB`[x])*
             ((1+i)^(-1 * set$`Loan Age`[x])))
    PV <- PV + PMT
  }
  NPV = PV - set$`Current UPB`[1] + set$`Current UPB`[dim(set)[1]] * (1+i)^(-dim(set)[1])
  return(NPV)
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

grab = function(orig.list ,perf.list ,index){
  newl = lapply(perf.list, function(x){
    a = NULL
    if(as.character(orig.list[[index]]$`Loan Sequence Number`) == as.character(x$`Sequence Number`)){
      a = c(a,x)
    }
  }
  )
  return(a[which(as.character(a) != "NULL")])
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
AL = loan.def$`Actual Loss`
NPV = pmt * ((1-vinreal) / i) - OUPB + vit * (CUPB + AL)
return(NPV)
}
npv = function(orig.list, perf.list, index, i){
  set = grab(orig.list = orig.list, perf.list = perf.list, index = index)
  status = classify(set)
  if (status == "Prepaid" | status == "Current"){
    NPV = prepaid.npv(set,i)
  }else{
    NPV = default.npv(set,i)
  }
  return(NPV)
}