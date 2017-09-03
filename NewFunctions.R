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
prepaid.npv(set = set1, i = i)

classify <- function(set){
  if(is.na(set$`Zero Balance`[dim(set)[1]])){
    set$`CAT`[1]= "Current"
  }else{
    if(set$`Zero Balance`[dim(set)[1]] == 1){
      set$`CAT`[1] = "Prepaid"
    } else
    if((set$`Zero Balance`[dim(set)[1]]== 3) |(set$`Zero Balance`[dim(set)[1]]== 9) ){
      set$`CAT`[1] = "Default"
    } 
  }
}
