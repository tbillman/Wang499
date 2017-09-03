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
  if (as.character(fred.p[x,index]) == as.character(fred.o[index,20])){
    set = rbind(set,fred.p[x,])
    }
  }
}
set1 = set.grab(1)
