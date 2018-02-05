#investigation
#there seems to be 32793 unique loan sequence numbers for org
#but there are 50000 in the performance data?
#also their heads and tails are the same
#what gives?
os = as.character(unique(org$`Loan Sequence Number`))
ps = as.character(unique(perf$`Sequence Number`))
osn =sapply(os, function(x){
  return(substr(x,6,12))
})
psn = sapply(ps, function(x){
  return(substr(x,6,12))
})
osn = as.numeric(osn); psn = as.numeric(psn)
par(mfrow = c(2,1))
hist(osn)
hist(psn)
length(psn)
length(osn)
dif = sapply(osn, function(x){
  print(x)
  mch =  which(psn == x)
  if(length(mch) == 1){
    return(NA)
  } else return(x)
})
dif[!is.na(dif)]
a = sapply(diff,is.null)
length(a)
which(a == FALSE)
matched = match(osn, psn)
special = psn[which(is.na(matched) * 1:length(matched) != 0)]
which(is.na(matched) * 1:length(matched) != 0)[1]
sets[[450]]
special[1]
