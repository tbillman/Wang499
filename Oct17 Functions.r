set.grab <- function(org, perf){
  sets = as.list(NULL)
  a = as.character(org$`Loan Sequence Number`)
  b = as.character(perf$`Sequence Number`)
  info.matrix = outer(a,b,"==")
  sets = lapply(1:length(a),function(x){
    set = NULL
    app = setdiff(info.matrix[x,] * c(1:length(b)),0)
    set = rbind(perf[app,])
    sets[[x]] = set
  })
  return(sets)
}
start.time = Sys.time()
set1 = set.grab(org = org, perf = perf)
end.time = Sys.time()
time.taken= start.time - end.time
