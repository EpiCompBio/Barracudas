parangon = function(data){
  moyenne = apply(data,2,mean)
  initialisation = sum((moyenne - data[1,])^2)
  indice = rownames(data)[1]
  for (i in 2:nrow(data)){
    if(sum((moyenne - data[i,])^2)<initialisation){
      initialisation = sum((moyenne - data[i,])^2)
      indice = rownames(data)[i]
    }
  }
  return(indice)
}