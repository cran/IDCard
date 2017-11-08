id.15to18=function(id.15){

  if(length(id.15)==1){
    rst=id.15to18.single(id.15)
  }
  if(length(id.15)>1){
    rst=id.15to18.single(id.15[1])
    for(i in 2:length(id.15)){
      rst=c(rst,id.15to18.single(id.15[i]))
    }
  }
  return(rst)
}


