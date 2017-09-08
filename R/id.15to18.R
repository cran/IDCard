id.15to18=function(id.15){

  id.15.c=as.character(id.15)

  if(nchar(id.15.c)!=15){stop("The length of id.15 is not 15 !")}

  id.17=paste(substr(id.15.c,1,6),"19",substr(id.15.c,7,15),sep="")

  id.17.s=sum(c(as.numeric(substr(id.17,1,1))*7,
                as.numeric(substr(id.17,2,2))*9,
                as.numeric(substr(id.17,3,3))*10,
                as.numeric(substr(id.17,4,4))*5,
                as.numeric(substr(id.17,5,5))*8,
                as.numeric(substr(id.17,6,6))*4,
                as.numeric(substr(id.17,7,7))*2,
                as.numeric(substr(id.17,8,8))*1,
                as.numeric(substr(id.17,9,9))*6,
                as.numeric(substr(id.17,10,10))*3,
                as.numeric(substr(id.17,11,11))*7,
                as.numeric(substr(id.17,12,12))*9,
                as.numeric(substr(id.17,13,13))*10,
                as.numeric(substr(id.17,14,14))*5,
                as.numeric(substr(id.17,15,15))*8,
                as.numeric(substr(id.17,16,16))*4,
                as.numeric(substr(id.17,17,17))*2))

  mod=id.17.s %% 11

  if(mod==0){last=1}
  if(mod==1){last=0}
  if(mod==2){last="X"}
  if(mod==3){last=9}
  if(mod==4){last=8}
  if(mod==5){last=7}
  if(mod==6){last=6}
  if(mod==7){last=5}
  if(mod==8){last=4}
  if(mod==9){last=3}
  if(mod==10){last=2}

  id.18=paste(id.17,last,sep="")

  return(id.18)

}
