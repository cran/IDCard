is.id=function(id,code){

  rst=data.frame(id,length=TRUE,area=TRUE,birth=TRUE,last3=TRUE)
  rst$id=as.character(rst$id)

  for(i in 1:nrow(rst)){
    #length
    if(nchar(rst[i,"id"])!=15 & nchar(rst[i,"id"])!=18){
      rst[i,"length"]=FALSE
      rst[i,"area"]=FALSE
      rst[i,"birth"]=FALSE
      rst[i,"last3"]=FALSE
    }

    #area code
    if(nchar(rst[i,"id"])==15 | nchar(rst[i,"id"])==18){
      if(!any(as.numeric(substr(rst[i,"id"],1,6)) %in% code)){
        rst[i,"area"]=FALSE
        rst[i,"birth"]=FALSE
        rst[i,"last3"]=FALSE
      }
    }

    #birthdate
    if(nchar(rst[i,"id"])==15){
      birth=paste("19",substr(rst[i,"id"],7,8),"-",substr(rst[i,"id"],9,10),"-",
                  substr(rst[i,"id"],11,12),sep="")
      birth=as.Date(birth,"%Y-%m-%d")
      if(is.na(birth)){
        rst[i,"birth"]=FALSE
        rst[i,"last3"]=FALSE
      }else{
        today=as.Date(format(Sys.time(), "%Y-%m-%d"),"%Y-%m-%d")
        if(difftime(today,birth,units="days")<0){
          rst[i,"birth"]=FALSE
          rst[i,"last3"]=FALSE
        }
      }
    }
    if(nchar(rst[i,"id"])==18){
      birth=paste(substr(rst[i,"id"],7,10),"-",substr(rst[i,"id"],11,12),"-",
                  substr(rst[i,"id"],13,14),sep="")
      birth=as.Date(birth,"%Y-%m-%d")
      if(is.na(birth)){
        rst[i,"birth"]=FALSE
        rst[i,"last3"]=FALSE
      }else{
        today=as.Date(format(Sys.time(), "%Y-%m-%d"),"%Y-%m-%d")
        if(difftime(today,birth,units="days")<0){
          rst[i,"birth"]=FALSE
          rst[i,"last3"]=FALSE
        }
      }
    }

    #identical code
    if(nchar(rst[i,"id"])==15){
      last=str_extract(paste(substr(rst[i,"id"],13,15)),"[0-9]{1,}")
      if(nchar(last)<3){rst[i,"last3"]=FALSE}
    }
    if(nchar(rst[i,"id"])==18){
      last=str_extract(paste(substr(rst[i,"id"],13,15)),"[0-9]{1,}")
      if(nchar(last)<3){rst[i,"last3"]=FALSE}
    }

  }
  return(rst)
}


