

list_to_df <- function(listfordf){
  if(!is.list(listfordf)) stop("it should be a list")
  n <- length(listfordf)
  strcols <- names(unlist(listfordf[[1]]))
  strrows <- names(listfordf)
  if(is.null(strrows))strrows <- 1:n
  if(n>1){
    lapply(2:n, function(i){
      strcols <<- unique(c(strcols,names(unlist(listfordf[[i]]))))
    })
  }

  df <- data.frame(matrix(0,n,NROW(strcols)))
  colnames(df) <- strcols
  rownames(df) <- strrows

  for(i in 1:n){
    x <- unlist(listfordf[[i]])
    for(j in 1:NCOL(df)){
      if(is.na(x[strcols[j]])){
        df[i,j] <- NA
      } else{
        df[i,j] <- x[strcols[j]]
      }
    }
  }

  return(df)
}
create_route_env <- function(params){
  if(!is.list(params)){
    params <- list()
  }
  route_env = list2env(params)
  return(route_env)
}
convert_to_xts <- function(candles){
  tryCatch({
    x <- data.frame(matrix(unlist(candles),ncol = 6,byrow = TRUE))
    for(i in 2:6)x[,i]<- as.numeric(as.character(x[,i]))
    x <- xts::as.xts(x[,2:6],as.POSIXct(gsub("T"," ",x[,1])))
    colnames(x) <- c("open","high","low","close","volume")
  }, error=function(e){
    message(e$message)
    stop(NoDataException)
  })
  return(x)
}
