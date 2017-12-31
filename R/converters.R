#' Converter for agricolae's table to data.frame
#' 
#' @param adt agricolae's data table
#' @param test statistical test that return an specific table. It works with \code{fridman}, \code{kruskal},...     
#' @author Omar Benites
#' @importFrom tibble data_frame
#' @export

agr2df <- function(adt, test= "fridman"){
  
  if(test=="fridman"){
   rank <- std <- r <- Min <- Max <- NULL  
   trt <- rownames(adt)
   rk  <- as.vector(adt$rank)
   std <- adt$std
   rep <- adt$r
   min <- adt$Min
   max <- adt$Max
   
   df <- tibble::data_frame(Treatment = trt, Rank= rk, StDev = std, Rep= rep, Min = min, Max = max)
   #rename_tables(df)
  }
  
  if(test == "kruskal"){
    #rank <- std <- r <- Min <- Max <- NULL
    new_names <- c("trt", names(adt))
    trt <- rownames(adt)
    rk  <- as.vector(adt[,1])
    y_mean <- adt[,2]  
    std <- adt$std
    rep <- adt$r
    min <- adt$Min
    max <- adt$Max
  
   df <- tibble::data_frame(Treatment = trt, Rank= rk, Mean = y_mean, StDev = std, Rep= rep, Min = min, Max = max)
  }
  
  if(test=="durbin"){
  
    new_names <- c("trt", names(adt))
    trt <- rownames(adt)
    y_mean <- as.vector(adt[,1])
    
    std <- adt$std
    rep <- adt$r
    min <- adt$Min
    max <- adt$Max
    
    df <- tibble::data_frame(Treatment = trt, Mean = y_mean, StDev = std, Rep= rep, Min = min, Max = max)
      
    
  }
  
  
  
  
   return(df)
}
  

#' Rename tables
#' 
#' @param dt data table
#' @param new_names new column names
#' @author Omar Benites
#' @importFrom data.table setnames
#' @export

rename_tables <- function(dt, new_names){
  
  old_names <- colnames(dt)
  dt <- data.table::setnames(x = dt, old = old_names,  new= new_names )
  
}





