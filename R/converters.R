#' Converter for agricolae's table to data.frame
#' 
#' @param adt agricolae's data table
# @param  shinyserver output
# @param session shinyserver session
# @param values reactive values
#' @author Omar Benites
#' @importFrom tibble data_frame
#' @export

agr2df <- function(adt){
  
   ranSum <- std <- r <- Min <- Max <- NULL  
   trt <- rownames(adt)
   rk  <- as.vector(adt$rankSum)
   std <- adt$std
   rep <- adt$r
   min <- adt$Min
   max <- adt$Max
   
   df <- tibble::data_frame(trt = trt, rank= rk, stdev = std, rep= rep, min = min, max = max)
   return(df)
}
  




