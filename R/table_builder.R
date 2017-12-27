#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`


#' Table builder
#' 
#' @param data data. Must be in the tidy format
#' @param test statistical test that return an specific table. It works with \code{Median}, \code{Jonckheere},...     
#' @author Omar Benites
#' @importFrom tibble data_frame
#' @importFrom dplyr group_by %>% 
#' @export
#' 
tb_builder <- function(data, group, y ,test="jonk"){
  
    #sumi <- paste("sum(", y,")", sep="")
    #mean_y <- paste("mean(", y,")", sep="")
    median_y <- paste("median(", y, ",na.rm=TRUE" ,")", sep="")
    min_y <- paste("min(", y, ",na.rm= TRUE" ,")", sep="")
    max_y <- paste("max(", y, ",na.rm= TRUE" ,")", sep="")
    
    dt <- data %>% 
          group_by_(group) %>% 
          summarise_(n = ~n(), Median = median_y, Min = min_y, Max = max_y)
  dt
  #References
  #http://dplyr.tidyverse.org/articles/programming.html
  #http://rmhogervorst.nl/cleancode/blog/2016/06/13/NSE_standard_evaluation_dplyr.html  
}

