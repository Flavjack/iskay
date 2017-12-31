#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`


#' Summary table
#' 
#' @param data data. Must be in the tidy format
#' @param group group by
#' @param y response variable
#' @param test statistical test that return an specific table. It works with \code{Median}, \code{Jonckheere},...     
#' @author Omar Benites
#' @importFrom tibble data_frame
#' @importFrom dplyr group_by_ summarise_ n
#' @export
#' 
test_summary <- function(data, group, y, test="jonk"){
  
  #sumi <- paste("sum(", y,")", sep="")
  #mean_y <- paste("mean(", y,")", sep="")
  median_y <- paste("median(", y, ",na.rm=TRUE" ,")", sep="")
  min_y <- paste("min(", y, ",na.rm= TRUE" ,")", sep="")
  max_y <- paste("max(", y, ",na.rm= TRUE" ,")", sep="")
    
  dt <- data %>% 
          group_by_(group) %>% 
          summarise_(n = ~n(), Median = median_y, Min = min_y, Max = max_y)
  return(dt)
  #References
  #http://dplyr.tidyverse.org/articles/programming.html
  #http://rmhogervorst.nl/cleancode/blog/2016/06/13/NSE_standard_evaluation_dplyr.html  
}

#' Statistical tables from non-parametric analysis
#' @title Return tables (data frames) derived from statistical analysis
#' @param x independent variable
#' @param y dependent variable
#' @param hyp hypothethical testing
#' @param jud judges.
#' @param param statistical parameter such as \code{mu}, \code{median}, etc.
#' @param test statistical test that return an specific table. It works with \code{Median}, \code{Jonckheere},...
#' @param sg Significant level. By defualt \code{alpha=0.05}
#' @param comparison Wheter calculate comparison between samples means by treatment.
#' @author Omar Benites
#' @importFrom tibble data_frame
#' @importFrom dplyr group_by left_join select_
#' @importFrom broom glance
#' @importFrom agricolae kruskal friedman Median.test 
#' @importFrom exactRankTests wilcox.exact
#' @importFrom clinfun jonckheere.test
#' @importFrom rcompanion pairwiseMedianTest cldList 
#' @export
#' 

test_analysis <- function(x , y, hyp, param, jud, test = "friedman", sg = 0.05, comparison=FALSE) {
  
    if(test=="friedman"){
      
      if(comparison == FALSE){
        outfrim <- friedman( judge = jud, trt = x, evaluation = y, group=TRUE, alpha = sg)
      }
      if(comparison){
        outfrim <- friedman( judge = jud, trt = x, evaluation = y, group=FALSE, alpha = sg)
      }
      
      dtfrimeans  <- agr2df(outfrim$means)
      #print(dtfrmeans)
      groupfri <- rename_tables(outfrim$groups, c("Treatment", "Rank", "Groups")) %>% 
                  select_(-2) #remove sum of ranks columns  
      #lef join by trt fo  r merging groups column
      dt <- dplyr::left_join(dtfrimeans, groupfri, by="Treatment")
      statistic <- outfrim$statistic
      parameter <- outfrim$parameters
      comparison <- outfrim$comparison
      
      #dt: table of means and letters for significance differences
      out <- list(dt= dt, statistic = statistic, parameter = parameter, comparison= comparison )
    }
    
    if(test == "kruskal"){
      
      if(comparison == FALSE){
        outkru <- kruskal(y = y, trt = x, group = TRUE,alpha = sg)
      }
      if(comparison){
        outkru <- kruskal(y = y, trt = x, group = FALSE,alpha = sg)
      }
      
      dtkrumeans <- agr2df(outkru$means,test = "kruskal")
      groupkru <- rename_tables(outkru$groups, c("Treatment", "Means", "Groups")) %>% 
                  select_(-2) #remove sum of ranks columns  
      #lef join by trt for merging groups column
      dt <- dplyr::left_join(dtkrumeans, groupkru, by="Treatment")
      statistic <- outkru$statistic
      parameter <- outkru$parameters
      comparison <- outkru$comparison
      out <- list(dt= dt, statistic = statistic, parameter = parameter, comparison= comparison)
    }
  
    if(test == "jonckheere"){
      #ToDo: g = x need to be numeric
      #0.0 ---> In this case x =y (response variable) and g =x (independent variable) 
      outjonck <- jonckheere.test(x= y, g= x, alternative = hyp)
      statistic <- broom::glance(outjonck)
      out <- list(dt=NULL, statistic = statistic, parameter = NULL, comparison=NULL)
    }  
  
    if(test == "median"){
      
      out <- Median.test(y = y, trt = x, console=TRUE) 
      outmed <- out$Medians %>%  
                rename_tables(c("Treatment", "Median", "grather", "lessEqual"))  
      #in this case x = y (response variable) , g : explanatory variable
      pwmed <- rcompanion::pairwiseMedianTest(x = y, g = x, method = "fdr")
      pwmed <- rcompanion::cldList(p.adjust ~ Comparison,
                                   data = pwmed, threshold = sg) %>% 
               select_(-3) %>% 
               rename_tables(c("Treatment","Significance"))    
      
      dt <- dplyr::left_join(outmed, pwmed, by="Treatment")
      
      statistic <- outmed$statistics
      parameter <- outmed$parameters
      comparison <- outmed$comparison
      
      out <- list(dt= dt, statistic = statistic, parameter = parameter, comparison= comparison)
      
    }
  
    if(test == "durbin"){
      
      if(comparison == FALSE){
        outdurbin <-durbin.test(judge = jud, trt = x, evaluation = y, group = TRUE, alpha = sg)
      }
      if(comparison == TRUE){
        outdurbin <-durbin.test(judge = jud, trt = x, evaluation = y, group = FALSE)
      }
      
      dtdurbin  <- agr2df(outdurbin$means,test = "durbin")
      #print(dtfrmeans)
      rankdurbin <- rename_tables(outdurbin$rank, c("Treatment", "Rank"))# %>% 
                      #select_(-2) #remove sum of ranks columns  
      #lef join by trt fo  r merging groups column
      dt <- dplyr::left_join(dtdurbin, rankdurbin, by="Treatment")
      
      statistic <- outdurbin$statistics
      parameter <- outdurbin$parameters
      comparison <- outdurbin$comparison
      
      out <- list(dt= dt, statistic = statistic, parameter = parameter, comparison= comparison )
    }
  
  
  
    if(test == "manwithney"){
      outmanw <- wilcox.exact(x =  x, y = y, alternative = hyp, mu= param )
      statistic <- broom::glance(outmanw)
      out <- list(dt=NULL, statistic = statistic, parameter = NULL, comparison=NULL)
      
    }
  
    if(test == "wilcoxon"){
      outwilcox <- wilcox.exact(y ~ x, mu= param, alternative = hyp, paired=TRUE)
      statistic <- broom::glance(outwilcox)
      out <- list(dt=NULL, statistic = statistic, parameter = NULL, comparison=NULL)
    }  
  
  out <- out  
  invisible(out)
}

  
