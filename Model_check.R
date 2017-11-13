#Function that checks model assumptions
#paul Harmon

#idea: function that rewrites the plot.lm() function to produce nice looking plots


model_check <- function(lm.all, plot_number = 'ALL'){
  
  #loads the packages necessary
  if(require('ggplot2')==FALSE){install.packages('ggplot2')}
  library(ggplot2)
  
  if(require('gridExtra')==FALSE){install.packages('gridExtra')}
  library(gridExtra)
  
  
  
  diagnostic.df <- as.data.frame(cbind(resid(lm.all),fitted(lm.all),rstudent(lm.all),rstandard(lm.all)))
  names(diagnostic.df) <- c("resid","fitted","student","standard")
  
  
  #QQplot
  resid_plot <- ggplot(diagnostic.df) + geom_point(aes(x = fitted, y = resid)) + geom_smooth(aes(x = fitted, y = resid),method = 'gam') + ggtitle("Residual Plot") + theme_classic()
  
  
  
  qq_plot <- ggplot(diagnostic.df) + geom_abline(aes(slope = 1, intercept = 0),color = "red", size = 2) + geom_qq(aes(sample = standard)) + ggtitle("ggQQ Plot") + theme_classic()
  
  
  scale_loc <- ggplot(diagnostic.df,aes(x = fitted, y = sqrt(standard))) + geom_point() + geom_smooth(method = 'gam',fill = NA, color = 'orange',size = 1.5) + theme_classic()
  
  library(grDevices)
  
  outlier_plot <- ggplot(diagnostic.df,aes(x = hatvalues(lm.all), y = standard)) + 
    geom_point(color = ifelse(cooks.distance(lm.all)>4/lm.all$df.residual,"tomato","gray30")) + 
    geom_smooth(method = 'gam', fill = NA, color = "lightblue") + theme_classic() + 
    ggtitle("Outlier Check") + xlab("Leverage") + ylab("Standardized Residuals")
  
  plotlist <- list(resid_plot,qq_plot,scale_loc,outlier_plot)
  
  
  
  
  if(plot_number == 'ALL'){
    return(grid.arrange(resid_plot,qq_plot,scale_loc,outlier_plot, ncol=2))}
  else {
    return(plotlist[plot_number])}
  
}

#test cases
lm.all <- lm(Sepal.Length ~ Sepal.Width, data = iris)
model_check(lm.all)
model_check(lm.all,2)
