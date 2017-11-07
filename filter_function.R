##Introductory R-Script for Filtering Dynamically##
setwd("X:/PH_Desktop/M121 2017/M121_Tableau")
#Goal:
#Filter M121 Students in the following order

#MPLEX level 3 or higher
#M096/M097
##ACT 23 or Higher
#SAT 540 or higher
#Other, including M105, Missoula M115, STAT 216
#Else


#read in a dataset
library(readr); library(dplyr)
test.mat <- read_csv("Fall_16.csv" ) #reads in a tibble


M121_filter <- function(dataset ){
  
  
  #check for duplicates, and keeps only the non-duplicated observations
  test.mat.2 <- dplyr::filter(dataset,!duplicated(dataset) == TRUE)
  
  #change a couple of variable types
  test.mat.2$SECOND_TIME <- ifelse(is.na(test.mat.2$LAST_GRDE== FALSE),1,0)
  
  #create a variable that tells you where to filter
  filter <- c()
  for(j in 1:nrow(test.mat.2)){
    if (!is.na(test.mat.2$LAST_GRDE[j])) {
      filter[j] <- 1
    } else if(test.mat.2$MATH_PLCMNT[j] >= 3 & !is.na(test.mat.2$MATH_PLCMNT[j])){filter[j] <- 2
    }else if(test.mat.2$MATH_CRSE[j] %in% c(96,97)) #M096/097
    { filter[j] <- 3
    
    }else if(is.na(test.mat.2$ACT_MATH[j]) == FALSE & test.mat.2$ACT_MATH[j] >= 23) #ACT 23 or higher
    {filter[j] <- 4
    
    }else if(!is.na(test.mat.2$SAT_MATHEMATICS[j]) & test.mat.2$SAT_MATHEMATICS[j] >= 540) #SAT
    {filter[j] <- 5
    }else if(is.na(test.mat.2$STAT_TERM[j]) == FALSE) #OTHER (need to add field for Missoula transfer students)
    { filter[j] <- 6
    }else if(!is.na(test.mat.2$MATH_CRSE[j]) & test.mat.2$MATH_CRSE[j] == 105) #OTHER (need to add field for Missoula transfer students)
    { filter[j] <- 6
    }else if(!is.na(test.mat.2$MSLASUBJ[j])) #OTHER (need to add field for Missoula transfer students)
    { filter[j] <- 6
    }
    #ELSE (NOT IN ABOVE CATEGORY)
    else{filter[j] <- 0
    }
  }
  
  #put it in the test.mat.2 object
  test.mat.2$filter <- filter
  
  
  return(list(newdata = test.mat.2, filter = table(filter)))
}

#Some notes: 
#this function has been checked and validated on a test dataset with MS Excel. (10-18-17)

M121_filter(s.12)