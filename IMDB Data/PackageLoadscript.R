InstallAndOrLoad <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}



listOfRequiredPackages <- c("syuzhet","data.table","plyr","magrittr"
                            ,"openxlsx","dplyr","tidyr","rvest","stringr"
                            ,"wordcloud","tm","twitteR","ROAuth","base64enc",
                            "ggplot2","ggrepel","ggthemes","scales","car","GGally"
                            ,"VIM","formattable","caret","plotly","corrplot")




InstallAndOrLoad(listOfRequiredPackages)