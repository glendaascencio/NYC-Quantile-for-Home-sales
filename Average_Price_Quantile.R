
#Glenda Ascencio                                              August 4, 2015

### Libraries
library(dplyr)
library(ggplot2)

### Setting the work directory
setwd("~/public-schools/Glenda_R_Code")

### Loading the average prices dataframes for the shiny application
load("~/public-schools/shiny_app_df.RData")

#############################################################################
###                 Quantiles Average Prices Per School Zone             ###
#############################################################################
summary(ga$avg_price)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Maga. 
# 54.78  307.40  505.20  642.90  850.60 3804.00 
avq1 <- quantile(ga$avg_price)[2]
avq2 <- quantile(ga$avg_price)[3]
avq3 <- quantile(ga$avg_price)[4]

ga <- transform(ga, Avg_Price_Quartile = ifelse(
                                 #if
                                  avg_price <= avq1,
                                  #then
                                  "Q1",
                                  #else do another ifelse
                                  ifelse(
                                     avg_price > avq1 & avg_price <= avq2,
                                     "Q2",
                                     ifelse(
                                       avg_price > avq2 & avg_price <= avq3,
                                       "Q3",
                                       "Q4"
                                     )
                                  )
                              )
               )
##View the average price 
View(ga)

### Graph the avg price per each quantile
ggplot(data = ga, 
       aes(x = DBN, y = avg_price, color = Avg_Price_Quartile)) + 
  geom_point()+
  xlab('School Zones') + 
  ylab('Average Price') +
  ggtitle('Average Price per each quantile In Each School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

#############################################################################
###                           Median Quantile Average Price               ###
#############################################################################
summary(ga$med_avg_price)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Maga. 
# 54.78  299.00  499.40  633.40  838.70 3022.00 

ga <- transform(ga, Avg_Price_Quartile = ifelse(
  #if
  med_avg_price <= avq1,
  #then
  "Q1",
  #else do another ifelse
  ifelse(
    med_avg_price > avq1 & med_avg_price <= avq2,
    "Q2",
    ifelse(
      med_avg_price > avq2 & med_avg_price <= avq3,
      "Q3",
      "Q4"
    )
  )
)
)

### Graph the avg price per each quantile
ggplot(data = ga, 
       aes(x = DBN, y = med_avg_price, color = Avg_Price_Quartile)) + 
  geom_point()+
  xlab('School Zones') + 
  ylab('Medium Average Price') +
  ggtitle('Average Price per each quantile In Each School Zone') +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=80, hjust=1),
        legend.background = element_rect(fill = "transparent"))

##### Save the file
save(ga, file = sprintf('%s/shiny_app_df.RData', data_dir))

