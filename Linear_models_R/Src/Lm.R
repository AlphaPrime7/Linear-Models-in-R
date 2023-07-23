#Theme: Linear models and data exploration
#Author: Tingwei Adeck
#Input data: data.Q3.csv
#Output: 
#1. R function called unique identifier developed while working on the project
#2. Two LM plots with one fitted with outlier containing data vs no outlier data
#Set wd
getwd()
setwd("~/rlearn")
# some fun with the lattice package, another great visualization tool
install.packages('outliers')
data.Q3 = read.csv(file = "data.Q3.csv", head=TRUE)
library(lattice)
library(outliers)
bwplot(Y ~ X, data = data.Q3)
qqnorm(data.Q3$X, main = "QQplot$X")
qqline(data.Q3$X)
qqnorm(data.Q3$Y, main = "QQplot$Y")
qqline(data.Q3$Y) #test normality of variables (x,y)
q3lm <- lm(Y ~ X, data = data.Q3)
plot(q3lm)
cooksd <- cooks.distance(q3lm) #what is cooks distance

#test relationship of pairs when dealing with multi-regressions. 
#Test for co linearity
pairs(Y ~ X, data = data.Q3) 

plot(cooksd) #find outliers clearly from this plot
abline(h = 4*mean(cooksd, na.rm=T), col="red")

#Extract and see the outlier values
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
print(influential)
car::outlierTest(q3lm) #test for outlier significance
head(data.Q3[influential, ])

#Remove outliers
data.Q3_nooutl <- data.Q3[-influential, ]
data.Q3treated <- rm.outlier(data.Q3, fill = FALSE, median = FALSE, opposite = FALSE)
plot(data.Q3_nooutl)

#convert the cooksd list into a matrix for writing a function to extract the outlier
cd_df <- as.data.frame(cooksd)
cd_df$unique_id <- c(1:nrow(cd_df)) #Create this column using automation
print(cd_df)

#an alternative way to create a unique id with inbuilt code. Think of this math, so many roads to ROME
unique_identifier <- function(df){
  for(i in 1:nrow(df)){
    x <- 0
    x <- x + i
    df$unique_id[i] <- x
  }
  return(df)
}
cd_df <- unique_identifier(cd_df)
print(cd_df)

#function to see if cd algorithm can be replicated
#utilization of a vector to get outlier values (cooksd)
#to get the data corresponding to the cooksd will be to iterate through the data and pass the
#value of the condition of equality of boxplot$out data to actual data, to get the desired values
cd_vector <- as.vector(cooksd)
out_vector <- c() #create an empty vector
for(j in cd_vector){
  if(j > 4*mean(cd_vector)){
    out_vector=c(out_vector, j)
  }
}
print(out_vector) #this is an important developmental step for me. see how an R data structure works and how a loop can be applied to solve a problem

#Another very simple way to get the outliers and location is to use inbuilt R
outliers_df <- cd_df[cd_df$cooksd > (4*mean(cd_df$cooksd)),]
print(outliers_df)

#New model with outlier removed
q3lmtreated <- lm(Y ~ X, data = data.Q3treated)

#Final plot of cleaned up data compared to outlier data using gridExtra
install.packages('gridExtra')
library(ggplot2)
require(gridExtra)
clean_plot <- ggplot(data.Q3treated, aes(x=X, y=Y)) + geom_point(size=2, shape=25) + geom_smooth(method=lm) + labs(title = "Y vs X") + stat_ellipse()
dirty_plot <- ggplot(data.Q3, aes(x=X, y=Y)) + geom_point(size=2, shape=25) + geom_smooth(method=lm) + labs(title = "Y vs X") + stat_ellipse()
grid.arrange(clean_plot, dirty_plot, ncol=2)
