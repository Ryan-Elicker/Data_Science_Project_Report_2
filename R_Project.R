# Importing libraries. 
library(readr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)

# Importing the data frames.
yelp_user <- read_csv("R_In_Class/OA 11.6 - yelp_academic_dataset_user.json.csv")
View(yelp_user)
yelp_business <- read_csv("R_In_Class/OA 11.7 - yelp_academic_dataset_business.json.csv")
View(yelp_business)

# Prints out the column names.
names(yelp_user)

# This prints a correlation matrix showing the correlations to 
# evaluate the relationships and all of the other relation related data.
corr_matrix <- cor(yelp_user[sapply(yelp_user,is.numeric)])
View(corr_matrix)

# This is a linear model of funny votes vs cool votes.
linear_model <- lm(yelp_user$funny_votes~yelp_user$cool_votes)
print(linear_model)

# This makes a scatter plot and linear regression line.
ggplot(yelp_user) + geom_point(aes(x = cool_votes, y = funny_votes)) +
     geom_smooth(aes(x=cool_votes, y=funny_votes), method="lm", se=F) +
     labs(x="Cool Votes", y="Funny Votes")
# This defines a string variable and then prints it.
equation <- "y = 0.8733x +0.2916" 
print(equation)

# This makes a linear model, but with fans and review count.
linear_model <- lm(yelp_user$fans~yelp_user$review_count)
print(linear_model)

# This makes a scatter plot and linear regression line.
ggplot(yelp_user) + geom_point(aes(x = review_count, y = fans)) + 
      geom_smooth(aes(x = review_count, y = fans), method = "lm", se=F) +
      labs(x = "Number of Reviews", y = "Number of Fans")
# Yes, generally as the number of reviews increases, the 
# number of fans does as well, as shown by the positive slope.

# Linear model, but with fans and useful votes.
linear_model <- lm(yelp_user$fans~yelp_user$useful_votes)
print(linear_model)

# This makes a scatter plot and linear regression line.
ggplot(yelp_user) + geom_point(aes(x = useful_votes, y = fans)) +
      geom_smooth(aes(x = useful_votes, y = fans), method = "lm", se=F) +
      labs(x = "Number of Useful Votes",y = "Number of Fans")
# I used a linear regression again to find that the slope is still
# positive when number of useful votes is used against number of fans,
# meaning that they are still related.

# This makes the data numeric only.
user_data <- yelp_user[, c("review_count", "average_stars", "cool_votes", "funny_votes", "useful_votes", "fans")]

# Everything between here and the next comment makes an elbow plot to tell the ideal number of headers.
wcss <- function(k){
  kmeans(user_data, centers = k)$tot.withinss
}

k_values <- 1:10

wcss_values <- sapply(k_values, wcss)
print(wcss_values)
elbow_plot <- data.frame(k = k_values, wcss = wcss_values)
View(elbow_plot)

ggplot(elbow_plot, aes(x = k, y = wcss)) + geom_line() + geom_point()

# This used k-means and I think it works as intended.
user_cluster <- kmeans(yelp_user[,3, 11], 4)
yelp_user$cluster <- as.factor(user_cluster$cluster)
ggplot(yelp_user) + geom_point(aes(x=fans, y=review_count, color=cluster))
# I decided to use four clusters because the elbow plot showed that would
# be the best number.

# This is another k-means cluster defined scatter plot.
user_cluster <- kmeans(yelp_user[,7, 11], 4)
yelp_user$cluster <- as.factor(user_cluster$cluster)
ggplot(yelp_user) + geom_point(aes(x=fans, y=useful_votes, color=cluster))
# I chose to use four clusters because the elbow plot implied that 
# would be the best number to choose.
