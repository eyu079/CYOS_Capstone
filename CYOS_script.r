
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

#Load data set
df <- read.csv("nyc-rolling-sales.csv", header = TRUE)

#It's interesting to see which borough had the most amount of 
#Manhattan (1), Bronx (2), Brooklyn (3), Queens (4), and Staten Island (5).
df %>% ggplot() + 
  geom_bar(aes(BOROUGH))

#remove commercial units and only focus on residential units
df <- filter(df, COMMERCIAL.UNITS == 0)
df <- filter(df, RESIDENTIAL.UNITS > 0)

#clean data (change to numeric, remove transfer " - " values)
df <- df %>%  mutate(SALE.PRICE = strtoi(str_replace(SALE.PRICE, " -  ", "0")))
mean(df$SALE.PRICE, na.rm=TRUE)

df <- df %>%  mutate(GROSS.SQUARE.FEET= strtoi(str_replace(GROSS.SQUARE.FEET, " -  ", "0")))
mean(df$GROSS.SQUARE.FEET, na.rm=TRUE)

df %>% ggplot(aes(GROSS.SQUARE.FEET,SALE.PRICE),na.rm = TRUE) + 
  geom_point(alpha = 0.5) 

#might be of interest to remove the out lyers where there is price that is greater 
#than 500*10^6 and square footage greater than 2000*10^3

row.zero.price <- which(df$SALE.PRICE>100 & df$GROSS.SQUARE.FEET > 40)
df<-df[row.zero.price,]

#remove any of the elements of the dataset which are not needed
df <- subset(df,select=-c(NEIGHBORHOOD, ZIP.CODE, BUILDING.CLASS.CATEGORY,
                           TAX.CLASS.AT.PRESENT,BLOCK, EASE.MENT, BUILDING.CLASS.AT.PRESENT,
                           ADDRESS, APARTMENT.NUMBER, TAX.CLASS.AT.TIME.OF.SALE,
                           BUILDING.CLASS.AT.TIME.OF.SALE))

set.seed(1)
y <- df$SALE.PRICE
index <- createDataPartition(y , p = 0.5, list = FALSE)
test <- df[index,]
train <- df[-index,]
sale.price.test <- df$SALE.PRICE[-index]

#Linear model (1) is using only gross square feet (2) is using both
#borough information and the gross square feet to improve the modeling
lm_model1 <- lm(sale.price.test ~ GROSS.SQUARE.FEET, data = train)

m <- mean(train$SALE.PRICE)
sqrt(mean((m - test$SALE.PRICE)^2))

y_hat <- predict(lm_model1, test)
sqrt(mean((y_hat - test$SALE.PRICE)^2))

lm_model2 <- lm(sale.price.test ~ GROSS.SQUARE.FEET + BOROUGH, data = train)

y_hat <- predict(lm_model2, test)
sqrt(mean((y_hat - test$SALE.PRICE)^2))

#Second model is using nearest neighbors, first is to do cross validation to find the nearest neighbor
train_knn_xval <- train(SALE.PRICE ~ GROSS.SQUARE.FEET, data = train, method = "knn",
                   tuneGrid = data.frame(k = seq(5, 100, 10)))
y_hat <- predict(train_knn_xval, test)
sqrt(mean((y_hat - test$SALE.PRICE)^2))
ggplot(train_knn_xval, highlight = TRUE)

#plug in the nearest neighbor of k = 65 to give best results
train_knn <- train(SALE.PRICE ~ GROSS.SQUARE.FEET, data = train, method = "knn",
                   tuneGrid = data.frame(k = 65))

y_hat <- predict(train_knn, test)
sqrt(mean((y_hat - test$SALE.PRICE)^2))


