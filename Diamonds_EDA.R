library(ggplot2)
library(tidyverse)

# 1 ----------------------------------------------------------------------------
str(diamonds)
head(diamonds)

# 2 ----------------------------------------------------------------------------

# 2.1 Plot price and weight against the physical ordering of the data
# Price
diamonds %>% 
  ggplot(aes(x=1:nrow(diamonds), y=price)) + 
  geom_point() +
  ggtitle("Price Vs Physical Ordering") + 
  xlab("Row Index") + 
  ylab("Price")
# The graph gets a little clearer if we make y a log scale.
diamonds %>% 
  ggplot(aes(x=1:nrow(diamonds), y=log(price))) + 
  geom_point()+
  ggtitle("log(Price) Vs Physical Ordering") + 
  xlab("Row Index") + 
  ylab("log(Price)")
# Which prices are missing?
diamonds %>% 
  ggplot(aes(x=1:nrow(diamonds), y=log(price))) + 
  geom_point() +
  scale_x_continuous(limits=c(43700,44100)) +
  ggtitle("log(Price) Vs Physical Ordering (Subset)") + 
  xlab("Row Index") + 
  ylab("log(Price)")
# Weight
diamonds %>% 
  ggplot(aes(x=1:nrow(diamonds), y=log(carat)))+
  geom_point()+
  ggtitle("log(Carat) Vs Physical Ordering") + 
  xlab("Row Index")+
  ylab("log(Carat)")
# Subset
diamonds %>% 
  ggplot(aes(x=1:nrow(diamonds),y=log(carat))) +
  geom_point() +
  scale_x_continuous(limits=c(43700,44100)) +
  ggtitle("log(Carat) Vs Physical Ordering (Subset)") + 
  xlab("Row Index") + 
  ylab("log(Carat)")

# 2.2 Checking consistency of x, y, and z measurements with weight (carat)
diamonds %>% 
  ggplot(aes(x=log(carat), y=log(x*y*z))) + 
  geom_point() +
  ggtitle("log(x*y*z) against log(Carat)") + 
  xlab("log(carat)") + 
  ylab("log(x*y*z)")
# What data-entry error has happened?
vol <- diamonds$x * diamonds$y * diamonds$z
outlier_index <- which.max(vol)
# Outlier data
diamonds[outlier_index, ]

# 2.3 Studying the distributions of the variables
diamonds %>%
  ggplot(aes(x=carat)) + 
  geom_histogram() +
  ggtitle("Distribution of Weight")

diamonds %>%
  ggplot(aes(x=carat, fill=clarity)) + 
  geom_histogram(center=0.01,binwidth=0.01)

caratbreaks <- seq(0.2,5.2,by=0.1)
diamonds %>%
  ggplot(aes(x=carat, fill=clarity)) + 
  geom_histogram(center=0.01,binwidth=0.01) +
  scale_x_continuous(breaks=caratbreaks) + 
  ggtitle("Distribution of carats from 0.2 to 1.5") + 
  xlim(0.2, 1.5)

# 2.4 Checking the distribution of prices
diamonds %>%
  ggplot(aes(x=price, fill=clarity)) + 
  geom_histogram(bins=300) +
  ggtitle("Distribution of Price with bins = 300") +
  scale_x_continuous(minor_breaks=seq(500,15000, by=100), limits=c(1000,2000))
  
# 3 ----------------------------------------------------------------------------

diamonds %>%
  ggplot(aes(x=carat, y=price)) + 
  geom_point() +
  ggtitle("Price versus Weight in Carats")

# 3.1 According to Messing’s theory, price is proportional to exp (C x weight1/3).
#     Is this plausible?
diamonds %>%
  ggplot(aes(x=carat^(1/3), y=price)) + 
  geom_point() +
  ggtitle("Price versus Weight^(1/3)")

# log scale
diamonds %>%
  ggplot(aes(x=carat^(1/3), y=log(price))) + 
  geom_point() +
  ggtitle("log(Price) versus Weight^(1/3)")

# 3.2 Might a power law fit the data better?

diamonds %>%
  ggplot(aes(x=log(carat), y=log(price))) + 
  geom_point() +
  ggtitle("log(Price) versus log(Carat)")


# 4 ----------------------------------------------------------------------------

diamonds %>%
  ggplot(aes(x=color, fill=color)) + 
  geom_bar() +
  ggtitle("Distribution of Color")

diamonds %>%
  ggplot(aes(x=clarity, fill=clarity)) + 
  geom_bar() +
  ggtitle("Distribution of Clarity")

diamonds %>%
  ggplot(aes(x=cut, fill=cut)) + 
  geom_bar() +
  ggtitle("Distribution of Cut")
  

# 5 ----------------------------------------------------------------------------

# 5.1
# Price Vs Clarity
diamonds %>%
  ggplot(aes(x=clarity, y=price)) +
  geom_boxplot() +
  stat_boxplot(geom ="errorbar", width = 0.5) + 
  ggtitle("Price versus Clarity with Whiskers")

diamonds %>%
  ggplot(aes(x=clarity, y=price)) +
  geom_violin() +
  ggtitle("Price versus Clarity (Violin)")

# Price Vs Color
diamonds %>%
  ggplot(aes(x=color, y=price)) +
  geom_boxplot() +
  stat_boxplot(geom ="errorbar", width = 0.5) + 
  ggtitle("Price versus Color with Whiskers")

diamonds %>%
  ggplot(aes(x=color, y=price)) +
  geom_violin() +
  ggtitle("Price versus Color (Violin)")

# Price Vs Color
diamonds %>%
  ggplot(aes(x=cut, y=price)) +
  geom_boxplot() +
  stat_boxplot(geom ="errorbar", width = 0.5) + 
  ggtitle("Price versus Cut with Whiskers")

diamonds %>%
  ggplot(aes(x=cut, y=price)) +
  geom_violin() +
  ggtitle("Price versus Cut (Violin)")

# 5.2 Does the distribution of clarity, color and cut change with carat-weight?

caratfactor <- cut(diamonds$carat, quantile(diamonds$carat), include.lowest=TRUE)
str(caratfactor)

diamonds$caratfactor <- caratfactor
str(diamonds)

diamonds %>%
  ggplot(aes(x=caratfactor, fill=clarity)) + 
  geom_bar(position="fill") + 
  ggtitle("Clarity versus Caratfactor")

diamonds %>%
  ggplot(aes(x=clarity, fill=caratfactor)) + 
  geom_bar(position="fill") + 
  ggtitle("Caratfactor versus Clarity")

# 5.3 The effect of clarity, color, and cut on price: second attempt

halfcaratdiamonds <- subset(diamonds,carat > 0.49 & carat < 0.55 )
onecaratdiamonds <- subset(diamonds, carat > 0.99 & carat < 1.2 )
str(onecaratdiamonds)

halfcaratdiamonds %>%
  ggplot(aes(x=clarity,y=price)) + 
  geom_boxplot() + 
  ggtitle("Price distribution for Clarity")

halfcaratdiamonds %>%
  ggplot(aes(x=color,y=price)) + 
  geom_boxplot() + 
  ggtitle("Price distribution for Colour")
  
halfcaratdiamonds %>%
  ggplot(aes(x=cut,y=price)) + 
  geom_boxplot() + 
  ggtitle("Price distribution for Cut")


# 6 ----------------------------------------------------------------------------

diamonds %>%
  ggplot(aes(x=log(price), y=log(x*y*z), color= clarity)) +
  geom_point() +
  ggtitle("log(x*y*z) Vs log(Price)")

