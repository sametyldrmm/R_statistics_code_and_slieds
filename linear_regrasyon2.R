library(ggplot2)  # ggplot2 kütüphanesini kullanılabilir hale getir
library(dplyr)    # dplyr kütüphanesini kullanılabilir hale getir
all_data <- read.csv("2.csv", sep = ",", fileEncoding = "UTF-8")


# Sale ID,Date,AveragePrice,TotalAvocados,Small 4046,Extra Large 4770,Large 4225,Total Bags,Small Bags,Large Bags,XLarge Bags,type,Cities hepsi tek tek factore değiştirilicek

is.factor(all_data$type)
all_data$type <- as.factor(all_data$type)
summary(all_data$type)

is.factor(all_data$Cities)
all_data$Cities <- as.factor(all_data$Cities)
summary(all_data$Cities)

all_data_type = sapply(all_data, class)


conventional <- subset(all_data, type == "conventional")
conventional$AveragePrice <- as.numeric(gsub("\\$","",conventional$AveragePrice))

y <- conventional$AveragePrice
x <- conventional$TotalAvocados

conventional_linear_regression <- lm(y ~ poly(x, 2, raw = TRUE))
summary(conventional_linear_regression)
# Polinom katsayılarını elde etme
coefficients <- coef(conventional_linear_regression)