library(ggplot2)
library(dplyr)

all_data <- read.csv("2.csv", sep = ",", fileEncoding = "UTF-8")

is.factor(all_data$type)
all_data$type <- as.factor(all_data$type)
summary(all_data$type)

is.factor(all_data$Cities)
all_data$Cities <- as.factor(all_data$Cities)
summary(all_data$Cities)

all_data_type <- sapply(all_data, class)

organic <- subset(all_data, type == "organic")
organic$AveragePrice <- as.numeric(gsub("\\$","",organic$AveragePrice))

y <- organic$AveragePrice
X <- organic$TotalAvocados

# Veri setini birleştirme
data <- data.frame(y = y, X = X)

# ANOVA analizi
result <- aov(y ~ X, data = data)

# Sonuçları görüntüleme
print(summary(result))

