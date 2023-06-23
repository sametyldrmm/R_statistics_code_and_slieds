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

conventional <- subset(all_data, type == "conventional")
conventional$AveragePrice <- as.numeric(gsub("\\$","",conventional$AveragePrice))

y <- conventional$AveragePrice
X <- conventional$TotalAvocados

# Veri setini birleştirme
data <- data.frame(y = y, X = X)

# ANOVA analizi
result <- aov(y ~ X, data = data)

# Sonuçları görüntüleme
print(summary(result))

# Bu çıktı, ANOVA analizinin sonuçlarını göstermektedir. İlgili çıktıda aşağıdaki bilgiler yer almaktadır:

# Df (Degree of Freedom): Serbestlik derecesi, modeldeki bağımsız değişkenin sayısıdır.
# Sum Sq (Sum of Squares): Kareler toplamı, değişkenler arasındaki toplam varyansın ölçüsüdür.
# Mean Sq (Mean Square): Ortalama kareler, ilgili varyansın ortalama değeridir (Kareler toplamı / Serbestlik derecesi).
# F value: F değeri, varyans analizi test istatistiğidir ve değişkenler arasındaki farkın anlamlılığını gösterir.
# Pr(>F): F değerine karşılık gelen p değeri, test istatistiğinin anlamlılığını belirtir. Küçük bir p değeri, değişkenler arasındaki farkın istatistiksel olarak anlamlı olduğunu gösterir.
# Çıktıdaki *** sembolü, p değerinin oldukça düşük olduğunu ve değişkenler arasındaki farkın istatistiksel olarak anlamlı olduğunu göstermektedir. Bu durumda, X değişkeninin AveragePrice üzerinde anlamlı bir etkisi olduğu söylenebilir.

# Özetle, ANOVA analizi sonucunda, değişkenler arasındaki istatistiksel olarak anlamlı farklılıkların olduğunu belirlemek için F değeri ve p değeri kullanılır. Bu çıktıda, X değişkeninin AveragePrice üzerinde anlamlı bir etkisi olduğu sonucuna varılmaktadır.