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
# *************************ORGANİC-AVARAGEPRİCE BAŞLAR*******************
# organic avakadolar ve ortalama price ile oluşturulan linear regrasyon modeli
organic <- subset(all_data, type == "organic")
organic$AveragePrice <- as.numeric(gsub("\\$","",organic$AveragePrice))

y <- organic$AveragePrice
x <- organic$TotalAvocados

organic_linear_regression <- lm(y ~ poly(x, 2, raw = TRUE))
print(organic_linear_regression)
# Polinom katsayılarını elde etme
coefficients <- coef(organic_linear_regression)
print(coefficients)
# Polinom fonksiyonunu tanımlama
# **********************burası eklenecekmi ? *******************
# polinom_func <- function(x) {
#   coefficients[1] + coefficients[2] * x + coefficients[3] * x^2
# }
# *************************ORGANİC-AVARAGEPRİCE BİTER*******************


# *************************conventional-AVARAGEPRİCE BAŞLAR*******************

conventional <- subset(all_data, type == "conventional")
conventional$AveragePrice <- as.numeric(gsub("\\$","",conventional$AveragePrice))

y <- conventional$AveragePrice
x <- conventional$TotalAvocados

conventional_linear_regression <- lm(y ~ poly(x, 2, raw = TRUE))
summary(conventional_linear_regression)
# Polinom katsayılarını elde etme
coefficients <- coef(conventional_linear_regression)
# *************************conventional-AVARAGEPRİCE BİTER*******************

conventional <- subset(all_data, type == "conventional")

y <- conventional$AveragePrice
X <- conventional[, c("TotalAvocados", "Small", "ExtraLarge", "Large", "TotalBags")]
# print(X)
# # Lineer regresyon modelini oluşturma
linear_model <- lm(y ~ ., data = X)

# # Modelin özetini görüntüleme
summary(linear_model)

# # Regresyon katsayılarını elde etme
# coefficients <- coef(linear_model)
# print(coefficients)

# # Tahmin yapma
# new_data <- data.frame(TotalAvocados = c(1500000), Small = c(1000000), ExtraLarge = c(50000), Large = c(300000), TotalBags = c(1200000))
# predicted <- predict(linear_model, newdata = new_data)
# print(predicted)




# Lineer regresyon modeli, lm() fonksiyonu kullanılarak oluşturuluyor:
# organic_linear_regression <- lm(y ~ poly(x, 2, raw = TRUE))
# Bu model, "y ~ poly(x, 2, raw = TRUE)" ifadesi ile tanımlanıyor. poly() fonksiyonu, bağımsız değişkenin polinomik bir terimle dönüştürülmesini sağlar. Burada, 2. dereceden bir polinom kullanılarak doğrusal olmayan bir ilişkiyi modellemek için polinomik bir dönüşüm yapılıyor.

# Sonrasında, modelin katsayılarına erişmek için "coef()" fonksiyonu kullanılıyor:

# coefficients <- coef(organic_linear_regression)
# Elde edilen katsayılar, polinom fonksiyonunun katsayılarıdır. Bu katsayılar kullanılarak polinom fonksiyonu tanımlanır ve çizim işlemi gerçekleştirilir.

# Bu şekilde, "AveragePrice" ve "TotalAvocados" arasındaki ilişkiyi modelleyen ve doğrusal olmayan bir eğriyi temsil eden bir lineer regresyon grafik elde edilir.

# 