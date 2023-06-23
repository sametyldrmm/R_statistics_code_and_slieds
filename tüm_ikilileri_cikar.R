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
# print(all_data_type)
# glimpse(all_data)


organic <- subset(all_data, type == "organic")
organic$Date <- as.character(organic$Date)
organic$Date <- as.integer(format(as.Date(organic$Date), "%Y"))

conventional <- subset(all_data, type == "conventional")
conventional$Date <- as.character(conventional$Date)
conventional$Date <- as.integer(format(as.Date(conventional$Date), "%Y"))

years <- c(2015, 2016, 2017, 2018)
years_organic <- c()
years_organic_Small <- c()
years_organic_Large <- c()
years_organic_Extra_Large <- c()

years_conventional <- c()
years_conventional_Small <- c()
years_conventional_Large <- c()
years_conventional_Extra_Large <- c()

for(year in years){
  organic_year <- subset(organic, Date == year)
  
  sum_organic_year <- sum(organic_year$TotalAvocados)
  years_organic <- c(years_organic, sum_organic_year)
  
  sum_organic_year_Small <- sum(organic_year$Small)
  years_organic_Small <- c(years_organic_Small, sum_organic_year_Small)
  
  sum_organic_year_Large <- sum(organic_year$Large)
  years_organic_Large <- c(years_organic_Large, sum_organic_year_Large)
  
  sum_organic_year_Extra_Large <- sum(organic_year$Extra.Large)  
  years_organic_Extra_Large <- c(years_organic_Extra_Large, sum_organic_year_Extra_Large)


  
  conventional_year <- subset(conventional, Date == year)
  
  sum_conventional_year <- sum(conventional_year$TotalAvocados)
  years_conventional <- c(years_conventional, sum_conventional_year)
  
  sum_conventional_year_Small <- sum(conventional_year$Small)
  years_conventional_Small <- c(years_conventional_Small, sum_conventional_year_Small)
  
  sum_conventional_year_Large <- sum(conventional_year$Large)
  years_conventional_Large <- c(years_conventional_Large, sum_conventional_year_Large)
  
  sum_conventional_year_Extra_Large <- sum(conventional_year$Extra.Large)
  years_conventional_Extra_Large <- c(years_conventional_Extra_Large, sum_conventional_year_Extra_Large)
}
# şimdi bu verileri grafiğe dökelim

# Örnek veri seti oluşturma
category <- years
value1 <- years_conventional #1
value2 <- years_organic     #2

# Verileri birleştirme
data <- data.frame(category, value1, value2)

# Bar grafiği oluşturma
#fill değiş
#fill değiş scale fill aynı olsun
ggplot(data, aes(x = category)) +
  geom_bar(aes(y = value1, fill = "Conventioanal Avacado"), stat = "identity", width = 0.5) +
  geom_bar(aes(y = value2, fill = "Organic Avacado"), stat = "identity", width = 0.5) +
  labs(title = "Comparison of total avocado in years", x = "Years", y = "Total Avacados",width = 10) +
  scale_fill_manual(values = c("Conventioanal Avacado" = "blue", "Organic Avacado" = "red")) +
  theme_minimal()

# aynı şekilde small, large, extra large için de yapalım şimdi

