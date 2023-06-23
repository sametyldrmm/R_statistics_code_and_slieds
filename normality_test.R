library(ggplot2)
library(dplyr)

# Veri setini yükleme ve faktörleri ayarlama
all_data <- read.csv("2.csv", sep = ",", fileEncoding = "UTF-8")
all_data$type <- as.factor(all_data$type)
all_data$Cities <- as.factor(all_data$Cities)

# Organik alt kümesini oluşturma
organic <- subset(all_data, type == "organic")

# Shapiro-Wilk Testi
shapiro_result <- shapiro.test(organic$TotalAvocados)
print(shapiro_result)
# Veri setini ggplot kullanarak normallik grafiği oluşturma
ggplot(organic, aes(x = TotalAvocados)) +
  geom_density() +
  labs(title = "Normallik Grafiği",
       subtitle = paste("Shapiro-Wilk Test Sonucu: p =", shapiro_result$p.value),
       x = "TotalAvocados",
       y = "Yoğunluk")
