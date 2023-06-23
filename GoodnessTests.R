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

all_data$AveragePrice <- as.numeric(gsub("\\$","",all_data$AveragePrice))
# chisq.test(all_data$AveragePrice, all_data$TotalAvocados)
new_data <- all_data$AveragePrice[seq(1, 330, by = 15)]
new_data2 <- all_data$TotalAvocados[seq(1, 330, by = 15)]

new_data <- all_data$AveragePrice %>%
  group_by(grp = ceiling(row_number() / 3)) %>%
  summarise(mean_value = mean(value))

# Sonuçları görüntüleme
print(result)
print(fisher.test(new_data, new_data2))
print(chisq.test(new_data, new_data2))

# Fisher'in Kesin Testi:

# Null hipotezi (H0): İki değişken (new_data ve new_data2) arasında ilişki yoktur.
# Alternatif hipotezi (H1): İki değişken arasında bir ilişki vardır.
# p-value = 1: Elde edilen p-değeri, null hipotezine göre iki değişken arasında bir ilişki olduğunu reddetmemize yetecek kadar anlamlı bir kanıt sağlamaz. Yani, iki değişken arasında anlamlı bir ilişki olduğunu söyleyemeyiz.
# Pearson Ki-Kare Testi:

# Null hipotezi (H0): İki değişken (new_data ve new_data2) arasında bir ilişki yoktur.
# Alternatif hipotezi (H1): İki değişken arasında bir ilişki vardır.
# X-squared = 440, df = 420, p-value = 0.2412: Elde edilen p-değeri, null hipotezine göre iki değişken arasında bir ilişki olduğunu reddetmemize yetecek kadar anlamlı bir kanıt sağlamaz. Yani, iki değişken arasında anlamlı bir ilişki olduğunu söyleyemeyiz. Ancak, uyarı mesajı, ki-kare yaklaşımının doğru olmayabileceğini belirtmektedir. Bu nedenle, sonucun güvenilirliği hakkında bir şüphe bulunmaktadır.
# Sonuç olarak, her iki test de iki değişken arasında anlamlı bir ilişki olduğunu desteklememektedir. Ancak, ki-kare testindeki uyarı mesajı nedeniyle sonucun güvenilirliği hakkında bir şüphe bulunmaktadır.