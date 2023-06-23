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



x = all_data$TotalAvocados
mean(x)
sd(x)

t.test(x, h = 0)

# T-test sonucuna bakıldığında:

# T-testi, TotalAvocados sütunundaki verilerin ortalama değerinin 0'dan farklı olup olmadığını test etmek için kullanılmıştır.
# "One Sample t-test" ifadesi, tek bir örnekleme seti üzerinde gerçekleştirilen t-testini ifade etmektedir.
# "t" değeri, t-testi istatistiğini göstermektedir. Bu durumda, t değeri 17.888'dir.
# "df" değeri, t-testinin serbestlik derecesini göstermektedir. Bu durumda, serbestlik derecesi 337'dir.
# "p-value" değeri, p-değerini göstermektedir. p-değerinin < 2.2e-16 olduğu belirtilmiştir, yani çok düşük bir p-değeri elde edilmiştir. Bu, TotalAvocados sütunundaki verilerin ortalama değerinin 0'dan anlamlı bir şekilde farklı olduğunu göstermektedir.
# "alternative hypothesis" ifadesi, alternatif hipotezi belirtmektedir. Bu durumda, alternatif hipotez "true mean is not equal to 0" olarak tanımlanmıştır.
# "95 percent confidence interval" ifadesi, %95 güven aralığını göstermektedir. Bu aralık, TotalAvocados sütunundaki verilerin ortalama değerinin 894,330.8 ile 1,115,326.0 arasında olduğunu tahmin etmemize olanak sağlar.
# "sample estimates" ifadesi, örnekleme tahminini göstermektedir. Bu durumda, TotalAvocados sütunundaki verilerin örnekleme ortalaması 1,004,828 olarak hesaplanmıştır.
# Sonuç olarak, bu t-test sonucuna dayanarak TotalAvocados sütunundaki verilerin ortalama değerinin 0'dan anlamlı bir şekilde farklı olduğu sonucuna varabiliriz. Bu bilgi, TotalAvocados verilerinin popülasyon ortalaması hakkında bilgi sağlar ve 0'dan farklı olduğunu gösterir.
