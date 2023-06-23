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

organic <- subset(all_data, type == "organic")
organic$AveragePrice <- as.numeric(gsub("\\$","",organic$AveragePrice))

conventional <- subset(all_data, type == "conventional")
conventional$AveragePrice <- as.numeric(gsub("\\$","",conventional$AveragePrice))


# birtek bu alt kısmın fotosu eklenecek 
summary <- shapiro.test(organic$AveragePrice)
print(summary)

summary <- shapiro.test(conventional$AveragePrice)
print(summary)

# shapiro.test() fonksiyonu, bir veri setinin normal dağılıma uygun olup olmadığını test etmek için kullanılır. Bu test, verinin normal dağılıma yakınsayıp yakınsamadığını değerlendirir.

# Çıktıdaki bilgiler şunları ifade eder:

# data: Test edilen veri setinin adı veya açıklaması
# W: Shapiro-Wilk test istatistiği değeri
# p-value: Hipotez testi sonucunda hesaplanan p değeri
# Bu çıktıda, organic$AveragePrice veri seti için yapılan Shapiro-Wilk normalite testi sonucunda elde edilen istatistiksel değerler yer alır.

# Shapiro-Wilk test istatistiği (W), verinin normal dağılıma ne kadar yakın olduğunu gösterir. Değer 1'e yaklaştıkça, verinin normal dağılıma daha yakın olduğu söylenebilir.
# p değeri, verinin normal dağılıma uygun olup olmadığı hakkında istatistiksel bir kanıt sağlar. Eğer p değeri belirli bir anlamlılık düzeyinden küçükse (genellikle 0.05 olarak kabul edilir), normal dağılıma uygunluğu reddedilir ve veri seti normal dağılıma uymaz. Bu durumda, normal dağılım varsayımı ihlal edilmiş olabilir.
# Bu çıktıya göre, organic$AveragePrice veri setinin normal dağılıma uygun olduğunu gösteren bir p değeri elde edilmiştir (p-value = 1.265e-07). Bu durumda, veri setinin normal dağılıma uygun olduğu söylenebilir.






# histogram çizdirme sırayla çalıştırılacak ss alınaacak
summary <-  hist(conventional$AveragePrice)
print(summary)
# summary <-  hist(organic$AveragePrice)
# print(summary)



summary <-  wilcox.test(conventional$AveragePrice,conventional$TotalAvocados)
print(summary)
# Bu çıktı, "Wilcoxon rank sum test with continuity correction" adlı bir testin sonuçlarını göstermektedir. Bu test, iki bağımsız grup arasında istatistiksel olarak anlamlı bir fark olup olmadığını belirlemek için kullanılır. Test, gruplardaki verilerin sıralanması üzerine dayanır.

# Çıktıdaki bilgiler şunları ifade eder:

# data: Test edilen veri setlerinin adı veya açıklaması
# W: Wilcoxon rank sum test istatistiği değeri
# p-value: Hipotez testi sonucunda hesaplanan p değeri
# alternative hypothesis: Alternatif hipotez, yani testin incelenen gruplar arasında bir yer değiştirme farklılığı olup olmadığını değerlendirdiği bilgisi verilir.
# Bu çıktıya göre, conventional$AveragePrice ve conventional$TotalAvocados veri setleri arasında istatistiksel olarak anlamlı bir fark olduğu sonucuna varılır. p değeri çok küçük olduğundan (p-value < 2.2e-16), gruplar arasındaki yer değiştirme farkının sıfır olmadığı ve istatistiksel olarak anlamlı olduğu sonucuna varılır.

# Wilcoxon rank sum test, bağımsız iki grup arasında medyan veya dağılım farklarını değerlendirmek için kullanılan parametrik olmayan bir testtir. Bu test, gruplar arasında istatistiksel olarak anlamlı bir fark olup olmadığını belirlemek için tercih edilir. Özellikle, gruplar arasındaki farkın sıralanmış veriler üzerinden değerlendirilmesi gereken durumlarda kullanılır.

summary <-  wilcox.test(organic$AveragePrice,organic$TotalAvocados)
print(summary)

# Bu çıktı, "Wilcoxon rank sum test with continuity correction" adlı bir testin sonuçlarını göstermektedir. Bu test, iki bağımsız grup arasında istatistiksel olarak anlamlı bir fark olup olmadığını belirlemek için kullanılır. Test, gruplardaki verilerin sıralanması üzerine dayanır.

# Çıktıdaki bilgiler şunları ifade eder:

# data: Test edilen veri setlerinin adı veya açıklaması
# W: Wilcoxon rank sum test istatistiği değeri
# p-value: Hipotez testi sonucunda hesaplanan p değeri
# alternative hypothesis: Alternatif hipotez, yani testin incelenen gruplar arasında bir yer değiştirme farklılığı olup olmadığını değerlendirdiği bilgisi verilir.
# Bu çıktıya göre, conventional$AveragePrice ve conventional$TotalAvocados veri setleri arasında istatistiksel olarak anlamlı bir fark olduğu sonucuna varılır. p değeri çok küçük olduğundan (p-value < 2.2e-16), gruplar arasındaki yer değiştirme farkının sıfır olmadığı ve istatistiksel olarak anlamlı olduğu sonucuna varılır.

# Wilcoxon rank sum test, bağımsız iki grup arasında medyan veya dağılım farklarını değerlendirmek için kullanılan parametrik olmayan bir testtir. Bu test, gruplar arasında istatistiksel olarak anlamlı bir fark olup olmadığını belirlemek için tercih edilir. Özellikle, gruplar arasındaki farkın sıralanmış veriler üzerinden değerlendirilmesi gereken durumlarda kullanılır.
all_data$AveragePrice <- as.numeric(gsub("\\$","",all_data$AveragePrice))


summary <-  tapply(all_data$TotalAvocados,all_data$type,mean)
print(summary)

# tapply() fonksiyonu, verileri belirli bir faktör veya grupa göre özetlemek veya toplamak için kullanılan bir R fonksiyonudur. İki temel parametre alır: X ve INDEX.

# X: Özetlemek veya toplamak istediğiniz veri vektörü veya matrisidir.
# INDEX: Verilerin gruplanmasını sağlayan faktör veya faktörlerin vektörüdür. Bu faktörler, verilerin hangi gruplara ait olduğunu belirtir.


summary = kruskal.test(conventional$TotalAvocados,conventional$AveragePrice)
print(summary)

# Kruskal-Wallis testi, kategorik bir bağımsız değişkenin (faktör) ve sürekli bir bağımlı değişken arasındaki ilişkiyi değerlendirmek için kullanılan bir istatistiksel testtir. Bu test, grupların medyan değerleri arasında istatistiksel olarak anlamlı bir farklılık olup olmadığını belirlemek için kullanılır.

# Testin hipotezleri şunlardır:

# Null hipotezi (H0): Grupların medyan değerleri arasında fark yoktur.
# Alternatif hipotezi (H1): Grupların medyan değerleri arasında fark vardır.
# Kruskal-Wallis testi, grupların sırasıyla bağımsız örneklemeler olduğunu ve her bir grubun dağılımının sürekli ve benzer olduğunu varsayar.

# Bu çıktıda görülen Kruskal-Wallis testi, conventional$TotalAvocados ve conventional$AveragePrice değişkenleri arasındaki gruplar arasında medyan değerler açısından istatistiksel olarak anlamlı bir farklılık olup olmadığını değerlendirmektedir.

# Çıktıdaki değerler şu şekildedir:

# Kruskal-Wallis chi-squared: Test istatistiği değeridir. Büyük bir değere sahipse, gruplar arasında medyan değerler açısından anlamlı bir fark olduğunu gösterir.
# df: Testin serbestlik derecesidir. Grup sayısının bir eksik değeridir.
# p-value: Hipotez testinin p değeridir. H0 hipotezini reddetmek için kullanılır. Eğer p değeri belirli bir anlamlılık düzeyinden (genellikle 0.05) daha küçükse, gruplar arasında anlamlı bir fark olduğu sonucuna varılır.
# Bu çıktıda, p-value değeri 0.06564 olarak verilmiştir. Genellikle kabul edilen anlamlılık düzeyi (α) 0.05'tir. Eğer p-value değeri bu anlamlılık düzeyinden küçükse (p-value < α), gruplar arasında anlamlı bir fark olduğu sonucuna varılır. Ancak bu durumda p-value değeri 0.06564 olduğu için, belirli bir anlamlılık düzeyinde (α = 0.05) gruplar arasında anlamlı bir fark olduğu iddia edilemez. Bu nedenle, conventional$TotalAvocados ve conventional$AveragePrice arasındaki gruplar arasında medyan değerler açısından anlamlı bir fark olmadığı sonucuna varılabilir.


summary = kruskal.test(organic$TotalAvocados,organic$AveragePrice)
print(summary)

# Bu çıktıda görülen Kruskal-Wallis testi, organic$TotalAvocados ve organic$AveragePrice değişkenleri arasındaki gruplar arasında medyan değerler açısından istatistiksel olarak anlamlı bir farklılık olup olmadığını değerlendirmektedir.

# Çıktıdaki değerler şu şekildedir:

# Kruskal-Wallis chi-squared: Test istatistiği değeridir. Büyük bir değere sahipse, gruplar arasında medyan değerler açısından anlamlı bir fark olduğunu gösterir.
# df: Testin serbestlik derecesidir. Grup sayısının bir eksik değeridir.
# p-value: Hipotez testinin p değeridir. H0 hipotezini reddetmek için kullanılır. Eğer p değeri belirli bir anlamlılık düzeyinden (genellikle 0.05) daha küçükse, gruplar arasında anlamlı bir fark olduğu sonucuna varılır.
# Bu çıktıda, p-value değeri 0.2944 olarak verilmiştir. Genellikle kabul edilen anlamlılık düzeyi (α) 0.05'tir. Eğer p-value değeri bu anlamlılık düzeyinden küçükse (p-value < α), gruplar arasında anlamlı bir fark olduğu sonucuna varılır. Ancak bu durumda p-value değeri 0.2944 olduğu için, belirli bir anlamlılık düzeyinde (α = 0.05) gruplar arasında anlamlı bir fark olduğu iddia edilemez. Bu nedenle, organic$TotalAvocados ve organic$AveragePrice arasındaki gruplar arasında medyan değerler açısından anlamlı bir fark olmadığı sonucuna varılabilir.