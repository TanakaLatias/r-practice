rm(list = ls())

# 主要都市の気温のデータセット（2906327×8）を使用
# https://www.kaggle.com/datasets/sudalairajkumar/daily-temperature-of-major-cities
temp <- read.csv("temperature.csv")
nrow(temp)
length(temp)

# アメリカ/中国の主要５都市の1995年8月の気温と2019年8月の気温を比較
us <- temp[temp$Country=="US",]
china <- temp[temp$Country=="China",]
usCities <- c("New York City", "Philadelphia", "Washington", "Allentown", "Boston")
us199508 <- us[us$Year==1995 & us$Month==8 & us$Day %in% days & us$City %in% usCities,]
us201908 <- us[us$Year==2019 & us$Month==8 & us$Day %in% days & us$City %in% usCities,]
china199508 <- china[china$Year==1995 & china$Month==8 & china$Day %in% days,]
china201908 <- china[china$Year==2019 & china$Month==8 & china$Day %in% days,]

temperature2 <- temp[temp$Year %in% c(1995,2019) | temp$Month==8, ]
temperature2 <- temperature2[temperature2$City %in% usCities | temperature2$Country =="China", ]
nrow(temperature2)
write.csv(temperature2, "temperature2.csv", row.names = FALSE)

mean(us199508$AvgTemperature)
mean(china199508$AvgTemperature)
mean(us201908$AvgTemperature)
mean(china201908$AvgTemperature)

table(us199508$City)
table(us201908$City)
table(china199508$City)
table(china201908$City)

t1995 <- c(us199508$AvgTemperature, china199508$AvgTemperature)
t2019 <- c(us201908$AvgTemperature, china201908$AvgTemperature)
countryName <- c(rep("US", 186), rep("China", 155))

# 共分散分析（lm）
summary(lm(t2019~t1995+countryName))
# 2019年8月のUSとChinaの切片の差は-3.43974（アメリカの方が気温が低い）で、
# p値が9.1e-11のためその差は統計的に有意である

# 共分散分析（aov）
summary(aov(t2019~t1995+countryName))

# 回帰係数の等質性
summary(aov(t2019~t1995*countryName))
# 今回はt1995:countryNameのp値が0.0686なので、5%水準で回帰線が平行であると結論づけられる
# 平行であるならば、共分散分析の結果に信ぴょう性があるといえる

# t検定によって群の効果を吟味
sa <- t2019-t1995
var.test(sa~countryName)
t.test(sa~countryName, var.equal=F)
# 結果：p値が9.065e-06なので帰無仮説は棄却される
# すなわち、群による差（アメリカと中国の５都市での気温差）があると結論づけられる
