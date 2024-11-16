rm(list = ls())

# 車の価格予測のデータセット（22000×3）を使用
# https://www.kaggle.com/datasets/ravishah1/carvana-predict-car-prices
car <- read.csv("car.csv")
nrow(car)
length(car)

# 車の製造年、走行マイル、値段をそれぞれ代入
year <- car$Year
miles <- car$Miles
price <- car$Price

# 製造年と走行マイルから予測
# Multiple R-squared:  0.2412なので、値段の24%ぐらいを説明できている
data <- lm(price~year+miles)
summary(data)

# プロットする
# 走行マイルが少ない車は、値段の幅が広くなっている
plot(miles, price)
abline(lm(price~miles))

hist(car$Year)
hist(car$Miles)
hist(car$Price)

# 値段が25000以上のもの、製造年が2024以上のものを外れ値とみなし除いてみる
car2 <- car[(car$Price<25000 & car$Year < 2024), ]
nrow(car2)
hist(car2$Year)
hist(car2$Price)

miles2 <- car2$Miles
price2 <- car2$Price
year2 <- car2$Year

# 製造年と走行マイルから予測
# Multiple R-squared:  0.4035なので、値段の40%ぐらいを説明できている
data2 <- lm(price2~miles2+year2)
summary(data2)
plot(miles2, price2)
abline(lm(price2~miles2))

# 回帰係数の等質性
summary(aov(price2~miles2*year2))
# 今回miles2:year2のp値が2e-16以下なので、5%水準で回帰線が平行でない（非線形）と結論づけられる
# すなわち、そもそも共分散分析の信ぴょう性が低い疑いがある

# 回帰線の様子を調べる
library(ggplot2)
ggplot(car2, aes(x = miles2, y = price2, color = factor(year2))) +
  geom_line() +
  labs(title = "Interaction Effect of miles2 and year2 on price2",
       x = "Miles2",
       y = "Price2",
       color = "Year2") +
  theme_minimal()
