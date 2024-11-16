rm(list = ls())

# ウォルマートのデータセット（6435×8）を使用
# https://www.kaggle.com/datasets/mikhail1681/walmart-sales
walmart <- read.csv("walmart.csv")
nrow(walmart)
length(walmart)

# 2010/02/05 週の売上と 2012/10/16 週の売上
feb2010 <- walmart[walmart$Date=="05-02-2010",]
oct2012 <- walmart[walmart$Date=="26-10-2012",]

# 対応のあるt検定
# 統計量は、帰無仮説のもとで df=n-1 の t分布 にしたがう
# 帰無仮説：μ=0（売上の変化の母平均は０である）
# 対立仮説：μ≠0（売上の変化の母平均は０ではない）
t.test(feb2010$Weekly_Sales, oct2012$Weekly_Sales, paired = T)
curve(dt(x,44),-3,3)
abline(v=qt(0.025, 44))
abline(v=qt(0.975, 44))
qt(0.025, 44)
qt(0.975, 44)
pt(3.0559, 44, lower.tail = F)
2*pt(3.0559, 44, lower.tail = F)
# 結果：p値が0.003805なので帰無仮説は棄却される
# また、本当の母平均の差は95%の確率で31830.26～155130.81の間にある
# すなわち、２つの母平均の差が０（母平均が等しい）の可能性は低いと結論づけられる
# ２年で売上は増えたといえる

# 対応がないとみなした場合のt検定
var.test(feb2010$Weekly_Sales, oct2012$Weekly_Sales)
t.test(feb2010$Weekly_Sales, oct2012$Weekly_Sales, var.equal = T)
# 結果：p値が0.4426なので帰無仮説は棄却されない
# また、本当の母平均の差は95%の確率で-147358.4～334319.5の間にある
# すなわち、２つの母平均の差が０（母平均が等しい）の可能性があると結論づけられる

# ---------------------------------

# 店舗ごと売上平均ランキング
store_sales_rank = numeric(45)
for (i in 1:45) {
  sales <- walmart[walmart$Store==i,]
  store_sales_rank[i]=mean(sales$Weekly_Sales)
  names(store_sales_rank)[i] <- paste("Store", i)
}
store_sales_rank <- sort(store_sales_rank)
print(store_sales_rank)
plot(store_sales_rank, type="p", main="", xlab="インデックス", ylab="売上平均", pch=16)

# 店舗ごと売上線グラフ
plot(scale(walmart[walmart$Store==1, ]$Weekly_Sales), type="l", ylim = c(-3, 7))
for (i in 2:45) {
  lines(scale(walmart[walmart$Store==i, ]$Weekly_Sales))
}

# 店舗ごとに売上の高かった日TOP5を集計
sales_top5_dates <- character(225)
for (i in 1:45) {
  store <- walmart[walmart$Store==i,]
  store_sorted <- store[order(store$Weekly_Sales, decreasing = TRUE), ]
  sales_top5_dates[i*5-4]=store_sorted[1,]$Date
  sales_top5_dates[i*5-3]=store_sorted[2,]$Date
  sales_top5_dates[i*5-2]=store_sorted[3,]$Date
  sales_top5_dates[i*5-1]=store_sorted[4,]$Date
  sales_top5_dates[i*5]=store_sorted[5,]$Date
}
sales_top5_dates <- sort(table(sales_top5_dates), decreasing = TRUE)
sales_top5_dates
# 考えたこと：クリスマスやブラックフライデーに売上が上がりやすそうである

# 店舗ごとに売上の低かった日BOTTOM5を集計
sales_bottom5_dates <- character(225)
for (i in 1:45) {
  store <- walmart[walmart$Store==i,]
  store_sorted <- store[order(store$Weekly_Sales, decreasing = F), ]
  sales_bottom5_dates[i*5-4]=store_sorted[1,]$Date
  sales_bottom5_dates[i*5-3]=store_sorted[2,]$Date
  sales_bottom5_dates[i*5-2]=store_sorted[3,]$Date
  sales_bottom5_dates[i*5-1]=store_sorted[4,]$Date
  sales_bottom5_dates[i*5]=store_sorted[5,]$Date
}
sales_bottom5_dates <- sort(table(sales_bottom5_dates), decreasing = TRUE)
sales_bottom5_dates
# 大晦日に売上が下がりそうである
# 1月や9月に売上が低い月が多いのはなぜだろう

# ---------------------------------

# 二元配置分散分析（対応なし）
# ・要因Aの主効果：休日週か非休日週かで売上に差がある
# ・要因Bの主効果：店舗によって売上に差がある
# ・要因Aと要因Bの交互作用効果
# 帰無仮説：主効果/交互作用効果がない
# 対立仮説：主効果/交互作用効果がある

# 今回は平均売上が近い16, 29, 37の３店舗で休日10週/非休日10週の分析をする
w_16_29_37 <- walmart[walmart$Store %in% c(16,29,37), ]
# 休日フラグ1の週が10週ある
holidays_data <- w_16_29_37[w_16_29_37$Holiday_Flag==1, ]
# 休日フラグ0の週が133週あり、そこからクリスマス等を除き10週をランダムに取る
holiday_flag_0 <- unique(w_16_29_37[w_16_29_37$Holiday_Flag==0, ]$Date)
remove <- c("26-11-2010", "17-12-2010", "24-12-2010", "25-11-2011", "23-12-2011")
holiday_flag_0 <- setdiff(holiday_flag_0, remove)
random10_holiday_flag_0 <- sample(holiday_flag_0, 10)
not_holidays_data <- w_16_29_37[w_16_29_37$Date %in% random10_holiday_flag_0, ]

sales_figure <- c(holidays_data$Weekly_Sales, not_holidays_data$Weekly_Sales)
holi_not_holi <- factor(c(rep("Holi", 30), rep("notHoli", 30)))
store123 <- factor(rep(c(rep("16",10),rep("29",10),rep("37",10)),2))
summary(aov(sales_figure~holi_not_holi*store123))

for (i in 1:10) {
  random10_holiday_flag_0 <- sample(holiday_flag_0, 10)
  not_holidays_data <- w_16_29_37[w_16_29_37$Date %in% random10_holiday_flag_0, ]
  sales_figure <- c(holidays_data$Weekly_Sales, not_holidays_data$Weekly_Sales)
  a <- summary(aov(sales_figure~holi_not_holi*store123))
  print(paste("---", i, "---"))
  print(a[[1]][, "Pr(>F)"])
  print(mean(holidays_data$Weekly_Sales))
  print(mean(not_holidays_data$Weekly_Sales))
}
# どの非休日週が選ばれるかによって、要因Aの主効果に有意差があるかないか分かれる
# だいたい3/10ぐらいの確率で有意差ありとなる
