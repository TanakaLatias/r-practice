rm(list = ls())

# バレンタインのデータセット（6×9）を使用
# https://www.kaggle.com/datasets/joebeachcapital/valentines-day-consumer-data
valentine1 <- read.csv("valentine1.csv")
nrow(valentine1)
length(valentine1)

# 支出額、商品カテゴリ、年代をそれぞれ代入
spendings <- c(valentine1$Candy, valentine1$Flowers, valentine1$Jewelry,
               valentine1$GreetingCards, valentine1$EveningOut, valentine1$Clothing,
               valentine1$GiftCards)
categories <- factor(c(rep("Candy", 6), rep("Flowers", 6), rep("Jewelry", 6),
                       rep("GreetingCards", 6), rep("EveningOut", 6),
                       rep("Clothing", 6), rep("GiftCards", 6)))
ages <- factor(rep(c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"), 7))

# 一元配置分散分析（対応なし）
# 統計量は、帰無仮説のもとで 分子の自由度（群の数-1）と分母の自由度（各群のデータ数-1）の F分布 にしたがう
# 帰無仮説：各商品カテゴリの平均支出額の母平均は等しい
# 対立仮説：各商品カテゴリの平均支出額の母平均は等しくない
curve(df(x,6,35),0,5)
summary(aov(spendings~categories))
TukeyHSD(aov(spendings~categories))
# 結果：p値が5.01e-09なので帰無仮説は棄却される
# すなわち、商品カテゴリ間のうち、少なくとも１つは平均支出額の母平均が等しくないと結論づけられる
# また TukeyHSD によりどのカテゴリ間が等しくないか確認できる

# 一元配置分散分析（対応あり）
# 統計量、帰無仮説、対立仮説は ↑ の対応なし版と同じ
summary(aov(spendings~categories+ages))
# 結果：商品カテゴリのp値が2.82e-11、年代のp値が0.000458なのでそれぞれ帰無仮説は棄却される
# 商品カテゴリ間でも年代間でも、少なくとも１つは有意差があると結論づけられる

# valentine1 と同様
valentine2 <- read.csv("valentine2.csv")
nrow(valentine2)
length(valentine2)

# 支出額、商品カテゴリ、性別をそれぞれ代入
spendings2 <- c(valentine2$Candy, valentine2$Flowers, valentine2$Jewelry,
                valentine2$GreetingCards, valentine2$EveningOut, valentine2$Clothing,
                valentine2$GiftCards)
categories2 <- factor(c(rep("Candy", 2), rep("Flowers", 2), rep("Jewelry", 2),
                        rep("GreetingCards", 2), rep("EveningOut", 2),
                        rep("Clothing", 2), rep("GiftCards", 2)))
genders2 <- factor(rep(c("M", "F"), 7))

# 一元配置分散分析（対応なし）
summary(aov(spendings2~categories2))
TukeyHSD(aov(spendings2~categories2))

# 一元配置分散分析（対応あり）
summary(aov(spendings2~categories2+genders2))

# 一元配置分散分析（対応なし）に関して、年代か性別のどちらを行に持ってくるかに依って
# 有意差があるかないか変わるのは変ではないか？
# 性別が２つしかないためサンプルサイズが小さく、有意差があるとみなされないのかもしれない
mean(valentine1$Candy)
mean(valentine1$Flowers)
mean(valentine1$Jewelry)
mean(valentine1$GreetingCards)
mean(valentine1$EveningOut)
mean(valentine1$Clothing)
mean(valentine1$GiftCards)

mean(valentine2$Candy)
mean(valentine2$Flowers)
mean(valentine2$Jewelry)
mean(valentine2$GreetingCards)
mean(valentine2$EveningOut)
mean(valentine2$Clothing)
mean(valentine2$GiftCards)
