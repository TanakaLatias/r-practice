rm(list = ls())

# A/Bテストのデータセット（90189×5）を使用
# https://www.kaggle.com/datasets/zahrazolghadr/ab-test-cookie-cats
ab <- read.csv("ab1.csv")
nrow(ab)
length(ab)

# バージョン、1日後帰還率、7日後帰還率をそれぞれ代入
ver <- ab$version
ret1 <- ab$retention_1
ret7 <- ab$retention_7
# 01の数値変換
ver01 <- ifelse(ab$version=="gate_30",1,0)
ret1_01 <- ifelse(ab$retention_1=="TRUE",1,0)
ret7_01 <- ifelse(ab$retention_7=="TRUE",1,0)

# クロス集計表とファイ係数（バージョン-1日後帰還率）
# ファイ係数は0.00594072なので、「バージョン30の人ほど1日後帰還率が高い」とはいえない
table(ver, ret1)
cor(ver01, ret1_01)

# クロス集計表とファイ係数（バージョン-7日後帰還率）
# ファイ係数は0.01053681なので、「バージョン30の人ほど7日後帰還率が高い」とはいえない
table(ver, ret7)
cor(ver01, ret7_01)

# クロス集計表とファイ係数（1日後帰還率-7日後帰還率）
# ファイ係数は0.3274012なので、「1日後帰還率が高いほど7日後帰還率が高いがある」と少しいえる
table(ret1, ret7)
cor(ret1_01, ret7_01)

# フィッシャーの正確確率検定
fisher.test(table(ret1_01, ret7_01), alternative="greater")
# p値が2.2e-16以下なので、1日後帰還率が高いほど7日後帰還率が高い傾向があると結論づけられる

# ab1 と同様
ab2 <- read.csv("ab2.csv")
nrow(ab2)
length(ab2)

# キャンペーンページ、コンバージョン率をそれぞれ代入
conTreat <- ab2$con_treat
conv <- ab2$converted
# 01の数値変換
conTreat01 <- ifelse(conTreat=="control",1,0)
conv01 <- ifelse(conv==1,1,0)
# クロス集計表とファイ係数（キャンペーンページ-コンバージョン率）
# ファイ係数は0.002279375なので、「キャンペーンページが新しい人ほどコンバージョン率が高い」とはいえない
table(conTreat, conv)
cor(conTreat01, conv01)

# フィッシャーの正確確率検定
fisher.test(table(conTreat01, conv01), alternative="greater")
# p値が0.1091なので、キャンペーンページの新/旧はコンバージョンに無関係と結論づけられる
