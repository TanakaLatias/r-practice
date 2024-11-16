rm(list = ls())

# アイリスのデータセット（150×6）を使用
# https://www.kaggle.com/datasets/uciml/iris
iris <- read.csv("Iris.csv")
nrow(iris)
length(iris)

# table で Species の種類数を確認
table(iris$Species)
setosa <- iris[iris$Species=="Iris-setosa",]
versicolor <- iris[iris$Species=="Iris-versicolor",]
virginica <- iris[iris$Species=="Iris-virginica",]

# セトサの SepalLengthCm（がく片の長さ/cm） のヒストグラム等
hist(setosa$SepalLengthCm, breaks = seq(3.5, 6.5, by = 0.2),
     main="セトサのがく片の長さ（cm）のヒストグラム", xlab="cm", ylab="度数")
summary(setosa$SepalLengthCm)
var(setosa$SepalLengthCm)
sd(setosa$SepalLengthCm)

# アイリスの PetalLengthCm（花びらの長さ/cm）と PetalWidthCm（花びらの幅/cm）の散布図
plot(iris$PetalLengthCm, iris$PetalWidthCm)
# ↑の相関係数
cor(iris$PetalLengthCm, iris$PetalWidthCm)

# t分布を用いた検定（１つの平均値の検定、母分散σ^2が未知）
# 統計量は、帰無仮説のもとで自由度 df=n-1 の t分布 にしたがう
# 今回の場合、アイリスのSepalLengthCm（がく片の長さ/cm）に関して以下のように仮説を立てる
# 帰無仮説：μ=6（母平均は6である）
# 対立仮説：μ≠6（母平均は6でない）
t.test(iris$SepalLengthCm, mu=6)
curve(dt(x,149),-3,3)
abline(v=qt(0.025, 149))
abline(v=qt(0.975, 149))
qt(0.025, 149)
qt(0.975, 149)
pt(-2.3172, 149)
2*pt(-2.3172, 149)
# 結果：p値が0.02186なので帰無仮説は棄却される
# また、本当の母平均は95%の確率で5.709732～5.976934の間にある
# すなわち、母平均が６の可能性は低いと結論づけられる

# 相関係数の検定（無相関検定）
# 統計量は、帰無仮説のもとで自由度 df=n-2 の t分布 にしたがう
# 今回の場合、アイリスのSepalLengthCm（がく片の長さ/cm）とSepalWidthCm（がく片の幅/cm）に関して以下のように仮説を立てる
# 帰無仮説：ρ=0（母相関は0である）
# 対立仮説：ρ≠0（母相関は0ではない）
cor.test(iris$SepalLengthCm, iris$SepalWidthCm)
curve(dt(x,148),-3,3)
abline(v=qt(0.025, 148))
abline(v=qt(0.975, 148))
qt(0.025, 148)
qt(0.975, 148)
pt(-1.3386, 148)
2*pt(-1.3386, 148)
# 結果：p値が0.1828なので帰無仮説は棄却されない
# また、本当の母相関は95%の確率で-0.26498618～0.05180021の間にある
# すなわち、母相関が０の可能性があると結論づけられる

# 分散の等質性の検定（t検定の前提条件）
# セトサのがく片の長さ（cm）とバージカラーのがく片の長さ（cm）に関して以下のように仮説を立てる
# 帰無仮説：2つの母分散が等しい
# 対立仮説：2つの母分散が等しくない
var.test(setosa$SepalLengthCm, versicolor$SepalLengthCm)
# 結果：p値が0.008657なので帰無仮説は棄却される
# ２つの母分散は等しくないと結論づけられる
# ↓　分散が等質でないとして、独立な２群のt検定にすすむ

# 独立な２群のt検定
# 統計量は、帰無仮説のもとで自由度 df = n1 + n2 -2 の t分布 にしたがう
# 帰無仮説：２つの母平均は等しい
# 対立仮説：２つの母平均は等しくない
t.test(setosa$SepalLengthCm, versicolor$SepalLengthCm, var.equal = F)
# ↑ 分散が等質なとき：var.equal = T、等質でないとき：var.equal = F
curve(dt(x,98),-3,3)
abline(v=qt(0.025, 98))
abline(v=qt(0.975, 98))
qt(0.025, 98)
qt(0.975, 98)
pt(-10.521, 98)
2 * pt(-10.521, 98)
# 結果：p値が2.2e-16以下なので帰無仮説は棄却される
# また、本当の母平均の差は95%の確率で-1.1057074～-0.7542926の間にある
# すなわち、２つの母平均の差が０（母平均が等しい）の可能性は低いと結論づけられる
