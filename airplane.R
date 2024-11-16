rm(list = ls())

# 航空機利用客のデータセット（103904×25）を使用
# https://www.kaggle.com/datasets/teejmahal20/airline-passenger-satisfaction
airplane <- read.csv("airplane.csv")
nrow(airplane)
length(airplane)

# 今回クラスが Business、Eco、EcoPlus と３種類あるので、多数派の BusinessとEco の２種類に絞る
airplane2 <- airplane[airplane$Class %in% c("Business", "Eco"), ]

# 顧客タイプ、クラス、満足度をそれぞれ代入
customerType <- airplane2$CustomerType
class <- airplane2$Class
satisfaction <- airplane2$satisfaction
# 01の数値変換
customerType01 <- ifelse(customerType=="Loyal Customer", 1, 0)
class01 <- ifelse(class=="Business", 1, 0)
satisfaction01 <- ifelse(satisfaction=="satisfied", 1, 0)

# クロス集計表とファイ係数（顧客タイプ-クラス）
# ファイ係数は約 0.109 なので、「ロイヤル顧客ほどビジネスクラスを選ぶ傾向がある」とはいえない
table(customerType, class)
cor(customerType01, class01)

# クロス集計表とファイ係数（顧客タイプ-満足度）
# ファイ係数は約 0.199 なので、「ロイヤル顧客ほど満足する傾向がある」とはあまりいえない
table(customerType, satisfaction)
cor(customerType01, satisfaction01)

# クロス集計表とファイ係数（クラス-満足度）
# ファイ係数は約 0.511 なので、「ビジネスクラスを選んだ人ほど満足する傾向がある」といえる
table(class, satisfaction)
cor(class01, satisfaction01)

# カイ二乗検定
# 統計量は、帰無仮説のもとで自由度 (行の数-1)×(列の数-1) の カイ二乗分布 にしたがう（今回は自由度１）
# 今回の場合、クラスと満足度に関して以下のように仮説を立てる
# 帰無仮説：２つの変数は独立である
# 対立仮説：２つの変数には連関がある
chisq.test(table(class, satisfaction), correct = F)
curve(dchisq(x,1),0,5)
abline(v=qchisq(0.95, 1))
qchisq(0.95, 1)
pchisq(25141, 1, lower.tail = F)
# 結果：p値が2.2e-16以下なので帰無仮説は棄却される
# すなわち、２つの変数が独立である可能性は低いと結論づけられる
# （※注意）このデータはサンプルサイズが大きいので、検定の結果が有意になりやすくなっている

# カイ二乗検定と似たものに「フィッシャーの正確確率検定」がある
# これはサンプルサイズが小さいときや 2×2 のときに有効である
fisher.test(table(class01, satisfaction01), alternative="greater")

# 比率を保ったままサンプルサイズを約1/100にしてみる
table(class, satisfaction)
little_sample <- matrix(c(15, 38, 34, 9),nr=2)
chisq.test(little_sample, correct = F)
# 結果：p値が2.2e-16以下 → 7.489e-07と大きくなった
# 有意という結果は変わらないが、サンプルサイズを小さくすることで有意になりにくくなった

# サンプルサイズをさらに半分にする
little_sample2 <- matrix(c(8,19,17,5),nr=2)
chisq.test(little_sample2, correct = F)
# 結果：p値が7.489e-07→0.0009057と更に大きくなった

# ---------------------------------

# 質問紙尺度
# データの９～２２行目は、顧客のサービスに対する５段階満足度の値が入っている
library(psy)
expairplane <- airplane[,9:22]
cronbach(expairplane)
# 結果：alpha が0.7以上あるため信頼性が高く、測定項目間に一定の一貫性がある可能性が高い
# すなわち、項目が一貫して「その顧客から見た航空サービスの全体的な満足度」を測定できていると考えられる

# 因子分析
# さきほどの５段階満足度表に関して、顧客の選択の背景にどのような因子があるのか調べる
cor(airplane[,9:22])
fa.parallel(airplane[,9:22])
# 因子数５が推薦されたため、５で分析する
factanal(airplane[,9:22], factors = 5)
# 結果の解釈、以下のような５因子に分類されたのではと考えた
# 1: 席の快適さ（フードドリンク、席の落ち着き、娯楽、清潔さ）
# 2: 機内サービス（娯楽、搭乗サービス、レッグルームサービス、手荷物の取り扱い、機内サービス）
# 3: 空港での快適さ（出発/到着時間の満足度、オンライン予約、ゲートの位置）
# 4: システムの快適さ（Wi-fi、オンライン予約、オンライン搭乗）
# 5: その他
