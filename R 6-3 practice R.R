#C5.0採用事後修剪法
#需特別注意的是:C5.0的輸出變數(ex:本題的iris.train$Species)其資料型態必須是因子,若非因子型態可使用factor()函數來轉換
install.packages("C50")
library(C50)

data(iris)
np = ceiling(0.1*nrow(iris))
np

test.index = sample(1:nrow(iris),np)  #隨機抽10%資料
iris.test = iris[test.index,]   #測試資料
iris.train = iris[-test.index,] #訓練資料

# 設定C5.0相關引數
c = C5.0Control(subset = FALSE,          #表示是否使用部分子集合資料
                bands = 0,               
                winnow = FALSE,          #表示是否使用屬性篩選
                noGlobalPruning = FALSE, #表示是否執行決策樹修剪
                CF = 0.25,               #CF為信賴水準
                minCases = 2,            #建立一個節點時最少需要幾筆資料(案例)
                fuzzyThreshold = FALSE,  
                sample = 0,              #當作訓練資料的比例
                seed = sample.int(4096,size = 1) - 1L, #隨機亂數
                earlyStopping = TRUE     
                ) 
#第5個屬性為目標屬性,C5.0兩種寫法
iris_treeModel1 <- C5.0(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                        data = iris.train,type = "class")
iris_treeModel2 <- c5.0(x = iris.train[,-5],y = iris.train$Species) # x為應變數(輸入屬性)),y為解釋變數(輸出屬性)?
summary(iris_treeModel1)

test.output = predict(iris_treeModel1,iris.test[,-5],type = "class")
n = length(test.output)
number = 0
for (i in 1:n) {
  if(test.output[i] == iris.test[i,5])  #計算測試資料的正確率
  {
    number = number + 1
  }
}
test.accuracy = number/n*100
test.accuracy
iris.test[,5]
