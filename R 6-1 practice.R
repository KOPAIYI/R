#rpart(formula,data,weights,subset,na.action=na.rpart,method,parms,control,...)
#formula 表示模型的公式,ex:Y~X1+X2+X3
#data 表示為建立模型用的資料名稱
#method 表示使用的方法 ex:"anova","poisson","class","exp"
#control 表示為控制此演算法之引數,可在rpart.contorl中設定(6-2範例)

install.packages("rpart")
library(rpart)

data(iris)
np = ceiling(0.1*nrow(iris)) #nrow(iris)傳回iris資料筆數
np
test.index = sample(1:nrow(iris),np) #10%為測試資料
iris.testdata = iris[test.index,] #測試資料
iris.traindata = iris[-test.index,] #訓練資料

#利用rpart()建立訓練資料的決策樹iris.tree
iris.tree = rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,method = "class", data = iris.traindata)
iris.tree

summary(iris.tree)
plot(iris.tree) ; text(iris.tree) #text()將文本添入至繪圖中

#顯示訓練資料的正確率
species.traindata = iris$Species[-test.index]
train.predict = factor(predict(iris.tree,iris.traindata,type = "class"),levels = levels(species.traindata))

table.traindata = table(species.traindata,train.predict)
table.traindata
correct.traindata = sum(diag(table.traindata))/sum(table.traindata)*100
correct.traindata

#顯示測試資料的正確率
species.testdata = iris$Species[test.index]
test.predict = factor(predict(iris.tree,iris.testdata,type = "class"),levels = levels(species.testdata))
table.testdata = table(species.testdata,test.predict)
table.testdata
correct.testdata = sum(diag(table.testdata))/sum(table.testdata)*100
correct.testdata
