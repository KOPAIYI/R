#6-2 設定適當rpart.control()函數的引數來改善決策數分類結果
#rpart.control(minsplit=20,minbucket=round(minsplit/3),cp=0,maxdepth=30,....)
#minsplit 表示建立一個新個節點(Node)時最少需要幾筆資料
#minbucket 表示建立葉節點(Leaf Node)時最少需要幾筆資料
#cp 決定計算複雜度(Complexity)的參數,用於修剪樹的分支
#maxdepth 表示樹的深度

library(rpart)
data(iris)
require(rpart.plot)

np = ceiling(0.1*nrow(iris))
np
test.index = sample(1:nrow(iris),np)
iris.testdata = iris[test.index,]
iris.traindata = iris[-test.index,]

iris.tree = rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,method = "class",data = iris.traindata, 
control = rpart.control(minsplit = 5,cp = 0.0001,maxdepth = 30))

#要畫出決策樹(視覺化),雖然用平常的plot()就可以達成,
plot(iris.tree);text(iris.tree)

#但rpart有專屬的繪圖套件rpart.plot，函式是prp(),
#說真的，用prp()畫出來的決策樹，比較好看一些：
prp(iris.tree,              #模型
    faclen = 0,             #呈現的變樹不要縮寫
    fallen.leaves = TRUE,   #讓樹枝以垂直方式呈現
    shadow.col = "gray",    #最下面的節點塗上陰影
    extra = 2)              # number of correct classifications / number of observations in that node

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

