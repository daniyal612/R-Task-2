df <- read.csv("E:/Machine Learning With R/Data Files/Data Files/Logistic Reg Dataset/House-Price.csv", header = TRUE)

str(df)

summary(df)
boxplot(df$n_hot_rooms)

pairs(~df$Sold+df$rainfall)

barplot(table(df$airport))
barplot(table(df$bus_ter))

# 1) Rainfall, n_hot_rooms have outliers
# 2) n_hos_beds has missing values
# 3) Bus_ter is useless


quantile(df$n_hot_rooms,0.99)
uv <- 3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>uv] <- uv

summary(df$n_hot_rooms)


lv <- 0.3*quantile(df$rainfall, 0.01)
df$rainfall[df$rainfall<lv] <- lv
summary(df$rainfall)

mean(df$n_hos_beds)
mean(df$n_hos_beds,na.rm = TRUE)

which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds,na.rm = TRUE)
summary(df$n_hos_beds)

which(is.na(df$n_hos_beds))



df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4

# Deleting Variables

df2 <- df[,-6:-9]
df = df2

rm(df2)

df <- df[,-13]


# Dummy variable creation

df <- dummy.data.frame(df)
df <- df[,-8]
df <- df[,-13]

# Logistic Regression with single predictor

glm.fit = glm(Sold~price , data= df, family = binomial)
summary(glm.fit)


# Logistic Regression with multiple predictor

glm.fit = glm(Sold~. , data= df, family = binomial)
summary(glm.fit)

glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]

glm.pred = rep("NO",506)
glm.pred[glm.probs>0.5] = "YES"

table(glm.pred,df$Sold)

# Linear discriminant analysis
lda.fit = lda(Sold~., data = df)

lda.fit
lda.pred = predict(lda.fit, df)
lda.pred$posterior

lda.class = lda.pred$class
table(lda.class, df$Sold)

sum(lda.pred$posterior[ ,1]>0.8)


# quadratic discriminant analysis
qda.fit = qda(Sold~., data = df)

qda.fit
qda.pred = predict(qda.fit, df)
qda.pred$posterior

qda.class = qda.pred$class
table(qda.class, df$Sold)

sum(qda.pred$posterior[ ,1]>0.8)

# TEST-TRAIN Split

set.seed(0)
split = sample.split(df,SplitRatio = 0.8)
train_set = subset(df,split == TRUE)
test_set = subset(df,split == FALSE)

train.fit = glm(Sold~., data = train_set, family = binomial)
test.probs = predict(train.fit,test_set, type = 'response')

test.pred = rep ('NO',120)
test.pred[test.probs>0.5] = 'YES'
table(test.pred,test_set$Sold)

# KNN classifier

trainX = train_set[,-16]
testX = test_set[,-16]
trainy = train_set$Sold
testy = test_set$Sold

k = 3

trainX_s = scale(trainX)
testX_s = scale(testX)

set.seed(0)

knn.pred = knn(trainX_s, testX_s,trainy, k = k)

table(knn.pred,testy)



















