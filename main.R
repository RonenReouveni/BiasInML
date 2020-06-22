head(adult,100)
str(adult)


col_names <- c("age","workclass", "fnlwgt","education","educationNum","maritalStatus","occupation","relationship","race","sex","capitalGain","capitalLoss","hoursPerWeek","nativeCountry","income")
colnames(adult) <- col_names
myData <- adult 
refData <- adult
incomeChar <- as.character(myData$income)
incomeChar[which(incomeChar == " <=50K")] = 1
incomeChar[which(incomeChar == " >50K")] = 2
incomeFull <- as.numeric(incomeChar)
myData$income <- incomeFull

str(myData)


fixFactor <- function(x) {
  as.character(x)
}


myData$workclass <- fixFactor(myData$workclass)
myData$education <- fixFactor(myData$education)
myData$maritalStatus <- fixFactor(myData$maritalStatus)
myData$occupation <- fixFactor(myData$occupation)
myData$relationship <- fixFactor(myData$relationship)
myData$race <- fixFactor(myData$race)
myData$sex <- fixFactor(myData$sex)
myData$nativeCountry <- fixFactor(myData$nativeCountry)

myData$workclass <- gsub(" ","",myData$workclass)
myData$education <- gsub(" ","",myData$education)
myData$maritalStatus <- gsub(" ","",myData$maritalStatus)
myData$occupation <- gsub(" ","",myData$occupation)
myData$relationship <- gsub(" ","",myData$relationship)
myData$race <- gsub(" ","",myData$race)
myData$sex <- gsub(" ","",myData$sex)
myData$nativeCountry <- gsub(" ","",myData$nativeCountry)


myCleanData <- myData[-which(myData$workclass == "?"),]
myCleanData <- myCleanData[-which(myCleanData$nativeCountry == "?"),]
myCleanData <- myCleanData[-which(myCleanData$occupation == "?"),]


myCleanData$workclass <- as.factor(myCleanData$workclass)
myCleanData$education <- as.factor(myCleanData$education)
myCleanData$maritalStatus <- as.factor(myCleanData$maritalStatus)
myCleanData$occupation <- as.factor(myCleanData$occupation)
myCleanData$relationship <- as.factor(myCleanData$relationship)
myCleanData$race <- as.factor(myCleanData$race)
myCleanData$sex <- as.factor(myCleanData$sex)
myCleanData$nativeCountry <- as.factor(myCleanData$nativeCountry)
myCleanData$income <- as.factor(myCleanData$income)

str(myCleanData)





#this distrobution of race in the study is very skewed white 
#a lack of samples for other races means training and testing set must be closer to .5 to get enough samples to test
tapply(myCleanData$income,myCleanData$race, length)
counts <- c(286, 895, 2817, 231,25933)
race_intro <- c("AIE","API","Black","Other","White")
df_intro <- data.frame(counts,race_intro)


ggplot(df_intro, aes(x=race_intro, y =counts )) + 
  geom_bar(stat="identity") +
  theme_minimal()



#the data skews heavily male but we have enough sample of both genders to work with 
tapply(myCleanData$income,myCleanData$sex, length)
countsG <- c(9782, 20380)
gen_intro <- c("Female","Male")
dfG_intro <- data.frame(countsG,gen_intro)


ggplot(dfG_intro, aes(x=gen_intro, y =countsG )) + 
  geom_bar(stat="identity") +
  theme_minimal()









install.packages("randomForest")
install.packages("pROC")
install.packages("reshape2")
install.packages("caret")
install.packages("ISLR")
install.packages("tree")
install.packages("kernlab")
install.packages("e1071")


library(caret)
library(e1071)
library(randomForest)
library(pROC)
library(ggplot2)
library(reshape2)
library(cluster)
library(ISLR)
library(tree)
library(kernlab)



str(myCleanData)
head(myCleanData)

D = myCleanData[,1:14]
Y = myCleanData[,15]




#full data is array of indecies 
fullData <- nrow(myCleanData)
train_idx <- sample(c(1:fullData), size = fullData * 0.5, replace = FALSE)






str(myCleanData)

#SVM model (85.07% accurate) NEEDS WORK
svm1 <-svm(income ~ ., data = myCleanData)
summary(svm1)
pred <- predict(svm1,D)
Z <- myCleanData[15]
confusionMatrix(pred,Z$income)





#svm class rate 84.49%
svm_noT <- svm(income ~., data = myCleanData[train_idx,])
svm_noT_pred <- predict(svm_noT, myCleanData[-train_idx,])
svm_result <- table(svm_noT_pred,myCleanData$income[-train_idx])
svm_rate <- (svm_result[1,1] + svm_result[2,2])/(svm_result[1,1] + svm_result[2,2] + svm_result[1,2] + svm_result[2,1])
svm_rate


#svm class rate 84.74%
support2 <- ksvm(income ~., data = myCleanData[train_idx,], kernal="besseldot", kpar = "automatic", C=3)
supp_pred2 <- predict(support2, myCleanData[-train_idx,])
supp_result2 <- table(supp_pred2,myCleanData$income[-train_idx])
supp_rate2 <- (supp_result2[1,1] + supp_result2[2,2])/(supp_result2[1,1] + supp_result2[2,2] + supp_result2[1,2] + supp_result2[2,1])
supp_rate2



#RANDOM FOREST STARTS HERE
rF2 <- randomForest(D[train_idx,],Y[train_idx])
pred <- as.data.frame(predict(rF2, D[-train_idx,],type="prob"))
rF2$importance

#full data with full rate (85.7%)
newCol_1 <- ifelse(pred>.5, "1","2")
table(newCol_1[,1], Y[-train_idx])
mean(newCol_1[,1] == Y[-train_idx])

plot(roc(Y[-train_idx], as.numeric(pred[,1])))
rF2$importance
rF2


















X <- myCleanData[-train_idx,1:14]
P <- myCleanData[-train_idx,15]


R <- X[which(X$race=="Black"),]
TR <- P[which(X$race=="Black")]



#black classification rate (92%)
blackpred <- predict(rF2, R ,type="prob")
blackDf <- data.frame(blackpred)
newCol <- ifelse(blackDf$X1>.5, "1","2")
predTable <- table(newCol, TR)
mean(newCol==TR)

nrow(blackDf)



#classification rates for group 1 and 2 based on race
acc1 <- predTable[1,1]/(predTable[2,1] + predTable[1,1])
acc2 <- predTable[2,2]/(predTable[1,2] + predTable[2,2])

black2rate <- acc2



#white classification rate (85.17)
W <- X[which(X$race=="White"),]
AL <- P[which(X$race=="White")]
length(AL)



whitepred <- predict(rF2, W ,type="prob")
whiteDF <- data.frame(whitepred)
newCol_2 <- ifelse(whiteDF$X1>.5, "1","2")
predTable2 <- table(newCol_2, AL)
mean(newCol_2==AL)



#classification rates for group 1 and 2 based on race
acc_1 <- predTable2[1,1]/(predTable2[2,1] + predTable2[1,1])
acc_2 <- predTable2[2,2]/(predTable2[1,2] + predTable2[2,2])

white2rate <- acc_2





str(myCleanData)
summary(myCleanData)

#Amer-Indian-Eskimo classification rate (89.65%)
IN <- X[which(X$race=="Amer-Indian-Eskimo"),]
EM <- P[which(X$race=="Amer-Indian-Eskimo")]
length(EM)

IN_EM <- predict(rF2, IN ,type="prob")
IN_EM_DF <- data.frame(IN_EM)
newCol_3 <- ifelse(IN_EM_DF$X1>.5, "1","2")
predTable3 <- table(newCol_3, EM)
mean(newCol_3==EM)


#classification rates for group 1 and 2 based on race
acc_11 <- predTable3[1,1]/(predTable3[2,1] + predTable3[1,1])
acc_22 <- predTable3[2,2]/(predTable3[1,2] + predTable3[2,2])

AIE2rate <- acc_22






#Asian-Pac-Islander classification rate (81.89%)
summary(X)
str(X)
APC <- X[which(X$race=="Asian-Pac-Islander"),]
APC_t <- P[which(X$race=="Asian-Pac-Islander")]
length(APC_t)

APC_EM <- predict(rF2, APC ,type="prob")
APC_t_DF <- data.frame(APC_EM)
newCol_4 <- ifelse(APC_t_DF$X1>.5, "1","2")
predTable4 <- table(newCol_4, APC_t)
mean(newCol_4==APC_t)


#classification rates for group 1 and 2 based on race
acc_111 <- predTable4[1,1]/(predTable4[2,1] + predTable4[1,1])
acc_222 <- predTable4[2,2]/(predTable4[1,2] + predTable4[2,2])

APE2rate <- acc_222




#Other classification rate (77.89%)
summary(X)
str(X)
OT <- X[which(X$race=="Other"),]
OT_T <- P[which(X$race=="Other")]
length(OT_T)

OT_EM <- predict(rF2, OT ,type="prob")
OT_t_DF <- data.frame(OT_EM)
newCol_5 <- ifelse(OT_t_DF$X1>.5, "1","2")
predTable5 <- table(newCol_5, OT_T)
mean(newCol_5==OT_T)

#classification rates for group 1 and 2 based on race
acc_1111 <- predTable5[1,1]/(predTable5[2,1] + predTable5[1,1])
acc_2222 <- predTable5[2,2]/(predTable5[1,2] + predTable5[2,2])

Other2rate <- acc_2222

table(newCol, TR)
mean(newCol==TR)





#take rates and put them into a vector 
#vector of classification rate for only factor 2
classVector <- c(black2rate, white2rate,AIE2rate,APE2rate,Other2rate)
needDF <- data.frame(classVector)

needDF$race <- c("black","white","AIE","APE","Other")

needDF$prob <- (1-needDF$classVector)

colnames(needDF) <- c("classRate","race","missClassRate")





#classification rate based on race ONLY FOR CAT 2
ggplot(needDF, aes(x=race,y=classRate))+
  geom_bar(stat="identity")+
  theme_minimal()



#missclassification rate for each rate ONLY FOR CAT 2
ggplot(needDF, aes(x=race,y=missClassRate))+
  geom_bar(stat="identity")+
  theme_minimal()




#random forest rate
mean(newCol_1[,1] == Y[-train_idx])




#WEIGHTED average
(mean(newCol==TR) * length(TR) + 
    mean(newCol_2==AL) * length(AL) + 
    length(EM)* mean(newCol_3==EM) + 
    length(APC_t)*mean(newCol_4==APC_t) + 
    length(OT_T)*mean(newCol_4==APC_t))/nrow(X)







str(X)

#92.9% classification rate
Gen <- X[which(X$sex=="Female"),]
Gen_T <- P[which(X$sex=="Female")]

O_Gen <- predict(rF2, Gen, type = "prob")
OG_DF <- data.frame(O_Gen)
newCol_6F <- ifelse(OG_DF$X1>.5, "1","2")
predTable6F <- table(newCol_6F, Gen_T)
mean(newCol_6F==Gen_T)


#classification rates for gender
acc_F1 <- predTable6F[1,1]/(predTable6F[2,1] + predTable6F[1,1])
acc_F2 <- predTable6F[2,2]/(predTable6F[1,2] + predTable6F[2,2])


genFMiss <- acc_F2



GenM <- X[which(X$sex=="Male"),]
Gen_TM <- P[which(X$sex=="Male")]

O_GenM <- predict(rF2, GenM, type = "prob")
OG_DM <- data.frame(O_GenM)
newCol_6M <- ifelse(OG_DM$X1>.5, "1","2")
predTable6M <- table(newCol_6M, Gen_TM)
mean(newCol_6M==Gen_TM)

acc_M1 <- predTable6M[1,1]/(predTable6M[2,1] + predTable6M[1,1])
acc_M2 <- predTable6M[2,2]/(predTable6M[1,2] + predTable6M[2,2])

genMMiss <- acc_M2


genRate <- c(genFMiss,genMMiss)
gender <- c("female","male")
missGclass <- 1 - genRate
genDf <- data.frame(genRate,gender,missGclass)


#missclassification rate for each gender ONLY FOR CAT 2
ggplot(genDf, aes(x=gender,y=genRate))+
  geom_bar(stat="identity")+
  theme_minimal()

#missclassification rate for each gender ONLY FOR CAT 2
ggplot(genDf, aes(x=gender,y=missGclass))+
  geom_bar(stat="identity")+
  theme_minimal()






















#really cool forest visualiztions that I dont understand at all
# devtools::install_github("MI2DataLab/randomForestExplainer")
install.packages("randomForestExplainer")
library(randomForestExplainer)
min_depth_frame <- min_depth_distribution(rF2)
save(min_depth_frame, file = "min_depth_frame.rda")
load("min_depth_frame.rda")
head(min_depth_frame, n = 10)
plot_min_depth_distribution(min_depth_frame)
importance_frame <- measure_importance(rF2)
save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
importance_frame
plot_importance_rankings(importance_frame)

rF2$importance














#LARGER DECISSION TREE
fun <- tree(income ~. -nativeCountry ,myCleanData)
plot(fun)
text(fun)




#Creating a decision tree with only integers for interesting visualization 
str(myCleanData)
onlyInts <- myCleanData[,c(1,3,5,11,12,13,15)]
str(onlyInts)

tree1 <- tree(income ~ ., onlyInts)
plot(tree1)
text(tree1)
summary(tree1)



#Random Forest with only integers 
head(onlyInts)
A <- onlyInts[,1:6]
B <- onlyInts[,7]
rfInt <- randomForest(A,B)
rfInt_1 <- randomForest(A[train_idx,],B[train_idx])
pred_1 <- predict(rfInt_1, A[-train_idx,],type="prob")
plot(roc(B[-train_idx], as.numeric(pred_1[,1])))
rfInt_1$importance
rfInt_1
































#Logistic regression %85 accuracy BROKEN UGHHHHHHHHHHHHHHH
lg <- glm(income ~ . , myCleanData, family = 'binomial', subset = train_idx)
lg_probs <- predict(lg, newdata =  myCleanData[!train_idx,], type = "response") 
lg_pred <- ifelse(lg_probs>.5, "2","1")
table(lg_pred, fixit$income)
mean(lg_pred==fixit$income)












onlyInts[,1] <- norm_func(onlyInts[,1])
onlyInts[,2] <- norm_func(onlyInts[,2])
onlyInts[,3] <- norm_func(onlyInts[,3])
onlyInts[,4] <- norm_func(onlyInts[,4])
onlyInts[,5] <- norm_func(onlyInts[,5])
onlyInts[,6] <- norm_func(onlyInts[,6])
str(onlyInts)

A <- onlyInts[,1:6]
B <- onlyInts[,7]
rfInt <- randomForest(A,B)
rfInt_1 <- randomForest(A[train_idx,],B[train_idx])
pred_1 <- predict(rfInt_1, A[-train_idx,],type="prob")
plot(roc(B[-train_idx], as.numeric(pred_1[,1])))
rfInt_1$importance
rfInt_1




install.packages("ggalt")
install.packages("ggfortify")
library(ggplot2)
library(ggalt)
library(ggfortify)
theme_set(theme_classic())

# Compute data with principal components ------------------
df <- onlyInts[,1:6]
pca_mod <- prcomp(df)  # compute principal components
pca_mod
# Data frame of principal components ----------------------
df_pc <- data.frame(pca_mod$x, income=onlyInts$income)  # dataframe of principal components
df_pc_1 <- df_pc[df_pc$Species == "1", ] 
df_pc_2 <- df_pc[df_pc$Species == "2", ]  

# Plot ----------------------------------------------------
ggplot(df_pc, aes(PC1, PC2, col=income)) + 
  geom_point(aes(shape=income), size=2) +   # draw points
  labs(title="Iris Clustering", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: Iris") + 
  coord_cartesian(xlim = c(min(df_pc$PC1), max(df_pc$PC1)), 
                  ylim =  c(min(df_pc$PC2), max(df_pc$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_1, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_2, aes(x=PC1, y=PC2)) 






library(cluster)    # clustering algorithms
install.packages("factoextra")
library(factoextra) # clustering algorithms & visualization

k2 <- kmeans(onlyInts[,1:6], centers = 2, nstart = 25)

fviz_cluster(k2, data = onlyInts[,1:6])

































workingData <- myCleanData[,-c(11,12)]

str(workingData)

#pairs(workingData, col = workingData$income)

install.packages("randomForest")
library(randomForest)

M = workingData[,1:12]
L = workingData[,13]
fun <- randomForest(M, L)
fun
fun$importance



str(workingData)
summary(workingData)

betterData <- workingData[,-5]
str(workingData)
str(betterData)

M = betterData[,1:11]
L = betterData[,12]


m_l <- randomForest(M,L)
m_l
head(M)


library(rpart)
install.packages("e1071")
library(e1071)

levels(workingData$race) 
summary(workingData$race)

bestData <- betterData[,1]
bestData1 <- betterData[,3]
bestData2 <- betterData[,10]

onlyInts <- data.frame(bestData,bestData1,bestData2)



X = onlyInts 
W = betterData[,12]

ints <- randomForest(X,W)
ints



test <- workingData[workingData$race == "Black",]
head(test)
str(workingData$race)


fit <- glm(income ~ education, workingData, family = 'binomial')


fit1 <- glm(education ~ race, workingData, family = 'binomial')
summary(fit1)

summary(fit)



fit1 <-naiveBayes(income ~ education, data = workingData)
fit1






