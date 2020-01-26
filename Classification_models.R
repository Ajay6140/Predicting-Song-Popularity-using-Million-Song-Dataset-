## Create new environment, classification
cls <- new.env()
library(readxl)

##
## Load dataset
##
## Predict popularity
View(music)
str(music)
cls$dt <- music
View(dt)
dim(cls$dt)
## Build the training/validate/test datasets.
## Split into 70/15/15 train/test/val
test <- 0.3
train <- 0.7

cls$nrow <- nrow(cls$dt)
cat(paste("Number of rows: ", cls$nrow, "\n"))

cls$sample <- cls$train <- sample(cls$nrow, train*cls$nrow)
cls$validate <- sample(setdiff(seq_len(cls$nrow), cls$train), val*cls$nrow)
cls$test <- setdiff(setdiff(seq_len(cls$nrow), cls$train), test*cls$nrow)

## The following variable selections have been noted.

cls$input <- c("loudness", "tempo", "time_signature", "key", "mode", "duration")
cls$numeric <- c("loudness", "tempo", "time_signature", "key", "mode", "duration")

cls$target  <- "hit"
cls$ident   <- "track_id"
cls$ignore  <- c("artist_name", "title", "energy", "danceability", "song_hotttnesss")

##
## Decision Tree 
##

View(music)
library(rpart, quietly=TRUE)

cls$dtfit <- rpart(hit ~ .,
                   data=cls$dt[cls$train, c(cls$input, cls$target)],
                   method="class",parms=list(split = "information"),control=rpart.control(minsplit=50,minbucket=50,cp=0.0,maxdepth = 20))

#tree <- rpart(f11~TSU+TSL+TW+TP,data = Z24train,method="class",control =rpart.control(minsplit =1,minbucket=1, cp=0))

help(rpart)

plot(cls$dtfit, uniform=TRUE,  main="Decision Tree")
text(cls$dtfit, use.n=TRUE, all=TRUE, cex=.7)


#CREATING CONFUSION MATRIX 

new <- data.frame(cls$dt)

View(new)

#new$train <- sample(nrow(new)), train*(nrow(new))

#cls$test <- setdiff(setdiff(seq_len(cls$nrow), cls$train), test*cls$nrow)

nr=nrow(music)
trnIndex = sample(1:nr, size = round(0.75*nr), replace=FALSE) 
newTRN=new[trnIndex,]   
newTST = new[-trnIndex,]
dim(newTRN)
dim(newTST)

cm <- table(pred=predict(cls$dtfit,newTST, type="class"), true=newTST$hit)


#help(table)

n = sum(cm) 
diag = diag(cm)  
rowsums = apply(cm, 2, sum) 
colsums = apply(cm, 1, sum) 
p = rowsums / n
q = colsums / n 
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)
accuracy
precision
recall
f1
cm


##
## SVM
##

library(e1071, quietly=TRUE)
cls$svmfit <- svm(hit ~ ., data=newTRN)
help(svm)
print(cls$svmfit)
summary(cls$svmfit)
cls$svmpr <- predict(cls$svmfit, newdata=na.omit(newTRN))


#plot(cls$svmfit,newTST)    NOT WORKING???!!!

print("Finished classifying using SVM")

##
## Random Forest
##

library(randomForest, quietly=TRUE)
print("Classifying using Random Forest...")
cls$rffit <- randomForest               (factor(hit) ~.,
                                        data=newTST,
                                        ntree=100,
                                        mtry=2,
                                        importance=TRUE,
                                        na.action=na.omit,
                                        replace=FALSE)

help(randomForest)

print("Finished classifying using Random Forest")

##
## Evaluate results
##

library(ggplot2, quietly=TRUE)
library(plyr, quietly=TRUE)

print("Obtain the response from the classifyers...")
cls$dtpr <- predict(cls$dtfit, newdata=na.omit(newTST))
cls$svmpr <- predict(cls$svmfit, newdata=na.omit(newTST))

View(cls$svmpr)

cls$rfpr <- predict(cls$rffit, newdata=na.omit(cls$dt[cls$test, c(cls$input, cls$target)]))

## plot tree

cat(paste("Saving a plot of the decision Tree \n"))
jpeg('pop_dt_plot.jpg')
plot(cls$dtfit, uniform=TRUE, 
     main="Classification Tree for Hit")
text(cls$dtfit, use.n=TRUE, all=TRUE, cex=.8)
dev.off()

## Plot count of popularity distribution

## Plot count of genre distribution

print("Creating a popularity distribution")
jpeg('popularity_distribution.jpg')
g1<-ggplot(cls$dt, aes(x=factor(1), fill=factor(popular))) + geom_bar(width = 1)
plot(g1 + coord_polar(theta="y"))
dev.off()
print("Saved popular distribution plot")