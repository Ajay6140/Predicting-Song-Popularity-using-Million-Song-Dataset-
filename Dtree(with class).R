##
## Thomas Astad Sve
##
##

## Create new environment, classification

cls <- new.env()
library(readxl)

##
## Load dataset
##
## Predict popularity

View(music)
cls$dt <- music
View(dt)
dim(cls$dt)

cols <- c("artist.id","artist.name","artist_mbtags","location","mode","release.name","similar","song.id","terms","title")

#music[cols] <- lapply(cls$dt[cols], factor)
#sapply(cls$dt, class)
#str(cls$dt)

str(cls$dt)

## Build the training/validate/test datasets.
## Split into 70/15/15 train/test/val

test <- 0.3
train <- 0.7

cls$nrow <- nrow(cls$dt)
cat(paste("Number of rows: ", cls$nrow, "\n"))

cls$sample <- cls$train <- sample(cls$nrow, train*cls$nrow)
cls$test <- setdiff(setdiff(seq_len(cls$nrow), cls$train), test*cls$nrow)

## The following variable selections have been noted.

cls$input <- c("loudness","artist.hotttnesss", "tempo", "time_signature", "mode", "duration")
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
                   method="class",control=rpart.control(minisplit=1,minibucket=1,cp=0,maxdepth = 5))

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
trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE) 
newTRN=new[trnIndex,]   
newTST = new[-trnIndex,]
dim(newTRN)

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
