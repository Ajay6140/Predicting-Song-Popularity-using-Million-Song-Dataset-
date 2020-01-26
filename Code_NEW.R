library(rhdf5)

# Load data-one song.
filePath = paste("C:/Users/arpit/Documents/MillionSongSubset/data/A/A/A/TRAAAAW128F429D538.h5",sep="")
h5ls(filePath)
dataset = h5read(file=filePath, name="/analysis/beats_start")
View(dataset)
str(dataset)
#build a function to read the songs data component of every song in hdf5 files
import_row<-function(path){
  filePath = paste(path,sep="")
  h5ls(filePath)
  dataset = h5read(file=filePath, name="/analysis/songs")
  View(dataset)
  str(dataset)
}
#Read all the rows in the folder
function_data_combined <- function(directory)
{
  mylist <- list.files(path = directory, full.names = TRUE)
  res <- lapply(mylist, h5read, name="/analysis/songs")
  return(res)
}
#run function
myData <- function_data_combined("C:/Users/arpit/Documents/MillionSongSubset/Merged_files")
library(plyr)
data_final<-ldply(myData, rbind)

#Finally we have our datafile to work on!
#Importing the dataset
music<-read.csv(choose.files())

sum(is.na (music$song.hotttnesss)) #dependent variable missing in 4351 obs

#Take dep variable from spotify api

devtools::install_github('charlie86/spotifyr')

install.packages('spotifyr')
# https://www.rcharlie.com/spotifyr/

library(spotifyr)

Sys.setenv(SPOTIFY_CLIENT_ID = '7fc55dfcfe3f41d2a3dfd472f33dd17f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '8d4bd80c99f2409987afc5a533b64a9c')

access_token <- get_spotify_access_token()
################
#test
kid_a <- get_tracks(artist_name = "Radiohead", track_name = "Kid A", return_closest_track = TRUE)
class(kid_a)
kida1<-as.data.frame(kid_a)
s=kid_a
str(kida1)

# NOT RUN {
track_popularaty <- get_track_popularity(kida1)
# }

track_popularaty['track_popularity'] / 10
##############

titles<-music$title
artist<-music$artist.name

track<-as.data.frame(matrix(1:60000,nrow=10000,ncol=6))
View(track)

track_popularity<-as.data.frame(matrix(ncol=4,nrow=10000))

song_hotness <- vector(mode="numeric", length=10000)

art<-paste(artist[1],sep="")

for (i in 1:length(titles)) {
  art<-paste(artist[i],sep="")
  trk<-paste(titles[i],sep="")
  s<-get_tracks(artist_name = art, track_name = trk, return_closest_track = TRUE)
  if(length(as.data.frame(s))==0){
    s=kid_a }
  track[i,]=as.data.frame(s)
}

track2<-unique(track)
write.csv(track2,"tracks.csv")
colnames(track2)<-colnames(s)

for (i in 1:length(track2$track_name)) {
  track_popularity[i,]=get_track_popularity(track2[i,])
}
popularity_spotify<-track_popularity[1:6693,]
track_spotify<-track2[1:6693,]
spotify_op<-cbind(track_spotify,popularity_spotify)
write.csv(spotify_op,"spotify_op.csv")
getwd()

##############################################################################################
spotift_data<-read.csv(choose.files())

library(dplyr)
spotify_subset<-select(spotift_data,track_name,V2)
colnames(spotify_subset)<-c('title','spotify_popularity')
spotify_non_missing<-filter(spotify_subset,!spotify_popularity=="NA")

data <- subset(spotify_non_missing, !duplicated(spotify_non_missing[,1])) 

data$title  <- as.character(data$title)
music$title <- as.character(music$title)

###Making it to the range from 0 to 1

data$spotify_popularity <- data$spotify_popularity/100

merged_data=left_join(music,data,by="title")

additional<-filter(merged_data,is.na(song.hotttnesss),is.na(spotify_popularity)==FALSE)

merged_data$spotify_popularity <- as.numeric(merged_data$spotify_popularity)
merged_data$target_h<-coalesce(merged_data$song.hotttnesss,merged_data$spotify_popularity)



final_data<- merged_data[!(is.na(merged_data$target_h)),]

View(final_data)
#Now the data from spotify and the base data has been integrated
#Taking out the variables which are not required

write.csv(final_data,"interim_data.csv")
final_data<-read.csv(choose.files())
getwd()
table(final_data$artist_mbtags_count)

#data_1 <-final_data[ ,-which(names(final_data) %in% c("artist.id","artist.name","artist_mbtags","title","artist_mbtags1"))]
################################################################################


#Missing value imputation- we use linear models and knn to impute missing values


################################################################################


#artist_hotness

final_data<- 
final_data[is.na(final_data$familiarity),]
final_data[is.na(final_data)] <- 0
final_data[is.na(final_data$familiarity),]
lm_imputation1<-lm(artist.hotttnesss~familiarity,data=final_data,subset=artist.hotttnesss>0)
#dim(final_data)
#dim((filter(final_data,artist.hotttnesss>0)))
#dim((filter(final_data,artist.hotttnesss>0 & !familiarity=='NA')))
#View(lm_imputation1$model)

pred.1 <- predict (lm_imputation1, final_data)
#View(pred.1)

#impute_1 <- function (a, a.impute){ 
 # for(i in 1:dim(a)){
  #  ifelse((a[i]==0 | is.na(a[i])),a.impute[i],a)
  #}
#}

impute <- function (a, a.impute){ 
  ifelse ( ((a==0)|(is.na(a))&(is.na(a.impute)==FALSE)), a.impute, a)
}

final_data$artist.hotness <- impute (final_data$artist.hotttnesss, pred.1) 
sum(is.na(pred.1))

check<-final_data[final_data$artist.hotttnesss==0,]
table(final_data$artist.hotness)
final_data <-final_data[ , -which(names(final_data) %in%  c("artist.hotttnesss"))]
data_1<-final_data
str(data_1)
as<-table(data_1$familiarity)
View(as)
#check<-data_1[data_1$familiarity==0,]
sum(is.na(data_1$artist.hotness))
data_1[data_1$artist.hotness==0,]


#arist familiarity
str(data_1)
lm_imputation2<-lm(familiarity~artist.hotness,data=data_1,subset=familiarity>0)
#str(lm_imputation2)
pred.2 <- predict (lm_imputation2, data_1)
data_1$artist_familiarity <- impute(data_1$familiarity, pred.2) 

check<-data_1[data_1$familiarity==0,]
data_1 <-data_1[ , -which(names(data_1) %in%  c("familiarity"))]

sum(is.na(data_1$artist_familiarity))
#Knn imputation for song characteristics
library(VIM)
data_2 <-data_1[ ,which(names(data_1) %in%  
                          c("bars_start","beats_start","end_of_fade_in",
                            "loudness","tempo","terms","artist.name","mode","start_of_fade_out",
                            "tatums_start","time_signature"))]

data_2$bars_start<-ifelse(data_2$bars_start==0,NA,data_1$bars_start)
data_2$beats_start<-ifelse(data_2$beats_start==0,NA,data_1$beats_start)
data_2$end_of_fade_in<-ifelse(data_2$end_of_fade_in==0,NA,data_1$end_of_fade_in)
data_2$loudness<-ifelse(data_2$loudness==0,NA,data_1$loudness)
data_2$tatums_start<-ifelse(data_2$tatums_start==0,NA,data_1$tatums_start)
data_2$time_signature<-ifelse(data_2$time_signature==0,NA,data_1$time_signature)
data_2$loudness<-ifelse(data_2$loudness==0,NA,data_1$loudness)
summary(data_2)

?kNN
#Imputing all song characteristics using KNN
data_2_imputed<-kNN(data_2,variable=c("bars_start","beats_start","end_of_fade_in",
                                         "loudness","start_of_fade_out",
                                         "tatums_start","time_signature"),k=3)
summary(data_2_imputed)
View(data_2_imputed[data_2_imputed$bars_start_imp==TRUE,])
data_2_imputed <-data_2_imputed[ ,which(names(data_2_imputed) %in%  
                           c("bars_start","beats_start","end_of_fade_in",
                             "loudness","start_of_fade_out",
                             "tatums_start","time_signature"))]
data_3 <-data_1[ ,-which(names(data_1) %in%  
                          c("bars_start","beats_start","end_of_fade_in",
                            "loudness","start_of_fade_out",
                            "tatums_start","time_signature"))]
data_4<-cbind(data_3,data_2_imputed)
summary(data_4)
#KNN done, now removing id variables, artist mbtags had low count, 
#confidence variables dont carry any meaning, release id, name as well as it will be distinct for every song
#Song hottness and spotify popularity have been coalesced earlier to form target_h, so we remove the two as well

data_5 <-data_4[ ,-which(names(data_4) %in%  
                           c("artist.id",
                             "artist_mbtags",
                             "artist_mbtags_count",
                             "bars_confidence",
                             "beats_confidence",
                             "key_confidence",
                             "mode_confidence",
                             "release.id",
                             "release.name",
                             "song.hotttnesss",
                             "song.id",
                             "tatums_confidence",
                             "terms_freq",
                             "time_signature_confidence",
                             "title",
                             "spotify_popularity"
                           ))]

data_5$artist.name<-as.factor(data_5$artist.name)
library(dplyr)

#yr<-group_by(data_5,artist.name)
#year_avg<-summarize(yr, yr_mean = mean(year, na.rm = T))
#year_avg[year_avg$yr_mean==0,]
#cant impute year using average of year on artist.name as we thought 
#so we'll just replace missing values by NA

data_5$yr1<-as.numeric(data_5$year)
data_5$decade<-data_5$yr1-(data_5$yr1%%10)
data_5$decade1<-as.character(data_5$decade)
data_5$decade2<-ifelse(data_5$decade1=="0","Not Available",
                                 data_5$decade1)
dim(data_5)

final_data_for_model <-data_5[, -which(names(data_5) %in% 
                                                    c("decade1","decade","yr1","year"))]

dim(final_data_for_model)
write.csv(final_data_for_model,"interim_data2.csv")
getwd()
final_data_for_model <-final_data_for_model[, -which(names(final_data_for_model) %in% 
                                         c("X"))]

#We have our final dataset but we now convert the "terms" variable to document 
#Oh shit forgot about tempo-have to IMPUTE VALUES for tempo

final_data_for_model$tempo<-ifelse(final_data_for_model$tempo==0,NA,
                                      final_data_for_model$tempo)
summary(final_data_for_model)
dim(final_data_for_model)
library(mice)
dim(final_data_for_model)
?mice
#tempData <- mice(final_data_for_model,where = is.na(final_data_for_model),m=1,meth='pmm',seed=500)

for(i in 1:ncol(final_data_for_model))
  {
  final_data_for_model[is.na(final_data_for_model[,i]), i] <- mean(final_data_for_model[,i], na.rm = TRUE)
}
summary(final_data_for_model)
dim(final_data_for_model)
#frequency matrix

library(quanteda)
#class(final_data_for_model$terms)
final_data_for_model$terms<-as.character(final_data_for_model$terms)
dfma<-dfm(final_data_for_model$terms)
class(dfma)
dfma1<-convert(dfma, to = "data.frame")
View(head(dfma1))
str(dfma1)
#370 columns added
final_data_for_modelling<-data.frame(final_data_for_model,dfma1)
str(final_data_for_modelling)
View(head(final_data_for_modelling))

dim(final_data_for_modelling)

final_data_for_modelling<-final_data_for_modelling[, -which(names(final_data_for_modelling) %in% 
                                                            c("document","terms","similar"))]

final_data_for_modelling$location<-as.factor(final_data_for_modelling$location)
str(final_data_for_modelling)

#######################################Modeling#########################################

final_data_for_modelling$target_hotness<-ifelse(final_data_for_modelling$target_h>0.5,"1","0")

table(final_data_for_modelling$target_hotness)
#Final-data=Million song dataset - msd
msd<-final_data_for_modelling[, -which(names(final_data_for_modelling) %in% 
                                                              c("target_h","location","artist.name"))]
msd$decade2<-as.factor(msd$decade2)
msd$target_hotness<-as.factor(msd$target_hotness)
#75% train , 25% test

nrow=nrow(msd)
set.seed(123) 
index=sample(1:nrow,size=(0.75*nrow))
index
train_data=msd[index,]
test_dat=msd[-index,]
class(train_data$target_hotness)
summary(msd)

class(train_data$target_hotness)
#train_data=msd[index,]
#test_dat=msd[-index,]
all_vars<-colnames(train_data)
write.csv(all_vars,"vars.csv")
table(test_dat$target_hotness)



#Random forest for feature selection for KNN
#install.packages("randomForest")

Y<-msd$target_hotness
X=msd[,-386]
library(randomForest)
set.seed(1234)
fit <- randomForest(x=X,y=Y,ntree=500,importance=TRUE)
?randomForest
imp_var<-importance(fit)
varImpPlot(fit)
print(fit) # view results 
importance(fit) # importance of each predictor
table(msd$decade2)

input <- c('decade2',
           'artist_familiarity',
           'artist.hotness',
           'loudness',
           'metal',
           'dancehall',
           'punk',
           'smooth',
           'folktronica',
           'illbient',
           'trip',
           'ccm',
           'grunge',
           'techno',
           'space',
           'tempo',
           'target_hotness'
)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

msd_1<-msd[,input]
msd_normalized<-as.data.frame(lapply(msd_1[,c("loudness","tempo")], normalize))
msd_2<-msd_1[, -which(names(msd_1) %in% c("loudness","tempo"))]
msd_3<-data.frame(msd_2,msd_normalized)
set.seed(123) 
nrow
index=sample(1:nrow,size=(0.75*nrow))
index
train_data=msd_3[index,]
test_dat=msd_3[-index,]

dim(train_data)
dim(test_dat)
#target  <- c("target_hotness")
#c<-train_data[, c(input,target)]

#install.packages("class")

library(class)
?knn
train_data_2 <- train_data %>% mutate_if(is.factor,as.numeric)
test_data_2 <- test_dat %>% mutate_if(is.factor,as.numeric)
str(train_data_2)
prc_test_pred <- knn(train = train_data_2[,-15], test = test_data_2[,-15],
                     cl = train_data_2$target_hotness, k=9,prob=FALSE)

prc_test_predtn<-ifelse(prc_test_pred=="1",0,1)

test_data_2$target_hotness<-ifelse(test_dat$target_hotness=="1",0,1)

cm <- table(pred=prc_test_predtn, true=test_data_2$target_hotness)

cm

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

install.packages("caret")
library(ISLR)
library(rlang)
library(caret)

set.seed(400)

ctrl <- trainControl(method="repeatedcv",repeats = 3)

knnFit <- train(target_hotness ~ ., data = train_data_2, method = "knn", 
                trControl = ctrl,tuneLength = 20)
knnFit

#Use plots to see optimal number of k:
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)

plot(knnFit)

#Decision trees

library(rpart)
train_data=msd[index,]
test_dat=msd[-index,]
dtfit <- rpart(target_hotness ~ .,
               data=train_data,
               method="class",control=rpart.control(minisplit=20,minibucket=20,cp=0.008,maxdepth = 30))
summary(dtfit)
plot(dtfit, uniform=TRUE,  main="Decision Tree")
text(dtfit, use.n=TRUE, all=TRUE, cex=.7)

pqrs = predict(dtfit,test_dat, type="class",probability = TRUE) 

cm <- table(pred=predict(dtfit,test_dat, type="class"), true=test_dat$target_hotness)

cm

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

#tune the model and try for varying value of cp and minsplit 

install.packages("sos")
library(e1071)
obj3 <- tune.rpart(target_hotness ~ .,
                   data=train_data,minsplit = c(5,10,15,25,30,50,70))
summary(obj3)
obj4 <- tune.rpart(target_hotness ~ .,
                   data=train_data,cp = c(0.005,0.008,0.01,0.015,0.02,0.05,0.075,0.1,0.2))
summary(obj4)


#######Logistic Regression#######

train_data=msd[index,]
test_dat=msd[-index,]
mylogit <- glm(target_hotness ~ .,
               data=train_data, family = "binomial")

summary(mylogit)
pred_LR <- predict(mylogit, newdata = test_dat, type = "response")
aw<-as.data.frame(pred_LR)
threshold=0.52
aw$plr<-ifelse(aw$pred_LR>threshold,1,0)
cm2 <- table(pred=aw$plr, true=test_dat$target_hotness)
cm2
#help(table)
n = sum(cm2) 
diag = diag(cm2)  
rowsums = apply(cm2, 2, sum) 
colsums = apply(cm2, 1, sum) 
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

#Now we use regularization (L1 and L2) and tune the model by trying various lambda values to get 

#install.packages("glmnet")

library(Matrix)
library(glmnet)
remove.packages("glmnet")
library(glmnet)

gtrain_data=msd[index,]
test_dat=msd[-index,]
Y<-train_data$target_hotness
X<-train_data[,-386]

na=X[is.na(X)]
na=Y[is.na(Y)]
View(na)

?cv.glmnet

train_sparse <- model.matrix(~.,X)
train_sparse
str(Y)


CV=cv.glmnet(x=train_sparse,y=Y,family="binomial",type.measure="class",alpha=1,nlambda=100)
print(CV)

#lambda.lse=0.112565

s=summary(CV)
?glmnet
model_l1<-glmnet(x=train_sparse,y=Y,family="binomial",alpha=1,lambda=0.116526)


X_test<-test_dat[,-386]
test_sparse <- model.matrix(~.,X_test)

pred_l1 <- predict(model_l1,test_sparse, type = "response",probability = TRUE)
View(pred_l1)
?predict.glm
#pred_l1 <- predict.glm(object=model_l1, newdata = test_sparse, type = "response")
aw<-as.data.frame(pred_l1)
View(aw)
threshold=0.34
aw$plr<-ifelse(aw$s0>threshold,1,0)
cm2 <- table(pred=aw$plr, true=test_dat$target_hotness)
cm2
#help(table)
n = sum(cm2) 
diag = diag(cm2)  
rowsums = apply(cm2, 2, sum) 
colsums = apply(cm2, 1, sum) 
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
###########Now Ridge#################

CV=cv.glmnet(x=train_sparse,y=Y,family="binomial",type.measure="class",alpha=0,nlambda=100)
print(CV)
model_l1<-glmnet(x=train_sparse,y=Y,family="binomial",alpha=0,lambda=0.206077)

pred_l1 <- predict(model_l1,test_sparse, type = "response")

aw<-as.data.frame(pred_l1)
tr<-function(threshold) {

aw$plr<-ifelse(aw$s0>threshold,1,0)
cm2 <- table(pred=aw$plr, true=test_dat$target_hotness)
cm2
#help(table)
n = sum(cm2) 
diag = diag(cm2)  
rowsums = apply(cm2, 2, sum) 
colsums = apply(cm2, 1, sum) 
p = rowsums / n
q = colsums / n 
accuracy = sum(diag) / n 
return(accuracy)
}
tr(0.3)
tr(0.35)
tr(0.4)
tr(0.45)
tr(0.5)
tr(0.55)
tr(0.6)
tr(0.41)
tr(0.42)
tr(0.43)
tr(0.44)
tr(0.45)
threshold=0.42
aw$plr<-ifelse(aw$s0>threshold,1,0)
cm2 <- table(pred=aw$plr, true=test_dat$target_hotness)
n = sum(cm2) 
diag = diag(cm2)  
rowsums = apply(cm2, 2, sum) 
colsums = apply(cm2, 1, sum) 
p = rowsums / n
q = colsums / n 
accuracy = sum(diag) / n 
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)
accuracy
precision
recall
f1


##############Random forest###############

train_data=msd[index,]
test_dat=msd[-index,]
library(randomForest)
set.seed(1234)
Y<-train_data$target_hotness
X=train_data[,-386]

fit <- randomForest(x=X,y=Y,ntree=500,importance=TRUE)

cm <- table(pred=predict(fit,test_dat, type="class"), true=test_dat$target_hotness)

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

################Naive Bayes#################

msd_for_NB<-msd
class(msd_for_NB$target_hotness)

msd_for_NB$latitude <- cut(msd_for_NB$latitude,pretty(msd_for_NB$latitude,5),include.lowest = TRUE)
msd_for_NB$beats_start <- cut(msd_for_NB$beats_start,pretty(msd_for_NB$beats_start,5),include.lowest = TRUE)
msd_for_NB$duration <- cut(msd_for_NB$duration,pretty(msd_for_NB$duration,5),include.lowest = TRUE)
msd_for_NB$longitude <- cut(msd_for_NB$longitude,pretty(msd_for_NB$longitude,5),include.lowest = TRUE)
msd_for_NB$tempo <- cut(msd_for_NB$tempo,pretty(msd_for_NB$tempo,5),include.lowest = TRUE)
msd_for_NB$artist.hotness <- cut(msd_for_NB$artist.hotness,pretty(msd_for_NB$artist.hotness,5),include.lowest = TRUE)
msd_for_NB$artist_familiarity <- cut(msd_for_NB$artist_familiarity,pretty(msd_for_NB$artist_familiarity,5),include.lowest = TRUE)
msd_for_NB$bars_start <- cut(msd_for_NB$bars_start,pretty(msd_for_NB$bars_start,5),include.lowest = TRUE)
msd_for_NB$end_of_fade_in <- cut(msd_for_NB$end_of_fade_in,pretty(msd_for_NB$end_of_fade_in,5),include.lowest = TRUE)
msd_for_NB$loudness <- cut(msd_for_NB$loudness,pretty(msd_for_NB$loudness,5),include.lowest = TRUE)
msd_for_NB$start_of_fade_out <- cut(msd_for_NB$start_of_fade_out,pretty(msd_for_NB$start_of_fade_out,5),include.lowest = TRUE)
msd_for_NB$tatums_start <- cut(msd_for_NB$tatums_start,pretty(msd_for_NB$tatums_start,5),include.lowest = TRUE)
msd_for_NB$time_signature <- cut(msd_for_NB$time_signature,pretty(msd_for_NB$time_signature,5),include.lowest = TRUE)
#Converting all terms variables to factors
names <- c(2,5,16:386)
msd_for_NB[,names] <- lapply(msd_for_NB[,names] , factor)
str(msd_for_NB)
naive_bayes_data<-msd_for_NB

train_data=naive_bayes_data[index,]
test_dat=naive_bayes_data[-index,]

#Model
naive_bayes_model <- naiveBayes(target_hotness ~.,data = train_data,laplace = 1)
?naiveBayes

prediction <- predict(naive_bayes_model, newdata=test_dat,probability = TRUE)

cm <- table(pred=prediction, true=test_dat$target_hotness)

cm
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

################SVM#################
library(e1071, quietly=TRUE)
print("Classifying using SVM...")

svm_data <- msd[,-386] %>% mutate_if(is.factor,as.numeric)

svm_data2<-scale(svm_data)
tv<-as.data.frame(msd[,386])
colnames(tv)<-c("target_hotness")
svm_data2=data.frame(svm_data2,tv)
#View(svm_data2)
train_data=as.data.frame(svm_data2[index,])
test_dat=as.data.frame(svm_data2[-index,])

class(test_dat$target_hotness)


svmfit <- svm(target_hotness ~ ., data=train_data)
help(svm)
print(svmfit)
summary(svmfit)

svmpr <- predict(svmfit, newdata=test_dat)
cm <- table(pred=svmpr, true=test_dat$target_hotness)
cm
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


library(ROCR)
data(ROCR.test_dat)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
pred2 <- prediction(abs(ROCR.simple$predictions + 
                          rnorm(length(ROCR.simple$predictions), 0, 0.1)), 
                    ROCR.simple$labels)
perf <- performance( pred, "tpr", "fpr" )
perf2 <- performance(pred2, "tpr", "fpr")
plot( perf, colorize = TRUE)
plot(perf2, add = TRUE, colorize = TRUE)

#install.packages(pROC)
library(pROC)
?roc
predict12345<-as.numeric(prediction) # convert naive bayes predictions to numeric 
predictABC<-as.numeric(pqrs) # convert Dtree predictions to numeric 
#predictABCDE<-as.numeric(pred_l1) #convert RIDGE predictions to numeric 

roc1= roc(test_dat$target_hotness,svmpr)    #SVM 
roc2= roc(test_dat$target_hotness,predictABC)#Dtree
roc3= roc(test_dat$target_hotness,predict12345) #NB 
#roc4= roc(test_dat$target_hotness,pred_l1)  #RIDGE
roc5= roc(test_dat$target_hotness,pred_LR)  #logistic 

plot(roc1, col = 1, lty = 2, main = "ROC")
plot(roc5, col = 4, lty = 3, add = TRUE)
plot(roc2,col=2,lty=4,add=TRUE) 
plot(roc3,col=3,lty=5,add=TRUE) 
#plot(roc4,col=5,lty=1,add=TRUE)




