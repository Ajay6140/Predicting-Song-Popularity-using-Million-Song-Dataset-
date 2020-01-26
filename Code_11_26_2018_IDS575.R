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
final_data<-interim_data
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

final_data_for_model <-data_5[, -which(names(data_5) %in%   c("decade1","decade","yr1","year"))]
dim(final_data_for_model)

write.csv(final_data_for_model,"interim_data2.csv")
getwd()
#final_data_for_model <-final_data_for_model[, -which(names(final_data_for_model) %in% c("X"))]
#View(final_data_for_model)

#We have our final dataset but we now convert the "terms" variable to document 
#Oh shit forgot about tempo-have to IMPUTE VALUES for tempo

final_data_for_model$tempo<-ifelse(final_data_for_model$tempo==0,NA,
                                      final_data_for_model$tempo)
summary(final_data_for_model)
library(mice)
?mice
#tempData <- mice(final_data_for_model,where = is.na(final_data_for_model),m=1,meth='pmm',seed=500)

for(i in 1:ncol(final_data_for_model)){
  final_data_for_model[is.na(final_data_for_model[,i]),i] <- mean(final_data_for_model[,i], na.rm = TRUE)
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

View(final_data_for_modelling)
final_data_for_modelling$location<-as.factor(final_data_for_modelling$location)
str(final_data_for_modelling)
dim(final_data_for_modelling)
#######################################Modeling#########################################

final_data_for_modelling$target_hotness<-ifelse(final_data_for_modelling$target_h>0.5,"1","0")

table(final_data_for_modelling$target_hotness)
#Final-data=Million song dataset - msd
msd<-final_data_for_modelling[, -which(names(final_data_for_modelling) %in% 
                                                              c("target_h","location","artist.name"))]
msd$decade2<-as.factor(msd$decade2)

#75% train , 25% test
nrow=nrow(msd)
set.seed(123) 
index=sample(1:nrow,size=(0.75*nrow))
#index
train_data=msd[index,]
test_dat=msd[-index,]
class(train_data$target_hotness)
dim(test_data)
summary(msd)
msd$target_hotness<-as.factor(msd$target_hotness)
class(train_data$target_hotness)
train_data=msd[index,]
test_dat=msd[-index,]
all_vars<-colnames(train_data)
write.csv(all_vars,"vars.csv")
table(test_dat$target_hotness)

dim(train_data)
View(train_data)

#Random forest for feature selection for KNN

install.packages("randomForest")

library(randomForest)
?randomForest
dim(msd)
fit <- randomForest(target_hotness~.,data=train_data,ntree= 200, importance=TRUE)

importance(rfModel)
varImpPlot(rfModel)
print(fit) # view results 
importance(fit) # importance of each predictor
table(msd$decade2)

input <- c("loudness", "tempo", "time_signature","decade2", "mode", "duration","artist.hotness")
target  <- c("target_hotness")

c<-train_data[, c(input,target)]
install.packages("class")
library(class)
prc_test_pred <- knn(train = train_data[, c(input,target)],
                     test = test_dat[, c(input,target)],cl = target_hotness, k=4)

#Decision trees

library(rpart)
dtfit <- rpart(target_hotness ~ .,
               data=train_data,
               method="class",control=rpart.control(minisplit=20,minibucket=20,cp=0.008,maxdepth = 30))

plot(dtfit, uniform=TRUE,  main="Decision Tree")
text(dtfit, use.n=TRUE, all=TRUE, cex=.7)

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



























##############Random forest###############

rfModel = randomForest(factor(target_hotness) ~ ., data=train_data[, c(input,target)], ntree=400, importance=TRUE )

cm <- table(pred=predict(rfModel,test_dat, type="class"), true=test_dat$target_hotness)
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
svmfit <- svm(target_hotness ~ ., data=newTST)
help(svm)
print(svmfit)
summary(svmfit)

svmpr <- predict(svmfit, newdata=na.omit(newTST))

n = sum(svmpr) 
diag = diag(svmpr)  
rowsums = apply(svmpr, 2, sum) 
colsums = apply(svmpr, 1, sum) 
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

View(newTST)
dim(newTST)










