library(readxl)
music <- read_excel("/Volumes/AJAY/Grad School/Stats Models and Methods for Business Analytics /FINAL PROJECT /music.xlsx")
View(music)

abc<- table(music$song.hotttnesss)
View(abc)
sum(is.na (music$song.hotttnesss))

install.packages("DataExplorer")
library(DataExplorer)
plot_str(music)
plot_intro(music)
abc<-introduce(music)
View(abc)
abc1<-t(abc)
View(abc1)
plot_missing(music)
plot_histogram(music)
plot_density(music)
plot_correlation(music,type='continuous','song.hotness')
plot_bar(music)
create_report(music,output_file = "report.html",output_dir=getwd(),y=NULL,config=list())
help("create_report")
getwd()
plot_bar(music$mode)
qq_data <- music[, c("song.hotttnesss", "tempo","terms")]
plot_qq(qq_data)
plot_qq(qq_data,by="terms")
na.omit(music)
plot_prcomp(na.omit(music), variance_cap = 0.8)
help("plot_prcomp")
plot_boxplot(qq_data,by="terms")
plot_scatterplot(music)
summary(music)
plot(music$song.hotttnesss)
plot_histogram(music$song.hotttnesss) 
plot_histogram(music)
help(plot_histogram)


#DECISION TREE 

attributes(music)
str(music)
cols <- c("artist.id","artist.name","artist_mbtags","location","mode","release.name","similar","song.id","terms","title")
music[cols] <- lapply(music[cols], factor)
sapply(music, class)

str(music)

plot(music$song.hotttnesss,music$hit)

dim(music)

music$hit<-NA

View(music)

music$hit <- ifelse(music$song.hotttnesss>0.66, 1, 0)

library(rpart)
ctr=rpart.control(maxdepth = 7)
musictree = rpart(hit~artist.hotttnesss+bars_start+bars_confidence+beats_confidence,beats_start+duration+end_of_fade_in+key+familiarity+key_confidence+latitude+longitude+mode+mode_confidence+start_of_fade_out+tatums_confidence+tatums_start+tempo,data=music,method="class",parms = list(split = 'gini'),control=ctr)
print(musictree)
summary(musictree)
class(music)

plot(musictree, uniform=TRUE,  main="Decision Tree")
text(musictree, use.n=TRUE, all=TRUE, cex=.7)

help(rpart)

library(rpart.plot)
rpart.plot::prp(musictree, type=1, extra=1)



