rm(list=ls(all=TRUE))
#library("utf8")
#setwd("D:/Рабочий стол/Учеба/Methods and models of applied research/Codes part 4")
setwd("D:/Dropbox/FinalProject/TM")
Sys.setlocale("LC_CTYPE", "russian")
#Sys.getlocale()
library(readxl)
library(psych)
library(data.table)
library(dplyr)
library(ngram)
word_data <- read_excel('data_en_mod.xlsx')

head(word_data)
set.seed(1001)
text<-word_data$text


head(text)


sapply(c("magrittr", "data.table","ggplot2",
         'stm','ngram','corrplot',
         'readxl','tm'), require, character.only = T)



#word_data$text<-as.character(text$description)
#text$txt_clean<-as.character(text$txt_clean)

word_data$text<-as.character(word_data$text)
word_data$length_text <- matrix(0, length(word_data$id), 1)

#count number of words in each string
for (i in 1:length(word_data$text)){word_data$length_text[i]<-wordcount(word_data$text[i])}


par(mfrow = c(1, 1)) 

hist(word_data$length_text,breaks=1000, main="Length of text before cleaning",xlab="# of words")

hist(word_data$length_text, xlim = c(0,2510), breaks = 10000)
hist(word_data$length_text, xlim = c(2500,31000), ylim = c(0,50), breaks = 10000)

summary(word_data$length_text)

hist(word_data$year, xlab="year", xlim = c(1999, 2020), 
     breaks = seq(1999,2020,1), labels = FALSE)

axis(side = 1, 
     at = seq(min(word_data$year), max(word_data$year), 1),
     tck = -0.02,
     labels = FALSE)

axis(side = 1, 
     at = seq(min(word_data$year), max(word_data$year), 1),
     tck = 0,
     labels = c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
                2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))

hist(word_data$month, xlab="month", xlim = c(0,12), breaks = seq(-1,12,1))

Length_by_year<-matrix(0,1,length(years))
year_idd<-1
for (year_id in years){
  Length_by_year[,year_idd]<-sum(word_data$length_text[word_data$year==year_id])
  year_idd<-year_idd+1
}
barplot(Length_by_year, main="Length of responses by year",xlab="year", names.arg = years)



library(tictoc)
library(textstem)

justloaddata <- 1
if(justloaddata==0){
  
  
  tic("total time to lemmatize 24338 obs:")
  text_lemm <- lemmatize_strings(text)
  toc()
  #tic("total time to stem 24338 obs:")
  #text_stem <- stem_strings(text)
  #toc()
  
  save(text_lemm, file = "lemm.RData")
  #save(text_stem, file = "stem.RData")
}
toc()
load("lemm.RData")
#load("stem.RData")


text[[3]]
print(text_lemm[[3]]) #check the outcome of lemmatizer
#print(text_stem[[3]]) #check the outcome of stemming

#lemmatizer looks better
text<-text_lemm

# pre-processing:
tic("total time to pre-processing 22927 obs:")

text <- gsub("'", "", text)             # remove apostrophes
text <- gsub("[[:punct:]]", " ", text)  # replace punctuation with space
text <- gsub("[[:cntrl:]]", " ", text)  # replace control characters (tab, escape etc) with space
text <- gsub("^[[:space:]]+", "", text) # remove whitespace at beginning of documents
text <- gsub("[[:space:]]+$", "", text) # remove whitespace at end of documents
text <- gsub("[0-9]", "", text)         #remove numbers
text <- gsub("[^A-Za-z ]","", text)     #keep only english letters basically (avoid a and alike) 
text <- tolower(text)  # force to lowercase

toc()

word_data$text_clean <- text

word_data$text_clean[[1]]

library(tm)
library(stopwords)
stop_words <- stopwords(source = "smart")
stops <- read.csv(file = "custom_stop_words.csv", sep = ";", header = F)
stops <- as.character(stops$V1)
mystopwords<-c(stop_words,stops)

#Idk why, but metatext = text doesn't work (Error in `[.text.table`(metatext, , i) : 
  #j (the 2nd argument inside [...]) is a single symbol but column name 'i' is not found. 
  #Perhaps you intended DT[, ..i]. This difference to text.frame is deliberate and explained in FAQ 1.1.), so

#meta <- word_data

tic("Total time:")
library(stm)
processed <- textProcessor(word_data$text_clean,
                           metadata = word_data, lowercase = FALSE,
                           removestopwords = TRUE, removenumbers = TRUE,
                           removepunctuation = TRUE, ucp = FALSE, stem = FALSE,
                           wordLengths = c(3, Inf), #remove single standing letters like s brilev 
                           sparselevel = 1, language = "en",
                           customstopwords=mystopwords,#add additional stopwords if you like 
                           verbose = TRUE)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh=4,verbose = TRUE)
toc()
#Removing 26097 of 39838 terms (41270 of 2229350 tokens) due to frequency 
#Your corpus now has 24338 documents, 13741 terms and 2188080 tokens.> toc()
#Total time:: 189.42 sec elapsed

length(out$meta$id)
length(word_data$id)

#print which texts were deleted 
junk<-word_data$text[-which(word_data$id %in% out$meta$id)]
print(junk)
#OK!

docs <- out$documents
vocab <- out$vocab
meta <-out$meta
junk<-out$wordcounts[out$wordcounts>4]
freq_vocab<-out$wordcounts[out$wordcounts>4]#table(unlist(out$documents))
vocab_app<-vocab[order(freq_vocab,decreasing = TRUE)]
list_frequent_words<-data.frame(vocab_app,sort(freq_vocab,decreasing = TRUE))
print(list_frequent_words[1:20,])

tic("computing number of topics tooks:")
justloaddata <- 1
if(justloaddata==0){
  
  storage<-searchK(out$documents, out$vocab,
                   prevalence =~ meta$year,
                   K = c(3:40), M=30,#number of keywords per topic to calculate exclusivity
                   N = floor(0.2 *length(out$documents)),#increase the held-out log likelihood
                   heldout.seed =1,init.type="Spectral")
  
  save(storage, file = "putin.RData")
}
load("putin.RData")
toc()

Kchoice<-21

png('searchK.png',width = 12, height = 8, pointsize = 17, units = 'in', res = 1000)

par(mfrow = c(1, 3) ,mar=c(4,4,1,2)) 
plot(storage$results$K,storage$results$heldout,ylab="Heldout log-likelihood",xlab="",xaxt="n",col=ifelse(storage$results$K %in% Kchoice, 'red', 'blue'))
axis(1, at=1:length(storage$results$K), labels=storage$results$K)
plot(storage$results$K,storage$results$exclus,ylab="Exclusivity",xlab="Number of Topics",xaxt="n",col=ifelse(storage$results$K %in% Kchoice, 'red', 'blue'))
axis(1, at=1:length(storage$results$K), labels=storage$results$K)
plot(storage$results$K,storage$results$semcoh,ylab="Semantic coherence",xlab="",xaxt="n",col=ifelse(storage$results$K %in% Kchoice, 'red', 'blue'))
axis(1, at=1:length(storage$results$K), labels=storage$results$K)

dev.off()

justloaddata <- 1
if(justloaddata==0){
  
  ncpSelect2 <- selectModel(out$documents,
                            out$vocab,
                            K=Kchoice,
                            prevalence =~ meta$year + meta$term,
                            #+meta$Gender+meta$PoliticalOrientation
                            max.em.its=1000,
                            #data=out$meta,
                            runs=20,       #20
                            init.type="Spectral",
                            seed=1,
                            verbose=TRUE) #, emtol=1) Defaults to .001%.
  save(list = ls(all = TRUE), file = "stm_Putin.Rdata")
}
load("stm_Putin.Rdata")


#fill the lable here (in ENGLISH PLEASE)
topiclabel<-as.character(matrix(0,Kchoice,1))
topiclabel[1]<-"T1: International cooperation"
topiclabel[2]<-"T2: Culture and Religion"
topiclabel[3]<-"T3: War and Memory"
topiclabel[4]<-"T4: Global Safety"
topiclabel[5]<-"T5: Sports"
topiclabel[6]<-"T6: International Forum"
topiclabel[7]<-"T7: Law and Elections"
topiclabel[8]<-"T8: Energy resources and production"
topiclabel[9]<-"T9: Business growth"
topiclabel[10]<-"T10: Сooperation"
topiclabel[11]<-"T11: Сongratulations"
topiclabel[12]<-"T12: Problems and solutions"
topiclabel[13]<-"T13: Defense and Armaments"
topiclabel[14]<-"T14: CIS countries"
topiclabel[15]<-"T15: The Crime Problem"
topiclabel[16]<-"T16: Executive"
topiclabel[17]<-"T17: Discuss"
topiclabel[18]<-"T18: Condolences"
topiclabel[19]<-"T19: Education and Science"
topiclabel[20]<-"T20: Regional development"
topiclabel[21]<-"T21: Сooperation and trade"

par(mfrow = c(1, 1) ,mar=c(2,2,2,2)) 
ncpPrevFit_Putin <- ncpSelect2$runout[[1]]


#plot proportions of topics in the corpus
png('proportions of topics.png',width = 18, height = 16, pointsize = 17, units = 'in', res = 1000)
plot(ncpPrevFit_Putin, type="summary", main="", n = 5)
colSums(ncpPrevFit_Putin$theta)/sum(ncpPrevFit_Putin$theta)
dev.off()

#plot their FREX words
png('FREX.png',width = 20, height = 20, pointsize = 20, units = 'in', res = 800)
par(mfrow = c(1, 1))
plot(ncpPrevFit_Putin, type="labels", labeltype="frex",text.cex = 1, n=10,  width = 150)
dev.off()

#plot the prevalence of topics over documents
png('prevalence of topics.png',width = 20, height = 20, pointsize = 17, units = 'in', res = 1000)
par(mfrow = c(1, 1) ,mar=c(1,1,1,1)) 
plot(ncpPrevFit_Putin, type="hist", )
dev.off()

#plot frequent and exclusive words of the documents
#frexweight is analog to lambda in LDAvis
labelTopics(ncpPrevFit_Putin, topics=NULL, n=10, frexweight = 0.5)



#print 3 illusrative responses for each topic
nk<-3
for (ci in 1:Kchoice){
  cii<-ci+1
  thoughts1<-findThoughts(ncpPrevFit_Putin, texts=meta$text, n=nk, topics=ci)$docs[[1]]
  topic_proportions<-as.matrix(make.dt(ncpPrevFit_Putin))[findThoughts(ncpPrevFit_Putin, texts=meta$text, n=nk,topics=ci)$index[[1]],cii]
  
  message("3 representative responses with highest prevalence together with the values of those prevalences and words being   analyzed (i.e. excluding rare words dropped out from analysis) of Topic :")
  print(ci)
  print(thoughts1[1:nk])
  print(topic_proportions)
}

#plotting those quotes
plotQuote(thoughts1[1], width = 70, text.cex = 1, main = "Topic 1")
plotQuote(thoughts1[1:nk], width = 160, text.cex = 1)


#following https://github.com/bstewart/stm/blob/master/R/labelTopics.R

logbeta <- ncpPrevFit_Putin$beta$logbeta[[1]]
wordcounts <- ncpPrevFit_Putin$dim$wcounts$x
frexlabels <-calcfrex(logbeta, 0, wordcounts)
problabels<-apply(logbeta, 1, order, decreasing=TRUE)

nb.cols <- length(out$vocab)
threshold<-3000
mycolors <- c(rep("#FFFFE5",length(out$vocab)-threshold),colorRampPalette(rev(heat.colors(100)))(threshold)) #heat.colors(10) rev(heat.colors(50))

pal<-colorRampPalette(c("blue", "red"))
mycolorsRB<-c(rep("darkblue",length(out$vocab)-threshold),pal(threshold))

vec<-matrix(0,length(out$vocab),Kchoice)
for (j in 1:Kchoice){
  jj<-length(out$vocab)
  for (i in 1:length(out$vocab)){
    vec[frexlabels[i,j],j]<-jj #the higher the value, the more exclusive the word is
    jj<-jj-1
  }
}

#add_vector<-colSums(ncpPrevFit_Putin$theta)/sum(ncpPrevFit_Putin$theta)*2#c(0,0,0,1)
#equal_vector<-matrix(mean(add_vector)*1.3,Kchoice,1)#c(0,0,0,1)
png('wordclouds_black.png',width = 20, height = 20, pointsize = 17, units = 'in', res = 1000)
par(mfrow = c(5, 5) ,mar=c(1,2,1,1)) 
#vector to normalize fonts between wordclouds
set.seed(1)
for (ci in 1:Kchoice){
  cloud(ncpPrevFit_Putin, topic = ci, max.words = 30,scale=c(3,.1),rot.per=0,random.order=FALSE)#,colors=fiftyGreys[frexlabels[,1]])
  title(topiclabel[ci])
}
dev.off()

png('wordclouds_redyellow.png',width = 20, height = 20, pointsize = 17, units = 'in', res = 1000)
par(mfrow = c(5, 5) ,mar=c(1,2,1,1)) 
set.seed(1)
for (ci in 1:Kchoice){
  cloud(ncpPrevFit_Putin, topic = ci, max.words = 30,scale=c(3,.1),colors=mycolors[vec[,ci]],random.order=FALSE, random.color=FALSE, ordered.colors=TRUE,rot.per=0)
  title(topiclabel[ci])
  box("figure", col="black", lwd = 5)
}
dev.off()


png('wordclouds_redblue.png', width = 20, height = 20, pointsize = 17, units = 'in', res = 1000)
par(mfrow = c(5, 5) ,mar=c(1,2,1,1)) 
set.seed(1)
for (ci in 1:Kchoice){
  cloud(ncpPrevFit_Putin, topic = ci, max.words = 30,scale=c(3,.1),colors=mycolorsRB[vec[,ci]],random.order=FALSE, random.color=FALSE, ordered.colors=TRUE,rot.per=0)
  title(topiclabel[ci])
  box("figure", col="black", lwd = 5)
}
dev.off()




library(RColorBrewer)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#col_vector =grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
set.seed(100)
col=sample(col_vector, Kchoice)#12 and not Kchoice to have more distinct colors

# Figure 1: Topical prevalence over co-variates
png('ols.png', width = 20, height = 20, pointsize = 17, units = 'in', res = 1000)
prep <- estimateEffect(1:Kchoice ~ year,#+meta$term, #+meta$CT.Acceptability, 
                       ncpPrevFit_Putin,
                       meta=meta, 
                       uncertainty = "Global")
par(mfrow = c(5, 5) ,mar=c(1,2,1,1)) 

for(k_i in 1:Kchoice){
plot.estimateEffect(prep, 
                    #main="year",
                    covariate = "year", 
                    topics = c(k_i),
                    model=ncpPrevFit_Putin, 
                    method="continuous",
                    labeltype="custom",
                    custom.labels=topiclabel[k_i],
                    ylim=c(0,.3), 
                    linecol=col[k_i],
                    ylab="Expected topic prevalence",ci.level=0.95
)
}
dev.off()
summary(prep)


#---------------------------------------------------------------
years <- c(1999:2020)

# Figure 1: Topical prevalence over co-variates
topic_year_share<-matrix(0,length(years),Kchoice)
year_idd<-1
for (year_id in years){
  topic_year_share[year_idd,]<-colSums(ncpPrevFit_Putin$theta[meta$year==year_id,])/sum(ncpPrevFit_Putin$theta[meta$year==year_id,])
  year_idd<-year_idd+1
}
#rowSums(topic_year_share)
topic_year_share<-as.data.frame(topic_year_share)
rownames(topic_year_share)<-years#[-c(4,12)]
colnames(topic_year_share)<-topiclabel

par(mfrow = c(1, 1)) 

png('topic_year_share.png', width = 20, height = 20, pointsize = 17, units = 'in', res = 1000)

matplot(rownames(topic_year_share), topic_year_share[,1:Kchoice], type='l', lty=1,lwd=2,xlab='Years', ylab='Share', col=col[1:Kchoice],xaxt='n',ylim=c(0,0.3))
axis(side=1, at=c(1999:2020), labels=years)
legend('topright', inset= c(0.2, 0.01), 
       legend=colnames(topic_year_share[,1:Kchoice]),
       pch=2, col=col[1:Kchoice], 
       horiz = FALSE, cex = 1,  xpd = TRUE, text.width = 2, box.lty = 0 )

dev.off()

png('topics_by_years.png', width = 20, height = 20, pointsize = 17, units = 'in', res = 1000)
par(mfrow = c(5, 5) ,mar=c(1,2,1,1)) 
for(k_i in 1:Kchoice){
  matplot(rownames(topic_year_share), topic_year_share[,k_i], type='l', lty=1,lwd=2, xlab='Years', ylab='Share', col=col[k_i],xaxt='n',ylim=c(0,0.3))
  axis(side=1, at=c(1999:2020), labels=years)
  legend('topleft', inset=0, legend=colnames(topic_year_share)[k_i],pch=3, col=col[k_i])
}
dev.off()


dt.proportions_news1<- make.dt(ncpPrevFit_Putin)
dt.proportions_news1<-as.matrix(dt.proportions_news1[,-1])

TopicCorrMatrix1<-matrix(0,Kchoice,Kchoice)


for (i in 1:Kchoice){
  for (j in 1:Kchoice){
    TopicCorrMatrix1[i,j]<-cor(dt.proportions_news1[,i],dt.proportions_news1[,j])
  }
}


library(corrplot)
rownames(TopicCorrMatrix1)<-topiclabel

#colnames(TopicCorrMatrix1)<-c("T1", "T2", "T3", "T4","T5","T6","T7","T8","T9","T10","T11","T12","T13")
colnames(TopicCorrMatrix1)<-c("T1", "T2", "T3", "T4","T5","T6","T7","T8","T9","T10",
                              "T11","T12","T13", "T15", "T15", "T16", "T17","T18","T19","T20","T21")


par(mfrow = c(1, 1) ,mar=c(1,2,1,1)) 

TopicCorrMatrix1<-TopicCorrMatrix1-diag(Kchoice)
corrplot(TopicCorrMatrix1, is.corr = FALSE, order="hclust", method = "square",tl.cex=.5,tl.col = "black",cl.lim = c(-1, 1),
         col=colorRampPalette(c("blue","white","red"))(200))

png('CorrelationMatrix1_.png',width = 20, height = 20, units = 'in', res = 1000)
corrplot(TopicCorrMatrix1, is.corr = FALSE, order="hclust", method = "square",tl.cex=1.2,tl.col = "black",cl.lim = c(-1, 1),col=colorRampPalette(c("blue","white","red"))(200))
dev.off()

#a less convenient feature to show the same thing is in STM
mod.out.corr <- topicCorr(ncpPrevFit_Putin)
plot.topicCorr(mod.out.corr)


