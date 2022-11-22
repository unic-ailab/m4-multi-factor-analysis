library(sqldf)
library(caret)
library(Hmisc)
library(abind)
library(data.table)
library(reshape2)
library(ggplot2)
library(ggridges)
library(xlsx)

#m4 dataset path
m4filesPath <- paste0(Sys.getenv('USERPROFILE'),"\\Downloads\\M4-methods-master\\")
#multi factor analysis path
m4MultifactorPath <- paste0(Sys.getenv('USERPROFILE'),"\\Downloads\\m4-multi-factor-analysis-main\\")

#library(viridis)

setwd(paste0(m4filesPath,'Point Forecasts'))
roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

# set the list of the top-10 methods
top10methods <- c("118","245","237","072","069","036","078","260","238","039")

# get list of files from working directory
top_10 <- TRUE
if (top_10){
  files <- as.character(unlist(c(matrix(top10methods))))
  tmp_files<- NULL
  for (i in 1:length(files)){if(nchar(files[i])==2){
    untar(paste('submission-',files[i],'.rar',sep = ''))
    tmp_files<-rbind(tmp_files,paste('submission-0',files[i],'.csv',sep = ''))}
    else{
      untar(paste('submission-',files[i],'.rar',sep = ''))
      tmp_files<- rbind(tmp_files,paste('submission-',files[i],'.csv',sep = ''))}}
  files <-as.character(tmp_files)
  rm(tmp_files)
} else {
  files <- list.files()
}

# Function to replace all NA values to zero 
zero.replace <- function(x) { replace(x, is.na(x), 0) }

preds<-list()
# Load the candidate Predictions and zero NA values
for (i in 1:length(files)){
  preds[[i]] <- zero.replace(read.csv(files[i]))
}

methods <- sapply(strsplit(sapply(strsplit(files, "-"), "[", 2), ".csv"), "[", 1)

# load method types
setwd(paste0(m4MultifactorPath,'data'))
idToMethod <- (((read.csv('IDtoMethodType.csv'))))
methods.plus <- NULL
for(i in 1:length(methods)){
  strQ <- paste("select * from idToMethod where id=='",methods[i],"'",sep = '') 
  tmpQ <-  sqldf(strQ)
  methods.plus <- rbind(methods.plus,paste(tmpQ[1,c(1)],'-',tmpQ[1,c(2)]))
}
rm(strQ,tmpQ)


### START HERE EVERY NEXT ITERATION ####

# set working directory for True Values
setwd(paste0(m4filesPath,'\\Dataset\\Test'))
# codify the type of predictions
type <- list(Daily='D',Hourly='H',Monthly='M',Quarterly='Q',Weekly='W',Yearly='Y',
             D='Daily',H='Hourly',M='Monthly',Q='Quarterly',W='Weekly',Y='Yearly')

# Select type of predictions to filter e.g. H for hourly
# Daily     : D
# Hourly    : H
# Weekly    : W
# Monthly   : M
# Quarterly : Q
# Yearly    : Y

#### Change filter using one of the above types e.g. Hourly ####
filter <- as.character(type['Yearly'])
#### ------------------------------------------------ ####

# Load ther true values
true_values <-read.csv(paste(type[filter],'-test.csv',sep=''))

# Set the number of series to calculate
n.series <-1:nrow(true_values)

# calculate and create the error percentage lists from preds and true values
errors <- list()
for (i in 1:length(preds)){
  tmp_pred <- data.frame(preds[i])
  tmp <- sqldf(paste('select * from tmp_pred where id like "%',filter,'%"',sep = ''))
  errors[[i]] <-  (cbind(id=true_values[n.series,c(1)],(true_values[n.series,c(2:ncol(true_values))] - tmp[n.series,c(2:ncol(true_values))])/true_values[n.series,c(2:ncol(true_values))] ))
}
rm(tmp,tmp_pred)# preds

# create the error series list per method
series <- list()
for (i in 1:length(errors)){
  # get current method error list
  tmp <- (errors[[i]])
  # remove first row
  tmp <- tmp[-1]
  # create a list to store the series in a row
  inner.series <- list()
  for (j in 1:nrow(tmp)){
    inner.series[[j]] <- tmp[j,c(1:ncol(tmp))]
  }
  series[[i]] <- inner.series
}
rm(tmp)

# create the compare series list
compare.series <- list()
for (i in n.series){
  compare.series[[i]] <- (sapply(series,'[[',i))}

err_sums<-NULL
for (j in n.series){
  # get current j series
  ts <- matrix( unlist((compare.series[[j]])),ncol=length(preds),nrow=ncol(true_values)-1)
  err_sums<-rbind(err_sums, abs((colSums(ts))))
  cat("error percentage sum", j,' from ', nrow(true_values), "\r")
}

# for every merhod create a list of top 10 best/worst
best_series <- NULL
worst_series <- NULL
num_series <- 20
for (j in 1:ncol(err_sums)) {
  # get current method error's sums
  tmp_method_errors <- matrix(err_sums[,c(j)])
  # convert to data table to get row names
  tmp_method_errors<-as.data.table(data.frame(tmp_method_errors), keep.rownames = TRUE)
  colnames(tmp_method_errors)<-c('rn','errs')
  tmp_method_errors <- tmp_method_errors[with(tmp_method_errors, order(errs)),c('rn')]
  if(j==1){
    #best_series<-head(tmp_method_errors,n=num_series)
    best_series<- apply(head(tmp_method_errors,n=num_series), 2, rev)
    worst_series<- apply(tail(tmp_method_errors,n=num_series), 2, rev)
  }else {
    #best_series <- cbind(best_series, head(tmp_method_errors,n=num_series))
    best_series <-  cbind(best_series, apply(head(tmp_method_errors,n=num_series), 2, rev))
    worst_series <- cbind(worst_series, apply(tail(tmp_method_errors,n=num_series), 2, rev))
  }}

authors<-c('Smyl','Manso','Pawlikowski','Jaganathan','Fiorucci','Petropoulos','Shaub','Legaki','Doornik','Pedregal')
methods.rank <- c(1:10)
methods.plus.author<-NULL
for (k in 1:length(methods)){
  methods.plus.author<-rbind(methods.plus.author,paste(methods.rank[k],'-',authors[k],sep=''))
}

colnames(best_series) <- methods.plus.author
colnames(worst_series) <- methods.plus.author

#write.xlsx(worst_series[1:5,],'worst5_yearly.xlsx')

# here start the rest of the code to extract worst lengths
setwd(paste0(m4filesPath,'\\Dataset\\Train'))

train_values <- read.csv(paste(type[filter],'-train.csv',sep=''))

# get all the train series lengths
series_length <- NULL
for (i in 2:nrow(train_values)){
  current_series <- (train_values[i,])
  series_length<-rbind(series_length,ncol(current_series[colSums(!is.na(current_series)) > 0]))
  cat("record ", i,' from ', nrow(train_values), "\r")}

# calculate the train series lengths
worst_series_length <- NULL
for (i in 1:ncol(worst_series)){
  tmp_series_length <-NULL
  for (j in 1:nrow(worst_series)){
    tmp_series_length <- rbind(tmp_series_length,series_length[as.numeric(worst_series[j,c(i)]),])
  }
  worst_series_length<-cbind(worst_series_length,tmp_series_length)
}

colnames(worst_series_length) <- methods.plus.author
# # plot the histograms of worst series
# x.labs <-  seq(0,round(nrow(worst_series_length),0),4)
# y.labs <- seq(0,roundUpNice(max(worst_series_length, na.rm=T)), 10)
# par(mar=c(2, 1.5, 1, 1), mfrow=c(2,5), oma = c(2, 1.5, 2, 0.1))
# for (i in 1:ncol(worst_series_length)){
#   hist(worst_series_length[,c(i)], ylim = c(0,20), xlab=colnames(worst_series_length)[i] ,main=NULL)
#   title(main=colnames(worst_series_length)[i],cex.main=.85,lwd=1)
#   if(i==1 || i==6){mtext("Frequency", side=2, line=2, adj=0.5,cex=0.7)}
#   if(i==10){mtext("Series Lengths", side=1, outer=T, adj=0.5,cex=0.7)}
# }

#worst series plots
dfm <- melt(worst_series_length,id.vars = 1)

cols <- c('green','deepskyblue3','firebrick','gold4','darkorchid','grey70','lightblue4','mediumblue','gray41','tan2')
theme_set(theme_gray(base_size = 10))

p1 <- ggplot(dfm, aes(x = value, y = Var2, group = Var2,color=Var2, height = ..density..)) + 
geom_density_ridges(stat = "binline", bins = 20, scale = 0.95, draw_baseline = TRUE)+
theme(axis.text.x=element_text(colour="grey20", size=10),axis.text.y = element_text(colour="grey20",size=10))+
theme(legend.key=element_rect(colour="white"),legend.title=element_text(size=rel(0.8), face="bold", hjust=0),
     panel.background=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=.5),
     strip.background=element_rect(fill="grey90", colour="grey50")) +
xlab("Series Lengths") +
ylab("") +
guides(fill=guide_legend(title=NULL)) +theme(legend.position = "none") +
scale_color_manual(values=cols) 

p1 + scale_x_continuous(limits = c(0, 200))#+ theme(axis.text.y=element_blank())+ylab("") + scale_y_discrete(breaks=NULL)


### calculate the train best series lengths 
best_series_length <- NULL
for (i in 1:ncol(best_series)){
  tmp_series_length <-NULL
  for (j in 1:nrow(best_series)){
    tmp_series_length <- rbind(tmp_series_length,series_length[as.numeric(best_series[j,c(i)]),])
  }
  best_series_length<-cbind(best_series_length,tmp_series_length)
}

 colnames(best_series_length) <- methods.plus.author
# # plot the histograms of worst series
# x.labs <-  seq(0,round(nrow(best_series_length),0),4)
# y.labs <- seq(0,roundUpNice(max(best_series_length, na.rm=T)), 10)
# par(mar=c(2, 1.5, 1, 1), mfrow=c(2,5), oma = c(2, 1.5, 2, 0.1))
# for (i in 1:ncol(best_series_length)){
#   hist(best_series_length[,c(i)], ylim = c(0,20), xlab=colnames(best_series_length)[i] ,main=NULL)
#   title(main=colnames(best_series_length)[i],cex.main=.85,lwd=1)
#   if(i==1 || i==6){mtext("Frequency", side=2, line=2, adj=0.5,cex=0.7)}
#   if(i==10){mtext("Series Lengths", side=1, outer=T, adj=0.5,cex=0.7)}
# }

#best series plots
dfm <- melt(best_series_length,id.vars = 1)

cols <- c('green','deepskyblue3','firebrick','gold4','darkorchid','grey70','lightblue4','mediumblue','gray41','tan2')
theme_set(theme_gray(base_size = 10))

p2 <- ggplot(dfm, aes(x = value, y = Var2, group = Var2,color=Var2, height = ..density..)) + 
  geom_density_ridges(stat = "binline", bins = 20, scale = 0.95, draw_baseline = TRUE)+
  theme(axis.text.x=element_text(colour="grey20", size=10),axis.text.y = element_text(colour="grey20",size=10))+
  theme(legend.key=element_rect(colour="white"),legend.title=element_text(size=rel(0.8), face="bold", hjust=0),
        panel.background=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=.5),
        strip.background=element_rect(fill="grey90", colour="grey50")) +
  xlab("Series Lengths") +
  ylab("") +
  guides(fill=guide_legend(title=NULL)) +theme(legend.position = "none") +
  scale_color_manual(values=cols) 

p2 + scale_x_continuous(limits = c(0,  200))#+ theme(axis.text.y=element_blank())+ylab("") + scale_y_discrete(breaks=NULL)


#plot series lengths
dfm <- melt(series_length,id.vars = 1)
ggplot(as.data.frame(series_length), aes(x = V1)) +
geom_histogram(stat='bin',binwidth=10,color='grey50',fill='grey90') +
theme(axis.text.x=element_text(colour="grey20", size=10),axis.text.y = element_text(colour="grey20",size=10))+
theme(legend.key=element_rect(colour="white"),legend.title=element_text(size=rel(0.8), face="bold", hjust=0),
      panel.background=element_blank(), panel.border=element_rect(colour = "black", fill=NA, size=.15),
      strip.background=element_rect(fill="grey90", colour="grey50")) +
xlab("Series Lengths") +
ylab("Frequency") +
guides(fill=guide_legend(title=NULL)) +theme(legend.position = "none") +
scale_color_manual(values=cols) 
 
 


