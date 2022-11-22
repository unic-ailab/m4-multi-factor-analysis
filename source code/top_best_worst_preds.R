library(sqldf)
library(caret)
library(Hmisc)
library(abind)
library(data.table)
library(xlsx)

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

# reset par settings
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
cf_DFinf2NA <- function(x)
{
  for (i in 1:ncol(x)){
    x[,i][is.infinite(x[,i])] = 0
  }
  return(x)
}

# Function to replace all NA values to zero 
zero.replace <- function(x) { replace(x, is.na(x), 0) }

#m4 dataset path
m4filesPath <- paste0(Sys.getenv('USERPROFILE'),"\\Downloads\\M4-methods-master\\")
#multi factor analysis path
m4MultifactorPath <- paste0(Sys.getenv('USERPROFILE'),"\\Downloads\\m4-multi-factor-analysis-master\\")

# set list of top-10 methods
top10methods <- c("118","245","237","072","069","036","078","260","238","039")

#set working directory to m4 competition folder
setwd(paste0(m4filesPath,'Point Forecasts'))

#get the list of files from m4 directory 
#(TRUE=TOP_10),(FALSE=ALL METHODS)
top_10 <- TRUE
if (top_10){
  files <- as.character(unlist(c(matrix(top10methods))))
  tmp_files<- NULL
  for (i in 1:length(files)){if(nchar(files[i])==2){
    untar(paste('submission-',files[i],'.rar',sep = ''))
    tmp_files<-rbind(tmp_files,paste('submission-',files[i],'.csv',sep = ''))}
    else{tmp_files<- rbind(tmp_files,paste('submission-',files[i],'.csv',sep = ''))}}
  files <-as.character(tmp_files)
  rm(tmp_files)
} else {
  files <- list.files()
}

#store predictions in a list
preds<-list()

#load the candidate predictions replace with zero NA values
for (i in 1:length(files)){
  preds[[i]] <- zero.replace(read.csv(files[i]))
}

#store method's id
methods <- sapply(strsplit(sapply(strsplit(files, "-"), "[", 2), ".csv"), "[", 1)

#change working directory
setwd(paste0(m4MultifactorPath,'data'))

#load file that connects id and method type
idToMethod <- (((read.csv('IDtoMethodType.csv'))))

#concatenate method id and method type
methods.plus <- NULL
for(i in 1:length(methods)){
  strQ <- paste("select * from idToMethod where id=='",methods[i],"'",sep = '') 
  tmpQ <-  sqldf(strQ)
  methods.plus <- rbind(methods.plus,paste(tmpQ[1,c(1)],'-',tmpQ[1,c(2)]))
}
rm(strQ,tmpQ)

##### FOR EVERY NEXT TYPE OF PREDICTION --- RUN FROM THIS POINT --- #####

# set working directory for True Values
setwd(paste0(m4filesPath,'\\Dataset\\Test'))

#codify the type of predictions
type <- list(Daily='D',Hourly='H',Monthly='M',Quarterly='Q',Weekly='W',Yearly='Y',D='Daily',H='Hourly',M='Monthly',Q='Quarterly',W='Weekly',Y='Yearly')

#enumerate the type of predictions
# Hourly    : H
# Daily     : D
# Weekly    : W
# Monthly   : M
# Quarterly : Q
# Yearly    : Y

#### Change filter using one of the above types e.g. Hourly ####
filter <- as.character(type['Quarterly'])
#### ------------------------------------------------ ####

#load the true values
true_values <- read.csv(paste(type[filter],'-test.csv',sep=''))

#calculate the number of series exist in a prediction type
n.series <- 1:nrow(true_values)

#calculate and create the error percentage lists
errors <- list()
for (i in 1:length(preds)){
  tmp_pred <- data.frame(preds[i])
  tmp <- sqldf(paste('select * from tmp_pred where id like "%',filter,'%"',sep = ''))
  errors[[i]] <-  (cbind(id=true_values[n.series,c(1)], (true_values[n.series,c(2:ncol(true_values))] - tmp[n.series,c(2:ncol(true_values))])/true_values[n.series,c(2:ncol(true_values))] ))
}
rm(tmp,tmp_pred)

#create the error percentage series list per method
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

#create the compare series list ()
compare.series <- list()
for (i in n.series){
  compare.series[[i]] <- (sapply(series,'[[',i))}

#*calculate the cumulative error percentage sum 
#*for every series and method
#*
err_sums <- NULL
for (j in n.series){
  # get current j series, (ts table stores the error per series and method)
  ts <- matrix( unlist((compare.series[[j]])),ncol=length(preds),nrow=ncol(true_values)-1)
  #stores the cumulative error per series and method 
  err_sums<-rbind(err_sums, abs((colSums(ts))))
  cat("error percentage sum", j,' from ', nrow(true_values), "\r")
}

# for every method create a list of top 4 best/worst series
best_series <- NULL; worst_series <- NULL
#set the number of series to include in the best/worst series list
num_series <- 4

for (j in 1:ncol(err_sums)) {
  #get current method's error sum
  tmp_method_errors <- matrix(err_sums[,c(j)])
  # convert to data table to get row names
  tmp_method_errors<-as.data.table(data.frame(tmp_method_errors), keep.rownames = TRUE)
  colnames(tmp_method_errors)<-c('rn','errs')
  tmp_method_errors <- tmp_method_errors[with(tmp_method_errors, order(errs)),c('rn')]
  if(j==1) {
    best_series <- head(tmp_method_errors,n=num_series)
    worst_series <- apply(tail(tmp_method_errors,n=num_series), 2, rev)}
  else {
    best_series <- cbind(best_series, head(tmp_method_errors,n=num_series))
    worst_series <- cbind(worst_series, apply(tail(tmp_method_errors,n=num_series), 2, rev))}
  }
rm(tmp_method_errors)

if(top_10==TRUE){
authors<-c('Smyl','Manso','Pawlikowski','Jaganathan','Fiorucci','Petropoulos','Shaub','Zampeta','Doornik','Pedregal')
methods.rank <- c(1:10)
methods.plus.author<-NULL
for (k in 1:length(methods)){
  methods.plus.author<-rbind(methods.plus.author,paste(methods.rank[k],'-',authors[k],sep=''))
}

colnames(best_series) <- methods.plus.author
colnames(worst_series) <- methods.plus.author
}

#store worst series in an excel file - if necessary
# setwd(m4MultifactorPath)
# write.xlsx(worst_series, paste('worst_series_',type[filter],'.xlsx'), sheetName = "Sheet1",col.names = TRUE, row.names = FALSE, append = FALSE)

err_sums<-NULL
#calculate cumulative error sums per series (all methods in a series)
 for (j in n.series){
#get current j series
 ts <- matrix( unlist((compare.series[[j]])),ncol=length(preds),nrow=ncol(true_values)-1)
 err_sums<-rbind(err_sums, abs(sum(colSums(ts))))
 cat("error percentage sum", j,' from ', nrow(true_values), "\r")
 }

#convert to data table to get row names
err_sums<-as.data.table(data.frame(err_sums), keep.rownames = TRUE)
colnames(err_sums)<-c('rn','errs')
#sort err_sums by error and get the indexes
index<-(err_sums[with(err_sums, order(errs)),c('rn')])

#collect the first 10 as best 
best_10_series <- head(index,n=10)
#collect the last 10 as worst
worst_10_series <- tail(index,n=10)

authors<-c('Smyl','Manso','Pawlikowski','Jaganathan','Fiorucci','Petropoulos','Shaub','Zampeta','Doornik','Pedregal')
methods.rank <- c(1:10)
methods.plus.author<-NULL
for (k in 1:length(methods)){
   methods.plus.author<-rbind(methods.plus.author,paste(methods.rank[k],'-',authors[k],sep=''))}

#plot the best from category
as.numeric(unlist(head(index,n=10)))
cols <- c('green','deepskyblue3','firebrick','gold4','darkorchid','grey70','lightblue4','mediumblue','gray41','tan2')

#top #4 series
#par(mar=c(0.5, 0.5, 1, 1.5), mfrow=c(1,5), oma = c(3, 3, 2, 0.1)) #five in a row
par(mar=c(0.1, 1.5 ,1.0 , 0.5), mfrow=c(2,2), oma = c(2, 2, 0.1, 0.1)) #four in a row - bottom/left/up/right
for (i in 1:num_series){
  no.series <- as.numeric(unlist( best_10_series[i]))
  ts <- ts(matrix( unlist((compare.series[[no.series]])),ncol=length(methods),nrow=ncol(true_values)-1))
  colnames(ts)<-authors
  x.labs <-  seq(0,round(nrow(ts),0),2)
  y.labs <- round(seq(min(ts, na.rm=T)-0.01,max(ts, na.rm=T)+0.01, ((max(ts, na.rm=T)+0.01)-(min(ts, na.rm=T)-0.01))/3),2)
  colnames(ts)<- methods
  
  plot(ts, plot.type = "single", xaxt = "n", yaxt = "n", lty = 1:2,col = cols,lwd=.5,
         ylim=c(min(y.labs),max(y.labs)))
  if(i>2){    axis(1, at=x.labs,x.labs,cex.axis=.85,mgp=c(0,.5,.0))}
    axis(2, at=y.labs,y.labs,cex.axis=.85,mgp=c(0,.75,.0))
    title(main=paste('#',best_10_series[i],sep = ''),cex.main=1,lwd=1)
    if (i==1 || i==3){mtext("% Error", side=2, line=2, adj=0.5,cex=1)}
  }

#bottom #4 series
#par(mar=c(0.5, 0.5, 1, 1.5), mfrow=c(1,5), oma = c(3, 3, 2, 0.1)) #five in a row 
par(mar=c(0.1, 1.5 ,1.0 , 0.5), mfrow=c(2,2), oma = c(2, 2, 0.1, 0.1)) #four in a row - bottom/left/up/right
for (i in 1:num_series){
   no.series <-as.numeric(unlist(worst_10_series[11-i]))
   ts <- ts(matrix( unlist((compare.series[[no.series]])),ncol=length(methods),nrow=ncol(true_values)-1))
   colnames(ts)<- methods
   x.labs <-  seq(0,round(nrow(ts),0),2)
   y.labs <- round(seq(min(ts, na.rm=T)-0.01,max(ts, na.rm=T)+0.01, ((max(ts, na.rm=T)+0.01)-(min(ts, na.rm=T)-0.01))/3),1)
   
   plot(ts, plot.type = "single", xaxt = "n", yaxt = "n", lty = 1:2,col = cols,lwd=.5,
        ylim = c(min(y.labs),max(y.labs)))
   if(i>2){axis(1, at=x.labs,x.labs,cex.axis=.85,mgp=c(0,.75,.0))}
     axis(2, at=y.labs,y.labs,cex.axis=.85,mgp=c(0,.75,.0))
     title(main=paste('#',worst_10_series[11-i],sep = ''),cex.main=1,lwd=1)
     if(i==1 || i==3){mtext("% Error", side=2, line=2, adj=0.5,cex=1)}
 }

#####  BEST / WORST SERIES UP HERE  #####

# plot the legend of the above methods
# par(mar=c(0.1, 0.1, 0.1, 0.1), mfrow=c(1,1), oma = c(0.1, 0.1, 0.1, 0.1))
# plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
# legend("center",inset=c(0.,0),xpd = TRUE,ncol=5, legend=methods.plus.author, col=cols, lty=1:2, cex=0.8,horiz=F,lwd=2)

#### BEST / WORST LENGTHS FROM HERE #####
# start loading train data
setwd(paste0(m4filesPath,'\\Dataset\\Train'))
train_values <- read.csv(paste(type[filter],'-train.csv',sep=''))

# Hourly    : H
# Daily     : D
# Weekly    : W
# Monthly   : M
# Quarterly : Q
# Yearly    : Y

#*must be the same as filter type - line 100, 
#*unless best, worst series are loaded from excel file
filter <- as.character(type['Quarterly'])

no.series<-NULL
#load numbers of series to study
for (i in 1:5){no.series <- rbind(no.series, as.numeric(unlist(worst_10_series[11-i])))}

# insert manual series to study
# no.series <- c(21199,2837,9269,21384,20572)#

setwd(paste0(m4filesPath,'\\Dataset\\Test'))
# Load the true values
true_values <-read.csv(paste(type[filter],'-test.csv',sep=''))
{
#set true values for series to study - prepare timeseries to plot
df<- t((((true_values[no.series,c(2:ncol(true_values))]))))

# plot the true values for certain series
plot.title <- paste(type[filter],' Train/Test Values',sep = '')

cols <- c('green','deepskyblue3','firebrick','darkorchid','mediumblue','gray41','tan2','grey70','lightblue4')
# test values for the series under study
df_t<- t((((train_values[no.series,c(2:ncol(train_values))]))))

cols<-c(cols[1:length(no.series)])
column_values <- NULL
for (j in 1:length(no.series)){
  tmp_df_t<-df_t
  tmp_df_t<-tmp_df_t[complete.cases(tmp_df_t[,c(j)]),]
  column_values<-rbind(column_values,nrow(tmp_df_t))
}
# train values for the series under study
df_t<-df_t[complete.cases(df_t[,c(which(column_values==max(column_values)))]),]
}

# prepare dataset to plot
{
f2 <- matrix(1:(length(no.series) *nrow(df)),ncol =length(no.series))
for (i in 1:nrow(f2)){f2[i,]<- NA}
df_t <- rbind(df_t,f2)

cnames <- c(colnames(df_t))
new_cnames <- NULL
for(i in 1:length(no.series)){
  new_cnames<-cbind(new_cnames,paste(cnames[i],'t',sep = ''))
}

colnames(df) <- new_cnames
f<-matrix(1:(length(no.series)*(nrow(df_t)-nrow(df))),ncol =length(no.series))
for (i in 1:nrow(f)){f[i,]<-NA}
f<-rbind(f,df)
df<-cbind(df_t,f)
}

# set the appropriate train-test pairs - preview df data frame first i.e. [1,6], [2,7]
ts <- ts(df[,c(5,10)])
{      
cols <-c('#000080','#000080')
cols<-c(cols[1:length(no.series)])
x.labs <-  seq(0,round(nrow(ts),0) ,10)
y.labs <- seq(0,roundUpNice(max(ts, na.rm=T)),roundUpNice(round(max(ts, na.rm=T)/5,0)))
par(mar=c(.5, .5 ,1.0 , 0.5), mfrow=c(1,1), oma = c(2, 2, 0.1, 0.1)) 
# par(fig = c(0, 1, 0, 1), oma = c(0, 1, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(ts, plot.type = "single", xaxt = "n",yaxt = "n", lty = c(1,1),col = cols,lwd=.5,xlab= '',
     ylab='',cex=0.7,cex.lab=0.7)
axis(1, at=x.labs,cex.axis=0.7,mgp=c(0,0.25,.0))
axis(2, at=y.labs,y.labs,cex.axis=0.7,mgp=c(0.,.5,.0))
mtext("Values", side=2, line=1.25, adj=0.5,cex=.75)
mtext(text = "Observations", side = 1.25,line = 1,cex=.75)}

 
