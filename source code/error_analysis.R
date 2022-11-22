library(sqldf)
# Function to replace all NA values to zero 
zero.replace <- function(x) { replace(x, is.na(x), 0) }

#m4 dataset path
m4filesPath <- paste0(Sys.getenv('USERPROFILE'),"\\Downloads\\M4-methods-master\\")
#multi factor analysis path
m4MultifactorPath <- paste0(Sys.getenv('USERPROFILE'),"\\Downloads\\m4-multi-factor-analysis-main\\")


# Set Working Directory for Prediction Files  
setwd(paste0(m4MultifactorPath,'data'))
# Select methods to work on
methods <- as.character(unlist(((read.table('least_corr_methods.txt' )))))
idToMethod <- (((read.csv('IDtoMethodType.csv'))))
methods.plus <- NULL
for(i in 1:length(methods)){
  strQ <- paste("select * from idToMethod where id=='",methods[i],"'",sep = '') 
  tmpQ <-  sqldf(strQ)
  methods.plus <- rbind(methods.plus,paste(tmpQ[1,c(1)],'-',tmpQ[1,c(2)]))
}

rm(strQ,tmpQ)

setwd(paste0(m4filesPath,'Point Forecasts'))
preds<-list()
# unzip and Load the under study Predictions and zero the NA values
for (i in 1:length(methods)){
  untar(paste('submission-',methods[i],'.rar',sep = ''))
  preds[[i]] <- zero.replace(read.csv(sprintf('submission-%s.csv',methods[i])))
}

# Set Working Directory for True Values
setwd(paste0(m4filesPath,'\\Dataset\\Test'))
# Codify the type of predictions
type <- list(Daily='D',Hourly='H',Monthly='M',Quarterly='Q',Weekly='W',Yearly='Y',
             D='Daily',H='Hourly',M='Monthly',Q='Quarterly',W='Weekly',Y='Yearly')

# Select type of predictions to filter e.g. H for hourly
# Daily     : D
# Hourly    : H
# Monthly   : M
# Quarterly : Q
# Weekly    : W
# Yearly    : Y

#!!!!! For every NEXT study type START HERE !!!!!
#### Change filter using one of the above types e.g. Hourly ####
filter <- as.character(type['Yearly'])
#### ------------------------------------------------ ####

# Set the number of series to calculate
n.series <-1:10

# Load the true values
true_values <-read.csv(paste(type[filter],'-test.csv',sep=''))

# Calculate and create the error lists
errors <- list()
for (i in 1:length(preds)){
  tmp_pred <- data.frame(preds[i])
  tmp <- sqldf(paste('select * from tmp_pred where id like "%',filter,'%"',sep = ''))
  errors[[i]] <-  (cbind(id=true_values[n.series,c(1)],true_values[n.series,c(2:ncol(true_values))] - tmp[n.series,c(2:ncol(true_values))]))
}
rm(tmp,tmp_pred)

# create error series list per method
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

# create the compare series list
compare.series <- list()
for (i in n.series){
  compare.series[[i]] <- (sapply(series,'[[',i))
}

# par(mfrow=c(1,1))
# Set the number of error series to compare
no.series <- 5
ts <- ts(matrix( unlist((compare.series[[no.series]])),ncol=length(methods),nrow=ncol(true_values)-1))
colnames(ts)<- methods

plot.title<- paste('Comparison of Least Correlated Methods & Series ',filter,no.series,sep = '')
cols <- c('green','deepskyblue3','firebrick','gold4','darkorchid','grey70',
          'lightblue4','mediumblue','gray41','tan2','snow3')

# one series at a time
# plot(ts, yax.flip = FALSE, main=plot.title)

# all series in a plot
plot(ts, plot.type = "single", lty = 1:6,col = cols,lwd=2,xlab= paste(type[filter],'Series'),
        ylab='Error', main=plot.title)
legend("left",inset=c(0,0),xpd = TRUE,ncol=3, legend=c(methods.plus), col=cols, lty=1:6, cex=0.8,horiz=F,lwd=2)

