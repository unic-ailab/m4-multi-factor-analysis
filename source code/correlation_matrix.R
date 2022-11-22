library(sqldf)
library(caret)
library(corrplot)
library(Hmisc)
library(abind)

#set color palettes
{col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", 
                           "white","cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582","#FDDBC7",
                           "#FFFFFF", "#D1E5F0", "#92C5DE","#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow",
                           "#7FFF7F","cyan", "#007FFF", "blue", "#00007F"))}

#m4 dataset path
m4filesPath <- paste0(Sys.getenv('USERPROFILE'),"\\Downloads\\M4-methods-master\\")
#multi factor analysis path
m4MultifactorPath <- paste0(Sys.getenv('USERPROFILE'),"\\Downloads\\m4-multi-factor-analysis-main\\")

# set list of top-10 methods
top10methods <- c("118","245","237","072","069","036","078","260","238","039")

#set path to m4-point forecasts predictions
setwd(paste0(m4filesPath,'Point Forecasts'))

# study the top-10 methods
top_10 <- TRUE

#loop through methods - get file names and unzip rar files
if (top_10){
  files <- as.character(unlist(c(matrix(top10methods))))
  tmp_files<- NULL
  for (i in 1:length(files)){if(nchar(files[i])==2){
    untar(paste('submission-',files[i],'.rar',sep = ''))
    tmp_files<-rbind(tmp_files,paste('submission-0',files[i],'.csv',sep = ''))
    }
    else{
      untar(paste('submission-',files[i],'.rar',sep = ''))
      tmp_files<- rbind(tmp_files,paste('submission-',files[i],'.csv',sep = ''))}}
  files <-as.character(tmp_files)
  rm(tmp_files)
  } else {
  files <- list.files()}

# Function to replace all NA values to zero 
zero.replace <- function(x) { replace(x, is.na(x), 0) }
preds<-list()

# Load under study Predictions and zero the NA values
for (i in 1:length(files)){ preds[[i]] <- zero.replace(read.csv(files[i]))}

methods <- sapply(strsplit(sapply(strsplit(files, "-"), "[", 2), ".csv"), "[", 1)

# load method types
setwd(paste0(m4MultifactorPath,'\\data'))
idToMethod <- (((read.csv('IDtoMethodType.csv'))))
methods.plus <- NULL
for(i in 1:length(methods)){
  strQ <- paste("select * from idToMethod where id=='",methods[i],"'",sep = '') 
  tmpQ <-  sqldf(strQ)
  methods.plus <- rbind(methods.plus,paste(tmpQ[1,c(1)],'-',tmpQ[1,c(2)]))
}
rm(strQ,tmpQ)

# set working directory for True Values
setwd(paste0(m4filesPath,'\\Dataset\\Test'))

# codify the type of predictions
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
{
# Load selected type true values
true_values <-read.csv(paste(type[filter],'-test.csv',sep=''))

# Set the number of series to calculate
n.series <-1:nrow(true_values)

# calculate and create the error percentage lists
errors <- list()
for (i in 1:length(preds)){
  tmp_pred <- data.frame(preds[i])
  tmp <- sqldf(paste('select * from tmp_pred where id like "%',filter,'%"',sep = ''))
  errors[[i]] <-  (cbind(id=true_values[n.series,c(1)], (true_values[n.series,c(2:ncol(true_values))]
                    - tmp[n.series,c(2:ncol(true_values))])/true_values[n.series,c(2:ncol(true_values))]   ))
  cat(" errors percentage of method ", i,' from ', length(preds), "\n")
  }
rm(tmp,tmp_pred)

# create the error percentage series list per method
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
for (i in n.series){compare.series[[i]] <- (sapply(series,'[[',i))}

# loop through, compare series and calculate avg corr-matrix
for (j in  seq(1,nrow(true_values)-1,by=2)){
   # get current j series
   ts <- (matrix( unlist((compare.series[[j]])),ncol=length(preds),nrow=ncol(true_values)-1))
   corrMatrix <- cor(ts, method = "pearson", use = "complete.obs")
   if(any(is.na(corrMatrix) == TRUE ,na.rm = FALSE)){next}
  
   # get series j+1
   ts_ <- (matrix( unlist((compare.series[[j+1]])),ncol=length(preds),nrow=ncol(true_values)-1))
   corrMatrix_ <- cor(ts_, method = "pearson", use = "complete.obs")
   if(any(is.na(corrMatrix_) == TRUE ,na.rm = FALSE)){next}
   # calculate avg matrix
   avg_ <-  apply(abind(corrMatrix,corrMatrix_, along=3),  1:2, mean)
   if(j==1){corrMatrix_avg <-avg_} else {
     corrMatrix_avg <-apply(abind(corrMatrix_avg,avg_, along=3),  1:2, mean)
   }
   cat("series", j,' from ', nrow(true_values), "\n")
}

#top 10 authors
authors<-c('Smyl','Manso','Pawlikowski','Jaganathan','Fiorucci','Petropoulos','Shaub','Zampeta','Doornik','Pedregal')
methods.rank <- c(1:10)

methods.plus.author<-NULL
for (k in 1:length(methods)){
  methods.plus.author<-rbind(methods.plus.author,paste(methods.rank[k],'-',authors[k],sep=''))
}

rownames(corrMatrix_avg)<- methods.plus.author
colnames(corrMatrix_avg)<- methods.plus.author

col <- colorRampPalette(c("#85AACE")) 

#create correlation plot for top-10 methods
corrplot(corrMatrix_avg, method="pie", col=col(1),  cl.pos = "n",
         type="upper", number.cex = .6,
         addCoef.col = "grey20", # Add coefficient of correlation
         tl.srt=45, #Text label color and rotation
         # Combine with significance
         # p.mat = p.mat,  sig.level = 0.05,
         tl.col = "black",cl.lim = c(0,1),
         # hide correlation coefficient on the principal diagonal
         diag=TRUE)
}
