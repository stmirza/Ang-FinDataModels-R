/* CHAPTER 1 - PRICES */

/* Check working directory */
getwd()

/* 1.2 Importing Price Data from Yahoo Finance */

* Import data into R *
data.AMZN<-read.csv("AMZN.csv",header=TRUE)
head(data.AMZN)
tail(data.AMZN)

* Convert date variable from Factor to Date *
class(data.AMZN$Date)
date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
head(date)
tail(date)
class(date)

* Combining date and data.AMZN *
data.AMZN<-cbind(date, data.AMZN[,-1])
head(data.AMZN)
tail(data.AMZN)

* Sort data in chronological order *
data.AMZN<-data.AMZN[order(data.AMZN$date),]
head(data.AMZN)
tail(data.AMZN)

* Convert from data.frame object to xts object *
class(data.AMZN)
library(xts)
data.AMZN<-xts(data.AMZN[,2:7],order.by=data.AMZN[,1])
head(data.AMZN)
tail(data.AMZN)
class(data.AMZN)

* Rename variables *
names(data.AMZN)
names(data.AMZN)<-
  paste(c("AMZN.Open","AMZN.High","AMZN.Low","AMZN.Close","AMZN.Adjusted","AMZN.Volume"))
head(data.AMZN)
tail(data.AMZN)

/* 1.3 Checking the Data */

* Plotting the data *
plot(data.AMZN$AMZN.Close)
data.missing<-data.AMZN[-400:-500,]
plot(data.missing$AMZN.Close)

* Checking the Dimension *
dim(data.AMZN)

* Outputting summary statistics *
summary(data.AMZN)

/* 1.4 Basic Data Manipulation Techniques */

* Keeping and deleting one row *
AMZN.onlyFirst<-data.AMZN[1,]
AMZN.onlyFirst
AMZN.delFirst<-data.AMZN[-1,]
head(AMZN.delFirst)

* Keeping first and last rows *
data.AMZN[c(1,nrow(data.AMZN)),]

* Keeping contiguous rows *
AMZN.first.week<-data.AMZN[2:6,]
AMZN.first.week
AMZN.last30<-data.AMZN[((nrow(data.AMZN)-29)):nrow(data.AMZN),]
AMZN.last30
nrow(AMZN.last30)

* Keeping first three rows and last row *
data.AMZN[c(1:3,nrow(data.AMZN)),]

* Keeping and deleting one column *
names(data.AMZN)
AMZN.onlyPrice<-data.AMZN[,4]
AMZN.onlyPrice[c(1:3,nrow(AMZN.onlyPrice)),]
AMZN.onlyPrice2<-data.AMZN$AMZN.Close
AMZN.onlyPrice2[c(1:3,nrow(AMZN.onlyPrice2)),]

AMZN.delAdjPrice<-data.AMZN[,-6]
AMZN.delAdjPrice[c(1:3,nrow(AMZN.delAdjPrice)),]

* Keeping non-contiguous columns *
AMZN.OpenClose<-data.AMZN[,c(1,4)]
AMZN.OpenClose[c(1:3,nrow(AMZN.OpenClose)),]

* Keeping contiguous columns *
AMZN.PriceVol<-data.AMZN[,4:5]
AMZN.PriceVol[c(1:3,nrow(AMZN.PriceVol)),]

* Keeping contiguous and non-contiguous columns *
AMZN.OpenCloseVol<-data.AMZN[,c(1,4:5)]
AMZN.OpenCloseVol[c(1:3,nrow(AMZN.OpenCloseVol)),]

AMZN.OpenCloseVol<-data.AMZN[,c(-2:3,-6)]
AMZN.OpenCloseVol[c(1:3,nrow(AMZN.OpenCloseVol)),]

* Subsetting rows and columns *
data.vwap<-data.AMZN[((nrow(data.AMZN)-29)):nrow(data.AMZN),c(4,5)]
data.vwap[c(1:3,nrow9data.vwap)),]

* Subsetting using dates *
class(data.AMZN)
xts.2012<-subset(data.AMZN[,4],
  index(data.AMZN) >= "2012-01-01" &
  index(data.AMZN) <= "2012-12-31")
xts.2012[c(1:3,nrow(xts.2012))]

AMZN.2012<-cbind(index(data.AMZN),
  data.frame(data.AMZN[,4]))
AMZN.2012[c(1:3,nrow(AMZN.2012)),]
class(AMZN.2012)

names(AMZN.2012)[1]<-paste("date")
rownames(AMZN.2012)<-seq(1,nrow(AMZN.2012),1)
AMZN.2012[c(1:3,nrow(AMZN.2012)),]

AMZN.2012<-subset(AMZN.2012,
  AMZN.2012$date >= "2012-01-01"&
  AMZN.2012$date <= "2012-12-31")
AMZN.2012[c(1:3,nrow(AMZN.2012)),]

* Converting daily prices to weekly and monthly prices *
wk<-data.AMZN
data.weekly<-to.weekly(wk)
data.weekly[c(1:3,nrow(data.weekly)),]


data.AMZN[2:6,]
sum(data.AMZN[2:6,5])

mo<-data.AMZN
data.monthly<-to.monthly(mo)
data.monthly[c(1:3,nrow(data.monthly)),]

* Plotting a candlestick chart using monthly data *
library(quantmod)
OHLC<-data.monthly[-1,6]
AMZN.ohlc<-as.quantmod.OHLC(OHLC,
  col.names=c("Open","High","Low","Close","Volume"))
class(AMZN.ohlc)
AMZN.ohlc[c(1:3,nrow(AMZN.ohlc)),]

chartSeries(AMZN.ohlc,
  theme="white.mono",
  name="AMZN OHLC")

/* Comparing Capital Gains of Multiple Securities Over Time */

ls()
rm(list=ls())
ls()

* Import data for each of the four securities *
data.AMZN<-read.csv("AMZN Yahoo.csv",header=TRUE)
date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
data.AMZN<-cbind(date, data.AMZN[,-1])
data.AMZN<-data.AMZN[order9data.AMZN$date),]
data.AMZN<-xts(data.AMZN[,2:7],order.by=data.AMZN[,1])
names(data.AMZN)<-
  paste(c("AMZN.Open","AMZN.High","AMZN.Low",
  "AMZN.Close","AMZN.Volume","AMZN.Adjusted"))
data.AMZN[c(1:3,nrow(data.AMZN)),]
* ^repeat for each security ticker: AMZN, YHOO, IBM, GSPC *

* Combine data into one data object *
Close.Prices<-data.AMZN$AMZN.Close
Close.Prices<-cbind(Close.Prices,data.GSPC$GSPC.Close,
  data.YHOO$YHOO.Close,data.IBM$IBM.Close)
Close.Prices[c(1:3,nrow(Close.Prices)),]

* Convert data into a data.frame *
multi.df<-cbind(index(Close>Prices),
  data.frame(Close.Prices))
names(multi.df)<-paste(c("date","AMZN","GSPC","YHOO","IBM"))
rownames(multi.df)<-seq(1,nrow(multi.df),1)
multi.df[c(1:3,nrow(multi.df)),]

* Calculate normalized values for each security *
multi.df$AMZN.idx<-multi.df$AMZN/multi.df$AMZN[1]
multi.df$GSPC.idx<-multi.df$GSPC/multi.df$GSPC[1]
multi.df$YHOO.idx<-multi.df$YHOO/multi.df$YHOO[1]
multi.df$IBM.idx<-multi.df$IBM/multi.df$IBM[1]
options(digits=5)
multi.df[c(1:3,nrow(multi.df)),]
options(digits=7)

* Plot the capital appreciation of each security *
plot(x=multi.df$date,
  y=multi.df$GSPC.idx,
  type="l",
  xlab="Date",
  ylab="Value of Investment ($)",
  col="black",
  lty=1,
  lwd=2,
  main="Value of $1 Investment in AMZN, IBM, YHOO, and the S&P 500 Index
  December 31, 2010 - December 31, 2013")
