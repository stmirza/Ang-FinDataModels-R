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

/* 1.5 Comparing Capital Gains of Multiple Securities Over Time */

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

lines(x=multi.df$date,
  y=multi.df$AMZN.idx,
  col="black",
  lty=2,
  lwd=1)
lines(x=multi.df$date,
  y=multi.df$IBM.idx,
  col="gray",
  lty=2,
  lwd=1)
lines(x=multi.df$date,
  y=multi.df$YHOO.idx,
  col="gray",
  lty=1,
  lwd=1)

abline(h=1,lty=1,col="black)

legend("topleft",
  c("AMZN","IBM","YHOO","S&P 500 Index"),
  col=c("black","gray","gray","black"),
  lty=c(2,2,1,1),
  lwd=c(1,1,1,2))

* Fix the y-axis to encompass range of normalized values for all securities *
y.range<-range(multi.df[,6:9])
y.range

plot(x=multi.df$date,
  y=multi.df$GSPC.idx,
  type="l",
  xlab="Date",
  ylim=y.range,
  ylab="Value of Investment ($)",
  col="black",
  lty=1,
  lwd=2,
  main="Value of $1 Investment in AMZN, IBM, YHOO, and the S&P 500 Index
  December 31, 2010 - December 31, 2013")
lines(x=multi.df$date,
  y=multi.df$AMZN.idx,
  col="black",
  lty=2,
  lwd=1)
lines(x=multi.df$date,
  y=multi.df$IBM.idx,
  col="gray",
  lty=2,
  lwd=1)
lines(x=multi.df$date,
  y=multi.df$YHOO.idx,
  col="gray",
  lty=1,
  lwd=1)
abline(h=1,lty=1,col="black)
legend("topleft",
  c("AMZN","IBM","YHOO","S&P 500 Index"),
  col=c("black","gray","gray","black"),
  lty=c(2,2,1,1),
  lwd=c(1,1,1,2))

/* 1.5.1 Alternative Presentation of a Normalized Price Chart */
* Setup chart layout in R *
par(oma=c(0,0,3,0))

* Let R know we will be plotting 4 charts with 2 charts in each column and 2 charts in each row *
par(mfrow=c(2,2))

* Create the 4 plots *
plot(x=multi.df$date,
  xlab="",
  y=multi.df$YHOO.idx,
  ylim=y.range,
  ylab="",
  type="l",
  col="gray",
  main="Amazon Stock")
lines(x=multi.df$date,y=multi.df$GSPC.idx,col="gray")
lines(x=multi.df$date,y=multi.df$IBM.idx,col="gray")
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="black",lwd=2)
abline(h=1)
  
plot(x=multi.df$date,
  xlab="",
  y=multi.df$YHOO.idx,
  ylim=y.range,
  ylab="",
  type="l",
  col="gray",
  main="IBM Stock")
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="gray")
lines(x=multi.df$date,y=multi.df$GSPC.idx,col="gray")
lines(x=multi.df$date,y=multi.df$IBM.idx,col="black",lwd=2)
abline(h=1)

plot(x=multi.df$date,
  xlab="",
  y=multi.df$GSPC.idx,
  ylim=y.range,
  ylab="",
  type="l",
  col="gray",
  main="Yahoo Stock")
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="gray")
lines(x=multi.df$date,y=multi.df$IBM.idx,col="gray")
lines(x=multi.df$date,y=multi.df$YHOO.idx,col="black",lwd=2)
abline(h=1)

plot(x=multi.df$date,
  xlab="",
  y=multi.df$YHOO.idx,
  ylim=y.range,
  ylab="",
  type="l",
  col="gray",
  main="S&P 500 Index")
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="gray")
lines(x=multi.df$date,y=multi.df$IBM.idx,col="gray")
lines(x=multi.df$date,y=multi.df$GSPC.idx,col="black",lwd=2)
abline(h=1)

* Create a global title for the charts *
title1="Value of $1 Invested in Amazon, IBM, Yahoo, and the Market"
title2="December 31, 2010 - December 31, 2013"
title(main=paste(title1,"\n",title2),outer=T)

/* 1.6 Technical Analysis Examples */

/* 1.6.1 Trend: Simple Moving Average Crossover */

* Obtain closing prices for Amazon.com stock *
AMZN.sma<-data.AMZN[,4]
AMZN.sma[c(1:3,nrow(AMZN.sma)),]

* Calculate the rolling 50-day and 200-day moving average price *
AMZN.sma$sma50<-rollmeanr(AMZN.sma$AMZN.Close,k=50)
AMZN.sma$sma200<-rollmeanr(AMZN.sma$AMZN.Close,k=200)
AMZN.sma[c(1:3,nrow(AMZN.sma)),]
AMZN.sma[48:52,]

* Subset to only show 2012 and 2013 data *
AMZN.sma[198:202,]
AMZN.sma2012<-subset(AMZN.sma,
  index(AMZN.sma)>="2012-01-01")
AMZN.sma2012[c(1:3,nrow(AMZN.sma2012)),]

* Plot the SMA *
y.range<-range(AMZN.sma2012,na.rm=TRUE)
y.range
par(mfrow=c(1,1))
plot(x=index(AMZN.sma2012),
  xlab="Date",
  y=AMZN.sma2012$AMZN.Close,
  ylim=y.range,
  ylab="Price ($)",
  type="l",
  main="Amazon - Simple Moving Average
    January 1, 2012 - December 31, 2013")
lines(x=index(AMZN.sma2012),y=AMZN.sma2012$sma50)
lines(x=index(AMZN.sma2012),y=AMZN.sma2012$sma200,lty=2)
legend("topleft",
  c("Amazon Price","50-Day Moving Average","200-Day Moving Average"),
  lty=c(1,1,2))

/* 1.6.2 Volatility: Bollinger Bands */

* Obtain closing prices for Amazon.com stock *
AMZN.bb<-data.AMZN[,4]
AMZN.bb[c(1:3,nrow(AMZN.bb)),]

* Calculate the rolling 20-day mean and standard deviation *
AMZN.bb$avg<-rollmeanr(AMZN.bb$AMZN.Close,k=20)
AMZN.bb$sd<-rollapply(AMZN.bb$AMZN.Close,width=20,FUN=sd,fill=NA)
AMZN.bb[c(1:3,nrow(AMZN.bb)),]
AMZN.bb[18:22,]

* Subset to only show 2013 data *
AMZN.bb2013<-subset(AMZN.bb,
  index(AMZN.bb)>="2013-01-01")
AMZN.bb2013[c(1:3,nrow(AMZN.bb2013)),]

* Calculate the Bollinger Bands *
AMZN.bb2013$sd2up<-AMZN.bb2013$avg+2*AMZN.bb2013$sd
AMZN.bb2013$sd2down<-AMZN.bb2013$avg-2*AMZN.bb2013$sd
AMZN.bb2013[c(1:3,nrow(AMZN.bb2013)),]

* Plot the Bollinger Bands *
y.range<-range(AMZN.bb2013[,-3],na.rm=TRUE)
y.range
plot(x=index(AMZN.bb2013),
  xlab="Date",
  y=AMZN.bb2013$AMZN.Close,
  ylim=y.range,
  ylab="Price ($)",
  type="l",
  lwd=3,
  main="Amazon - Bollinger Bands (20 days, 2 deviations)
    January 1, 2013 - December 31, 2013")
lines(x=index(AMZN.bb2013),y=AMZN.bb2013$avg,lty=2)
lines(x=index(AMZN.bb2013),y=AMZN.bb2013$sd2up,col="gray40")
lines(x=index(AMZN.bb2013),y=AMZN.bb2013$sd2down,col="gray40")
legend("topleft",
  c("Amazon Price","20-Day Moving Average","Upper Band","Lower Band"),
  lty=c(1,2,1,1),
  lwd=c(3,1,1,1),
  col=c("black","black","gray40","gray40"))

/* 1.6.3 Momentum: Relative Strength Index */

* Relative Strength Index is equal to 100 - (100)/(1 + RS) *
* where RS is equal to the up average divided by the down average *
* with the averages calculated using the Wilder Exp MA *

* Obtain closing prices for Amazon.com stock *
AMZN.RSI<-data.AMZN[,4]
AMZN.RSI$delta<-diff(AMZN.RSI$AMZN.Close)
AMZN.RSI[c(1:3,nrow(AMZN.RSI)),]

* Create dummy variables to indicate whether price went up or down *
AMZN.RSI$up<-ifelse(AMZN.RSI$delta>0,1,0)
AMZN.RSI$down<-ifelse(AMZN.RSI$delta<0,1,0)
AMZN.RSI[c(1:3,nrow(AMZN.RSI)),]

* Calculate prices for up and down days *
AMZN.RSI$up.val<-AMZN.RSI$delta*AMZN.RSI$up
AMZN.RSI$down.val<--AMZN.RSI$delta*AMZN.RSI$down
AMZN.RSI<-AMZN.RSI[-1,]
AMZN.RSI[c(1:3,nrow(AMZN.RSI)),]

* Calculate initial up and down 14-day averages *
AMZN.RSI$up.first.avg<-rollapply(AMZN.RSI$up.val,
  width=14,FUN=mean,fill=NA,na.rm=TRUE)
AMZN.RSI$down.first.avg<-rollapply(AMZN.RSI$down.val,
  width=14,FUN=mean,fill=NA,na.rm=TRUE)
AMZN.RSI[c(1:3,nrow(AMZN.RSI)),]

* Calculate the Wilder Exponential Moving Average to calculate final up and down 14-day moving averages *
up.val<-as.numeric(AMZN.RSI$up.val)
down.val<-as.numeric(AMZN.RSI$down.val)

AMZN.RSI$up.avg<-AMZN.RSI$up.first.avg
for (i in 15:nrow(AMZN.RSI)) {
  AMZN.RSI$up.avg[i] <-
    ((AMZN.RSI$up.avg[i-1]*13+up.val[i])/14)
}
AMZN.RSI$down.avg<-AMZN.RSI$down.first.avg
for (i in 15:nrow(AMZN.RSI)) {
  AMZN.RSI$down.avg[i] <-
    ((AMZN.RSI$down.avg[i-1]*13+down.val[i])/14)
}
AMZN.RSI[c(1:20,nrow(AMZN.RSI)),]

* Calculate the RSI *
AMZN.RSI$RS<-AMZN.RSI$up.avg/AMZN.RSI$down.avg
AMZN.RSI$RSI<-100-(1--/(1+AMZN.RSI$RS))
AMZN.RSI[c(14:30,nrow(AMZN.RSI)),]

* Subset to show only 2012 and 2013 data *
AMZN.RSI2012<-subset(AMZN.RSI[,ncol(AMZN.RSI)],
  index(AMZN.RSI)>="2012-01-01")
AMZN.RSI2012[c(1:3,nrow(AMZN.RSI2012)),]

* Plot the RSI *
title1<-"Amazon - Relative Strength Index"
title2<-"January 2012 - December 2013"
plot(x=index(AMZN.RSI2012),
  xlab="Date",
  y=AMZN.RSI2012$RSI,
  ylab="RSI (14-Day Moving Average)",
  ylim=c(0,100),
  type="l",
  main=paste(title1,"\n",title2))
abline(h=c(30,70),lty=2)

/* end */
