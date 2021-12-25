pdf('Task1_graphs.pdf')
data=read.csv("C:/Users/VEDANG SAWANT/Desktop/Forage/Quantium/QVI_data.csv")
summary(data)
data$DATE=as.Date(data$DATE)
data$month=as.Date(cut(data$DATE,breaks='month'))
data$year=as.Date(cut(data$DATE,breaks = 'year'))

library(Amelia)
missmap(data)
#No missing observations are present in given data
View(data)
#Total sales in each years
library(ggplot2)
ggplot(data=data,aes(year,TOT_SALES))+stat_summary(fun=sum,geom='bar')+geom_col(fill='black')
#Total sales in each month
sum(data$TOT_SALES)
ggplot(data = data,aes(month,TOT_SALES))+ stat_summary(fun = sum, geom = 'bar')+ geom_col(fill='black')
ggplot(data = data,aes(month,PROD_QTY))+ stat_summary(fun = sum, geom = 'bar')+ geom_col(fill='black')
sales_month=data.frame(aggregate(data$TOT_SALES, by=list(data$month),FUN=sum))
quantity_month=data.frame(aggregate(data$PROD_QTY, by=list(data$month), FUN=sum))
View(sales_month)
#maximum sales & quantity order in which month
which.max(sales_month$x)
sales_month[6,]
which.max(quantity_month$x)
quantity_month[6,]

ggplot(data = data,aes(x=TOT_SALES,y=PROD_QTY))+geom_point()
#Total number of customers
library(dplyr)
count(distinct(data,LYLTY_CARD_NBR))


#Total transactions
count(distinct(data,TXN_ID))
#Total sales per customer
sales_cust=data.frame(aggregate(data$TOT_SALES,by=list(data$LYLTY_CARD_NBR),FUN=sum))
#Maximum sales of which customer
which.max(sales_cust$x)
sales_cust[60925,]

##Total sales by each brand 
sales_brand=data.frame(aggregate(data$TOT_SALES,by=list(data$BRAND),FUN=sum))
#Maximum sales of which brand
which.max(sales_brand$x)
sales_brand[10,]
ggplot(data,aes(BRAND,TOT_SALES))+stat_summary(fun = sum, geom = 'bar')+geom_col(fill='black')


#Total sales by store number
sales_store=data.frame(aggregate(data$TOT_SALES,by=list(data$STORE_NBR),FUN=sum))
#Maximum sales by which store
which.max(sales_store$x)
sales_store[226,]

#Total sales by customer segments
sales_premium=data.frame(aggregate(data$TOT_SALES, by=list(data$PREMIUM_CUSTOMER), FUN=sum))
which.max(sales_premium$x)
sales_premium[2,]
which.min(sales_premium$x)
sales_premium[3,]
ggplot(sales_premium,aes(x=Group.1,y=x))+geom_col()+geom_col(fill='black')
#Mainstream customers are having maximum total sales
sales_family=data.frame(aggregate(data$TOT_SALES, by=list(data$LIFESTAGE), FUN=sum))
sales_family=data.frame(aggregate(data$LYLTY_CARD_NBR, by=list(data$LIFESTAGE), FUN=length))
which.max(sales_family$x)
sales_family[4,]
ggplot(sales_family,aes(x=Group.1,y=x))+geom_col()+geom_col(fill='black')
#Older singles/Couples are having maximum total sales

sales_chips=data.frame(aggregate(data$TOT_SALES, by=list(data$PROD_NAME), FUN=sum))
which.max(sales_chips$x)
sales_chips[12,]
which.min(sales_chips$x)
sales_chips[106,]
ggplot(sales_chips,aes(x=Group.1,y=x))+geom_col()+geom_col(fill='black')

ggplot(data,aes(x=data$TOT_SALES,y=data$PACK_SIZE))+geom_point()

#Outliers
names(data)
boxplot(data$TOT_SALES)
boxplot(data$PACK_SIZE)

remove_outliers<-function(x,na.rm=TRUE){
  qnt<-quantile(x,probs=c(.25,.75),na.rm=TRUE)
  H<-1.5*IQR(x,na.rm)
  y<-x
  y[x<qnt[1]-H]<-NA
  y[x>qnt[2]+H]<-NA
  x<-y
  x[!is.na(x)]
}
boxplot(remove_outliers(data$TOT_SALES))
boxplot(remove_outliers(data$PACK_SIZE))
dev.off()



