data=read.csv("C:/Users/VEDANG SAWANT/Desktop/Forage/Quantium/QVI_data.csv")

salesbystore=data.frame(number=data$STORE_NBR,sales=data$TOT_SALES)
anova=aov(sales~number,data=salesbystore)
summary(anova)

sales_store=data.frame(aggregate(data$TOT_SALES,by=list(data$STORE_NBR),FUN=sum))
which.max(sales_store$x)
sales_store[226,]

quantitybynumber=data.frame(number=data$STORE_NBR,quantity=data$PROD_QTY)
anova=aov(quantity~number,data=quantitybynumber)
summary(anova)


quantity_store=data.frame(aggregate(data$PROD_QTY,by=list(data$STORE_NBR),FUN=sum))
which.max(quantity_store$x)
quantity_store[226,]
ggplot(quantity_store,aes(x=Group.1,y=x))+geom_col()+geom_col(fill='black')

sales_store=data.frame(aggregate(data$TOT_SALES,by=list(data$STORE_NBR),FUN=sum))
which.max(sales_store$x)
sales_store[226,]
ggplot(sales_store,aes(x=Group.1,y=x))+geom_col()+geom_col(fill='black')

sales_store[88,]
sales_store[86,]
sales_store[77,]

quantity_store[77,]
quantity_store[88,]
quantity_store[86,]
