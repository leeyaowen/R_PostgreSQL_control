library(DBI)
library(RPostgreSQL) 
drv<-dbDriver("PostgreSQL") #宣告資料庫名稱 
con<-dbConnect(drv,user="postgres",password="2717484",dbname="forest") #建立連線字串
dbGetQuery(con,"select * from plotdata where dbh>20") #下SQL(可視資料)
data<- dbGetQuery(con,"select * from plotdata where dbh>20") #下SQL指向資料
View(data) 

sp<-iconv(data$sp,"UTF-8","CP950") #解決UTF-8亂碼問題，轉換為繁體中文(CP950)

library(spatstat)
x3<-as.numeric(data$x3) #data type轉換，字串轉數字
y3<-as.numeric(data$y3)
mypattern<-ppp(x3,y3,c(0,500),c(0,500)) #進入spatstat分析階段
plot(mypattern)
L <- envelope(mypattern, Lest)
plot(L,.-r~r,ylab=expression(L(r)), xlab = "d (m)",main="title",legend=FALSE)
