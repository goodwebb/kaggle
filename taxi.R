
#所使用的套件
library(dplyr) #data manipulation資料整理
library(tidyr) #data manipulation
library(lubridate)#Date data manipulation
library(timeDate)# Date data manipulation
library(data.table)
library(plotly)  # Data visualization
library(ggplot2) # Data visualization
library(randomForest) # ML
#library(Imap)
#1.介紹
#資料集是美國紐約計程車旅程相關資訊,以下內容會透過R做探索性統計分析(EDA)
#http://www.freetoursbyfoot.com/wp-content/uploads/2013/11/New-York-Taxi.jpg
#from:http://www.freetoursbyfoot.com
#

train <- read.csv("C:/Users/user/Desktop/taxi/train.csv", stringsAsFactors = F)
summary(train)
#vendor_id:值只有1和2來可能是區分租車公司
#pickup_datetime:乘車時間
#dropoff_datetime:下車時間
#passenger_count :乘客數
#pickup/dropoff_longitute/latitute :上下車的經緯度
#trip_duration:此主題的目標變數，乘車的時間

#新增與修改欄位性質
train$pickup_month<-month(train$pickup_datetime)
train$passenger_count<-as.factor(train$passenger_count)
train$store_and_fwd_flag<-as.factor(train$store_and_fwd_flag)
train$pickup_hour<-hour(train$pickup_datetime)
train$pickup_isWeekend<-isWeekend(train$pickup_datetime)
train$pickup_dayOfWeek<-dayOfWeek(as.timeDate(train$pickup_datetime))
train$pickup_dayOfWeek<-as.factor(train$pickup_dayOfWeek)
train$vendor_id<-as.factor(train$vendor_id)
train<-train%>%
  mutate(dis=gdist(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,units="km"))


#2.feature visualisations

#trip_duration
#針對我們關注的trip_duration畫圖
#a.呈現lognormal分布
#b.有少數資料只有10秒左右，相當可疑
#c.在1e5秒也有出現一小段高峰，
train %>%
  ggplot(aes(trip_duration)) +
  geom_histogram(fill = "blue", bins = 150)+
  scale_x_log10() 
#查看那一小段高鋒資料:
train %>%
  arrange(desc(trip_duration))%>%
  select(trip_duration, pickup_datetime, dropoff_datetime,dis)%>%
  head(10)
#a.這些資料是跨日旅程,但有些只有1到5公里卻需要花這麼多時間,有點可疑

## passager_count
#計算出passenger_count次數和周末總數
test1<-train%>%
  group_by(passenger_count,pickup_isWeekend)%>%
  count()
#ggplot圖
#p <- ggplot(data=test1, aes(x=passenger_count, y=n, fill=pickup_isWeekend)) +
#  geom_bar(stat="identity", position=position_dodge())
#畫出passenger_count次數與pickup_isWeekend關係圖
test2<-test1%>%spread(pickup_isWeekend,n)
test2<-as.data.table(test2)
plot_ly(test2, x = ~passenger_count, y = ~`FALSE`, name = 'IsWeekend:F', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~`TRUE`, name = 'IsWeekend:T', mode = 'lines+markers')%>%
  layout(yaxis=list(title="n"))
#a.不管非周末或周末,資料為乘客1人為較多
#b.乘客人數為0,有少數資料
##passanger和vender_id
#由下圖可知,vendor_id:2,可以載客人數到9,vendor_id:1載客數最多只能到6人
test22<-train%>%
  group_by(passenger_count,vendor_id)%>%
  count()
test22<-test22%>%spread(vendor_id,n)
test22<-as.data.table(test22)
plot_ly(test22, x = ~passenger_count, y = ~`1`, name = 'vendor_id:1', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~`2`, name = 'vendor_id:2', mode = 'lines+markers')%>%
  layout(yaxis=list(title="n"))

##時段
#我們分別對每天時段(pickup_hour)與星期幾(pickup_dayOfWeek)分析,下圖可以知道5點以前周末(Sat,Sun)相對其他天多人搭乘
test3<-train%>%
  group_by(pickup_dayOfWeek,pickup_hour)%>%
  count()
#kaggle線顯示不出來
#plot_ly(test3, x = ~pickup_hour, y = ~n, color=~pickup_dayOfWeek, type = 'scatter', mode = 'lines+markers')
#另外一個方法:
test33<-test3%>%spread(pickup_dayOfWeek,n)
test33<-as.data.table(test33)

plot_ly(test33, x = ~pickup_hour, y = ~`Mon`, name = 'Mon', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~`Tue`, name = 'Tue', mode = 'lines+markers')%>%
  add_trace(y = ~`Wed`, name = 'Wed', mode = 'lines+markers')%>%
  add_trace(y = ~`Thu`, name = 'Thu', mode = 'lines+markers')%>%
  add_trace(y = ~`Fri`, name = 'Fri', mode = 'lines+markers')%>%
  add_trace(y = ~`Sat`, name = 'Sat', mode = 'lines+markers')%>%
  add_trace(y = ~`Sun`, name = 'Sun', mode = 'lines+markers')%>%
  layout(yaxis=list(title="n"))
#plot_ly(test1,x=~passenger_count,y=~n,color=~pickup_isWeekend,
#        type="scatter",mode="markers")
#星期幾總數(pickup_dayOfWeek)
#a.Sat,Fri比較多人搭乘
test5<-train%>%
      group_by(pickup_dayOfWeek)%>%
      count()
test5%>%
  plot_ly(x=~pickup_dayOfWeek,y=~n,type="bar")
test55<-train%>%
  group_by(pickup_dayOfWeek,passenger_count)%>%
  count()
test55%>%
  plot_ly(x=~pickup_dayOfWeek,y=~n,color=~passenger_count,type="bar",mode="markers")
#feature relation
# trip_duration v.s. dis
#a.有幾個數據相當可疑,可能是人工捏照的數據EX:短時間長距離(左邊)或是長時間超過24小時短距離(右下部分)
#b.大致上呈現trip_duration和distanc呈現正相關
ggplot(data=train, aes(x=dis, y=dis))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  labs(x = "Direct distance [km]", y = "Trip duration [s]")
###ML
#接著真的我們trip_dutation做預測,也是這個競賽所關注的地方
#Clean Data
#對資料做清理動作,讓之後模型較穩健
#a.移除距離只有0的資料
zero_dist <- train %>%
  filter(near(dis,0)&trip_duration > 60)
nrow(zero_dist)
#b.移除距離大和時間短的資料,距離大於100km,時間小於3600秒
ranktest<-train%>%
  arrange(desc(dis))
head(ranktest,20)
aa<-train%>%
  filter(dis>100&trip_duration<3600)
aa
#c.移除10小時以上距離只有5公里以內
ranktest<-train%>%
  arrange(desc(trip_duration))
head(ranktest,20)
train%>%
  filter(dis<5&trip_duration>36000)
##最後處理完資料如下:
finaldata<-train%>%
  filter(!(near(dis,0)&trip_duration > 60),
         !(dis>100&trip_duration<3600),
         !(dis<5&trip_duration>36000))
#RandomForest
 
mark<-sample(1:nrow(finaldata),nrow(finaldata)*0.9)
traindata<-finaldata[mark,]
testdata<-finaldata[-mark,]


iris.rf <- randomForest(log(trip_duration) ~ dis+pickup_isWeekend+pickup_hour+pickup_month
                        +pickup_longitude+pickup_latitude+dropoff_longitude+dropoff_latitude  
                        +passenger_count+vendor_id+pickup_dayOfWeek+store_and_fwd_flag, data=finaldata, importance=TRUE,
                        proximity=TRUE)
iris.rf
#畫出模型裡相對重要變數的圖
varImpPlot(iris.rf)
y<-predict(iris.rf,testdata)
mse<-sum((y-log(testdata$trip_duration))^2)/nrow(testdata)
mse