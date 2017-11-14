library(dtplyr)
library(formattable)
library(ggplot2)
library(plotly)
library(dplyr)
##讀取資料
movieData<-read.csv("C:/Users/user/Desktop/movie_metadata.csv", stringsAsFactors = F)
#清理資料
movieData$movie_title<-gsub('\xc2\xa0','',movieData$movie_title)
movieData$director_name<-gsub('\xc3','',movieData$director_name)
###imdb 評分 v.s 導演 
#1,前20名排行imdb評分高的導演
temp<-movieData%>%
  select(director_name,imdb_score)%>%
  group_by(director_name)%>%
  summarise(avg=mean(imdb_score),n=n())%>%
  arrange(desc(avg))
formattable(temp[1:20,], list(avg = color_bar("pink")),align = 'l')
#仔細看發現前20都是新生代(只拍過一部)，因此覺得應該分開去討論，分隔點為作品為5部基準，資料集增加一欄位導演作品數
movieData2<-temp[temp$n>5,]
formattable(movieData2[1:20,], list(avg = color_bar("orange")),align = 'l')
#以下為將前幾名導演畫出圖比較
temp31<-movieData[movieData$director_name=="Quentin Tarantino",]
temp32<-movieData[movieData$director_name=="Christopher Nolan",]
temp33<-movieData[movieData$director_name=="Stanley Kubrick",]
temp34<-movieData[movieData$director_name=="James Cameron",]
temp3<-rbind(temp31,temp32,temp33,temp34)
plot_ly(temp3,x=~title_year,y=~imdb_score,color=~director_name,
        type="scatter",text=~movie_title)%>%
  add_lines(x = ~title_year,showlegend = FALSE)%>%
  dplyr::filter(imdb_score == max(imdb_score)) %>%
  layout(annotations = list(x = ~title_year, y = ~imdb_score, text =~paste("Movie:",movie_title), showarrow = T))
#最高分為Christopher Nolan，名作為全面啟動(inception)和蝙蝠俠黑暗騎士系列(The Dark Knight)，
#新生代前幾名散佈圖如下:
newdierctordata<-left_join(temp[1:10,],movieData,by="director_name")
plot_ly(newdierctordata[1:10,],x=~title_year,y=~imdb_score,color=~director_name,
        type="scatter",text=~movie_title)

##票房

temp5<-movieData%>%
  select(director_name,imdb_score,gross)%>%
  group_by(director_name)%>%
  summarise(avgscore=mean(imdb_score),avggross=mean(gross))%>%
  arrange(desc(avgscore))%>%
  filter(!is.na(avggross))
#
#plot_ly(temp5,x=~avgscore,y=~avggross,color=~director_name,
#        type="scatter")

plot_ly(temp5,x=~avgscore,y=~avggross,
              type="scatter")
plot_ly(movieData,x=~imdb_score,y=~gross,
        type="scatter")%>% 
  dplyr::arrange(desc(gross)) %>%
  layout(annotiations = list(y = ~gross[1:10], x = ~imdb_score
                            , text =~movie_title, showarrow = T))
##將資料增加欄位為導演總電影片
newdata<-left_join(movieData,temp,by="director_name")
#去除導演為空的
newdata2<-newdata%>%filter(director_name!="")
##ndnewdata<-newdata[newdata$director_name=="",]
newdata2$nq<-ifelse(newdata2$n>5,"new","old")
ggplot(newdata2,aes(x=title_year,y=gross,col=nq))+geom_point()
##ggplot(ndnewdata,aes(x=title_year,y=country))+geom_point()
col3 <- colorRamp(c("red","blue"))
newdata2$movie_title<-gsub('\xc2\xa0','',newdata2$movie_title)
plot_ly(newdata2,x=~title_year,y=~gross,color=~nq,colors=col3)
##票房和海報人頭關係
plot_ly(newdata2,x=~gross,y=~facenumber_in_poster,color=~nq,
        colors=col3,type="scatter",mode="markers",text=~paste("Movie:",movie_title))
plot_ly(newdata2,x=~gross,y=~facenumber_in_poster,color=~content_rating,
        type="scatter",mode="markers",text=~paste("Movie:",movie_title))



###2.電影劇情分類討論
#首先對genres欄位去做分析，並重新計算所有劇情分類個數
genres<-movieData$genres
test<-NULL
for(i in 1:length(genres)){
  str<-strsplit(genres[i], "|", fixed = TRUE)[[1]]
  test<-c(test,str)
}
#計算總數
ttable<-table(test)
newtest<-data.frame(ttable)

#plot_ly(newtest, labels = ~test, values = ~Freq
#        , type = 'pie', textinfo = 'label+percent') 
#畫圖表示，可以看出此資料集電影劇情分類次數
plot_ly(newtest,labels = ~test,textinfo = 'label+percent', values = ~Freq) %>%
  add_pie(hole = 0.6)
#此分別討論次數較多的Drama,Comedy,Thriller，當中去了解imdb評分和gross關係圖
DramaData<-movieData%>%filter(grepl("Drama",genres)==T)
ComedyData<-movieData%>%filter(grepl("Comedy",genres)==T)
ThrillerData<-movieData%>%filter(grepl("Thriller",genres)==T)
p1<-plot_ly(DramaData,y=~gross,x=~imdb_score,
        type="scatter",mode="markers",name="Drama",text=~paste("Movie:",movie_title))
  
p2<-plot_ly(ComedyData,y=~gross,x=~imdb_score,
            type="scatter",mode="markers",name="Comedy",text=~paste("Movie:",movie_title))
p3<-plot_ly(ThrillerData,y=~gross,x=~imdb_score,
            type="scatter",mode="markers",name="Thriller",text=~paste("Movie:",movie_title))
subplot(p1,p2,p3)
#由圖中可知，高imdb評分不一定有高票房，但是低imdb評分有低票房趨勢。在Drama中，比較突出有Titanic(7.7,658.6723M)和
#The Dark Knight(9,533.3161M)
#genres v.s. actor_1
#接著對關注的分類片來分析，了解拍攝最多的第一主角(actor_1)分別是哪些?
ComedyData$class="Comedy"
DramaData$class="Drama"
ThrillerData$class="Thriller"
classnewData<-rbind(ComedyData,DramaData,ThrillerData)
temp6<-classnewData%>%
  filter(actor_1_name!="")%>%
  group_by(class,actor_1_name)%>%
  summarise(n=n())%>%
  arrange(desc(n))
plot_ly() %>%
  add_pie(data = temp6[temp6$class=="Drama",][1:5,], labels = ~actor_1_name,textinfo = 'label+percent', values = ~n
          ,name = "Drama",domain = list(x = c(0, 0.4), y = c(0.4, 1))) %>%
  add_pie(data = temp6[temp6$class=="Comedy",][1:5,],labels = ~actor_1_name,textinfo = 'label+percent', values = ~n,
          name = "Comedy", domain = list(x = c(0.6, 1), y = c(0.4, 1))) %>%
  add_pie(data = temp6[temp6$class=="Thriller",][1:5,], labels = ~actor_1_name,textinfo = 'label+percent', values = ~n,
          name = "Thriller", domain = list(x = c(0.25, 0.75), y = c(0, 0.6))) %>%
  layout(title = "Pie Charts with actor_1 by genres", showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#從piechart中，在Drama和Thriller中Robert De Nira為暫居第一名，而在Comedy中，Robin williams為第一
#接著關注喜劇片(comedy)，gross和imdb_score關係圖中，Robin williams的片在gorss和評分表現還不錯，
m<-ComedyData[ComedyData$actor_1_name=="Robin Williams",]
##按照gross編排
m<-m%>%
  arrange(desc(gross))
a <- list(
  y = m$gross[1:5],
  x = m$imdb_score[1:5],
  text = m$movie_title[1:5],
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 20,
  ay = -40
)
m2<-ComedyData[ComedyData$movie_title=="Toy Story 3",]
plot_ly(ComedyData,x=~imdb_score,y=~gross,type="scatter",
        text=~paste("Actor_1:",actor_1_name,"<br>",movie_title))%>%
  layout(annotations=a)%>%
  add_annotations(y=m2$gross,x=m2$imdb_score,text = m2$movie_title,bgcolor="red")
#其中紅色部分是喜劇片中有名的玩具總動員，gross和imdb_score都很高