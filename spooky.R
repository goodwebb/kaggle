 
install.packages("tidytext")
library(tidytext)
library(dplyr)
library(wordcloud2)
library(plotly)
####介紹
#這次資料是以萬聖節為主題的分析，主要內容為三位知
#名的作者專門寫恐怖故事Edgar Allan Poe, Mary Shelley, and HP Lovecraft
#預測他們寫的故事段落分別是這三位哪位?
##資料整理
train <- read.csv("C:/Users/user/Desktop/spooky/train.csv", stringsAsFactors = F)
#資料介紹
summary(train)
#分別有三個欄位，其中author欄位的EAP是Edgar Allan Poe,HPL是HP Lovecraft,MWS是Mary Shelley
#文字計算
#我們使用了tidytext中unnest_tokens來修剪詞段分析詞彙，並用stop_words(英文中的介詞或語助詞)排除不必要的詞
#
t1<-train %>%
  unnest_tokens(word, text)
train_book<-t1%>%
  anti_join(stop_words,by="word") #移除stop_word EX:語助詞 the,a
##wordcloud
#1.all
#這邊使用的文字雲是wordcloud2套件,是一種互動式圖檔
train_book_count<-train_book%>%
  count(word)
Ranktrain<-train_book_count%>%
  arrange(desc(n))
wordcloud2(Ranktrain[1:500,],size = 0.3, gridSize = 5.0,shape = "star")
#從上圖可以看出,在所有詞段中,time和found是相對其他字眼使用最多
#以下畫出前50名出現最多的詞，前四名分別為time(729),life(563),night(559)和found(559)
rankcolors<-c(rep('rgba(222,45,38,0.8)',4),rep('rgba(204,204,204,1)',46))
plot_ly(Ranktrain[1:50,],x=~n,y=~word,
        type="bar",marker = list(color = rankcolors))
#2.Mary Shelley:
#以作者Mary Shelley去分析
#以下所使用的文字雲是wordcloud中的letterCloud，它可以畫出想要的字型輪廓
MSword<-train_book%>%
  filter(author == "MWS")%>%
  count(word)
MSRanktrain<-MSword%>%
  arrange(desc(n))
wordcloud2(MSRanktrain[1:500,],size = 0.3, gridSize = 5.0,shape = "cardioid")
##letterCloud(MSRanktrain[1:500,],size = 0.3, gridSize = 5.0,word = "M")
rankcolors2<-c(rep('rgba(30,45,204,30)',4),rep('rgba(204,204,204,1)',46))
#以下畫出前50名出現最多的詞，前四名分別為life(329),love(273),heart(262)和raymond(248)
plot_ly(MSRanktrain[1:50,],x=~n,y=~word,
        type="bar",marker = list(color = rankcolors2)) 
#3.Edgar Allan Poe
#以作者Edgar Allan Poe去分析
EAPword<-train_book%>%
  filter(author == "EAP")%>%
  count(word)
EAPRanktrain<-EAPword%>%
  arrange(desc(n))
wordcloud2(EAPRanktrain[1:500,],size = 0.3, gridSize = 5.0,shape = "diamond")
#letterCloud(EAPRanktrain[1:500,],size = 0.3, gridSize = 5.0,word = "E")
#以下畫出前50名出現最多的詞，前四名分別為time(260),found(230),length(178)和day(174)
rankcolors3<-c(rep('rgba(30,45,20,204)',4),rep('rgba(204,204,204,1)',46))
plot_ly(EAPRanktrain[1:50,],x=~n,y=~word,
        type="bar",marker = list(color = rankcolors3))
#4.HP Lovecraft
#以作者HP Lovecraft去分析
HPLword<-train_book%>%
  filter(author == "HPL")%>%
  count(word)
HPLRanktrain<-HPLword%>%
  arrange(desc(n))
wordcloud2(HPLRanktrain[1:500,],size = 0.3, gridSize = 5.0,shape = "pentagon")
#會影響到
##letterCloud(HPLRanktrain[1:500,],size = 0.3, gridSize = 5.0,word = "H")
#以下畫出前50名出現最多的詞，前四名分別為night(254),time(238),house(188)和found(186)
rankcolors4<-c(rep('rgba(30,204,20,23)',4),rep('rgba(204,204,204,1)',46))
plot_ly(HPLRanktrain[1:50,],x=~n,y=~word,
        type="bar",marker = list(color = rankcolors4))


#
#wordcloud(train_book_count$word, train_book_count$n,
#          max.words = 600, scale = c(1.8, 0.5),

#          colors = RColorBrewer::brewer.pal(9, "YlOrRd")[3:9])
