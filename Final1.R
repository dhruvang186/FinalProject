#install plyr package
r=getOption("repos")
r["CRAN"]="http://cran.us.r-project.org"
options(repos=r)
install.packages("plyr")
library(plyr)

#install FSA package
install.packages("FSA")
library(FSA) 

install.packages("plotrix")
library(plotrix)
#install FSAdata package
install.packages("FSAdata")
library(FSAdata)

#install magrittr package
install.packages("magrittr")
library(magrittr)

#install dplyr package
install.packages("dplyr")
library(dplyr)

#install tidyverse package
install.packages("tidyverse")
library(tidyverse)

#install tidyr package
install.packages("tidyr")
library(tidyr)

install.packages("ggplot2")
library(ggplot2)

library(readxl)
insurance <- read_excel("C:/Users/dhruvang/Downloads/insurance.xlsx")
insurance

agency<-table(insurance$Agency)
agency
barplot(agency, las=2, col = "violet", main = "Top Agencies for Insurance", ylim = c(0,4000))

agencytype<-table(insurance$`Agency Type`)
agencytype
#piechart
lbls<-round(agencytype/sum(agencytype) * 100, 1)
pie3D(agencytype, labels = lbls, col = c("white","orange"), main="Types of Agency")
legend("top",inset = 0.05, c("Agency","Airlines Travel"), cex = 0.6, 
       fill = c("white","orange"))

plot(insurance$`Net Sales`,insurance$`Commision (in value)`,
     ylab = "Commision",
     xlab = "Net Sales",
     main="Scatterplot Sales vs commision")

claim<-table(insurance$Claim)
claim
pl<-barplot(claim, ylim = c(0,7000), col = "lightgreen", main = "Insurance claimed")
text(pl,claim+3,  labels = claim, pos = 3)


age<-table(insurance$Age)
age
hist(insurance$Age,xlab = "Age",main = "Insurance across age group", density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="brown")

channel<-table(insurance$`Distribution Channel`)
channel
ll<-round(channel/sum(channel)*100,1)
pie(channel,labels = ll, main = "Mode of distribution channel")
legend("topright",inset = 0.05, c("Offline","Online"), cex = 0.8, 
       fill = c("white","lightblue"))

#plot1
ggplot(data = insurance,
       aes(x = insurance$Agency,
           y = insurance$`Net Sales`))+
  geom_line(colour = 'darkgreen',
            size = 3, 
            stat = 'identity',
            alpha = 0.5)+
  geom_smooth()+
  coord_flip()+
  theme_bw()+
  labs(title = 'Sales done by agencies',
       x = 'Agency',
       y = 'Sales')

#plot2
pfg = ggplot(data = insurance,
             aes(y = insurance$Agency,
                 x = insurance$`Commision (in value)`))+
  geom_bar(fill = 'cadetblue',
           stat = 'identity',
           position = 'stack',)+
  xlab("Agency")+
  coord_flip()+
  theme_bw()+
  ylab("Commision")+
  labs(title = 'Commision of every agency')
pfg

#boxplot
ggplot(insurance, aes(x=insurance$Age, y=insurance$Gender)) + 
  geom_boxplot(fill="lightblue") + 
  xlab("age")+ ylab("Gender")+
  labs(title = 'Age and Gender of people taking insurance')

summary(insurance)
mean(insurance$Age)
mean(insurance$Duration)
mean(insurance$`Net Sales`)
mean(insurance$`Commision (in value)`)



