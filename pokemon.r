read.csv("C:/Users/Shivam Singh/Desktop/pokemon.csv")->pokemon
View(pokemon)

library(dplyr)
pokemon %>% select(-1)->pokemon

#renaming columns
colnames(pokemon)[2]<-"Primary_type"
colnames(pokemon)[3]<-"Secondary_type"

#Understanding data
str(pokemon)

#convert logical datatype to factor
#as.factor(pokemon$isLegendary)->pokemon$isLegendary

#Differnet primary types of pokemon
table(pokemon$Primary_type)

#seleting Grass pokemon
library(dplyr)
pokemon %>% filter(Primary_type=="Grass")->Grass_pokemon
Grass_pokemon %>% filter(Secondary_type=="Poison")->Grass_Poison_pokemon
range(Grass_Poison_pokemon$Speed)
Grass_Poison_pokemon %>% filter(Speed==90)->My_Grass_pokemon
View(My_Grass_pokemon)

#selecting water pokemon
pokemon %>% filter(Primary_type=="Water")->Water_pokemon
Water_pokemon %>% filter(Secondary_type=="Psychic")->Water_Psychic_pokemon
range(Water_Psychic_pokemon$Defense)
Water_Psychic_pokemon %>% filter(Defense==180)->My_Water_pokemon
View(My_Water_pokemon)

#selecting fire pokemon
pokemon %>% filter(Primary_type=="Fire")->Fire_pokemon
Fire_pokemon %>% filter(Secondary_type=="Fighting")->Fire_Fighting_pokemon
range(Fire_Fighting_pokemon$Attack)
Fire_Fighting_pokemon %>% filter(Attack==160)->My_Fire_pokemon
View(My_Fire_pokemon)

#My three pokemon 
rbind(My_Fire_pokemon,My_Water_pokemon,My_Grass_pokemon)->My_Pokemons

#linear regression is uesd on dataset to see what are factors that
#determine pokemon attack

#spliting data
sample.split(pokemon$Attack,SplitRatio = 0.65)->split_index
subset(pokemon,split_index==T)->train1
subset(pokemon,split_index==F)->test1

#Building Model1(attack v/s defence)

lm(Attack~Defense,data=train1)->mod_regress
predict(mod_regress,test1)->result_regress
cbind(Actual=test1$Attack,Predicted=result_regress)->Final_Data
as.data.frame(Final_Data)->Final_Data#as this a matrix
View(Final_Data)

#Finding error
(Final_Data$Actual-Final_Data$Predicted)->error
cbind(Final_Data,error)->Final_Data
rmse1<-sqrt(mean(Final_Data$error^2))
rmse1

#Building Model1(attack w.r.t defence,speed,hp)
lm(Attack~Defense+Speed+HP,data=train1)->mod_regress2
predict(mod_regress2,test1)->result_regress2
cbind(Actual=test1$Attack,Predicted=result_regress2)->Final_Data2
as.data.frame(Final_Data2)->Final_Data2
View(Final_Data2)

(Final_Data2$Actual-Final_Data2$Predicted)->error2
cbind(Final_Data2,error2)->Final_Data2
rmse2<-sqrt(mean(Final_Data2$error^2))
rmse2

#As obsereved the 2nd linear regression model is better than the 1st one

#Classification wheather a pokemon is legendary or not
#----------------------------------------

library(caTools)
sample.split(pokemon$Legendary,SplitRatio = 0.65)->split_values
subset(pokemon,split_values==T)->train
subset(pokemon,split_values==F)->test

nrow(train)
nrow(test)

#Decision tree
#--------------------------

library(rpart)

#Building model
rpart(Legendary~. ,data=train)->mod1
predict(mod1,test,type="class")->result1
table(test$Legendary,result1)

#Evaluating model
library(caret)
confusionMatrix(table(test$Legendary,result1))

#Building Model
rpart(Legendary~Attack+Defense+Speed ,data=train)->mod2
predict(mod2,test,type="class")->result2
table(test$Legendary,result2)

confusionMatrix(table(test$Legendary,result2))

#1st model is better than 2nd