setwd("C:/Users/Reeshav/Desktop/INDIAMART AUDIO/") 


bf_train=read.csv("bank-full_train.csv",sep=",",header=T)
bf_test=read.csv("bank-full_test.csv",sep=",",header=T)
library(dplyr)
glimpse(all)

bf_test$y= NA
bf_train$data = 'train'
bf_test$data = 'test'

all= rbind(bf_train,bf_test)

apply(all,2,function(x) length(unique(x)))

all$y[all$y == "no"] <- 0
all$y[all$y== "yes"] <- 1
all$y <- as.integer(all$y)
class(all$y)

all$default[all$default == "no"] <- 0
all$default[all$default == "yes"] <- 1
all$default <- as.integer(all$default)
class(all$default)

CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
} 

all=all %>% 
  select(-ID,-poutcome)

head(all)
for_dummy_vars=c('job','marital','education','housing','loan','contact','month')

for(var in for_dummy_vars){
  all=CreateDummies(all,var,100)
}

for(col in names(all)){
  
  if(sum(is.na(all[,col]))>0 & !(col %in% c("data","y"))){
    
    all[is.na(all[,col]),col]=mean(all[all$data=='train',col],na.rm=T)
  }
  
}

head(all)

bf_train = all %>% filter(data == 'train') %>% select(-data) 
bf_test= all %>% filter(data == 'test') %>% select(-y, -data) 

any(is.na(bf_train))
any(is.na(bf_test))

glimpse(bf_train)
glimpse(bf_test)

################################################################### USING RANDOM SAMPLING

set.seed(123)
s <- sample(1:nrow(bf_train), 0.8*nrow(bf_train))
bf_train_1 <- bf_train[s,]
bf_train_2 <- bf_train[-s,]

####################################################################

library(car)
library(caret)

for_vif <- lm(y~. -month_may -job_management -education_secondary -contact_unknown -job_technician -job_unemployed -default -job_entrepreneur 
              -job_services  -education_primary -marital_married, data = bf_train_1 )
sort(vif(for_vif), decreasing = T)
summary(for_vif)

###################################################################################

log_fit <- glm(y~. -month_may -job_management -education_secondary -contact_unknown -job_technician -job_unemployed -default -job_entrepreneur 
               -job_services -job_self_employed -education_primary -month_aug, data = bf_train_1, family = "binomial")

log_fit_1 <- glm(y ~ age + balance + day + duration + campaign + pdays + previous + 
                   job_student + job_housemaid + job_retired + job_admin. + 
                   job_blue_collar + marital_single + education_tertiary + housing_yes + 
                   loan_no + contact_cellular + month_mar + month_sep + month_oct + 
                   month_jan + month_feb + month_apr + month_nov + month_jun + 
                   month_jul, data = bf_train_1, family = "binomial")

summary(log_fit)

bf_train$score <- predict(log_fit, newdata = bf_train, type = "response")

glimpse(bf_train)

library(ggplot2)
ggplot(bf_train,aes(y=y,x=score,color=factor(y)))+
  geom_point()+geom_jitter()


cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)
for (i in cutoffs){
  predicted=as.numeric(bf_train$score>i)
  TP=sum(predicted==1 & bf_train$y==1)
  FP=sum(predicted==1 & bf_train$y==0)
  FN=sum(predicted==0 & bf_train$y==1)
  TN=sum(predicted==0 & bf_train$y==0)
  cutoff_data=rbind(cutoff_data,c(i,TP,FP,FN,TN))
}
View(cutoff_data)
## lets remove the dummy data cotaining top row in data frame cutoff_data
cutoff_data=cutoff_data[-1,]


cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP, #total positives and negatives
         Sn=TP/P, #sensitivity
         Sp=TN/N, #specificity
         KS=abs((TP/P)-(FP/N)),
         Accuracy=(TP+TN)/(P+N),
         Lift=(TP/P)/((TP+FP)/(P+N)),
         Precision=TP/(TP+FP),
         Recall=TP/P
  ) %>% 
  select(-P,-N)

KS_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
KS_cutoff

bf_test$score <- predict(log_fit, newdata = bf_test, type = "response")


bf_test$y= ifelse(bf_test$score > KS_cutoff, 1,0)

table(bf_test$left)

bf_test$leftfinal=factor(bf_test$left,levels = c(0,1),labels=c("no","yes"))

table(bf_test$leftfinal)
bf_test$left

write.csv(bf_test$y,file = "final_sub.csv", row.names = F)
