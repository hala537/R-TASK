#read dataset
mydata <- read.csv("G4_howell.csv")

View(mydata)

#First Of all we will make data cleaning

# Re-coding the sex field to Male and Female
mydata$gender[mydata$sex=="F"] = "Female"
mydata$gender[mydata$sex=="M"]  = "Male"

# display all rows that have null values
mydata[!complete.cases(mydata) , ]


# Replace all null values in weight based on mean of weight to men to all men

mydata$weight = as.numeric(mydata$weight) # to make them numeric for getting mean
male_mean<-mean(mydata[mydata$gender=="Male" , 'weight' ], na.rm=T)
mydata[is.na(mydata$weight) & mydata$gender == "Male" , 'weight'] = male_mean


#using mice function
mydata[ ! complete.cases(mydata), ]

pre.imp <- mice(mydata , m = 5 , meth = c("pmm","","pmm","pmm","") , maxit = 1)

pre.imp$imp
mydatanew<-complete(pre.imp,4)
mydata

# and the same thing for female null weight

female_mean<-mean(mydata[mydata$gender=="Female" , 'weight' ], na.rm=T)
mydata[is.na(mydata$weight) & mydata$gender == "Female" , 'weight'] = female_mean


#Re_code another variable like height
mydata$dwarf_or_normal[mydata$height <= 150] <- "dwarf"
mydata$dwarf_or_normal[mydata$height > 150] <- "normal"


#use (if else) for Re_coding the weight

weight_mean = mean(as.numeric(mydata$weight))
mydata$fat_or_not = as.factor(ifelse(mydata$weight > weight_mean,"Fat" ,"Normal"))


#Re_code previous code

mydata$fat01[mydata$fat_or_not=="Normal"]= 0
mydata$fat01[mydata$fat_or_not=="Fat"]= 1
mydata$fat01<-as.factor(mydata$fat01)



# show  mean of fat or normal people

mean_fat<-mean(mydata$fat01==0)
mean_normal<-mean(mydata$fat01==1)
mean_fat
mean_normal

# get only fat people as a subset

fat_people = mydata[mydata$fat01 == 1 , ]

# get only normal people in height as a subset

tall_people<-mydata[mydata$height > 150 , ]

# get only fat and short people in height as a subset

fat_and_short_people <- mydata[mydata$fat01 == 1 & mydata$height <= 150 , ]


# display the data ordered ascending based on to 2 variables

ordered_data <- mydata[order(mydata$height ,mydata$weight) , ]

View(ordered_data)

# display the first 10 rows

head_data<-head(mydata ,10)

# display the the last 50 rows

tail_data<-tail(mydata ,10)


# Data visualization

library(ggplot2)

# displaying the effect of height on weight using scatter plot
#(co_relation)

fig_1 <-ggplot(mydata , aes(x=weight  , y= height))
fig_1 + geom_point() + ggtitle("Co_relation between the height and weight")


# displaying the effect of height on weight colored by the groups of age range using scatterplot
fig_2 = ggplot(mydata , aes(weight , height))
fig_2 + geom_point(aes(color=age)) + stat_smooth(se=FALSE)  


# displaying distribution of weight using histogram
fig_3 <- ggplot(mydata , aes(weight))
fig_3 +  geom_histogram(binwidth = 8)
fig_3 +  geom_histogram(fill = "green")+ ggtitle("weight distribution")+labs(x="weight" , y="Frequency")

# displaying distribution of height using histogram
fig_4 <-ggplot(mydata , aes(height))
fig_4 + geom_histogram(binwidth = 8)
fig_4 + geom_histogram(fill = "red")+ ggtitle("Height distribution") 

# Making Summary For fat01 to gender and weight groups using Bar chart

fig5<-ggplot(mydata , aes(x=fat01  ,fill= gender))
fig5 +geom_bar()+labs(y=" Fat Count" ,title="Weight category rate")
fig5 +geom_bar() +theme_light()+facet_wrap(~weight)
















