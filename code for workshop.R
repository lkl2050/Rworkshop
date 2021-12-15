
#set working directory 
getwd()
setwd("/Users/cairo/Documents/r_workshop")

?getwd  #always use ? or ?? to check the function

######Generate data 
sample_size = 200

condition = sample(x = 1:3, size  = sample_size, replace = TRUE)  #numeric variable  
condition = sample(x = c("A", "B", "C"), size  = sample_size, replace = TRUE) #character/string variable
condition

class(condition)
class(condition[0])

condition = as.factor(condition) #factor variable 
class(condition)

result = rnorm(n = sample_size, mean = 0, sd = 1)
result

gender =  sample(x = c("Male", "Female"), size  = sample_size, replace = TRUE)
age = sample(18:22, size = sample_size,  replace = TRUE)



##  dataframe is the basic "table" in R 
df <- data.frame(condition, result, gender, age) #generate dataframe from existing varaibles
head(df) 
View(df)

df$weight = rnorm(n = sample_size, mean = 60, sd = 5) #add new column to dataframe
head(df) 

library(psych) #descriptives 
describe(df)
describeBy(df, gender)

#subset by index
df[1:10,]
df[,1:10]

#subset by name
df["result"]
df$result
df[c("result", "gender")]

df_male <- subset(df, gender== "Male")
df_male_20 <- subset(df, age >= 20 & gender== "Male",select=c(result, gender))

##change values in df
df$result = df$result * 5


######
write.csv(df,'mydf3.csv')  #create fake dataset can often help you debug

mydf = read.csv(file= "mydf2.csv")
head(mydf)

#often need manual data clean before you import to R

class(mydf) #always keep track of the type of your object, half of bug come from this source

install.packages("readxl")
library(readxl)
mydf = read_excel("mydf2.csv", sheet = 1)
class(mydf)
head(mydf)

###########t-test and ANOVA. Cross check your results with the software you are familiar with
ttest_result = t.test(result ~ gender, data = mydf) #between-subject comparison

ttest_result

library(yarrr)
apa(ttest_result)

head(mydf)

t.test(mydf$weight, mydf$result, paired = T) #within-subject comparison

cor.test(mydf$weight, mydf$result) #correlation test 


###
library(car)
reg_mod <- lm(result ~ condition * gender, data = mydf)
summary(reg_mod)
Anova(reg_mod)


options(contrasts = c("contr.helmert", "contr.poly"))
reg_mod = lm(result ~ condition * gender, data = mydf)
Anova(reg_mod,type=3)

#############
###Cronbach's alpha
library(psych)
d <- data.frame(mydf$result, mydf$weight)
psych::alpha(d)

######### effect sizes
library(effectsize)

d_result = cohens_d(result ~ gender, data = mydf)
d_result
convert_d_to_r(d = d_result)
interpret_d(d = d_result, rules = "cohen1988")

#########Mediation with Hayes's PROCESS
mydf = read_excel("my_test_df.xlsx", sheet = 1)

process(data = mydf, y = "result", x = "gender", m ="age", model = 4)

class(mydf$gender)
mydf$gender = ifelse(mydf$gender=="Male", 1, 0) #

