library(ggplot2)

mydf = read.csv(file= "mydf3.csv")

head(mydf)

#line plot 

image_result = ggplot(mydf, aes(x=result, y=weight)) 

image_result + geom_point()

image_result  + geom_point() + geom_smooth(method="lm")

image_result  + geom_point() + geom_smooth(method="lm") + xlim(c(-1, 2)) + ylim(c(0, 3)) 

image_result + geom_point(aes(col=gender), size=3) + geom_smooth(method="lm", col="firebrick", size=2)

image_result + geom_point(aes(col=gender), size=3) + 
  geom_smooth(method="lm", col="firebrick", size=2)+
  scale_color_manual(values=c("grey29","lightcoral"))

image_result + geom_point(aes(col=gender), size=3) + 
  geom_smooth(method="lm", col="firebrick", size=2)+
  labs(x = "gaming", y = "happiness") + guides(color=guide_legend(title="G")) +
  scale_color_manual(values=c("grey29","lightcoral"))+ theme_bw()


#######################
image_weight = ggplot(mydf, aes(x=condition, y=result)) 

image_weight + geom_boxplot(aes(col = gender))


library(psych)
describeBy(mydf, condition)

### create new dataset if necessary
#use weight as the DV

df_forplot<-data.frame(mean=c(59.07,61.07,59.18),sd=c(4.95,4.81,4.61),Condition=as.factor(c("A","B","C")))

ggplot(df_forplot,aes(x=Condition))+
geom_boxplot(aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd, col =Condition ),stat="identity")



#histogram
image_result3<-ggplot(mydf, aes(x=weight, fill=gender, color=gender)) +
  geom_histogram(position="identity", alpha=0.5)

image_result3

library(plyr)
mu <- ddply(mydf, "gender", summarise, grp.mean=mean(weight))
head(mu)

image_result3+geom_vline(data=mu, aes(xintercept=grp.mean, color=gender),
             linetype="dashed")



############## more complex plot
library(data.table)
dat <- read.table(text = "utilitarian, hedonic
                  1   41.71  47.85
                  2   20.49  27.30 
                  3   37.80  24.85",
                  sep = "",header = TRUE)
rownames(dat)= c("liking", "disliking", "neutral")

datm <- melt(cbind(dat, ind = rownames(dat)), id.vars = c('ind'))

library(scales)
ggplot(data= datm,aes(x = variable, y = value, fill = ind)) + 
  geom_bar(position = "fill",stat = "identity", width=0.3) +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format()) + 
  scale_fill_manual(values=c("grey70", "grey40","black"))+
  labs(x = "", y = "Attitude Proportion")+
  theme(text = element_text(size=15), panel.background = element_rect(fill = "white"), axis.line = element_line(size = .5, colour = "black"),
        axis.text = element_text(colour = "black"), axis.ticks = element_line(colour='black'), legend.title=element_blank()) 


