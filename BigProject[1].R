install.packages(c("MASS","dplyr","ggplot2","cluster","arules","arulesViz"))
library(MASS)
library(dplyr)
library(ggplot2)
library(cluster)
library(arules)
library(arulesViz)

#create a sample dataset
set.seed(123)
df<-data.frame(
  Gender = sample(c("Male","Female"),50,replace = TRUE),
  Age = sample(18:40,50,replace = TRUE),
  Score = round(rnorm(50,70,10),2),
  Department = sample(c("CS","Math","Physics"),50,replace = TRUE),
  Pass = sample(c(1,0),50,replace = TRUE) # 1 = Pass, 0 = Fail 
)
df
tail(df)

#1.Chi Square test
chisq_test_result<- chisq.test(table(df$Gender,df$Department))
print(chisq_test_result)

#2.T-Test
t_test_result<-t.test(Score~Gender,data = df)
print(t_test_result)

#3.ANOVA
anova_result<-aov(Score~Department,data = df)
print(summary(anova_result))

#4.Correlation Analysis
correlation_result<-cor.test(df$Age,df$Score)
print(correlation_result)

#5.Maximum Likelihood Estimation(MLE)
library(MASS)
mle_result<-fitdistr(df$Score,"normal")
print(mle_result)

#6.Regression Analysis
regression_model<-lm(Score~Age + Gender,data = df)
print(summary(regression_model))

#7.Logistic Regression 
logistic_model<-glm(Pass~Age+Gender,data=df,family=binomial)
print(summary(logistic_model))

#8.K-means clustering
set.seed(123)
kmeans_result<-kmeans(df[,c("Age","Score")],centers = 3)
print(kmeans_result)

#9.plot clusters
library(ggplot2)
df$cluster<-as.factor(kmeans_result$cluster)
ggplot(df,aes(x = Age,y = Score,color = cluster)) + geom_point() + ggtitle("K-Means Clustering")

#10.Association Rules Analysis
library(arules)
df_assoc <- df[,c("Gender", "Department")]
df_assoc[] <- lapply(df_assoc, as.factor)
trans <- as(df_assoc, "transactions")
summary(trans)

#Adjusted thresholds for apriori algorithm
library(arulesViz)
rules<-apriori(trans,parameter = list(supp = 0.05,conf = 0.06))
if(length(rules)>0){
  inspect(rules)
  #Visualize the rules
  plot(rules,method="graph",engine = "htmlwidget")
}else{
  cat("No rules generated...! try adjusting support and confidence values.\n")
}




