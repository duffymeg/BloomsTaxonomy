#=============================================================================================================#
# Script created by Meghan Duffy, duffymeg@umich.edu
# Script created in version R 3.1.2 
# This script is for analyzing Bloom's taxonomy levels of exams from Fall 2012 & Fall 2014
# Meg and Cindee Giffen assigned Bloom's levels to each exam question, working from key 1 for
# each exam. The numbers correspond to:
# 1 = Remember; basic recall
# 2 = Understand; e.g., given a new example, be able to recognize a concept covered in class
# 3 = Apply; e.g., recognizing what equation needed to be used, figuring out what information 
#      from the question goes into that equation, and correctly solving
# 4 = Analyze; give students completely new data that they have to interpret in light of the 
#      concepts covered; have students make predictions (either verbally or graphically) based on 
#      a novel scenario
#=============================================================================================================#

# Set working directory
#use line below if working on pc
setwd("C:/Users/duffymeg/Box Sync/Teaching/Michigan/IntroBio/ExamAnalyses/BloomsTaxonomyAnalysis")
#use line below if working on mac
# setwd("~/Box Sync/Teaching/Michigan/IntroBio/ExamAnalyses/BloomsTaxonomyAnalysis")

# load data
bloomsdata <- read.csv("BloomsTaxonomy.csv", header=TRUE, na.strings="?", dec=".", strip.white=TRUE)

#summarize Bloom's levels by exam
library(dplyr)
bloomsavg <- bloomsdata %>% group_by(Semester,Exam) %>% summarise(avg=mean(BloomsLevel)) %>% as.data.frame

#plot
library(ggplot2)
p <- ggplot(bloomsavg, aes(x=Semester, y=avg)) + 
  geom_boxplot() + theme_bw() +
  labs(x="Semester", y=expression(paste("Mean Bloom's level"))) + theme(
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )
p

#redoing to drop x-axis labels for eventual multiplot
p <- ggplot(bloomsavg, aes(x=Semester, y=avg)) + 
  geom_boxplot() + theme_bw() +
  labs(x="Semester", y=expression(paste("Mean Bloom's level"))) + theme(
    axis.title.y = element_text(size=18),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=14)
  )
p

#compare Bloom's levels from Fall 2012 to Fall 2014
t.test(bloomsavg$avg~bloomsavg$Semester)

#add text with t-test result to figure
p1 <- p + annotate("text",x=1,y=2.3, label = "t = 3.01, p = 0.025", size=8)
#for above, would be better if could figure out a way to automatically link text on figure
#with results of stats, in case need to rerun on updated data file


#now load exam mean grades
examgrades <- read.csv("ExamGradesFall20122014.csv", header=TRUE, na.strings="?", dec=".", strip.white=TRUE)

#plot exam grades
q <- ggplot(examgrades, aes(x=Semester, y=MeanGrade)) + 
  geom_boxplot() + theme_bw() +
  labs(x="Semester", y=expression(paste("Mean exam grade"))) + theme(
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )
q

#compare exam grades from Fall 2012 to Fall 2014
t.test(examgrades$MeanGrade~examgrades$Semester)

#add text with t-test result to figure
q1 <- q + annotate("text",x=1,y=85.0, label = "t = 0.375, p = 0.72", size=8)
#for above, would be better if could figure out a way to automatically link text on figure
#with results of stats, in case need to rerun on updated data file

#plot together:
library(grid)
grid.draw(rbind(ggplotGrob(p1),ggplotGrob(q1),size="last"))
