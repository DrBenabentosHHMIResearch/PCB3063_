# Genetics Analysis 
file.choose()

PCB <-read.csv("/Users/claudiabarnett/Downloads/genetics roaster.csv",skip=1)
nrow(PCB) #number of rows
ncol(PCB) #number of colums
str(PCB) #structure
ls(PCB)

# RUN LIBRARIES
library(car)
library(psych)
library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(dplyr)
library(base) #ifelse
library(scales)
library(reshape2)
library(viridis) #best. color. palette. evar.
library(lattice)

#################################################################
#################################################################

#CLEAN UP SPREADSHEET
#Remove LAs
table(PCB$Name)
table(PCB$Name, PCB$Term)
#Camila Granados Cifuentes, De Etta Kay Mills, Helena Schmidtmayerova, Heather Bracken-grissom, 
#   Steven Oberbauer, Matthew DeGennaro, Martin Tracey, Manuel Barbieri, Jose Eirin-Lopez, 

PCB$Acad.Prog <- NULL                   
PCB$Acad.Plan <- NULL
PCB$Compl.Term <- NULL  
PCB$Acad.Plan.1 <- NULL
PCB$Acad.Prog.1 <- NULL
PCB$Admit.Term <- NULL

is.na(PCB$Max.Score) <- !PCB$Max.Score #changes SAT scores 0 to NA
table(PCB$Max.Score, useNA="always")


## CLEANING DUPLICATES DUE TO MULTIPLE INSTRUCTORS
#Remove all of the Learning Assistants
PCBsubset <- subset(PCB, !(Name=="Adrian Abreu"|Name=="Bria Bradshaw"|Name=="Briana Cervantes"|Name=="Catalina Garzon"|
                              Name=="Charles Golightly III"|Name=="Christina Burns"|Name=="Enelys Moreno Hernandez"|
                              Name=="Gabriel De La Iglesia"|Name=="Gabrielle Edmund"|Name=="Hans Lapica"|
                              Name=="Hilma Gallegos"|Name=="Jany Nunez"|Name=="Jeffrey Valencia"|
                              Name=="Jennifer Carvalho"|Name=="Jessica Rodriguez Carrasco"|
                              Name=="Jevon Rhule"|Name=="Juan Bedoya"|Name=="Jumana Afaghani"|
                              Name=="Karla Gomez"|Name=="Kevin Fundora"|Name=="Lauren Peterson"|
                              Name=="Najm Shaikh"|Name=="Nashua Wisdom"|Name=="Priscilla Polo"|
                              Name=="Roberto Martinez Pena"|Name=="Samantha Powers"|
                              Name=="Stephanie Bernard"|Name=="Valeria Saldana"|Name=="Victor Lopez"))

#Remove Jose Alberte - only PLTL (didn't teach any on his own)
PCBsubset <- subset(PCBsubset, Name!="Jose Alberte")

#Remove Steven Oberbauer - co-taught with Martin Tracey but didn't teach any on his own
PCBsubset <- subset(PCBsubset, Name!="Steven Oberbauer")


length(unique(PCBsubset$ID)) #7704 unique students (same as full sample; PCB)
length(unique(PCB$ID)) #7704 unique students

PCBsubset2 <- unique(PCBsubset2)

#Check that there are no duplicates
PCBsubset.r <- PCB[(duplicated(PCBsubset[c("Term","ID","Section")])|
                            duplicated(PCBsubset[c("Term","ID","Section")], fromLast=TRUE)),] #0 rows - no duplicates

#write new csv with non-duplicate rows
#write.csv(PCBsubset, "/Users/claudiabarnett/Downloads/genetics_nondup_170707.csv")


#################################################################
#################################################################
#CREATE VARIABLES

#Create a variable by semester (Fall, Spring, Summer)
table(PCBsubset$Term)

PCBsubset$Semester <- with(PCBsubset, ifelse((Term=="1108"| Term=="1118"| Term=="1128"|
                                                Term=="1138"| Term=="1148"| Term=="1158"|
                                                Term=="1168"), "Fall", 
                                             (ifelse(Term=="1111"| Term=="1121"|
                                                       Term=="1131"| Term=="1141"| Term=="1151"|
                                                       Term=="1161"|Term=="1171", "Spring", "Summer"))))

table(PCBsubset$Term, PCBsubset$Semester)

#Create a variable for LAs for Helena
#subset Helena
table(PCBsubset$Name, PCBsubset$Term)
#create a variable in the new subset

#################################################################
#################################################################

##Find the percent of grades by a specific term 
#Percent of As, Bs, Cs, etc. 
#command ddplyr, summarise - (library plyr)

table(PCBsubset$Grade)
#Graph for all the Terms together
df1 <- ddply(PCBsubset, .(), summarise, 
              percA = sum(PCBsubset$Grade=="A"|PCBsubset$Grade=="A-", is.na=TRUE)/nrow(PCBsubset)*100,
              percB = sum(PCBsubset$Grade=="B"|PCBsubset$Grade=="B-"|PCBsubset$Grade=="B+", is.na=TRUE)/nrow(PCBsubset)*100,
              percC = sum(PCBsubset$Grade=="C"|PCBsubset$Grade=="C+",is.na=TRUE)/nrow(PCBsubset)*100,
              percD = sum(PCBsubset$Grade=="D"|PCBsubset$Grade=="D-"|PCBsubset$Grade=="D+"|PCBsubset$Grade=="C-", is.na=TRUE)/nrow(PCBsubset)*100,
              percDR = sum(PCBsubset$Grade=="DR", is.na=TRUE)/nrow(PCBsubset)*100)
              
df2 <- melt(df1, c("percA", "percB","percC","percD","percDR"), id.vars=.())
bargraph1 <- ggplot(df2, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "DRs"))
print(bargraph1)

#Graph by Term
PCBsubset$Term <- as.factor(PCBsubset$Term)
str(PCBsubset$Term)

df1 <- ddply(PCBsubset, "Term", summarise, 
             percA = sum(PCBsubset$Grade=="A"|PCBsubset$Grade=="A-", is.na=TRUE)/nrow(PCBsubset)*100,
             percB = sum(PCBsubset$Grade=="B"|PCBsubset$Grade=="B-"|PCBsubset$Grade=="B+", is.na=TRUE)/nrow(PCBsubset)*100,
             percC = sum(PCBsubset$Grade=="C"|PCBsubset$Grade=="C+",is.na=TRUE)/nrow(PCBsubset)*100,
             percD = sum(PCBsubset$Grade=="D"|PCBsubset$Grade=="D-"|PCBsubset$Grade=="D+"|PCBsubset$Grade=="C-", is.na=TRUE)/nrow(PCBsubset)*100,
             percDR = sum(PCBsubset$Grade=="DR", is.na=TRUE)/nrow(PCBsubset)*100)

df2 <- melt(df1, c("percA", "percB","percC","percD","percDR"), id.vars=c("Term"))
ggplot(df2, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "DRs"))


#Graph by Instructor
PCBsubset$Term <- as.factor(PCBsubset$Term)
str(PCBsubset$Term)

## Why does it standardized everything for all semesters ## 
df1 <- ddply(PCBsubset, "Name", summarise, 
             percA = sum(PCBsubset$Grade=="A"|PCBsubset$Grade=="A-", is.na=TRUE)/nrow(PCBsubset)*100,
             percB = sum(PCBsubset$Grade=="B"|PCBsubset$Grade=="B-"|PCBsubset$Grade=="B+", is.na=TRUE)/nrow(PCBsubset)*100,
             percC = sum(PCBsubset$Grade=="C"|PCBsubset$Grade=="C+",is.na=TRUE)/nrow(PCBsubset)*100,
             percD = sum(PCBsubset$Grade=="D"|PCBsubset$Grade=="D-"|PCBsubset$Grade=="D+"|PCBsubset$Grade=="C-", is.na=TRUE)/nrow(PCBsubset)*100,
             percF = sum(PCBsubset$Grade=="F", is.na=TRUE)/nrow(PCBsubset)*100,
             percDR = sum(PCBsubset$Grade=="DR", is.na=TRUE)/nrow(PCBsubset)*100,
             pass = percA+percB+percC,
             fail = percD, percF, percDR)

df2 <- melt(df1, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
ggplot(df2, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)

PCBHS <- subset(PCBsubset, !(Name=="Camila Granados Cifuentes"|Name=="Case Okubo"|Name=="De Etta Kay Mills"|Name=="Heather Bracken-grissom"|Name=="Jeffrey Wells"|Name=="Jose Eirin-Lopez"|Name=="Manuel Barbieri"|Name=="Martin Tracey"|Name=="Matthew DeGennaro"))
table(PCBHS$Name)

Grades_PCBHS1 <- ddply(PCBHS, "Name", summarise, 
             percA = sum(PCBHS$Grade=="A"|PCBHS$Grade=="A-", is.na=TRUE)/nrow(PCBHS)*100,
             percB = sum(PCBHS$Grade=="B"|PCBHS$Grade=="B-"|PCBHS$Grade=="B+", is.na=TRUE)/nrow(PCBHS)*100,
             percC = sum(PCBHS$Grade=="C"|PCBHS$Grade=="C+",is.na=TRUE)/nrow(PCBHS)*100,
             percD = sum(PCBHS$Grade=="D"|PCBHS$Grade=="D-"|PCBHS$Grade=="D+"|PCBHS$Grade=="C-", is.na=TRUE)/nrow(PCBHS)*100,
             percF = sum(PCBHS$Grade=="F", is.na=TRUE)/nrow(PCBHS)*100,
             percDR = sum(PCBHS$Grade=="DR", is.na=TRUE)/nrow(PCBHS)*100,
             pass = percA+percB+percC,
             fail = percD, percF, percDR)

Grades_PCBHS2 <- melt(Grades_PCBHS1, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## Table for Helena all terms grade distribution
HSAllTermGD <- ggplot(Grades_PCBHS2, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(HSAllTermGD)

## Repeat For All terms## 

HSGD1108<- subset(PCBHS, Term=="1108")

HSGD1108 <- ddply(HSGD1108, "Term", summarise, 
                       percA = sum(HSGD1108$Grade=="A"|HSGD1108$Grade=="A-", is.na=TRUE)/nrow(HSGD1108)*100,
                       percB = sum(HSGD1108$Grade=="B"|HSGD1108$Grade=="B-"|HSGD1108$Grade=="B+", is.na=TRUE)/nrow(HSGD1108)*100,
                       percC = sum(HSGD1108$Grade=="C"|HSGD1108$Grade=="C+",is.na=TRUE)/nrow(HSGD1108)*100,
                       percD = sum(HSGD1108$Grade=="D"|HSGD1108$Grade=="D-"|HSGD1108$Grade=="D+"|HSGD1108$Grade=="C-", is.na=TRUE)/nrow(HSGD1108)*100,
                       percF = sum(HSGD1108$Grade=="F", is.na=TRUE)/nrow(HSGD1108)*100,
                       percDR = sum(HSGD1108$Grade=="DR", is.na=TRUE)/nrow(HSGD1108)*100,
                       pass = percA+percB+percC,
                       fail = percD, percF, percDR)

HSGD1108 <- melt(HSGD1108, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1108Plot <- ggplot(HSGD1108, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1108Plot)


################################################################
HSGD1111<- subset(PCBHS, Term=="1111")

HSGD1111 <- ddply(HSGD1111, "Term", summarise, 
                  percA = sum(HSGD1111$Grade=="A"|HSGD1111$Grade=="A-", is.na=TRUE)/nrow(HSGD1111)*100,
                  percB = sum(HSGD1111$Grade=="B"|HSGD1111$Grade=="B-"|HSGD1111$Grade=="B+", is.na=TRUE)/nrow(HSGD1111)*100,
                  percC = sum(HSGD1111$Grade=="C"|HSGD1111$Grade=="C+",is.na=TRUE)/nrow(HSGD1111)*100,
                  percD = sum(HSGD1111$Grade=="D"|HSGD1111$Grade=="D-"|HSGD1111$Grade=="D+"|HSGD1111$Grade=="C-", is.na=TRUE)/nrow(HSGD1111)*100,
                  percF = sum(HSGD1111$Grade=="F", is.na=TRUE)/nrow(HSGD1111)*100,
                  percDR = sum(HSGD1111$Grade=="DR", is.na=TRUE)/nrow(HSGD1111)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1111 <- melt(HSGD1111, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1111Plot <- ggplot(HSGD1111, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1111Plot)


################################################################

HSGD1118<- subset(PCBHS, Term=="1118")

HSGD1118 <- ddply(HSGD1118, "Term", summarise, 
                  percA = sum(HSGD1118$Grade=="A"|HSGD1118$Grade=="A-", is.na=TRUE)/nrow(HSGD1118)*100,
                  percB = sum(HSGD1118$Grade=="B"|HSGD1118$Grade=="B-"|HSGD1118$Grade=="B+", is.na=TRUE)/nrow(HSGD1118)*100,
                  percC = sum(HSGD1118$Grade=="C"|HSGD1118$Grade=="C+",is.na=TRUE)/nrow(HSGD1118)*100,
                  percD = sum(HSGD1118$Grade=="D"|HSGD1118$Grade=="D-"|HSGD1118$Grade=="D+"|HSGD1118$Grade=="C-", is.na=TRUE)/nrow(HSGD1118)*100,
                  percF = sum(HSGD1118$Grade=="F", is.na=TRUE)/nrow(HSGD1118)*100,
                  percDR = sum(HSGD1118$Grade=="DR", is.na=TRUE)/nrow(HSGD1118)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1118 <- melt(HSGD1118, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1118Plot <- ggplot(HSGD1118, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1118Plot)


################################################################

HSGD1121<- subset(PCBHS, Term=="1121")

HSGD1121 <- ddply(HSGD1121, "Term", summarise, 
                  percA = sum(HSGD1121$Grade=="A"|HSGD1121$Grade=="A-", is.na=TRUE)/nrow(HSGD1121)*100,
                  percB = sum(HSGD1121$Grade=="B"|HSGD1121$Grade=="B-"|HSGD1121$Grade=="B+", is.na=TRUE)/nrow(HSGD1121)*100,
                  percC = sum(HSGD1121$Grade=="C"|HSGD1121$Grade=="C+",is.na=TRUE)/nrow(HSGD1121)*100,
                  percD = sum(HSGD1121$Grade=="D"|HSGD1121$Grade=="D-"|HSGD1121$Grade=="D+"|HSGD1121$Grade=="C-", is.na=TRUE)/nrow(HSGD1121)*100,
                  percF = sum(HSGD1121$Grade=="F", is.na=TRUE)/nrow(HSGD1121)*100,
                  percDR = sum(HSGD1121$Grade=="DR", is.na=TRUE)/nrow(HSGD1121)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1121 <- melt(HSGD1121, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## 3rd table##
HSGD1121Plot <- ggplot(HSGD1121, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1121Plot)

################################################################

HSGD1128<- subset(PCBHS, Term=="1128")

HSGD1128 <- ddply(HSGD1128, "Term", summarise, 
                  percA = sum(HSGD1128$Grade=="A"|HSGD1128$Grade=="A-", is.na=TRUE)/nrow(HSGD1128)*100,
                  percB = sum(HSGD1128$Grade=="B"|HSGD1128$Grade=="B-"|HSGD1128$Grade=="B+", is.na=TRUE)/nrow(HSGD1128)*100,
                  percC = sum(HSGD1128$Grade=="C"|HSGD1128$Grade=="C+",is.na=TRUE)/nrow(HSGD1128)*100,
                  percD = sum(HSGD1128$Grade=="D"|HSGD1128$Grade=="D-"|HSGD1128$Grade=="D+"|HSGD1128$Grade=="C-", is.na=TRUE)/nrow(HSGD1128)*100,
                  percF = sum(HSGD1128$Grade=="F", is.na=TRUE)/nrow(HSGD1128)*100,
                  percDR = sum(HSGD1128$Grade=="DR", is.na=TRUE)/nrow(HSGD1128)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1128 <- melt(HSGD1128, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1128Plot <- ggplot(HSGD1128, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1128Plot)

################################################################

HSGD1131<- subset(PCBHS, Term=="1131")

HSGD1131 <- ddply(HSGD1131, "Term", summarise, 
                  percA = sum(HSGD1131$Grade=="A"|HSGD1131$Grade=="A-", is.na=TRUE)/nrow(HSGD1131)*100,
                  percB = sum(HSGD1131$Grade=="B"|HSGD1131$Grade=="B-"|HSGD1131$Grade=="B+", is.na=TRUE)/nrow(HSGD1131)*100,
                  percC = sum(HSGD1131$Grade=="C"|HSGD1131$Grade=="C+",is.na=TRUE)/nrow(HSGD1131)*100,
                  percD = sum(HSGD1131$Grade=="D"|HSGD1131$Grade=="D-"|HSGD1131$Grade=="D+"|HSGD1131$Grade=="C-", is.na=TRUE)/nrow(HSGD1131)*100,
                  percF = sum(HSGD1131$Grade=="F", is.na=TRUE)/nrow(HSGD1131)*100,
                  percDR = sum(HSGD1131$Grade=="DR", is.na=TRUE)/nrow(HSGD1131)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1131 <- melt(HSGD1131, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1131Plot <- ggplot(HSGD1131, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1131Plot)

################################################################
HSGD1138<- subset(PCBHS, Term=="1138")

HSGD1138 <- ddply(HSGD1138, "Term", summarise, 
                  percA = sum(HSGD1138$Grade=="A"|HSGD1138$Grade=="A-", is.na=TRUE)/nrow(HSGD1138)*100,
                  percB = sum(HSGD1138$Grade=="B"|HSGD1138$Grade=="B-"|HSGD1138$Grade=="B+", is.na=TRUE)/nrow(HSGD1138)*100,
                  percC = sum(HSGD1138$Grade=="C"|HSGD1138$Grade=="C+",is.na=TRUE)/nrow(HSGD1138)*100,
                  percD = sum(HSGD1138$Grade=="D"|HSGD1138$Grade=="D-"|HSGD1138$Grade=="D+"|HSGD1138$Grade=="C-", is.na=TRUE)/nrow(HSGD1138)*100,
                  percF = sum(HSGD1138$Grade=="F", is.na=TRUE)/nrow(HSGD1138)*100,
                  percDR = sum(HSGD1138$Grade=="DR", is.na=TRUE)/nrow(HSGD1138)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1138 <- melt(HSGD1138, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1138Plot <- ggplot(HSGD1138, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1138Plot)
################################################################
HSGD1141<- subset(PCBHS, Term=="1141")

HSGD1141 <- ddply(HSGD1141, "Term", summarise, 
                  percA = sum(HSGD1141$Grade=="A"|HSGD1141$Grade=="A-", is.na=TRUE)/nrow(HSGD1141)*100,
                  percB = sum(HSGD1141$Grade=="B"|HSGD1141$Grade=="B-"|HSGD1141$Grade=="B+", is.na=TRUE)/nrow(HSGD1141)*100,
                  percC = sum(HSGD1141$Grade=="C"|HSGD1141$Grade=="C+",is.na=TRUE)/nrow(HSGD1141)*100,
                  percD = sum(HSGD1141$Grade=="D"|HSGD1141$Grade=="D-"|HSGD1141$Grade=="D+"|HSGD1141$Grade=="C-", is.na=TRUE)/nrow(HSGD1141)*100,
                  percF = sum(HSGD1141$Grade=="F", is.na=TRUE)/nrow(HSGD1141)*100,
                  percDR = sum(HSGD1141$Grade=="DR", is.na=TRUE)/nrow(HSGD1141)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1141 <- melt(HSGD1141, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1141Plot <- ggplot(HSGD1141, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1141Plot)
################################################################
HSGD1148<- subset(PCBHS, Term=="1148")

HSGD1148 <- ddply(HSGD1148, "Term", summarise, 
                  percA = sum(HSGD1148$Grade=="A"|HSGD1148$Grade=="A-", is.na=TRUE)/nrow(HSGD1148)*100,
                  percB = sum(HSGD1148$Grade=="B"|HSGD1148$Grade=="B-"|HSGD1148$Grade=="B+", is.na=TRUE)/nrow(HSGD1148)*100,
                  percC = sum(HSGD1148$Grade=="C"|HSGD1148$Grade=="C+",is.na=TRUE)/nrow(HSGD1148)*100,
                  percD = sum(HSGD1148$Grade=="D"|HSGD1148$Grade=="D-"|HSGD1148$Grade=="D+"|HSGD1148$Grade=="C-", is.na=TRUE)/nrow(HSGD1148)*100,
                  percF = sum(HSGD1148$Grade=="F", is.na=TRUE)/nrow(HSGD1148)*100,
                  percDR = sum(HSGD1148$Grade=="DR", is.na=TRUE)/nrow(HSGD1148)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1148 <- melt(HSGD1148, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1148Plot <- ggplot(HSGD1148, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1148Plot)
################################################################
HSGD1148<- subset(PCBHS, Term=="1148")

HSGD1148 <- ddply(HSGD1148, "Term", summarise, 
                  percA = sum(HSGD1148$Grade=="A"|HSGD1148$Grade=="A-", is.na=TRUE)/nrow(HSGD1148)*100,
                  percB = sum(HSGD1148$Grade=="B"|HSGD1148$Grade=="B-"|HSGD1148$Grade=="B+", is.na=TRUE)/nrow(HSGD1148)*100,
                  percC = sum(HSGD1148$Grade=="C"|HSGD1148$Grade=="C+",is.na=TRUE)/nrow(HSGD1148)*100,
                  percD = sum(HSGD1148$Grade=="D"|HSGD1148$Grade=="D-"|HSGD1148$Grade=="D+"|HSGD1148$Grade=="C-", is.na=TRUE)/nrow(HSGD1148)*100,
                  percF = sum(HSGD1148$Grade=="F", is.na=TRUE)/nrow(HSGD1148)*100,
                  percDR = sum(HSGD1148$Grade=="DR", is.na=TRUE)/nrow(HSGD1148)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1148 <- melt(HSGD1148, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1148Plot <- ggplot(HSGD1148, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1148Plot)
################################################################
HSGD1151<- subset(PCBHS, Term=="1151")

HSGD1151 <- ddply(HSGD1151, "Term", summarise, 
                  percA = sum(HSGD1151$Grade=="A"|HSGD1151$Grade=="A-", is.na=TRUE)/nrow(HSGD1151)*100,
                  percB = sum(HSGD1151$Grade=="B"|HSGD1151$Grade=="B-"|HSGD1151$Grade=="B+", is.na=TRUE)/nrow(HSGD1151)*100,
                  percC = sum(HSGD1151$Grade=="C"|HSGD1151$Grade=="C+",is.na=TRUE)/nrow(HSGD1151)*100,
                  percD = sum(HSGD1151$Grade=="D"|HSGD1151$Grade=="D-"|HSGD1151$Grade=="D+"|HSGD1151$Grade=="C-", is.na=TRUE)/nrow(HSGD1151)*100,
                  percF = sum(HSGD1151$Grade=="F", is.na=TRUE)/nrow(HSGD1151)*100,
                  percDR = sum(HSGD1151$Grade=="DR", is.na=TRUE)/nrow(HSGD1151)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1151 <- melt(HSGD1151, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1151Plot <- ggplot(HSGD1151, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1151Plot)
################################################################
HSGD1158<- subset(PCBHS, Term=="1158")

HSGD1158 <- ddply(HSGD1158, "Term", summarise, 
                  percA = sum(HSGD1158$Grade=="A"|HSGD1158$Grade=="A-", is.na=TRUE)/nrow(HSGD1158)*100,
                  percB = sum(HSGD1158$Grade=="B"|HSGD1158$Grade=="B-"|HSGD1158$Grade=="B+", is.na=TRUE)/nrow(HSGD1158)*100,
                  percC = sum(HSGD1158$Grade=="C"|HSGD1158$Grade=="C+",is.na=TRUE)/nrow(HSGD1158)*100,
                  percD = sum(HSGD1158$Grade=="D"|HSGD1158$Grade=="D-"|HSGD1158$Grade=="D+"|HSGD1158$Grade=="C-", is.na=TRUE)/nrow(HSGD1158)*100,
                  percF = sum(HSGD1158$Grade=="F", is.na=TRUE)/nrow(HSGD1158)*100,
                  percDR = sum(HSGD1158$Grade=="DR", is.na=TRUE)/nrow(HSGD1158)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1158 <- melt(HSGD1158, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1158Plot <- ggplot(HSGD1158, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1158Plot)
################################################################
HSGD1161<- subset(PCBHS, Term=="1161")

HSGD1161 <- ddply(HSGD1161, "Term", summarise, 
                  percA = sum(HSGD1161$Grade=="A"|HSGD1161$Grade=="A-", is.na=TRUE)/nrow(HSGD1161)*100,
                  percB = sum(HSGD1161$Grade=="B"|HSGD1161$Grade=="B-"|HSGD1161$Grade=="B+", is.na=TRUE)/nrow(HSGD1161)*100,
                  percC = sum(HSGD1161$Grade=="C"|HSGD1161$Grade=="C+",is.na=TRUE)/nrow(HSGD1161)*100,
                  percD = sum(HSGD1161$Grade=="D"|HSGD1161$Grade=="D-"|HSGD1161$Grade=="D+"|HSGD1161$Grade=="C-", is.na=TRUE)/nrow(HSGD1161)*100,
                  percF = sum(HSGD1161$Grade=="F", is.na=TRUE)/nrow(HSGD1161)*100,
                  percDR = sum(HSGD1161$Grade=="DR", is.na=TRUE)/nrow(HSGD1161)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1161 <- melt(HSGD1161, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1161Plot <- ggplot(HSGD1161, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1161Plot)
################################################################
HSGD1168<- subset(PCBHS, Term=="1168")

HSGD1168 <- ddply(HSGD1168, "Term", summarise, 
                  percA = sum(HSGD1168$Grade=="A"|HSGD1168$Grade=="A-", is.na=TRUE)/nrow(HSGD1168)*100,
                  percB = sum(HSGD1168$Grade=="B"|HSGD1168$Grade=="B-"|HSGD1168$Grade=="B+", is.na=TRUE)/nrow(HSGD1168)*100,
                  percC = sum(HSGD1168$Grade=="C"|HSGD1168$Grade=="C+",is.na=TRUE)/nrow(HSGD1168)*100,
                  percD = sum(HSGD1168$Grade=="D"|HSGD1168$Grade=="D-"|HSGD1168$Grade=="D+"|HSGD1168$Grade=="C-", is.na=TRUE)/nrow(HSGD1168)*100,
                  percF = sum(HSGD1168$Grade=="F", is.na=TRUE)/nrow(HSGD1168)*100,
                  percDR = sum(HSGD1168$Grade=="DR", is.na=TRUE)/nrow(HSGD1168)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1168 <- melt(HSGD1168, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1168Plot <- ggplot(HSGD1168, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1168Plot)
################################################################
HSGD1171<- subset(PCBHS, Term=="1171")

HSGD1171 <- ddply(HSGD1171, "Term", summarise, 
                  percA = sum(HSGD1171$Grade=="A"|HSGD1171$Grade=="A-", is.na=TRUE)/nrow(HSGD1171)*100,
                  percB = sum(HSGD1171$Grade=="B"|HSGD1171$Grade=="B-"|HSGD1171$Grade=="B+", is.na=TRUE)/nrow(HSGD1171)*100,
                  percC = sum(HSGD1171$Grade=="C"|HSGD1171$Grade=="C+",is.na=TRUE)/nrow(HSGD1171)*100,
                  percD = sum(HSGD1171$Grade=="D"|HSGD1171$Grade=="D-"|HSGD1171$Grade=="D+"|HSGD1171$Grade=="C-", is.na=TRUE)/nrow(HSGD1171)*100,
                  percF = sum(HSGD1171$Grade=="F", is.na=TRUE)/nrow(HSGD1171)*100,
                  percDR = sum(HSGD1171$Grade=="DR", is.na=TRUE)/nrow(HSGD1171)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1171 <- melt(HSGD1171, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1171Plot <- ggplot(HSGD1171, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1171Plot)
################################################################

#Grid Arrange

grid.arrange(HSGD1108Plot, HSGD1111Plot, ncol=1)
grid.arrange(HSGD1118Plot, HSGD1121Plot, ncol=1)
grid.arrange(HSGD1128Plot, HSGD1131Plot, ncol=1)
grid.arrange(HSGD1138Plot, HSGD1141Plot, ncol=1)
grid.arrange(HSGD1148Plot, HSGD1151Plot, ncol=1)
grid.arrange(HSGD1158Plot, HSGD1161Plot, ncol=1)
grid.arrange(HSGD1168Plot, HSGD1171Plot, ncol=1)

# Check for LA's

HSGDNoLAs<- subset(PCBHS, Term=="1108"|Term=="1111"|Term=="1118"|Term=="1121"|Term=="1128"|Term=="1131"|Term=="1138"|Term=="1141"|Term=="1148"|Term=="1151"|Term=="1161")

HSGDNoLAs<- ddply(HSGDNoLAs, "Name", summarise, 
                  percA = sum(HSGDNoLAs$Grade=="A"|HSGDNoLAs$Grade=="A-", is.na=TRUE)/nrow(HSGDNoLAs)*100,
                  percB = sum(HSGDNoLAs$Grade=="B"|HSGDNoLAs$Grade=="B-"|HSGDNoLAs$Grade=="B+", is.na=TRUE)/nrow(HSGDNoLAs)*100,
                  percC = sum(HSGDNoLAs$Grade=="C"|HSGDNoLAs$Grade=="C+",is.na=TRUE)/nrow(HSGDNoLAs)*100,
                  percD = sum(HSGDNoLAs$Grade=="D"|HSGDNoLAs$Grade=="D-"|HSGDNoLAs$Grade=="D+"|HSGDNoLAs$Grade=="C-", is.na=TRUE)/nrow(HSGDNoLAs)*100,
                  percF = sum(HSGDNoLAs$Grade=="F", is.na=TRUE)/nrow(HSGDNoLAs)*100,
                  percDR = sum(HSGDNoLAs$Grade=="DR", is.na=TRUE)/nrow(HSGDNoLAs)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGDNoLAs <- melt(HSGDNoLAs, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
HSGDNoLAsPlot <- ggplot(HSGDNoLAs, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 GD in Semesters w/o LAs",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(HSGDNoLAsPlot)

HSGDwLAs <- subset(PCBHS, Term=="1158"|Term=="1168"|Term=="1171")
HSGDwLAs<- ddply(HSGDwLAs, "Name", summarise, 
                  percA = sum(HSGDwLAs$Grade=="A"|HSGDwLAs$Grade=="A-", is.na=TRUE)/nrow(HSGDwLAs)*100,
                  percB = sum(HSGDwLAs$Grade=="B"|HSGDwLAs$Grade=="B-"|HSGDwLAs$Grade=="B+", is.na=TRUE)/nrow(HSGDwLAs)*100,
                  percC = sum(HSGDwLAs$Grade=="C"|HSGDwLAs$Grade=="C+",is.na=TRUE)/nrow(HSGDwLAs)*100,
                  percD = sum(HSGDwLAs$Grade=="D"|HSGDwLAs$Grade=="D-"|HSGDwLAs$Grade=="D+"|HSGDwLAs$Grade=="C-", is.na=TRUE)/nrow(HSGDwLAs)*100,
                  percF = sum(HSGDwLAs$Grade=="F", is.na=TRUE)/nrow(HSGDwLAs)*100,
                  percDR = sum(HSGDwLAs$Grade=="DR", is.na=TRUE)/nrow(HSGDwLAs)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGDwLAs <- melt(HSGDwLAs, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
HSGDwLAsPlot <- ggplot(HSGDwLAs, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 GD in Semesters with LAs",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(HSGDwLAsPlot)

## Plots for Helena comparing semesters with and without LAs ##
grid.arrange(HSGDNoLAsPlot, HSGDwLAsPlot, ncol=1)

## Inserting 1 for terms with LAs and 0 for terms without LAs for Helena S. ##
PCBHS$LA <- with(PCBHS, ifelse(( Term=="1158"| Term=="1168"| Term=="1171"), "1", 
                               (ifelse(Term=="1108"| Term=="1111"| Term=="1115"|
                               Term=="1118"| Term=="1121"| Term=="1125"| Term=="1128"|
                               Term=="1131"| Term=="1135"| Term=="1138"| Term=="1141"| 
                               Term=="1145"| Term=="1148"| Term=="1151"| Term=="1155"| 
                               Term=="1161"| Term=="1165", "0", "NA"))))
####################################################################################
HSGD1168<- subset(PCBHS, Term=="1168" | Term== "1171")

HSGD1168 <- ddply(PCBHS, "Term", summarise, 
                  percA = sum(HSGD1168$Grade=="A"|HSGD1168$Grade=="A-", is.na=TRUE)/nrow(HSGD1168)*100,
                  percB = sum(HSGD1168$Grade=="B"|HSGD1168$Grade=="B-"|HSGD1168$Grade=="B+", is.na=TRUE)/nrow(HSGD1168)*100,
                  percC = sum(HSGD1168$Grade=="C"|HSGD1168$Grade=="C+",is.na=TRUE)/nrow(HSGD1168)*100,
                  percD = sum(HSGD1168$Grade=="D"|HSGD1168$Grade=="D-"|HSGD1168$Grade=="D+"|HSGD1168$Grade=="C-", is.na=TRUE)/nrow(HSGD1168)*100,
                  percF = sum(HSGD1168$Grade=="F", is.na=TRUE)/nrow(HSGD1168)*100,
                  percDR = sum(HSGD1168$Grade=="DR", is.na=TRUE)/nrow(HSGD1168)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR)

HSGD1168 <- melt(HSGD1168, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Term"))
## First table##
HSGD1168Plot <- ggplot(HSGD1168, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Term~.)
print(HSGD1168Plot)


####################################################################################
# Grade distribution across all professors per term ##

##1108

GD1108<- subset(PCBsubset, Term=="1108")
table(GD1108$Grade, GD1108$Name)
table(GD1108$Name)

GD1108 <- ddply(GD1108, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                  percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                  percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                  pass = percA+percB+percC,
                  fail = percD, percF, percDR) 

GD1108 <- melt(GD1108, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1108Plot <- ggplot(GD1108, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1108",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1108Plot)

##1111
GD1111<- subset(PCBsubset, Term=="1111")

GD1111 <- ddply(GD1111, "Name", summarise, 
                percA = sum(GD1111$Grade=="A"|GD1111$Grade=="A-", is.na=TRUE)/nrow(GD1111)*100,
                percB = sum(GD1111$Grade=="B"|GD1111$Grade=="B-"|GD1111$Grade=="B+", is.na=TRUE)/nrow(GD1111)*100,
                percC = sum(GD1111$Grade=="C"|GD1111$Grade=="C+",is.na=TRUE)/nrow(GD1111)*100,
                percD = sum(GD1111$Grade=="D"|GD1111$Grade=="D-"|GD1111$Grade=="D+"|GD1111$Grade=="C-", is.na=TRUE)/nrow(GD1111)*100,
                percF = sum(GD1111$Grade=="F", is.na=TRUE)/nrow(GD1111)*100,
                percDR = sum(GD1111$Grade=="DR", is.na=TRUE)/nrow(GD1111)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1111 <- melt(GD1111, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1111Plot <- ggplot(GD1111, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1108",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1111Plot)

##1115
GD1115<- subset(PCBsubset, Term=="1115")

GD1115 <- ddply(GD1115, "Name", summarise, 
                percA = sum(GD1115$Grade=="A"|GD1115$Grade=="A-", is.na=TRUE)/nrow(GD1115)*100,
                percB = sum(GD1115$Grade=="B"|GD1115$Grade=="B-"|GD1115$Grade=="B+", is.na=TRUE)/nrow(GD1115)*100,
                percC = sum(GD1115$Grade=="C"|GD1115$Grade=="C+",is.na=TRUE)/nrow(GD1115)*100,
                percD = sum(GD1115$Grade=="D"|GD1115$Grade=="D-"|GD1115$Grade=="D+"|GD1115$Grade=="C-", is.na=TRUE)/nrow(GD1115)*100,
                percF = sum(GD1115$Grade=="F", is.na=TRUE)/nrow(GD1115)*100,
                percDR = sum(GD1115$Grade=="DR", is.na=TRUE)/nrow(GD1115)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1115 <- melt(GD1115, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1115Plot <- ggplot(GD1115, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1115",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1115Plot)

##1118

GD1118<- subset(PCBsubset, Term=="1118")

GD1118 <- ddply(GD1118, "Name", summarise, 
                percA = sum(GD1118$Grade=="A"|GD1118$Grade=="A-", is.na=TRUE)/nrow(GD1118)*100,
                percB = sum(GD1118$Grade=="B"|GD1118$Grade=="B-"|GD1118$Grade=="B+", is.na=TRUE)/nrow(GD1118)*100,
                percC = sum(GD1118$Grade=="C"|GD1118$Grade=="C+",is.na=TRUE)/nrow(GD1118)*100,
                percD = sum(GD1118$Grade=="D"|GD1118$Grade=="D-"|GD1118$Grade=="D+"|GD1118$Grade=="C-", is.na=TRUE)/nrow(GD1118)*100,
                percF = sum(GD1118$Grade=="F", is.na=TRUE)/nrow(GD1118)*100,
                percDR = sum(GD1118$Grade=="DR", is.na=TRUE)/nrow(GD1118)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1118 <- melt(GD1118, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1118Plot <- ggplot(GD1118, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1118",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1118Plot)

##1121
GD1121<- subset(PCBsubset, Term=="1121")

GD1121 <- ddply(GD1121, "Name", summarise, 
                percA = sum(GD1121$Grade=="A"|GD1121$Grade=="A-", is.na=TRUE)/nrow(GD1121)*100,
                percB = sum(GD1121$Grade=="B"|GD1121$Grade=="B-"|GD1121$Grade=="B+", is.na=TRUE)/nrow(GD1121)*100,
                percC = sum(GD1121$Grade=="C"|GD1121$Grade=="C+",is.na=TRUE)/nrow(GD1121)*100,
                percD = sum(GD1121$Grade=="D"|GD1121$Grade=="D-"|GD1121$Grade=="D+"|GD1121$Grade=="C-", is.na=TRUE)/nrow(GD1121)*100,
                percF = sum(GD1121$Grade=="F", is.na=TRUE)/nrow(GD1121)*100,
                percDR = sum(GD1121$Grade=="DR", is.na=TRUE)/nrow(GD1121)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1121 <- melt(GD1121, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1121Plot <- ggplot(GD1121, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1121",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1121Plot)

##1125

GD1125<- subset(PCBsubset, Term=="1125")

GD1125 <- ddply(GD1125, "Name", summarise, 
                percA = sum(GD1125$Grade=="A"|GD1125$Grade=="A-", is.na=TRUE)/nrow(GD1125)*100,
                percB = sum(GD1125$Grade=="B"|GD1125$Grade=="B-"|GD1125$Grade=="B+", is.na=TRUE)/nrow(GD1125)*100,
                percC = sum(GD1125$Grade=="C"|GD1125$Grade=="C+",is.na=TRUE)/nrow(GD1125)*100,
                percD = sum(GD1125$Grade=="D"|GD1125$Grade=="D-"|GD1125$Grade=="D+"|GD1125$Grade=="C-", is.na=TRUE)/nrow(GD1125)*100,
                percF = sum(GD1125$Grade=="F", is.na=TRUE)/nrow(GD1125)*100,
                percDR = sum(GD1125$Grade=="DR", is.na=TRUE)/nrow(GD1125)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1125 <- melt(GD1125, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1125Plot <- ggplot(GD1125, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1125",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1125Plot)

##1128
GD1128<- subset(PCBsubset, Term=="1128")

GD1128 <- ddply(GD1128, "Name", summarise, 
                percA = sum(GD1128$Grade=="A"|GD1128$Grade=="A-", is.na=TRUE)/nrow(GD1128)*100,
                percB = sum(GD1128$Grade=="B"|GD1128$Grade=="B-"|GD1128$Grade=="B+", is.na=TRUE)/nrow(GD1128)*100,
                percC = sum(GD1128$Grade=="C"|GD1128$Grade=="C+",is.na=TRUE)/nrow(GD1128)*100,
                percD = sum(GD1128$Grade=="D"|GD1128$Grade=="D-"|GD1128$Grade=="D+"|GD1128$Grade=="C-", is.na=TRUE)/nrow(GD1128)*100,
                percF = sum(GD1128$Grade=="F", is.na=TRUE)/nrow(GD1128)*100,
                percDR = sum(GD1128$Grade=="DR", is.na=TRUE)/nrow(GD1128)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1128 <- melt(GD1128, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1128Plot <- ggplot(GD1128, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1128",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1128Plot)

##1131
GD1131<- subset(PCBsubset, Term=="1131")

GD1131 <- ddply(GD1131, "Name", summarise, 
                percA = sum(GD1131$Grade=="A"|GD1131$Grade=="A-", is.na=TRUE)/nrow(GD1131)*100,
                percB = sum(GD1131$Grade=="B"|GD1131$Grade=="B-"|GD1131$Grade=="B+", is.na=TRUE)/nrow(GD1131)*100,
                percC = sum(GD1131$Grade=="C"|GD1131$Grade=="C+",is.na=TRUE)/nrow(GD1131)*100,
                percD = sum(GD1131$Grade=="D"|GD1131$Grade=="D-"|GD1131$Grade=="D+"|GD1131$Grade=="C-", is.na=TRUE)/nrow(GD1131)*100,
                percF = sum(GD1131$Grade=="F", is.na=TRUE)/nrow(GD1131)*100,
                percDR = sum(GD1131$Grade=="DR", is.na=TRUE)/nrow(GD1131)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1131 <- melt(GD1131, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1131Plot <- ggplot(GD1131, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1131",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1131Plot)

##1135

GD1135<- subset(PCBsubset, Term=="1135")

GD1135 <- ddply(GD1135, "Name", summarise, 
                percA = sum(GD1135$Grade=="A"|GD1135$Grade=="A-", is.na=TRUE)/nrow(GD1135)*100,
                percB = sum(GD1135$Grade=="B"|GD1135$Grade=="B-"|GD1135$Grade=="B+", is.na=TRUE)/nrow(GD1135)*100,
                percC = sum(GD1135$Grade=="C"|GD1135$Grade=="C+",is.na=TRUE)/nrow(GD1135)*100,
                percD = sum(GD1135$Grade=="D"|GD1135$Grade=="D-"|GD1135$Grade=="D+"|GD1135$Grade=="C-", is.na=TRUE)/nrow(GD1135)*100,
                percF = sum(GD1135$Grade=="F", is.na=TRUE)/nrow(GD1135)*100,
                percDR = sum(GD1135$Grade=="DR", is.na=TRUE)/nrow(GD1135)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1135 <- melt(GD1135, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1135Plot <- ggplot(GD1135, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1135",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1135Plot)

##1138
GD1138<- subset(PCBsubset, Term=="1138")

GD1138 <- ddply(GD1108, "Name", summarise, 
                percA = sum(GD1138$Grade=="A"|GD1138$Grade=="A-", is.na=TRUE)/nrow(GD1138)*100,
                percB = sum(GD1138$Grade=="B"|GD1138$Grade=="B-"|GD1138$Grade=="B+", is.na=TRUE)/nrow(GD1138)*100,
                percC = sum(GD1138$Grade=="C"|GD1138$Grade=="C+",is.na=TRUE)/nrow(GD1138)*100,
                percD = sum(GD1138$Grade=="D"|GD1138$Grade=="D-"|GD1138$Grade=="D+"|GD1138$Grade=="C-", is.na=TRUE)/nrow(GD1138)*100,
                percF = sum(GD1138$Grade=="F", is.na=TRUE)/nrow(GD1138)*100,
                percDR = sum(GD1138$Grade=="DR", is.na=TRUE)/nrow(GD1138)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1138 <- melt(GD1138, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1138Plot <- ggplot(GD1138, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1138",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1138Plot)

##1141
GD1141<- subset(PCBsubset, Term=="1141")

GD1141 <- ddply(GD1141, "Name", summarise, 
                percA = sum(GD1141$Grade=="A"|GD1141$Grade=="A-", is.na=TRUE)/nrow(GD1141)*100,
                percB = sum(GD1141$Grade=="B"|GD1141$Grade=="B-"|GD1141$Grade=="B+", is.na=TRUE)/nrow(GD1141)*100,
                percC = sum(GD1141$Grade=="C"|GD1141$Grade=="C+",is.na=TRUE)/nrow(GD1141)*100,
                percD = sum(GD1141$Grade=="D"|GD1141$Grade=="D-"|GD1141$Grade=="D+"|GD1141$Grade=="C-", is.na=TRUE)/nrow(GD1141)*100,
                percF = sum(GD1141$Grade=="F", is.na=TRUE)/nrow(GD1141)*100,
                percDR = sum(GD1141$Grade=="DR", is.na=TRUE)/nrow(GD1141)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1141 <- melt(GD1141, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1141Plot <- ggplot(GD1141, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1141",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1141Plot)

##1145
GD1145<- subset(PCBsubset, Term=="1145")

GD1145 <- ddply(GD1145, "Name", summarise, 
                percA = sum(GD1145$Grade=="A"|GD1145$Grade=="A-", is.na=TRUE)/nrow(GD1145)*100,
                percB = sum(GD1145$Grade=="B"|GD1145$Grade=="B-"|GD1145$Grade=="B+", is.na=TRUE)/nrow(GD1145)*100,
                percC = sum(GD1145$Grade=="C"|GD1145$Grade=="C+",is.na=TRUE)/nrow(GD1145)*100,
                percD = sum(GD1145$Grade=="D"|GD1145$Grade=="D-"|GD1145$Grade=="D+"|GD1145$Grade=="C-", is.na=TRUE)/nrow(GD1145)*100,
                percF = sum(GD1145$Grade=="F", is.na=TRUE)/nrow(GD1145)*100,
                percDR = sum(GD1145$Grade=="DR", is.na=TRUE)/nrow(GD1145)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1145 <- melt(GD1145, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1145Plot <- ggplot(GD1145, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1145",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1145Plot)

##1148
GD1148<- subset(PCBsubset, Term=="1148")

GD1148 <- ddply(GD1148, "Name", summarise, 
                percA = sum(GD1148$Grade=="A"|GD1148$Grade=="A-", is.na=TRUE)/nrow(GD1148)*100,
                percB = sum(GD1148$Grade=="B"|GD1148$Grade=="B-"|GD1148$Grade=="B+", is.na=TRUE)/nrow(GD1148)*100,
                percC = sum(GD1148$Grade=="C"|GD1148$Grade=="C+",is.na=TRUE)/nrow(GD1148)*100,
                percD = sum(GD1148$Grade=="D"|GD1148$Grade=="D-"|GD1148$Grade=="D+"|GD1148$Grade=="C-", is.na=TRUE)/nrow(GD1148)*100,
                percF = sum(GD1148$Grade=="F", is.na=TRUE)/nrow(GD1148)*100,
                percDR = sum(GD1148$Grade=="DR", is.na=TRUE)/nrow(GD1148)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1148 <- melt(GD1148, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1148Plot <- ggplot(GD1148, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1148",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1148Plot)

##1151

GD1151<- subset(PCBsubset, Term=="1151")

GD1151 <- ddply(GD1108, "Name", summarise, 
                percA = sum(GD1151$Grade=="A"|GD1151$Grade=="A-", is.na=TRUE)/nrow(GD1151)*100,
                percB = sum(GD1151$Grade=="B"|GD1151$Grade=="B-"|GD1151$Grade=="B+", is.na=TRUE)/nrow(GD1151)*100,
                percC = sum(GD1151$Grade=="C"|GD1151$Grade=="C+",is.na=TRUE)/nrow(GD1151)*100,
                percD = sum(GD1151$Grade=="D"|GD1151$Grade=="D-"|GD1151$Grade=="D+"|GD1151$Grade=="C-", is.na=TRUE)/nrow(GD1151)*100,
                percF = sum(GD1151$Grade=="F", is.na=TRUE)/nrow(GD1151)*100,
                percDR = sum(GD1151$Grade=="DR", is.na=TRUE)/nrow(GD1151)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1151<- melt(GD1151, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1151Plot <- ggplot(GD1151, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1151",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1151Plot)

##1155
GD1155<- subset(PCBsubset, Term=="1155")

GD1155 <- ddply(GD1155, "Name", summarise, 
                percA = sum(GD1155$Grade=="A"|GD1155$Grade=="A-", is.na=TRUE)/nrow(GD1155)*100,
                percB = sum(GD1155$Grade=="B"|GD1155$Grade=="B-"|GD1155$Grade=="B+", is.na=TRUE)/nrow(GD1155)*100,
                percC = sum(GD1155$Grade=="C"|GD1155$Grade=="C+",is.na=TRUE)/nrow(GD1155)*100,
                percD = sum(GD1155$Grade=="D"|GD1155$Grade=="D-"|GD1155$Grade=="D+"|GD1155$Grade=="C-", is.na=TRUE)/nrow(GD1155)*100,
                percF = sum(GD1155$Grade=="F", is.na=TRUE)/nrow(GD1155)*100,
                percDR = sum(GD1155$Grade=="DR", is.na=TRUE)/nrow(GD1155)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1155 <- melt(GD1155, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1155Plot <- ggplot(GD1155, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1155",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1155Plot)

##1158
GD1158<- subset(PCBsubset, Term=="1158")

GD1158 <- ddply(GD1158, "Name", summarise, 
                percA = sum(GD1158$Grade=="A"|GD1158$Grade=="A-", is.na=TRUE)/nrow(GD1158)*100,
                percB = sum(GD1158$Grade=="B"|GD1158$Grade=="B-"|GD1158$Grade=="B+", is.na=TRUE)/nrow(GD1158)*100,
                percC = sum(GD1158$Grade=="C"|GD1158$Grade=="C+",is.na=TRUE)/nrow(GD1158)*100,
                percD = sum(GD1158$Grade=="D"|GD1158$Grade=="D-"|GD1158$Grade=="D+"|GD1158$Grade=="C-", is.na=TRUE)/nrow(GD1158)*100,
                percF = sum(GD1158$Grade=="F", is.na=TRUE)/nrow(GD1158)*100,
                percDR = sum(GD1158$Grade=="DR", is.na=TRUE)/nrow(GD1158)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1158<- melt(GD1158, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1158Plot <- ggplot(GD1158, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1158",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1158Plot)

##1161
GD1161<- subset(PCBsubset, Term=="1161")

GD1161 <- ddply(GD1161, "Name", summarise, 
                percA = sum(GD1161$Grade=="A"|GD1161$Grade=="A-", is.na=TRUE)/nrow(GD1161)*100,
                percB = sum(GD1161$Grade=="B"|GD1161$Grade=="B-"|GD1161$Grade=="B+", is.na=TRUE)/nrow(GD1161)*100,
                percC = sum(GD1161$Grade=="C"|GD1161$Grade=="C+",is.na=TRUE)/nrow(GD1161)*100,
                percD = sum(GD1161$Grade=="D"|GD1161$Grade=="D-"|GD1161$Grade=="D+"|GD1161$Grade=="C-", is.na=TRUE)/nrow(GD1161)*100,
                percF = sum(GD1161$Grade=="F", is.na=TRUE)/nrow(GD1161)*100,
                percDR = sum(GD1161$Grade=="DR", is.na=TRUE)/nrow(GD1161)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1161 <- melt(GD1161, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1161Plot <- ggplot(GD1161, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1161",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1161Plot)

##1165

GD1165<- subset(PCBsubset, Term=="1165")

GD1165 <- ddply(GD1165, "Name", summarise, 
                percA = sum(GD1165$Grade=="A"|GD1165$Grade=="A-", is.na=TRUE)/nrow(GD1165)*100,
                percB = sum(GD1165$Grade=="B"|GD1165$Grade=="B-"|GD1165$Grade=="B+", is.na=TRUE)/nrow(GD1165)*100,
                percC = sum(GD1165$Grade=="C"|GD1165$Grade=="C+",is.na=TRUE)/nrow(GD1165)*100,
                percD = sum(GD1165$Grade=="D"|GD1165$Grade=="D-"|GD1165$Grade=="D+"|GD1165$Grade=="C-", is.na=TRUE)/nrow(GD1165)*100,
                percF = sum(GD1165$Grade=="F", is.na=TRUE)/nrow(GD1165)*100,
                percDR = sum(GD1165$Grade=="DR", is.na=TRUE)/nrow(GD1165)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1165<- melt(GD1165, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1165Plot <- ggplot(GD1165, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1165",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1165Plot)

##1168

GD1168<- subset(PCBsubset, Term=="1168")

GD1168 <- ddply(GD1168, "Name", summarise, 
                percA = sum(GD1168$Grade=="A"|GD1168$Grade=="A-", is.na=TRUE)/nrow(GD1168)*100,
                percB = sum(GD1168$Grade=="B"|GD1168$Grade=="B-"|GD1168$Grade=="B+", is.na=TRUE)/nrow(GD1168)*100,
                percC = sum(GD1168$Grade=="C"|GD1168$Grade=="C+",is.na=TRUE)/nrow(GD1168)*100,
                percD = sum(GD1168$Grade=="D"|GD1168$Grade=="D-"|GD1168$Grade=="D+"|GD1168$Grade=="C-", is.na=TRUE)/nrow(GD1168)*100,
                percF = sum(GD1168$Grade=="F", is.na=TRUE)/nrow(GD1168)*100,
                percDR = sum(GD1168$Grade=="DR", is.na=TRUE)/nrow(GD1168)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1168<- melt(GD1168, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1168Plot <- ggplot(GD1168, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1168",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1168Plot)

##1171

GD1171<- subset(PCBsubset, Term=="1171")

GD1171<- ddply(GD1171, "Name", summarise, 
                percA = sum(GD1171$Grade=="A"|GD1171$Grade=="A-", is.na=TRUE)/nrow(GD1171)*100,
                percB = sum(GD1171$Grade=="B"|GD1171$Grade=="B-"|GD1171$Grade=="B+", is.na=TRUE)/nrow(GD1171)*100,
                percC = sum(GD1171$Grade=="C"|GD1171$Grade=="C+",is.na=TRUE)/nrow(GD1171)*100,
                percD = sum(GD1171$Grade=="D"|GD1171$Grade=="D-"|GD1171$Grade=="D+"|GD1171$Grade=="C-", is.na=TRUE)/nrow(GD1171)*100,
                percF = sum(GD1171$Grade=="F", is.na=TRUE)/nrow(GD1171)*100,
                percDR = sum(GD1171$Grade=="DR", is.na=TRUE)/nrow(GD1171)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR)

GD1171<- melt(GD1171, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1171Plot <- ggplot(GD1171, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1171",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1171Plot)

####################################################################################
# Grade distribution across all professors per term ## AGAIN 

##1108

GD1108<- subset(PCBsubset, Term=="1108")
table(GD1108$Grade, GD1108$Name)
table(GD1108$Name)

GD1108 <- ddply(GD1108, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1108 <- melt(GD1108, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1108Plot <- ggplot(GD1108, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1108",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1108Plot)

#1111
GD1111<- subset(PCBsubset, Term=="1111")
table(GD1111$Grade, GD1111$Name)
table(GD1111$Name)

GD1111 <- ddply(GD1111, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1111 <- melt(GD1111, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1111Plot <- ggplot(GD1111, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1111",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1111Plot)

#1115
GD1115<- subset(PCBsubset, Term=="1115")
table(GD1115$Grade, GD1115$Name)
table(GD1115$Name)

GD1115 <- ddply(GD1115, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1115 <- melt(GD1115, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1115Plot <- ggplot(GD1115, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1115",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1115Plot)

#1118
GD1118<- subset(PCBsubset, Term=="1118")
table(GD1118$Grade, GD1118$Name)
table(GD1118$Name)

GD1118 <- ddply(GD1118, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1118 <- melt(GD1118, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1118Plot <- ggplot(GD1118, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1118",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1118Plot)

#1121
GD1121<- subset(PCBsubset, Term=="1121")
table(GD11218$Grade, GD1121$Name)
table(GD1121$Name)

GD1121 <- ddply(GD1121, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1121 <- melt(GD1121, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1121Plot <- ggplot(GD1121, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1121",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1121Plot)

#1125
GD1125<- subset(PCBsubset, Term=="1125")
table(GD1125$Grade, GD1125$Name)
table(GD1125$Name)

GD1125 <- ddply(GD1125, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1125 <- melt(GD1125, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1125Plot <- ggplot(GD1125, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1125",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1125Plot)

#1128
GD1128<- subset(PCBsubset, Term=="1128")
table(GD1128$Grade, GD1128$Name)
table(GD1128$Name)

GD1128 <- ddply(GD1128, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1128 <- melt(GD1128, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1128Plot <- ggplot(GD1128, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1128",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1128Plot)

#1131
GD1131<- subset(PCBsubset, Term=="1131")
table(GD1131$Grade, GD1131$Name)
table(GD1131$Name)

GD1131 <- ddply(GD1131, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1131 <- melt(GD1131, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1131Plot <- ggplot(GD1131, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1131",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1131Plot)

#1135
GD1135<- subset(PCBsubset, Term=="1135")
table(GD1135$Grade, GD1108$Name)
table(GD1135$Name)

GD1135 <- ddply(GD1135, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1135 <- melt(GD1135, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1135Plot <- ggplot(GD1135, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1135",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1135Plot)

#1138
GD1138<- subset(PCBsubset, Term=="1138")
table(GD1138$Grade, GD1138$Name)
table(GD1138$Name)

GD1138 <- ddply(GD1138, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1138 <- melt(GD1138, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1138Plot <- ggplot(GD1138, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1138",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1138Plot)

#1141
GD1141<- subset(PCBsubset, Term=="1141")
table(GD1141$Grade, GD1141$Name)
table(GD1141$Name)

GD1141 <- ddply(GD1141, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1141 <- melt(GD1141, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1141Plot <- ggplot(GD1141, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1141",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1141Plot)

#1145
GD1145<- subset(PCBsubset, Term=="1145")
table(GD1145$Grade, GD1145$Name)
table(GD1145$Name)

GD1145 <- ddply(GD1145, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1145 <- melt(GD1145, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1145Plot <- ggplot(GD1145, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1145",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1145Plot)

#1148
GD1148<- subset(PCBsubset, Term=="1148")
table(GD1148$Grade, GD1148$Name)
table(GD1148$Name)

GD1148 <- ddply(GD1148, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1148 <- melt(GD1148, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1148Plot <- ggplot(GD1148, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1148",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1148Plot)

#1151
GD1151<- subset(PCBsubset, Term=="1151")
table(GD1151$Grade, GD1151$Name)
table(GD1151$Name)

GD1151 <- ddply(GD1151, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1151 <- melt(GD1151, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1151Plot <- ggplot(GD1151, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1151",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1151Plot)

#1155
GD1155<- subset(PCBsubset, Term=="1155")
table(GD1155$Grade, GD1155$Name)
table(GD1155$Name)

GD1155 <- ddply(GD1155, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1155 <- melt(GD1155, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1155Plot <- ggplot(GD1155, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1155",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1155Plot)

#1158
GD1158<- subset(PCBsubset, Term=="1158")
table(GD1158$Grade, GD1158$Name)
table(GD1158$Name)

GD1158 <- ddply(GD1158, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1158 <- melt(GD1158, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1158Plot <- ggplot(GD1158, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1158",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1158Plot)

#1161
GD1161<- subset(PCBsubset, Term=="1161")
table(GD1161$Grade, GD1161$Name)
table(GD1161$Name)

GD1161 <- ddply(GD1161, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1161 <- melt(GD1161, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1161Plot <- ggplot(GD1161, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1161",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1161Plot)

#1165
GD1165<- subset(PCBsubset, Term=="1165")
table(GD1165$Grade, GD1165$Name)
table(GD1165$Name)

GD1165 <- ddply(GD1165, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1165 <- melt(GD1165, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1165Plot <- ggplot(GD1165, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1165",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1165Plot)

#1168
GD1168<- subset(PCBsubset, Term=="1168")
table(GD1168$Grade, GD1168$Name)
table(GD1168$Name)

GD1168 <- ddply(GD1168, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1168 <- melt(GD1168, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1168Plot <- ggplot(GD1168, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1168",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1168Plot)

#1171
GD1171<- subset(PCBsubset, Term=="1171")
table(GD1171$Grade, GD1171$Name)
table(GD1171$Name)

GD1171 <- ddply(GD1171, c("Name"), summarise, 
                percA = (sum(Grade=="A"|Grade=="A-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percB = (sum(Grade=="B"|Grade=="B-"|Grade=="B+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percC = (sum(Grade=="C"|Grade=="C+", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percD = (sum(Grade=="D"|Grade=="D-"|Grade=="D+"|Grade=="C-", na.rm=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percF = (sum(Grade=="F", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                percDR = (sum(Grade=="DR", is.na=TRUE))/sum(!is.na(Grade), na.rm=TRUE)*100,
                pass = percA+percB+percC,
                fail = percD, percF, percDR) 

GD1171 <- melt(GD1171, c("percA", "percB","percC","percD","percF","percDR"), id.vars=c("Name"))
## First table##
GD1171Plot <- ggplot(GD1171, aes(x=variable, y=value)) + geom_bar(stat = "identity") + 
  labs(title="PCB3063 Grade Distribution 1171",x="Grade",y="Percentage (%)")+
  scale_x_discrete(labels=c("As", "Bs", "Cs","Ds", "Fs", "DRs"))+
  facet_grid(Name~.)
print(GD1171Plot)

-------------------
  PCBsubset$LA_Inst <- with(PCBsubset, ifelse(( Name=="De Etta Kay Mills"| Name=="Matthew DeGennaro"| Name=="Helena Schmidtmayerova"), "LA Instr.", 
                                 (ifelse(Name=="Case Okubo"| Name=="Heather Bracken-grissom "| Name=="Jeffrey Wells"|
                                           Name=="Jose Eirin-Lopez"| Name=="Manuel Barbieri"| Name=="Martin Tracey", "No LAs", "NA"))))


PCBsubset_Updated <- subset(PCBsubset, !(Name=="De Etta Kay Mills"| Name=="Matthew DeGennaro"| Name=="Helena Schmidtmayerova"))

## Subset of only Helena ##
PCBsubset_HS <- subset(PCBsubset, Name=="Helena Schmidtmayerova")
