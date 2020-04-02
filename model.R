library(dplyr)
library(readxl)
library(foreign)
MyData <- read_excel("Battle Student Athlete Research 2017_March 5, 2020_12.51.xlsx")
View(MyData)
names(MyData)

PGG <- tbl_df(MyData)
PGG1 <- select(PGG, -Gender)
PGGM <- filter(PGG, Gender == "Male")
PGGF <- filter(PGG, Gender == "Female")

#table 1 (no probability values)
cor(select(PGG1, Grittiness, Growth)) 
cor(select(PGGM, Grittiness, Growth))
cor(select(PGGF, Grittiness, Growth)) 


#table 2 and 4 correlation values (no probability values)
cor(PGG1) 

summary(PGG)

summary(PGG1)

summary(PGGM)

summary(PGGF)

#table 3 correlation & probability
Factor1 <- MyData$"Factor 1 Perception of Career Development/Exploration"
Factor2 <- MyData$"Factor 2 Career vs. Sport Identity"
Factor3 <- MyData$"Factor 3 Locus of Control"
Factor4 <- MyData$"Factor 4 Barriers to Career Development"
Factor5 <- MyData$"Factor 5 Sport to Work Relationship"
Grit <- MyData$Grittiness
Grow <- MyData$Growth



MF11 <- lm(Factor1~Grit+Grow) 
MF11

MF12 <- lm(scale(Factor1)~scale(Grit)+scale(Grow))
MF12

summary(MF12)
anova(MF12)

MF21 <- lm(Factor2~Grit+Grow)
MF21

MF22 <- lm(scale(Factor2)~scale(Grit)+scale(Grow))
MF22
summary(MF22)
anova(MF22)


MF31 <- lm(Factor3~Grit+Grow)
MF31

MF32 <- lm(scale(Factor3)~scale(Grit)+scale(Grow))
MF32
summary(MF32)
anova(MF32)

MF41 <- lm(Factor4~Grit+Grow)
MF41

MF42 <- lm(scale(Factor4)~scale(Grit)+scale(Grow))
MF42
summary(MF42)
anova(MF42)


MF51 <- lm(Factor5~Grit+Grow)
MF51

MF52 <- lm(scale(Factor5)~scale(Grit)+scale(Grow))
MF52
summary(MF52)
anova(MF52)



#table 5
Atti <- MyData$Attitude
Moti <- MyData$Motivation
GoCo <- MyData$"Goals and Commitment"
PeSk <- MyData$"People Skills"
SeTa <- MyData$"Self Talk"
MeIm <- MyData$"Mental Imagery"
DeAn <- MyData$"Dealing with Anxiety"
DeEm <- MyData$"Dealing with Emotions"
Conc <- MyData$Concentration

#Attitude
M11 <- lm(Atti~Grit+Grow)
M11

M12 <- lm(scale(Atti)~scale(Grit)+scale(Grow))
M12
summary(M12)
anova(M12)

#Motivation
M21 <- lm(Moti~Grit+Grow)
M21

M22 <- lm(scale(Moti)~scale(Grit)+scale(Grow))
M22
summary(M22)
anova(M22)

#Goals and Commitment
M31 <- lm(GoCo~Grit+Grow)
M31
M32 <- lm(scale(GoCo)~scale(Grit)+scale(Grow))
M32
summary(M32)
anova(M32)

#People Skills
M41 <- lm(PeSk~Grit+Grow)
M41
M42 <- lm(scale(PeSk)~scale(Grit)+scale(Grow))
M42
summary(M42)
anova(M42)

#Self Talk
M51 <- lm(SeTa~Grit+Grow)
M51
M52 <- lm(scale(SeTa)~scale(Grit)+scale(Grow))
M52
summary(M52)
anova(M52)

#Mental Imagery
M61 <- lm(MeIm~Grit+Grow)
M61
M62 <- lm(scale(MeIm)~scale(Grit)+scale(Grow))
M62
summary(M62)
anova(M62)

#Dealing With Anxiety
M71 <- lm(DeAn~Grit+Grow)
M71
M72 <- lm(scale(DeAn)~scale(Grit)+scale(Grow))
M72
summary(M72)
anova(M72)

#Dealing with Emotions
M81 <- lm(DeEm~Grit+Grow)
M81
M82 <- lm(scale(DeEm)~scale(Grit)+scale(Grow))
M82
summary(M82)
anova(M82)

#Concentration
M91 <- lm(Conc~Grit+Grow)
M91
M92 <- lm(scale(Conc)~scale(Grit)+scale(Grow))
M92
summary(M92)
anova(M92)