# Load data
data = read.csv('C:/Users/PC/Documents/Projects/Github/Visual-Proprioceptive/VisualProprioceptive_Analysis/R_Analysis/DF_Combined_62485.csv')

# Remove uneccesary columns with NaNs first 
data <- subset(data, select = -c(Head_Position, Head_Rotation, RealHandPos, StartZone))
data <- na.omit(data)
data <- data[!is.infinite(rowSums(data)),]
View(data)

# Convert to nominal factor 
#data$Participant_ID = factor(data$Participant_ID)
#data$Trial = factor(data$Trial)
#data$TargetPos = factor(data$TargetPos)
data$Experiment = factor(data$Experiment)


summary(data)

install.packages("dplyr")
library(plyr)
library(dplyr)

# Explore data in light of EndError metric 
ddply(data, ~ TargetPos * Experiment, function(data) summary(data$JND))
ddply(data, ~ TargetPos * Experiment, summarise, JND = mean(JND), sd = sd(JND))

# Histograms of data
hist(data[data$TargetPos == "0.132" & data$Experiment == "Vis",]$JND)
hist(data[data$TargetPos == "0.332" & data$Experiment == "Vis",]$JND)
hist(data[data$TargetPos == "0.553" & data$Experiment == "Vis",]$JND)

hist(data[data$TargetPos == "0.132" & data$Experiment == "Vis",]$JND)
hist(data[data$TargetPos == "0.332" & data$Experiment == "Vis",]$JND)
hist(data[data$TargetPos == "0.553" & data$Experiment == "Vis",]$JND)

# hist(data[data$Phase == "Washout" & data$Group == "Reward",]$MAE_Chunk)

#height_reorder <- with(data, reorder(data$height=="low", data$height=="mid", data$height=="high", FUN=mean))

# Reorder box plot height values 
# data$height2 <- factor(data$height, levels = c("low", "mid", "high"))
# data$tempos2 <- factor(data$tempos, levels = c("80", "120", "160"))
# data$TargetID2 <- factor(data$TargetID, levels = c("row_A1", "row_A2", "row_A3","row_A4", "row_A5", "row_A6",
#                                                    "row_B1", "row_B2", "row_B3","row_B4", "row_B5", "row_B6",
#                                                    "row_C1", "row_C2", "row_C3","row_C4", "row_C5", "row_C6"))

boxplot(data$JND ~ data$TargetPos)
boxplot(data$JND ~ data$Experiment)
with(data, interaction.plot(TargetPos, Experiment, JND))

# install.packages("statmod")
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("car")
library(lme4)
library(lmerTest)
library(car)

# Post hoc pairwise comparisons packages 
# install.packages("multcomp")
# install.packages("lsmeans")
library(multcomp)
library(lsmeans)

# Set sum-to-zero contrasts for the Anova cells 
# contrasts(data$Phase) <- "contr.sum"
# contrasts(data$Group) <- "contr.sum"
# contrasts(data$Trial) <- "contr.sum"


# LMM order effect test
# Subject is a random effect 
#m = lmer(EndError_cleaned ~ (tempos * height * TargetID)/TrialNum + (1|PtxID), data=data)
#m = lmer(EndError_cleaned ~ (tempos * height * TargetID) + (1|height:tempos:TargetID:TrialNum) + (1|PtxID), data=data)

# -------------------------------------------------------------------------------------------------
# -------------------------------------- Mean Absolute Error --------------------------------------
# -------------------------------------------------------------------------------------------------

# see if new Errors data seems Gamma-distributed
# install.packages("fitdistrplus")
library(fitdistrplus)
fit = fitdist(df[df$TargetPos == "0.132" & df$Experiment == "Vis",]$JND, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test

#------------------------------------------------
#----------------- LMER ------------------------
#------------------------------------------------

#m = lmer(formula = JND ~ 1 + TargetPos + Experiment + TargetPos:Experiment + (1|Participant_ID), data=data)
m = glmer(JND ~ (TargetPos * Experiment) + (1|TargetPos:Experiment:Trial) + (1|Participant_ID), data=data, family=poisson, nAGQ=0) # new, correct syntax
summary(m)
Anova(m, type=3, test.statistic = "F")

# not in Coursera video; treat "Trial" as a nested random effect.
# m = glmer(Errors ~ (Keyboard * Posture) + (1|Keyboard:Posture:Trial) + (1|Subject), data=mbltxttrials, family=poisson, nAGQ=0) # new, correct syntax
# Anova(m, type=3)

# Post Hoc Analysis part 
# Positional Error post hoc analysis
summary(glht(m, lsm(pairwise ~ Phase * Group)), test=adjusted(type="holm"))
with(df, interaction.plot(Phase, Group, MeanAbsErr))


library(multcomp) # for glht
library(emmeans) # for emm

# perform post hoc pairwise comparisons
with(df, interaction.plot(Phase, Group, MeanAbsErr, ylim=c(0, max(df$MeanAbsErr)))) # for convenience
summary(glht(m, emm(pairwise ~ Phase * Group)), test=adjusted(type="holm"))





install.packages("tidyverse")
library(tidyverse) # ggplot

## GG-Plot
ggplot(data = data,
       aes(x = TargetPos, 
           y = JND, 
           col = as.factor(Experiment)))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size     = .7,
             alpha    = .8, 
             position = "jitter")+
  geom_smooth(method = lm,
              se     = FALSE,
              size   = 2,
              alpha  = .8)+
  theme_minimal()+
  labs(title    = "Linear Relationship for Different Years of Teacher Experience as Observed", 
       subtitle = "The linear relationship between the two is not the same for all classes", 
       col      = "Years of\nTeacher\nExperience")







# # -------------------------------------------------------------------------------------------------
# # -------------------------------------- Path Offset ----------------------------------------------
# # -------------------------------------------------------------------------------------------------
# 
# m2 = glmer(PathOffsetNoLag_cleaned ~ (TargetID * tempos * height) + (1|PtxID), data=data)
# Anova(m2, type=3, test.statistic = "F")
# 
# # Path offset post hoc analysis
# summary(glht(m2, lsm(pairwise ~ TargetID * tempos)), test=adjusted(type="holm"))
# with(data, interaction.plot(TargetID, height, EndError_cleaned))
# 
# 
# # -------------------------------------------------------------------------------------------------
# # -------------------------------------- Angular Error ----------------------------------------------
# # -------------------------------------------------------------------------------------------------
# 
# dataAng = read.csv('E:/Projects/QuestAccuracyAnalysis/AngularError.csv')
# # Convert to nominal factor 
# data$Ptx = factor(data$Ptx)
# data$Tempo = factor(data$Tempo)
# data$Joint = factor(data$Joint)
# data$AngErr_NoLag = factor(data$AngErr_NoLag)
# summary(dataAng)
# 
# m2 = glmer(AngErr_NoLag ~ (Tempo * Joint) + (1|Ptx), data=dataAng)
# Anova(m2, type=3, test.statistic = "F")
# 
# # Path offset post hoc analysis
# summary(glht(m2, lsm(pairwise ~ Tempo + Joint)), test=adjusted(type="holm"))
# with(dataAng, interaction.plot(Tempo, Joint, AngErr_NoLag))
