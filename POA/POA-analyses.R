library(tidyverse)
library(stringr)

results.plot.path <-  "plots/"

# Basic plotting theme settings
basic.theme <- theme(
	panel.background = element_rect(
		fill = "transparent",colour = NA),
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
	plot.background = element_rect(
		fill = "transparent",colour = NA),
	legend.background = element_rect(
		fill="transparent"),
	legend.text = element_text(size=30),
	legend.title = element_text(size=30),
	legend.key = element_rect(colour = NA, fill = NA),
	legend.key.height = unit(2, "lines"),
	axis.text.x = element_text(size=30),
	axis.title.x = element_text(size=30),
	axis.text.y = element_text(size=30),
	axis.title.y = element_text(size=30),
	strip.text = element_text(size=30),
	panel.spacing = unit(2, "lines"),
	plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

rare.palate <- c("gray60", "forestgreen", "firebrick1")

# Read in and set up data ######################################################
################################################################################

poa.tblt <- read_csv("POA-compiled.csv",
                     col_types = cols(PID = col_character()))
poa.stim <- read_csv("POA-stimuli.csv")
poa.ptcp <- read_csv("../Tablet-Master-20171110.csv") %>%
  mutate(PID = str_extract(PID, "[0-9]+"))

poa.all <-  poa.tblt %>%
  # add in stimulus info
  left_join(poa.stim) %>%
  # add in participant info
  left_join(poa.ptcp) %>%
  mutate(PID = factor(PID),
         AgeNum = recode(Age,
                         "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7,
                         "8" = 8, "9" = 9, "10" = 10, "11" = 11, "12" = 12,
                         "15" = 14, "21" = 14, "23" = 14, "28" = 14,
                         "37" = 14, "42" = 14, "44" = 14, "46" = 14, "A" = 14),
         AgeGrp = recode(Age,
                         "3" = 1, "4" = 1, 
                         "5" = 2, "6" = 2,
                         "7" = 3, "8" = 3,
                         "9" = 4, "10" = 4, "11" = 4, "12" = 4,
                         "15" = 5, "21" = 5, "23" = 5, "28" = 5,
                         "37" = 5, "42" = 5, "44" = 5, "46" = 5,
                         "A" = 5),
         Correct = ifelse(Tag == "'correct image clicked'", 1,
                          ifelse(Tag == "'incorrect image clicked'", 0,
                                 ifelse(Tag == "'task 2 skipped'",
                                        -2, 999)))) %>%
  # remove the "trial info" for the reward screens
  # and the skipped trials (9/754 = 1.19% of trials)
  filter(Correct != 999 & Correct != -2) %>%
  mutate_if(is.character,funs(factor(.))) %>%
  select(-Tier, -StimulusCode, -BeginTime, -EndTime,
         -Tag, -Comments, -Task2) %>%
  rename(TrialStart = BeginTime2, TrialEnd = EndTime2)

# Add col for each participant with their average filler correctness
flr.perf <- poa.all %>%
  filter(StimType == "filler") %>%
  group_by(PID) %>%
  summarise(filler.m = mean(Correct)) %>%
  arrange(filler.m)

poa.all <-  poa.all %>% left_join(flr.perf)

# Analyze data #################################################################
################################################################################

# Make plots ###################################################################
################################################################################
pid.avg <- poa.all %>%
  group_by(PID, AgeNum, StimType, Rare) %>%
  summarise(m.corr = mean(Correct)) %>%
  group_by(PID) %>%
  mutate(fitline = ifelse(AgeNum < 14, "C", "A"))
pid.avg$Contrast <- factor(pid.avg$Rare, labels=c("Frequent", "Rare", "Filler"))
pid.avg$Contrast <- factor(pid.avg$Contrast, levels=c("Filler", "Frequent", "Rare"))

pid.avg.c <- pid.avg %>% filter(fitline == "C")
pid.avg.a <- pid.avg %>% filter(fitline == "A")

corr.age <- ggplot() +
  geom_jitter(data = pid.avg.c,
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              size=6, alpha=0.6) +
  geom_jitter(data = pid.avg.a,
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              size=6, alpha=0.2) +
  geom_smooth(data = pid.avg.c,
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              method = "lm", size=3) +
  geom_violin(data = pid.avg.a,
              aes(x = AgeNum, y = m.corr, fill=Contrast),
              alpha = 0.5) +
  scale_x_continuous(breaks=c(2,4,6,8,10,12,15),
                     labels = c("2","4","6","8","10","12", "A")) +
  scale_colour_manual(values = rare.palate) +
  scale_fill_manual(values = rare.palate) +
  coord_cartesian(ylim=c(0,1), xlim=c(2,16)) +
      ylab("Proportion correct") + xlab("Age (yrs)")	+
	basic.theme +
	theme(axis.text.x =
			element_text(size=20, angle=0, hjust=0.5),
			axis.text.y =
			element_text(size=20),
			axis.title.x =
			element_text(size=20),
			axis.title.y =
			element_text(size=20),
		legend.title =
			element_text(size=22),
		legend.text =
		  element_text(size=20))

ggsave(plot = corr.age,
       filename = "FIN-correct_by_age.png",
       path = results.plot.path,
       width = 30,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

