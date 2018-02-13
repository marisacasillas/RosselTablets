library(tidyverse)
library(stringr)
library(lme4)
source("geom_pirate.R")

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

# Set custom palette
rare.palette <- c("gray60", "forestgreen", "firebrick1")
rare.paletteGN <- c("forestgreen", "firebrick1", "gray60")
rare.paletteRD <- c("firebrick1", "forestgreen", "gray60")

# Help with checking model residuals
qqplot.data <- function (vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  d <- data.frame(resids = vec)

  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)

}

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

# Crete substitute cols for 'Rare' and 'Complex'
poa.all$Contrast <- factor(poa.all$Rare,
                           labels=c("Frequent", "Rare", "Filler"))
poa.all$Contrast <- factor(poa.all$Contrast,
                           levels=c("Filler", "Frequent", "Rare"))
poa.all$Closure <- factor(poa.all$Complex,
                           labels=c("Single", "Double", "Filler"))
poa.all$Closure <- factor(poa.all$Closure,
                           levels=c("Filler", "Single", "Double"))
poa.all$Pair <- factor(poa.all$Pair,
                       levels=c("filler1", "filler2", "filler3", "filler4",
                                "d-k", "t-k", "p-k", "dp-kp", "tp-kp",
                                "d-t", "nd-nt", "nn-n", "dp-tp"))
poa.all <-  poa.all %>%
  select(-Rare, -Complex)

# Quality control: Exclusions ##################################################
################################################################################
### Participant exclusions ###
# Add col listing each participants' average filler correctness
flr.perf <- poa.all %>%
  filter(StimType == "filler") %>%
  group_by(PID) %>%
  summarise(filler.m = mean(Correct)) %>%
  arrange(filler.m)
# One child below 75% correct on fillers
flr.rem <-  as.character(flr.perf$PID[which(flr.perf$filler.m < 0.75)])

# Check for possible hearing problems with adults
frq.perf <- poa.all %>%
  filter(Contrast == "Frequent" & AgeGrp == 5) %>%
  group_by(PID) %>%
  summarise(frequent.m = mean(Correct)) %>%
  arrange(frequent.m)
# Two adults below 75% on frequent contrasts
frq.rem <-  as.character(frq.perf$PID[which(frq.perf$frequent.m < 0.75)])

# Remove those three participants
poa.all <-  poa.all %>%
  left_join(flr.perf) %>%
  left_join(frq.perf) %>%
  filter(filler.m >= 0.75 & is.na(frequent.m) | frequent.m >= 0.75)

### Item exclusions ###
# Based just on adult judgments
itm.perf <-  poa.all %>%
  filter(AgeGrp == 5) %>%
  group_by(Pair, StimType, Contrast, Closure) %>%
  summarise(itm.m = mean(Correct))
# One stimulus pair is below chance: 50% and
# one pair is below 75% performance overall
itm.rem <-  as.character(itm.perf$Pair[
  which(itm.perf$itm.m < 0.75)])

# Remove those stimuli
poa.all <-  poa.all %>%
  left_join(itm.perf) %>%
  filter(itm.m >= 0.75)

# Plot the by-item averages for adults pre-item exclusion
stim.scores <-  ggplot(itm.perf, aes(x = Pair, y = itm.m)) +
  geom_col(aes(colour = Contrast, fill = Contrast)) +
  facet_grid(. ~ Closure, scales = "free_x", space = "free") +
  scale_colour_manual(values = rare.palette) +
  scale_fill_manual(values = rare.palette) +
  coord_cartesian(ylim=c(0,1)) +
      ylab("Proportion correct") + xlab("Stimulus")	+
  geom_hline(yintercept = .75, linetype = "dashed") +
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

ggsave(plot = stim.scores,
       filename = "SUP-correct_by_item-postPtcpExcl-preItemExcl.png",
       path = results.plot.path,
       width = 70,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Plot the by-item averages for adults post-item exclusion
itm.perf.excl <-  poa.all %>%
  filter(AgeGrp == 5) %>%
  group_by(Pair, StimType, Contrast, Closure) %>%
  summarise(itm.m = mean(Correct))

stim.scores.excl <-  ggplot(
  itm.perf.excl, aes(x = Pair, y = itm.m)) +
  geom_col(aes(colour = Contrast, fill = Contrast)) +
  facet_grid(. ~ Closure, scales = "free_x", space = "free") +
  scale_colour_manual(values = rare.palette) +
  scale_fill_manual(values = rare.palette) +
  coord_cartesian(ylim=c(0,1)) +
      ylab("Proportion correct") + xlab("Stimulus")	+
  geom_hline(yintercept = .75, linetype = "dashed") +
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

ggsave(plot = stim.scores.excl,
       filename = "SUP-correct_by_item-postPtcpExcl-postItemExcl.png",
       path = results.plot.path,
       width = 70,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Sanity check #################################################################
################################################################################
poa.all$AgeGrpBin <- ifelse(poa.all$AgeGrp < 5, "Child", "Adult")
by.pair.avgs <- poa.all %>%
  filter(StimType != "filler") %>%
  group_by(AgeGrpBin, Pair, Word) %>%
  summarize(avg.cor = mean(Correct))
k.pair.avgs <- by.pair.avgs[grep("k", by.pair.avgs$Pair),]
k.pair.avgs$Pair = factor(k.pair.avgs$Pair, levels = c(
  "p-k", "t-k", "d-k", "dp-kp"))
k.pair.avgs$Word = factor(k.pair.avgs$Word, levels = c(
  "pAA", "tAA", "dee", "dpNee", "kAA", "kee", "kpNee",
  "dii", "dpI", "kEmkEm", "kma", "ndAA", "nee", "ngomo",
  "nnuu", "ntAA", "nuu", "pala", "te", "tii", "tpee", "tpI",
  "wNAA", "yi"))

by.pair.agegrp.perf <-  ggplot(
  k.pair.avgs, aes(x = Word, y = avg.cor)) +
  geom_col(aes(colour = Pair, fill = Pair)) +
  facet_wrap(~ AgeGrpBin + Pair, ncol=4, scales = "free") +
  coord_cartesian(ylim=c(0,1)) +
      ylab("Proportion correct") + xlab("Word")	+
  geom_hline(yintercept = .75, linetype = "dashed") +
	basic.theme

ggsave(plot = by.pair.agegrp.perf,
       filename = "SUP-correct_by_item_k-pairs-postPtcpExcl-postItemExcl.png",
       path = results.plot.path,
       width = 60,
       height = 45,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Analyze data #################################################################
################################################################################
poa.all$ContrastFR <- factor(poa.all$Contrast,
                            levels=c("Frequent", "Rare", "Filler"))
poa.all$ClosureSI <- factor(poa.all$Closure,
                            levels=c("Single", "Double", "Filler"))
poa.a <- poa.all %>% filter(AgeGrp == 5 & Contrast != "Filler")
poa.c <- poa.all %>% filter(AgeGrp < 5 & Contrast != "Filler")


# Hypotheses for adults:
# - No effect of contrast rarity
# - No effect of # of closures
# - No # of closure-by-contrast rarity effect
a.m0 <- glmer(Correct ~ (1|PID) + (1|Pair) + (1|Word),
            data = poa.a, family = "binomial",
             glmerControl(optimizer="bobyqa",
                          optCtrl = list(maxfun = 100000)))

a.m <- glmer(Correct ~ ContrastFR * ClosureSI +
               (1|PID) + (1|Pair) + (1|Word),
             data = poa.a, family = "binomial",
             glmerControl(optimizer="bobyqa",
                          optCtrl = list(maxfun = 100000)))


# Hypotheses for children:
# - Improvement with age
# - Effect of contrast rarity
#    - Gets smaller with age
# - No effect of # of closures
#    - No # of closure-by-contrast rarity effect

c.m0 <- glmer(Correct ~ (1|PID) + (1|Pair) + (1|Word),
            data = poa.c, family = "binomial",
             glmerControl(optimizer="bobyqa",
                          optCtrl = list(maxfun = 100000)))

c.m <- glmer(Correct ~ AgeNum * ContrastFR + ContrastFR * ClosureSI +
               (1|PID) + (1|Pair) + (1|Word),
             data = poa.c, family = "binomial",
             glmerControl(optimizer="bobyqa",
                          optCtrl = list(maxfun = 100000)))

# Try to justify addition of nuisance variables: Age + Multilingual
c.m.n1 <- glmer(Correct ~ AgeNum * ContrastFR + ContrastFR * ClosureSI +
               Gender +
               (1|PID) + (1|Pair) + (1|Word),
             data = poa.c, family = "binomial",
             glmerControl(optimizer="bobyqa",
                          optCtrl = list(maxfun = 100000)))
anova(c.m, c.m.n1)

c.m.n2 <- glmer(Correct ~ AgeNum * ContrastFR + ContrastFR * ClosureSI +
               Multilingual +
               (1|PID) + (1|Pair) + (1|Word),
             data = poa.c, family = "binomial",
             glmerControl(optimizer="bobyqa",
                          optCtrl = list(maxfun = 100000)))
anova(c.m, c.m.n2)


# Make plots ###################################################################
################################################################################
##### Overall: single + double articulation #####
pid.avg <- poa.all %>%
  group_by(PID, AgeNum, StimType, Contrast) %>%
  summarise(m.corr = mean(Correct)) %>%
  group_by(PID) %>%
  mutate(fitline = ifelse(AgeNum < 14, "C", "A"))

pid.avg.c <- pid.avg %>% filter(fitline == "C")
pid.avg.a <- pid.avg %>% filter(fitline == "A")


### Adult performance ###
corr.adu <- ggplot(pid.avg.a, aes(x = Contrast, y = m.corr)) +
  geom_pirate(aes(color=Contrast, fill=Contrast)) +
  scale_color_manual(values = rare.palette) +
  scale_fill_manual(values = rare.palette) +
  coord_cartesian(ylim=c(0,1)) +
      ylab("Proportion correct") + xlab("Stimulus type")	+
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

ggsave(plot = corr.adu,
       filename = "FIN-correct_by_type-adultsOnly.png",
       path = results.plot.path,
       width = 20,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

### Child + adult performance ###
# Overall
corr.age <- ggplot() +
  geom_jitter(data = pid.avg.c,
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              size=6, alpha=0.6) +
  geom_jitter(data = pid.avg.a,
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              size=6, alpha=0.1) +
  geom_smooth(data = pid.avg.c,
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              method = "lm", size=3) +
  geom_violin(data = pid.avg.a,
              aes(x = AgeNum, y = m.corr, fill=Contrast),
              alpha = 0.6, trim = TRUE,
              draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_x_continuous(breaks=c(2,4,6,8,10,12,15),
                     labels = c("2","4","6","8","10","12", "A")) +
  geom_hline(yintercept = .50, linetype = "dashed") +
  scale_colour_manual(values = rare.palette) +
  scale_fill_manual(values = rare.palette) +
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

##### Closure: single vs. double articulation #####
pid.avg.cl <- poa.all %>%
  group_by(PID, AgeNum, StimType, Contrast, Closure) %>%
  summarise(m.corr = mean(Correct)) %>%
  group_by(PID) %>%
  mutate(fitline = ifelse(AgeNum < 14, "C", "A"))

pid.avg.cl.c <- pid.avg.cl %>% filter(fitline == "C")
pid.avg.cl.a <- pid.avg.cl %>% filter(fitline == "A")


### Adult performance ###
corr.adu.cl <- ggplot(pid.avg.cl.a, aes(x = Contrast, y = m.corr)) +
  facet_grid(~ Closure, scales = "free_x", space = "free") +
  geom_pirate(aes(color=Contrast, fill=Contrast)) +
  scale_color_manual(values = rare.palette) +
  scale_fill_manual(values = rare.palette) +
  coord_cartesian(ylim=c(0,1)) +
      ylab("Proportion correct") + xlab("Stimulus type")	+
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

ggsave(plot = corr.adu.cl,
       filename = "FIN-correct_by_type_and_closure-adultsOnly.png",
       path = results.plot.path,
       width = 25,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

### Child + adult performance ###
# Overall
corr.age.cl <- ggplot() +
  facet_grid(~ Closure, scales = "free_x", space = "free") +
  geom_jitter(data = pid.avg.cl.c,
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              size=6, alpha=0.6) +
  geom_jitter(data = pid.avg.cl.a,
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              size=6, alpha=0.1) +
  geom_smooth(data = pid.avg.cl.c,
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              method = "lm", size=3) +
  geom_violin(data = pid.avg.cl.a,
              aes(x = AgeNum, y = m.corr, fill=Contrast),
              alpha = 0.6, trim = TRUE,
              draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_x_continuous(breaks=c(2,4,6,8,10,12,15),
                     labels = c("2","4","6","8","10","12", "A")) +
  geom_hline(yintercept = .50, linetype = "dashed") +
  scale_colour_manual(values = rare.palette) +
  scale_fill_manual(values = rare.palette) +
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

ggsave(plot = corr.age.cl,
       filename = "FIN-correct_by_age_and_closure.png",
       path = results.plot.path,
       width = 60,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

source("POA-buildup_figs.R")
