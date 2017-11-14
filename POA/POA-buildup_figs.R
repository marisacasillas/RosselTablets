# Build-up plots
corr.age.0 <- ggplot() +
  geom_jitter(data = pid.avg.a,
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              size=6, alpha=0.1) +
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
		  element_text(size=20),
		legend.position = "none")

corr.age.1 <- ggplot() +
  geom_jitter(data = subset(pid.avg.c, Contrast == "Filler"),
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              size=6, alpha=0.6) +
  geom_smooth(data = subset(pid.avg.c, Contrast == "Filler"),
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              method = "lm", size=3) +
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
		  element_text(size=20),
		legend.position = "none")

corr.age.2 <- ggplot() +
  geom_jitter(data = subset(pid.avg.c, Contrast == "Frequent"),
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              size=6, alpha=0.6) +
  geom_smooth(data = subset(pid.avg.c, Contrast == "Frequent"),
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              method = "lm", size=3) +
  scale_x_continuous(breaks=c(2,4,6,8,10,12,15),
                     labels = c("2","4","6","8","10","12", "A")) +
  geom_hline(yintercept = .50, linetype = "dashed") +
  scale_colour_manual(values = rare.paletteGN) +
  scale_fill_manual(values = rare.paletteGN) +
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
		  element_text(size=20),
		legend.position = "none")

corr.age.3 <- ggplot() +
  geom_jitter(data = subset(pid.avg.c, Contrast == "Rare"),
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              size=6, alpha=0.6) +
  geom_smooth(data = subset(pid.avg.c, Contrast == "Rare"),
              aes(x = AgeNum, y = m.corr, color=Contrast, fill=Contrast),
              method = "lm", size=3) +
  scale_x_continuous(breaks=c(2,4,6,8,10,12,15),
                     labels = c("2","4","6","8","10","12", "A")) +
  geom_hline(yintercept = .50, linetype = "dashed") +
  scale_colour_manual(values = rare.paletteRD) +
  scale_fill_manual(values = rare.paletteRD) +
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
		  element_text(size=20),
		legend.position = "none")

ggsave(plot = corr.age.0,
       filename = "FIN-correct_by_age_0.png",
       path = results.plot.path,
       width = 30,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

ggsave(plot = corr.age.1,
       filename = "FIN-correct_by_age_1.png",
       path = results.plot.path,
       width = 30,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

ggsave(plot = corr.age.2,
       filename = "FIN-correct_by_age_2.png",
       path = results.plot.path,
       width = 30,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

ggsave(plot = corr.age.3,
       filename = "FIN-correct_by_age_3.png",
       path = results.plot.path,
       width = 30,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)