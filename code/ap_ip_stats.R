library(tidyverse)
library(lme4)
library(lmerTest)

# This is the code used in the AP juncture study presented in Section 6.4.4
# Load data
full_data <- read_csv("../data/ap_data_final.csv")

# The AP data file includes data from relative clauses too, but
# we omit these for simplicity.
full_data <- full_data %>% 
  filter(Type != 'relative')

# Fit statistical models
dur_lme <- lmer(Duration ~ HasCoda + Role + Num.Syls + (1|Subject), 
                data=full_data)
summary(dur_lme)

mean_f0_lme <- lmer(Mean.f0 ~ Role + (1|Subject), 
                    data=full_data)
summary(mean_f0_lme)

int_lme <- lmer(Intensity ~ Role + (1|Subject), 
                data=full_data)
summary(int_lme)

# Generate plots
ggplot(data=full_data, aes(x=HasCoda, y=C.Duration, fill=Role)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0, size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        plot.title = element_text(size=30, hjust=0.5),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20)) +
  xlab("Stem ends in coda") +
  ylab("Duration (Z Score)") +
  scale_fill_discrete(name = "Subjecthood", labels = c("Non-subject", "Subject"))
  ggsave("ap_ip_plot_duration.pdf")

  ggplot(data=full_data, aes(x=HasCoda, y=C.Intensity, fill=Role)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0, size=20),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=25),
          axis.title.y = element_text(size=25),
          plot.title = element_text(size=30, hjust=0.5),
          legend.title=element_text(size=20),
          legend.text=element_text(size=20)) +
    xlab("Stem ends in coda") +
    ylab("Intensity (Z Score)") +
    scale_fill_discrete(name = "Subjecthood", labels = c("Non-subject", "Subject"))
  ggsave("ap_ip_plot_intensity.pdf")

  ggplot(data=full_data, aes(x=HasCoda, y=C.Mean.f0, fill=Role)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0, size=20),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=25),
          axis.title.y = element_text(size=25),
          plot.title = element_text(size=30, hjust=0.5),
          legend.title=element_text(size=20),
          legend.text=element_text(size=20)) +
    xlab("Stem ends in coda") +
    ylab("Mean f0 (Z Score)") +
    scale_fill_discrete(name = "Subjecthood", labels = c("Non-subject", "Subject"))
  ggsave("ap_ip_plot_mean_f0.pdf")
