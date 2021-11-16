library(tidyverse)
library(lme4)
library(lmerTest)
library(ggpubr)

# This is the code used in the stress study presented in Section 6.3.

data <- read.csv("../data/stress_data_final.csv")
# Make Subject a factor and change ordering of Stressed factor
data <- data %>% 
  mutate(Subject = as_factor(Subject),
         Stressed = fct_relevel(Stressed, "unstressed", "stressed"))

# Run statistical models
m_duration <- lmer(Duration ~ Stressed + Word.position * Sentence.position + (1|Word) + (1|Subject), data=data)
summary(m_duration)

m_intensity <- lmer(Intensity ~ Stressed + Word.position * Sentence.position + (1|Word) + (1|Subject), data=data)
summary(m_intensity)

m_pitch <- lmer(Pitch ~ Stressed + Word.position * Sentence.position + (1|Word) + (1|Subject), data=data)
summary(m_pitch)

# Create plots

initial <- data[data$Sentence.position == "initial",]
medial <- data[data$Sentence.position == "medial",]

axis_text <- 15
axis_title <- 20
legend_title <- 20

p1 <- ggplot(initial, aes(x=Word.position, y=Pitch_z, fill=Stressed)) +
  ylim(-2, 4) +
  geom_boxplot() +
  labs(x = "Position in word", y = "f0 (Z score)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size=axis_text),
        axis.title = element_text(size=axis_title),
        legend.text = element_text(size=legend_title)) +
  scale_x_discrete(labels=c("first" = "Initial syllable", 
                            "second" = "Final syllable"))

p2 <- ggplot(medial, aes(x=Word.position, y=Pitch_z, fill=Stressed)) +
  geom_boxplot() +
  ylim(-2, 4) +
  labs(x="Position in word", y="f0 (Z score)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size=axis_text),
        axis.title = element_text(size=axis_title),
        legend.text = element_text(size=legend_title)) +
  scale_x_discrete(labels=c("first" = "Initial syllable", 
                            "second" = "Final syllable"))

ggarrange(p1, p2,
          labels = c("Sentence-initial", "Sentence-medial"),
          ncol = 2, nrow = 1,
          common.legend = TRUE)
ggsave("fig6-5.pdf")

# Duration plots
p3 <- ggplot(initial, aes(x=Word.position, y=Duration_z, fill=Stressed)) +
  geom_boxplot() +
  ylim(-3, 3) +
  labs(x="Position in word", y="Duration (Z score)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size=axis_text),
        axis.title = element_text(size=axis_title),
        legend.text = element_text(size=legend_title)) +
  scale_x_discrete(labels=c("first" = "Initial syllable", 
                            "second" = "Final syllable"))

p4 <- ggplot(medial, aes(x=Word.position, y=Duration_z, fill=Stressed)) +
  geom_boxplot() +
  ylim(-3, 3) +
  labs(x="Position in word", y="Duration (Z score)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size=axis_text),
        axis.title = element_text(size=axis_title),
        legend.text = element_text(size=legend_title)) +
  scale_x_discrete(labels=c("first" = "Initial syllable", 
                            "second" = "Final syllable"))

ggarrange(p3, p4,
          labels = c("Sentence-initial", "Sentence-medial"),
          ncol = 2, nrow = 1,
          common.legend = TRUE)
ggsave("fig6-3.pdf")

# Intensity plots
p5 <- ggplot(initial, aes(x=Word.position, y=Intensity_z, fill=Stressed)) +
  geom_boxplot() +
  ylim(-3, 3) +
  labs(x="Position in word", y="Intensity (Z score)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size=axis_text),
        axis.title = element_text(size=axis_title),
        legend.text = element_text(size=legend_title)) +
  scale_x_discrete(labels=c("first" = "Initial syllable", 
                            "second" = "Final syllable"))

p6 <- ggplot(medial, aes(x=Word.position, y=Intensity_z, fill=Stressed)) +
  geom_boxplot() +
  ylim(-3, 3) +
  labs(x="Position in word", y="Intensity (Z score)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size=axis_text),
        axis.title = element_text(size=axis_title),
        legend.text = element_text(size=legend_title)) +
  scale_x_discrete(labels=c("first" = "Initial syllable", 
                            "second" = "Final syllable"))

ggarrange(p5, p6,
          labels = c("Sentence-initial", "Sentence-medial"),
          ncol = 2, nrow = 1,
          common.legend = TRUE)
ggsave("fig6-4.pdf")

# Duration in seconds
p7 <- ggplot(initial, aes(x=Word.position, y=Duration, fill=Stressed)) +
  geom_boxplot() +
  ylim(0, 0.3) +
  labs(x="Position in word", y="Duration (seconds)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size=axis_text),
        axis.title = element_text(size=axis_title),
        legend.text = element_text(size=legend_title)) +
  scale_x_discrete(labels=c("first" = "Initial syllable", 
                            "second" = "Final syllable"))

p8 <- ggplot(medial, aes(x=Word.position, y=Duration, fill=Stressed)) +
  geom_boxplot() +
  ylim(0, 0.3) +
  labs(x="Position in word", y="Duration (seconds)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size=axis_text),
        axis.title = element_text(size=axis_title),
        legend.text = element_text(size=legend_title)) +
  scale_x_discrete(labels=c("first" = "Initial syllable", 
                            "second" = "Final syllable"))

ggarrange(p7, p8,
          labels = c("Sentence-initial", "Sentence-medial"),
          ncol = 2, nrow = 1,
          common.legend = TRUE)
ggsave("duration_seconds.pdf")
