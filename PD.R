rm(list=ls())

library(glmnet)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(ggpmisc)
library(table1)
library(scales)
library(Rmisc)

datapath='C:/Users/PD'
savepath='C:/Users/PD'

setwd(datapath)

data1 <- read.csv('new_final_list1_update_2.csv', header = TRUE)
data2 <- read.csv('newPDtest.csv', header = TRUE)

for (iz in 1:dim(data1)[1]) {
  data1[iz, 'individual_ID'] <- paste0(
    data1$Freesufer_Path1[iz],
    data1$Freesufer_Path2[iz],
    data1$Freesufer_Path3[iz],
    sep = "_"
  )
}

data1 <- data1 %>% distinct(individual_ID, .keep_all = TRUE)
rownames(data1) <- data1$individual_ID

for (iz in 1:dim(data2)[1]) {
  data2[iz, 'individual_ID'] <- paste0(
    data2$Freesufer_Path1[iz],
    data2$Freesufer_Path2[iz],
    data2$Freesufer_Path3[iz],
    sep = "_"
  )
}

data2 <- data2 %>% distinct(individual_ID, .keep_all = TRUE)
rownames(data2) <- data2$individual_ID

ind <- intersect(rownames(data1), rownames(data2))
data2 <- cbind(data2[ind, ], data1[ind, ])

data2[data2$ml_stage == 0, 'Subtype'] <- 'PD.NA'
data2[data2$ml_stage != 0 & data2$ml_subtype == 0, 'Subtype'] <- 'PD-putamen'
data2[data2$ml_stage != 0 & data2$ml_subtype == 1, 'Subtype'] <- 'PD-cerebellum'
data2[data2$ml_stage != 0 & data2$ml_subtype == 2, 'Subtype'] <- 'PD-cortex'

data2 <- data2[data2$Age > 40 & data2$Age < 100, ]

data2[, 'PD-putamen'] <- data2$prob_S0
data2[, 'PD-cerebEllum'] <- data2$prob_S1
data2[, 'PD-cortex'] <- 1 - (data2$prob_S1 + data2$prob_S0)

data2$Subtype <- factor(
  data2$Subtype,
  levels = c('PD.NA', 'PD-putamen', 'PD-cerebellum', 'PD-cortex')
)

data2$MMSE <- as.numeric(as.character(data2$MMSE))
data2$DBS_improvement_rate_off[data2$DBS_improvement_rate_off < 0] <- NA

setwd(savepath)

data3 <- data2[!is.na(data2$Preoperative_med_improvement_rate) & data2$ml_stage > 0, ]

data3[data3$ml_stage > 0 & data3$ml_stage <= 3, 'new_stage'] <- 'Phase1'
data3[data3$ml_stage > 3 & data3$ml_stage <= 10, 'new_stage'] <- 'Phase2'
data3[data3$ml_stage > 10 & data3$ml_stage <= 30, 'new_stage'] <- 'Phase3'

data3$Subtype <- factor(
  data3$Subtype,
  levels = c("PD-putamen", "PD-cerebellum", "PD-cortex")
)

new_data <- summarySE(
  data3,
  measurevar = 'Preoperative_med_improvement_rate',
  groupvars = c('Subtype', 'new_stage')
)

results <- data.frame()

for(sub in levels(data3$Subtype)){
  temp <- subset(data3, Subtype == sub)
  for(phase in c("Phase2", "Phase3")){
    test_data <- subset(temp, new_stage %in% c("Phase1", phase))
    p_val <- wilcox.test(
      DBS_improvement_rate_off ~ new_stage,
      data = test_data
    )$p.value
    results <- rbind(
      results,
      data.frame(Subtype = sub, phase = phase, pval = p_val)
    )
  }
}

star_data <- data.frame(
  new_stage = "Phase2",
  y = 0.95,
  Subtype = "PD-putamen"
)

ggplot(
  new_data,
  aes(
    x = new_stage,
    y = Preoperative_med_improvement_rate,
    group = Subtype,
    color = Subtype,
    shape = Subtype
  )
) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(
      ymin = Preoperative_med_improvement_rate - se,
      ymax = Preoperative_med_improvement_rate + se
    ),
    width = .05
  ) +
  theme_classic() +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(-1, 1, 0.2),
    labels = percent_format(accuracy = 1)
  ) +
  ylab("Motor improvement rate\n(pre-med-on)") +
  xlab(NULL) +
  scale_x_discrete(
    labels = c(
      "Phase1" = "Early atrophy phase\n(stage 1-3)",
      "Phase2" = "Middle atrophy phase\n(stage 4-10)",
      "Phase3" = "Late atrophy phase\n(stage >10)"
    )
  ) +
  scale_shape_manual(
    values = c(
      "PD-putamen" = 16,
      "PD-cerebellum" = 16,
      "PD-cortex" = 16
    )
  ) +
  theme(
    legend.position = c(0, 0),
    legend.justification = c(0, 0),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "transparent", colour = NA),
    axis.title.x = element_text(color = "black", size = 10),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    axis.ticks = element_line(color = "black")
  ) +
  geom_text(
    data = star_data,
    aes(x = new_stage, y = y, color = Subtype),
    label = "*",
    size = 8,
    show.legend = FALSE
  )

ggsave("Phase_for_medication.png", width = 6, height = 3, dpi = 300)
ggsave("Phase_for_medication.pdf", width = 6, height = 3)
