
library(data.table)
library(tidyverse)

#net_cols <- c("#0072B2", "#009E73", "#D55E00", "#56B4E9", "#E69F00", "#F0E442", "#CC79A7")
net_cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# AGE
d_subsample = read_csv('/home/mgell/Work/reliability/input/learning_curve_collected/ridgeCV_zscore-source_Schaefer400x17_nodenoise_UKB_5000_z-beh_Age_when_attended_assessment_centre_wnoise_mean_all_new.csv')
colnames(d_subsample) <- c("250","403","652","1054","1704","2753","4450", "reliability")

d <- melt(setDT(d_subsample), id.vars = "reliability", variable.name = "sample",variable.factor = FALSE)
d$sample <- as.numeric(d$sample)


plt = ggplot(d, aes(reliability,value,colour=as.factor(sample))) + geom_point(size = 1.5, alpha = 0.3) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),se = FALSE) +
  theme_classic() + ylim(c(-0.1,0.4)) + 
  ylab('Accuracy (R2)') +
  xlab('Reliability') +
  scale_colour_manual(values = net_cols, name = 'Training set size')
ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/Age_R2_wnoise_rel_05_to_09.png'), plt, width=4.5, height=3)
#ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/OHBM_Age_R2_wnoise_rel_05_to_09.png'), plt, width=4, height=3)

# dif view
plt2 = ggplot(d2, aes(sample, value, colour = as.factor(reliability))) + geom_point(size = 1.5, alpha = 0.3) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 5),se = FALSE) +
  theme_classic() +
  ylab('Accuracy (R2)') +
  labs(colour = 'Reliability') +
  xlab('Training set size') +
  scale_colour_manual(values = net_cols)
ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/Age_R2_wnoise_rel_05_to_09_alternative.png'), plt2, width=5, height=3)

# # zoom
# plt3 = ggplot(d2, aes(sample, value, colour = as.factor(reliability))) + geom_point(size = 1.5, alpha = 0.3) +
#   geom_smooth(method = lm, formula = y ~ log(x), se = FALSE) +
#   theme_classic() + ylab('Accuracy (R2)') +
#   xlim(c(200,2000)) + 
#   scale_y_continuous(limits = c(-0.05,0.25), breaks = c(-0.05,0,0.05,0.15,0.25)) +
#   labs(colour = 'Reliability') +
#   scale_colour_manual(values = net_cols) +
#   xlab('Training set size')
# ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/Age_R2_wnoise_rel_05_to_09_zoomed.png'), plt3, width=4, height=3)

d2 = d
d2 = d2[d2$reliability < 1.0,]
plt3 = ggplot(d2, aes(sample, value, colour = as.factor(reliability))) + geom_point(size = 1.5, alpha = 0.3) +
  geom_smooth(method = lm, formula = y ~ log(x), se = FALSE) +
  theme_classic() + ylab('Accuracy (R2)') +
  scale_x_continuous(limits = c(200,2000), breaks = c(250,500,1000,2000,2000)) + 
  scale_y_continuous(limits = c(-0.05,0.3), breaks = c(-0.05,0,0.05,0.15,0.25)) +
  labs(colour = 'Reliability') +
  scale_colour_manual(values = net_cols) + 
  xlab('Training set size')
ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/Age_R2_wnoise_rel_05_to_09_zoomed2.png'), plt3, width=4, height=3)


d_subsample %>% group_by(reliability) %>% summarise_at(colnames(d_subsample)[1:7], list(name = mean))
d_subsample %>% group_by(reliability) %>% summarise_at(colnames(d_subsample)[1:7], list(name = sd))


means = d_subsample %>% group_by(reliability) %>% summarise_at(colnames(d_subsample)[1:7], list(name = mean))
write_csv(means, 'temp.csv')
sds = d_subsample %>% group_by(reliability) %>% summarise_at(colnames(d_subsample)[1:7], list(name = sd))
write_csv(sds, 'temp2.csv')

df_sum = read_csv('/home/mgell/Work/reliability/input/UKB_age_mean_sd_samples.csv')
df_sum$reliability = as.factor(df_sum$reliability)

plt3 = ggplot(df_sum, aes(x = sample)) + 
  geom_point(aes(y = R2_mean, colour = reliability), size=1) +
  geom_line(aes(y = R2_mean, colour = reliability), size=0.8) +
  geom_ribbon(aes(y = R2_mean, ymin = R2_mean - 2*R2_sd, ymax = R2_mean + 2*R2_sd, fill = reliability), alpha = .1) +
  theme_classic() + 
  scale_x_continuous(limits = c(200,2000), breaks = c(250,500,1000,2000,2000)) + 
  scale_y_continuous(limits = c(-0.05,0.3), breaks = c(-0.05,0,0.05,0.15,0.25)) +
  scale_colour_manual(values = net_cols, name = "Reliability",
                      labels = unique(df_sum$reliability)) +
  scale_fill_manual(values = net_cols, name = "Reliability",
                    labels = unique(df_sum$reliability)) +
  xlab('Training set size') +
  ylab('Age Prediction Accuracy (R2)')
ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/OHBM_Age_R2_wnoise_rel_05_to_09_zoomed2.png'), plt3, width=4, height=3)



# show just one reliability band
ggplot(d, aes(reliability,value,colour=as.factor(sample))) + geom_point(size = 1.5, alpha = 0.8, position = position_dodge(width = 0.05)) +
  theme_classic() +  
  ylab('Accuracy (R2)') +
  xlab('Reliability') + xlim(c(0.61,0.79)) +
  scale_colour_manual(values = net_cols, name = 'Training set size')






# HGS
d_subsample = read_csv('/home/mgell/Work/reliability/input/learning_curve_collected/ridgeCV_zscore-source_Schaefer400x17_nodenoise_UKB_5000_z-beh_Hand_grip_strength_mean_lr_wnoise_mean_all.csv')

d <- melt(setDT(d_subsample), id.vars = "reliability", variable.name = "sample",variable.factor = FALSE)
d$sample <- as.numeric(d$sample)


plt = ggplot(d, aes(reliability,value,colour=as.factor(sample))) + geom_point(size = 1.5, alpha = 0.3) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),se = FALSE) +
  theme_classic() + ylim(c(-0.1,0.4)) + 
  ylab('Accuracy (R2)') +
  xlab('r(original,simulated)') +
  scale_colour_manual(values = net_cols, name = 'Training set size')
ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/HGS_R2_wnoise_rel_05_to_09.png'), plt, width=4.5, height=3)

# dif view
plt2 = ggplot(d2, aes(sample, value, colour = as.factor(reliability))) + geom_point(size = 1.5, alpha = 0.3) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 5),se = FALSE) +
  theme_classic() +
  ylab('Accuracy (R2)') +
  labs(colour = 'r(original,simulated)') +
  xlab('Training set size') +
  ylim(c(min(d2$value),0.3)) +
  scale_colour_manual(values = net_cols)
ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/HGS_R2_wnoise_rel_05_to_09_alternative.png'), plt2, width=5, height=3)
# 
# # zoom
# plt3 = ggplot(d2, aes(sample, value, colour = as.factor(reliability))) + geom_point(size = 1.5, alpha = 0.3) +
#   geom_smooth(method = lm, formula = y ~ log(x), se = FALSE) +
#   theme_classic() + ylab('Accuracy (R2)') +
#   xlim(c(200,2000)) + 
#   scale_y_continuous(limits = c(-0.05,0.25), breaks = c(-0.05,0,0.05,0.15,0.25)) +
#   labs(colour = 'r(empirical,simulated)') +
#   scale_colour_manual(values = net_cols) +
#   xlab('Training set size')
# ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/HGS_R2_wnoise_rel_05_to_09_zoomed.png'), plt3, width=4, height=3)


d2 = d
d2 = d2[d2$reliability < 1.0,]
plt3 = ggplot(d2, aes(sample, value, colour = as.factor(reliability))) + geom_point(size = 1.5, alpha = 0.3) +
  geom_smooth(method = lm, formula = y ~ log(x), se = FALSE) +
  theme_classic() + ylab('Accuracy (R2)') +
  scale_x_continuous(limits = c(200,2000), breaks = c(250,500,1000,2000)) + 
  scale_y_continuous(limits = c(-0.05,0.3), breaks = c(-0.05,0,0.05,0.15,0.25)) +
  labs(colour = 'r(original,simulated)') +
  scale_colour_manual(values = net_cols) + 
  xlab('Training set size')
ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/HGS_R2_wnoise_rel_05_to_09_zoomed2.png'), plt3, width=4.2, height=3)


d_subsample %>% group_by(reliability) %>% summarise_at(colnames(d_subsample)[1:7], list(name = mean))
d_subsample %>% group_by(reliability) %>% summarise_at(colnames(d_subsample)[1:7], list(name = sd))


means = d_subsample %>% group_by(reliability) %>% summarise_at(colnames(d_subsample)[1:7], list(name = mean))
write_csv(means, 'temp.csv')
sds = d_subsample %>% group_by(reliability) %>% summarise_at(colnames(d_subsample)[1:7], list(name = sd))
write_csv(sds, 'temp2.csv')

df_sum = read_csv('/home/mgell/Work/reliability/input/UKB_grip_mean_sd_samples.csv')
df_sum$reliability = as.factor(df_sum$reliability)

plt3 = ggplot(df_sum, aes(x = sample)) + 
  geom_point(aes(y = R2_mean, colour = reliability), size=1) +
  geom_line(aes(y = R2_mean, colour = reliability), size=0.8) +
  geom_ribbon(aes(y = R2_mean, ymin = R2_mean - 2*R2_sd, ymax = R2_mean + 2*R2_sd, fill = reliability), alpha = .1) +
  theme_classic() + 
  scale_x_continuous(limits = c(200,2000), breaks = c(250,500,1000,2000,2000)) + 
  scale_y_continuous(limits = c(-0.05,0.3), breaks = c(-0.05,0,0.05,0.15,0.25)) +
  scale_colour_manual(values = net_cols, name = "r(original,simulated)",
                      labels = unique(df_sum$reliability)) +
  scale_fill_manual(values = net_cols, name = "r(original,simulated)",
                    labels = unique(df_sum$reliability)) +
  xlab('Training set size') +
  ylab('Accuracy (R2)')
ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/HGS_R2_wnoise_rel_05_to_09_zoomed2.png'), plt3, width=4.2, height=3)









# TMT
d_subsample = read_csv('/home/mgell/Work/reliability/input/learning_curve_collected/ridgeCV_zscore-source_Schaefer400x17_nodenoise_UKB_5000_z-beh_TMT_B_duration_to_complete_wnoise_mean_all.csv')

d <- melt(setDT(d_subsample), id.vars = "reliability", variable.name = "sample",variable.factor = FALSE)
d$sample <- as.numeric(d$sample)


plt = ggplot(d, aes(reliability,value,colour=as.factor(sample))) + geom_point(size = 1.5, alpha = 0.3) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3),se = FALSE) +
  theme_classic() + ylim(c(-0.1,0.1)) + 
  ylab('Accuracy (R2)') +
  xlab('r(original,simulated)') +
  scale_colour_manual(values = net_cols, name = 'Sample')
ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/TMT_B_R2_wnoise_rel_07_to_095.png'), plt, width=4, height=3)


# dif view
d2 = d
d2 = d2[d2$reliability < 1.0,]
plt2 = ggplot(d2, aes(sample, value, colour = as.factor(reliability))) + geom_point(size = 1.5, alpha = 0.3) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 5),se = FALSE) +
  theme_classic() + 
  ylab('Accuracy (R2)') +
  xlab('Training set size') +
  labs(colour = 'r(original,simulated)') +
  scale_colour_manual(values = net_cols)
ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/TMT_B_R2_wnoise_rel_07_to_095_alternative.png'), plt2, width=4.5, height=3)









ggplot(d, aes(sample,value)) + geom_point(size = 1.5, color = 'deeppink3') +
  geom_smooth(method = lm, formula = y ~ log(x)) + #, formula = y ~ splines::bs(x, 3)) +
  theme_classic() + ylim(c(-0.25,0.4))
#geom_point(aes(empirical$reliability,empirical$test_R2), size = 3, colour = 'skyblue3') + 
plt = ggplot(d, aes(reliability,value,colour=as.factor(sample))) + geom_point(size = 1.5) +
  #geom_smooth(method = lm, formula = y ~ log(x)) + #, formula = y ~ splines::bs(x, 3)) +
  theme_classic() + ylim(c(-0.1,0.4))
ggsave(paste0('/home/mgell/Work/reliability/plots/UKB/subsample_res/test_age_R2_wnoise_rel_05_to_095.png'), plt, width=4, height=3)

