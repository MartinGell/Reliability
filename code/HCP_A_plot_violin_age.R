
library(tidyverse)


beh = 'Age'

f = 'ridgeCV_zscore_averaged-source_Schaefer400x17_WM+CSF+GS_hcpaging_695_zscored-beh_interview_age_wnoise_all.csv'
plt_out = '/home/mgell/Work/reliability/plots/simulation_res/'

d = read_csv(paste0('/home/mgell/Work/reliability/input/',f))
d$test_MAE = d$test_MAE*-1

empirical = d[1,]
d = d[-1,]

metrics = colnames(d)
rel_max = max(d$reliability)
rel_min = min(d$reliability)


# Violins
plt1 = ggplot(d,aes(reliability,test_R2,group=as.factor(reliability))) + 
  geom_violin(color = 'deeppink3', width=0.1, position=position_dodge(width = 0)) + 
  #  geom_violin(color = 'deeppink3') +
  geom_boxplot(colour = "grey", width=0.01, position=position_dodge(width = 0),outlier.size = 0) +
  theme_classic() +
  ylim(c(-0.1,0.8)) + 
  scale_x_continuous(limits = c(0.45,1.05), breaks = c(seq(0.5,1.0,0.1))) +
  xlab('Reliability') + 
  ylab('Age Prediction Accuracy (R2)') +
  theme(legend.position="none") +
  geom_point(aes(empirical$reliability,empirical$test_R2), size = 3, colour = 'skyblue3')
#ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt1, width=3.5, height=3)
ggsave(paste0(plt_out,'OHBM',beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt1, width=3.5, height=3)



plt2 = ggplot(d,aes(as.factor(reliability),test_MAE)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(80,180)) + xlab('Reliability') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_MAE), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[5],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt2, width=3.5, height=3)


plt3 = ggplot(d,aes(as.factor(reliability),test_r)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(0.0,1.0)) + xlab('Reliability') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_r), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[9],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt3, width=3.5, height=3)








beh = 'Age_Seitzman'

f = 'ridgeCV_zscore_averaged-source_Seitzman_nodes300_WM+CSF+GS_hcpaging_650_zscored-beh_interview_age_wnoise_all.csv'
plt_out = '/home/mgell/Work/reliability/plots/simulation_res/'

d = read_csv(paste0('/home/mgell/Work/reliability/input/',f))
d$test_MAE = d$test_MAE*-1

empirical = d[1,]
d = d[-1,]

metrics = colnames(d)
rel_max = max(d$reliability)
rel_min = min(d$reliability)


# Violins
plt1 = ggplot(d,aes(as.factor(reliability),test_R2)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(-0.1,0.8)) + xlab('Reliability') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_R2), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt1, width=3.5, height=3)


plt2 = ggplot(d,aes(as.factor(reliability),test_MAE)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(80,180)) + xlab('Reliability') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_MAE), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[5],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt2, width=3.5, height=3)


plt3 = ggplot(d,aes(as.factor(reliability),test_r)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(0.0,1.0)) + xlab('Reliability') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_r), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[9],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt3, width=3.5, height=3)










beh = 'age_SVR'

f = 'svr_heuristic_zscore_averaged-source_Schaefer400x17_WM+CSF+GS_hcpaging_695_zscored-beh_interview_age_wnoise_all.csv'
plt_out = '/home/mgell/Work/reliability/plots/simulation_res/'

d = read_csv(paste0('/home/mgell/Work/reliability/input/',f))
d$test_MAE = d$test_MAE*-1

empirical = d[1,]
d = d[-1,]

metrics = colnames(d)
rel_max = max(d$reliability)
rel_min = min(d$reliability)



# Violins
plt1 = ggplot(d,aes(as.factor(reliability),test_R2)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(-0.11,0.8)) + xlab('Reliability') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_R2), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt1, width=3.5, height=3)


plt2 = ggplot(d,aes(as.factor(reliability),test_MAE)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(80,180)) + xlab('Reliability') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_MAE), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[5],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt2, width=3.5, height=3)


plt3 = ggplot(d,aes(as.factor(reliability),test_r)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(0.0,1.0)) + xlab('Reliability') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_r), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[9],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt3, width=3.5, height=3)





# get mean and sd of prediction at each reliability band
df_m = d %>% group_by(reliability) %>%
  summarise_at(vars(metrics[c(-1,-2,-13,-14)]), list(name = mean))
colnames(df_m)[2:11] <- metrics[c(-1,-2,-13,-14)]

df_sd = d %>% group_by(reliability) %>%
  summarise_at(vars(metrics[c(-1,-2,-13,-14)]), list(name = sd))
colnames(df_sd)[2:11] <- metrics[c(-1,-2,-13,-14)]

