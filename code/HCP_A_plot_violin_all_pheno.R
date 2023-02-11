
library(tidyverse)


beh = 'Grip'

f = 'ridgeCV_zscore_averaged-source_Schaefer400x17_WM+CSF+GS_hcpaging_695_zscored-beh_HCP_A_motor_wnoise_all.csv'
plt_out = '/home/mgell/Work/reliability/plots/simulation_res/'

d = read_csv(paste0('/home/mgell/Work/reliability/input/',f))
d$test_MAE = d$test_MAE*-1

empirical = d[1,]
d = d[-1,]

metrics = colnames(d)
rel_max = max(d$reliability)
rel_min = min(d$reliability)


plt1 = ggplot(d,aes(reliability,test_R2)) + geom_point(size = 1.5, color = 'deeppink3') + 
  theme_classic() + ylim(c(-0.1,0.2)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(empirical$reliability,empirical$test_R2), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt1, width=3.5, height=3)


plt2 = ggplot(d,aes(reliability,test_MAE)) + geom_point(size = 1.5, color = 'deeppink3') + 
  theme_classic() + ylim(c(9,13)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(empirical$reliability,empirical$test_MAE), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[5],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt2, width=3.5, height=3)


plt3 = ggplot(d,aes(reliability,test_r)) + geom_point(size = 1.5, color = 'deeppink3') + 
  theme_classic() + ylim(c(0.0,0.5)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(empirical$reliability,empirical$test_r), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[9],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt3, width=3.5, height=3)


# r2 with line
plt4 = ggplot(d,aes(reliability,test_R2)) + geom_point(size = 1.5, color = 'deeppink3') + 
  theme_classic() + ylim(c(-0.1,0.2)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(empirical$reliability,empirical$test_R2), size = 3, colour = 'skyblue3') + 
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3))
ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'_fitted.png'), plt4, width=3.5, height=3)


# Violins
plt1 = ggplot(d,aes(as.factor(reliability),test_R2)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(-0.1,0.3)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_R2), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt1, width=3.5, height=3)


plt2 = ggplot(d,aes(as.factor(reliability),test_MAE)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(8.5,14)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_MAE), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[5],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt2, width=3.5, height=3)


plt3 = ggplot(d,aes(as.factor(reliability),test_r)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(0.0,0.6)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_r), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[9],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt3, width=3.5, height=3)









beh = 'Total'

f = 'ridgeCV_zscore_averaged-source_Schaefer400x17_WM+CSF+GS_hcpaging_695_zscored-beh_HCP_A_total_wnoise_all.csv'
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
  theme_classic() + ylim(c(-0.1,0.3)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_R2), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt1, width=3.5, height=3)


plt2 = ggplot(d,aes(as.factor(reliability),test_MAE)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(8.5,14)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_MAE), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[5],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt2, width=3.5, height=3)


plt3 = ggplot(d,aes(as.factor(reliability),test_r)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(0.0,0.6)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_r), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[9],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt3, width=3.5, height=3)









beh = 'crycog'

f = 'ridgeCV_zscore_averaged-source_Schaefer400x17_WM+CSF+GS_hcpaging_695_zscored-beh_HCP_A_cryst_wnoise_all.csv'
plt_out = '/home/mgell/Work/reliability/plots/simulation_res/'

df = read_csv(paste0('/home/mgell/Work/reliability/input/',f))
df$test_MAE = df$test_MAE*-1

empirical = df[1,]
d = df[-1,]

metrics = colnames(d)
rel_max = max(d$reliability)
rel_min = min(d$reliability)


plt1 = ggplot(d,aes(reliability,test_R2)) + geom_point(size = 1.5, color = 'deeppink3') + 
  theme_classic() + ylim(c(-0.1,0.3)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(empirical$reliability,empirical$test_R2), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt1, width=3.5, height=3)

#with violin
ggplot(d,aes(as.factor(reliability),test_R2)) + geom_violin(size = 1.5, color = 'deeppink3') + 
  theme_classic() + ylim(c(-0.1,0.3)) + xlab('r(original,simulated)') + theme(legend.position="none")+
  geom_point(aes(as.factor(empirical$reliability),empirical$test_R2), size = 3, colour = 'skyblue3')

#dotplot
ggplot(d,aes(as.factor(reliability),test_R2)) + geom_dotplot(binaxis="y", stackdir="center", binwidth=0.003, fill = 'deeppink3') + 
  theme_classic() + ylim(c(-0.1,0.3)) + xlab('r(original,simulated)') + theme(legend.position="none")+
  geom_point(aes(as.factor(empirical$reliability),empirical$test_R2), size = 3, colour = 'skyblue3')

#dotplot zoom on good reliability
d2 = d[d$reliability   >= 0.6,]
d2 = d2[d2$reliability <= 0.8,]

rel_max2 = max(d2$reliability)
rel_min2 = min(d2$reliability)

pltx = ggplot(d2,aes(as.factor(reliability),test_R2)) + 
  geom_violin(fill = "#0072B2", colour  = "#0072B2", alpha = 0.2) +
  geom_dotplot(binaxis="y", stackdir="center", binwidth=0.004, fill = "#0072B2", alpha = 0.7) + 
  theme_classic() + ylim(c(-0.05,0.15)) + 
  xlab('r(original,simulated)') + ylab('R2') +
  theme(legend.position="none")
ggsave(paste0(plt_out,beh,'_dot_plot_',metrics[7],'_wnoise_rel_',rel_max2,'_to_',rel_min2,'.png'), pltx, width=3.5, height=3.5)


plt2 = ggplot(d,aes(reliability,test_MAE)) + geom_point(size = 1.5, color = 'deeppink3') + 
  theme_classic() + ylim(c(9,14)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(empirical$reliability,empirical$test_MAE), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[5],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt2, width=3.5, height=3)


plt3 = ggplot(d,aes(reliability,test_r)) + geom_point(size = 1.5, color = 'deeppink3') + 
  theme_classic() + ylim(c(0.0,0.6)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(empirical$reliability,empirical$test_r), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[9],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt3, width=3.5, height=3)


#
plt4 = ggplot(d,aes(reliability,test_R2)) + geom_point(size = 1.5, color = 'deeppink3') + 
  theme_classic() + ylim(c(-0.1,0.3)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(empirical$reliability,empirical$test_R2), size = 3, colour = 'skyblue3') + 
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3))
ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'_fitted.png'), plt4, width=3.5, height=3)

df_m = df %>% group_by(reliability) %>%
  summarise_at(vars(metrics[c(-1,-2,-13,-14)]), list(name = mean))
colnames(df_m)[2:11] <- metrics[c(-1,-2,-13,-14)]

df_sd = df %>% group_by(reliability) %>%
  summarise_at(vars(metrics[c(-1,-2,-13,-14)]), list(name = sd))
colnames(df_sd)[2:11] <- metrics[c(-1,-2,-13,-14)]

# pltm = ggplot(df_m, aes(reliability,test_R2)) + 
#   geom_point(size=1, colour = 'deeppink3') +
#   geom_line(size=0.8, colour = 'deeppink3') +
#   theme_classic() + ylim(c(-0.1,0.3))
# ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'_mean.png'), pltm, width=4.5, height=3.5)

df_m = df_m %>% select(c('reliability', 'test_R2'))
df_sum = cbind(df_m,df_sd$test_R2)
colnames(df_sum) = c('reliability', 'R2_mean', 'R2_sd')

pltm = ggplot(df_sum, aes(x = reliability)) + 
  geom_point(aes(y = R2_mean), size=1, colour = 'deeppink3') +
  geom_line(aes(y = R2_mean), size=0.8, colour = 'deeppink3') +
  geom_ribbon(aes(y = R2_mean, ymin = R2_mean - 2*R2_sd, ymax = R2_mean + 2*R2_sd), fill = 'skyblue3', alpha = .2) +
  theme_classic() + ylim(c(-0.1,0.3))
ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'_mean_sd.png'), pltm, width=4.5, height=3.5)


ggplot(data = df_join, aes(x = date, group = variable)) + 
  geom_line(aes(y = mean, color = variable), size = 1) + 
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = variable), alpha = .2)
  
  
  
  # Violins
plt1 = ggplot(d,aes(as.factor(reliability),test_R2)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(-0.1,0.3)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_R2), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[7],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt1, width=3.5, height=3)


plt2 = ggplot(d,aes(as.factor(reliability),test_MAE)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(8.5,14)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_MAE), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[5],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt2, width=3.5, height=3)


plt3 = ggplot(d,aes(as.factor(reliability),test_r)) + geom_violin(color = 'deeppink3') + 
  theme_classic() + ylim(c(0.0,0.6)) + xlab('r(original,simulated)') + theme(legend.position="none") +
  geom_point(aes(as.factor(empirical$reliability),empirical$test_r), size = 3, colour = 'skyblue3')
ggsave(paste0(plt_out,beh,'_',metrics[9],'_wnoise_rel_',rel_max,'_to_',rel_min,'.png'), plt3, width=3.5, height=3)


