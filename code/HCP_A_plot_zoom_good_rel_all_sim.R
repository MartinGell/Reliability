
library(tidyverse)



#net_cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # original order
net_cols <- c("#0072B2", "#009E73", "#D55E00", "#56B4E9", "#E69F00", "#F0E442", "#CC79A7") # reordering for 3 colours only



plt_in = '/home/mgell/Work/reliability/input/'
plt_out = '/home/mgell/Work/reliability/plots/simulation_res/'

f = 'ridgeCV_zscore_averaged-source_Schaefer400x17_WM+CSF+GS_hcpaging_695_zscored-beh_HCP_A_total_wnoise_all.csv'
d_t = read_csv(paste0(plt_in,f))

f = 'ridgeCV_zscore_averaged-source_Schaefer400x17_WM+CSF+GS_hcpaging_695_zscored-beh_HCP_A_motor_wnoise_all.csv'
d_m = read_csv(paste0(plt_in,f))

f = 'ridgeCV_zscore_averaged-source_Schaefer400x17_WM+CSF+GS_hcpaging_695_zscored-beh_HCP_A_cryst_wnoise_all.csv'
d_c = read_csv(paste0(plt_in,f))


d_t$beh = 'total_cog'
d_m$beh = 'grip'
d_c$beh = 'cryst_cog'

d = rbind(d_t,d_m,d_c)



metrics = colnames(d)

d2 = d[d$reliability   >= 0.6,]
d2 = d2[d2$reliability <= 0.8,]

rel_max2 = max(d2$reliability)
rel_min2 = min(d2$reliability)

# 0.7 to 0.6
pltx = ggplot(d2,aes(as.factor(reliability),test_R2,colour=beh,fill=beh)) + 
  geom_violin(alpha = 0.2) +
  geom_dotplot(binaxis="y", stackdir="center", binwidth=0.004, alpha = 0.7, position=position_dodge(0.9)) + 
  theme_classic() + 
  xlab('r(original,simulated)') + ylab('R2') +
  scale_colour_manual(values = net_cols, name = "Variable",
                      labels = c("Cryst. cog.", "Grip strength", "Total cog.")) +
  scale_fill_manual(values = net_cols, name = "Variable",
                    labels = c("Cryst. cog.", "Grip strength", "Total cog.")) +
  xlab('r(original,simulated)')
ggsave(paste0(plt_out,'all_behs_dot_plot_',metrics[7],'_wnoise_rel_',rel_max2,'_to_',rel_min2,'.png'), pltx, width=6.5, height=3.5)


# 0.8 to 0.6
pltx = ggplot(d2,aes(as.factor(reliability),test_R2,colour=beh,fill=beh)) + 
  geom_violin(alpha = 0.2) +
  geom_dotplot(binaxis="y", stackdir="center", binwidth=0.004, alpha = 0.7, position=position_dodge(0.9)) + 
  theme_classic() + 
  xlab('r(original,simulated)') + ylab('R2') +
  scale_colour_manual(values = net_cols, name = "Variable",
                      labels = c("Cryst. cog.", "Grip strength", "Total cog.")) +
  scale_fill_manual(values = net_cols, name = "Variable",
                    labels = c("Cryst. cog.", "Grip strength", "Total cog.")) +
  xlab('r(original,simulated)')+
  theme(legend.position="none")
ggsave(paste0(plt_out,'all_behs_dot_plot_',metrics[7],'_wnoise_rel_',rel_max2,'_to_',rel_min2,'.png'), pltx, width=8, height=2.5)


pltx = ggplot(d2,aes(as.factor(reliability),test_R2,colour=beh,fill=beh)) + 
  geom_violin(alpha = 0.2) +
  geom_dotplot(binaxis="y", stackdir="center", binwidth=0.0055, alpha = 0.7, position=position_dodge(0.9)) + 
  theme_classic() + 
  xlab('r(original,simulated)') + ylab('R2') +
  scale_colour_manual(values = net_cols, name = "Variable",
                      labels = c("Cryst. cog.", "Grip strength", "Total cog.")) +
  scale_fill_manual(values = net_cols, name = "Variable",
                    labels = c("Cryst. cog.", "Grip strength", "Total cog.")) +
  xlab('r(original,simulated)')
  #theme(legend.position="none")
ggsave(paste0(plt_out,'all_behs_dot_plot_',metrics[7],'_wnoise_rel_',rel_max2,'_to_',rel_min2,'.png'), pltx, width=7, height=2)
