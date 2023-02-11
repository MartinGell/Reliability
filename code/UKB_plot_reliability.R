
library(tidyverse)
library(psych)


plt_out = '/home/mgell/Work/reliability/plots/UKB/'

d = read.csv('/home/mgell/Work/reliability/input/collected/ridgeCV_zscore_confound_removal_wcategorical_averaged-source_Schaefer400x17_nodenoise_UKB_5000_z-beh_UKB_5000_subs_FC_all_cogs_all_behs.csv')

rel = read.csv('/home/mgell/Work/reliability/res/cog_plus_cors_pearson_all.csv', header = FALSE)
colnames(rel) = c('behs','rel_r')



# OR
T1 <- read.csv('/home/mgell/Work/reliability/input/cog_plus_2.0_data.csv', check.names = FALSE)
T2 <- read.csv('/home/mgell/Work/reliability/input/cog_plus_3.0_data.csv', check.names = FALSE)


cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]
df <- data.frame('rel_r' = rel)

behs = colnames(T1)
colnames(T2) = behs
ICC2 = numeric(length(behs))
ICC2_upper = numeric(length(behs))
ICC2_lower = numeric(length(behs))
i = 1
for (beh_i in behs) {
  x <- ICC(data.frame(T1[,beh_i],T2[,beh_i]))
  ICC <- x$results$ICC[2]
  ICC2[i] = ICC
  ICC2_upper[i] = x$results$`upper bound`[2]
  ICC2_lower[i] = x$results$`lower bound`[2]
  i = i+1
}

df$rel_ICC2 <- ICC2
df$ICC2_low <- ICC2_upper
df$ICC2_upp <- ICC2_lower
df$behs <- behs

rel=df

# w logs
d = d[c(-1,-2,-3,-5,-7,-8,-12,-18),]
rel = rel[c(-1,-2,-3,-4,-6,-8,-9,-13,-16,-20),]

# no logs
#d = d[c(-1,-18,-20,-21,-22,-23,-24,-25),]
#rel = rel[c(-1,-2,-16,-20,-22,-23,-24,-25,-26,-27),]

d$test_MAE = d$test_MAE*-1

if (all(d$beh == rel$behs)) {
  d$reliability = rel$rel_r
  d$reliability = rel$rel_ICC2
}

#d = d[-14,] # Age

# plot reliabilities
#r
names = read.delim('/home/mgell/Work/reliability/res/cog_plus_cors_pearson_all.csv', header = FALSE, sep = '-')
rel$behs = names$V1
rel = rel[c(-16,-20),]

plt = ggplot(rel, aes(x = behs, y = rel_r)) +
  geom_bar(stat = "identity", width = .75) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x=element_blank()) +
  ylab('cor(neuroimaging session, follow-up session)') +
  xlab('Behaviour')
ggsave(paste0(plt_out,'UKB_all_behs_reliability.png'), plt, width=6, height=8)

#icc
names = read.delim('/home/mgell/Work/reliability/res/cog_plus_cors_pearson_all.csv', header = FALSE, sep = '-')
rel=df
rel$behs = names$V1
rel = rel[c(-16,-20),]

plt = ggplot(rel, aes(x = behs, y = rel_ICC2)) +
  geom_bar(stat = "identity", width = .75) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x=element_blank()) +
  ylab('Reliability (ICC2)') +
  xlab('Behaviour')
ggsave(paste0(plt_out,'UKB_all_behs_reliability_ICC.png'), plt, width=6, height=8)









cor(d$reliability,d$test_r)
text = paste0("italic(r)==", round(cor(d$reliability,d$test_r),digits = 2))
text2 = paste0("italic(P)<", "0.001")

plt = ggplot(d, aes(reliability,test_r)) +
  geom_smooth(method = lm, se = FALSE, colour = 'darkgray', size = 1.5, alpha = 0.6) +
  geom_point(colour = "#56B4E9", size = 2) +
  theme_classic() +
  ylab('r') + xlab('reliability') +
  #ylim(c(-0.1,0.5)) +
  #annotate('text', x = 0.35, y = 0.35, label=paste("Correlation:", round(cor(d$reliability,d$test_r),digits = 2)), size=5)
  scale_y_continuous(limits = c(-0.1,0.55), breaks = c(-0.1,0,0.1,0.3,0.5)) +
  annotate('text', x = 0.30, y = 0.45, label=text, size=5, parse = TRUE) +
  annotate('text', x = 0.31, y = 0.39, label=text2, size=5, parse = TRUE)

#ggsave(paste0(plt_out,'HCP_YA_all_behs_r.png'), plt, width=4.5, height=3.5)
#ggsave(paste0(plt_out,'HCP_YA_all_behs_r_conf_removed.png'), plt, width=4.5, height=3.5)


#r
cor.test(d$reliability,d$test_R2)
text  = paste0("italic(r)==", round(cor(d$reliability,d$test_R2),digits = 2))
text2 = paste0("italic(P)==", "0.005")

plt = ggplot() +
  geom_smooth(data=d, aes(reliability,test_R2), method = lm, se = FALSE, colour = 'lightgray', size = 1.5, alpha = 0.4) +
  geom_point(data=d, aes(reliability,test_R2), colour = '#56B4E9', size = 2) +
  theme_classic() +
  ylim(c(-0.1,0.2)) +
  ylab('R2') + xlab('Reliability (ICC2)') +
  scale_x_continuous(breaks = c(seq(0.2,0.9,0.1))) +
  annotate('text', x = 0.30, y = 0.15, label=text,  size=5, parse = TRUE) +
  annotate('text', x = 0.31, y = 0.12, label=text2, size=5, parse = TRUE)

#ggsave(paste0(plt_out,'UKB_all_behs_r2.png'), plt, width=4.5, height=3.5)

plt2 = plt + 
  #geom_segment(aes(x = 0.6, y = -0.1, xend = 0.6, yend = 0.3, colour= "red"), inherit.aes = FALSE, size=1, alpha=0.4, linetype= "dashed") +
  geom_segment(aes(x = min(d$reliability), y = 0, xend = max(d$reliability), yend = 0,  colour= "red"), inherit.aes = FALSE, size=1, alpha=0.4, linetype= "dashed") +
  theme(legend.position = "none")

#ggsave(paste0(plt_out,'UKB_all_behs_r2_conf_removed_dashed.png'), plt2, width=4.5, height=3.5)
#ggsave(paste0(plt_out,'UKB_all_behs_ICC_r2_conf_removed_dashed.png'), plt2, width=4.5, height=3.5)
#ggsave(paste0(plt_out,'UKB_all_behs_r2_conf_removed_dashed_nologs.png'), plt2, width=4.5, height=3.5)



# no reg line
plt = ggplot() +
  geom_point(data=d, aes(reliability,test_R2), colour = '#56B4E9', size = 2) +
  theme_classic() +
  ylab('R2') + xlab('reliability') +
  geom_segment(aes(x = 0.6, y = -0.1, xend = 0.6, yend = 0.3, colour= "red"), inherit.aes = FALSE, size=1, alpha=0.4, linetype= "dashed") +
  geom_segment(aes(x = min(d$reliability), y = 0, xend = max(d$reliability), yend = 0,  colour= "red"), inherit.aes = FALSE, size=1, alpha=0.4, linetype= "dashed") +
  theme(legend.position = "none")

ggsave(paste0(plt_out,'UKB_all_behs_r2_wconf_dashed.png'), plt, width=4.5, height=3.5)
