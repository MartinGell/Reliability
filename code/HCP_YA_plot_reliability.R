
# Correlate HCP test retest data to get a sence of reliabilitites

library(tidyverse)
library(corrplot)
library(psych)

# Load
T1 <- read.csv('/home/mgell/Work/reliability/text_files/HCP_YA_retest_T1_beh.csv')
T2 <- read.csv('/home/mgell/Work/reliability/text_files/HCP_YA_retest_T2_beh.csv')

behs <- c(
    'PicSeq_AgeAdj', 'CardSort_AgeAdj', 'Flanker_AgeAdj', 'PMAT24_A_CR',
    'ReadEng_AgeAdj', 'PicVocab_AgeAdj', 'ProcSpeed_AgeAdj', 'DDisc_AUC_200',
    'DDisc_AUC_40K', 'ListSort_AgeAdj', 'SCPT_SPEC',
    'CogFluidComp_AgeAdj', 'CogTotalComp_AgeAdj', 'CogCrystalComp_AgeAdj',
    'ER40_CR', 'ER40_CRT', 
    'Strength_AgeAdj', 'Dexterity_AgeAdj', 
    'WM_Task_2bk_Acc', 'WM_Task_2bk_Median_RT',
    'Language_Task_Acc', 'Language_Task_Median_RT',
    'Language_Task_Math_Acc', 'Language_Task_Math_Median_RT', 
    'Language_Task_Story_Median_RT',
    'Social_Task_Perc_Random',
    'Social_Task_Median_RT_Random', 'Social_Task_Median_RT_TOM',
    'Relational_Task_Acc', 'Relational_Task_Median_RT',
    'Relational_Task_Match_Acc', 'Relational_Task_Match_Median_RT',
    'Relational_Task_Rel_Acc', 'Relational_Task_Rel_Median_RT',
    'Emotion_Task_Median_RT',
    'Emotion_Task_Face_Median_RT',
    'Emotion_Task_Shape_Median_RT',
    'Gambling_Task_Perc_Larger', 'Gambling_Task_Perc_Smaller', 
    'Gambling_Task_Median_RT_Larger', 'Gambling_Task_Median_RT_Smaller'
    )

T1 <- T1 %>% select(all_of(behs))
T2 <- T2 %>% select(all_of(behs))


T1$SCPT_SPEC_log <- log(T1$SCPT_SPEC)
#T1$DDisc_AUC_200 <- log(T1$DDisc_AUC_200)
T1$WM_Task_2bk_Acc_log <- log(T1$WM_Task_2bk_Acc)
T1$Language_Task_Acc_log <- log(T1$Language_Task_Acc)
T1$Language_Task_Math_Acc_log <- log(T1$Language_Task_Math_Acc)
T1$Social_Task_Median_RT_Random_log <- log(T1$Social_Task_Median_RT_Random)
T1$Social_Task_Median_RT_TOM_log <- log(T1$Social_Task_Median_RT_TOM)
T1$Relational_Task_Match_Acc_log <- log(T1$Relational_Task_Match_Acc)
T1$Gambling_Task_Median_RT_Larger_log <- log(T1$Gambling_Task_Median_RT_Larger)
T1$Gambling_Task_Median_RT_Smaller_log <- log(T1$Gambling_Task_Median_RT_Smaller)

T2$SCPT_SPEC_log <- log(T2$SCPT_SPEC)
#T2$DDisc_AUC_200 <- log(T2$DDisc_AUC_200)
T2$WM_Task_2bk_Acc_log <- log(T2$WM_Task_2bk_Acc)
T2$Language_Task_Acc_log <- log(T2$Language_Task_Acc)
T2$Language_Task_Math_Acc_log <- log(T2$Language_Task_Math_Acc)
T2$Social_Task_Median_RT_Random_log <- log(T2$Social_Task_Median_RT_Random)
T2$Social_Task_Median_RT_TOM_log <- log(T2$Social_Task_Median_RT_TOM)
T2$Relational_Task_Match_Acc_log <- log(T2$Relational_Task_Match_Acc)
T2$Gambling_Task_Median_RT_Larger_log <- log(T2$Gambling_Task_Median_RT_Larger)
T2$Gambling_Task_Median_RT_Smaller_log <- log(T2$Gambling_Task_Median_RT_Smaller)

behs = c(behs,c('SCPT_SPEC_log','WM_Task_2bk_Acc_log',
                'Language_Task_Acc_log','Language_Task_Math_Acc_log',
                'Social_Task_Median_RT_Random_log','Social_Task_Median_RT_TOM_log',
                'Relational_Task_Match_Acc_log',
                'Gambling_Task_Median_RT_Larger_log','Gambling_Task_Median_RT_Smaller_log'))


cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]
d <- data.frame(t(rel))


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

d[2,] <- t(ICC2)
d[3,] <- t(ICC2_upper)
d[4,] <- t(ICC2_lower)

colnames(d) <- behs


# ggplot(data = NULL, aes(x = colnames(d), y = as.numeric(d[1,]))) +
#   geom_bar(stat = "identity", width = .75) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90),
#         axis.title.x=element_blank()) +
#   ylim(c(min(d),1.0)) +
#   ylab('correlation(T1,T2)')

# Now remove the gambling behs that appear to have negative reliability???

new_behs <- c(
  'PicSeq_AgeAdj', 'CardSort_AgeAdj', 'Flanker_AgeAdj', 'PMAT24_A_CR',
  'ReadEng_AgeAdj', 'PicVocab_AgeAdj', 'ProcSpeed_AgeAdj', 'DDisc_AUC_200',
  'DDisc_AUC_40K', 'ListSort_AgeAdj', 'SCPT_SPEC',
  'CogFluidComp_AgeAdj', 'CogTotalComp_AgeAdj', 'CogCrystalComp_AgeAdj',
  'ER40_CR', 'ER40_CRT', 
  'Strength_AgeAdj', 'Dexterity_AgeAdj', 
  'WM_Task_2bk_Acc', 'WM_Task_2bk_Median_RT',
  'Language_Task_Acc', 'Language_Task_Median_RT',
  'Language_Task_Math_Acc', 'Language_Task_Math_Median_RT', 
  'Language_Task_Story_Median_RT',
  'Social_Task_Perc_Random',
  'Social_Task_Median_RT_Random', 'Social_Task_Median_RT_TOM',
  'Relational_Task_Acc', 'Relational_Task_Median_RT',
  'Relational_Task_Match_Acc', 'Relational_Task_Match_Median_RT',
  'Relational_Task_Rel_Acc', 'Relational_Task_Rel_Median_RT',
  'Emotion_Task_Median_RT',
  'Emotion_Task_Face_Median_RT',
  'Emotion_Task_Shape_Median_RT',
  'Gambling_Task_Median_RT_Larger', 'Gambling_Task_Median_RT_Smaller',
  'SCPT_SPEC_log','WM_Task_2bk_Acc_log',
  'Language_Task_Acc_log','Language_Task_Math_Acc_log',
  'Social_Task_Median_RT_Random_log','Social_Task_Median_RT_TOM_log',
  'Relational_Task_Match_Acc_log',
  'Gambling_Task_Median_RT_Smaller_log'
)

df <- d %>% select(all_of(new_behs))
#d[order(d$reliability_icc2),]

# plt = ggplot(data = NULL, aes(x = colnames(df), y = as.numeric(df[2,]))) +
#   geom_bar(stat = "identity", width = .75) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90),
#         axis.title.x=element_blank()) +
#   ylim(c(0,1.0)) +
#   ylab('Reliability (ICC2)')
# ggsave('/home/mgell/Work/reliability/plots/HCP_YA_behaviour/HCP_YA_rel_ICC.png', plt)# width=6, height=8)
# 
# all_behs = data.frame('all_behs' = new_behs)
# write_delim(all_behs, '/home/mgell/Work/Prediction_HCP/code/opts/HCP_YA_behs2predict.txt', col_names = FALSE)





# Plots separately
plt_out = '/home/mgell/Work/reliability/plots/HCP_YA_res/'

#d = read_csv('/home/mgell/Work/reliability/input/ridgeCV_zscore_averaged-source_Schaefer400x17_WM+CSF+GS_hcpya_316_zscored-beh_HCP_YA_beh_confs_unrelated_all_behs.csv')
#d = read_csv('/home/mgell/Work/reliability/input/ridgeCV_zscore_confound_removal_wcategorical_averaged-source_Schaefer400x17_WM+CSF+GS_hcpya_316_zscored-beh_HCP_YA_beh_confs_unrelated_all_behs.csv')

# d = read_csv('/home/mgell/Work/reliability/input/ridgeCV_zscore_stratified_KFold_confound_removal_wcategorical_averaged-source_Schaefer400x17_WM+CSF+GS_hcpya_771_zscored-beh_HCP_YA_beh_confs_all_restricted_all_behs.csv')
d = read_csv('/home/mgell/Work/reliability/input/ridgeCV_zscore_stratified_KFold_confound_removal_wcategorical_averaged-source_Schaefer400x17_WM+CSF+GS_hcpya_771_zscored-beh_HCP_YA_beh_all_all_behs.csv')
#d = read_csv('/home/mgell/Work/reliability/input/ridgeCV_zscore_stratified_KFold_averaged-source_Schaefer400x17_WM+CSF+GS_hcpya_771_zscored-beh_HCP_YA_beh_confs_all_restricted_all_behs.csv')

d = d[-1,]
d$test_MAE = d$test_MAE*-1

if (all(d$beh == colnames(df))) {
  d$reliability_r = as.numeric(unlist(df[1,]))
  d$reliability_icc2 = as.numeric(unlist(df[2,]))
  d$reliability_icc2_upper = as.numeric(unlist(df[3,]))
  d$reliability_icc2_lower = as.numeric(unlist(df[4,]))
} 


d = d[c(-11,-19,-21,-23,-27,-28,-31,-39),]

cor(d$reliability,d$test_r)
text = paste0("italic(r)==", round(cor(d$reliability,d$test_r),digits = 2))
text2 = paste0("italic(P)<", "0.001")

plt = ggplot(d, aes(reliability,test_r)) +
  geom_smooth(method = lm, se = FALSE, colour = 'lightgray', size = 1.5, alpha = 0.6) +
  geom_point(colour = "#56B4E9", size = 2) +
  theme_classic() +
  ylab('r') + xlab('reliability') +
  scale_x_continuous(breaks = c(seq(0.2,0.9,0.1))) +
  #ylim(c(-0.1,0.5)) +
  #annotate('text', x = 0.35, y = 0.35, label=paste("Correlation:", round(cor(d$reliability,d$test_r),digits = 2)), size=5)
  scale_y_continuous(limits = c(-0.1,0.55), breaks = c(-0.1,0,0.1,0.3,0.5)) +
  annotate('text', x = 0.30, y = 0.45, label=text, size=5, parse = TRUE) +
  annotate('text', x = 0.31, y = 0.39, label=text2, size=5, parse = TRUE)

#ggsave(paste0(plt_out,'HCP_YA_all_behs_r.png'), plt, width=4.5, height=3.5)
ggsave(paste0(plt_out,'HCP_YA_all_behs_r_conf_removed.png'), plt, width=4.5, height=3.5)



cor.test(d$reliability_r,d$test_R2)
text  = paste0("italic(r)==", round(cor(d$reliability_r,d$test_R2),digits = 2))
text2 = paste0("italic(P)<", "0.001")

plt = ggplot() +
  geom_smooth(data=d, aes(reliability_r,test_R2), method = lm, se = FALSE, colour = 'lightgray', size = 1.5, alpha = 0.4) +
  geom_point(data=d, aes(reliability_r,test_R2), colour = '#56B4E9', size = 2) +
  theme_classic() +
  ylim(c(-0.1,0.2)) +
  ylab('R2') + xlab('Reliability (r)') +
  scale_x_continuous(breaks = c(seq(0.2,0.9,0.1))) +
  annotate('text', x = 0.30, y = 0.15, label=text,  size=5, parse = TRUE) +
  annotate('text', x = 0.31, y = 0.12, label=text2, size=5, parse = TRUE)

#ggsave(paste0(plt_out,'HCP_YA_all_behs_r2.png'), plt, width=4.5, height=3.5)
#ggsave(paste0(plt_out,'HCP_YA_all_behs_r2_conf_removed.png'), plt, width=4.5, height=3.5)

# Add dashed lines at 0.6 rel and 0
plt2 = plt + 
  #geom_segment(aes(x = 0.6, y = -0.1, xend = 0.6, yend = 0.2, colour= "red"), inherit.aes = FALSE, size=1, alpha=0.4, linetype= "dashed") +
  geom_segment(aes(x = 0.2, y = 0,    xend = max(d$reliability_r), yend = 0,  colour= "red"), inherit.aes = FALSE, size=1, alpha=0.4, linetype= "dashed") +
  theme(legend.position = "none")
#ggsave(paste0(plt_out,'HCP_YA_all_behs_r2_conf_removed_dashed_log.png'), plt2, width=4.5, height=3.5)





cor.test(d$reliability_icc2,d$test_R2)
text  = paste0("italic(r)==", round(cor(d$reliability_r,d$test_R2),digits = 2))
text2 = paste0("italic(P)<", "0.001")

plt = ggplot() +
  geom_smooth(data=d, aes(reliability_icc2,test_R2), method = lm, se = FALSE, colour = 'lightgray', size = 1.5, alpha = 0.4) +
  geom_point(data=d, aes(reliability_icc2,test_R2), colour = '#56B4E9', size = 2) +
  theme_classic() +
  ylim(c(-0.1,0.2)) +
  ylab('Accuracy (R2)') + xlab('Reliability (ICC2)') +
  scale_x_continuous(breaks = c(seq(0.2,0.9,0.1))) +
  annotate('text', x = 0.30, y = 0.15, label=text,  size=5, parse = TRUE) +
  annotate('text', x = 0.31, y = 0.12, label=text2, size=5, parse = TRUE)

#ggsave(paste0(plt_out,'HCP_YA_all_behs_r2.png'), plt, width=4.5, height=3.5)
#ggsave(paste0(plt_out,'HCP_YA_all_behs_r2_conf_removed.png'), plt, width=4.5, height=3.5)

# Add dashed lines at 0.6 rel and 0
plt2 = plt + 
  #geom_segment(aes(x = 0.6, y = -0.1, xend = 0.6, yend = 0.2, colour= "red"), inherit.aes = FALSE, size=1, alpha=0.4, linetype= "dashed") +
  geom_segment(aes(x = 0.2, y = 0,    xend = max(d$reliability_icc2), yend = 0,  colour= "red"), inherit.aes = FALSE, size=1, alpha=0.4, linetype= "dashed") +
  theme(legend.position = "none")
ggsave(paste0(plt_out,'HCP_YA_all_behs_r2_conf_removed_dashed_log_ICC_obhm.png'), plt2, width=3.5, height=3)
#ggsave(paste0(plt_out,'OHBM_HCP_YA_all_behs_r2_conf_removed_dashed_log_ICC_obhm.png'), plt2, width=3.5, height=3)


cor.test(d$reliability_icc2_upper,d$test_R2)
cor.test(d$reliability_icc2_lower,d$test_R2)




cor(d$reliability,d$test_MAE)

ggplot(d, aes(reliability,test_MAE)) +
  geom_smooth(method = lm, se = FALSE, colour = 'skyblue3', size = 1.5, alpha = 0.6) +
  geom_point(colour = 'deeppink3', size = 2) +
  theme_classic()



d_pos = d
d_pos = d_pos[d$test_R2 > 0,]
cor.test(d_pos$reliability,d_pos$test_R2)

ggplot() +
  geom_smooth(data=d_pos, aes(reliability,test_R2), method = lm, se = FALSE, colour = 'lightgray', size = 1.5, alpha = 0.4) +
  geom_point(data=d_pos, aes(reliability,test_R2), colour = '#56B4E9', size = 2) +
  theme_classic() +
  ylim(c(-0.1,0.2)) +
  ylab('R2') + xlab('Reliability') +
  scale_x_continuous(breaks = c(seq(0.2,0.9,0.1)))




# Plot ridge + svr
plt_out = '/home/mgell/Work/reliability/plots/HCP_YA_res/'

#d_ridge = read_csv('/home/mgell/Work/reliability/input/ridgeCV_zscore_averaged-source_Schaefer400x17_WM+CSF+GS_hcpya_316_zscored-beh_HCP_YA_beh_confs_unrelated_all_behs.csv')
#d_svr = read_csv()

d_ridge = read_csv('/home/mgell/Work/reliability/input/ridgeCV_zscore_confound_removal_wcategorical_averaged-source_Schaefer400x17_WM+CSF+GS_hcpya_316_zscored-beh_HCP_YA_beh_confs_unrelated_all_behs.csv')
d_svr = read_csv('/home/mgell/Work/reliability/input/svr_heuristic_zscore_confound_removal_wcategorical_averaged-source_Schaefer400x17_WM+CSF+GS_hcpya_316_zscored-beh_HCP_YA_beh_confs_unrelated_all_behs.csv')

d_ridge$algorithm = 'Ridge'
d_svr$algorithm =   'SVR'

d_ridge = d_ridge[-1,]
d_svr = d_svr[-1,]

d_ridge$reliability = as.numeric(unlist(df[1,]))
d_svr$reliability =   as.numeric(unlist(df[1,]))

cor(d_ridge$reliability,d_ridge$test_R2)
cor(d_svr$reliability,d_svr$test_R2)

d = rbind(d_ridge, d_svr)

d$test_MAE = d$test_MAE*-1


# r
cor(d_ridge$reliability,d_ridge$test_r)
cor(d_svr$reliability,d_svr$test_r)

plt = ggplot(d, aes(reliability, test_r, colour = algorithm)) +
  geom_smooth(method = lm, se = FALSE, size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  theme_classic() +
  ylim(c(-0.1,0.4))
ggsave(paste0(plt_out,'HCP_YA_all_behs_r_conf_removed_ridge_svr.png'), plt, width=4.5, height=3.5)


# R2
cor(d_ridge$reliability,d_ridge$test_R2)
cor(d_svr$reliability,d_svr$test_R2)

plt = ggplot(d, aes(reliability, test_R2, colour = algorithm)) +
  geom_smooth(method = lm, se = FALSE, size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  theme_classic() +
  ylim(c(-0.15,0.15))
ggsave(paste0(plt_out,'HCP_YA_all_behs_r2_conf_removed_ridge_svr.png'), plt, width=4.5, height=3.5)


















d$reliability = (d$reliability*2/10)+0.68


plt2=ggplot() +
  #geom_smooth(data=d, aes(reliability,test_R2), method = lm, se = FALSE, colour = 'lightgray', size = 1.5, alpha = 0.4) +
  geom_point(data=d, aes(reliability,test_R2), colour = '#56B4E9', size = 2) +
  theme_classic() +
  ylim(c(-0.1,0.2)) +
  ylab('R2') + xlab('Reliability') +
  scale_x_continuous(breaks = c(seq(0.2,0.9,0.1))) +
  geom_segment(aes(x = 0.6, y = -0.1, xend = 0.6, yend = 0.2, colour= "red"), inherit.aes = FALSE, size=1, alpha=0.4, linetype= "dashed") +
  geom_segment(aes(x = 0.2, y = 0,    xend = max(d$reliability), yend = 0,  colour= "red"), inherit.aes = FALSE, size=1, alpha=0.4, linetype= "dashed") +
  theme(legend.position = "none")

ggsave(paste0(plt_out,'HCP_YA_all_behs_r2_conf_removed_TEST.png'), plt2, width=4.5, height=3.5)

