
# Correlate HCP test retest data to get a sence of reliabilitites

import pandas as pd
import matplotlib.pyplot as plt


# Load
T1 = pd.read_csv('/home/mgell/Work/reliability/text_files/HCP_YA_retest_T1_beh.csv')
T2 = pd.read_csv('/home/mgell/Work/reliability/text_files/HCP_YA_retest_T2_beh.csv')
df = pd.read_csv('/home/mgell/Work/reliability/text_files/HCP_YA_S1200_beh.csv')

# Filter
behs =  [
    'PicSeq_AgeAdj', 'CardSort_AgeAdj', 'Flanker_AgeAdj', 'PMAT24_A_CR', 
    'ReadEng_AgeAdj', 'PicVocab_AgeAdj', 'ProcSpeed_AgeAdj', 'DDisc_AUC_200',
    'DDisc_AUC_40K', 'ListSort_AgeAdj', 'SCPT_SEN', 'SCPT_SPEC',
    'CogFluidComp_AgeAdj', 'CogTotalComp_AgeAdj', 'CogCrystalComp_AgeAdj',
    'ER40_CR', 'ER40_CRT', 
    'Strength_AgeAdj', 'Dexterity_AgeAdj', 
    'WM_Task_2bk_Acc', 'WM_Task_2bk_Median_RT',
    'Language_Task_Acc', 'Language_Task_Median_RT',
    'Language_Task_Math_Acc', 'Language_Task_Math_Median_RT', 
    'Language_Task_Story_Acc', 'Language_Task_Story_Median_RT',
    'Social_Task_Perc_Random', 'Social_Task_Perc_TOM',
    'Social_Task_Median_RT_Random', 'Social_Task_Median_RT_TOM',
    'Relational_Task_Acc', 'Relational_Task_Median_RT',
    'Relational_Task_Match_Acc', 'Relational_Task_Match_Median_RT',
    'Relational_Task_Rel_Acc', 'Relational_Task_Rel_Median_RT',
    'Emotion_Task_Acc', 'Emotion_Task_Median_RT',
    'Emotion_Task_Face_Acc', 'Emotion_Task_Face_Median_RT',
    'Emotion_Task_Shape_Acc', 'Emotion_Task_Shape_Median_RT',
    'Gambling_Task_Perc_Larger', 'Gambling_Task_Perc_Smaller', 
    'Gambling_Task_Median_RT_Larger', 'Gambling_Task_Median_RT_Smaller'
    ]

T1 = T1[behs]
T2 = T2[behs]
df = df[behs]

# Plot histograms
fig1 = plt.gcf()
df.hist(figsize=(25, 25))
plt.show()
plt.savefig('/home/mgell/Work/reliability/plots/HCP_YA_behaviour/HCP_YA_beh_hist.png',dpi=600)
plt.close()