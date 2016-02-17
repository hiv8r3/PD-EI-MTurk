require(magrittr)
require(dplyr)

# read in data
dat = read.delim("RawData.txt")

# add identifiers (i.e., subject numbers)
dat$Subject = 1:nrow(dat)

# how many completed fully?
length(dat$end[!(is.na(dat$end))]) # 483 completed fully

# defines items/column in each scale
UCLA = c("UCLA_1_1", "UCLA_1_2", "UCLA_1_3", "UCLA_1_4",             # UCLA loneliness
         "UCLA_1_5", "UCLA_1_6", "UCLA_1_7", "UCLA_1_8",
         "UCLA_1_9", "UCLA_1_10","UCLA_2_1", "UCLA_2_2",
         "UCLA_2_3", "UCLA_2_4", "UCLA_2_5", "UCLA_2_6",
         "UCLA_2_7", "UCLA_2_8", "UCLA_2_9", "UCLA_2_10")

SWLS = c("SLWS_1", "SLWS_2", "SLWS_3", "SLWS_4", "SLWS_5")          # Satisfaction with Life Scale

SE = c("SE_1_1", "SE_1_2", "SE_1_3", "SE_1_4",                      # Self esteem
       "SE_1_5", "SE_1_6", "SE_1_7", "SE_1_8",
       "SE_1_9", "SE_1_10","SE_2_1", "SE_2_2",
       "SE_2_3", "SE_2_4", "SE_2_5", "SE_2_6",
       "SE_2_7", "SE_2_8", "SE_2_9", "SE_2_10")

Mood = c("Mood_1", "Mood_2",  "Mood_3", "Mood_4",                   # Mood 
         "Mood_5", "Mood_6",  "Mood_7", "Mood_8",
         "Mood_9", "Mood_10", "Mood_11","Mood_12",
         "Mood_13","Mood_14", "Mood_15","Mood_16")

Disc = c("Disc_1", "Disc_2", "Disc_3", "Disc_4",                    # Perceived discrimination (general)
         "Disc_5", "Disc_6", "Disc_7", "Disc_8",
         "Disc_9")

DiscR = c("DiscR_1", "DiscR_2", "DiscR_3", "DiscR_4",               # Perceived discrimination (race)
         "DiscR_5", "DiscR_6", "DiscR_7", "DiscR_8",
         "DiscR_9")

EIS = c("EIS_1","EIS_2",                                            # Existential Isolation
        "EIS_3","EIS_4",
        "EIS_5","EIS_6")

WellB = c("Well_B_1", "Well_B_2", "Well_B_3", "Well_B_4",            # Well-being (Bradburn)
         "Well_B_5", "Well_B_6", "Well_B_7", "Well_B_8",
         "Well_B_9", "Well_B_10")

WellR = c("Rau_1", "Rem_2", "Rsa_6", "Rau_7",                        # Well-being (Ryff)
          "Rem_8", "Rsa_12","Rau_13","Rem_14",
          "Rsa_18","Rau_19","Rem_20","Rsa_24",
          "Rau_25","Rem_26","Rsa_30","Rau_31",
          "Rem_32","Rsa_36","Rau_37","Rem_38",
          "Rsa_42")


# look at subjects who didn't complete fully, only take columns for questionnaire data
notComplete = 
  cbind(select(dat[is.na(dat$end),], Subject)) %>%
  cbind(select(dat[is.na(dat$end),], one_of(UCLA))) %>%
  cbind(select(dat[is.na(dat$end),], one_of(SWLS))) %>%
  cbind(select(dat[is.na(dat$end),], one_of(SE))) %>%
  cbind(select(dat[is.na(dat$end),], one_of(Mood))) %>%
  cbind(select(dat[is.na(dat$end),], one_of(Disc))) %>%
  cbind(select(dat[is.na(dat$end),], one_of(DiscR))) %>%
  cbind(select(dat[is.na(dat$end),], one_of(EIS))) %>%
  cbind(select(dat[is.na(dat$end),], one_of(WellB))) %>%
  cbind(select(dat[is.na(dat$end),], one_of(WellR)))

# make a data frame with percentage of missing data for each subject
missing = data.frame(Subject = NULL, PercMissing = NULL)
for (i in 1:nrow(notComplete)) {
  temp = notComplete[notComplete$Subject == i,]
  k = sum(is.na(temp))/117
  missing = rbind(missing, data.frame(Subject = i, PercMissing = k))
  }

hist(missing$PercMissing, breaks = 30)
  
nrow(missing[missing$PercMissing > .6,])   # 20 subjects missing more than 60%
nrow(missing[missing$PercMissing > .5,])   # 24 subjects missing more than 50%
nrow(missing[missing$PercMissing > .4,])   # 28 subjects missing more than 40%

badSubs = missing[missing$PercMissing > .4,]


######################################################################################
####################### Calculate scores for each participant ########################
######################################################################################

noBS = dat[!(dat$Subject %in% badSubs$Subject),]

# UCLA Loneliness scale
# 20 items, on scale of 1 (always) to 4 (never) # mistake, should be 1 (never) to 4 (always)
# Reverse scored: 1, 5, 6, 9, 10, 15, 16, 19, 20 # would be if normal. 
# Instead, rev score: 2, 3, 4, 7, 8, 11, 12, 13, 14, 17, 18 
# Higher score is more lonely

UCLAdat = select(noBS, one_of(UCLA)) %>%
  cbind(select(noBS, Subject))

for (i in unique(UCLAdat$Subject)) {
  UCLAdat$UCLA_1_2.rev[UCLAdat$Subject == i] = 5 - UCLAdat$UCLA_1_2[UCLAdat$Subject == i]
  UCLAdat$UCLA_1_3.rev[UCLAdat$Subject == i] = 5 - UCLAdat$UCLA_1_3[UCLAdat$Subject == i]
  UCLAdat$UCLA_1_4.rev[UCLAdat$Subject == i] = 5 - UCLAdat$UCLA_1_4[UCLAdat$Subject == i]
  UCLAdat$UCLA_1_7.rev[UCLAdat$Subject == i] = 5 - UCLAdat$UCLA_1_7[UCLAdat$Subject == i]
  UCLAdat$UCLA_1_8.rev[UCLAdat$Subject == i] = 5 - UCLAdat$UCLA_1_8[UCLAdat$Subject == i]
  UCLAdat$UCLA_2_1.rev[UCLAdat$Subject == i] = 5 - UCLAdat$UCLA_2_1[UCLAdat$Subject == i]
  UCLAdat$UCLA_2_2.rev[UCLAdat$Subject == i] = 5 - UCLAdat$UCLA_2_2[UCLAdat$Subject == i]
  UCLAdat$UCLA_2_3.rev[UCLAdat$Subject == i] = 5 - UCLAdat$UCLA_2_3[UCLAdat$Subject == i]
  UCLAdat$UCLA_2_4.rev[UCLAdat$Subject == i] = 5 - UCLAdat$UCLA_2_4[UCLAdat$Subject == i]
  UCLAdat$UCLA_2_7.rev[UCLAdat$Subject == i] = 5 - UCLAdat$UCLA_2_7[UCLAdat$Subject == i]
  UCLAdat$UCLA_2_8.rev[UCLAdat$Subject == i] = 5 - UCLAdat$UCLA_2_8[UCLAdat$Subject == i]
}

# creates UCLA score by averaging across all 20 items (including reversed scored items)
UCLAdat = mutate(UCLAdat, UCLA_total = 
                   (UCLA_1_1 + UCLA_1_2.rev + UCLA_1_3.rev + UCLA_1_4.rev +
                      UCLA_1_5 + UCLA_1_6 + UCLA_1_7.rev + UCLA_1_8.rev +
                      UCLA_1_9 + UCLA_1_10 + UCLA_2_1.rev + UCLA_2_2.rev +
                      UCLA_2_3.rev + UCLA_2_4.rev + UCLA_2_5 + UCLA_2_6 +
                      UCLA_2_7.rev + UCLA_2_8.rev + UCLA_2_9 + UCLA_2_10)/20)

# histogram shows scores normally distributed
hist(UCLAdat$UCLA_total)

# create subscale just for interpersonal loneliness items
UCLAdat = mutate(UCLAdat, UCLA_sub = (UCLA_1_2.rev + UCLA_1_3.rev + UCLA_1_4.rev +
                                        UCLA_2_1.rev + UCLA_2_9 + UCLA_2_10)/6)

# histogram shows scores normally distributed
hist(UCLAdat$UCLA_sub, breaks = 30)

require(psych)
# cronbach's alpha for 6-item subscale: .89
alpha(UCLAdat[,c(22,23,24,27, 19, 20)])
# cronbach's alpha for whole UCLA scale: .93
alpha(UCLAdat[,c(22:32,1, 5, 6, 9, 10, 15, 16, 19, 20)])

# Satisfaction with Life scale
# 5 items, on scale of 1 (strongly disagree) to 7 (strongly agree)
# Average scores
# Higher score is more satisfied with life

SWLSdat = select(noBS, one_of(SWLS)) %>%
  cbind(select(noBS, Subject))

# creates SWLS score by averaging 5 items (although doc on drive says sum?)
SWLSdat = mutate(SWLSdat, SWLS_total = (SLWS_1 + SLWS_2 + SLWS_3 + SLWS_4 + SLWS_5)/5)

# histogram shows scores somewhat normally distribtued but flat
hist(SWLSdat$SWLS_total, breaks = 20)


# Self-esteem scale (Heatherton)
# 20 items, on scale of 1 (not at all) to 5 (extremely)
# Reverse scored: 2, 4, 5, 7, 8, 10, 13, 15, 16, 17, 18, 19, 20
# Higher score is higher self-esteem
# 3 subscales: performance, social, appearance

SEdat = select(noBS, one_of(SE)) %>%
  cbind(select(noBS, Subject))

for (i in unique(SEdat$Subject)) {
  SEdat$SE_1_2.rev[SEdat$Subject == i] = 6 - SEdat$SE_1_2[SEdat$Subject == i]
  SEdat$SE_1_4.rev[SEdat$Subject == i] = 6 - SEdat$SE_1_4[SEdat$Subject == i]
  SEdat$SE_1_5.rev[SEdat$Subject == i] = 6 - SEdat$SE_1_5[SEdat$Subject == i]
  SEdat$SE_1_7.rev[SEdat$Subject == i] = 6 - SEdat$SE_1_7[SEdat$Subject == i]
  SEdat$SE_1_8.rev[SEdat$Subject == i] = 6 - SEdat$SE_1_8[SEdat$Subject == i]
  SEdat$SE_1_10.rev[SEdat$Subject == i] = 6 - SEdat$SE_1_10[SEdat$Subject == i]
  SEdat$SE_2_3.rev[SEdat$Subject == i] = 6 - SEdat$SE_2_3[SEdat$Subject == i]
  SEdat$SE_2_5.rev[SEdat$Subject == i] = 6 - SEdat$SE_2_5[SEdat$Subject == i]
  SEdat$SE_2_6.rev[SEdat$Subject == i] = 6 - SEdat$SE_2_6[SEdat$Subject == i]
  SEdat$SE_2_7.rev[SEdat$Subject == i] = 6 - SEdat$SE_2_7[SEdat$Subject == i]
  SEdat$SE_2_8.rev[SEdat$Subject == i] = 6 - SEdat$SE_2_8[SEdat$Subject == i]
  SEdat$SE_2_9.rev[SEdat$Subject == i] = 6 - SEdat$SE_2_9[SEdat$Subject == i]
  SEdat$SE_2_10.rev[SEdat$Subject == i] = 6 - SEdat$SE_2_10[SEdat$Subject == i]
}

# creates SE total score by averaging 20 items (although doc on drive says sum?)
SEdat = mutate(SEdat, SE_total = (SE_1_1 + SE_1_2.rev + SE_1_3 + SE_1_4.rev +
                                          SE_1_5.rev + SE_1_6 + SE_1_7.rev + SE_1_8.rev +
                                          SE_1_9 + SE_1_10.rev + SE_2_1 + SE_2_2 +
                                          SE_2_3.rev + SE_2_4 + SE_2_5.rev + SE_2_6.rev +
                                          SE_2_7.rev + SE_2_8.rev + SE_2_9.rev + SE_2_10.rev)/20)

# creates SE performance sub score by averaging 1, 4, 5, 9, 14, 18, 19
SEdat = mutate(SEdat, SE_perf = (SE_1_1 + SE_1_4.rev + SE_1_5.rev + 
                                    SE_1_9 + SE_2_4 + SE_2_8.rev + SE_2_9.rev)/7)

# creates SE social sub score by averaging 2, 8, 10, 13, 15, 17, 20
SEdat = mutate(SEdat, SE_social = (SE_1_2.rev + SE_1_8.rev + SE_1_10.rev + 
                                    SE_2_3.rev + SE_2_5.rev + SE_2_7.rev + SE_2_10.rev)/7)

# creates SE appearance sub score by averaging 3, 6, 7, 11, 12, 16
SEdat = mutate(SEdat, SE_appear = (SE_1_3 + SE_1_6 + SE_1_7.rev + SE_2_1 + SE_2_2 + SE_2_6.rev)/6)

# SE_total is slightly negatively skewed
hist(SEdat$SE_total, breaks = 30)
# SE_perf is highly negatively skewed
hist(SEdat$SE_perf, breaks = 30)
# SE_social is slightly negatively skewed
hist(SEdat$SE_social, breaks = 30)
# SE_appear is fairly normal
hist(SEdat$SE_appear, breaks = 30)


# Mood items
# 16 items, on scale of 1 (not at all) to 5 (extremely)
# Higher score is more negative mood
# Not sure about sub scales

Mooddat = select(noBS, one_of(Mood)) %>%
  cbind(select(noBS, Subject))

# creates Mood total score by averaging 16 items
Mooddat = mutate(Mooddat, Mood_total = (Mood_1 + Mood_2 + Mood_3 + Mood_4 + Mood_5 + Mood_6
                                        + Mood_7 + Mood_8 + Mood_9 + Mood_10 + Mood_11 + Mood_12
                                        + Mood_13 + Mood_14 + Mood_15 + Mood_16)/16)
# heavily positively skewed
hist(Mooddat$Mood_total, breaks = 30)



# Perceived discrimination (general)
# 9 items, on scale of 1 (never) to 6 (almost every day)
# Higher score is higher perceived discrimination

PDdat = select(noBS, one_of(Disc)) %>%
  cbind(select(noBS, Subject))

# creates PD (general) score by summing 9 items
PDdat = mutate(PDdat, PD_general = (Disc_1 + Disc_2 + Disc_3 + Disc_4 + Disc_5 + Disc_6 + Disc_7 +
                                      Disc_8 + Disc_9))

# positively skewed
hist(PDdat$PD_general, breaks = 30)


# Perceived discrimination (race)
# 9 items, on scale of 1 (never) to 6 (almost every day)
# Higher score is higher perceived discrimination

PDRdat = select(noBS, one_of(DiscR)) %>%
  cbind(select(noBS, Subject))

# creates PD (race) score by summing 9 items
PDRdat = mutate(PDRdat, PD_race = (DiscR_1 + DiscR_2 + DiscR_3 + DiscR_4 + DiscR_5 + DiscR_6 + DiscR_7 +
                                      DiscR_8 + DiscR_9))

# positively skewed
hist(PDRdat$PD_race, breaks = 30)



# Existential Isolation Scale
# 6 items, on scale of 1 (strongly disagree) to 9 (strongly agree)
# Reverse scored: 1, 2, 3, 6
# Higher score is higher existential isolation

EIdat = select(noBS, one_of(EIS)) %>%
  cbind(select(noBS, Subject))

for (i in unique(EIdat$Subject)) {
  EIdat$EIS_1.rev[EIdat$Subject == i] = 10 - EIdat$EIS_1[EIdat$Subject == i]
  EIdat$EIS_2.rev[EIdat$Subject == i] = 10 - EIdat$EIS_2[EIdat$Subject == i]
  EIdat$EIS_3.rev[EIdat$Subject == i] = 10 - EIdat$EIS_3[EIdat$Subject == i]
  EIdat$EIS_6.rev[EIdat$Subject == i] = 10 - EIdat$EIS_6[EIdat$Subject == i]
}

# creates EI total score by averaging 6 items
EIdat = mutate(EIdat, EI_total = (EIS_1.rev + EIS_2.rev + EIS_3.rev + EIS_4 +
                                    EIS_5 + EIS_6.rev)/6)

# normally distributed
hist(EIdat$EI_total, breaks = 30)



# Psychological wellbeing (Bradburn)- also called affect balance scale
# 5 positive affect items, 5 negative affect items
# Answer yes (1) or no (2) to each item
# Number of "yes"s on NA scale subtracted from "yes"s on PA scale
# Higher score is more wellbeing

WellBdat = select(noBS, one_of(WellB)) %>%
  cbind(select(noBS, Subject))

for (i in unique(WellBdat$Subject)) {       
  # calculate positive affect score
  PAtemp = WellBdat[WellBdat$Subject == i,1:5]
  WellBdat$WellB_PA[WellBdat$Subject == i] = 10 - sum(PAtemp)
  # calculate negative affect score
  NAtemp = WellBdat[WellBdat$Subject == i,6:10]
  WellBdat$WellB_NA[WellBdat$Subject == i] = 10 - sum(NAtemp)
  # subtract NA from PA for total score
  WellBdat$WellB_total[WellBdat$Subject == i] = (10 - sum(PAtemp)) - (10 - sum(NAtemp))
  
}  

# distribution is eh
hist(WellBdat$WellB_total, breaks = 30)



# Psychological wellbeing (Ryff)
# 21 items, scored from 1 (strongly disagree) to 6 (strongly agree)
# Reverse score: 13, 14, 18, 19, 26, 30, 31, 32, 36
# Higher score is more wellbeing
# Subscales: autonomy, environmental mastery, self-acceptance

WellRdat = select(noBS, one_of(WellR)) %>%
  cbind(select(noBS, Subject))

for (i in unique(WellRdat$Subject)) {
  WellRdat$Rau_13.rev[WellRdat$Subject == i] = 10 - WellRdat$Rau_13[WellRdat$Subject == i]
  WellRdat$Rem_14.rev[WellRdat$Subject == i] = 10 - WellRdat$Rem_14[WellRdat$Subject == i]
  WellRdat$Rsa_18.rev[WellRdat$Subject == i] = 10 - WellRdat$Rsa_18[WellRdat$Subject == i]
  WellRdat$Rau_19.rev[WellRdat$Subject == i] = 10 - WellRdat$Rau_19[WellRdat$Subject == i]
  WellRdat$Rem_26.rev[WellRdat$Subject == i] = 10 - WellRdat$Rem_26[WellRdat$Subject == i]
  WellRdat$Rsa_30.rev[WellRdat$Subject == i] = 10 - WellRdat$Rsa_30[WellRdat$Subject == i]
  WellRdat$Rau_31.rev[WellRdat$Subject == i] = 10 - WellRdat$Rau_31[WellRdat$Subject == i]
  WellRdat$Rem_32.rev[WellRdat$Subject == i] = 10 - WellRdat$Rem_32[WellRdat$Subject == i]
  WellRdat$Rsa_36.rev[WellRdat$Subject == i] = 10 - WellRdat$Rsa_36[WellRdat$Subject == i]
}

# creates total wellbeing score by averaging all 21 items
WellRdat = mutate(WellRdat, WellR_total = (Rau_1 + Rem_2 + Rsa_6 + Rau_7 +
                                           Rem_8 + Rsa_12 + Rau_13.rev + Rem_14.rev +
                                           Rsa_18.rev + Rau_19.rev +
                                           Rem_20 + Rsa_24 + Rau_25 + Rem_26.rev +
                                           Rsa_30.rev + Rau_31.rev +
                                           Rem_32.rev + Rsa_36.rev +
                                           Rau_37 + Rem_38 + Rsa_42)/21)

# creates autonomy wellbeing score by averaging 7 items
WellRdat = mutate(WellRdat, WellR_auto = (Rau_1 +
                                             Rau_7 +
                                             Rau_13.rev +
                                             Rau_19.rev +
                                             Rau_25 +
                                             Rau_31.rev +
                                             Rau_37)/7)

# creates environmental mastery wellbeing score by averaging 7 items
WellRdat = mutate(WellRdat, WellR_envir = (Rem_2 +
                                             Rem_8 +
                                             Rem_14.rev +
                                             Rem_20 +
                                             Rem_26.rev +
                                             Rem_32.rev +
                                             Rem_38)/7)

# creates self-acceptance wellbeing score by averaging 7 items
WellRdat = mutate(WellRdat, WellR_accept = (Rsa_6 +
                                             Rsa_12 +
                                             Rsa_18.rev +
                                             Rsa_24 +
                                             Rsa_30.rev +
                                             Rsa_36.rev +
                                             Rsa_42)/7)

# all look pretty normally distributed
hist(WellRdat$WellR_total, breaks = 30)
hist(WellRdat$WellR_auto, breaks = 30)
hist(WellRdat$WellR_envir, breaks = 30)
hist(WellRdat$WellR_accept, breaks = 30)



##########################################################################################
####################### add all calculated scores to original data frame #################
####################### (doesn't include bad subjects) ###################################
##########################################################################################

datWithTotScores = left_join(noBS, UCLAdat[,21:34], by = "Subject") %>%
  cbind(select(SWLSdat, SWLS_total)) %>%
  cbind(select(SEdat, SE_total)) %>%
  cbind(select(SEdat, SE_perf)) %>%
  cbind(select(SEdat, SE_social)) %>%
  cbind(select(SEdat, SE_appear)) %>%
  cbind(select(Mooddat, Mood_total)) %>%
  cbind(select(PDdat, PD_general)) %>%
  cbind(select(PDRdat, PD_race)) %>%
  cbind(EIdat[,8:11]) %>%
  cbind(select(EIdat, EI_total)) %>%
  cbind(select(WellBdat, WellB_total)) %>%
  cbind(select(WellBdat, WellB_PA)) %>%
  cbind(select(WellBdat, WellB_NA)) %>%
  cbind(select(WellRdat, WellR_total)) %>%
  cbind(select(WellRdat, WellR_auto)) %>%
  cbind(select(WellRdat, WellR_envir)) %>%
  cbind(select(WellRdat, WellR_accept))

write.table(datWithTotScores, file = "datWithCalcScores.txt", sep = "\t", row.names = F)

