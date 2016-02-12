dat = read.delim("datWithCalcScores.txt")

# UCLA loneliness: 20 items
UCLAtime = dat$T3_3 + dat$T4_3
hist(UCLAtime, breaks = 30)
m1 = mean(UCLAtime, na.rm = T)
s1 = sd(UCLAtime, na.rm = T)

sum(UCLAtime < 20, na.rm = T) # 14 subjects
sum(UCLAtime < (m1-s1), na.rm = T) # 56 subjects
sum(UCLAtime > (m1+3*s1), na.rm = T) # 11 subjects

subs1 = dat$Subject[(dat$T3_3 + dat$T4_3) < (m1-s1)]
subs1 = c(subs1, dat$Subject[(dat$T3_3 + dat$T4_3) > (m1+3*s1)])

# Satisfaction with life: 5 items
SWLStime = dat$T5_3
hist(SWLStime, breaks = 30)
m2 = mean(SWLStime, na.rm = T)
s2 = sd(SWLStime, na.rm = T)

sum(SWLStime < 5, na.rm = T) # 3 subjects
sum(SWLStime < (m2-s2), na.rm = T) # 2 subjects
sum(SWLStime > (m2+3*s2), na.rm = T) # 8 subjects

subs2 = dat$Subject[dat$T5_3 < (m2-s2)]
subs2 = c(subs2, dat$Subject[dat$T5_3 > (m1+3*s1)])

# Self-esteem: 20 items
SEtime = dat$T6_3
hist(SEtime, breaks = 30)
m3 = mean(SEtime, na.rm = T)
s3 = sd(SEtime, na.rm = T)

sum(SEtime < 20, na.rm = T) # 67 subjects
sum(SEtime < (m3-s3), na.rm = T) # 3 subjects
sum(SEtime > (m3+3*s3), na.rm = T) # 11 subjects

subs3 = dat$Subject[dat$T6_3 < (m3-s3)]
subs3 = c(subs3, dat$Subject[dat$T6_3 > (m3+3*s3)])

# Mood: 16 items
Moodtime = dat$T7_3
hist(Moodtime, breaks = 30)
m4 = mean(Moodtime, na.rm = T)
s4 = sd(Moodtime, na.rm = T)

sum(Moodtime < 16, na.rm = T) # 28 subjects
sum(Moodtime < (m4-s4), na.rm = T) # 23 subjects
sum(Moodtime > (m4+3*s4), na.rm = T) # 7 subjects

subs4 = dat$Subject[dat$T7_3 < (m4-s4)]
subs4 = c(subs4, dat$Subject[dat$T7_3 > (m4+3*s4)])

# PD: 9 items
PDtime = dat$T8_3 + dat$T9_3
hist(PDtime, breaks = 30)
m5 = mean(PDtime, na.rm = T)
s5 = sd(PDtime, na.rm = T)

sum(PDtime < 9, na.rm = T) # 0 subjects
sum(PDtime < (m5-s5), na.rm = T) # 19 subjects
sum(PDtime > (m5+3*s5), na.rm = T) # 7 subjects

subs5 = dat$Subject[dat$T8_3 + dat$T9_3 < (m5-s5)]
subs5 = c(subs5, dat$Subject[dat$T8_3 + dat$T9_3 > (m5+3*s5)])

# PDR: 9 items
PDRtime = dat$T10_3 + dat$T11_3
hist(PDRtime, breaks = 30)
m6 = mean(PDRtime, na.rm = T)
s6 = sd(PDRtime, na.rm = T)

sum(PDRtime < 9, na.rm = T) # 1 subjects
sum(PDRtime < (m6-s6), na.rm = T) # 0 subjects
sum(PDRtime > (m6+3*s6), na.rm = T) # 7 subjects

subs6 = dat$Subject[dat$T10_3 + dat$T11_3 < (m6-s6)]
subs6 = c(subs6, dat$Subject[dat$T10_3 + dat$T11_3 > (m6+3*s6)])

# EIS: 9 items
EIStime = dat$T12_3
hist(EIStime, breaks = 30)
m7 = mean(EIStime, na.rm = T)
s7 = sd(EIStime, na.rm = T)

sum(EIStime < 9, na.rm = T) # 10 subjects
sum(EIStime < (m7-s7), na.rm = T) # 1 subjects
sum(EIStime > (m7+3*s7), na.rm = T) # 8 subjects

subs7 = dat$Subject[dat$T12_3 < (m7-s7)]
subs7 = c(subs7, dat$Subject[dat$T12_3 > (m7+3*s7)])

# WellB: 10 items
WellBtime = dat$T13_3
hist(WellBtime, breaks = 30)
m8 = mean(WellBtime, na.rm = T)
s8 = sd(WellBtime, na.rm = T)

sum(WellBtime < 10, na.rm = T) # 14 subjects
sum(WellBtime < (m8-s8), na.rm = T) # 0 subjects
sum(WellBtime > (m8+3*s8), na.rm = T) # 7 subjects

subs8 = dat$Subject[dat$T13_3 < (m8-s8)]
subs8 = c(subs8, dat$Subject[dat$T13_3 > (m8+3*s8)])

# WellR: 10 items
WellRtime = dat$T14_3
hist(WellRtime, breaks = 30)
m9 = mean(WellRtime, na.rm = T)
s9 = sd(WellRtime, na.rm = T)

sum(WellRtime < 10, na.rm = T) # 24 subjects
sum(WellRtime < (m9-s9), na.rm = T) # 2 subjects
sum(WellRtime > (m9+3*s9), na.rm = T) # 8 subjects

subs9 = dat$Subject[dat$T14_3 < (m9-s9)]
subs9 = c(subs9, dat$Subject[dat$T14_3 > (m9+3*s9)])

# total time taken
totalTime = UCLAtime + SWLStime + SEtime + Moodtime + PDtime + PDRtime + EIStime + WellBtime + WellRtime
hist(totalTime, breaks = 30)

badsubs1 = unique(c(subs1, subs2, subs3, subs4, subs5, subs6, subs7, subs8, subs9)) # 116 subjects


#############################################################################################
##################### Attention checks ##########################################
##########################################################################################

# Complete in one sitting (1) or interuppted (2)
a = (dat$Interr == 2) 
sum(a, na.rm = T) # 12 subjects
subs10 = dat$Subject[dat$Interr == 2]

# Not distracted (1) or distracted (2)
b = (dat$Distr == 2) 
sum(b, na.rm = T) # 21 subjects
subs11 = dat$Subject[dat$Distr == 2]

# Had total privacy (1) or others influenced response (2)
c = (dat$Privacy == 2) 
sum(c, na.rm = T) # 4 subjects
subs12 = dat$Subject[dat$Privacy == 2]

# Completed on full size (1) or mobile (2)
sum(dat$Scrn_Size == 2, na.rm = T) # 49 subjects

# People who have answered 1 or more questions with undesirable response
sum(a + b + c == 1, na.rm = T) # 24 subjects
sum(a + b + c == 2, na.rm = T) # 5 subjects
sum(a + b + c == 3, na.rm = T) # 1 subject

subs13 = dat$Subject[(dat$Interr + dat$Distr + dat$Privacy) >= 5] # 3 items + 2 yeses 

badsubs2 = unique(c(badsubs1, subs13)) # 117 subjects (only added 1 on top of ones whose time was unacceptable)
