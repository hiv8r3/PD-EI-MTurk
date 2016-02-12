# Separate into two separate data sets for white participants and minority

dat = read.delim("datWithCalcScores.txt")


latSub = dat[!(is.na(dat$Ethn_1 == 1)),] # 75 Latino

blackSub = dat[!(is.na(dat$Ethn_3 == 1)),] # 187 Black

asianSub = dat[!(is.na(dat$Ethn_4 == 1)),] # 114 Asian

nativeSub = dat[!(is.na(dat$Ethn_5 == 1)),] # 17 Native American

meSub = dat[!(is.na(dat$Ethn_6 == 1)),] # 3 Middle Eastern

otherSub = dat[!(is.na(dat$Ethn_7 == 1)),] # 25 other

whiteSub = dat[!(is.na(dat$Ethn_2 == 1)),] # 165 White subs

# combine non-White subs without double counting people who said more than one
# note: counts people who are mixed (White and something else)
nonWhiteSubs = rbind(latSub, blackSub[!(blackSub$Subject %in% latSub$Subject),])
nonWhiteSubs = rbind(nonWhiteSubs, asianSub[!(asianSub$Subject %in% nonWhiteSubs$Subject),])
nonWhiteSubs = rbind(nonWhiteSubs, nativeSub[!(nativeSub$Subject %in% nonWhiteSubs$Subject),])
nonWhiteSubs = rbind(nonWhiteSubs, meSub[!(meSub$Subject %in% nonWhiteSubs$Subject),])

nrow(nonWhiteSubs) # 377
length(unique(nonWhiteSubs$Subject)) #377

# Examine subjects who said "other"
View(otherSub[,c(134:141,155:170)])
View(otherSub[grep("white", otherSub$Ethn_7_TEXT),c(134:141,155:170)])

# add some to nonWhites 
nW = c(92, 152, 175, 258, 348, 371, 411, 437, 496, 511, 535, 539, 561)
nonWhiteSubs = rbind(nonWhiteSubs, otherSub[otherSub$Subject %in% nW & !(otherSub$Subject %in% nonWhiteSubs$Subject),])

# some have no race info at all
noInfo = c(2, 5, 17, 23, 24, 28, 30, 32, 34, 35, 36, 38, 40, 95)

# the rest should be white
WhiteSubs = dat[!(dat$Subject %in% nonWhiteSubs$Subject) & !(dat$Subject %in% noInfo),]

# check to make sure it all adds up
nrow(WhiteSubs) # 142
nrow(nonWhiteSubs) # 389
length(noInfo) # 14

length(unique(dat$Subject)) # it all adds up to 545 subjects

# write files for white subjects and non white subjects
write.table(WhiteSubs, "WhiteSubsWithCalcScores.txt", sep = "\t", row.names = F)
write.table(nonWhiteSubs, "nonWhiteSubsWithCalcScores.txt", sep = "\t", row.names = F)
