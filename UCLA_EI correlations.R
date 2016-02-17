require(Hmisc)
require(dplyr)

# investigating relationships between UCLA items and EI
# first looks at correlations between UCLA items and EI_total
# then factor analysis with UCLA items and EI items included
# each done separately for each sample

nonWhite = read.delim("noBS_nonwhiteData.txt") # bad subjects taken out (answered too quickly or were distracted)
White = read.delim("noBS_whiteData.txt")
all = read.delim("noBS_allData.txt")

cor(nonWhite$UCLA_total, nonWhite$EI_total)
# correlation in nonWhite sample is .54

cor(White$UCLA_total[!(is.na(White$UCLA_total))], White$EI_total[!(is.na(White$UCLA_total))])
# correlation in White sample is .62

datNW = select(nonWhite, starts_with("UCLA")) %>%
  select(-UCLA_1_2,
         -UCLA_1_3,
         -UCLA_1_4,
         -UCLA_1_7,
         -UCLA_1_8,
         -UCLA_2_1,
         -UCLA_2_2,
         -UCLA_2_3,
         -UCLA_2_4,
         -UCLA_2_7,
         -UCLA_2_8,
         -UCLA_total) %>%
  cbind(nonWhite$EI_total)

correlNW = cor(datNW)
sort(correlNW[,21])

datW = select(White, starts_with("UCLA")) %>%
  select(-UCLA_1_2,
         -UCLA_1_3,
         -UCLA_1_4,
         -UCLA_1_7,
         -UCLA_1_8,
         -UCLA_2_1,
         -UCLA_2_2,
         -UCLA_2_3,
         -UCLA_2_4,
         -UCLA_2_7,
         -UCLA_2_8,
         -UCLA_total) %>%
  cbind(White$EI_total)
datW =  datW[!(is.na(datW$UCLA_1_7)),]

correlW = cor(datW)
sort(correlW[,21])


################ Factor analysis #######################

# for nonWhite sample
datNW2 = select(nonWhite, starts_with("UCLA")) %>%
  cbind(select(nonWhite, starts_with("EIS"))) %>%
  select(-UCLA_1_2,
         -UCLA_1_3,
         -UCLA_1_4,
         -UCLA_1_7,
         -UCLA_1_8,
         -UCLA_2_1,
         -UCLA_2_2,
         -UCLA_2_3,
         -UCLA_2_4,
         -UCLA_2_7,
         -UCLA_2_8,
         -UCLA_total,
         -EIS_1,
         -EIS_2,
         -EIS_3,
         -EIS_6)

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors, 
# with oblique (promax) rotation 
fitNW <- factanal(datNW2, 2, rotation="promax")
print(fitNW, digits=2, cutoff=.3, sort=TRUE)


# for White sample
datW2 = select(White, starts_with("UCLA")) %>%
  cbind(select(White, starts_with("EIS"))) %>%
  select(-UCLA_1_2,
         -UCLA_1_3,
         -UCLA_1_4,
         -UCLA_1_7,
         -UCLA_1_8,
         -UCLA_2_1,
         -UCLA_2_2,
         -UCLA_2_3,
         -UCLA_2_4,
         -UCLA_2_7,
         -UCLA_2_8,
         -UCLA_total,
         -EIS_1,
         -EIS_2,
         -EIS_3,
         -EIS_6)
datW2 =  datW2[!(is.na(datW2$UCLA_1_7)),]

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors, 
# with oblique (promax) rotation 
fitW <- factanal(datW2, 2, rotation="promax")
print(fitW, digits=2, cutoff=.3, sort=TRUE)
