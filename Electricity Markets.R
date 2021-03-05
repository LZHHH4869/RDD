library(ggplot2)
library(plyr)

#Data Importation for 2014 to 2018####
#data18 = read.csv("C:/Users/Nyarlathotep/Documents/Econ - Markets for Electricity/LiteUpTexas_mod.csv")

LiteUp2_df = read.csv("C:/Users/Nyarlathotep/Documents/Econ - Markets for Electricity/LiteUpTexas_mod_2.csv")

acs18_basic =  read.csv("C:/Users/Nyarlathotep/Documents/Econ - Markets for Electricity/acs2018_basic/acs2018_basic.csv")
acs18_flags =  read.csv("C:/Users/Nyarlathotep/Documents/Econ - Markets for Electricity/acs2018_flags/acs2018_flags.csv")
acs18_household =  read.csv("C:/Users/Nyarlathotep/Documents/Econ - Markets for Electricity/acs2018_household/acs2018_household.csv")
acs18_unit =  read.csv("C:/Users/Nyarlathotep/Documents/Econ - Markets for Electricity/acs2018_unit/acs2018_unit.csv")

#Joining out columns by a common key
acs18_df = merge(acs18_basic, acs18_household, by = "SERIALNO")
acs18_df = merge(acs18_df, acs18_unit, by = "SERIALNO")
acs18_df = merge(acs18_df, acs18_flags, by = "SERIALNO")

#Importing data for 2019 and transformation####
#acs19_df = read.csv("C:/Users/Nyarlathotep/Documents/Econ - Markets for Electricity/csv_htx/psam_h48.csv")

#Matching the column label for PUMA in 2014 to 2018
#acs19_df$PUMA = acs19_df$PUMA10

#Find differences between 
#colnames(acs19_df)[colnames(acs19_df) %in% colnames(acs18_df)]

#Adding a year variable.
#acs19_df$year = substr(acs19_df$SERIALNO, 1, 4)

#Importing the Aggrated Lite UP Data####

#Time Indices
#data18$month_index = rep(c(seq(9, 12), seq(1,8)), 7)
#data18$time_index = 100*as.numeric(data18$Year) + data18$month_index
#data18$index = seq(1, 84)

#Import information on deregulation
puma_ma =  read.csv("C:/Users/Nyarlathotep/Documents/Econ - Markets for Electricity/PUMA Matching.csv")
puma_dereg = subset(puma_ma, App.City == 1)
puma_reg = subset(puma_ma, App.City == 0)

#Adjusting Variables####
acs18_df$dereg = rep(NA, length(acs18_df$PUMA))
acs18_df$dereg = replace(acs18_df$dereg, acs18_df$PUMA %in% unique(puma_dereg$Code), 1)
acs18_df$dereg = replace(acs18_df$dereg, acs18_df$PUMA %in% unique(puma_reg$Code), 0)

#add an adjusted energy cost variable
acs18_df$ELEAJ = acs18_df$ELEP*acs18_df$ADJHSG/1000000

#add energy costs as a share of income
acs18_df$share = acs18_df$ELEAJ/(acs18_df$HINCP*acs18_df$ADJINC/1000000)

#we need to add a variable that indicates if we are in the pre-treatment period.
acs18_df$treat = rep(NA, length(acs18_df$SERIALNO))
acs18_df$treat = replace(acs18_df$treat,
                         as.numeric(acs18_df$year) > 2016, 
                         "Post")
acs18_df$treat = replace(acs18_df$treat,
                         as.numeric(acs18_df$year) <= 2016, 
                         "Pre")

#Join Lite-UP Texas Data to the acs data
acs18_df = join(acs18_df, subset(LiteUp2_df, year >= 2014), by = "year", type = "left")

#Second treat variable that measures magnitude.
#acs18_df$treat2 = acs18_df$treat*acs18_df$Avg.Yearly.Expense

#Adding an interaction variable between SNAP status and residence tenure. The theory is, those that own houses will be more aware of Lite-Up Texas and thus, more likely to utilize the program.Ideally, it will be a stronger instrument to help us predict the outcome.

#acs18_df$FS_Ten = rep(NA, length(acs18_df$TEN))
#acs18_df$FS_Ten = replace(acs18_df$FS_Ten, acs18_df$TEN <= 2, 1)
#acs18_df$FS_Ten = replace(acs18_df$FS_Ten, acs18_df$TEN >= 3, 0)

#Thrid treat variable narrows the population to those that both had food stamps during the treatment period in the treatment areas and had a long tenure.
#acs18_df$treat3 = acs18_df$treat*acs18_df$Avg.Yearly.Expense.3

#######ACS graphs#######

ggplot(subset(acs18_df, FS == 1 & dereg >= 0), aes(x = as.factor(year), y = as.numeric(ELEAJ), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Monthly Electricity Cost for SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  ylim(0, 300) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

ggplot(subset(acs18_df, FS == 1 & dereg >= 0 & (R65 == 1 | R65 == 2)), aes(x = as.factor(year), y = as.numeric(ELEAJ), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Monthly Electricity Share for Elderly SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

ggplot(subset(acs18_df, FS == 1 & dereg >= 0), aes(x = as.factor(year), y = as.numeric(share), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Monthly Electricity Share for SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Share (in dollars)") +
  ylim(0, .5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

ggplot(subset(acs18_df, HINCP<= 40000 & dereg >= 0), aes(x = as.factor(year), y = as.numeric(ELEAJ), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Electricity Cost for SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Electricity Cost") +
  ylim(0, 300) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

ggplot(subset(acs18_df, FS == 1 & dereg >= 0 & (R65 == 1 | R65 == 2)), aes(x = as.factor(year), y = as.numeric(share), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Monthly Electricity Share for Elderly SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Share (in dollars)") +
  ylim(0, .5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

#Regressions##########

#Let's see if we can predict energy usage.

lm_1 = lm(ELEAJ ~
            dereg*Avg.Yearly.Expense +
            dereg +
            Avg.Yearly.Expense +
            FULP +
            HINCP +
            RMSP +
            GASP +
            HISPEED +
            WATP +
            as.factor(PUMA), data = subset(acs18_df, FS == 1 & dereg >= 0))

lm_1_1 = lm(ELEAJ ~
            dereg*treat +
            dereg +
            treat +
            FULP +
            HINCP +
            RMSP +
            GASP +
            HISPEED +
            WATP +
            as.factor(PUMA), data = subset(acs18_df, FS == 1 & dereg >= 0))

lm_2 = lm(ELEAJ ~
            dereg*Avg.Yearly.Expense +
            dereg +
            Avg.Yearly.Expense +
            FULP +
            HINCP +
            RMSP +
            GASP +
            HISPEED +
            WATP +
            as.factor(PUMA), data = subset(acs18_df, FS == 1 & dereg >= 0 & (R65 == 1 | R65 == 2)))

lm_2_1 = lm(ELEAJ ~
            dereg*treat +
            dereg +
            treat+
            FULP +
            HINCP +
            RMSP +
            GASP +
            HISPEED +
            WATP +
            as.factor(PUMA), data = subset(acs18_df, FS == 1 & dereg >= 0 & (R65 == 1 | R65 == 2)))


lm_3 = lm(ELEAJ ~
            dereg*Avg.Yearly.Expense +
            dereg +
            Avg.Yearly.Expense +
            FULP +
            HINCP +
            RMSP +
            GASP +
            HISPEED +
            WATP +
            as.factor(PUMA), data = subset(acs18_df, FS == 1 & dereg >= 0 & (R65 == 1 | R65 == 2) & (TEN == 1 | TEN == 2)))

lm_3_1 = lm(ELEAJ ~
            dereg*treat +
            treat +
            Avg.Yearly.Expense +
            FULP +
            HINCP +
            RMSP +
            GASP +
            HISPEED +
            WATP +
            as.factor(PUMA), data = subset(acs18_df, FS == 1 & dereg >= 0 & (R65 == 1 | R65 == 2) & (TEN == 1 | TEN == 2)))

#Lite Up graphs#######
#plot(data18$index,data18$Avg.Kilowatt.Per.Participant, type = "o")

#ggplot(data = data18, aes(x = as.factor(month_index), y = as.numeric(Avg.Kilowatt.Per.Participant), group = Year, color = Year)) +
#  geom_line() +
#  ggtitle("Fig. 1: Avg. Energy Subsidy by Month (grouped by Year)")  +
#  xlab("Year") +
#  ylab("Count") +
#  theme(plot.title = element_text(hjust = 0.5))
#Summary Statistics####
prop.table(table(acs18_df$FS, acs18_df$year,acs18_df$dereg ))

#Stat Graphs Continuous####

ggplot(subset(acs18_df, FS == 1 & dereg >= 0), aes(x = as.factor(year), y = as.numeric(HINCP), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Income for SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  ylim(0, 85000) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))


ggplot(subset(acs18_df, FS == 1 & dereg >= 0), aes(x = as.factor(year), y = as.numeric(FULP), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Fuel Costs for SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  ylim(0, 1200) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

ggplot(subset(acs18_df, FS == 1 & dereg >= 0), aes(x = as.factor(year), y = as.numeric(GASP), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Gas Costs for SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  ylim(0, 150) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

ggplot(subset(acs18_df, FS == 1 & dereg >= 0), aes(x = as.factor(year), y = as.numeric(WATP), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Water Costs for SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  ylim(0, 150) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

ggplot(subset(acs18_df, FS == 1 & dereg >= 0), aes(x = as.factor(year), y = as.numeric(RMSP), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Number of rooms for SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  ylim(0, 10) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))
#Stat Graphs - Discrete####

ggplot(subset(acs18_df, FS == 1 & dereg >= 0), aes(x = as.factor(year), fill = as.factor(dereg))) +
  geom_bar(position="dodge") +
  ggtitle("Number of SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  #ylim(0, 150) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

ggplot(subset(acs18_df, (R65 == 1 | R65 == 2 )& dereg >= 0), aes(x = as.factor(year), fill = as.factor(dereg))) +
  geom_bar(position="dodge") +
  ggtitle("Number of Households with elders (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  #ylim(0, 150) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

#Stat Graphs - Outcome####
ggplot(subset(acs18_df, FS == 1 & dereg >= 0), aes(x = as.factor(year), y = as.numeric(ELEAJ), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Electricity cost for SNAP recipients (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  #ylim(0, 85000) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

ggplot(subset(acs18_df, FS == 1 & dereg >= 0 & (R65 == 1 | R65 == 2)), aes(x = as.factor(year), y = as.numeric(ELEAJ), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Electricity cost for SNAP 65+ Households (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  #ylim(0, 85000) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))

ggplot(subset(acs18_df, FS == 1 & dereg >= 0 & (R65 == 1 | R65 == 2) & (TEN == 1 | TEN == 2)), aes(x = as.factor(year), y = as.numeric(ELEAJ), fill = as.factor(dereg))) +
  geom_boxplot(position=position_dodge(1)) +
  ggtitle("Electricity cost for SNAP 65+ Permanent Households (grouped by regulation status)")  +
  xlab("Year") +
  ylab("Cost (in dollars)") +
  #ylim(0, 85000) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Regulation", labels = c("Regulated", "Deregulated"))