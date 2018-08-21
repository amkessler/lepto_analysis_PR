#multiple mortality CDC files - territories:
# https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm


library(tidyverse)
library(ggplot2)


### import fixed width files ####

#create function with field lengths and names

import_mmdc_file <- function(myfilename) {
  deaths <- read_fwf(myfilename, 
                     fwf_widths(c(18, 
                                  1, 
                                  1, 
                                  2, 
                                  3, 
                                  2, 
                                  1,
                                  2,
                                  2,
                                  2,
                                  3,
                                  13,
                                  1,
                                  7,
                                  2,
                                  2,
                                  1,
                                  1,
                                  2,
                                  2,
                                  1,
                                  4,
                                  1,
                                  2,
                                  2,
                                  2,
                                  2,
                                  1,
                                  1,
                                  1,
                                  16,
                                  4,
                                  1,
                                  1,
                                  1,
                                  1,
                                  34,
                                  1,
                                  1,
                                  4,
                                  3,
                                  1,
                                  3,
                                  3,
                                  2,
                                  1,
                                  2
                     ), 
                     c("reserved1", 
                       "record.type", 
                       "resident.status", 
                       "state.occur", 
                       "county.occur",
                       "reserved2",
                       "pop.size.county",
                       "state.of.residence",
                       "reserved3",
                       "state.recode",
                       "county.residence",
                       "reserved4",
                       "pop.county",
                       "reserved5",
                       "state.birth.recode",
                       "education.1989",
                       "education.2003",
                       "education.flag",
                       "month.of.death",
                       "reserved6",
                       "gender",
                       "detail.age",
                       "age.subflag",
                       "age.recode52",
                       "age.recode27",
                       "age.recode12",
                       "infant.age.recode22",
                       "place.of.death",
                       "marital.status",
                       "day.of.week.death",
                       "reserved7",
                       "current.data.year",
                       "work.injury",
                       "manner.of.death",
                       "method.of.disposition",
                       "autopsy",
                       "reserved8",
                       "activity.code",
                       "place.of.injury",
                       "cause.of.death.icd10",
                       "358.cause.recode",
                       "reserved9",
                       "113.cause.recode",
                       "130.infant.recode",
                       "39.cause.recode",
                       "reserved10",
                       "number.entityaxis.conditions"
                     )))
  return(deaths)
}


#feed files into import function
mmdc10 <- import_mmdc_file("VS10MORT.txt")
mmdc11 <- import_mmdc_file("VS11MORT.txt")
mmdc12 <- import_mmdc_file("VS12MORT.txt")
mmdc13 <- import_mmdc_file("VS13MORT.txt")
mmdc14 <- import_mmdc_file("VS14MORT.txt")
mmdc15 <- import_mmdc_file("VS15MORT.txt")
mmdc16 <- import_mmdc_file("VS16MORT.txt")







### combine into one####

mmdc_all <- rbind(mmdc10, mmdc11, mmdc12, mmdc13, mmdc14, mmdc15, mmdc16)

mmdc_all %>% 
  group_by(state.occur) %>% 
  tally()


### pull out just PR and lepto cases ####

pr_only <- mmdc_all %>% 
  filter(state.occur == "PR")

#note: there were no A270 or A279 cases, only A279
pr_lepto <- pr_only %>% 
  filter(cause.of.death.icd10 == "A279")


#group by year, month
pr_lepto %>% 
  group_by(current.data.year) %>% 
  tally()

lepto_by_month <- pr_lepto %>% 
  group_by(current.data.year, month.of.death) %>% 
  tally()


#export lepto files
write.csv(pr_lepto, "pr_lepto.csv")
write.csv(lepto_by_month, "pr_lepto_bymonth.csv")




#****************
### other analysis ####

#all causes in PR
pr_causesbyyear <- pr_only %>% 
  group_by(cause.of.death.icd10, current.data.year) %>% 
  tally()


ggplot(pr_causesbyyear, aes(x=current.data.year, y=n)) +
  geom_col() +
  labs(title = "Total deaths in Puerto Rico", x = "Year", 
     y = "Num deaths", col = "") 


#go from long to wide 
pr_causesbyyear_wide <- pr_causesbyyear %>% 
  spread(current.data.year, n)

#avg, min, max
temp <- pr_causesbyyear_wide[,2:8]

mean_val <- apply(temp, 1, function(x) mean(x, na.rm = TRUE))
max_val <- apply(temp, 1, function(x) max(x, na.rm = TRUE))
min_val <- apply(temp, 1, function(x) min(x, na.rm = TRUE))

pr_causesbyyear_wide$mean.val <- round(mean_val,3)
pr_causesbyyear_wide$max.val <- max_val
pr_causesbyyear_wide$min.val <- min_val





#at least 10 deaths in one year
ten_or_more <- pr_causesbyyear_wide %>% 
  filter(max.val > 9)

