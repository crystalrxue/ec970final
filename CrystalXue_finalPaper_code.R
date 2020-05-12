## Crystal Xue
## Ec 970: Economics of Immigration
## Final Paper
rm(list=ls())

setwd("~/Downloads/finalPaper")
## Start log file
sink(file = "CrystalXue_finalPaper_output.txt", split=TRUE)
install.packages("haven")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lmtest")
install.packages("sandwich")
install.packages("stargazer")

library(haven)
library(tidyverse)
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)

## Read in the data
raw <- read_dta("linked_1880-1900_couples.dta")

## Pull out relevant observations (head of household and/or spouse need occupational scores for BOTH years)

## If relate_1 and relate_2 both equal 1 or 2
## If neither occscore_1 nor occscore_2 equal 0
## If either bpl_1 or bpl_2 is greater than 99
## KEEP all entries with the same serial_2
## KEEP all immediate family members who are recorded in 1900

family <- list()

for (i in 1:nrow(raw)) {
  if(!is.na(raw$relate_1[i]) & !is.na(raw$relate_2[i])){
    if(raw$relate_1[i]==1 & raw$relate_2[i]==1 | raw$relate_1[i]==2 & raw$relate_2[i]==2) {
      if(raw$occscore_1[i]!=0 && raw$occscore_2[i]!=0) {
        if(raw$bpl_1[i]>99 || raw$bpl_2[i]>99) {
          family<-c(family,raw$serial_2[i])
          ## print(raw$serial_2[i])
        }  
      }
    }
  }
}

family
selected <- filter(raw, serial_2 %in% family) %>%
  filter(relate_2<=3, !is.na(relate_2))

fam <- data.frame(matrix(unlist(family), nrow=1820, byrow=T),stringsAsFactors=FALSE) %>%
  rename(
    serial_2 = matrix.unlist.family...nrow...1820..byrow...T.,
  )

fam$without_child <- 0
fam$first_child <- ""
fam$last_child <- ""

## remove families without children

for (n in 1:nrow(fam)) {
  temp <- filter(selected, selected$serial_2 == fam$serial_2[n])
  ## print(max(temp$relate_2))
  ## print(max(temp$relate_2)<3)
  if(max(temp$relate_2)<3) {
    fam$without_child[n] <- 1
  }
}

fam <- filter(fam, fam$without_child==0)

## Clean data (retain 3 names - parent, first child, and last child)

## need to ensure they have children in the 20 year time difference (so throw out anything older than 20)

for (i in 1:nrow(fam)) {
  immediate <- filter(raw, raw$serial_2 == fam$serial_2[i], raw$relate_2==3, is.na(raw$age_1), raw$age_2<=20)
  if (is.data.frame(immediate) & nrow(immediate)==0) {
  }
  else {
    fam$first_child[i] = immediate$namefrst_2[which.min(immediate$age_2)]
    ## print(fam$first_child[i])
    fam$last_child[i] = immediate$namefrst_2[which.max(immediate$age_2)]
    ## print(fam$last_child[i])
  }
}

## filter out entries without values
fam <- filter(fam, fam$first_child!="")
fam <- fam[-2]

## filter out middle initials
fam$first_child <- gsub( " .*$", "", fam$first_child)
fam$last_child <- gsub( " .*$", "", fam$last_child)
fam$first_child <- gsub("\\?","",fam$first_child)
fam$last_child <- gsub("\\?","",fam$last_child)

comb <- inner_join(fam, selected) %>%
  filter(relate_2==1 | relate_2==2)

## Calculate indicator variables for native spouse and native English-speaker
comb$native_spouse <- 1

for (i in 1:nrow(fam)) {
  temp <- filter(comb, comb$serial_2==fam$serial_2[i])
  ## print(fam$serial_2[i])
  ## print(temp)
  comb <- filter(comb, (comb$serial_2==fam$serial_2[i] & comb$bpl_2>99) | comb$serial_2!=fam$serial_2[i])
  ## print(min(temp$bpl_2))
  if(min(temp$bpl_2)>99) {
    comb$native_spouse[i] <- 0
  }
  ## print(comb$native_spouse[i])
}

comb$native_eng <- 0

for (i in 1:nrow(comb)) {
  if(comb$bpl_2[i] == 150 | comb$bpl_2[i] == 410 | comb$bpl_2[i] == 411 | comb$bpl_2[i] == 412 | comb$bpl_2[i] == 414) {
    comb$native_eng[i] <- 1
  }
  ## print(comb$bpl_2[i])
  ## print(comb$native_eng[i])
}

comb$age_mig <- comb$yrimmig_2 - comb$birthyr_2

## Keep only immigrants with occupational scores for both years
comb <- filter(comb, comb$occscore_1!=0 & comb$occscore_2!=0)

## there are two women in the entire dataset of about 1,100 people
print(count(comb, comb$sex_2==1))

## Clean up first name of parent
comb$namefrst_2 <- gsub( " .*$", "", comb$namefrst_2)
comb$namefrst_2 <- gsub( "?", "", comb$namefrst_2)

## Calculate foreignness index (given that you are a migrant, what is the probability that you would have this name)

comb$parent_index <- 0.00
comb$first_index <- 0.00
comb$last_index <- 0.00

## parent
for(i in 1:nrow(comb)) {
  migrant_name <- tally(raw, raw$namefrst_1 == comb$namefrst_2[i] & raw$bpl_2>99)
  ## print(migrant_name)
  native_name <- tally(raw, raw$namefrst_1 == comb$namefrst_2[i] & raw$bpl_2<100)
  ## print(native_name)
  migrant_count <- tally(raw, raw$bpl_2>99)
  native_count <- tally(raw, raw$bpl_2<100)
  comb$parent_index[i] <- 100*(migrant_name/migrant_count)/((migrant_name/migrant_count) + (native_name/native_count))
  ## print(comb$parent_index[i])
}

## first child
for(i in 1:nrow(comb)) {
  migrant_name <- tally(raw, raw$namefrst_1 == comb$first_child[i] & raw$bpl_2>99)
  ## print(migrant_name)
  native_name <- tally(raw, raw$namefrst_1 == comb$first_child[i] & raw$bpl_2<100)
  ## print(native_name)
  migrant_count <- tally(raw, raw$bpl_2>99)
  native_count <- tally(raw, raw$bpl_2<100)
  comb$first_index[i] <- 100*(migrant_name/migrant_count)/((migrant_name/migrant_count) + (native_name/native_count))
  ## print(comb$first_index[i])
}

## last child
for(i in 1:nrow(comb)) {
  migrant_name <- tally(raw, raw$namefrst_1 == comb$last_child[i] & raw$bpl_2>99)
  ## print(migrant_name)
  native_name <- tally(raw, raw$namefrst_1 == comb$last_child[i] & raw$bpl_2<100)
  ## print(native_name)
  migrant_count <- tally(raw, raw$bpl_2>99)
  native_count <- tally(raw, raw$bpl_2<100)
  comb$last_index[i] <- 100*(migrant_name/migrant_count)/((migrant_name/migrant_count) + (native_name/native_count))
  ## print(comb$last_index[i])
}

## Dependent and Independent Variables
comb$parent_index <- as.numeric(comb$parent_index)
comb$first_index <- as.numeric(comb$first_index)
comb$last_index <- as.numeric(comb$last_index)

comb$change_occ <- comb$occscore_2 - comb$occscore_1
comb$change_parent_last <- comb$last_index - comb$parent_index
comb$change_first_last <- comb$last_index - comb$first_index

comb$change_first_last[comb$change_first_last == 0] <- NaN

## Levels Analysis
## 1880
level_1880 <- lm(comb$occscore_1 ~ comb$last_index + comb$native_spouse + comb$native_eng + comb$age_mig + 
                   comb$last_index*comb$native_spouse + comb$last_index*comb$native_eng + comb$age_mig*comb$last_index)
summary(level_1880)

## 1900
level_1900 <- lm(comb$occscore_2 ~ comb$last_index + comb$native_spouse + comb$native_eng + comb$age_mig + 
                   comb$last_index*comb$native_spouse + comb$last_index*comb$native_eng + comb$age_mig*comb$last_index)
summary(level_1900)

## General Differences Regressions
basic_parent_last <- lm(comb$change_occ ~ comb$change_parent_last)
summary(basic_parent_last)
coeftest(basic_parent_last, vcov = vcovHC(basic_parent_last, type = "HC0"))

parent_last <- lm(comb$change_occ ~ comb$change_parent_last + comb$native_spouse + comb$native_eng + comb$age_mig + 
                    comb$change_parent_last*comb$native_spouse + comb$change_parent_last*comb$native_eng + comb$age_mig*comb$change_parent_last)
summary(parent_last)
coeftest(parent_last, vcov = vcovHC(parent_last, type = "HC0"))
anova(basic_parent_last, parent_last)

basic_first_last <- lm(comb$change_occ ~ comb$change_first_last)
summary(basic_first_last)
coeftest(basic_first_last, vcov = vcovHC(basic_first_last, type = "HC0"))

first_last <- lm(comb$change_occ ~ comb$change_first_last + comb$native_spouse + comb$native_eng + comb$native_eng + comb$age_mig + 
                   comb$change_first_last*comb$native_spouse + comb$change_first_last*comb$native_eng + comb$age_mig*comb$change_first_last)
summary(first_last)
coeftest(first_last, vcov = vcovHC(first_last, type = "HC0"))
anova(basic_first_last, first_last)

## Country-Specific Differences Regression (453 (Germany) - 500, 410 (England) - 154, 150 (Canada) -119)

## Find most common origin countries
common <- count(comb, comb$bpl_1)

## Create datasets
germany <- comb %>% filter(bpl_2==453)
england <- comb %>% filter(bpl_2==410)
canada <- comb %>% filter(bpl_2==150)

## Germany
ger <- lm(germany$change_occ ~ germany$change_parent_last + germany$native_spouse + germany$age_mig + 
            germany$native_spouse*germany$change_parent_last + germany$age_mig*germany$change_parent_last)
summary(ger)

## England
eng <- lm(england$change_occ ~ england$change_parent_last + england$native_spouse + england$age_mig + 
            england$native_spouse*england$change_parent_last + england$change_parent_last*england$age_mig)
summary(eng)

## Canada
can <- lm(canada$change_occ ~ canada$change_parent_last + canada$native_spouse + canada$age_mig + 
            canada$native_spouse*canada$change_parent_last + canada$age_mig*canada$change_parent_last)
summary(can)

## Create the tables
## Levels Analysis
levels <- stargazer(level_1880, level_1900, 
                                       type = "latex", 
                                       title = c("Levels Analysis Using 1880 and 1900 Occupational Data"), 
                                       covariate.labels = c("Last Child's Foreignness Index", "Native Spouse (indicator)", "Native English Speaker (indicator)", "Age Migrated (indicator)", "Native Spouse (interaction)", "Native English Speaker (interaction)", "Age Migrated (interaction)"), 
                                       dep.var.labels = c("1880 Occupational Score", "1900 Occupational Score"), 
                                       omit.stat = c("rsq", "adj.rsq", "ser"))

## exporting the table
sink("levels.tex")
print(levels,type="latex",useViewer=FALSE)
sink()

## General Differences Regression
general <- stargazer(basic_parent_last, parent_last, basic_first_last, first_last, 
                               type = "latex", 
                               title = c("Differences Regression"), 
                               dep.var.labels = "Change in Occupational Score",
                               covariate.labels = c("Between Parent and Last Child", "Between First and Last Child"),
                               omit = c("native_spouse", "native_eng", "age_mig"),
                               omit.stat = c("rsq", "ser", "f"),
                               add.lines = list(c("Native Spouse Controls", "No", "Yes", "No", "Yes"), c("Native English Speaker Controls", "No", "Yes", "No", "Yes"), c("Age Migrated Controls", "No", "Yes", "No", "Yes")))
## exporting the table
sink("general.tex")
print(general,type="latex",useViewer=FALSE)
sink()

## Country Specific Differences Regression
countries <- stargazer(ger, eng, can, 
                     type = "latex", 
                     title = c("Country-Specific Differences Regression"), 
                     dep.var.labels = c("Change in Occupational Score"),
                     column.labels = c("Germany", "England", "Canada"),
                     covariate.labels = c("Between Parent and Last Child"),
                     omit = c("native_spouse", "age_mig"),
                     omit.stat = c("rsq", "ser", "f"),
                     add.lines = list(c("Native Spouse Controls", "Yes", "Yes", "Yes"), c("Age Migrated Controls", "Yes", "Yes", "Yes")))
## exporting the table
sink("countries.tex")
print(general,type="latex",useViewer=FALSE)
sink()

## Export the data
write_dta(comb, "combined.dta")

## Summary Statistics:
## Average age
summary(comb$age_1)
## Average age of migration
summary(comb$age_mig)
## Average number of years US (if possible)
comb$time_us <- comb$age_1 - comb$age_mig
summary(comb$time_us)
## Average median income in 1880
summary(comb$occscore_1)
## Average median income in 1900
summary(comb$occscore_2)

## Close and save log file
sink()