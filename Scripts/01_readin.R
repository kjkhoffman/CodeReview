#code for consolidating multiple spreadsheets for NLF data. 
#load in libraries
library(ggplot2)
library(janitor)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)

# #forgot to save this code :( never again - KKH
# cdom <- read.csv("~/GitHubRepos/FinlayLab/Project Code/NLF_NLA_2022/NLFLakesCDOM.csv")
# nlanlf <- read.csv("~/GitHubRepos/FinlayLab/Project Code/NLF_NLA_2022/NLFLakesSamples.csv")
# shim_aug16 <- read.csv("~/GitHubRepos/FinlayLab/Shimadzu Processing and Data/processeddata/Shimadzu_16August2022.csv")
# shim_jul12 <- read.csv("~/GitHubRepos/FinlayLab/Shimadzu Processing and Data/processeddata/Shimadzu_12July2022.csv")
# shim_may1 <- read.csv("~/GitHubRepos/FinlayLab/Shimadzu Processing and Data/processeddata/nlanlf_may1_kkh.csv")
# shim_may4 <- read.csv("~/GitHubRepos/FinlayLab/Shimadzu Processing and Data/processeddata/nlanlf_may4_kkh.csv")
# 
# #need to clean both of these sheets and then combine them####
# #combine
# cdom <- clean_names(cdom)
# nlanlf <- clean_names(nlanlf)
# 
# head(cdom)
# cdom <- subset(cdom, select = c(date_run, site_id, absorbance, cdom_values, rose_cdom_values))
# head(nlanlf)
# nlanlf <-  subset(nlanlf, select = c(sample_id,
#                                      lake_name_state_id, state, county, date_collected,
#                                      analysis_requested, date_acidifed, sample_compromised))
# nlanlfcdom <- nlanlf %>% subset(analysis_requested == "CDOM")
# 
# write.csv(cdom, file="~/GitHubRepos/CodeReview/NLFData/cdom.csv")
# write.csv(nlanlf, file="~/GitHubRepos/CodeReview/NLFData/nlanlf.csv")
# 
# updated <- read.csv("~/GitHubRepos/FinlayLab/Project Code/NLF_NLA_2022/updatedcompiledforJ.csv") #249, 11
# 
# addinfo <- read.csv("~/GitHubRepos/FinlayLab/Project Code/NLF_NLA_2022/NLFaddinfo.csv")
# 
# addinfo <- clean_names(addinfo) #999, 13
# 
# upaddinfo <- updated %>% left_join(addinfo, by = c("sample_id" = "site_id"))#249, 23
# write.csv(upaddinfo, file="~/GitHubRepos/CodeReview/NLFData/upaddinfo.csv")
# 
# rm(addinfo, all3, cdom, nlanlf, nlanlfcdom, updated)
# 
# shim_aug16 <- read.csv("~/GitHubRepos/FinlayLab/Shimadzu Processing and Data/processeddata/Shimadzu_16August2022.csv")
# write.csv(shim_aug16, file="~/GitHubRepos/CodeReview/NLFData/shim_aug16.csv")
# shim_jul12 <- read.csv("~/GitHubRepos/FinlayLab/Shimadzu Processing and Data/processeddata/Shimadzu_12July2022.csv")
# write.csv(shim_jul12, file="~/GitHubRepos/CodeReview/NLFData/shim_jul12.csv")
# shim_may1 <- read.csv("~/GitHubRepos/FinlayLab/Shimadzu Processing and Data/processeddata/nlanlf_may1_kkh.csv")
# write.csv(shim_may1, file="~/GitHubRepos/CodeReview/NLFData/shim_may1.csv")
# shim_may4 <- read.csv("~/GitHubRepos/FinlayLab/Shimadzu Processing and Data/processeddata/nlanlf_may4_kkh.csv")
# write.csv(shim_may4, file="~/GitHubRepos/CodeReview/NLFData/shim_may4.csv")
# phosphorus <- read.csv("~/GitHubRepos/FinlayLab/Project Code/NLF_NLA_2022/NLANLF_TDP_SRP_KKH.csv")
# write.csv(phosphorus, file="~/GitHubRepos/CodeReview/NLFData/phosphorus.csv")

#match column names for join####
cdom <- read.csv("NLFData/cdom.csv")
nlanlf <- read.csv("NLFData/nlanlf.csv")

nlanlfcdom <- nlanlf %>% subset(analysis_requested == "CDOM")

#rename columns appropriately
names(nlanlf)[names(nlanlf)=="date_acidifed"] = "date_acidified"
names(cdom)[names(cdom)=="site_id"] = "sample_id"

all3 <- cdom %>% full_join(nlanlfcdom, by = "sample_id") #278, 12

#write.csv(all3, file="~/GitHubRepos/FinlayLab/Project Code/NLF_NLA_2022/output/combineddata.csv")

upaddinfo <- read.csv("NLFData/upaddinfo.csv")

#add in shimadzu data####
shim_aug16 <- read.csv("NLFData/shim_aug16.csv")
shim_jul12 <- read.csv("NLFData/shim_jul12.csv")
shim_may1 <- read.csv("NLFData/shim_may1.csv")
shim_may4 <- read.csv("NLFData/shim_may4.csv")

shim_jul12$dil_tn = NA
shim_jul12$dil_npoc = NA
shim_jul12$dilution_factor_1 = NA
shim_jul12$dilution = NA
shim_jul12$data = "jul12"
nlf_jul12 <- shim_jul12 %>% subset(sample_name=="nla") %>% subset(select = c("sample_name", "shim_sample_id","data", "npoc", "tn", 
                                                                             "date", "sample_id", "dil_tn", "dil_npoc", 
                                                                              "dilution_factor_1", "dilution")) #14, 11
shim_aug16$dil_tn = NA
shim_aug16$dil_npoc = NA
shim_aug16$dilution_factor_1 = NA
shim_aug16$dilution = NA
shim_aug16$data = "aug16"
nlf_aug16 <- shim_aug16 %>% subset(project=="nlf") %>% rename(shim_sample_id="shim_sample_id.x") %>% 
  subset(select = c("sample_name", "shim_sample_id","data", "npoc", "tn", 
                    "date", "sample_id", "dil_tn", "dil_npoc", "dilution_factor_1", "dilution")) #63, 11

shim_may1$date = NA
shim_may1$data = "aug16"
nlf_may1 <- shim_may1 %>% 
  subset(select = c("sample_name", "shim_sample_id","data", "npoc", "tn", 
                    "date", "sample_id", "dil_tn", "dil_npoc", "dilution_factor_1", "dilution")) #93, 11

shim_may4$date = NA
shim_may4$data = "may4"
nlf_may4 <- shim_may4 %>% 
  subset(select = c("sample_name", "shim_sample_id","data", "npoc", "tn", 
                    "date", "sample_id", "dil_tn", "dil_npoc", "dilution_factor_1", "dilution")) #68, 11

head(nlf_aug16)
head(nlf_jul12)
head(nlf_may1)
head(nlf_may4)

shimadzu <- rbind(nlf_aug16, nlf_jul12, nlf_may1, nlf_may4) #238, 10
head(shimadzu)
head(upaddinfo)

#reformat sample IDs to match! agh
shimadzu$sample_id<-gsub("_", "-", shimadzu$sample_id)

shimadzu_fix <-  shimadzu %>% 
  mutate(sample_id=replace(sample_id,sample_id=='10056265','nlf22-20365')) %>% 
  mutate(sample_id=replace(sample_id,sample_id=='10021205','nlf22-20672')) %>% 
  mutate(sample_id=replace(sample_id,sample_id=='10056271','nlf22-20659'))

shimadzu_fix$sample_id<-gsub("nla-", "nla22-", shimadzu_fix$sample_id)
shimadzu_fix$sample_id<-gsub("nla22-2", "2", shimadzu_fix$sample_id)
shimadzu_fix$sample_id<-gsub("nlf22-2", "2", shimadzu_fix$sample_id)
shimadzu_fix$sample_id<-gsub("nlf-2", "2", shimadzu_fix$sample_id)
shimadzu_fix$sample_id<-gsub("nla22-mn-2", "2", shimadzu_fix$sample_id)
shimadzu_fix$sample_id<-gsub(" dil", "-dil", shimadzu_fix$sample_id)
shimadzu_fix$sample_id<-gsub("10126-wi", "wi-10126", shimadzu_fix$sample_id)
shimadzu_fix$sample_id<-gsub("^20", "nlf22-20", shimadzu_fix$sample_id)
shimadzu_fix$sample_id<-gsub("^10", "nla22-mn-10", shimadzu_fix$sample_id)
shimadzu_fix$sample_id<-gsub(" qc rep", "-qc", shimadzu_fix$sample_id)
rm(shim_aug16, shim_jul12, shim_may1, shim_may4, nlf_aug16, nlf_jul12, nlf_may1, nlf_may4)

shimadzu_fix <- shimadzu_fix %>% mutate_if(is.character, str_to_upper)
#this is as far as I got, then realized I still have duplicates! OH NOOOOOOOO! 
#the shimadzu section below this can be ignored or run. 

unique(shimadzu_fix$sample_id)
shimdups <- shimadzu_fix %>%
  group_by(sample_id, date) %>%
  filter(n()>1) #36, 11

help <- shimdups[order(shimdups$sample_id),]

  #write.csv(upaddinfo, file="~/GitHubRepos/FinlayLab/Project Code/NLF_NLA_2022/upaddinfo_feb8.csv")
  #write.csv(shimadzu_fix, file="~/GitHubRepos/FinlayLab/Project Code/NLF_NLA_2022/NLANLF_DOC_TDN_KKH_20231206.csv")
  
#Start running again here!!!####
#Phosphorus Data####
#may302023
phosphorus <- read.csv("NLFData/phosphorus.csv")

a <- phosphorus %>% clean_names() %>% 
  group_by(sample) %>%
  filter(n()>2)

p_test <- phosphorus %>% clean_names() %>% mutate(x=replace(x,x=='20230531','20230530'))

p_test <-  p_test %>%  mutate(sample=replace(sample,sample=='NLF22-20055-Rep ','NLF22-20055-Rep')) %>% 
  pivot_wider(names_from = analysis, id_cols = c(sample,x),  values_from =c(p_ug_l, abs1, abs2, abs3) )

p_test$p_ug_l_SRP[p_test$p_ug_l_SRP <= 0] <- 0
p_test$p_ug_l_TDP[p_test$p_ug_l_TDP <= 0] <- 0
forview <- p_test %>% subset(select=c(sample, p_ug_l_SRP, p_ug_l_TDP, x))

#fix sample ID names for P####
p_test$x <- as.Date(p_test$x, format="%Y%m%d")
p_fix <- p_test %>% rename(c("date" = "x", "SRP_ugperL" = "p_ug_l_SRP", 
                             "TDP_ugperL"= "p_ug_l_TDP", "sample_id" = "sample"))

p_fix$sample_id<-gsub("nla-", "nla22-", p_fix$sample_id)
p_fix$sample_id<-gsub("nla22-2", "2", p_fix$sample_id)
p_fix$sample_id<-gsub("nlf22-2", "2", p_fix$sample_id)
p_fix$sample_id<-gsub("nlf-2", "2", p_fix$sample_id)
p_fix$sample_id<-gsub("nla22-mn-2", "2", p_fix$sample_id)
p_fix$sample_id<-gsub(" dil", "-dil", p_fix$sample_id)
p_fix$sample_id<-gsub("10126-wi", "wi-10126", p_fix$sample_id)
p_fix$sample_id<-gsub("^20", "nlf22-20", p_fix$sample_id)
p_fix$sample_id<-gsub("^10", "nla22-mn-10", p_fix$sample_id)
p_fix$sample_id<-gsub(" qc rep", "-qc", p_fix$sample_id)

p_fix <- p_fix %>% mutate_if(is.character, str_to_upper)
p_fix <- p_fix %>% mutate_at(c('TDP_ugperL', 'SRP_ugperL', 'abs1_SRP', 'abs2_SRP', 
                               'abs3_SRP','abs1_TDP', 'abs2_TDP', 'abs3_TDP'), as.numeric)

head(p_fix)
str(p_fix)

forview <- p_fix %>% subset(select=c(sample_id, TDP_ugperL, SRP_ugperL, date))

forview2 <- subset(forview, forview$TDP_ugperL < forview$SRP_ugperL) 

#remove standards from data
p_nostandard <- p_fix[!grepl("STAND", p_fix$sample_id),]

#time to join!!!####
#start with the Shimadzu data

head(upaddinfo) #249, 23
fullshim<- full_join(upaddinfo, shimadzu_fix, (by = "sample_id")) #293, 23
antishim <- anti_join(upaddinfo, shimadzu_fix, (by = "sample_id")) #25, 23 #returns all of the entries in one that are not the same as the other
leftshim <- left_join(upaddinfo, shimadzu_fix, by = "sample_id") #280, 23

unique(fullshim$sample_id)
unique(upaddinfo$sample_id)

dups <- upaddinfo %>%
  group_by(sample_id) %>%
  filter(n()>1) #41, 23
#these are ones that have multiple dates collected!

a <- fullshim %>%
  group_by(sample_id) %>%
  filter(n()>1) #95, 33

#keep unique shim sample ids because we know that upaddinfo has been sorted to the best of our abilities. 

fullp<- full_join(upaddinfo, p_fix, (by = "sample_id")) #363, 23
antip <- anti_join(upaddinfo, p_fix, (by = "sample_id")) #201, 23 #returns all of the entries in one that are not the same as the other
leftp <- left_join(upaddinfo, p_fix, by = "sample_id") #280, 23

b <- fullp %>%
  group_by(sample_id) %>%
  filter(n()>1) #127, 32

disaster <- full_join(fullshim, p_fix, (by = "sample_id")) #418, 42
disaster_anti <- anti_join(fullshim, p_fix, (by = "sample_id")) #230, 42
disaster_left <- left_join(fullshim, p_fix, (by="sample_id")) #355, 42
hmm <- unique(disaster) #not duplicates, just actual problems

c <- disaster %>%
  group_by(sample_id) %>%
  filter(n()>1) #191, 32

# 
# write.csv(c, file="~/GitHubRepos/FinlayLab/Project Code/NLF_NLA_2022/problems.csv")
# 