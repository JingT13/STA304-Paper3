#### Preamble ####
# Purpose: Clean the survey data downloaded from CHASS
# Author: Jingxian Zhai, Yingxuan Shi, Wantong Qiu
# Data: 20 March 2022
# Contact: jingxian.zhai@mail.utoronto.ca

#### Workspace setup ####
# Use R Projects, not setwd().
library(tidyverse)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(janitor)
library(fmsb)
library(devtools)
# Read in the raw data. 
rawdata <- read.csv("p3data.csv")

names(rawdata)

#Then, we selected all variables that we will use in analysis from the raw data, they are related to the number of divorced people, personal financial situation, child(ren) in family and personal education level. 
#Also, we remove the useless answers such as "skip", "don't know", etc. 

my_data1 <- as_tibble(raw_data, skip = 96) %>% 
  filter(cc_30_1<="6" & sep5yr<="2")
data_test <- my_data1 %>% select(cc_30_1, sep5yr)
data_test1 <- data_test %>% filter(cc_30_1 == 6) 
data_test2<-data_test1 %>% filter(sep5yr == 1)


my_data2 <- as_tibble(raw_data) %>% 
  filter(totccar<="2" & sep5yr<="2")
data_test_0 <- my_data2 %>% select(totccar, sep5yr)
data_test3 <- data_test_0 %>% filter(totccar == 1) 
data_test4<-data_test3 %>% filter(sep5yr == 2)


data0 <- my_data1 %>% 
  select(cc_30_1, sts_420, cc_40_1, sts_410, cc_10_1, sep20yr, sep5yr, ccw2c1c) 


###Moreover, we demonstrate the part of mutating data. We create new dataset for preparing the plots and tables.###

rawdata0 <- rawdata %>% 
  mutate(divorced = case_when(rawdata$sep5yr == "1" ~ "Divorced",
                                                    rawdata$sep5yr == "2" ~ "Married"))

rawdata1 <- rawdata %>% 
  mutate(Gender = case_when(rawdata$sex == "1" ~ "Male", rawdata$sex == "2" ~ "Female")) %>% mutate(age = case_when(rawdata$agegr10 == "1" ~ 15,
                                                                                                                                          rawdata$agegr10 == "2" ~ 25,
                                                                                                                                          rawdata$agegr10 == "3" ~ 35,
                                                                                                                                          rawdata$agegr10 == "4" ~ 45,
                                                                                                                                          rawdata$agegr10 == "5" ~ 55,
                                                                                                                                          rawdata$agegr10 == "6" ~ 65,
                                                                                                                                          rawdata$agegr10 == "7" ~ 75)) %>% mutate(num_divorced = case_when(
                                                                                                                                            age == 15&Gender == "Male" ~ 771,
                                                                                                                                            age == 15&Gender == "Female" ~ 765,
                                                                                                                                            age == 25&Gender == "Male" ~ 1312,
                                                                                                                                            age == 25&Gender == "Female" ~ 1498,
                                                                                                                                            age == 35&Gender == "Male" ~ 1506,
                                                                                                                                            age == 35&Gender == "Female" ~ 1689,
                                                                                                                                            age == 45&Gender == "Male" ~ 1432,
                                                                                                                                            age == 45&Gender == "Female" ~ 1635,
                                                                                                                                            age == 55&Gender == "Male" ~ 1872,
                                                                                                                                            age == 55&Gender == "Female" ~ 2238,
                                                                                                                                            age == 65&Gender == "Male" ~ 1595,
                                                                                                                                            age == 65&Gender == "Female" ~ 2017,
                                                                                                                                            age == 75&Gender == "Male" ~ 911,
                                                                                                                                            age == 75&Gender == "Female" ~ 1361,
                                                                                                                                            
                                                                                                                                          )) 

Job <- rawdata %>% 
  mutate(work_type = case_when(rawdata$rto_101 == "1" ~ "Full time",
                                                rawdata$rto_101 == "2" ~ "Part time")) %>% na.omit(work_type)

work0 <- rawdata %>% 
  mutate(Gender = case_when(rawdata$sex == "1" ~ "Male", rawdata$sex == "2" ~ "Female")) %>% mutate(age = case_when(rawdata$agegr10 == "1" ~ 15,
                                                                                                                                       rawdata$agegr10 == "2" ~ 25,
                                                                                                                                       rawdata$agegr10 == "3" ~ 35,
                                                                                                                                       rawdata$agegr10 == "4" ~ 45,
                                                                                                                                       rawdata$agegr10 == "5" ~ 55,
                                                                                                                                       rawdata$agegr10 == "6" ~ 65,
                                                                                                                                       rawdata$agegr10 == "7" ~ 75)) %>% mutate(num_divorced = case_when(
                                                                                                                                         age == 15&Gender == "Male" ~ 38,
                                                                                                                                         age == 15&Gender == "Female" ~ 55,
                                                                                                                                         age == 25&Gender == "Male" ~ 171,
                                                                                                                                         age == 25&Gender == "Female" ~ 237,
                                                                                                                                         age == 35&Gender == "Male" ~ 150,
                                                                                                                                         age == 35&Gender == "Female" ~ 195,
                                                                                                                                         age == 45&Gender == "Male" ~ 120,
                                                                                                                                         age == 45&Gender == "Female" ~ 135,
                                                                                                                                         age == 55&Gender == "Male" ~ 82,
                                                                                                                                         age == 55&Gender == "Female" ~ 95,
                                                                                                                                         age == 65&Gender == "Male" ~ 49,
                                                                                                                                         age == 65&Gender == "Female" ~ 32,
                                                                                                                                         age == 75&Gender == "Male" ~ 15,
                                                                                                                                         age == 75&Gender == "Female" ~ 8,
                                                                                                                                         
                                                                                                                                       )) %>% na.omit(rawdata$age) %>% filter(rawdata$sep5yr == 1) 
work1 <- rawdata %>% mutate(money = case_when(
  rawdata$ttlincg2 == "1" ~  "$25000",
  rawdata$ttlincg2 == "2" ~  "$25000 to $49999",
  rawdata$ttlincg2 == "3" ~  "$50000 to $74999",
  rawdata$ttlincg2 == "4" ~  "$75000 to $99999",
  rawdata$ttlincg2 == "5" ~  "more than $100000")) %>% na.omit(rawdata$money)

radarchart_data <- data.frame(Type1=c(60, 0, 14),
                              Type2=c(60, 0, 20),
                              Type3=c(60, 0, 2),
                              Type4=c(60, 0, 31),
                              Type5=c(60, 0, 50),
                              Type6=c(60, 0, 17))

gss <- rawdata %>% 
  select(sep5yr, 
         ehg3_01b)

gss2 <- gss %>% mutate(education_level = case_when(
  gss$ehg3_01b == "1" ~  "Blow high school",
  gss$ehg3_01b == "2" ~  "High school ",
  gss$ehg3_01b == "3" ~  "Trade certificate",
  gss$ehg3_01b == "4" ~  "College",
  gss$ehg3_01b == "5" ~  "University certificate",
  gss$ehg3_01b == "6" ~ "Bachelor’s degree(B.A.)",
  gss$ehg3_01b == "7" ~ "Above bachelor degree"))%>%na.omit(education_level)%>%filter(sep5yr =="1")
         