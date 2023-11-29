rm(list = ls())

library(tidyverse)
library(haven)
library(patchwork)
library(AER)
library(stargazer)

# We set the directory, import the dataset and transform it into a data frame:
setwd("") #set the directory in which NEW7080.dta is stored
dataset <- read_dta("NEW7080.dta")
dataset <- as.data.frame(dataset)

################################################################################

# Plots

dataset$COHORT <- as.integer(dataset$COHORT)

## First, we work with the cohort of men born between 1920 and 1929 with data from the 1970 census:

avg_20s <- dataset %>%
  filter(COHORT == 20) %>%
  group_by(YOB, QOB) %>%
  summarize(avg_educ = mean(EDUC, na.rm = TRUE)
            , avg_lwklywge = mean(LWKLYWGE, na.rm = TRUE))%>%
  mutate(YOBQ = YOB + (QOB/4) - 0.25) %>%
  mutate(FQ = 1*(QOB == 1))

### We plot the average years of education for men born in each quarter of each year

geduc_20s <- ggplot(avg_20s, aes(x = YOBQ, y = avg_educ)) +
  geom_point(aes(color = as.factor(FQ), fill = as.factor(FQ)), size = 2.5, shape = 22) +
  geom_line(color = "darkblue") +
  geom_text(aes(label = QOB), vjust = -0.02, nudge_y = 0.02) +
  labs(x = "Year of Birth", y = "Years of Education") +
  scale_x_continuous(breaks = seq(1920, 1929, by = 1)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c('transparent', "black")) +
  theme(legend.position = "none", panel.background = element_blank())

### We plot the average log weakly wage for men born in each quarter of each year

glwage_20s <- ggplot(avg_20s, aes(x = YOBQ, y = avg_lwklywge)) +
  geom_point(aes(color = as.factor(FQ), fill = as.factor(FQ)), size = 2.5, shape = 22) +
  geom_line(color = "darkblue") +
  geom_text(aes(label = QOB), vjust = -0.001, nudge_y = 0.0015) +
  labs(x = "Year of Birth", y = "Log Weekly Earnings") +
  scale_x_continuous(breaks = seq(1920, 1929, by = 1)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c('transparent', "black")) +
  theme(legend.position = "none", panel.background = element_blank())

#------------------------------------------------------------------------------#
## Second, we work with the cohort of men born between 1930 and 1939 with data from the 1980 census:

avg_30s <- dataset %>%
  filter(COHORT == 30) %>%
  group_by(YOB, QOB) %>%
  summarize(avg_educ = mean(EDUC, na.rm = TRUE)
            , avg_lwklywge = mean(LWKLYWGE, na.rm = TRUE))%>%
  mutate(YOBQ = YOB + (QOB/4) - 0.25) %>%
  mutate(FQ = 1*(QOB == 1))

### We plot the average years of education for men born in each quarter of each year

geduc_30s <- ggplot(avg_30s, aes(x = YOBQ, y = avg_educ)) +
  geom_point(aes(color = as.factor(FQ), fill = as.factor(FQ)), size = 2.5, shape = 22) +
  geom_line(color = "darkblue") +
  geom_text(aes(label = QOB), vjust = -0.02, nudge_y = 0.02) +
  labs(x = "Year of Birth", y = "Years of Education") +
  scale_x_continuous(breaks = seq(30, 39, by = 1)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c('transparent', "black")) +
  theme(legend.position = "none", panel.background = element_blank())

### We plot the average log weakly wage for men born in each quarter of each year

glwage_30s <- ggplot(avg_30s, aes(x = YOBQ, y = avg_lwklywge)) +
  geom_point(aes(color = as.factor(FQ), fill = as.factor(FQ)), size = 2.5, shape = 22) +
  geom_line(color = "darkblue") +
  geom_text(aes(label = QOB), vjust = -0.001, nudge_y = 0.0015) +
  labs(x = "Year of Birth", y = "Log Weekly Earnings") +
  scale_x_continuous(breaks = seq(30, 39, by = 1)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c('transparent', "black")) +
  theme(legend.position = "none", panel.background = element_blank())

#------------------------------------------------------------------------------#
## And third, we work with the cohort of men born between 1940 and 1949 with data from the 1980 census:

avg_40s <- dataset %>%
  filter(COHORT == 40) %>%
  group_by(YOB, QOB) %>%
  summarize(avg_educ = mean(EDUC, na.rm = TRUE)
            , avg_lwklywge = mean(LWKLYWGE, na.rm = TRUE))%>%
  mutate(YOBQ = YOB + (QOB/4) - 0.25) %>%
  mutate(FQ = 1*(QOB == 1))

### We plot the average years of education for men born in each quarter of each year

geduc_40s <- ggplot(avg_40s, aes(x = YOBQ, y = avg_educ)) +
  geom_point(aes(color = as.factor(FQ), fill = as.factor(FQ)), size = 2.5, shape = 22) +
  geom_line(color = "darkblue") +
  geom_text(aes(label = QOB), vjust = -0.02, nudge_y = 0.02) +
  labs(x = "Year of Birth", y = "Years of Education") +
  scale_x_continuous(breaks = seq(40, 49, by = 1)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c('transparent', "black")) +
  theme(legend.position = "none", panel.background = element_blank())

### We plot the average log weakly wage for men born in each quarter of each year

glwage_40s <- ggplot(avg_40s, aes(x = YOBQ, y = avg_lwklywge)) +
  geom_point(aes(color = as.factor(FQ), fill = as.factor(FQ)), size = 2.5, shape = 22) +
  geom_line(color = "darkblue") +
  geom_text(aes(label = QOB), vjust = -0.008, nudge_y = 0.008) +
  labs(x = "Year of Birth", y = "Log Weekly Earnings") +
  scale_x_continuous(breaks = seq(40, 49, by = 1)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c('transparent', "black")) +
  theme(legend.position = "none", panel.background = element_blank())

#------------------------------------------------------------------------------#
## We print the three years of education vs quarter and year of birth graphs together:

geduc_20s + geduc_30s + geduc_40s +
  plot_layout(ncol = 1, widths = c(1, 1, 1))

## We print the three log of weakly earnings vs quarter and year of birth graphs together:

glwage_20s + glwage_30s + glwage_40s +
  plot_layout(ncol = 1)


################################################################################

# Estimation

## First, we do the estimation for the cohort of men born between 1920 and 1929 (data from the 1970 Census):

data_20s <- dataset %>%
  filter(COHORT == 20)

ols_20s <- lm(LWKLYWGE ~ EDUC + RACE + MARRIED + SMSA + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                MT + AGEQ + AGEQSQ, data = data_20s)

summary(ols_20s)

tsls_20s <- ivreg(LWKLYWGE ~ EDUC + YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                    RACE + MARRIED + SMSA + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                    MT + AGEQ + AGEQSQ | 
                    QTR120 + QTR121 + QTR122 + QTR123 + QTR124 + QTR125 + QTR126 + QTR127 + QTR128 + QTR129 +
                    QTR220 + QTR221 + QTR222 + QTR223 + QTR224 + QTR225 + QTR226 + QTR227 + QTR228 + QTR229 +
                    QTR320 + QTR321 + QTR322 + QTR323 + QTR324 + QTR325 + QTR326 + QTR327 + QTR328 + QTR329 +
                    YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                    RACE + MARRIED + SMSA + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                    MT + AGEQ + AGEQSQ,
                  data = data_20s)
summary(tsls_20s)

#------------------------------------------------------------------------------#

## Second, we do the estimation for for the cohort of men born between 1930 and 1939 (data from the 1980 Census):

data_30s <- dataset %>%
  filter(COHORT == 30)

ols_30s <- lm(LWKLYWGE ~ EDUC + RACE + MARRIED + SMSA + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                MT + AGEQ + AGEQSQ, data = data_30s)

summary(ols_30s)

tsls_30s <- ivreg(LWKLYWGE ~ EDUC + YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                    RACE + MARRIED + SMSA + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                    MT + AGEQ + AGEQSQ | 
                    QTR120 + QTR121 + QTR122 + QTR123 + QTR124 + QTR125 + QTR126 + QTR127 + QTR128 + QTR129 +
                    QTR220 + QTR221 + QTR222 + QTR223 + QTR224 + QTR225 + QTR226 + QTR227 + QTR228 + QTR229 +
                    QTR320 + QTR321 + QTR322 + QTR323 + QTR324 + QTR325 + QTR326 + QTR327 + QTR328 + QTR329 +
                    YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                    RACE + MARRIED + SMSA + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                    MT + AGEQ + AGEQSQ,
                  data = data_30s)
summary(tsls_30s)

#------------------------------------------------------------------------------#

## Third, we do the estimation for for the cohort of men born between 1940 and 1949 (data from the 1980 Census):

data_40s <- dataset %>%
  filter(COHORT == 40)

ols_40s <- lm(LWKLYWGE ~ EDUC + RACE + MARRIED + SMSA + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                MT + AGEQ + AGEQSQ, data = data_40s)

summary(ols_40s)

tsls_40s <- ivreg(LWKLYWGE ~ EDUC + YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                    RACE + MARRIED + SMSA + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                    MT + AGEQ + AGEQSQ | 
                    QTR120 + QTR121 + QTR122 + QTR123 + QTR124 + QTR125 + QTR126 + QTR127 + QTR128 + QTR129 +
                    QTR220 + QTR221 + QTR222 + QTR223 + QTR224 + QTR225 + QTR226 + QTR227 + QTR228 + QTR229 +
                    QTR320 + QTR321 + QTR322 + QTR323 + QTR324 + QTR325 + QTR326 + QTR327 + QTR328 + QTR329 +
                    YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                    RACE + MARRIED + SMSA + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                    MT + AGEQ + AGEQSQ,
                  data = data_40s)
summary(tsls_40s)

#------------------------------------------------------------------------------#

## We print the results of the three TSLS estimations in one table:

tsls_list <- list(tsls_20s, tsls_30s, tsls_40s)

stargazer(tsls_list, title = "TSLS estimates", align = TRUE
          , omit.stat = c("f", "ser")
          , omit.table.layout = "n")

## We print the results of the three TSLS estimations in one table:

ols_list <- list(ols_20s, ols_30s, ols_40s)

stargazer(ols_list, title = "OLS estimates", align = TRUE
          , omit.stat = c("f", "ser")
          , omit.table.layout = "n")

### Note: this tables we're later edited in LaTex. Available upon request.


################################################################################

# Extension

## We estimate for al three cohort loking only at men living in rural areas.

data_20s_smsa0 <- data_20s %>%
  filter(SMSA == 0)

tsls_20s_smsa0 <- ivreg(LWKLYWGE ~ EDUC + YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                          RACE + MARRIED + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                          MT + AGEQ + AGEQSQ | 
                          QTR120 + QTR121 + QTR122 + QTR123 + QTR124 + QTR125 + QTR126 + QTR127 + QTR128 + QTR129 +
                          QTR220 + QTR221 + QTR222 + QTR223 + QTR224 + QTR225 + QTR226 + QTR227 + QTR228 + QTR229 +
                          QTR320 + QTR321 + QTR322 + QTR323 + QTR324 + QTR325 + QTR326 + QTR327 + QTR328 + QTR329 +
                          YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                          RACE + MARRIED + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                          MT + AGEQ + AGEQSQ,
                        data = data_20s_smsa0)
summary(tsls_20s_smsa0)


data_30s_smsa0 <- data_30s %>%
  filter(SMSA == 0)

tsls_30s_smsa0 <- ivreg(LWKLYWGE ~ EDUC + YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                          RACE + MARRIED + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                          MT + AGEQ + AGEQSQ | 
                          QTR120 + QTR121 + QTR122 + QTR123 + QTR124 + QTR125 + QTR126 + QTR127 + QTR128 + QTR129 +
                          QTR220 + QTR221 + QTR222 + QTR223 + QTR224 + QTR225 + QTR226 + QTR227 + QTR228 + QTR229 +
                          QTR320 + QTR321 + QTR322 + QTR323 + QTR324 + QTR325 + QTR326 + QTR327 + QTR328 + QTR329 +
                          YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                          RACE + MARRIED + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                          MT + AGEQ + AGEQSQ,
                        data = data_30s_smsa0)
summary(tsls_30s_smsa0)


data_40s_smsa0 <- data_40s %>%
  filter(SMSA == 0)

tsls_40s_smsa0 <- ivreg(LWKLYWGE ~ EDUC + YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                          RACE + MARRIED + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                          MT + AGEQ + AGEQSQ | 
                          QTR120 + QTR121 + QTR122 + QTR123 + QTR124 + QTR125 + QTR126 + QTR127 + QTR128 + QTR129 +
                          QTR220 + QTR221 + QTR222 + QTR223 + QTR224 + QTR225 + QTR226 + QTR227 + QTR228 + QTR229 +
                          QTR320 + QTR321 + QTR322 + QTR323 + QTR324 + QTR325 + QTR326 + QTR327 + QTR328 + QTR329 +
                          YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 +
                          RACE + MARRIED + NEWENG + MIDATL + ENOCENT + WNOCENT + SOATL + ESOCENT + WSOCENT +
                          MT + AGEQ + AGEQSQ,
                        data = data_40s_smsa0)
summary(tsls_40s_smsa0)

## We print the results in one table

tsls_list_smsa0 <- list(tsls_20s_smsa0, tsls_30s_smsa0, tsls_40s_smsa0)

stargazer(tsls_list_smsa0, title = "TSLS estimates for men living in rural areas", align = TRUE
          , omit.stat = c("f", "ser")
          , omit.table.layout = "n")
