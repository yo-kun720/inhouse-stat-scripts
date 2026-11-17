
library(pacman)
p_load(
  renv, readxl, tidyverse, magrittr, openxlsx, 
  epitools, DescTools, DescrTab2, ratesci, exact2x2,
  skimr, janitor, gt, gtsummary, 
  survminer, survival, ggsurvfit,
  cards, elrm, mice, lubridate
)

source("scripts/functions.R")


# データの読み込み ----------------------------------------------------------------

practice_data <- read_xlsx("data/practice_data.xlsx")
practice_saihi_data <- read_xlsx("data/practice_saihi_data.xlsx")


# EDCデータと採否データを統合
use_practice_data <- left_join(practice_data, practice_saihi_data)
use_practice_data %<>% 
  mutate(
    Treatment_Group = factor(Treatment_Group, levels = c("Control", "Treatment")),
    Gender = factor(Gender, levels = c("Male", "Female")),
    Smoker = factor(Smoker, levels = c('Yes', 'No')),
    Severity_scale = factor(Severity_scale, levels = c(3, 4, 5, 6))
  )



