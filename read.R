#' Written by Yosuke Shimizu
#' Short description:
#' 
#' 
#' last update 25/6/20
#' 
#' Environment information
R.version$platform
R.version$version.string

options(install.packages.check.source = "no")

install.packages("pacman") # この行は初回使用時のみ実行（2回目以降は実行不要）

library(pacman)
p_load(renv, readxl, tidyverse, magrittr, broom, openxlsx,
       epitools, DescTools, DescrTab2, ratesci, exact2x2,
       survminer, survival, ggsurvfit)

source("scripts/functions.R")

# read data ---------------------------------------------------------------
#' 使用するデータが格納されているフォルダを示すpathに適宜修正する
#' 基準となるフォルダは.Rprojが存在するフォルダとなる
#' フォルダ上の1個上の階層を表す際は"../"を用いる

# EDCデータ
data_org = read_xlsx('data/test_data.xlsx')

# 採否データ
saihi = read_xlsx("data/saihi_data.xlsx")


# EDCデータと採否データを統合
use_data = merge(data_org, saihi, by = c("ID"))
use_data %<>% mutate(Treatment_Group = factor(Treatment_Group, levels = c("Control", "Treatment")),
                     Gender = factor(Gender, levels = c("Male", "Female")),
                     Smoker = factor(Smoker, levels = c('Yes', 'No')),
                     Severity_scale = factor(Severity_scale, levels = c(3, 4, 5, 6)))
