# Table1 ベースライン時点における人口統計学的及び他の背景情報の要約 ------------------------------------

# ベースライン時のデータ
baseline_analysis_data_T01 <- use_practice_data %>% 
  filter(
    fas == "採用",
    Time_point == "Baseline"
  )

summary_items <- c('n', 'mean_SD', 'min_max', 'median_IQR')

results_T01 <- NULL

# 年齢
results_T01 <- tab1_continuous(
  dat = baseline_analysis_data_T01, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  var = "Age", 
  item_name = "年齢", 
  res = results_T01, 
  summary_stat = summary_items,
  digits = 1
)

# 性別
results_T01 <- tab1_discrete(
  dat = baseline_analysis_data_T01,
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  var = "Gender", 
  item_name = "性別", 
  labels = c("Male", "Female"),
  res = results_T01
)

# 喫煙
results_T01 <- tab1_discrete(
  dat = baseline_analysis_data_T01, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"), 
  var = "Smoker", 
  item_name = "喫煙", 
  labels = c('Yes', 'No'), 
  res = results_T01
)

# BMI
results_T01 <- tab1_continuous(
  dat = baseline_analysis_data_T01, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"), 
  var = "BMI", 
  item_name = "BMI", 
  res = results_T01, 
  summary_stat = summary_items
)

# 重症度スコア
results_T01 <- tab1_discrete(
  dat = baseline_analysis_data_T01, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"), 
  var = "Severity_scale", 
  item_name = "重症度スコア", 
  labels = c('3', '4', '5', '6'), 
  res = results_T01
)

export_table(results_T01, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T01", fig_name = NULL, add_table = FALSE, rowName = TRUE)


# Table2 主要評価項目の主解析 主要評価項目の達成割合 -------------------------------------------

primary_analysis_data_T02 <- use_practice_data %>%
  filter(
    fas == '採用', 
    Time_point == 'Followup_1'
  )

results_T02 <- binary_analysis(
  dat = primary_analysis_data_T02, 
  group_var = "Treatment_Group",
  group_labels = c("Control", "Treatment"),
  bin_var = 'Death_till_30',
  subjid_var = 'ID',
  CI_level = 0.95, 
  CI_sides = 'two.sided',
  Diff_CI_method = "score",
  Ratio_CI_method = "katz.log",
  test_method = "Chisq",
  test_sides = "two.sided",
  delta = 0
)

export_table(results_T02, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T02", fig_name = NULL, add_table = TRUE, rowName = TRUE)



# Table3 主要評価項目の副次解析 ロジスティック回帰モデルを用いた解析結果（Day 30までの死亡をイベント） ---------------

logistic_analysis_data_T03 <- use_practice_data %>%
  filter(fas == '採用', Time_point == 'Baseline') %>% 
  select(ID, Date_of_enroll, Age, Gender, Smoker, BMI, Severity_scale) %>% 
  mutate( # 性別と喫煙歴のrefを変更
    Gender = factor(Gender, levels = c("Female", "Male")),
    Smoker = factor(Smoker, levels = c("No", "Yes"))
  ) %>% 
  left_join(
    use_practice_data %>%
      filter(fas == '採用', Time_point == 'Followup_1') %>% 
      select(ID, Death_till_30)
  ) %>% 
  left_join(
    use_practice_data %>%
      filter(fas == '採用', Time_point == 'Followup_3') %>% 
      select(ID, Death_till_60)
  ) %>% 
  left_join(
    use_practice_data %>%
      filter(fas == '採用', Time_point == 'Followup_5') %>% 
      select(ID, Death_till_90)
  ) %>% 
  left_join(
    use_practice_data %>%
      filter(fas == '採用', Time_point == 'End_of_Study') %>% 
      select(ID, Treatment_Group, Observed_Time, Event, End_date)
  ) 



# gtsummaryカスタマイズ関数（ロジスティック回帰）

tbl_logistic_custom <- function(analysis_data){
  
  # gtsummaryの出力の表示をモックアップの形式に合わせる
  tbl_custom <- analysis_data %>% 
    as_tibble() %>% 
    # ラベルの修正
    mutate(
      Variable = if_else(
        `**Characteristic**` %in% c("Treatment_Group", "Age", "Gender", "Smoker", "BMI", "Severity_scale"), 
        `**Characteristic**`, NA_character_
      )
    ) %>% 
    fill("Variable", .direction = "down") %>%
    filter((Variable != `**Characteristic**`) | (!is.na(`**p-value**`))) %>% # p-valueの列名は何でもよい
    group_by(Variable) %>%
    mutate(
      Variable = if_else(row_number() == 1, Variable, ""),
      `OR (95%CI)` = if_else( # OR, 95%CI一つの列に統合
        is.na(`**95% CI**`), as.character(`**OR**`), paste0(`**OR**`, " (", `**95% CI**`, ")")
      ),
      `OR (95%CI)` = if_else(row_number() == 1 & is.na(`OR (95%CI)`), "ref", `OR (95%CI)`),
      across(c("OR (95%CI)", "**p-value**"),
             ~ if_else(row_number() == 1 & is.na(.x), "", .x))
    ) %>% 
    ungroup() %>% 
    select(Variable, `**Characteristic**`, `OR (95%CI)`, `**p-value**`) # 並び替え
  
  return(tbl_custom)
  
}


# ロジスティック回帰
results_T03 <- glm(
  Death_till_30 ~ Treatment_Group +
    Age + Gender + Smoker + BMI + Severity_scale,
  family = binomial(link = "logit"),
  data = logistic_analysis_data_T03
) %>% 
  tbl_regression(
    ., exponentiate = TRUE,
    estimate_fun =  ~style_number(.x, digits = 2),
    pvalue_fun =  ~style_pvalue(.x, digits = 3)
  ) %>% 
  tbl_logistic_custom(.)

export_table(results_T03, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T03", fig_name = NULL, add_table = TRUE, rowName = TRUE)



# Table4 副次評価項目の解析 生存時間解析（生存時間中央値と介入に関するハザード比） ----------------------------

survival_analysis_data_T04 <- use_practice_data %>%
  filter(fas == '採用', Time_point == 'Baseline') %>% 
  select(ID, Date_of_enroll, Age, Gender, Smoker, BMI, Severity_scale) %>% 
  mutate( # 性別と喫煙歴のrefを変更
    Gender = factor(Gender, levels = c("Female", "Male")),
    Smoker = factor(Smoker, levels = c("No", "Yes"))
  ) %>% 
  left_join(
    use_practice_data %>%
      filter(fas == '採用', Time_point == 'Followup_1') %>% 
      select(ID, Death_till_30)
  ) %>% 
  left_join(
    use_practice_data %>%
      filter(fas == '採用', Time_point == 'Followup_3') %>% 
      select(ID, Death_till_60)
  ) %>% 
  left_join(
    use_practice_data %>%
      filter(fas == '採用', Time_point == 'Followup_5') %>% 
      select(ID, Death_till_90)
  ) %>% 
  left_join(
    use_practice_data %>%
      filter(fas == '採用', Time_point == 'End_of_Study') %>% 
      select(ID, Treatment_Group, Observed_Time, Event, End_date)
  ) 


results_T04 <- survival_analysis(
  dat = survival_analysis_data_T04, 
  group_var = "Treatment_Group",
  group_labels = c("Control", "Treatment"),
  event_var = 'Death_till_30',
  time_var = "Observed_Time",
  max_obs_time = 30,
  HR_CI_level = 0.95
)

export_table(results_T04, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T04", fig_name = NULL, add_table = TRUE, rowName = TRUE)



# Figure1 副次評価項目の解析 Day XXまでの期間におけるKM曲線 -----------------------------------

KM = KM_curve(
  dat = survival_analysis_data_T04, 
  group_var = "Treatment_Group",
  group_labels = c("Control", "Treatment"),
  event_var = 'Death_till_30',
  time_var = "Observed_Time",
  max_obs_time = 30,
  KM_x_label = 'Days to death until Day 30',
  KM_x_axis_ticks = c(0, 10, 20, 30),
  RT_event_label = "Number of death"
)

ggsave(file = 'outputs/Figure_KM.jpeg', plot = KM, device = "jpeg", 
       dpi = 600, width = 4/3*4.8*2, height = 4.8*2)


# Table5 副次評価項目の解析 Cox比例ハザードモデルを用いた解析結果（Day 30までの死亡をイベント） -----------------

survival_analysis_data_T05 <- survival_analysis_data_T04


# gtsummaryカスタマイズ関数（Cox回帰）
tbl_cox_custom <- function(analysis_data){
  
  # gtsummaryの出力の表示をモックアップの形式に合わせる
  tbl_custom <- analysis_data %>% 
    as_tibble() %>% 
    # ラベルの修正
    mutate(
      Variable = if_else(
        `**Characteristic**` %in% c("Treatment_Group", "Age", "Gender", "Smoker", "BMI", "Severity_scale"), 
        `**Characteristic**`, NA_character_
      )
    ) %>% 
    fill("Variable", .direction = "down") %>%
    filter((Variable != `**Characteristic**`) | (!is.na(`**p-value**`))) %>% # p-valueの列名は何でもよい
    group_by(Variable) %>%
    mutate(
      Variable = if_else(row_number() == 1, Variable, ""),
      `HR (95%CI)` = if_else( # HR, 95%CI一つの列に統合
        is.na(`**95% CI**`), as.character(`**HR**`), paste0(`**HR**`, " (", `**95% CI**`, ")")
      ),
      `HR (95%CI)` = if_else(row_number() == 1 & is.na(`HR (95%CI)`), "ref", `HR (95%CI)`),
      across(c("HR (95%CI)", "**p-value**"),
             ~ if_else(row_number() == 1 & is.na(.x), "", .x))
    ) %>% 
    ungroup() %>% 
    select(Variable, `**Characteristic**`, `HR (95%CI)`, `**p-value**`) # 並び替え
  
  return(tbl_custom)
  
}

# Cox回帰
results_T05 <- coxph(
  Surv(time = Observed_Time, event = Death_till_30) ~ Treatment_Group +
  Age + Gender + Smoker + BMI + Severity_scale,
  data = survival_analysis_data_T05
) %>% 
  tbl_regression(
    ., exponentiate = TRUE,
    estimate_fun =  ~style_number(.x, digits = 2),
    pvalue_fun =  ~style_pvalue(.x, digits = 3)
  ) %>% 
  tbl_cox_custom(.)

export_table(results_T05, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T05", fig_name = NULL, add_table = TRUE, rowName = TRUE)


# 安全性解析対象集団 ---------------------------------------------------------------

safety_analysis_data <- use_practice_data %>% 
  filter(sas == '採用')

# Table21 安全性の解析 有害事象の発現割合 ------------------------------------------------

results_T21 <-  AE_all_item(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_cate_vars = c("Causal", "Seriousness", "Severity"), 
  AE_cate = list(
    c('否定できない', '否定できる'),
    c('重篤', '非重篤'),
    c('G1', 'G2', 'G3', 'G4', 'G5')
  ), 
  res = results_T21, 
  subjid_var = 'ID'
)

export_table(results_T21, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T21", fig_name = NULL, add_table = TRUE, rowName = TRUE)



# Table22 安全性の解析 各有害事象の発現割合 -------------------------------------------------------

# 有害事象の種類を確認
safety_analysis_data %>% distinct(AE_Term)

with_95CI <- FALSE # AE発生割合のCI有無

results_T22 <- NULL

# Nausea 吐気
results_T22 <- AE_each_item(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Nausea', 
  AE_label = '吐気',
  res = results_T22, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Vomiting 嘔吐
results_T22 <- AE_each_item(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Vomiting', 
  AE_label = '嘔吐',
  res = results_T22, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Diarrhea 下痢
results_T22 <- AE_each_item(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Diarrhea', 
  AE_label = '下痢',
  res = results_T22, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Headache 頭痛
results_T22 <- AE_each_item(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Headache', 
  AE_label = '頭痛',
  res = results_T22, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Dizziness めまい
results_T22 <- AE_each_item(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Dizziness', 
  AE_label = 'めまい',
  res = results_T22, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Fatigue 疲労
results_T22 <- AE_each_item(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Fatigue', 
  AE_label = '疲労',
  res = results_T22, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Fever 発熱
results_T22 <- AE_each_item(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Fever', 
  AE_label = '発熱',
  res = results_T22, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Anemia 貧血
results_T22 <- AE_each_item(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Anemia', 
  AE_label = '貧血',
  res = results_T22, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Hypertension 高血圧
results_T22 <- AE_each_item(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Hypertension', 
  AE_label = '高血圧',
  res = results_T22, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Rash 発疹
results_T22 <- AE_each_item(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Rash', 
  AE_label = '発疹',
  res = results_T22, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

export_table(results_T22, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T22", fig_name = NULL, add_table = TRUE, rowName = TRUE)



# Table23 安全性の解析 各有害事象の発現割合（因果関係別） ----------------------------------------

results_T23 <- NULL

# Nausea 吐気
results_T23 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Nausea', 
  AE_label = '吐気',
  AE_cate_vars = c("Causal"), 
                           AE_cate = list(c('否定できない', '否定できる')), 
  res = results_T23, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Vomiting 嘔吐
results_T23 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Vomiting', 
  AE_label = '嘔吐',
  AE_cate_vars = c("Causal"), 
  AE_cate = list(c('否定できない', '否定できる')), 
  res = results_T23, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Diarrhea 下痢
results_T23 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Diarrhea', 
  AE_label = '下痢',
  AE_cate_vars = c("Causal"), 
  AE_cate = list(c('否定できない', '否定できる')), 
  res = results_T23, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Headache 頭痛
results_T23 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Headache', 
  AE_label = '頭痛',
  AE_cate_vars = c("Causal"), 
  AE_cate = list(c('否定できない', '否定できる')), 
  res = results_T23, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Dizziness めまい
results_T23 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Dizziness', 
  AE_label = 'めまい',
  AE_cate_vars = c("Causal"), 
  AE_cate = list(c('否定できない', '否定できる')), 
  res = results_T23, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Fatigue 疲労
results_T23 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Fatigue', 
  AE_label = '疲労',
  AE_cate_vars = c("Causal"), 
  AE_cate = list(c('否定できない', '否定できる')), 
  res = results_T23, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Fever 発熱
results_T23 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Fever', 
  AE_label = '発熱',
  AE_cate_vars = c("Causal"), 
  AE_cate = list(c('否定できない', '否定できる')), 
  res = results_T23, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Anemia 貧血
results_T23 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Anemia', 
  AE_label = '貧血',
  AE_cate_vars = c("Causal"), 
  AE_cate = list(c('否定できない', '否定できる')), 
  res = results_T23, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Hypertension 高血圧
results_T23 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Hypertension', 
  AE_label = '高血圧',
  AE_cate_vars = c("Causal"), 
  AE_cate = list(c('否定できない', '否定できる')), 
  res = results_T23, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Rash 発疹
results_T23 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Rash', 
  AE_label = '発疹',
  AE_cate_vars = c("Causal"), 
  AE_cate = list(c('否定できない', '否定できる')), 
  res = results_T23, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)


export_table(results_T23, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T23", fig_name = NULL, add_table = TRUE, rowName = TRUE)



# Table24 安全性の解析 各有害事象の発現割合（重篤性別） -----------------------------------------

results_T24 <- NULL

# Nausea 吐気
results_T24 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Nausea', 
  AE_label = '吐気',
  AE_cate_vars = c("Seriousness"), 
  AE_cate = list(c('重篤', '非重篤')), 
  res = results_T24, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Vomiting 嘔吐
results_T24 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Vomiting', 
  AE_label = '嘔吐',
  AE_cate_vars = c("Seriousness"), 
  AE_cate = list(c('重篤', '非重篤')), 
  res = results_T24, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Diarrhea 下痢
results_T24 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Diarrhea', 
  AE_label = '下痢',
  AE_cate_vars = c("Seriousness"), 
  AE_cate = list(c('重篤', '非重篤')), 
  res = results_T24, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Headache 頭痛
results_T24 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Headache', 
  AE_label = '頭痛',
  AE_cate_vars = c("Seriousness"), 
  AE_cate = list(c('重篤', '非重篤')), 
  res = results_T24, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Dizziness めまい
results_T24 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Dizziness', 
  AE_label = 'めまい',
  AE_cate_vars = c("Seriousness"), 
  AE_cate = list(c('重篤', '非重篤')), 
  res = results_T24, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Fatigue 疲労
results_T24 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Fatigue', 
  AE_label = '疲労',
  AE_cate_vars = c("Seriousness"), 
  AE_cate = list(c('重篤', '非重篤')), 
  res = results_T24, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Fever 発熱
results_T24 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Fever', 
  AE_label = '発熱',
  AE_cate_vars = c("Seriousness"), 
  AE_cate = list(c('重篤', '非重篤')), 
  res = results_T24, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Anemia 貧血
results_T24 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Anemia', 
  AE_label = '貧血',
  AE_cate_vars = c("Seriousness"), 
  AE_cate = list(c('重篤', '非重篤')), 
  res = results_T24, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Hypertension 高血圧
results_T24 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Hypertension', 
  AE_label = '高血圧',
  AE_cate_vars = c("Seriousness"), 
  AE_cate = list(c('重篤', '非重篤')), 
  res = results_T24, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Rash 発疹
results_T24 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Rash', 
  AE_label = '発疹',
  AE_cate_vars = c("Seriousness"), 
  AE_cate = list(c('重篤', '非重篤')), 
  res = results_T24, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

export_table(results_T24, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T24", fig_name = NULL, add_table = TRUE, rowName = TRUE)



# Table25 安全性の解析 各有害事象の発現割合（重症度別） -----------------------------------------

results_T25 <- NULL

# Nausea 吐気
results_T25 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Nausea', 
  AE_label = '吐気',
  AE_cate_vars = c("Severity"), 
  AE_cate = list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
  res = results_T25, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Vomiting 嘔吐
results_T25 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Vomiting', 
  AE_label = '嘔吐',
  AE_cate_vars = c("Severity"), 
  AE_cate = list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
  res = results_T25, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Diarrhea 下痢
results_T25 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Diarrhea', 
  AE_label = '下痢',
  AE_cate_vars = c("Severity"), 
  AE_cate = list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
  res = results_T25, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Headache 頭痛
results_T25 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Headache', 
  AE_label = '頭痛',
  AE_cate_vars = c("Severity"), 
  AE_cate = list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
  res = results_T25, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Dizziness めまい
results_T25 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Dizziness', 
  AE_label = 'めまい',
  AE_cate_vars = c("Severity"), 
  AE_cate = list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
  res = results_T25, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Fatigue 疲労
results_T25 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Fatigue', 
  AE_label = '疲労',
  AE_cate_vars = c("Severity"), 
  AE_cate = list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
  res = results_T25, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Fever 発熱
results_T25 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Fever', 
  AE_label = '発熱',
  AE_cate_vars = c("Severity"), 
  AE_cate = list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
  res = results_T25, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Anemia 貧血
results_T25 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Anemia', 
  AE_label = '貧血',
  AE_cate_vars = c("Severity"), 
  AE_cate = list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
  res = results_T25, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Hypertension 高血圧
results_T25 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Hypertension', 
  AE_label = '高血圧',
  AE_cate_vars = c("Severity"), 
  AE_cate = list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
  res = results_T25, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)

# Rash 発疹
results_T25 <- AE_each_item_by_cate(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  AE_name_var = 'AE_Term',
  AE_item = 'Rash', 
  AE_label = '発疹',
  AE_cate_vars = c("Severity"), 
  AE_cate = list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
  res = results_T25, 
  subjid_var = 'ID',
  with_95CI = with_95CI
)


export_table(results_T25, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T25", fig_name = NULL, add_table = TRUE, rowName = TRUE)



# Table26 安全性の解析 臨床検査地の集計 -------------------------------------------------

timepoints <-  c("Followup_1", "Followup_2", "Followup_3", "Followup_4", "Followup_5")

results_T26 <- NULL

# 収縮期血圧
results_T26 <- summarise_labo_cont(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  var = "Systolic_bp", 
  item_name = "収縮期血圧", 
  res = results_T26, 
  summary_stat = summary_items,
  timepoints = timepoints,
  time_var = 'Time_point',
  subjid_var = 'ID',
  digits = 1
)

# 赤血球
results_T26 <- summarise_labo_cont(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  var = "RBC", 
  item_name = "赤血球", 
  res = results_T26, 
  summary_stat = summary_items,
  timepoints = timepoints,
  time_var = 'Time_point',
  subjid_var = 'ID',
  digits = 1
)

# 白血球
results_T26 <- summarise_labo_cont(
  dat = safety_analysis_data, 
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"),
  var = "WBC", 
  item_name = "白血球", 
  res = results_T26, 
  summary_stat = summary_items,
  timepoints = timepoints,
  time_var = 'Time_point',
  subjid_var = 'ID',
  digits = 1
)


# 尿検査
results_T26 <- summarise_labo_discre(
  dat = safety_analysis_data,
  group_var = "Treatment_Group", 
  group_labels = c("Control", "Treatment"), 
  var = "urine", 
  item_name = "尿検査", 
  labels = c('3-', '2-', '-', '±', '+', '2+', '3+'),
  res = results_T26, 
  timepoints = timepoints, 
  time_var = 'Time_point',
  subjid_var = 'ID'
)

export_table(results_T26, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T26", fig_name = NULL, add_table = TRUE, rowName = TRUE)





