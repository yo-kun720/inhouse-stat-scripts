# 連続量の集計の際に提示する要約統計量の種類を指定
summary_items = c('n', 'mean_SD', 'min_max', 'median_IQR')


# T01 背景情報の集計　---------------------------------------------------------------------
dat = use_data %>% filter(fas == "採用", Time_point == "Baseline")

res = NULL
res = tab1_continuous(dat = dat, 
                      group_var = "Treatment_Group", 
                      group_labels = c("Control", "Treatment"),
                      var = "Age", 
                      item_name = "年齢", 
                      res = res, 
                      summary_stat = summary_items,
                      digits = 1)

res = tab1_discrete(dat = dat,
                    group_var = "Treatment_Group", 
                    group_labels = c("Control", "Treatment"),
                    var = "Gender", 
                    item_name = "性別", 
                    labels = c("Male", "Female"),
                    res = res)

res = tab1_discrete(dat, "Treatment_Group", c("Control", "Treatment"), "Smoker", "喫煙", c('Yes', 'No'), res)
res = tab1_continuous(dat, "Treatment_Group", c("Control", "Treatment"), "BMI", "BMI", res, summary_items)
res = tab1_discrete(dat, "Treatment_Group", c("Control", "Treatment"), "Severity_scale", "重症度スコア", c('3', '4', '5', '6'), res)

export_table(res, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T01", fig_name = NULL, add_table = FALSE, rowName = TRUE)

# T02 2値データの解析(割合の比や差の算出) ---------------------------------------------------------------------
dat = use_data %>% filter(fas == '採用', Time_point == 'Followup_1') %>% 
  select(ID, Time_point, Treatment_Group, Death_till_30, Death_till_60, Death_till_90) %>% 
  merge(use_data %>% filter(fas == '採用', Time_point == 'Baseline') %>% select(ID, Age, Gender, Smoker, BMI, Severity_scale),
        by = "ID")

res = binary_analysis(dat = dat, 
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
                      delta = 0)

export_table(res, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T02", fig_name = NULL, add_table = TRUE, rowName = TRUE)

# T03 2値データの解析(ロジスティック回帰モデルによるオッズ比) --------------------------------------------------------------
res = logistic_table_multivar(dat           = dat,          # データフレーム
                              covar_labels  = c(Treatment_Group = "治療群", # 変数名=表示名
                                                Age             = "年齢",
                                                Gender          = "性別",
                                                Smoker          = "喫煙歴",
                                                BMI             = "BMI",
                                                Severity_scale  = "重症度スコア"),             
                              # 水準ラベル（必要なものだけ指定。未指定の因子はデータの既存順で ref 決定）
                              level_labels  = list(Treatment_Group = c(Control = "Control", Treatment = "Treatment"), # 因子の元レベル=>表示ラベル
                                                   Gender          = c(Female = "F (女性)", Male = "M (男性)"),
                                                   Smoker          = c(No = "0 (なし)", Yes = "1 (あり)")),
                              # BMI, Severity_scale は連続想定なので指定不要
                              outcome_var     = "Death_till_30",
                              OR_CI_level   = 0.95,
                              return_gt     = FALSE
)

export_table(res, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T03", fig_name = NULL, add_table = TRUE, rowName = TRUE)

# T04 生存時間解析(生存時間中央値と介入に関するハザード比) --------------------------------------------------------------
dat = merge(use_data %>% filter(fas == '採用', Time_point == 'Baseline') %>% select(ID, Date_of_enroll),
            use_data %>% filter(fas == '採用', Time_point == 'Followup_1') %>% select(ID, Death_till_30),
            by = "ID") %>% 
  merge(use_data %>% filter(fas == "採用", Time_point == 'Followup_3') %>% select(ID, Death_till_60),
        by = "ID") %>% 
  merge(use_data %>% filter(fas == "採用", Time_point == 'Followup_5') %>% select(ID, Death_till_90),
        by = "ID") %>% 
  merge(use_data %>% filter(fas == "採用", Time_point == 'End_of_Study') %>% 
          select(ID, Treatment_Group, Observed_Time, Event, End_date),
        by = "ID") %>% 
  merge(use_data %>% filter(fas == '採用', Time_point == 'Baseline') %>% select(ID, Age, Gender, Smoker, BMI, Severity_scale),
        by = "ID")

res = survival_analysis(dat = dat, 
                        group_var = "Treatment_Group",
                        group_labels = c("Control", "Treatment"),
                        event_var = 'Death_till_30',
                        time_var = "Observed_Time",
                        max_obs_time = 30,
                        HR_CI_level = 0.95)
export_table(res, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T04", fig_name = NULL, add_table = TRUE, rowName = TRUE)


# F01 KM曲線 ----------------------------------------------------------------
# R studioのPlotsタブにプロットが表示される
KM = KM_curve(dat = dat, 
              group_var = "Treatment_Group",
              group_labels = c("Control", "Treatment"),
              event_var = 'Death_till_30',
              time_var = "Observed_Time",
              max_obs_time = 30,
              KM_x_label = 'Days to death until Day 30',
              KM_x_axis_ticks = c(0, 10, 20, 30),
              RT_event_label = "Number of death")

ggsave(file = 'outputs/Figure_KM.jpeg', plot = KM, device = "jpeg", 
       dpi = 600, width = 4/3*4.8*2, height = 4.8*2)

# T05 生存時間解析(Cox比例ハザードモデルによるハザード比) --------------------------------------------------------------
res = cox_table_multivar(dat           = dat,          # データフレーム
                         covar_labels  = c(Treatment_Group = "治療群", # 変数名=表示名
                                           Age             = "年齢",
                                           Gender          = "性別",
                                           Smoker          = "喫煙歴",
                                           BMI             = "BMI",
                                           Severity_scale  = "重症度スコア"),             
                         # 水準ラベル（必要なものだけ指定。未指定の因子はデータの既存順で ref 決定）
                         level_labels  = list(Treatment_Group = c(Control = "Control", Treatment = "Treatment"), # 因子の元レベル=>表示ラベル
                                              Gender          = c(Female = "F (女性)", Male = "M (男性)"),
                                              Smoker          = c(No = "0 (なし)", Yes = "1 (あり)")),
                         # BMI, Severity_scale は連続想定なので指定不要
                         event_var     = "Death_till_30",
                         time_var      = "Observed_Time",
                         max_obs_time  = 30, 
                         HR_CI_level   = 0.95,
                         return_gt     = FALSE)

export_table(res, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T05", fig_name = NULL, add_table = TRUE, rowName = TRUE)


# T21 AE全体全種類 --------------------------------------------------------------------
dat = use_data %>% filter(sas == '採用')
res = NULL
res = AE_all_item(dat = dat, 
                  group_var = "Treatment_Group", 
                  group_labels = c("Control", "Treatment"),
                  AE_name_var = 'AE_Term',
                  AE_cate_vars = c("Causal", "Seriousness", "Severity"), 
                  AE_cate = list(c('否定できない', '否定できる'),
                                 c('重篤', '非重篤'),
                                 c('G1', 'G2', 'G3', 'G4', 'G5')), 
                  res = res, 
                  subjid_var = 'ID')
export_table(res, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T21", fig_name = NULL, add_table = TRUE, rowName = TRUE)


# T22 AE各項目の集計 ----------------------------------------------------------------
with_95CI = TRUE # AE発生割合のCI有無

dat = use_data %>% filter(sas == '採用')

res = NULL
res = AE_each_item(dat = dat, 
                   group_var = "Treatment_Group", 
                   group_labels = c("Control", "Treatment"),
                   AE_name_var = 'AE_Term',
                   AE_item = 'Fatigue', 
                   AE_label = '疲労',
                   res = res, 
                   subjid_var = 'ID',
                   with_95CI = with_95CI)
res = AE_each_item(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                   'Nausea', '吐気',
                   res, 'ID', with_95CI)
res = AE_each_item(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                   'Dizziness', 'めまい',
                   res, 'ID', with_95CI)
res = AE_each_item(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                   'Fever', '発熱',
                   res, 'ID', with_95CI)
export_table(res, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T22", fig_name = NULL, add_table = TRUE, rowName = TRUE)

# T23 因果関係ごとのAE各項目の集計 ----------------------------------------------------------------
dat = use_data %>% filter(sas == '採用')

res = NULL
res = AE_each_item_by_cate(dat = dat, 
                           group_var = "Treatment_Group", 
                           group_labels = c("Control", "Treatment"),
                           AE_name_var = 'AE_Term',
                           AE_item = 'Fatigue', 
                           AE_label = '疲労',
                           AE_cate_vars = c("Causal"), 
                           AE_cate = list(c('否定できない', '否定できる')), 
                           res = res, 
                           subjid_var = 'ID',
                           with_95CI = with_95CI)
res = AE_each_item_by_cate(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                           'Nausea', '吐気', c("Causal"), list(c('否定できない', '否定できる')), 
                           res, 'ID', with_95CI)
res = AE_each_item_by_cate(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                           'Dizziness', 'めまい', c("Causal"), list(c('否定できない', '否定できる')), 
                           res, 'ID', with_95CI)
res = AE_each_item_by_cate(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                           'Fever', '発熱', c("Causal"), list(c('否定できない', '否定できる')), 
                           res, 'ID', with_95CI)

export_table(res, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T23", fig_name = NULL, add_table = TRUE, rowName = TRUE)

# T24 重篤性ごとのAE各項目の集計 ----------------------------------------------------------------
dat = use_data %>% filter(sas == '採用')

res = NULL
res = AE_each_item_by_cate(dat = dat, 
                           group_var = "Treatment_Group", 
                           group_labels = c("Control", "Treatment"),
                           AE_name_var = 'AE_Term',
                           AE_item = 'Fatigue', 
                           AE_label = '疲労',
                           AE_cate_vars = c("Seriousness"), 
                           AE_cate = list(c('重篤', '非重篤')), 
                           res = res, 
                           subjid_var = 'ID', with_95CI)
res = AE_each_item_by_cate(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                           'Nausea', '吐気', c("Seriousness"), list(c('重篤', '非重篤')), 
                           res, 'ID', with_95CI)
res = AE_each_item_by_cate(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                           'Dizziness', 'めまい', c("Seriousness"), list(c('重篤', '非重篤')), 
                           res, 'ID', with_95CI)
res = AE_each_item_by_cate(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                           'Fever', '発熱', c("Seriousness"), list(c('重篤', '非重篤')), 
                           res, 'ID', with_95CI)

export_table(res, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T24", fig_name = NULL, add_table = TRUE, rowName = TRUE)

# T25 グレードごとのAE各項目の集計 ----------------------------------------------------------------
dat = use_data %>% filter(sas == '採用')

res = NULL
res = AE_each_item_by_cate(dat = dat, 
                           group_var = "Treatment_Group", 
                           group_labels = c("Control", "Treatment"),
                           AE_name_var = 'AE_Term',
                           AE_item = 'Fatigue', 
                           AE_label = '疲労',
                           AE_cate_vars = c("Severity"), 
                           AE_cate = list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
                           res = res, 
                           subjid_var = 'ID', with_95CI)
res = AE_each_item_by_cate(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                           'Nausea', '吐気', c("Severity"), list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
                           res, 'ID', with_95CI)
res = AE_each_item_by_cate(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                           'Dizziness', 'めまい', c("Severity"), list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
                           res, 'ID', with_95CI)
res = AE_each_item_by_cate(dat, "Treatment_Group", c("Control", "Treatment"), 'AE_Term',
                           'Fever', '発熱', c("Severity"), list(c('G1', 'G2', 'G3', 'G4', 'G5')), 
                           res, 'ID', with_95CI)

export_table(res, "outputs/result_output.xlsx", r_begin = 1, c_begin = 1, 
             sn = "T25", fig_name = NULL, add_table = TRUE, rowName = TRUE)



# T26 臨床検査値 -------------------------------------------------------------------
dat = use_data %>% filter(sas == '採用')
timepoints = c("Followup_1", "Followup_2", "Followup_3", "Followup_4", "Followup_5")

res = NULL
res = summarise_labo_cont(dat = dat, 
                          group_var = "Treatment_Group", 
                          group_labels = c("Control", "Treatment"),
                          var = "Systolic_bp", 
                          item_name = "収縮期血圧", 
                          res = res, 
                          summary_stat = summary_items,
                          timepoints = timepoints,
                          time_var = 'Time_point',
                          subjid_var = 'ID',
                          digits = 1)

res = summarise_labo_cont(dat, "Treatment_Group", c("Control", "Treatment"), "RBC", "赤血球", 
                          res, summary_items, timepoints, 'Time_point', 'ID', 1)
res = summarise_labo_cont(dat, "Treatment_Group", c("Control", "Treatment"), "WBC", "白血球", 
                          res, summary_items, timepoints, 'Time_point', 'ID', 1)
res = summarise_labo_discre(dat = dat,
                            group_var = "Treatment_Group", 
                            group_labels = c("Control", "Treatment"), 
                            var = "urine", 
                            item_name = "尿検査", 
                            labels = c('3-', '2-', '-', '±', '+', '2+', '3+'),
                            res = res, 
                            timepoints = timepoints, 
                            time_var = 'Time_point',
                            subjid_var = 'ID')

export_table(res, "outputs/result_output.xlsx", r_begin = 1, c_begin = 2, 
             sn = "T26", fig_name = NULL, add_table = TRUE, rowName = TRUE)
