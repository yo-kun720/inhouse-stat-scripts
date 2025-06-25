round2 = function(x, d = 0){
  #' 四捨五入関数
  #' last update: 2023/11/15
  #' @param x 四捨五入を行う対象の数値
  #' @param d 小数点以下 d+1 位で四捨五入実施
  x10d = round(as.numeric(x) * 10^d, 10)
  return(format(trunc(x10d + sign(x10d) * 0.5) / 10^d, nsmall = d))
}

calc_freqprop = function(data, bunbo, usena) {
  #' tab1_discrete内で用いる補助関数
  #' last update: 2025/6/20
  #' @param data 集計対象となるデータ
  #' @param bunbo 解析対象に含まれる人数 = 割合の分母
  #' @param usena 分割表に欠測の水準を含めるかどうか
  freq = table(data, useNA = usena)
  prop = round(freq / bunbo * 100, 1)
  result = paste0(freq, " (", format(prop, nsmall = 1), ")")
  return(result)
}

tab1_continuous = function(dat, group_var, group_labels,
                           var, item_name, res = res, 
                           summary_stat = c('n', 'median_IQR'),
                           digits = 1){
  #' 連続変数に対する集計関数
  #' last update: 2025/6/20
  #' @param dat 使用するデータセット
  #' @param group_var 治療群などの群を表す変数
  #' @param group_labels 治療群に対するラベル．Tab 1の列名となる
  #' @param var 集計する連続変数の列名
  #' @param item_name 結果出力時の変数のラベル
  #' @param res 結果を出力するオブジェクト
  #' @param summary_stat 提示する要約統計量 'n', 'mean_SD', 'median_IQR', 'min_max'を組合せて指定
  #' @param digits 結果出力時に小数点何位まで出力するか
  
  N = nrow(dat)
  tmpres = dat %>% rename(theitem = all_of(var)) %>% 
    filter(!is.na(theitem)) %>% 
    mutate(theitem = as.numeric(theitem)) %>% 
    summarise(n = n(),
              mean = mean(theitem, na.rm = T),
              sd = sd(theitem, na.rm = T),
              median = median(theitem, na.rm = T),
              q25 = quantile(theitem, na.rm = T, type = 2)["25%"],
              q75 = quantile(theitem, na.rm = T, type = 2)["75%"],
              min = quantile(theitem, na.rm = T, type = 2)["0%"],
              max = quantile(theitem, na.rm = T, type = 2)["100%"]) %>% 
    as.data.frame()
  tmpres %<>% mutate(n = paste0(n, ' (', N - n, ')'),
                     mean_SD = paste0(round2(mean, digits), ' (', round2(sd, digits), ')'),
                     median_IQR = paste0(round2(median, digits), ' (', round2(q25, digits), ", ", round2(q75, digits), ')'),
                     min_max = paste0(format(min, nsmall = digits), ", ", format(max, nsmall = digits))) 
  tmpres = tmpres[, summary_stat]
  tmpres = t(tmpres)
  tmpres = data.frame(
    item = c(item_name, rep("", nrow(tmpres) - 1)),
    measurement = rownames(tmpres),
    Total = tmpres[, 1]
  )
  
  for (g in group_labels) {
    N = dat %>% 
      filter(.data[[group_var]] == g) %>% 
      nrow()
    
    subres = dat %>% rename(theitem = all_of(var)) %>% 
      filter(!is.na(theitem)) %>% 
      filter(.data[[group_var]] == g) %>% 
      mutate(theitem = as.numeric(theitem)) %>% 
      summarise(n = n(),
                mean = mean(theitem, na.rm = T),
                sd = sd(theitem, na.rm = T),
                median = median(theitem, na.rm = T),
                q25 = quantile(theitem, na.rm = T, type = 2)["25%"],
                q75 = quantile(theitem, na.rm = T, type = 2)["75%"],
                min = quantile(theitem, na.rm = T, type = 2)["0%"],
                max = quantile(theitem, na.rm = T, type = 2)["100%"]) %>% 
      as.data.frame()
    subres %<>% mutate(n = paste0(n, ' (', N - n, ')'),
                       mean_SD = paste0(round2(mean, digits), ' (', round2(sd, digits), ')'),
                       median_IQR = paste0(round2(median, digits), ' (', round2(q25, digits), ", ", round2(q75, digits), ')'),
                       min_max = paste0(format(min, nsmall = digits), ", ", format(max, nsmall = digits)))
    subres = subres[, summary_stat]
    subres = t(subres)
    tmpres = cbind(tmpres, setNames(data.frame(subres[, 1]), g))
  }
  dat %<>% rename(theitem = all_of(var), 
                  group = all_of(group_var)) %>% 
    filter(!is.na(theitem))
  
  if(length(summary_stat) == 1){
    tmpres = bind_cols(tmpres[, 1], 
                       summary_stat, 
                       tmpres[, 2:ncol(tmpres)])
  }
  colnames(tmpres) = c('item', 'measurement', 'Total', group_labels)
  res %<>% bind_rows(tmpres)
  return(res)
}


tab1_discrete = function(dat, group_var, group_labels, 
                         var, item_name, labels, res = res){
  #' 離散変数に対する集計関数
  #' last update: 2025/6/20
  #' @param dat 使用するBLデータセット（基本的に1人1行）
  #' @param group_var 治療群などの群を表す変数
  #' @param group_labels 治療群に対するラベル．Tab 1の列名となる
  #' @param var 集計する離散変数の列名
  #' @param item_name 結果出力時の変数のラベル
  #' @param labels 離散変数の水準のラベル
  #' @param res 結果を出力するオブジェクト
  
  bunbo = length(unique(dat[[1]]))
  tmp = dat %>% dplyr::rename(the_item = all_of(var))
  
  usena = if (sum(is.na(tmp$the_item)) > 0) "always" else "no"
  levels_all = if (usena == "always") c(labels, "missing") else labels
  
  total_res = calc_freqprop(tmp$the_item, bunbo, usena)
  tmpres = data.frame(
    measurement = levels_all,
    Total = total_res
  )
  
  for (g in group_labels) {
    subg = dat %>% dplyr::filter(.data[[group_var]] == g)
    bunbo_sub = length(unique(subg[[1]]))
    subg = subg %>% dplyr::rename(the_item = all_of(var))
    sub_res = calc_freqprop(subg$the_item, bunbo_sub, usena)
    tmpres[[g]] = sub_res
  }
  
  tmpres = data.frame(
    item = c(item_name, rep("", nrow(tmpres) - 1)),
    tmpres
  )
  
  res = dplyr::bind_rows(res, tmpres)
  return(res)
}

binary_analysis = function(dat, group_var, group_labels, 
                           bin_var, subjid_var, 
                           CI_level = 0.95, CI_sides = 'two.sided', 
                           Diff_CI_method = "score",
                           Ratio_CI_method = "katz.log",
                           test_method,
                           test_sides = "two.sided",
                           delta = 0){
  #' 二値変数に対する解析を実施する関数
  #' last update: 2025/6/20
  #' @param dat 使用するBLデータセット（基本的に1人1行）
  #' @param group_var 治療群などの群を表す変数
  #' @param group_labels 治療群に対するラベル．水準の順序（factor関数のlevels引数）と同じにする．
  #' @param bin_var 解析対象となる2値変数名
  #' @param subjid_var 各患者のID変数名
  #' @param CI_level RR，RDのCIの信頼水準
  #' @param CI_sides CIの片側，両側指定．"two.sided", "left", "right"から選択
  #' @param Diff_CI_method RDのCIを算出する際の方法，詳細はDescTools::BinomDiffCIのヘルプを参照
  #' @param Ratio_CI_method RRのCIを算出する際の方法，詳細はDescTools::BinomRatioCIのヘルプを参照
  #' @param test_method p値を算出するための検定手法．"FM", "Fisher", "Chisq", "Score"から選択
  #' @param test_sides 検定の片側，両側指定．"two.sided", "greater", "less"から選択
  #' @param delta Farrington-Manning検定で想定する帰無仮説下での群間差（test_sidesの組み合わせにより比劣性検定も可能）
  
  N = dat %>% 
    select(all_of(subjid_var)) %>% 
    pull() %>% 
    unique() %>% 
    length()
  
  if(N != nrow(dat)){
    stop("Error: N in the analysis set and the number of rows in 'dat' are not equal.")
  }
  
  if(!test_method %in% c("FM", "Fisher", "Chisq", "Score")){
    stop("Error: You must choose one of 'test_method' from 'FM', 'Fisher', 'Chisq', and 'Score'.")
  }
  
  # 2×2分割表の作成
  table_dat = dat %>%
    group_by(.data[[group_var]]) %>%
    summarise(Event = sum(.data[[bin_var]]), 
              Non_Event = n() - sum(.data[[bin_var]])) %>%
    arrange(factor(.data[[group_var]], levels = group_labels))
  
  contingency_table = as.matrix(table_dat[, c("Non_Event", "Event")])
  rownames(contingency_table) = group_labels
  
  tmpres = c('', 'Proportion', 'Confidence Interval')
  
  # group_labels１のイベント数と全体数
  x1 = contingency_table[group_labels[1], "Event"]
  n1 = sum(contingency_table[group_labels[1], ])
  group1_prop = binom.exact(x1, n1, conf.level = CI_level)
  
  # group_labels２のイベント数と全体数
  x2 = contingency_table[group_labels[2], "Event"]
  n2 = sum(contingency_table[group_labels[2], ])
  group2_prop = binom.exact(x2, n2, conf.level = CI_level)
  
  # 信頼区間の算出
  Diff_CI = BinomDiffCI(x1 = x2, n1 = n2, x2 = x1, n2 = n1,
                        sides = CI_sides,
                        conf.level = CI_level,
                        method = Diff_CI_method)
  
  Ratio_CI = BinomRatioCI(x1 = x2, n1 = n2, x2 = x1, n2 = n1,
                          sides = CI_sides,
                          conf.level = CI_level,
                          method = Ratio_CI_method)  
  
  for (i in 1:length(group_labels)) {
    eval(parse(text = paste0("group_prop = group", i, "_prop"))) 
    tmpres %<>% rbind(c(group_labels[i], 
                        paste0(round2(group_prop$estimate*100, 1), " (",
                               group_prop$statistic, " / ",
                               group_prop$parameter, ")"),
                        paste0(round2(group_prop$conf.int*100, 1), 
                               collapse = ", ")))
    
  }
  
  # 各種検定のp値算出．2×2分割表においてはFM(delta = 0)とカイ二乗とScore検定は同一p値
  if(test_method == 'FM'){ # Farrington‐Manning検定の実施
    if(delta == 0){
      warning("Farrington-Manning test was pecified even though delta = 0.")
    }
    p_val = farrington.manning(group1 = c(rep(TRUE, x1), rep(FALSE, n1 - x1)),
                               group2 = c(rep(TRUE, x2), rep(FALSE, n2 - x2)),
                               delta = delta,
                               alternative = test_sides,
                               alpha = 1 - CI_level)$p.value
    
  }else if(test_method == 'Fisher'){ # Fisherの正確検定のp値
    p_val = exact2x2(matrix(c(x1, n1 - x1, x2, n2 - x2), 2, 2),
                     alternative = test_sides)$p.value
    
  }else if(test_method == 'Chisq'){ # Chi-squared test
    if(min(c(x1, n1 - x1, x2, n2 - x2)) < 5){
      warning(paste0("The minimum cell count is smaller than 5, that is ", min(c(x1, n1 - x1, x2, n2 - x2)), "."))
    }
    p_val = chisq.test(matrix(c(x1, n1 - x1, x2, n2 - x2), 2, 2),
                       correct = FALSE)$p.value
    
  }else if(test_method == 'Score'){ # スコア信頼区間 (Diff_CI_method = 'score')に対応する検定p値
    if(min(c(x1, n1 - x1, x2, n2 - x2)) < 5){
      warning(paste0("The minimum cell count is smaller than 5, that is ", min(c(x1, n1 - x1, x2, n2 - x2)), "."))
    }
    p_val = scoreci(x1 = x2, n1 = n2, x2 = x1, n2 = n1, 
                    distrib = 'bin',
                    contrast = 'RD',
                    level = CI_level,
                    skew = FALSE,
                    simpleskew = FALSE,
                    bcf = FALSE,
                    cc = FALSE)$pval[1, 'pval2sided']
  }
  
  if(p_val < 0.01){
    p_val = "<.01"
  }else{
    p_val = round2(p_val, 3)
  }
  
  tmpres %<>% rbind(c('', 'Point Estimate', 'p-value'))
  tmpres %<>% rbind(c("Risk Difference", 
                      paste0(round2(Diff_CI[1]*100, 1), " (",
                             round2(Diff_CI[2]*100, 1), ", ",
                             round2(Diff_CI[3]*100, 1), ")"),
                      p_val))
  
  tmpres %<>% rbind(c("Relative Risk", 
                      paste0(round2(Ratio_CI[1], 2), " (",
                             round2(Ratio_CI[2], 2), ", ",
                             round2(Ratio_CI[3], 2), ")"),
                      ""))
  
  return(tmpres)
}

survival_analysis = function(dat, group_var, group_labels, 
                             event_var, time_var,
                             max_obs_time,
                             HR_CI_level = 0.95){
  #' 生存時間変数に対する解析を実施する関数
  #' last update: 2025/6/20
  #' @param dat 使用するBLデータセット（基本的に1人1行）
  #' @param group_var 治療群などの群を表す変数
  #' @param group_labels 治療群に対するラベル．治療群に対するラベル．水準の順序（factor関数のlevels引数）と同じにする．
  #' @param event_var イベント発生を表す変数名．1がイベント．0が打ち切り．
  #' @param time_var イベント発生までの時間データを含む変数名
  #' @param max_obs_time 解析で検討する期間の最終時点（e.g., 90日間の死亡までの時間）
  #' @param HR_CI_level ハザード比に対するCIの信頼水準
  #' 
  
  dat %<>% mutate(!!sym(time_var) := if_else(!!sym(time_var) > max_obs_time, 
                                             max_obs_time,
                                             !!sym(time_var)))
  # 文字列を組み立ててフォーミュラに変換
  formula_text = paste0("Surv(", time_var, ", ", event_var, ") ~ ", group_var)
  fml = as.formula(formula_text)
  
  km_fit = survfit2(fml, data = dat)
  
  # 生存時間中央値に関する推定値
  km_df = as.data.frame(summary(km_fit)$table)
  Med_surv = c(paste0(round2(km_df$median, 0), " (",
                      round2(km_df$`0.95LCL`, 1), ", ",
                      round2(km_df$`0.95UCL`, 1), ")")) %>% 
    gsub("NA", "-", .)
  
  # Coxハザードモデルの適用
  cox_model = coxph(fml, data = dat)
  cox_summary = summary(cox_model, conf.int = HR_CI_level)
  
  # スコア検定のp値を抽出
  p_val = cox_summary$sctest['pvalue']
  if(p_val < 0.01){
    p_val = "<.01"
  }else{
    p_val = round2(p_val, 3)
  }
  
  tmpres = data.frame(Group = group_labels, 
                      Median_survival = Med_surv,
                      Hazard_ratio = c("", 
                                       paste0(round2(cox_summary$conf.int[, "exp(coef)"], 2), " (",
                                              round2(cox_summary$conf.int[, "lower .95"], 2), ", ",
                                              round2(cox_summary$conf.int[, "upper .95"], 2), ")")),
                      p_value = c("", p_val))
  
  return(tmpres)
}

KM_curve = function(dat, group_var, group_labels, 
                    event_var, time_var,
                    max_obs_time,
                    KM_x_label,
                    KM_x_axis_ticks,
                    RT_event_label = "Number of event"){
  #' 生存時間変数を用いてKM曲線を描画する関数
  #' last update: 2025/6/20
  #' @param dat 使用するBLデータセット（基本的に1人1行）
  #' @param group_var 治療群などの群を表す変数
  #' @param group_labels 治療群に対するラベル．治療群に対するラベル．水準の順序（factor関数のlevels引数）と同じにする．
  #' @param event_var イベント発生を表す変数名．1がイベント．0が打ち切り．
  #' @param time_var イベント発生までの時間データを含む変数名
  #' @param max_obs_time 解析で検討する期間の最終時点（e.g., 90日間の死亡までの時間）
  #' @param KM_x_label KM曲線のX軸ラベル
  #' @param KM_x_axis_ticks KM曲線のX軸における時点
  #' @param RT_event_label リスクテーブルにおけるイベント側のラベル
  
  dat %<>% mutate(!!sym(time_var) := if_else(!!sym(time_var) > max_obs_time, 
                                             max_obs_time,
                                             !!sym(time_var)))
  # 文字列を組み立ててフォーミュラに変換
  formula_text = paste0("Surv(", time_var, ", ", event_var, ") ~ ", group_var)
  fml = as.formula(formula_text)
  
  km_fit = survfit2(fml, data = dat)
  
  p = ggsurvfit(km_fit, type = 'survival', linewidth = 1.2) +
    labs(x = KM_x_label,
         y = "Survival probability") +
    add_censor_mark(size = 3) +
    add_confidence_interval() +
    add_pvalue(caption = "Log-rank {p.value}",
               location = "annotation", x = 25, y = 0.13, size = 6) +
    add_risktable(times = KM_x_axis_ticks, 
                  risktable_stats = c("n.risk", "cum.event"), 
                  stats_label = list(n.risk = "Number at risk", cum.event = RT_event_label),
                  size = 6, 
                  theme = theme_risktable_default(plot.title.size = 20, axis.text.y.size = 20)) +
    scale_color_manual(values = c('#4169e1', '#ff4500')) +
    scale_fill_manual(values = c('#4169e1', '#ff4500')) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, max_obs_time),
                       breaks = KM_x_axis_ticks) +
    theme_classic(base_size = 20) +
    theme(axis.title = element_text(size = 20), 
          axis.text = element_text(colour = "black"),
          axis.title.y = element_text(vjust = 8.5), # y軸ラベルの位置調整
          legend.position = 'bottom',
          plot.margin= unit(c(0, 2, 0, 2), "lines"))
  
  print(p)
}


AE_all_item = function(dat, group_var, group_labels, 
                       AE_name_var, AE_cate_vars, AE_cate, 
                       res = res, subjid_var = 'SUBJID'){
  #' AE全体を全体・因果関係別・重篤性別・重症度別に集計する関数
  #' last update: 2025/2/6
  #' @param dat 使用するBLデータセット（基本的に1人1行）
  #' @param group_var 治療群などの群を表す変数
  #' @param group_labels 治療群に対するラベル．治療群に対するラベル．水準の順序（factor関数のlevels引数）と同じにする．
  #' @param AE_name_var 発現したAEの名称が含まれる変数名
  #' @param AE_cate_vars カテゴリ別に集計したい変数の変数名（e.g., 因果関係や重症度）
  #' @param AE_cate AE_cate_varsで指定した変数に含まれるカテゴリ
  #' @param res 結果を出力するオブジェクト
  #' @param subjid_var 各患者のID変数名
  #' 
  
  tmpres = NULL
  # 全集団の集計
  N = dat %>% 
    select(all_of(subjid_var)) %>% 
    pull() %>% 
    unique() %>% 
    length()
  
  whole = dat %>% 
    filter(!is.na(.data[[AE_name_var]])) %>% 
    select(all_of(subjid_var)) %>% 
    pull() %>% 
    unique() %>% 
    length()
  
  tmp = c(N, paste0(whole, ' (', round2(whole/N*100, 1), ')'))
  for (i in 1:length(AE_cate_vars)) {
    for (j in 1:length(AE_cate[[i]])) {
      N_each_cate = dat %>% 
        filter(.data[[AE_cate_vars[i]]] == AE_cate[[i]][j]) %>% 
        select(all_of(subjid_var)) %>% 
        pull() %>% 
        unique() %>% 
        length()
      tmp %<>% c(paste0(N_each_cate, ' (', round2(N_each_cate/N*100, 1), ')'))
    }
  }
  tmpres %<>% rbind(tmp)
  
  for (g in group_labels) {
    N = dat %>% 
      filter(.data[[group_var]] == g) %>% 
      select(all_of(subjid_var)) %>% 
      pull() %>% 
      unique() %>% 
      length()
    
    whole = dat %>% 
      filter(.data[[group_var]] == g) %>% 
      filter(!is.na(.data[[AE_name_var]])) %>% 
      select(all_of(subjid_var)) %>% 
      pull() %>% 
      unique() %>% 
      length()
    
    tmp = c(N, paste0(whole, ' (', round2(whole/N*100, 1), ')'))
    cate_names = NULL
    for (i in 1:length(AE_cate_vars)) {
      cate_names %<>% c(AE_cate[[i]])
      for (j in 1:length(AE_cate[[i]])) {
        N_each_cate = dat %>% 
          filter(.data[[group_var]] == g) %>% 
          filter(.data[[AE_cate_vars[i]]] == AE_cate[[i]][j]) %>% 
          select(all_of(subjid_var)) %>% 
          pull() %>% 
          unique() %>% 
          length()
        tmp %<>% c(paste0(N_each_cate, ' (', round2(N_each_cate/N*100, 1), ')'))
      }
    }
    tmpres %<>% rbind(tmp)
  }
  tmpres %<>% as.data.frame()
  rownames(tmpres) = c('Whole', group_labels)
  colnames(tmpres) = c('N', "合計", cate_names)
  return(tmpres)
}

AE_each_item = function(dat, group_var, group_labels, 
                        AE_name_var, AE_item, AE_label,
                        res = res, subjid_var = 'SUBJID',
                        with_95CI = FALSE){
  #' 各AEを集計する関数
  #' last update: 2025/3/5
  #' @param dat 使用するBLデータセット（基本的に1人1行）
  #' @param group_var 治療群などの群を表す変数
  #' @param group_labels 治療群に対するラベル．治療群に対するラベル．水準の順序（factor関数のlevels引数）と同じにする．
  #' @param AE_name_var 発現したAEの名称が含まれる変数名
  #' @param AE_item AE_name_varに含まれる集計したいAEの名称
  #' @param AE_label 結果出力時のTable上で表示されるAEの名称
  #' @param res 結果を出力するオブジェクト
  #' @param subjid_var 各患者のID変数名
  #' @param with_95CI AE発生割合の信頼区間を出力するかをTRUE/FALSEで指定．TRUEは出力あり
  #' 
  
  # 全集団の集計
  N = dat %>% 
    select(all_of(subjid_var)) %>% 
    pull() %>% 
    unique() %>% 
    length()
  
  whole = dat %>% 
    filter(.data[[AE_name_var]] == AE_item) %>% 
    select(all_of(subjid_var)) %>% 
    pull() %>% 
    unique() %>% 
    length()
  
  whole_CI = binom.exact(whole, N, conf.level = 0.95)
  
  if(with_95CI){
    tmp = c(paste0(whole, ' (', round2(whole/N*100, 1), ')'), 
            paste0(round2(whole_CI$lower*100, 1), ", ", round2(whole_CI$upper*100, 1)))
  }else{
    tmp = c(paste0(whole, ' (', round2(whole/N*100, 1), ')'))
  }
  
  for (g in group_labels) {
    N = dat %>% 
      filter(.data[[group_var]] == g) %>% 
      select(all_of(subjid_var)) %>% 
      pull() %>% 
      unique() %>% 
      length()
    
    whole = dat %>% 
      filter(.data[[group_var]] == g) %>% 
      filter(.data[[AE_name_var]] == AE_item) %>% 
      select(all_of(subjid_var)) %>% 
      pull() %>% 
      unique() %>% 
      length()
    
    whole_CI = binom.exact(whole, N, conf.level = 0.95)
    
    if(with_95CI){
      tmp %<>% c(paste0(whole, ' (', round2(whole/N*100, 1), ')'), 
                 paste0(round2(whole_CI$lower*100, 1), ", ", round2(whole_CI$upper*100, 1)))
    }else{
      tmp %<>% c(paste0(whole, ' (', round2(whole/N*100, 1), ')'))
    }
  }
  tmp = t(c(AE_label, tmp)) %>% as.data.frame() 
  if(with_95CI){
    colnames(tmp) = c('AE name', 'Whole', "Whole CI", c(rbind(group_labels, paste(group_labels, "CI"))))
  }else{
    colnames(tmp) = c('AE name', 'Whole', group_labels)
  }
  return(rbind(res, tmp))
}

AE_each_item_by_cate = function(dat, group_var, group_labels, 
                                AE_name_var, AE_item, AE_label,
                                AE_cate_vars, AE_cate, 
                                res = res, subjid_var = 'SUBJID',
                                with_95CI = FALSE){
  #' 各AEを因果関係別や重篤性別，重症度別に集計する関数
  #' last update: 2025/3/5
  #' @param dat 使用するBLデータセット（基本的に1人1行）
  #' @param group_var 治療群などの群を表す変数
  #' @param group_labels 治療群に対するラベル．治療群に対するラベル．水準の順序（factor関数のlevels引数）と同じにする．
  #' @param AE_name_var 発現したAEの名称が含まれる変数名
  #' @param AE_item AE_name_varに含まれる集計したいAEの名称
  #' @param AE_label 結果出力時のTable上で表示されるAEの名称
  #' @param AE_cate_vars カテゴリ別に集計したい変数の変数名（e.g., 因果関係や重症度）
  #' @param AE_cate AE_cate_varsで指定した変数に含まれるカテゴリ
  #' @param res 結果を出力するオブジェクト
  #' @param subjid_var 各患者のID変数名
  #' @param with_95CI AE発生割合の信頼区間を出力するかをTRUE/FALSEで指定．TRUEは出力あり
  #' 
  
  tmp = NULL
  # 全集団の集計
  N = dat %>% 
    select(all_of(subjid_var)) %>% 
    pull() %>% 
    unique() %>% 
    length()
  cate_names = NULL
  for (i in 1:length(AE_cate_vars)) {
    cate_names %<>% c(paste('Whole', AE_cate[[i]]))
    for (j in 1:length(AE_cate[[i]])) {
      N_each_cate = dat %>% 
        filter(.data[[AE_name_var]] == AE_item) %>% 
        filter(.data[[AE_cate_vars[i]]] == AE_cate[[i]][j]) %>% 
        select(all_of(subjid_var)) %>% 
        pull() %>% 
        unique() %>% 
        length()
      
      CI = binom.exact(N_each_cate, N, conf.level = 0.95)
      
      if(with_95CI){
        tmp %<>% c(paste0(N_each_cate, ' (', round2(N_each_cate/N*100, 1), ')'), 
                   paste0(round2(CI$lower*100, 1), ", ", round2(CI$upper*100, 1)))
      }else{
        tmp %<>% c(paste0(N_each_cate, ' (', round2(N_each_cate/N*100, 1), ')'))
      }
      
    }
  }
  
  for (g in group_labels) {
    N = dat %>% 
      filter(.data[[group_var]] == g) %>% 
      select(all_of(subjid_var)) %>% 
      pull() %>% 
      unique() %>% 
      length()
    
    for (i in 1:length(AE_cate_vars)) {
      cate_names %<>% c(paste(g, AE_cate[[i]]))
      for (j in 1:length(AE_cate[[i]])) {
        N_each_cate = dat %>% 
          filter(.data[[group_var]] == g) %>% 
          filter(.data[[AE_name_var]] == AE_item) %>% 
          filter(.data[[AE_cate_vars[i]]] == AE_cate[[i]][j]) %>% 
          select(all_of(subjid_var)) %>% 
          pull() %>% 
          unique() %>% 
          length()
        CI = binom.exact(N_each_cate, N, conf.level = 0.95)
        
        if(with_95CI){
          tmp %<>% c(paste0(N_each_cate, ' (', round2(N_each_cate/N*100, 1), ')'), 
                     paste0(round2(CI$lower*100, 1), ", ", round2(CI$upper*100, 1)))
        }else{
          tmp %<>% c(paste0(N_each_cate, ' (', round2(N_each_cate/N*100, 1), ')'))
        }      }
    }
  }
  tmp = t(c(AE_label, tmp)) %>% as.data.frame() 
  if(with_95CI){
    colnames(tmp) = c('AE name', c(rbind(cate_names, paste(cate_names, "CI"))))
  }else{
    colnames(tmp) = c('AE name', cate_names)
  }
  return(rbind(res, tmp))
}

summarise_labo_cont = function(dat, group_var, group_labels,
                               var, item_name, res = res, 
                               summary_stat = c('n', 'mean_SD'),
                               timepoints, time_var,
                               subjid_var = 'SUBJID',
                               digits = 1){
  #' 臨床検査値の中の連続変数に対する集計関数
  #' last update: 2025/6/20
  #' @param dat 使用するデータセット
  #' @param group_var 治療群などの群を表す変数．単群の時はNULLを指定
  #' @param group_labels 治療群に対するラベル．単群の時はNULLを指定
  #' @param var 集計する連続変数の列名
  #' @param item_name 結果出力時の変数のラベル
  #' @param res 結果を出力するオブジェクト
  #' @param summary_stat 提示する要約統計量 'n', 'mean_SD', 'median_IQR', 'min_max'を組合せて指定
  #' @param timepoints 集計を行う検査値の測定時点
  #' @param time_var 測定時点を表す変数の変数名
  #' @param subjid_var 対象者個人を識別するためのIDの変数名
  #' @param digits 生データの小数点以下の桁数
  
  tmpres2 = NULL
  
  # group_labelsがNULLなら全体集計に切り替え
  if (is.null(group_labels)) {
    group_labels = "Total"
    dat$.__total_group = "Total"
    group_var = ".__total_group"
  }
  
  for (g in group_labels) {
    N = dat %>% 
      filter(.data[[group_var]] == g) %>% 
      select(all_of(subjid_var)) %>% 
      pull() %>% 
      unique() %>% 
      length()
    
    tmpres = NULL
    
    for (t in timepoints) {
      subres = dat %>% 
        rename(theitem = all_of(var)) %>% 
        filter(!is.na(theitem)) %>% 
        filter(.data[[group_var]] == g) %>% 
        filter(.data[[time_var]] == t) %>% 
        mutate(theitem = as.numeric(theitem)) %>% 
        summarise(n = n(),
                  mean = mean(theitem, na.rm = TRUE),
                  sd = sd(theitem, na.rm = TRUE),
                  median = median(theitem, na.rm = TRUE),
                  q25 = quantile(theitem, na.rm = TRUE, type = 2)["25%"],
                  q75 = quantile(theitem, na.rm = TRUE, type = 2)["75%"],
                  min = quantile(theitem, na.rm = TRUE, type = 2)["0%"],
                  max = quantile(theitem, na.rm = TRUE, type = 2)["100%"]) %>% 
        as.data.frame()
      
      subres %<>% mutate(
        n = paste0(n, " (", N - n, ")"),
        mean_SD = paste0(round2(mean, digits + 1), " (", round2(sd, digits + 1), ")"),
        median_IQR = paste0(round2(median, digits + 1), " (", round2(q25, digits), ", ", round2(q75, digits), ")"),
        min_max = paste0(format(min, nsmall = digits), ", ", format(max, nsmall = digits))
      )
      
      subres = subres[, summary_stat]
      subres = t(subres)
      subres = data.frame(stat_value = subres)
      
      if (is.null(tmpres)) {
        tmpres = subres
      } else {
        tmpres = cbind(tmpres, subres)
      }
    }
    
    tmpres = data.frame(
      arm = c(g, rep("", length(summary_stat) - 1)),
      category = summary_stat,
      tmpres
    )
    
    colnames(tmpres) = c("Arm", "Category", timepoints)
    tmpres2 %<>% bind_rows(tmpres)
  }
  
  res %<>% bind_rows(
    data.frame(
      item = c(item_name, rep("", length(group_labels) * length(summary_stat) - 1)),
      tmpres2
    )
  )
  
  return(res)
}

summarise_labo_discre = function(dat, group_var, group_labels, 
                                 var, item_name, labels, res = res, 
                                 timepoints, time_var, subjid_var = 'SUBJID'){
  #' 離散変数に対する集計関数
  #' last update: 2025/6/20
  #' @param dat 使用するデータセット．一人当たり複数時点数分の行数を許容
  #' @param group_var 治療群などの群を表す変数．単群の時はNULLを指定
  #' @param group_labels 治療群に対するラベル．単群の時はNULLを指定
  #' @param var 集計する離散変数の列名
  #' @param item_name 結果出力時の変数のラベル
  #' @param labels 離散変数の水準のラベル
  #' @param res 結果を出力するオブジェクト
  #' @param timepoints 集計を行う検査値の測定時点
  #' @param time_var 測定時点を表す変数の変数名
  #' @param subjid_var 対象者個人を識別するためのIDの変数名
  
  tmpres2 = NULL
  
  # group_labelsがNULLなら全体集計に切り替え
  if (is.null(group_labels)) {
    group_labels = "Total"
    dat$.__total_group = "Total"
    group_var = ".__total_group"
  }
  
  for (g in group_labels) {
    bunbo = dat %>% 
      filter(.data[[group_var]] == g) %>% 
      select(all_of(subjid_var)) %>% 
      pull() %>% 
      unique() %>% 
      length()
    
    tmpres = data.frame(Category = c(labels, "missing"))
    
    for (t in timepoints) {
      tmp = dat %>% 
        rename(theitem = all_of(var)) %>% 
        mutate(theitem = factor(theitem, levels = labels)) %>% 
        filter(.data[[group_var]] == g) %>% 
        filter(.data[[time_var]] == t)
      
      freq = table(tmp$theitem, useNA = "always")
      prop = round2(freq / bunbo * 100, 1)
      freqprop = paste0(freq, " (", format(prop, nsmall = 1), ")")
      
      subres = data.frame(stat_value = freqprop)
      tmpres = cbind(tmpres, setNames(subres, t))
    }
    
    tmpres = data.frame(
      Arm = c(g, rep("", nrow(tmpres) - 1)),
      tmpres
    )
    colnames(tmpres) = c("Arm", "Category", timepoints)
    tmpres2 %<>% bind_rows(tmpres)
  }
  
  res %<>% bind_rows(
    data.frame(
      item = c(item_name, rep("", length(group_labels) * (length(labels) + 1) - 1)),
      tmpres2
    )
  )
  
  return(res)
}

export_table = function(make_table, file_name = "dat.xlsx", r_begin = 3, c_begin = 2, 
                        sn = "sheet name", fig_name = NULL, add_table = FALSE, rowName = TRUE){
  #' make_tableをExcelファイルとして出力
  #' last update: 2024/11/14
  #' @param make_table 出力する対象となるRのオブジェクト
  #' @param file_name 出力先Excelのパス
  #' @param r_begin Excelシート内で出力を始める場所（何行目か）
  #' @param c_begin Excelシート内で出力を始める場所（何列目か）
  #' @param sn 出力するExcelシートのシート名
  #' @param fig_name 図をExcelシート内に出力する場合，その図のパス
  #' @param add_table FALSEの場合，新たなExcelとして作成．TRUEの場合，現状のfile_nameのExcelにシートを追加
  #' @param rowName TRUEの場合，make_tableの行名を出力．FALSEの場合，行名は出力なし．行名がない場合は行番号
  #' 
  
  tab = make_table
  row_begin = r_begin
  col_begin = c_begin
  sheetname = sn
  filename = file_name # prepare high resolution png or jpeg file (>720 dpi)
  figname = fig_name
  
  for (i in 1:ncol(tab)) {
    if(is.Date(tab[, i])){
      tab[, i] = as.character(tab[, i])
    }
  }
  
  if(add_table){
    wb = loadWorkbook(file_name)
    if(sn %in% getSheetNames(file_name)){
      removeWorksheet(wb = wb, sheet = sn)
    }
  }else{
    wb = createWorkbook() # create workbook
  }
  # add worksheet
  addWorksheet(wb, sheetname)
  # set font
  modifyBaseFont(wb, fontSize = 11, fontColour = "#000000", fontName = "Times New Roman")
  # set styles
  cellstyle = createStyle(fgFill = "#FFFFFF", valign = "center", halign = "left")
  idcolstyle = createStyle(fgFill = "#FFFFFF", valign = "center", halign = "center")
  horizontalstyle = createStyle(fgFill = "#FFFFFF", valign = "center", halign = "left", border = c('top'), borderStyle = 'medium')
  vertical_left = createStyle(fgFill = "#FFFFFF", valign = "center", halign = "left", border = c('left'), borderStyle = 'medium')
  vertical_right = createStyle(fgFill = "#FFFFFF", valign = "center", halign = "left", border = c('right'), borderStyle = 'medium')
  vertical_both = createStyle(fgFill = "#FFFFFF", valign = "center", halign = "left", border = c('left', 'right'), borderStyle = 'medium')
  lefttop = createStyle(fgFill = "#FFFFFF", valign = "center", halign = "left", border = c('top', 'bottom', 'left', 'right'), borderStyle = 'medium')
  rownamestyle = createStyle(fgFill = "#FFFFFF", valign = "center", halign = "left", border = c('top', 'bottom', 'left'), borderStyle = 'medium')
  horizontalthin = createStyle(fgFill = "#FFFFFF", border = c('bottom'), borderStyle = 'thin')
  # apply styles
  addStyle(wb = wb, sheet = sheetname, style = cellstyle, rows = 1:(nrow(tab) + row_begin + 1), cols = 1:(ncol(tab) + col_begin + 1), gridExpand = TRUE, stack = TRUE)
  addStyle(wb = wb, sheet = sheetname, style = horizontalstyle, rows = c(row_begin, row_begin + 1, (nrow(tab) + row_begin + 1)), cols = col_begin:(ncol(tab) + col_begin), gridExpand = TRUE, stack = TRUE)
  if(rowName){
    addStyle(wb = wb, sheet = sheetname, style = idcolstyle, rows = (row_begin + 1):(nrow(tab) + row_begin + 1), cols = col_begin + 1, gridExpand = TRUE, stack = TRUE)
    addStyle(wb = wb, sheet = sheetname, style = vertical_both, rows = row_begin:(nrow(tab) + row_begin), cols = col_begin, gridExpand = TRUE, stack = TRUE)
    addStyle(wb = wb, sheet = sheetname, style = vertical_left, rows = row_begin:(nrow(tab) + row_begin), cols = c(col_begin + ncol(tab) + 1), gridExpand = TRUE, stack = TRUE)
    addStyle(wb = wb, sheet = sheetname, style = lefttop, rows = row_begin, cols = col_begin, gridExpand = TRUE, stack = TRUE)
    addStyle(wb = wb, sheet = sheetname, style = rownamestyle, rows = row_begin, cols = col_begin + 1, gridExpand = TRUE, stack = TRUE)
  }else{
    addStyle(wb = wb, sheet = sheetname, style = idcolstyle, rows = (row_begin + 1):(nrow(tab) + row_begin + 1), cols = col_begin, gridExpand = TRUE, stack = TRUE)
    addStyle(wb = wb, sheet = sheetname, style = vertical_left, rows = row_begin:(nrow(tab) + row_begin), cols = c(col_begin + ncol(tab) + 1), gridExpand = TRUE, stack = TRUE)
    addStyle(wb = wb, sheet = sheetname, style = vertical_right, rows = row_begin:(nrow(tab) + row_begin), cols = c(col_begin - 1), gridExpand = TRUE, stack = TRUE)
    addStyle(wb = wb, sheet = sheetname, style = rownamestyle, rows = row_begin, cols = col_begin, gridExpand = TRUE, stack = TRUE)
  }
  if(nrow(tab) > 1){
    for (i in 1:(nrow(tab)-1)) {
      if(tab[i+1, 1] != ''){
        addStyle(wb = wb, sheet = sheetname, style = horizontalthin, rows = i + row_begin, cols = col_begin:(ncol(tab) + col_begin), gridExpand = TRUE, stack = TRUE)
      }
    }
  }
  
  # set columns width
  setColWidths(wb = wb, sheet = sheetname, cols = 1:(ncol(tab) + col_begin), widths = "auto")
  # no grid lines
  showGridLines(wb = wb, sheet = sheetname, showGridLines = FALSE)
  # write data
  writeData(wb, sheet = sheetname, x = tab, rowNames = rowName, xy = c(col_begin, row_begin))
  
  # if necessary, attach figure
  if(!is.null(figname)){
    insertImage(wb, file = figname, sheet = sheetname, startRow = row_begin, startCol = col_begin + ncol(tab) + 3, units = "cm", width = 18, height = 12)
  }
  
  # save excel file
  saveWorkbook(wb, filename, overwrite = TRUE)
}
