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

logistic_table_multivar = function(
    dat,
    covar_labels,            # 例: c(Treatment_Group="治療群", Age="年齢", Gender="性別", ...)
    level_labels = list(),   # 例: list(Gender = c(Male="男性", Female="女性"))
    outcome_var,             # 0/1 または 因子/文字（event_level で陽性側を指定）
    event_level = 1,         # outcome_var が因子/文字のとき「イベント」と見なす水準
    OR_CI_level = 0.95,      # OR の信頼水準
    return_gt = TRUE         # TRUE: gt テーブル, FALSE: data.frame
){
  #' Multivariable Logistic model table (image-like layout)
  #' - Continuous vars -> 1 row (per 1-unit increase)
  #' - Categorical vars -> ref row + rows for each non-ref level
  #' @param dat 使用するデータフレーム（1行=1症例）
  #' @param covar_labels 解析に入れる説明変数の"名前付きベクトル"（var名=表示名）
  #' @param level_labels 各因子の"名前付きベクトル"を入れたリスト。
  #'   ベクトルの「名前=元レベル値」「値=表示ラベル」。先頭要素が基準水準(ref)。
  #' @param outcome_var 目的変数の列名（0/1 または 因子/文字列）
  #' @param event_level outcome_var が因子/文字列のとき「イベント」と見なす水準
  #' @param OR_CI_level OR の信頼水準 (0–1)。既定 0.95
  #' @param return_gt TRUE で gt テーブル、FALSE で data.frame を返す
  #' @return gt テーブル（もしくは data.frame）
  
  req_pkgs = c("broom", "dplyr", "stringr", "rlang", "stats")
  miss = req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("Missing packages: ", paste(miss, collapse = ", "))
  `%>%` = get("%>%", asNamespace("dplyr"))
  `%||%` = function(a, b) if (!is.null(a)) a else b
  
  dat2 = dat
  
  # --- 目的変数を 0/1 に整形 ---
  y = dat2[[outcome_var]]
  if (is.logical(y)) {
    y_bin = as.integer(y)
  } else if (is.numeric(y)) {
    # 0/1 以外が混じっていれば警告
    uy = unique(na.omit(y))
    if (!all(uy %in% c(0,1))) warning("outcome_var has values other than 0/1; treating nonzero as 1.")
    y_bin = as.integer(y != 0)
  } else {
    # 因子/文字列 → event_level を 1、それ以外 0
    y_bin = as.integer(as.character(y) == as.character(event_level))
}
  dat2[[".y"]] = y_bin

  # --- 因子の水準順・ラベルの指定 ---
  disp_levels = list()
  raw_levels  = list()
  for (v in names(covar_labels)) {
    x = dat2[[v]]
    ll = level_labels[[v]]
    if (!is.null(ll)) {
      if (is.null(names(ll)))
        stop("level_labels[['", v, "']] must be a *named* character vector: names=raw levels, values=labels.")
      raw_levels[[v]]  = names(ll)
      disp_levels[[v]] = unname(ll)
      dat2[[v]] = factor(as.character(x), levels = raw_levels[[v]])
    } else {
      if (is.character(x) || is.factor(x)) {
        dat2[[v]] = factor(x)
        raw_levels[[v]]  = levels(dat2[[v]])
        disp_levels[[v]] = raw_levels[[v]]
      }
    }
  }
  
  # --- フォーミュラ作成 & モデル当て ---
  backtick = function(s) paste0("`", s, "`")
  rhs = paste(backtick(names(covar_labels)), collapse = " + ")
  fml = stats::as.formula(paste0(".y ~ ", rhs))
  
  fit = stats::glm(fml, data = dat2, family = stats::binomial(link = "logit"), na.action = stats::na.omit)
  
  td = broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE, conf.level = OR_CI_level)
  mm_terms = colnames(stats::model.matrix(fit))
  mm_terms = mm_terms[mm_terms != "(Intercept)"]
  
  fmt_num = function(x, d = 3) ifelse(is.na(x), "", formatC(x, format = "f", digits = d))
  fmt_p   = function(p) ifelse(is.na(p), "",
                                ifelse(p < 0.001, "<.001",
                                       formatC(round(p, 3), format = "f", digits = 3)))
  term_for = function(v, lev = NULL) {
    if (is.null(lev)) make.names(v) else make.names(paste0(v, lev))
  }
  make_orci = function(est, lo, hi, d = 3){
    if (any(is.na(c(est, lo, hi)))) "" else
      paste0(fmt_num(est, d), " (", fmt_num(lo, d), ", ", fmt_num(hi, d), ")")
  }
  
  out_rows = list()
  for (v in names(covar_labels)) {
    v_label = covar_labels[[v]]
    x = dat2[[v]]
    
    if (is.factor(x)) {
      levs_raw  = levels(x)
      levs_disp = (disp_levels[[v]] %||% levs_raw)
      # ref 行
      out_rows[[length(out_rows)+1]] = data.frame(
        項目 = v_label, 値 = levs_disp[1],
        OR_95CI = "ref", p_value = "", stringsAsFactors = FALSE
      )
      # 非ref 行
      if (length(levs_raw) >= 2) {
        for (k in 2:length(levs_raw)) {
          trm = term_for(v, levs_raw[k])
          row = td[match(trm, td$term), ]
          out_rows[[length(out_rows)+1]] = data.frame(
            項目 = "", 値 = levs_disp[k],
            OR_95CI = make_orci(row$estimate, row$conf.low, row$conf.high, d = 3),
            p_value = fmt_p(row$p.value),
            stringsAsFactors = FALSE
          )
        }
      }
    } else {
      # 連続変数
      trm = term_for(v, NULL)
      row = td[match(trm, td$term), ]
      out_rows[[length(out_rows)+1]] = data.frame(
        項目 = v_label, 値 = "",
        OR_95CI = make_orci(row$estimate, row$conf.low, row$conf.high, d = 3),
        p_value = fmt_p(row$p.value),
        stringsAsFactors = FALSE
      )
    }
  }
  
  res_df = dplyr::bind_rows(out_rows)
  if (!return_gt) return(res_df)
  
  res_df |>
    gt::gt() |>
    gt::cols_label(
      項目    = "項目",
      値      = "値",
      OR_95CI = "OR (95% 信頼区間)",
      p_value = "p-値"
    ) |>
    gt::tab_options(
      table.font.size = gt::px(12),
      data_row.padding = gt::px(4)
    ) |>
    gt::cols_align(align = "center", columns = c(OR_95CI, p_value)) |>
    gt::cols_align(align = "left",   columns = c(項目, 値))
}

survival_analysis = function(dat, group_var, group_labels, 
                             event_var, time_var,
                             max_obs_time,
                             HR_CI_level = 0.95){
  #' 生存時間変数に対する解析を実施する関数
  #' last update: 2025/6/20
  #' @param dat 使用するデータセット（1人1行）
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

cox_table_multivar = function(
    dat,
    covar_labels,            # 例: c(Treatment_Group="治療手法", Age="年齢", Gender="性別", ...)
    level_labels = list(),   # 例: list(Gender = c(Female="F (女性)", Male="M (男性)"))
    event_var,               # 例: "Event"  (1=event, 0=censor)
    time_var,                # 例: "Observed_Time"
    max_obs_time = NULL,     # 例: 90  (NULL なら変更しない)
    HR_CI_level = 0.95,      # 例: 0.95
    return_gt = TRUE         # TRUE で gt テーブルを返す。FALSE で data.frame を返す
){
  #' Multivariable Cox model table (image-like layout)
  #' - Continuous vars -> 1 row (per 1-unit increase)
  #' - Categorical vars -> ref row + rows for each non-ref level
  #' @param dat 使用するデータフレーム（1行=1症例）
  #' @param covar_labels 解析に入れる説明変数の"名前付きベクトル"（var名=表示名）
  #' @param level_labels 各因子の"名前付きベクトル"を入れたリスト。
  #'   ベクトルの「名前=元レベル値」「値=表示ラベル」。先頭要素が基準水準(ref)。
  #' @param event_var イベント指標(1=イベント, 0=打ち切り)の列名（文字列）
  #' @param time_var  イベント/打ち切り時刻の列名（文字列）
  #' @param max_obs_time 観察の最大時点。超過は時刻を切り詰め event=0 に変更（NULLなら無効）
  #' @param HR_CI_level HR の信頼水準 (0–1)。既定 0.95
  #' @param return_gt TRUE で gt テーブル、FALSE で data.frame を返す
  #' @return gt テーブル（もしくは data.frame）

  req_pkgs = c("survival", "broom", "dplyr", "stringr", "rlang")
  miss = req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("Missing packages: ", paste(miss, collapse = ", "))
  `%>%` = get("%>%", asNamespace("dplyr"))
  `%||%` = function(a, b) if (!is.null(a)) a else b
  
  # --- 事前整形（管理打ち切りの適用） ---
  dat2 = dat
  tvec = dat2[[time_var]]
  evec = dat2[[event_var]]
  if (!is.null(max_obs_time)) {
    dat2[[".time"]]  = pmin(tvec, max_obs_time)
    dat2[[".event"]] = ifelse(tvec > max_obs_time, 0L, evec)
  } else {
    dat2[[".time"]]  = tvec
    dat2[[".event"]] = evec
  }
  
  # --- 因子の水準順・ラベルの指定 ---
  disp_levels = list()
  raw_levels  = list()
  for (v in names(covar_labels)) {
    x = dat2[[v]]
    ll = level_labels[[v]]
    if (!is.null(ll)) {
      if (is.null(names(ll)))
        stop("level_labels[['", v, "']] must be a *named* character vector: names=raw levels, values=labels.")
      raw_levels[[v]]  = names(ll)
      disp_levels[[v]] = unname(ll)
      dat2[[v]] = factor(as.character(x), levels = raw_levels[[v]])
    } else {
      if (is.character(x) || is.factor(x)) {
        dat2[[v]] = factor(x)
        raw_levels[[v]]  = levels(dat2[[v]])
        disp_levels[[v]] = raw_levels[[v]]
      }
    }
  }
  
  # --- フォーミュラ作成 & モデル当て ---
  backtick = function(s) paste0("`", s, "`")
  rhs = paste(backtick(names(covar_labels)), collapse = " + ")
  fml = stats::as.formula(paste0("survival::Surv(.time, .event) ~ ", rhs))
  fit = survival::coxph(fml, data = dat2, ties = "efron", model = TRUE)
  
  td = broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE, conf.level = HR_CI_level)
  mm_terms = colnames(stats::model.matrix(fit))
  mm_terms = mm_terms[mm_terms != "(Intercept)"]
  
  fmt_num = function(x, d = 3) ifelse(is.na(x), "", formatC(x, format = "f", digits = d))
  fmt_p   = function(p) ifelse(is.na(p), "",
                                ifelse(p < 0.001, "<.001",
                                       formatC(round(p, 3), format = "f", digits = 3)))
  term_for = function(v, lev = NULL) {
    if (is.null(lev)) make.names(v) else make.names(paste0(v, lev))
  }
  make_hrci = function(est, lo, hi, d = 3){
    if (any(is.na(c(est, lo, hi)))) "" else
      paste0(fmt_num(est, d), " (", fmt_num(lo, d), ", ", fmt_num(hi, d), ")")
  }
  
  out_rows = list()
  for (v in names(covar_labels)) {
    v_label = covar_labels[[v]]
    x = dat2[[v]]
    
    if (is.factor(x)) {
      levs_raw  = levels(x)
      levs_disp = (disp_levels[[v]] %||% levs_raw)
      # ref 行
      out_rows[[length(out_rows)+1]] = data.frame(
        項目 = v_label, 値 = levs_disp[1],
        HR_95CI = "ref", p_value = "", stringsAsFactors = FALSE
      )
      # 非ref 行
      if (length(levs_raw) >= 2) {
        for (k in 2:length(levs_raw)) {
          trm = term_for(v, levs_raw[k])
          row = td[match(trm, td$term), ]
          out_rows[[length(out_rows)+1]] = data.frame(
            項目 = "", 値 = levs_disp[k],
            HR_95CI = make_hrci(row$estimate, row$conf.low, row$conf.high, d = 3),
            p_value = fmt_p(row$p.value),
            stringsAsFactors = FALSE
          )
        }
      }
    } else {
      # 連続変数
      trm = term_for(v, NULL)
      row = td[match(trm, td$term), ]
      out_rows[[length(out_rows)+1]] = data.frame(
        項目 = v_label, 値 = "",
        HR_95CI = make_hrci(row$estimate, row$conf.low, row$conf.high, d = 3),
        p_value = fmt_p(row$p.value),
        stringsAsFactors = FALSE
      )
    }
  }
  
  res_df = dplyr::bind_rows(out_rows)
  if (!return_gt) return(res_df)
  
  res_df |>
    gt::gt() |>
    gt::cols_label(
      項目    = "項目",
      値      = "値",
      HR_95CI = "HR (95% 信頼区間)",
      p_value = "p-値"
    ) |>
    gt::tab_options(
      table.font.size = gt::px(12),
      data_row.padding = gt::px(4)
    ) |>
    gt::cols_align(align = "center", columns = c(HR_95CI, p_value)) |>
    gt::cols_align(align = "left",   columns = c(項目, 値))
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

make_cleaning_report = function(path,
                                sheet = NULL,
                                vars  = NULL,   
                                skip  = 0,
                                top_n = 200,
                                title = "Data Cleaning Report") {
  #' Data cleaning HTML report generator (single block)
  #' - Pass a file path (xlsx/csv/tsv/rds) and get an HTML report.
  #' - The report hides code and shows only tables.
  #' @param path 解析するデータファイルのパス。拡張子は xlsx/xls/csv/tsv/rds をサポート。
  #' @param sheet 読み込むシート名または番号。未指定なら先頭シートを使用。
  #' @param vars レポート対象の変数名ベクトル。NULL なら全変数。
  #' @param skip 先頭から何行スキップして読み込むか（readxl::read_xlsx の skip）。既定は 0。
  #' @param top_n 文字／因子列の頻度表で表示する上位件数。大きくすると表も長くなります。
  #' @param title 出力HTMLのタイトル。レポートの見出しに表示。
  
  # Dependencies
  pkgs = c("rmarkdown", "readxl",
           "janitor", "skimr", "gt",
           "lubridate", "scales")
  miss = pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("Missing packages: ", paste(miss, collapse = ", "))
  
  `%||%` = function(a, b) if (is.null(a)) b else a
  
  # Output path
  base = tools::file_path_sans_ext(basename(path))
  out  = file.path(dirname(path), paste0(base, "_cleaning_report.html"))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  
  # vars を Rmd に渡すための文字列を準備
  sel_vars_string = if (is.null(vars)) "NULL" else
    paste0("c(", paste(sprintf('\"%s\"', vars), collapse = ", "), ")")
  
  # Build Rmd as a character vector (copy-safe)
  tf  = tempfile(fileext = ".Rmd")
  rmd = c(
    "---",
    paste0("title: \"", title, "\""),
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_depth: 3",
    "    toc_float: true",
    "    theme: readable",
    "    code_folding: \"none\"",
    "    df_print: paged",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, results = 'asis')",
    "library(dplyr); library(tidyr); library(readr); library(readxl)",
    "library(janitor); library(skimr); library(gt); library(stringr)",
    "library(purrr); library(lubridate); library(scales)",
    "`%||%` = function(a, b) if (is.null(a)) b else a",
    paste0("in_path = \"", normalizePath(path, winslash = "/", mustWork = FALSE), "\""),
    if (is.null(sheet)) "sheet = NULL" else paste0("sheet = \"", sheet, "\""),
    paste0("top_n = ", top_n),
    paste0("skip = ", skip),
    paste0("sel_vars = ", sel_vars_string),  
    "```",
    "",
    "## 0. 入力の読み込み",
    "",
    "```{r}",
    "ext = tools::file_ext(in_path)",
    "if (ext %in% c('xlsx','xls')) {",
    "  data_raw  = readxl::read_xlsx(in_path, sheet = sheet %||% 1, skip = skip %||% 0, guess_max = 1e5)",
    "  orig_names = names(data_raw)",
    "  data      = data_raw |> janitor::clean_names()",
    "} else if (ext %in% c('csv','tsv')) {",
    "  delim = if (ext == 'tsv') '\\t' else ','",
    "  data_raw  = readr::read_delim(in_path, delim = delim, show_col_types = FALSE)",
    "  orig_names = names(data_raw)",
    "  data      = data_raw |> janitor::clean_names()",
    "} else if (ext == 'rds') {",
    "  data_raw = readRDS(in_path); if (!is.data.frame(data_raw)) stop('RDS is not a data frame')",
    "  orig_names = names(data_raw)",
    "  data      = data_raw |> janitor::clean_names()",
    "} else stop('Unsupported extension: ', ext)",
    "",
    "# ---- 名称正規化関数（`…`/改行/余分な空白を統一） ----",
    "normalize_nm <- function(x){",
    "  x <- gsub('^`|`$', '', x);            # 先頭/末尾のバッククォート除去",
    "  x <- gsub('\\r\\n|\\r', '\\n', x);    # CRLF/CR → LF",
    "  x <- stringr::str_replace_all(x, '\\\\s+', ' ');",
    "  x <- stringr::str_trim(x);",
    "  x",
    "}",
    "",
    "# 対応表（元名／正規化元名／clean後名）",
    "name_map <- tibble(",
    "  original      = orig_names,",
    "  original_norm = normalize_nm(orig_names),",
    "  cleaned       = names(data)",
    ")",
    "",
    "# --- ここで列選択（NULL なら全列） ---",
    "if (!is.null(sel_vars)) {",
    "  sel_norm    = normalize_nm(sel_vars)",                 # 文字列を正規化",
    "  keep_clean  = intersect(sel_vars, names(data))",       # すでに clean 名で渡された場合",
    "  keep_orig   = name_map$cleaned[name_map$original_norm %in% sel_norm]",
    "  keep        = unique(c(keep_clean, keep_orig))",
    "  missing_vars = sel_vars[!(sel_vars %in% names(data) | sel_norm %in% name_map$original_norm)]",
    "  if (length(keep) == 0) stop('指定した vars がデータに存在しません（元名/clean後いずれでも指定可）。')",
    "  data = dplyr::select(data, dplyr::all_of(keep))",
    "} else {",
    "  missing_vars = character(0)",
    "}",
    "",
    "# --- 表示用に列名を original に置換（以降の出力は original 名で表示） ---",
    "disp_map = name_map |>",
    "  dplyr::filter(cleaned %in% names(data)) |>",
    "  dplyr::select(cleaned, original)",
    "disp_map = disp_map[match(names(data), disp_map$cleaned), ]",
    "names(data) = disp_map$original",
    "```",
    "",
    "## 0.1 対象列の確認",
    "",
    "```{r}",
    "gt(name_map) |> tab_header(title = 'Original vs Cleaned column names')",
    "```",
    "```{r}",
    "tibble(target_variables = names(data)) |>",
    "  gt() |>",
    "  tab_header(title = 'Variables included in the report')",
    "```",
    "",
    "```{r}",
    "if (length(missing_vars) > 0) {",
    "  tibble(not_found = missing_vars) |>",
    "    gt() |>",
    "    tab_header(title = 'Requested but not found in data (after clean_names)')",
    "} ",
    "```",
    "",
    "## 1. データ概要",
    "",
    "```{r}",
    "tibble(file = basename(in_path), n_rows = nrow(data), n_cols = ncol(data),",
    "       approx_size_mb = as.numeric(object.size(data))/(1024^2)) |>",
    "  gt() |>",
    "  fmt_number(columns = approx_size_mb, decimals = 2)",
    "```",
    "",
    "## 2. 欠損の状況（変数別）",
    "",
    "```{r}",
    "miss_tbl = tibble(variable = names(data),",
    "                   n_missing = vapply(data, function(x) sum(is.na(x)), integer(1))) |>",
    "  mutate(missing_rate = n_missing / nrow(data)) |>",
    "  arrange(desc(missing_rate))",
    "gt(miss_tbl) |>",
    "  fmt_percent(columns = missing_rate, decimals = 1)",
    "```",
    "",
    "## 3. skim サマリー（wide 形式）",
    "",
    "```{r}",
    "skim_tbl = skimr::skim(data) |> dplyr::arrange(skim_type, skim_variable)",
    "gt(skim_tbl) |>",
    "  tab_options(table.font.size = px(12), data_row.padding = px(1))",
    "```",
    "",
    "## 4. 文字／因子の頻度表（少ない順）",
    "",
    "```{r}",
    "# 文字/因子 列を抽出（original名に置換後の data を想定）",
    "char_vars = names(data)[vapply(data, function(x) is.character(x) || is.factor(x), logical(1))]",
    "if (length(char_vars) == 0) {",
    "  cat('**文字／因子列はありません。**\\n')",
    "} else {",
    "  freq_tbl = purrr::map_dfr(char_vars, function(v) {",
    "    x = as.character(data[[v]])",
    "    dplyr::tibble(variable = v, level = x) |>",
    "      tidyr::replace_na(list(level = '(NA)')) |>",
    "      dplyr::count(variable, level, name = 'n') |>",
    "      dplyr::group_by(variable) |>",
    "      dplyr::mutate(prop = n / sum(n)) |>",
    "      dplyr::arrange(n, level, .by_group = TRUE) |>",   # ★ 小さい順に並べ替え（tie は level で）
    "      dplyr::slice_head(n = top_n) |>",                 # ★ 少ない方から top_n 件を表示
    "      dplyr::mutate(cumprop = cumsum(prop)) |>",        # 昇順で累積割合
    "      dplyr::ungroup()",
    "  })",
    "  gt(freq_tbl, groupname_col = 'variable') |>",
    "    fmt_percent(columns = c(prop, cumprop), decimals = 1) |>",
    "    cols_label(level = 'level', n = 'n', prop = 'prop(%)', cumprop = 'cum(%)') |>",
    "    tab_options(table.font.size = px(12), data_row.padding = px(1))",
    "}",
    "```",
    "",
    "## 5. 数値列の要約とチェック",
    "",
    "```{r}",
    "num_vars = names(data)[vapply(data, is.numeric, logical(1))]",
    "if (length(num_vars) == 0) {",
    "  cat('**数値列はありません。**\\n')",
    "} else {",
    "  num_summary = purrr::map_dfr(num_vars, function(v) {",
    "    x = data[[v]]",
    "    tibble(variable = v,",
    "           n = sum(!is.na(x)), n_missing = sum(is.na(x)),",
    "           mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),",
    "           min = suppressWarnings(min(x, na.rm = TRUE)),",
    "           p01 = suppressWarnings(quantile(x, 0.01, na.rm = TRUE)),",
    "           p05 = suppressWarnings(quantile(x, 0.05, na.rm = TRUE)),",
    "           p25 = suppressWarnings(quantile(x, 0.25, na.rm = TRUE)),",
    "           p50 = suppressWarnings(quantile(x, 0.50, na.rm = TRUE)),",
    "           p75 = suppressWarnings(quantile(x, 0.75, na.rm = TRUE)),",
    "           p95 = suppressWarnings(quantile(x, 0.95, na.rm = TRUE)),",
    "           p99 = suppressWarnings(quantile(x, 0.99, na.rm = TRUE)),",
    "           max = suppressWarnings(max(x, na.rm = TRUE)),",
    "           n_zero = sum(x == 0, na.rm = TRUE),",
    "           n_negative = sum(x < 0, na.rm = TRUE),",
    "           n_unique = dplyr::n_distinct(x, na.rm = TRUE),",
    "           n_nonfinite = sum(!is.finite(x)))",
    "  })",
    "  gt(num_summary) |>",
    "    fmt_number(columns = where(is.numeric), decimals = 3)",
    "}",
    "```",
    "",
    "## 6. 日付／日時列の範囲",
    "",
    "```{r}",
    "is_dateish = function(x) inherits(x, 'Date') || inherits(x, 'POSIXt')",
    "date_vars = names(data)[vapply(data, is_dateish, logical(1))]",
    "if (length(date_vars) == 0) {",
    "  cat('**Date / POSIXt 列はありません。**\\n')",
    "} else {",
    "  date_summary = purrr::map_dfr(date_vars, function(v) {",
    "    x = data[[v]]",
    "    tibble(variable = v, n_missing = sum(is.na(x)),",
    "           min = suppressWarnings(min(x, na.rm = TRUE)),",
    "           p25 = suppressWarnings(quantile(x, 0.25, na.rm = TRUE)),",
    "           median = suppressWarnings(median(x, na.rm = TRUE)),",
    "           p75 = suppressWarnings(quantile(x, 0.75, na.rm = TRUE)),",
    "           max = suppressWarnings(max(x, na.rm = TRUE)))",
    "  })",
    "  gt(date_summary)",
    "}",
    "```",
    "",
    "## 7. 文字列の“怪しい値”検出",
    "",
    "```{r}",
    "if (length(char_vars) > 0) {",
    "  suspicious_tokens = c('', '.', '-', '--', 'NA', 'N/A', 'na', 'Na', '不明', '？', '不詳')",
    "  char_checks = purrr::map_dfr(char_vars, function(v) {",
    "    x = as.character(data[[v]])",
    "    tibble(variable = v,",
    "           n_trim_needed = sum(stringr::str_detect(x, '^\\\\s+|\\\\s+$'), na.rm = TRUE),",
    "           n_suspicious  = sum(x %in% suspicious_tokens, na.rm = TRUE),",
    "           n_blank_like  = sum(stringr::str_trim(x) == '' & !is.na(x)))",
    "  }) |>",
    "    arrange(desc(n_trim_needed + n_suspicious + n_blank_like))",
    "  gt(char_checks)",
    "}",
    "```"
  )
  writeLines(rmd, tf)
  
  # Render
  rmarkdown::render(
    tf,
    output_file = basename(out),
    output_dir  = dirname(out),
    quiet = TRUE,
    encoding = "UTF-8"
  )
  message("HTML report written: ", out)
  invisible(out)
}
