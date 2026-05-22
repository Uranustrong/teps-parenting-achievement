# =============================================================================
# Phase 4：迴歸分析（要求 D、回應 R → 學業成就 w2all3p）
# -----------------------------------------------------------------------------
# 讀 dr_scoring.R 產出的 analysis_data.rds。依變項 = w2all3p（綜合分析能力 IRT 分數），
# 以線性迴歸分析。
#
# 分析內容：
#   1. 全體模型：score ~ D + R（加法）、score ~ D * R（含交互）。
#   2. 迴歸預測熱圖：由 D*R 模型在 D×R 平面上的預測曲面。
#   3. 分層迴歸：固定城鄉 / 公私立 / 偏遠後，比較各層的截距與 D、R、D×R 斜率，
#      並以巢狀 F 檢定正式檢定「截距是否差」與「斜率是否隨該變項改變」。
#
# D_c、R_c 為「全體平均置中」後的分數。置中後：交互模型的 D_c、R_c 主效應 =「對方在
# 平均值時的斜率」，截距 =「在平均 D、平均 R 時的預測成就」。各分層共用同一置中基準，
# 截距才能跨層比較。
#
# 註：本檔以「# ---- 英數標籤 ----」標記區段，與其他腳本一致。
# =============================================================================

# ---- setup ----
# 工作目錄須為專案根目錄（含 Data/ 與 教養分析/）：由報告 .Rmd 以 source()
# 呼叫時已設定好；單獨執行請先 setwd() 到專案根目錄。
library(ggplot2)
library(dplyr)
library(tidyr)

base    <- "教養分析"
out_dir <- file.path(base, "outputs", "regression")
fig_dir <- file.path(base, "outputs", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

font_family <- "Heiti TC"
png_device  <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
theme_set(theme_minimal(base_size = 13, base_family = font_family) +
  theme(plot.title.position = "plot", panel.grid.minor = element_blank()))
save_plot <- function(p, file, w = 9, h = 5.5)
  ggsave(file.path(fig_dir, file), p, width = w, height = h, dpi = 180,
         device = png_device, bg = "white")

# ---- data ----
# 讀分析資料、剔除 w2all3p 遺漏者；置中 D、R；結構變項轉成有標籤的因子。
ana <- readRDS(file.path(base, "outputs", "dr_scoring", "analysis_data.rds"))$data
dat <- ana[!is.na(ana$w2all3p), ]
D_mean <- mean(dat$D_score)
R_mean <- mean(dat$R_score)
dat$D_c <- dat$D_score - D_mean
dat$R_c <- dat$R_score - R_mean
dat$urban <- factor(dat$w2urban3, levels = c(1, 2, 3),
                    labels = c("鄉村", "城鎮", "都市"))
dat$priv   <- factor(dat$w2priv, levels = c(0, 1), labels = c("公立", "私立"))
dat$far    <- factor(dat$w2far,  levels = c(0, 1), labels = c("非偏遠", "偏遠"))
dat$gender <- factor(dat$w2s445, levels = c(1, 2), labels = c("男", "女"))
achv_mean <- mean(dat$w2all3p)
N <- nrow(dat)

# ---- overall-models ----
# 全體：加法模型與交互模型。
M_add <- lm(w2all3p ~ D_c + R_c, data = dat)
M_int <- lm(w2all3p ~ D_c * R_c, data = dat)

coef_tab <- function(m, label) {
  s <- summary(m)$coefficients
  data.frame(模型 = label, 項 = rownames(s),
             估計 = s[, 1], 標準誤 = s[, 2], t = s[, 3], p = s[, 4],
             row.names = NULL)
}
overall_coef <- rbind(coef_tab(M_add, "D+R 加法模型"),
                      coef_tab(M_int, "D*R 交互模型"))
overall_fit <- data.frame(
  模型  = c("D+R 加法模型", "D*R 交互模型"),
  R2    = c(summary(M_add)$r.squared,     summary(M_int)$r.squared),
  adjR2 = c(summary(M_add)$adj.r.squared, summary(M_int)$adj.r.squared))
aov_int <- anova(M_add, M_int)   # 交互項是否顯著改善模型
write.csv(overall_coef, file.path(out_dir, "coef_overall.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(overall_fit,  file.path(out_dir, "fit_overall.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- regression-heatmap ----
# 由 D*R 交互模型，在 0–5 的 D×R 網格上預測學業成就，畫成預測曲面熱圖。
gv   <- seq(0, 5, 0.1)
grid <- expand.grid(D = gv, R = gv)
grid$D_c <- grid$D - D_mean
grid$R_c <- grid$R - R_mean
grid$pred <- predict(M_int, newdata = grid)
pA <- ggplot(grid, aes(D, R)) +
  geom_raster(aes(fill = pred)) +
  geom_contour(aes(z = pred), color = "white", alpha = 0.55, linewidth = 0.3) +
  scale_fill_gradient2(low = "#C53030", mid = "#FEFCBF", high = "#2C5282",
                       midpoint = achv_mean, name = "預測\nw2all3p") +
  coord_fixed() +
  scale_x_continuous(breaks = 0:5) + scale_y_continuous(breaks = 0:5) +
  labs(title = "迴歸預測：學業成就在 D × R 平面上的預測曲面",
       subtitle = sprintf("模型 w2all3p ~ D * R；adj R² = %.4f", summary(M_int)$adj.r.squared),
       x = "要求 D_score", y = "回應 R_score")
save_plot(pA, "07_regression_heatmap.png", 8, 7)

# ---- stratified-coef ----
# 固定結構變項，於各層內各自配適 D*R 模型，取出截距與 D、R、D×R 斜率。
strat_one <- function(svar, sname) {
  do.call(rbind, lapply(levels(dat[[svar]]), function(L) {
    sub <- dat[dat[[svar]] == L, ]
    co  <- summary(lm(w2all3p ~ D_c * R_c, data = sub))$coefficients
    data.frame(分層變項 = sname, 層級 = L, n = nrow(sub),
               截距 = co["(Intercept)", 1], 截距p = co["(Intercept)", 4],
               beta_D = co["D_c", 1],       D_p  = co["D_c", 4],
               beta_R = co["R_c", 1],       R_p  = co["R_c", 4],
               beta_DR = co["D_c:R_c", 1],  DR_p = co["D_c:R_c", 4],
               adjR2 = summary(lm(w2all3p ~ D_c * R_c, data = sub))$adj.r.squared,
               row.names = NULL)
  }))
}
strat_coef <- rbind(strat_one("urban",  "城鄉"),
                    strat_one("priv",   "公私立"),
                    strat_one("far",    "偏遠"),
                    strat_one("gender", "性別"))
write.csv(strat_coef, file.path(out_dir, "coef_stratified.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- slope-tests ----
# 巢狀 F 檢定。對每個結構變項 S：
#   截距是否差：比較 (D*R) vs (D*R + S)         —— S 是否使 baseline 移動。
#   斜率是否差：比較 (D*R + S) vs (D*R * S)     —— S 是否調節 D、R、D×R 斜率。
slope_test <- function(svar, sname) {
  mA <- lm(as.formula(sprintf("w2all3p ~ D_c * R_c + %s", svar)), data = dat)
  mB <- lm(as.formula(sprintf("w2all3p ~ D_c * R_c * %s", svar)), data = dat)
  a_int   <- anova(M_int, mA)
  a_slope <- anova(mA, mB)
  data.frame(分層變項 = sname,
             截距差_F = a_int$F[2],   截距差_p = a_int$`Pr(>F)`[2],
             斜率差_F = a_slope$F[2], 斜率差_p = a_slope$`Pr(>F)`[2])
}
slope_tests <- rbind(slope_test("urban",  "城鄉"),
                     slope_test("priv",   "公私立"),
                     slope_test("far",    "偏遠"),
                     slope_test("gender", "性別"))
write.csv(slope_tests, file.path(out_dir, "slope_tests.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- fig-stratified-heatmap ----
# 依城鄉分層的迴歸預測曲面（三層共用色階）：整體偏藍 / 紅反映截距差；
# 面內梯度型態反映 D、R 斜率。
gridU <- do.call(rbind, lapply(levels(dat$urban), function(L) {
  m <- lm(w2all3p ~ D_c * R_c, data = dat[dat$urban == L, ])
  g <- expand.grid(D = gv, R = gv)
  g$D_c <- g$D - D_mean; g$R_c <- g$R - R_mean
  g$pred <- predict(m, newdata = g); g$urban <- L; g
}))
gridU$urban <- factor(gridU$urban, levels = c("鄉村", "城鎮", "都市"))
pB <- ggplot(gridU, aes(D, R)) +
  geom_raster(aes(fill = pred)) +
  geom_contour(aes(z = pred), color = "white", alpha = 0.55, linewidth = 0.3) +
  facet_wrap(~urban) +
  scale_fill_gradient2(low = "#C53030", mid = "#FEFCBF", high = "#2C5282",
                       midpoint = achv_mean, name = "預測\nw2all3p") +
  coord_fixed() +
  scale_x_continuous(breaks = 0:5) + scale_y_continuous(breaks = 0:5) +
  labs(title = "迴歸預測曲面：依城鄉分層",
       subtitle = "三層共用色階——整體偏藍/紅反映截距(baseline)差異；面內梯度型態反映 D、R 斜率",
       x = "要求 D_score", y = "回應 R_score")
save_plot(pB, "08_regression_heatmap_by_urban.png", 13, 5.2)

# ---- fig-coef-compare ----
# 各分層的迴歸係數（截距、D、R、D×R 斜率）+ 95% CI，並排比較。
collect <- function(m, label, group) {
  ci <- confint(m); co <- summary(m)$coefficients
  tm <- c("(Intercept)", "D_c", "R_c", "D_c:R_c")
  data.frame(model = label, group = group, term = tm,
             est = co[tm, 1], lo = ci[tm, 1], hi = ci[tm, 2], row.names = NULL)
}
per_level <- function(svar, group) do.call(rbind, lapply(levels(dat[[svar]]),
  function(L) collect(lm(w2all3p ~ D_c * R_c, data = dat[dat[[svar]] == L, ]), L, group)))
cc <- rbind(collect(M_int, "全體", "全體"),
            per_level("urban",  "城鄉"),
            per_level("priv",   "公私立"),
            per_level("far",    "偏遠"),
            per_level("gender", "性別"))
cc$term  <- factor(cc$term, levels = c("(Intercept)", "D_c", "R_c", "D_c:R_c"),
                   labels = c("截距 β₀", "要求斜率 β_D", "回應斜率 β_R", "交互 β_DR"))
cc$model <- factor(cc$model, levels = rev(c("全體", "鄉村", "城鎮", "都市",
                                            "公立", "私立", "非偏遠", "偏遠",
                                            "男", "女")))
cc$group <- factor(cc$group, levels = c("全體", "城鄉", "公私立", "偏遠", "性別"))
pC <- ggplot(cc, aes(est, model, color = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55") +
  geom_pointrange(aes(xmin = lo, xmax = hi), linewidth = 0.6) +
  facet_wrap(~term, scales = "free_x", nrow = 1) +
  scale_color_manual(values = c("全體" = "#1A202C", "城鄉" = "#2B6CB0",
                                "公私立" = "#C05621", "偏遠" = "#38A169",
                                "性別" = "#805AD5"),
                     name = "分層") +
  labs(title = "各分層的迴歸係數比較（點為估計、線為 95% CI）",
       subtitle = "截距 β₀ 隨城鄉 / 公私立大幅移動；D、R、D×R 斜率在各層相對穩定",
       x = "係數估計", y = NULL)
save_plot(pC, "09_coef_comparison.png", 13, 6)

# ---- fig-slope-lines ----
# 圖 10：分層迴歸線——把每個結構變項拆開、各層各畫一條迴歸線。
#   「要求 D」列：回應 R 固定於平均；「回應 R」列：要求 D 固定於平均。
#   線平行 = 該變項只移截距、不調節斜率；線扇開 = 調節斜率。
make_lines <- function(svar, sname) {
  do.call(rbind, lapply(levels(dat[[svar]]), function(L) {
    m <- lm(w2all3p ~ D_c * R_c, data = dat[dat[[svar]] == L, ])
    rbind(
      data.frame(structural = sname, level = L,
                 predictor = "要求 D（回應固定於平均）", x = gv,
                 pred = predict(m, data.frame(D_c = gv - D_mean, R_c = 0))),
      data.frame(structural = sname, level = L,
                 predictor = "回應 R（要求固定於平均）", x = gv,
                 pred = predict(m, data.frame(D_c = 0, R_c = gv - R_mean))))
  }))
}
lines_df <- rbind(make_lines("urban",  "城鄉"),
                  make_lines("priv",   "公私立"),
                  make_lines("far",    "偏遠"),
                  make_lines("gender", "性別"))
lines_df$structural <- factor(lines_df$structural,
  levels = c("城鄉", "公私立", "偏遠", "性別"))
lines_df$level <- factor(lines_df$level,
  levels = c("鄉村", "城鎮", "都市", "公立", "私立", "非偏遠", "偏遠", "男", "女"))
line_cols <- c("鄉村" = "#90CDF4", "城鎮" = "#3182CE", "都市" = "#1A365D",
               "公立" = "#F6AD55", "私立" = "#9C4221",
               "非偏遠" = "#9AE6B4", "偏遠" = "#22543D",
               "男" = "#2C7A7B", "女" = "#D53F8C")
pD <- ggplot(lines_df, aes(x, pred, color = level)) +
  geom_line(linewidth = 1.1) +
  facet_grid(predictor ~ structural) +
  scale_color_manual(values = line_cols, name = "分層") +
  labs(title = "分層迴歸線：斜率平行還是扇開？",
       subtitle = "每條線為該層的迴歸預測——線平行 = 該變項只移截距；扇開 = 調節斜率",
       x = "分數（0–5）", y = "預測 w2all3p") +
  theme(legend.position = "right")
save_plot(pD, "10_slopes_by_group.png", 15, 6.6)

# ---- anova-typology ----
# 四類型單因子 ANOVA + Tukey 事後比較（面向理論的補充分析）。
# ANOVA 是迴歸的特例：aov(w2all3p ~ Type) 的 omnibus F 等於 lm(w2all3p ~ Type)。
aov_type <- aov(w2all3p ~ Type, data = dat)
aov_tab  <- summary(aov_type)[[1]]
lm_type  <- lm(w2all3p ~ Type, data = dat)          # 對照組：與 aov 為同一模型
tuk <- as.data.frame(TukeyHSD(aov_type)$Type)
names(tuk) <- c("diff", "lwr", "upr", "p_adj")
tuk$pair <- rownames(tuk)
tm <- aggregate(w2all3p ~ Type, dat,
                function(x) c(n = length(x), mean = mean(x), sd = sd(x)))
type_means <- data.frame(Type = tm$Type, n = tm$w2all3p[, "n"],
                         mean = tm$w2all3p[, "mean"], sd = tm$w2all3p[, "sd"])
write.csv(tuk[, c("pair", "diff", "lwr", "upr", "p_adj")],
          file.path(out_dir, "anova_tukey.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(type_means, file.path(out_dir, "anova_type_means.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 圖 11：Tukey 六對兩兩比較（平均差 ± 95% 同時信賴區間）。
tuk$sig      <- ifelse(tuk$p_adj < .05, "顯著 (p<.05)", "不顯著")
tuk$pair_lab <- factor(tuk$pair, levels = tuk$pair[order(tuk$diff)])
pE <- ggplot(tuk, aes(diff, pair_lab, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55") +
  geom_pointrange(aes(xmin = lwr, xmax = upr), linewidth = 0.8) +
  scale_color_manual(values = c("顯著 (p<.05)" = "#C05621", "不顯著" = "#718096"),
                     name = NULL) +
  labs(title = "四類型學業成就的 Tukey 事後兩兩比較",
       subtitle = "點為兩類型 w2all3p 平均差、線為 95% 同時信賴區間（CI 跨 0 = 不顯著）",
       x = "w2all3p 平均差", y = NULL)
save_plot(pE, "11_tukey_pairs.png", 9, 4.6)

# ---- chisq-typology ----
# 四類型 × 成就四分位的卡方獨立性檢定（理論面補充的「類別版」，與上方 ANOVA 對照）。
# ANOVA 比四類型的成就「平均」；卡方檢定「教養類型」與「成就高低類別」這兩個類別變項
# 是否獨立。與本節 ANOVA 一致採未加權；w2all3p 依四分位切成 Q1 低 ~ Q4 高四組。
qb <- quantile(dat$w2all3p, probs = 0:4 / 4)
dat$achv_q <- cut(dat$w2all3p, breaks = qb, include.lowest = TRUE,
                  labels = c("Q1 低", "Q2", "Q3", "Q4 高"))
chi_tab  <- table(教養類型 = dat$Type, 成就四分位 = dat$achv_q)  # 4×4 次數列聯表
chi_test <- chisq.test(chi_tab)                                   # Pearson 卡方獨立性檢定
# Cramér's V：卡方的效果量（n 大時卡方對微小偏離也會顯著，用 V 表關聯「強度」）。
chi_V <- sqrt(as.numeric(chi_test$statistic) /
              (sum(chi_tab) * (min(dim(chi_tab)) - 1)))
chi_rowpct <- prop.table(chi_tab, margin = 1) * 100               # 列百分比（每列加總 100%）
chi_resid  <- chi_test$stdres                                     # 標準化殘差（|.|>2 約 p<.05）
chi_summary <- data.frame(
  統計量 = c("卡方 χ²", "自由度 df", "p 值", "Cramér's V", "n"),
  值      = c(sprintf("%.2f", chi_test$statistic),
              sprintf("%d",   chi_test$parameter),
              sprintf("%.3e", chi_test$p.value),
              sprintf("%.4f", chi_V),
              sprintf("%d",   sum(chi_tab))))
write.csv(as.data.frame.matrix(chi_tab),
          file.path(out_dir, "chisq_table.csv"), fileEncoding = "UTF-8")
write.csv(round(as.data.frame.matrix(chi_rowpct), 2),
          file.path(out_dir, "chisq_rowpct.csv"), fileEncoding = "UTF-8")
write.csv(round(as.data.frame.matrix(chi_resid), 3),
          file.path(out_dir, "chisq_residuals.csv"), fileEncoding = "UTF-8")
write.csv(chi_summary, file.path(out_dir, "chisq_summary.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 圖 13：4×4 標準化殘差熱圖（藍 = 高於獨立期望、紅 = 低於期望；|殘差| > 2 約 p<.05）。
resid_df <- as.data.frame(as.table(chi_resid))
names(resid_df) <- c("Type", "achv_q", "resid")
resid_df$Type   <- factor(resid_df$Type,
  levels = rev(c("開明權威", "專制權威", "寬鬆放任", "忽視冷漠")))
resid_df$achv_q <- factor(resid_df$achv_q,
  levels = c("Q1 低", "Q2", "Q3", "Q4 高"))
pG <- ggplot(resid_df, aes(achv_q, Type, fill = resid)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = sprintf("%+.1f", resid)),
            family = font_family, size = 4.2) +
  scale_fill_gradient2(low = "#C53030", mid = "#FEFCBF", high = "#2C5282",
                       midpoint = 0, name = "標準化\n殘差") +
  scale_x_discrete(position = "top") +
  labs(title = "卡方標準化殘差：教養類型 × 成就四分位",
       subtitle = "藍 = 高於「兩變項獨立」期望、紅 = 低於期望；|殘差| > 2 約對應 p < .05",
       x = "成就四分位（w2all3p）", y = NULL) +
  coord_fixed()
save_plot(pG, "13_chisq_residuals.png", 9, 5.4)

# ---- forward-stepwise ----
# Forward stepwise：從空模型開始，每步加入「使 R² 增加最多」的變項，看解釋力從哪來。
# 候選為 6 個主效應（D、R 與四個結構變項）；交互 D×R 的貢獻見「全體迴歸」一節。
fs_cands <- c(D_c = "要求 D", R_c = "回應 R", urban = "城鄉",
              priv = "公私立", far = "偏遠", gender = "性別")
# 各候選單獨的 R²
solo_r2 <- sapply(names(fs_cands), function(v)
  summary(lm(as.formula(paste("w2all3p ~", v)), data = dat))$r.squared)
solo <- data.frame(var = unname(fs_cands), soloR2 = unname(solo_r2))
solo <- solo[order(-solo$soloR2), ]
write.csv(solo, file.path(out_dir, "stepwise_solo.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
# Forward stepwise 路徑
chosen <- character(0); prev <- 0
remaining <- names(fs_cands); path <- list()
for (s in seq_along(fs_cands)) {
  r2s <- sapply(remaining, function(v)
    summary(lm(as.formula(paste("w2all3p ~",
      paste(c(chosen, v), collapse = " + "))), data = dat))$r.squared)
  best <- remaining[which.max(r2s)]
  path[[s]] <- data.frame(step = s, var = unname(fs_cands[best]),
                          dR2 = max(r2s) - prev, cumR2 = max(r2s))
  chosen <- c(chosen, best); prev <- max(r2s)
  remaining <- setdiff(remaining, best)
}
fs <- do.call(rbind, path)
write.csv(fs, file.path(out_dir, "stepwise_path.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 圖 12：forward stepwise 的 ΔR² 路徑（每步貢獻 + 累積線）。
fs$lab <- factor(sprintf("%d. %s", fs$step, fs$var),
                 levels = sprintf("%d. %s", fs$step, fs$var))
pF <- ggplot(fs, aes(lab, dR2)) +
  geom_col(width = 0.62, fill = "#2B6CB0") +
  geom_text(aes(label = sprintf("+%.2f%%", 100 * dR2)),
            vjust = -0.4, family = font_family, size = 3.6) +
  geom_line(aes(y = cumR2, group = 1), color = "#C05621", linewidth = 0.9) +
  geom_point(aes(y = cumR2), color = "#C05621", size = 2.4) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 1), "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Forward stepwise：解釋力逐步建構",
       subtitle = "藍長條 = 該步新增的 ΔR²；橘線與點 = 累積 R²",
       x = "加入順序（每步選 R² 增加最多者）", y = "R²") +
  theme(axis.title.y = element_text())
save_plot(pF, "12_forward_stepwise.png", 9, 5)

# ============================================================================
# 主控台報告
# ============================================================================
cat("\n========== 迴歸樣本 ==========\n")
cat(sprintf("  n = %d（已剔除 w2all3p 遺漏 %d 人）\n", N, nrow(ana) - N))

cat("\n========== 全體模型 ==========\n")
of <- overall_fit; of[, -1] <- round(of[, -1], 4); print(of, row.names = FALSE)
cat(sprintf("\n交互項是否顯著改善（D+R vs D*R）：F = %.2f, p = %.3g\n",
            aov_int$F[2], aov_int$`Pr(>F)`[2]))
cat("\nD*R 交互模型係數：\n")
ct <- coef_tab(M_int, "D*R")[, -1]; ct[, -1] <- round(ct[, -1], 4)
print(ct, row.names = FALSE)

cat("\n========== 分層迴歸係數 ==========\n")
print(round(strat_coef[, c("n","截距","beta_D","beta_R","beta_DR","adjR2")], 3))

cat("\n========== 截距差 / 斜率差 巢狀 F 檢定 ==========\n")
print(round(slope_tests[, -1], 4))
cat("（截距差 p 小 = baseline 隨該變項移動；斜率差 p 大 = D/R 斜率不隨該變項改變）\n")

cat("\n========== 四類型單因子 ANOVA + Tukey ==========\n")
cat(sprintf("  ANOVA：F(%d, %d) = %.2f, p = %.3g\n",
            aov_tab$Df[1], aov_tab$Df[2], aov_tab[["F value"]][1], aov_tab[["Pr(>F)"]][1]))
cat(sprintf("  對照 lm(w2all3p ~ Type) 的 F = %.2f（與 ANOVA 相同，印證特例關係）\n",
            summary(lm_type)$fstatistic[1]))
cat("  Tukey 事後比較：\n")
tk <- tuk[, c("pair", "diff", "lwr", "upr", "p_adj")]
tk[, -1] <- round(tk[, -1], 4)
print(tk, row.names = FALSE)

cat("\n========== 四類型 × 成就四分位 卡方獨立性檢定 ==========\n")
cat(sprintf("  χ²(%d) = %.2f, p = %.3g, Cramér's V = %.3f, n = %d\n",
            chi_test$parameter, chi_test$statistic, chi_test$p.value,
            chi_V, sum(chi_tab)))
cat("  列聯表（次數）：\n")
print(chi_tab)
cat("  標準化殘差（>2 或 <-2 = 該格明顯偏離獨立期望）：\n")
print(round(chi_resid, 2))

cat("\n========== Forward stepwise ==========\n")
cat("各變項單獨 R²：\n")
solo_p <- solo; solo_p$soloR2 <- round(solo_p$soloR2, 4)
print(solo_p, row.names = FALSE)
cat("逐步路徑：\n")
fsd <- fs[, c("step", "var", "dR2", "cumR2")]
fsd[, 3:4] <- round(fsd[, 3:4], 4)
print(fsd, row.names = FALSE)

cat("\n已輸出迴歸表與圖至：", normalizePath(out_dir), "\n")
