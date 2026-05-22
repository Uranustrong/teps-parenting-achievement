# =============================================================================
# Phase 2：D / R 計分與四類型（祖父母內、外合為一個照顧單位）
# -----------------------------------------------------------------------------
# 前置：rebuild_caregiver.R 已產出 caregiver_rebuilt.csv。
#
# 計分規則（與小組討論定案）：
#   照顧單位：親爸、親媽、祖父母（內 + 外合為「一個」單位）、其他，各占分母 1。
#   每位照顧者的 D / R 分數 = 其在 5 題要求 / 5 題回應的勾選加總（特殊碼 99 當 0）。
#   祖父母單位分數 = 有「涉入」之祖父母的平均（涉入 = 10 題中至少 1 題被勾選）：
#       祖父、祖母都涉入 → (祖父 + 祖母) / 2
#       只有一位涉入     → 那一位
#       兩位都沒涉入     → 0 分
#   祖父母單位只要在「住∪顧」中就固定占分母 1（零涉入也算 1 位、貢獻 0 分），
#   與「在主要照顧者中、但教養分數為 0 的父母」同等對待。
#   D_score = Σ(各單位分數) / n_caregiver；中位數切分（嚴格 >）分四型。
#
# 註：本檔以「# ---- 英數標籤 ----」標記區段，方便閱讀與對應報告章節；程式碼可由
#     報告 .Rmd 以 source() 呼叫，也可直接以 Rscript 執行（須在專案根目錄下）。
# =============================================================================

# ---- setup ----
# 套件與輸出資料夾。工作目錄須為專案根目錄（含 Data/ 與 教養分析/）：
# 由報告 .Rmd 以 source() 呼叫時已設定好；單獨執行請先 setwd() 到專案根目錄。
library(haven)
base    <- "教養分析"
out_dir <- file.path(base, "outputs", "dr_scoring")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- read-align ----
# 讀入 rebuild_caregiver.R 的照顧者表 (cg) 與原始問卷檔 (d)，
# 並把 d 依 cg 的 stud_id 順序對齊，之後所有運算逐列對應。
cg <- read.csv(file.path(base, "outputs", "caregiver_rebuild", "caregiver_rebuilt.csv"),
               fileEncoding = "UTF-8")
d  <- as.data.frame(read_dta("Data/stata/w2_j_s_v6.0.dta", encoding = "BIG5"))
for (v in names(d)) d[[v]] <- as.numeric(d[[v]])

# 防呆：對齊前先確認 stud_id 能當唯一鍵、且每筆 cg 都在原檔找得到對應。
# 萬一資料對不齊，這裡會直接報錯中止，不會靜默產出錯誤結果。
stopifnot(
  "cg$stud_id 有重複"          = anyDuplicated(cg$stud_id) == 0,
  "原檔 stud_id 有重複"        = anyDuplicated(d$stud_id)  == 0,
  "有 cg$stud_id 在原檔找不到" = all(!is.na(match(cg$stud_id, d$stud_id)))
)
d <- d[match(cg$stud_id, d$stud_id), ]
# 對齊後再逐列確認 stud_id 完全相等——往後所有 cg／d 的逐列運算才成立。
stopifnot("對齊後 stud_id 未完全一致" = all(d$stud_id == cg$stud_id))

# ---- parenting-sums ----
# 逐人教養加總。每題子題：2=爸 3=媽 4=(外)祖父 5=(外)祖母 6=家中其他人。
# clean01() 把特殊碼 99 / NA 當 0；psum() 對某子題、跨指定題目加總。
clean01 <- function(x) ifelse(!is.na(x) & x %in% c(0, 1), x, 0)
D_items <- c("201", "202", "203", "204", "206")   # 要求 (Demandingness)
R_items <- c("212", "214", "217", "218", "219")   # 回應 (Responsiveness)
psum <- function(items, sub) rowSums(sapply(d[paste0("w2s", items, sub)], clean01))
F_D  <- psum(D_items, 2); M_D  <- psum(D_items, 3); GF_D <- psum(D_items, 4)
GM_D <- psum(D_items, 5); O_D  <- psum(D_items, 6)
F_R  <- psum(R_items, 2); M_R  <- psum(R_items, 3); GF_R <- psum(R_items, 4)
GM_R <- psum(R_items, 5); O_R  <- psum(R_items, 6)

# ---- gp-unit ----
# 祖父母單位分數：有涉入之祖父母的平均；都沒涉入 → 0。
# gp_in：祖父母單位是否存在（內或外任一進聯集）。gfi / gmi：祖父 / 祖母是否涉入。
gp_in <- (cg$u_GF == 1 | cg$u_GM == 1)
gfi   <- cg$gf_involved
gmi   <- cg$gm_involved
unit_D <- ifelse(gfi & gmi, (GF_D + GM_D) / 2,
          ifelse(gfi & !gmi, GF_D,
          ifelse(!gfi & gmi, GM_D, 0)))
unit_R <- ifelse(gfi & gmi, (GF_R + GM_R) / 2,
          ifelse(gfi & !gmi, GF_R,
          ifelse(!gfi & gmi, GM_R, 0)))

# ---- dr-score ----
# n_caregiver（祖父母內外整體算 1）與 D / R score。
# 祖父母單位只要存在就占分母 1（gp），分子放單位分數（零涉入時 unit_* = 0）。
gp <- as.integer(gp_in)
n_caregiver <- cg$u_F + cg$u_M + gp + cg$u_O
D_score <- (cg$u_F * F_D + cg$u_M * M_D + gp * unit_D + cg$u_O * O_D) / n_caregiver
R_score <- (cg$u_F * F_R + cg$u_M * M_R + gp * unit_R + cg$u_O * O_R) / n_caregiver

# ---- analysis-frame ----
# 組成分析資料框，只留最終樣本 (keep)。同時保留各單位分數，方便日後核對 / 除錯。
keep <- cg$keep
ana <- data.frame(
  stud_id = cg$stud_id,
  main_caregiver = cg$main_caregiver,
  u_F = cg$u_F, u_M = cg$u_M, u_GF = cg$u_GF, u_GM = cg$u_GM, u_O = cg$u_O,
  gf_involved = cg$gf_involved, gm_involved = cg$gm_involved,
  n_caregiver = n_caregiver,
  F_D = F_D, M_D = M_D, GF_D = GF_D, GM_D = GM_D, O_D = O_D, unit_D = unit_D,
  F_R = F_R, M_R = M_R, GF_R = GF_R, GM_R = GM_R, O_R = O_R, unit_R = unit_R,
  D_score = D_score, R_score = R_score,
  w2all3p = d$w2all3p, w2m3p = d$w2m3p, w2cf3p = d$w2cf3p,
  w2stwt1 = d$w2stwt1, w2stwt2 = d$w2stwt2,
  w2far = d$w2far, w2priv = d$w2priv, w2urban3 = d$w2urban3,
  w2s445 = d$w2s445   # 性別：1 男、2 女
)[keep, ]

# ---- median-split ----
# 中位數切分（嚴格 >）→ Maccoby & Martin 四類型。
D_med <- median(ana$D_score)
R_med <- median(ana$R_score)
ana$D_type <- ifelse(ana$D_score > D_med, "高要求", "低要求")
ana$R_type <- ifelse(ana$R_score > R_med, "高回應", "低回應")
ana$Type <- factor(
  ifelse(ana$D_type == "高要求" & ana$R_type == "高回應", "開明權威",
  ifelse(ana$D_type == "高要求" & ana$R_type == "低回應", "專制權威",
  ifelse(ana$D_type == "低要求" & ana$R_type == "高回應", "寬鬆放任", "忽視冷漠"))),
  levels = c("開明權威", "專制權威", "寬鬆放任", "忽視冷漠")
)

# ---- write-output ----
# 輸出最終分析資料（CSV 供檢視、RDS 供下游報告快速讀取）。
write.csv(ana, file.path(out_dir, "analysis_data.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
saveRDS(list(data = ana, D_med = D_med, R_med = R_med),
        file.path(out_dir, "analysis_data.rds"))

# ---- report-scores ----
# 主控台報告（一）：樣本數、n_caregiver 分布、D / R score 摘要。
cat("\n========== 最終分析樣本 ==========\n")
cat(sprintf("  n = %d\n", nrow(ana)))

cat("\n========== n_caregiver 分布（祖父母內外整體算 1）==========\n")
print(table(ana$n_caregiver))

cat("\n========== D_score / R_score ==========\n")
cat(sprintf("  D_score：中位數 %.3f，平均 %.3f，範圍 [%.2f, %.2f]\n",
            D_med, mean(ana$D_score), min(ana$D_score), max(ana$D_score)))
cat(sprintf("  R_score：中位數 %.3f，平均 %.3f，範圍 [%.2f, %.2f]\n",
            R_med, mean(ana$R_score), min(ana$R_score), max(ana$R_score)))

# ---- report-types ----
# 主控台報告（二）：四類型分布、四類型 × 學業成就 w2all3p。
cat("\n========== 四類型分布 ==========\n")
tt <- table(ana$Type)
for (i in seq_along(tt))
  cat(sprintf("  %-8s %5d  (%.1f%%)\n", names(tt)[i], tt[i], 100 * tt[i] / sum(tt)))

cat("\n========== 四類型 × 學業成就 w2all3p ==========\n")
for (lv in levels(ana$Type)) {
  s <- ana$w2all3p[ana$Type == lv]
  cat(sprintf("  %-8s n=%5d  平均 %.3f  (SD %.3f)\n",
              lv, sum(!is.na(s)), mean(s, na.rm = TRUE), sd(s, na.rm = TRUE)))
}

cat("\n已輸出：", normalizePath(file.path(out_dir, "analysis_data.csv")), "\n")
