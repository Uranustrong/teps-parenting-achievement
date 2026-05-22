# =============================================================================
# 從 TEPS 第二波學生原始檔，建立每位學生的「主要照顧者」——聯集法（同住 ∪ 照顧）
# -----------------------------------------------------------------------------
# 資料來源：Data/stata/w2_j_s_v6.0.dta（TEPS 第二波學生問卷原始檔）。
#
# 推論規則（與小組討論定案）：
#   步驟 1  品質旗標：w2refuse / w2sumerr / w2sumlog 三者皆 0 才保留。
#   步驟 2  主要照顧者 = 逐角色「同住 ∪ 照顧」聯集（w2s456x ∪ w2s457x）。
#   步驟 3  排除繼父母進聯集者；排除無任何親屬者（「只有其他」/ 無人）。
#
# 註：本檔以「# ---- 英數標籤 ----」標記區段，方便閱讀與對應報告章節；程式碼可由
#     報告 .Rmd 以 source() 呼叫，也可直接以 Rscript 執行（須在專案根目錄下）。
# =============================================================================

# ---- setup ----
# 套件與輸出資料夾。工作目錄須為專案根目錄（含 Data/ 與 教養分析/）：
# 由報告 .Rmd 以 source() 呼叫時已設定好；單獨執行請先 setwd() 到專案根目錄。
library(haven)
out_dir <- file.path("教養分析", "outputs", "caregiver_rebuild")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- read-raw ----
# 讀入 TEPS w2 學生原始檔。haven 讀進來是 labelled 欄位，全部轉成數值碼，
# 之後一律以數值比較（== 1、== 0、== 99）。
d <- as.data.frame(read_dta("Data/stata/w2_j_s_v6.0.dta", encoding = "BIG5"))
for (v in names(d)) d[[v]] <- as.numeric(d[[v]])
N0 <- nrow(d)

# ---- quality-flags ----
# 步驟 1：問卷自帶的三個品質旗標，必須同時為 0（無拒答、零不合理值、零邏輯衝突）
# 才保留。is1() 是「值等於 1 且非 NA」的小工具，後面判斷勾選都用它。
is1 <- function(x) !is.na(x) & x == 1
qpass <- d$w2refuse == 0 & d$w2sumerr == 0 & d$w2sumlog == 0

# ---- union-caregiver ----
# 步驟 2：逐角色取「同住題(w2s456x) ∪ 照顧題(w2s457x)」聯集。
# 住 / 顧題對祖父母只到「夫妻對」層級：內祖父母(G) = 父系一對、外祖父母(MG) = 母系一對。
u_F  <- is1(d$w2s4561) | is1(d$w2s4571)   # 親爸
u_M  <- is1(d$w2s4562) | is1(d$w2s4572)   # 親媽
u_GF <- is1(d$w2s4563) | is1(d$w2s4573)   # 內祖父母（父系，一對）
u_GM <- is1(d$w2s4564) | is1(d$w2s4574)   # 外祖父母（母系，一對）
u_S  <- is1(d$w2s4565) | is1(d$w2s4575)   # 養繼父母
u_O  <- is1(d$w2s4566) | is1(d$w2s4576)   # 其他
has_kin <- u_F | u_M | u_GF | u_GM        # 是否至少有一位「親屬」照顧者

# ---- exclusion ----
# 步驟 3：繼父母進入聯集者整案排除（單從問卷無法得知實際教養情形）；
# 無任何親屬者（只有「其他」或無人）排除。keep 即最終分析樣本旗標。
keep <- qpass & !u_S & has_kin

# ---- caregiver-string ----
# main_caregiver 字串、代碼、人數。token：F 親爸 / M 親媽 / G 內祖 / MG 外祖 / O 其他。
# 注意 n_caregiver 此處把內、外祖父母各算 1；「祖父母內外合為一單位」的合併在
# dr_scoring.R 重算，不在這裡。
tokens <- cbind(
  ifelse(u_F,  "F",  ""), ifelse(u_M,  "M",  ""), ifelse(u_GF, "G",  ""),
  ifelse(u_GM, "MG", ""), ifelse(u_O,  "O",  "")
)
main_caregiver <- apply(tokens, 1, function(r) paste(r[r != ""], collapse = "+"))
n_caregiver    <- u_F + u_M + u_GF + u_GM + u_O          # 繼父母案已排除，不計
caregiver_code <- u_F * 1 + u_M * 2 + u_GF * 4 + u_GM * 8 + u_O * 16

# ---- gp-involvement ----
# 祖父 / 祖母「涉入」診斷，供 dr_scoring.R 的祖父母計分規則使用。
# 教養 10 題的子題：(外)祖父 = 子題 4、(外)祖母 = 子題 5。
gf_items <- c("w2s2014","w2s2024","w2s2034","w2s2044","w2s2064",
              "w2s2124","w2s2144","w2s2174","w2s2184","w2s2194")
gm_items <- c("w2s2015","w2s2025","w2s2035","w2s2045","w2s2065",
              "w2s2125","w2s2145","w2s2175","w2s2185","w2s2195")
GF <- d[, gf_items]; GM <- d[, gm_items]
# 定義(b)：被填答 = 至少一題為有效 0/1（特殊碼 99 不算）。僅供診斷對照。
gf_answered <- rowSums(GF == 0 | GF == 1, na.rm = TRUE) > 0
gm_answered <- rowSums(GM == 0 | GM == 1, na.rm = TRUE) > 0
# 定義(a)：涉入 = 至少一題被勾選 (=1)。〔正式分析採用此定義〕
gf_involved <- rowSums(GF == 1, na.rm = TRUE) > 0
gm_involved <- rowSums(GM == 1, na.rm = TRUE) > 0

# ---- write-output ----
# 輸出重建後的照顧者表（全 13,247 列，附 keep 旗標供下游過濾）。
rebuilt <- data.frame(
  stud_id = d$stud_id,
  w2refuse = d$w2refuse, w2sumerr = d$w2sumerr, w2sumlog = d$w2sumlog,
  qpass = qpass,
  u_F = as.integer(u_F), u_M = as.integer(u_M), u_GF = as.integer(u_GF),
  u_GM = as.integer(u_GM), u_S = as.integer(u_S), u_O = as.integer(u_O),
  has_kin = has_kin, keep = keep,
  main_caregiver = main_caregiver, caregiver_code = caregiver_code,
  n_caregiver = n_caregiver,
  gf_answered = gf_answered, gm_answered = gm_answered,
  gf_involved = gf_involved, gm_involved = gm_involved
)
write.csv(rebuilt, file.path(out_dir, "caregiver_rebuilt.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- report-funnel ----
# 樣本 funnel：原始 → 品質旗標 → 排除繼父母 → 排除無親屬。
cat("\n========== 樣本 funnel ==========\n")
n_q   <- sum(qpass)
n_s   <- sum(qpass & u_S)
n_nok <- sum(qpass & !u_S & !has_kin)
n_fin <- sum(keep)
cat(sprintf("  原始樣本                         : %5d\n", N0))
cat(sprintf("  步驟1 品質旗標皆為 0              : %5d  (排除 %d)\n", n_q, N0 - n_q))
cat(sprintf("  步驟3a 排除「繼父母進聯集」        : %5d  (排除 %d)\n", n_q - n_s, n_s))
cat(sprintf("  步驟3b 排除「無親屬／只有其他」    : %5d  (排除 %d)\n", n_fin, n_nok))
cat(sprintf("  ── 聯集法最終樣本                 : %5d\n", n_fin))

# ---- report-composition ----
# 主要照顧者組成：人數分布、前 12 大組合、各角色出現人數。
cat("\n========== 主要照顧者人數分布（最終樣本）==========\n")
print(table(n_caregiver[keep]))

cat("\n========== 前 12 大主要照顧者組合 ==========\n")
tt <- sort(table(main_caregiver[keep]), decreasing = TRUE)
print(head(tt, 12))

cat("\n========== 各角色出現在主要照顧者中的人數 ==========\n")
cat(sprintf("  親爸 F  : %d\n", sum(u_F[keep])))
cat(sprintf("  親媽 M  : %d\n", sum(u_M[keep])))
cat(sprintf("  內祖 G  : %d\n", sum(u_GF[keep])))
cat(sprintf("  外祖 MG : %d\n", sum(u_GM[keep])))
cat(sprintf("  其他 O  : %d\n", sum(u_O[keep])))
grandU <- (u_GF | u_GM)
cat(sprintf("  含祖父母（內或外）: %d\n", sum(keep & grandU)))

# ---- report-edgecase ----
# 祖父母邊界個案診斷：對照定義(b) 與定義(a) 下「祖父母進聯集但零涉入」的人數。
cat("\n========== 祖父母邊界個案診斷 ==========\n")
sub <- keep & grandU
cat(sprintf("最終樣本中「祖父母進聯集」的學生：%d 人\n\n", sum(sub)))
cat("定義(b) 被填答 = 至少一題有效作答(0/1)；從沒被填答 = 10題全99\n")
print(table(祖父 = ifelse(gf_answered[sub], "被填答", "全99"),
            祖母 = ifelse(gm_answered[sub], "被填答", "全99")))
cat(sprintf("  -> 邊界個案（兩者皆從沒被填答）：%d 人\n\n",
            sum(!gf_answered[sub] & !gm_answered[sub])))
cat("定義(a) 涉入 = 至少一題被勾選(=1)；沒涉入 = 10題都沒被勾\n")
print(table(祖父 = ifelse(gf_involved[sub], "有涉入", "沒涉入"),
            祖母 = ifelse(gm_involved[sub], "有涉入", "沒涉入")))
edge_a <- sub & !gf_involved & !gm_involved
cat(sprintf("  -> 邊界個案（兩者皆沒涉入）：%d 人\n", sum(edge_a)))
# 邊界個案中，祖父母是否為「唯一」照顧者（無父母）。
gp_only <- edge_a & !u_F & !u_M
cat(sprintf("     其中祖父母為唯一照顧者（無父母同列）：%d 人\n", sum(gp_only)))

cat("\n已輸出：", normalizePath(file.path(out_dir, "caregiver_rebuilt.csv")), "\n")
