# =============================================================================
# Phase 3：視覺化
# -----------------------------------------------------------------------------
# 讀 dr_scoring.R 產出的 analysis_data.rds，產生四類型 / D×R / 學業成就等圖。
# 字型 Heiti TC + ragg::agg_png——macOS 上才能讓中文不變成空白方塊。
#
# 註：本檔以「# ---- 英數標籤 ----」標記區段，與其他腳本一致。
# =============================================================================

# ---- setup ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# 工作目錄須為專案根目錄（含 Data/ 與 教養分析/）：由報告 .Rmd 以 source()
# 呼叫時已設定好；單獨執行請先 setwd() 到專案根目錄。
base    <- "教養分析"
out_dir <- file.path(base, "outputs", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

obj   <- readRDS(file.path(base, "outputs", "dr_scoring", "analysis_data.rds"))
ana   <- obj$data
D_med <- obj$D_med
R_med <- obj$R_med

# ---- theme ----
# 共用字型、繪圖裝置與主題；新圖一律沿用，確保風格一致。
font_family <- "Heiti TC"
png_device  <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
theme_set(theme_minimal(base_size = 13, base_family = font_family) +
  theme(plot.title.position = "plot",
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank()))

type_levels <- c("開明權威", "專制權威", "寬鬆放任", "忽視冷漠")
type_colors <- c("開明權威" = "#2B6CB0", "專制權威" = "#C05621",
                 "寬鬆放任" = "#38A169", "忽視冷漠" = "#718096")

save_plot <- function(p, file, w = 9, h = 5.5)
  ggsave(file.path(out_dir, file), p, width = w, height = h, dpi = 180,
         device = png_device, bg = "white")

# 泡泡圖虛線的繪製位置：低組最大值與高組最小值的中點。分類本身仍以「D／R 嚴格大於
# 中位數」為準，此處只是讓虛線落在兩排離散泡泡「之間」、與顏色邊界對齊（直接畫在中位數
# 會穿過一排泡泡、且與顏色邊界錯開）；切出的高低兩組與中位數切分完全相同。
D_cut <- (max(ana$D_score[ana$D_score <= D_med]) +
          min(ana$D_score[ana$D_score >  D_med])) / 2
R_cut <- (max(ana$R_score[ana$R_score <= R_med]) +
          min(ana$R_score[ana$R_score >  R_med])) / 2

# ---- fig1-type-counts ----
# 圖 1：四類教養方式人數分布。
tc <- ana %>% count(Type) %>% mutate(pct = n / sum(n))
p1 <- ggplot(tc, aes(Type, n, fill = Type)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%s\n%.1f%%", comma(n), 100 * pct)),
            vjust = -0.2, family = font_family, size = 4) +
  scale_fill_manual(values = type_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Maccoby & Martin 四類教養方式人數分布",
       subtitle = sprintf("分析樣本 n = %s", comma(nrow(ana))),
       x = NULL, y = "人數")
save_plot(p1, "01_type_counts.png", 8, 5.2)

# ---- fig2-dr-bubble ----
# 圖 2：要求 D × 回應 R 泡泡圖。離散分數用 geom_count 疊成泡泡，避免 jitter 的視覺假象。
# 分類以中位數為準；虛線畫在 D_cut／R_cut（見上）只是繪圖位置，標示的仍是中位數高低分界。
p2 <- ggplot(ana, aes(D_score, R_score, color = Type)) +
  geom_count(alpha = 0.85) +
  scale_size_area(max_size = 13, name = "學生數") +
  scale_color_manual(values = type_colors, name = "教養類型") +
  geom_vline(xintercept = D_cut, linetype = "dashed", color = "#4A5568") +
  geom_hline(yintercept = R_cut, linetype = "dashed", color = "#4A5568") +
  scale_x_continuous(breaks = 0:5, limits = c(-0.3, 5.3)) +
  scale_y_continuous(breaks = 0:5, limits = c(-0.3, 5.3)) +
  labs(title = "要求 D × 回應 R 泡泡圖",
       subtitle = sprintf("四象限即四類型；分類以中位數嚴格切高低（D 中位數 %.2f、R 中位數 %.2f）\n虛線為高低分界，畫於兩排泡泡之間以對齊顏色邊界；泡泡大小 = 學生數",
                          D_med, R_med),
       x = "要求 D_score", y = "回應 R_score") +
  theme(axis.title.y = element_text(), legend.position = "right")
save_plot(p2, "02_DR_bubble.png", 10, 6.4)

# ---- fig3-achievement ----
# 圖 3：四類型 × 三種學業成就（IRT 分數）平均，誤差線為 95% CI。
achv <- ana %>%
  select(Type, w2all3p, w2m3p, w2cf3p) %>%
  pivot_longer(-Type, names_to = "outcome", values_to = "score") %>%
  filter(!is.na(score)) %>%
  mutate(outcome = recode(outcome,
    w2all3p = "綜合分析能力 w2all3p",
    w2m3p   = "數學分析能力 w2m3p",
    w2cf3p  = "一般分析能力 w2cf3p")) %>%
  group_by(outcome, Type) %>%
  summarise(n = n(), mean = mean(score), se = sd(score) / sqrt(n()), .groups = "drop")
p3 <- ggplot(achv, aes(Type, mean, fill = Type)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                width = 0.18, color = "#2D3748") +
  facet_wrap(~outcome, ncol = 3) +
  scale_fill_manual(values = type_colors, guide = "none") +
  labs(title = "四類教養方式的學業成就平均",
       subtitle = "誤差線為平均數 ± 1.96 × SE（描述性，未做檢定）",
       x = NULL, y = "IRT 分數平均") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        axis.title.y = element_text())
save_plot(p3, "03_achievement_by_type.png", 12, 5.6)

# ---- fig4-dr-distribution ----
# 圖 4：要求 D / 回應 R 分數分布；紅虛線為中位數切點。
sl <- ana %>%
  select(D_score, R_score) %>%
  pivot_longer(everything(), names_to = "dim", values_to = "score") %>%
  mutate(dim = recode(dim, D_score = "要求 D_score", R_score = "回應 R_score"))
meds <- data.frame(dim = c("要求 D_score", "回應 R_score"), m = c(D_med, R_med))
p4 <- ggplot(sl, aes(score)) +
  geom_histogram(binwidth = 0.25, fill = "#4A5568", color = "white") +
  geom_vline(data = meds, aes(xintercept = m), color = "#C53030",
             linetype = "dashed", linewidth = 0.7) +
  facet_wrap(~dim) +
  labs(title = "要求 D / 回應 R 分數分布",
       subtitle = "紅虛線為中位數；長條寬度（bin）0.25",
       x = "分數（0–5）", y = "人數") +
  theme(axis.title.y = element_text())
save_plot(p4, "04_DR_distribution.png", 10, 4.8)

# ---- fig5-caregiver-roles ----
# 圖 5：主要照顧者角色組成（同一學生可同時屬於多角色）。
roles <- data.frame(
  角色 = factor(c("親爸", "親媽", "內、外祖父母", "其他"),
               levels = c("親爸", "親媽", "內、外祖父母", "其他")),
  n = c(sum(ana$u_F), sum(ana$u_M),
        sum(ana$u_GF == 1 | ana$u_GM == 1), sum(ana$u_O)))
roles$pct <- roles$n / nrow(ana)
p5 <- ggplot(roles, aes(角色, n)) +
  geom_col(width = 0.65, fill = "#2B6CB0") +
  geom_text(aes(label = sprintf("%s\n%.1f%%", comma(n), 100 * pct)),
            vjust = -0.2, family = font_family, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "主要照顧者角色組成",
       subtitle = sprintf("各角色出現在主要照顧者中的人數（n = %s；可多角色）",
                          comma(nrow(ana))),
       x = NULL, y = "人數")
save_plot(p5, "05_caregiver_roles.png", 8, 5)

# ---- fig6-achievement-heatmap ----
# 圖 6：學業成就 w2all3p 在要求 D × 回應 R 連續平面上的熱圖。
# 每格 0.5×0.5，填色為格內平均；僅顯示 n ≥ 15 的格以免單點雜訊。
# 虛線用 D／R 中位數本身（非泡泡圖的視覺切點 D_cut／R_cut）——熱圖格邊落在 0.5 的
# 整數倍上，中位數恰在格邊、與格線對齊；視覺切點落在 2.x 反而會切穿格子。
bw <- 0.5
overall_achv <- mean(ana$w2all3p, na.rm = TRUE)
hm <- ana %>%
  filter(!is.na(w2all3p)) %>%
  mutate(Dbin = (pmin(floor(D_score / bw), 5 / bw - 1) + 0.5) * bw,
         Rbin = (pmin(floor(R_score / bw), 5 / bw - 1) + 0.5) * bw) %>%
  group_by(Dbin, Rbin) %>%
  summarise(mean_achv = mean(w2all3p), n = n(), .groups = "drop") %>%
  filter(n >= 15)
quad <- data.frame(x = c(4.4, 4.4, 0.6, 0.6), y = c(4.7, 0.3, 4.7, 0.3),
                   lab = c("開明權威", "專制權威", "寬鬆放任", "忽視冷漠"))
p6 <- ggplot(hm, aes(Dbin, Rbin, fill = mean_achv)) +
  geom_tile(width = bw * 0.95, height = bw * 0.95) +
  scale_fill_gradient2(low = "#C53030", mid = "#FEFCBF", high = "#2C5282",
                       midpoint = overall_achv, name = "w2all3p\n平均") +
  geom_vline(xintercept = D_med, linetype = "dashed", color = "#2D3748") +
  geom_hline(yintercept = R_med, linetype = "dashed", color = "#2D3748") +
  annotate("text", x = quad$x, y = quad$y, label = quad$lab,
           family = font_family, size = 3.4, color = "#2D3748", fontface = "bold") +
  coord_fixed() +
  scale_x_continuous(breaks = 0:5, limits = c(-0.05, 5.05)) +
  scale_y_continuous(breaks = 0:5, limits = c(-0.05, 5.05)) +
  labs(title = "學業成就在要求 D × 回應 R 平面上的分布",
       subtitle = sprintf("每格 %.1f×%.1f、填色為格內 w2all3p 平均（n ≥ 15）；色階中點 = 全體平均 %.2f；虛線為 D／R 中位數",
                          bw, bw, overall_achv),
       x = "要求 D_score", y = "回應 R_score") +
  theme(axis.title.y = element_text())
save_plot(p6, "06_achievement_heatmap.png", 8.5, 7.2)

message("已輸出 6 張圖至 ", normalizePath(out_dir))
