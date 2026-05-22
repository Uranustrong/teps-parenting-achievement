# TEPS 教養方式與學業成就分析

以「臺灣教育長期追蹤資料庫（TEPS）」第二波國中學生資料，探討**父母（與其他主要
照顧者）的教養方式**與**學業成就**之間的關聯。教養方式採 Maccoby & Martin（1983）
的兩維度架構——**要求（Demandingness）** × **回應（Responsiveness）**——交叉成
開明權威 / 專制權威 / 寬鬆放任 / 忽視冷漠四類型。

本 repo 收錄完整的 R 程式碼與報告，供組員與他人**重現**整條分析管線。

## 分析方法

- **主要照顧者**：由問卷「國小畢業前與誰住 ∪ 誰照顧」的聯集推得；排除繼父母家庭與
  無親屬者。
- **D / R 計分**：要求、回應各 5 題，依主要照顧者的勾選加總後平均。
- **四類型**：D、R 各以樣本中位數切高低，交叉成四類。
- **檢定**：連續迴歸（D + R、D × R 交互、分層截距／斜率）、四類型單因子 ANOVA +
  Tukey、教養類型 × 成就四分位的卡方獨立性檢定。

## 資料取得（重要）

分析使用 TEPS 第二波學生問卷檔 **`w2_j_s_v6.0.dta`**。該檔屬 TEPS／中研院調查研究
專題中心（SRDA）釋出之資料，**受資料使用規範約束、不隨本 repo 散布**。重現分析前，
請自行至 SRDA（<https://srda.sinica.edu.tw>）申請／下載，並放到：

```
Data/stata/w2_j_s_v6.0.dta
```

由原始資料衍生、含逐人記錄的中間檔（`caregiver_rebuilt.csv`、`analysis_data.csv` /
`.rds`）同樣不納入版本控制，會在執行時自動重新產生。

## 環境需求

- R（建議 4.x 以上）
- R 套件：`haven`、`dplyr`、`ggplot2`、`tidyr`、`scales`、`ragg`、`rmarkdown`、`knitr`
- 渲染 HTML 報告需 Pandoc——用 RStudio 開啟 `.Rmd` 並 **Knit** 會自動處理。

安裝套件：

```r
install.packages(c("haven","dplyr","ggplot2","tidyr","scales","ragg","rmarkdown","knitr"))
```

## 重現步驟

1. 取得 `w2_j_s_v6.0.dta`，放入 `Data/stata/`（見上）。
2. **工作目錄設為本 repo 根目錄**（含 `Data/` 與 `教養分析/` 的那一層）。用 RStudio
   的話，從 repo 根目錄開啟、或開啟 `.Rmd` 後直接 Knit，工作目錄會自動設好。
3. 擇一執行：

   **(a) 直接渲染報告**——會自動 `source()` 對應腳本、重算所有結果與圖：

   ```r
   rmarkdown::render("教養分析/reports/照顧者重建與DR計分.Rmd")  # 請先跑這份
   rmarkdown::render("教養分析/reports/迴歸分析.Rmd")            # 再跑這份
   ```

   **描述性報告須先跑**——它會產出迴歸報告所需的分析資料（`analysis_data.rds`）。

   **(b) 逐步執行腳本**（須依序）：

   ```r
   source("教養分析/scripts/rebuild_caregiver.R")  # 步驟 1–3：主要照顧者
   source("教養分析/scripts/dr_scoring.R")         # 步驟 4–5：D / R 計分、四類型
   source("教養分析/scripts/visualize.R")          # 描述性視覺化
   source("教養分析/scripts/regression.R")         # 迴歸 / ANOVA / 卡方
   ```

## 資料夾結構

```
.
├── README.md
├── Data/stata/            原始 .dta 放這裡（不在 repo 內，需自行取得）
└── 教養分析/
    ├── README.md          方法細節與衡量限制
    ├── scripts/           4 支 R 腳本：重建 → 計分 → 視覺化 → 迴歸
    ├── reports/           2 份 R Markdown 報告 + 已渲染的 HTML
    └── outputs/
        ├── figures/       圖檔（PNG）
        └── regression/    迴歸 / ANOVA / 卡方的彙總報表（CSV）
```

## 報告

- `教養分析/reports/照顧者重建與DR計分.html` — 資料建置、D / R 計分、四類型、描述性結果
- `教養分析/reports/迴歸分析.html` — 連續迴歸、分層、ANOVA + Tukey、卡方

## 說明

本研究為**描述與相關性**分析，未宣稱因果；為心理統計學課程期末專題。
