rm(list = ls())

install.packages("readxl")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("scales")
install.packages("fmsb")
install.packages("showtext")
install.packages("RColorBrewer")

library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(scales)
library(fmsb)  # 用于绘制雷达图
library(showtext) # 用于在图中正常显示中文
library (RColorBrewer)

##### 设置文件路径 #####
input_path <- 'C:/Users/Administrator/Desktop/用户价值分层与精准营销策略/user_personalized_features.xlsx'
output_dir <- 'C:/Users/Administrator/Desktop/用户价值分层与精准营销策略/result_r'

##### 数据加载与预处理 #####
# 给出数据框的行数与列数
df <- read_excel(input_path)
cat("数据规模:", nrow(df), "行 x", ncol(df), "列\n")

# 计算缺失值总数：对每列求和后，再对结果求和
cat("缺失值:", sum(is.na(df)), "\n")

# 异常值处理：将年龄不在[10,100]范围内的记录记为异常值
age_outliers <- sum(df$Age < 10 | df$Age > 100, na.rm = TRUE)
cat("年龄异常值:", age_outliers, "\n")

# 显示数据框的前6行
head(df)


##### 特征工程：构建RFM-I模型所需的所有指标 #####
# 定义最小-最大标准化函数
min_max_normalize <- function(vector, reverse = FALSE) {
  min_val <- min(vector, na.rm = TRUE)
  max_val <- max(vector, na.rm = TRUE)
  if (max_val == min_val) { # 处理全相同的情况
    return(rep(50, length(vector)))
  } else if (reverse) { # 处理倒序情况
    return(((max_val - vector) / (max_val - min_val)) * 100)
  } else {
    return(((vector - min_val) / (max_val - min_val)) * 100)
  }
}

# 计算意向(I)：先对两个指标标准化，再加权得到I
df$Time_Spent_Norm <- min_max_normalize(df$Time_Spent_on_Site_Minutes)
df$Pages_Viewed_Norm <- min_max_normalize(df$Pages_Viewed)
df$I_Score <- 0.5 * df$Time_Spent_Norm + 0.5 * df$Pages_Viewed_Norm

# 计算阻力系数：值越大，表示浏览多、但购买少
df$Friction <- df$Pages_Viewed / (df$Purchase_Frequency + 1)

# 计算忠诚度(L)：订阅且7天内登录为3分，未订阅且7天内登录为2分，其余为1分
calculate_loyalty <- function(subscribed, login_days) {
  if (subscribed & login_days < 7) {return(3)
  } else if (!subscribed & login_days < 7) {return(2)
  } else {return(1)}
}
df$L_Score <- mapply(calculate_loyalty, 
                     df$Newsletter_Subscription, df$Last_Login_Days_Ago)

# 计算购买力背景(D)：将用户按收入分位数，分为低、中、高三组
income_33 <- quantile(df$Income, 0.33, na.rm = TRUE)
income_66 <- quantile(df$Income, 0.66, na.rm = TRUE)
df$Income_Level <- cut(df$Income,
                       breaks = c(-Inf, income_33, income_66, Inf),
                       labels = c("Low", "Medium", "High"))

# 计算人货匹配度：兴趣与类别偏好相同为1、不同为0，全都为0
df$Interest_Match <- as.integer(df$Interests == df$Product_Category_Preference)

# 打印新定义的各个指标的范围或分布
cat(sprintf("I_Score 范围: [%.2f, %.2f]\n", min(df$I_Score, na.rm = TRUE), max(df$I_Score, na.rm = TRUE)))
cat(sprintf("Friction 范围: [%.2f, %.2f]\n", min(df$Friction, na.rm = TRUE), max(df$Friction, na.rm = TRUE)))
l_score_table <- table(df$L_Score)
cat("L_Score 分布: ")
cat(paste(names(l_score_table), l_score_table, sep = ":", collapse = ", "))
income_table <- table(df$Income_Level)
cat("Income_Level 分布: ")
cat(paste(names(income_table), income_table, sep = ":", collapse = ", "))
match_rate <- mean(df$Interest_Match, na.rm = TRUE) * 100
cat(sprintf("Interest_Match 匹配率: %.1f%%\n", match_rate))

# 显示新构建的指标的前6行
head(df[, c('User_ID','I_Score','Friction','L_Score','Income_Level','Interest_Match')])


##### 探索性分析 #####
# 1. 关键分布分析
# Total_Spending 分布：直方图
p1 <- ggplot(df, aes(x = Total_Spending)) +
  geom_histogram(bins = 50, fill = "#0B5CAD", alpha = 0.7, color = "black") +
  geom_vline(xintercept = median(df$Total_Spending, na.rm = TRUE), 
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Total_Spending Distribution (Long Tail Pattern)",
       x = "Total Spending", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Income vs Total_Spending：散点图
p2 <- ggplot(df, aes(x = Income, y = Total_Spending, color = Income_Level)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = c("Low" = "#FF6B6B", 
                                "Medium" = "#4ECDC4", 
                                "High" = "#0B5CAD")) +
  labs(title = "Income vs Total_Spending by Income Level",
       x = "Income", y = "Total Spending", color = "Income Level") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Last_Login_Days_Ago vs Newsletter_Subscription：分组箱线图
p3 <- ggplot(df, aes(x = factor(Newsletter_Subscription), y = Last_Login_Days_Ago)) +
  geom_boxplot(fill = "#FFD166", alpha = 0.3) +
  labs(title = "Login Days by Subscription Status",
       x = "Newsletter Subscription", y = "Last Login Days Ago") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# I_Score 分布：直方图
p4 <- ggplot(df, aes(x = I_Score)) +
  geom_histogram(bins = 30, fill = "#4ECDC4", alpha = 0.7, color = "black") +
  labs(title = "Intent Score (I_Score) Distribution",
       x = "I_Score", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 组合图形
grid <- grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
ggsave(file.path(output_dir, "eda_distributions.png"), grid, width = 14, height = 10, dpi = 150)

# 2. 相关性分析
numeric_cols <- c('Age', 'Income', 'Last_Login_Days_Ago', 'Purchase_Frequency', 
                  'Average_Order_Value', 'Total_Spending', 'Time_Spent_on_Site_Minutes', 
                  'Pages_Viewed', 'I_Score', 'Friction')
corr_matrix <- cor(df[, numeric_cols], use = "complete.obs") # use参数的作用是处理缺失值

png(file.path(output_dir, "correlation_matrix.png"), width = 1200, height = 1000, res = 150)
corrplot(corr_matrix, method = "color", type = "full", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("blue2", "white", "red2"))(200),
         title = "Feature Correlation Matrix", mar = c(0, 0, 1, 0))
dev.off()

# 3. 关键行为洞察
# 取出特定变量的相关系数
time_purchase_corr <- cor(df$Time_Spent_on_Site_Minutes, df$Purchase_Frequency, use = "complete.obs")
pages_purchase_corr <- cor(df$Pages_Viewed, df$Purchase_Frequency, use = "complete.obs")
cat("\n关键相关性分析:\n")
cat(sprintf("Time_Spent vs Purchase_Frequency 相关系数: %.3f\n", time_purchase_corr))
cat(sprintf("Pages_Viewed vs Purchase_Frequency 相关系数: %.3f\n", pages_purchase_corr))

# 高摩擦用户分析：筛选出Friction列的上四分位数(75%分位数)对应的用户，进行分析
high_friction <- df %>% filter(Friction > quantile(df$Friction, 0.75, na.rm = TRUE))
cat("\n高摩擦用户(前25%)特征:\n")
cat(sprintf("  - 平均浏览页数: %.1f\n", mean(high_friction$Pages_Viewed, na.rm = TRUE)))
cat(sprintf("  - 平均购买频率: %.1f\n", mean(high_friction$Purchase_Frequency, na.rm = TRUE)))
cat(sprintf("  - 平均消费金额: %.0f\n", mean(high_friction$Total_Spending, na.rm = TRUE)))

# 高收入低消费群体：选出高收入但消费低于中位数的用户，进行分析
spending_median <- median(df$Total_Spending, na.rm = TRUE)
high_income_low_spend <- df %>% 
  filter(Income_Level == "High", Total_Spending < spending_median)
high_income_users <- df %>% filter(Income_Level == "High")
cat("\n高收入低消费群体:\n")
cat(sprintf("  - 用户数: %d\n", nrow(high_income_low_spend)))
cat(sprintf("  - 占高收入群体比例: %.1f%%\n", 
            nrow(high_income_low_spend)/nrow(high_income_users)*100))


##### 构建RFM-I模型 #####
# 归一化R、F、M得分，并加权计算得到RFM综合得分
df$R_Score <- min_max_normalize(df$Last_Login_Days_Ago, reverse = TRUE)
df$F_Score <- min_max_normalize(df$Purchase_Frequency)
df$M_Score <- min_max_normalize(df$Total_Spending)
df$RFM_Score <- 0.2 * df$R_Score + 0.3 * df$F_Score + 0.5 * df$M_Score

# 计算意向(I)权重，将0-100的I_Score转换为0-0.2的权重
df$I_Weight <- df$I_Score / 500

# 计算最终得分：RFM得分 * (1 + I权重)
df$Final_Score <- df$RFM_Score * (1 + df$I_Weight)

# 打印各得分范围
cat(sprintf("R_Score 范围: [%.2f, %.2f]\n", min(df$R_Score, na.rm = TRUE), max(df$R_Score, na.rm = TRUE)))
cat(sprintf("F_Score 范围: [%.2f, %.2f]\n", min(df$F_Score, na.rm = TRUE), max(df$F_Score, na.rm = TRUE)))
cat(sprintf("M_Score 范围: [%.2f, %.2f]\n", min(df$M_Score, na.rm = TRUE), max(df$M_Score, na.rm = TRUE)))
cat(sprintf("RFM_Score 范围: [%.2f, %.2f]\n", min(df$RFM_Score, na.rm = TRUE), max(df$RFM_Score, na.rm = TRUE)))
cat(sprintf("I_Weight 范围: [%.2f, %.2f]\n", min(df$I_Weight, na.rm = TRUE), max(df$I_Weight, na.rm = TRUE)))
cat(sprintf("Final_Score 范围: [%.2f, %.2f]\n", min(df$Final_Score, na.rm = TRUE), max(df$Final_Score, na.rm = TRUE)))


##### 用户分层与画像 #####
# 计算摩擦度的60%分位数（用于修正逻辑）
friction_q60 <- quantile(df$Friction, 0.6, na.rm = TRUE)

# 定义用户分类函数
classify_user_func <- function(r, f, m, i, income, friction, l) {
  # 基础RFM分层逻辑：基于R、F、M得分的三维分类，得到8种基础用户分层
  if (r > 60 && f > 60 && m > 60) { base_label <- "重要价值用户"
  } else if (r > 60 && m > 60 && f < 40) { base_label <- "重要发展用户"
  } else if (r < 40 && f > 60 && m > 60) { base_label <- "重要保持用户"
  } else if (r < 40 && m > 60) { base_label <- "重要挽留用户"
  } else if (r > 60 && f > 40 && m < 40) { base_label <- "一般发展用户"
  } else if (r > 60 && f < 40 && m < 40) { base_label <- "一般维持用户"
  } else if (r < 40 && f < 40 && m < 40) { base_label <- "低价值用户"
  } else { base_label <- "一般用户"
  }
  
  # 关键修正逻辑：得到7种修正用户分层，识别出了特殊的用户群体
  if (base_label %in% c("低价值用户", "一般维持用户") && income == "High" && i > 60) {
    return("高潜沉睡用户")}
  if (income == "High" && i > 70 && f < 40 && m < 50) {
    return("纠结土豪")}
  if (base_label %in% c("一般维持用户", "一般发展用户") && i > 80 && friction > friction_q60) {
    return("犹豫型潜力用户")}
  if (income == "Low" && i > 70 && f < 40) {
    return("隐形活跃者")}
  if (income == "High" && r < 40 && m > 50) {
    return("高潜流失客")}
  if (m > 70 && f > 70 && i > 60) {
    return("核心VIP")}
  if (income == "Low" && i < 40 && m < 40) {
    return("羊毛党/低值")
  }
  
  return(base_label)
}

# 将分类函数应用到每一行
df$User_Segment <- mapply(classify_user_func,
                          df$R_Score, df$F_Score, df$M_Score, df$I_Score,
                          df$Income_Level, df$Friction, df$L_Score)

# 分层统计：最终按照用户数排序
segment_stats <- df %>%
  group_by(User_Segment) %>%
  summarise(用户数 = n(),
            平均消费 = round(mean(Total_Spending, na.rm = TRUE), 2),
            平均购买频率 = round(mean(Purchase_Frequency, na.rm = TRUE), 2),
            平均意向分 = round(mean(I_Score, na.rm = TRUE), 2),
            平均收入 = round(mean(Income, na.rm = TRUE), 2),
            平均综合分 = round(mean(Final_Score, na.rm = TRUE), 2)) %>%
  mutate(占比 = round(用户数 / nrow(df) * 100, 1)) %>%
  arrange(desc(用户数))
print(as.data.frame(segment_stats), row.names = FALSE)


##### 生成用户画像可视化 #####
# 1. 绘制雷达图
# 计算每个分层的指标平均值
segments <- unique(df$User_Segment)
metrics <- c('R_Score', 'F_Score', 'M_Score', 'I_Score')
segment_means <- df %>% group_by(User_Segment) %>%
  summarise(across(all_of(metrics), mean, na.rm = TRUE)) %>%
  as.data.frame()
rownames(segment_means) <- segment_means$User_Segment

# 设置保存路径、绘图布局（3行5列）与颜色
png(file.path(output_dir, "segment_radar.png"),
    width = 1800, height = 1100, res = 120)
par(mfrow = c(3, 5), mar = c(3, 3, 5, 3), oma = c(1, 1, 1, 1))
showtext_auto()
colors_border <- colorRampPalette(brewer.pal(8, "Set3"))(length(segments))
colors_fill <- scales::alpha(colors_border, 0.3)

for (i in 1:length(segments)) {
  segment_name <- segments[i]
  values <- as.numeric(segment_means[segment_name, metrics])
  
  # 构造fmsb数据框：前两行必须是Max和Min
  radar_df <- data.frame(
    R = c(100, 0, values[1]),
    F = c(100, 0, values[2]),
    M = c(100, 0, values[3]),
    I = c(100, 0, values[4])
  )
  
  # 绘制雷达图
  radarchart(radar_df, axistype = 1,
             # 形状设置
             pcol = colors_border[i], pfcol = colors_fill[i], 
             plwd = 2, plty = 1,
             # 网格设置
             cglcol = "grey80", cglty = 1, cglwd = 0.8,
             axislabcol = "grey50", caxislabels = seq(0, 100, 25),
             # 标签设置
             vlcex = 1.1)
  
  # line = 1 表示在绘图区上方第1行，cex 是字号，font = 2 为加粗
  mtext(segment_name, side = 3, line = 1.5, cex = 1.2, font = 2, col = "black")
}

dev.off() # 关闭图形设备

# 2. 创建分层分布柱状图
segment_counts <- df %>% count(User_Segment) %>%
  mutate(percentage = n / nrow(df) * 100) %>%
  arrange(desc(n))

p <- ggplot(segment_counts, aes(x = n, y = reorder(User_Segment, -n))) +
  geom_bar(stat = "identity", fill = "#0B5CAD", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = sprintf("%d (%.1f%%)", n, percentage)), 
            hjust = 0, nudge_x = 2, size = 3.5, color = "black") +
  labs(x = "用户数", y = "用户分层", title = "用户分层分布") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))  # 扩展x轴空间

ggsave(file.path(output_dir, "segment_distribution.png"), p, 
       width = 12, height = 6, dpi = 150)

# 重置图形参数
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)


##### ROI测算 #####
## 1. 基本参数：可以调整具体参数
total_budget <- 10000 # 总预算（元）
coupon_cost <- 10 # 每张优惠券成本（元）
aov <- mean(df$Average_Order_Value, na.rm = TRUE)  # 平均客单价（元）：从数据中动态计算

# 转化率假设（可根据历史数据校准）
# 核心用户
core_natural <- 0.25            # 核心用户无券自然转化率
core_with_coupon <- 0.30        # 核心用户发券后转化率
# 潜力用户
potential_natural <- 0.01       # 潜力用户自然转化率
potential_with_coupon <- 0.20   # 潜力用户发券后转化率（可调高至20%-25%）
# 一般用户（用于方案A的基线）
general_natural <- 0.02          # 一般用户自然转化率
# RFM前20%用户的自然转化率（介于核心和一般之间）
rfm20_natural <- 0.20           # 可根据实际数据调整，例如：RFM高分用户通常转化率较高
rfm20_with_coupon <- 0.30       # 发券后转化率（通常不会超过核心用户）

# 转化率边际提升
core_lift <- core_with_coupon - core_natural
potential_lift <- potential_with_coupon - potential_natural
rfm20_lift <- rfm20_with_coupon - rfm20_natural

## 2. 方案A：传统RFM策略
# 计算RFM得分的前20%阈值
top20_threshold <- quantile(df$RFM_Score, 0.80, na.rm = TRUE)
rfm_top20 <- df[df$RFM_Score >= top20_threshold, ]

# 计算目标用户数（不超过预算和用户总数）
target_a_count <- min(nrow(rfm_top20), floor(total_budget / coupon_cost))
# 计算成本

cost_a <- target_a_count * coupon_cost
# 计算边际收益和ROI
incremental_revenue_a <- target_a_count * rfm20_lift * aov
roi_a <- ifelse(cost_a > 0, (incremental_revenue_a - cost_a) / cost_a * 100, 0)

## 2. 方案B：优化RFI策略
# 定义核心用户
core_users <- df[df$User_Segment %in% c("核心VIP", "重要价值用户"), ]
# 定义潜在用户
potential_users <- df[df$User_Segment %in% c("纠结土豪", "高潜沉睡用户", 
                                             "犹豫型潜力用户", "高潜流失客"), ]

# 分配预算：优先核心用户（用80%预算），剩余给潜力用户
core_count <- min(nrow(core_users), floor(total_budget * 0.8 / coupon_cost))
remaining_budget <- total_budget - core_count * coupon_cost
potential_count <- min(nrow(potential_users), floor(remaining_budget / coupon_cost))

# 计算目标用户数
target_b_count <- core_count + potential_count
# 计算成本
cost_b <- target_b_count * coupon_cost

# 计算边际收益和ROI
incremental_revenue_b <- core_count * core_lift * aov + potential_count * potential_lift * aov
roi_b <- ifelse(cost_b > 0, (incremental_revenue_b - cost_b) / cost_b * 100, 0)

# 输出结果
cat(sprintf("  总预算: %d元\n", total_budget))
cat(sprintf("  优惠券成本: %d元/人\n", coupon_cost))
cat(sprintf("  平均客单价: %.0f元\n", aov))

cat(sprintf("\n【方案A - 传统RFM策略】\n"))
cat(sprintf("  目标用户数: %d\n", target_a_count))
cat(sprintf("  成本: %.0f元\n", cost_a))
cat(sprintf("  边际收益（增量）: %.0f元\n", incremental_revenue_a))
cat(sprintf("  边际ROI: %.1f%%\n", roi_a))

cat(sprintf("\n【方案B - 优化RFI策略】\n"))
cat(sprintf("  核心用户: %d\n", core_count))
cat(sprintf("  新挖掘潜力用户: %d\n", potential_count))
cat(sprintf("  总目标用户数: %d\n", target_b_count))
cat(sprintf("  实际成本: %.0f元 (预算使用率: %.1f%%)\n", cost_b, cost_b/total_budget*100))
cat(sprintf("  边际收益（增量）: %.0f元\n", incremental_revenue_b))
cat(sprintf("  边际ROI: %.1f%%\n", roi_b))

## 3. 图1：边际ROI对比可视化
strategies <- c("Traditional RFM", "Optimized RFI")
rois <- c(roi_a, roi_b)
colors <- c("#FF6B6B", "#0B5CAD")
roi_df <- data.frame(Strategy = strategies, ROI = rois, Color = colors)
# 将strategy转为因子并指定顺序（否则会按照字母顺序排列）
roi_df$Strategy <- factor(roi_df$Strategy, levels = c("Traditional RFM", "Optimized RFI"))

p1 <- ggplot(roi_df, aes(x = Strategy, y = ROI, fill = Strategy)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "black") +
  scale_fill_manual(values = colors) +
  geom_text(aes(label = sprintf("%.1f%%", ROI)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  labs(title = "边际ROI对比：传统RFM vs 优化RFI", 
       y = "边际ROI (%)", x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## 4. 图2：边际收益构成对比
marginal_data <- data.frame(
  Strategy = rep(strategies, each = 2),
  User_Type = rep(c("核心用户", "潜力用户"), 2),
  Marginal_Revenue = c(
    incremental_revenue_a, 0, # Traditional RFM: 方案A只有核心用户收益
    core_count * core_lift * aov, # Optimized RFI: 方案B核心用户收益
    potential_count * potential_lift * aov # Optimized RFI: 方案B潜力用户收益
  )
)
# 类似地，指定为因子：指定x轴顺序与堆叠顺序（核心在下，潜力在上）
marginal_data$Strategy <- factor(marginal_data$Strategy, levels = c("Traditional RFM", "Optimized RFI"))
marginal_data$User_Type <- factor(marginal_data$User_Type, levels = c("潜力用户", "核心用户"))

p2 <- ggplot(marginal_data, aes(x = Strategy, y = Marginal_Revenue, fill = User_Type)) +
  geom_col(alpha = 0.8) + 
  scale_fill_manual(values = c("核心用户" = "#0B5CAD", "潜力用户" = "#4ECDC4")) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "边际收益构成对比", 
       y = "边际收益 (元)", x = "", fill = "用户类型") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        legend.position = "right")

## 5. 组合并保存图形
combined_plot <- grid.arrange(p1, p2, nrow = 1, widths = c(1, 1.2))
ggsave(file.path(output_dir, "roi_comparison.png"), 
       combined_plot, width = 14, height = 5, dpi = 150)

## 6. 保存计算的数据框
write.xlsx(df, paste0(output_dir, "/rfm_analysis_results.xlsx"))


