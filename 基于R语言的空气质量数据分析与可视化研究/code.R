#数据准备
# 声明
library(readxl)
# 设置文件路径
file_path <- "D:\\Rproject\\AirQualityUCI.xlsx"
# 导入 Excel 文件
data <- read_excel(file_path)
# 获取除了 'Date' 和 'Time' 列以外的列名
cols_to_modify <- setdiff(names(data), c("Date", "Time"))
# 替换这些列中的 -200 为 NA
data[cols_to_modify][data[cols_to_modify] == -200] <- NA
# 定义要分析的变量
variables <- c("CO(GT)", "NMHC(GT)", "C6H6(GT)", "NOx(GT)", "NO2(GT)", "T", "RH", "AH")
#summary函数
summary(data[variables])
# sapply()函数
# 加载必要的包
if (!require(dplyr)) install.packages("dplyr")
if (!require(moments)) install.packages("moments")
#声明
library(dplyr)
library(moments)
# 使用 sapply() 对每个变量计算描述性统计量
stats <- sapply(data[variables], function(x) {
  c(
    Mean = mean(x, na.rm = TRUE),          # 平均值
    SD = sd(x, na.rm = TRUE),              # 标准差
    Median = median(x, na.rm = TRUE),      # 中位数
    Skewness = skewness(x, na.rm = TRUE),  # 偏态系数
    Min = min(x, na.rm = TRUE),            # 最小值
    Max = max(x, na.rm = TRUE),            # 最大值
    Quantiles = quantile(x, na.rm = TRUE), # 四分位数
    NA_Count = sum(is.na(x))               # 缺失值计数
  )
})
# 打印结果
print(stats)
#统计异常值
colSums(is.na(data))
# 计算分类变量的频数分布
table(data$"CO(GT)")
table(data$"NMHC(GT)")
table(data$"C6H6(GT)")
table(data$"NOx(GT)")
table(data$"NO2(GT)")
table(data$T)
table(data$RH)
table(data$AH)
# 选择变量并计算它们的相关系数矩阵
correlation_matrix <- cor(data[c("CO(GT)", "NMHC(GT)", "C6H6(GT)","NOx(GT)","NO2(GT)")], use = "complete.obs", method = "pearson")
print(correlation_matrix)
# cor.test相关性检验
var1 <- data$`CO(GT)`
var2 <- data$`NMHC(GT)`
# 使用 cor.test 进行皮尔逊相关性检验
test_result <- cor.test(var1, var2, alternative = "two.sided", method = "pearson")
print(test_result)
# cor2pcor计算偏相关系数矩阵
if (!require(corpcor)) {
  install.packages("corpcor")}
library(corpcor)
vars <- data[, c("CO(GT)", "NMHC(GT)", "NO2(GT)")]
# 计算相关系数矩阵
cor_matrix <- cor(vars, use = "complete.obs")
# 使用 cor2pcor 函数将相关系数矩阵转换为偏相关系数矩阵
pcor_matrix <- cor2pcor(cor_matrix)
print(pcor_matrix)
#卡方检验
RH_clean <- na.omit(data$RH)
AH_clean <- na.omit(data$AH)
# 将 RH 和 AH 离散化为 3 个区间
RH_binned <- cut(RH_clean, breaks = 3, labels = c("Low", "Medium", "High"))
AH_binned <- cut(AH_clean, breaks = 3, labels = c("Low", "Medium", "High"))
# 创建 RH 和 AH 的列联表
contingency_table_RH_AH <- table(RH_binned, AH_binned)
# 进行卡方检验
chisq_test_result_RH_AH_no_correction <- chisq.test(contingency_table_RH_AH, correct = FALSE)
print(chisq_test_result_RH_AH_no_correction)
#计算边际频数 边际百分比
# 创建列联表
contingency_table_RH_AH <- table(RH_binned, AH_binned)
# 计算 RH（相对湿度）的边际频数
marginal_RH <- margin.table(contingency_table_RH_AH, margin = 1)  # margin = 1 表示按行计算边际频数
# 计算 AH（绝对湿度）的边际频数
marginal_AH <- margin.table(contingency_table_RH_AH, margin = 2)  # margin = 2 表示按列计算边际频数
print("边际频数（RH）:")
print(marginal_RH)
print("边际频数（AH）:")
print(marginal_AH)
# 计算 RH（相对湿度）的边际百分比
prop_RH <- prop.table(contingency_table_RH_AH, margin = 1)*100  # margin = 1 按行计算比例
# 计算 AH（绝对湿度）的边际百分比
prop_AH <- prop.table(contingency_table_RH_AH, margin = 2) *100 # margin = 2 按列计算比例
# 输出边际百分比
print(prop_RH)
print(prop_AH)
#茎叶图
stem(data$T)
#箱线图
variables <- c("CO(GT)", "NMHC(GT)", "C6H6(GT)", "NOx(GT)", "NO2(GT)", "T", "RH", "AH")
variable_names <- c("CO(GT)" = "一氧化碳浓度", 
                    "NMHC(GT)" = "非甲烷总烃浓度", 
                    "C6H6(GT)" = "苯浓度", 
                    "NOx(GT)" = "氮氧化物浓度", 
                    "NO2(GT)" = "二氧化氮浓度", 
                    "T" = "温度", 
                    "RH" = "相对湿度", 
                    "AH" = "绝对湿度")
par(mfrow = c(2, 4), mar = c(5, 5, 3, 1))  
for (var in variables) {
  boxplot(data[[var]], 
          main = paste(variable_names[var], "的箱线图"),  
          ylab = variable_names[var],   
          col = "lightblue",          
          border = "darkblue",         
          notch = TRUE,              
          outline = TRUE,              
          las = 1,                     
          cex.axis = 1.2,              
          cex.lab = 1.2)               
}
#直方图和密度图
T_clean <- na.omit(data$T)
RH_clean <- na.omit(data$RH)
AH_clean <- na.omit(data$AH)
par(mfrow = c(3, 2))
# T 的直方图和密度图
hist(T_clean, main = "T 的直方图", xlab = "T (温度)", col = "skyblue", border = "white")
plot(density(T_clean), main = "T 的密度图", xlab = "T (温度)", col = "blue", lwd = 2)
# RH 的直方图和密度图
hist(RH_clean, main = "RH 的直方图", xlab = "RH (相对湿度)", col = "lightgreen", border = "white")
plot(density(RH_clean), main = "RH 的密度图", xlab = "RH (相对湿度)", col = "darkgreen", lwd = 2)
# AH 的直方图和密度图
hist(AH_clean, main = "AH 的直方图", xlab = "AH (绝对湿度)", col = "orange", border = "white")
plot(density(AH_clean), main = "AH 的密度图", xlab = "AH (绝对湿度)", col = "red", lwd = 2)
#绘制小提琴图
if (!require(vioplot)) {
  install.packages("vioplot")
}
library(vioplot)
T_clean <- na.omit(data$T)
RH_clean <- na.omit(data$RH)
AH_clean <- na.omit(data$AH)
dev.new(width = 10, height = 8) 
par(mar = c(5, 5, 4, 2) + 0.1) 
# 绘制小提琴图
vioplot(T_clean, RH_clean, AH_clean,
        names = c("T (温度)", "RH (相对湿度)", "AH (绝对湿度)"),
        col = c("skyblue", "lightgreen", "orange"))
title("T、RH 和 AH 的小提琴图", cex.main = 1.5)  # 调整标题大小
#克利夫兰图
T_clean <- na.omit(data$T)
RH_clean <- na.omit(data$RH)
AH_clean <- na.omit(data$AH)
par(mfrow = c(1, 3)) 
# 绘制 T 的克利夫兰图
dotchart(T_clean[1:100], main = "T 的克利夫兰图", xlab = "T (温度)", col = "blue")
# 绘制 RH 的克利夫兰图
dotchart(RH_clean[1:100], main = "RH 的克利夫兰图", xlab = "RH (相对湿度)", col = "green")
# 绘制 AH 的克利夫兰图
dotchart(AH_clean[1:100], main = "AH 的克利夫兰图", xlab = "AH (绝对湿度)", col = "orange")
par(mfrow = c(1, 1))  
#绘制折线图
T_clean <- na.omit(data$T)
RH_clean <- na.omit(data$RH)
AH_clean <- na.omit(data$AH)
# 检查三者是否长度一致（截取最小长度）
min_length <- min(length(T_clean), length(RH_clean), length(AH_clean))
T_clean <- T_clean[1:min_length]
RH_clean <- RH_clean[1:min_length]
AH_clean <- AH_clean[1:min_length]
# 绘制 T 的线图
plot(T_clean, type = "l", col = "blue", lwd = 2, ylim = range(c(T_clean, RH_clean, AH_clean)),
     xlab = "索引", ylab = "数值", main = "T、RH 和 AH 的线图", 
     cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.1) 
# 添加 RH 的线图
lines(RH_clean, col = "green", lwd = 2, lty = 2)
# 添加 AH 的线图
lines(AH_clean, col = "orange", lwd = 2, lty = 3)
# 添加网格线
grid(lwd = 1, col = "gray", lty = "dotted")  # 添加灰色虚线网格
# 添加图例
legend("topright", legend = c("T (温度)", "RH (相对湿度)", "AH (绝对湿度)"),
       col = c("blue", "green", "orange"), lty = c(1, 2, 3), lwd = 2, 
       cex = 1.2, box.lwd = 2, bg = "white")  
# 绘制柱状图
CO_clean <- as.numeric(na.omit(data$`CO(GT)`))
barplot(CO_clean,
        main = "CO(GT) 的柱状图", 
               ylab = "CO (GT)",           
        col = rainbow(length(CO_clean)),
        border = "black",
        space = 0.5,               
        las = 2)                    
# 绘制饼图
CO_clean <- as.numeric(na.omit(data$`CO(GT)`))
# 划定 CO(GT) 的区间
breaks <- c(0, 1, 3, 6, 12)  
CO_groups <- cut(CO_clean, breaks = breaks, right = FALSE, labels = c("0-1", "1-3", "3-6", "6-12"))
# 计算每个区间的频数
CO_freq <- table(CO_groups)
pie(CO_freq, 
    main = "CO(GT) 的区间分布",    
    col = rainbow(length(CO_freq)),  
    labels = paste(names(CO_freq), "\n", CO_freq),  
    cex.main = 1.5,                   
    cex.lab = 1.2)                    
# 使用 plot 绘制散点图
plot(RH_clean, 
     type = "p",               
     col = rgb(0, 1, 0, 0.5),   
     pch = 19,                  
          ylab = "RH (相对湿度)",   
     main = "RH 散点图",        
     cex.main = 1.6,           
     cex.lab = 1.3,             
     cex.axis = 1.2,            
     col.axis = "darkblue",     
     col.lab = "darkgreen",     
     las = 1,                  
     bg = "lightgray")          
# 使用 smoothScatter 绘制平滑散点图
RH_clean <- na.omit(data$RH)
# 使用 smoothScatter 绘制平滑散点图
smoothScatter(1:length(RH_clean), RH_clean, 
              main = "RH的高密度散点图", 
              xlab = "Index", 
              ylab = "RH (Relative Humidity)",
              colramp = colorRampPalette(c("white", "blue", "green", "yellow", "red")), # 设置渐变色
              pch = 16)  
#使用 pairs 绘制矩阵散点图，并添加回归线
#载入包
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
data_matrix <- data.frame(T_clean, RH_clean, AH_clean)
panel_with_regression <- function(x, y, ...) {
  # 绘制散点图
  points(x, y, ...)
  # 绘制回归线
  abline(lm(y ~ x), col = "red", lwd = 2)  # 红色回归线
}
# 使用 pairs 绘制矩阵散点图，并添加回归线
pairs(data_matrix, 
      main = "散点图矩阵带回归线",   
      col = rgb(0, 0, 1, 0.5),       
      pch = 19,                       
      cex.main = 1.6,                 
      cex.lab = 1.3,                   
      cex.axis = 1.2,                  
      col.axis = "darkblue",         
      col.lab = "darkgreen",           
      panel = panel_with_regression)   
# 绘制 Q-Q 图
# 设置图形布局：1行3列
par(mfrow = c(1, 3))
# 绘制 T_clean (温度) 的 Q-Q 图
qqnorm(T_clean, main = "温度 (T) 的 Q-Q 图", col = "blue", pch = 19)
qqline(T_clean, col = "red", lwd = 2)
# 绘制 AH_clean (绝对湿度) 的 Q-Q 图
qqnorm(AH_clean, main = "绝对湿度 (AH) 的 Q-Q 图", col = "green", pch = 19)
qqline(AH_clean, col = "orange", lwd = 2)
# 绘制 RH_clean (相对湿度) 的 Q-Q 图
qqnorm(RH_clean, main = "相对湿度 (RH) 的 Q-Q 图", col = "purple", pch = 19)
qqline(RH_clean, col = "brown", lwd = 2)
#检验方差同质性 
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
if (!requireNamespace("pwr", quietly = TRUE)) {
  install.packages("pwr")
}
library(car)
library(pwr)
# 提取 6:00 和 18:00 的数据，并去除 CO(GT) 列的 NA 值
data_combined <- na.omit(data.frame(
  CO_GT = c(data$`CO(GT)`[data$Time == "6:00:00"], 
            data$`CO(GT)`[data$Time == "18:00:00"]),
  Time = factor(rep(c("6:00 AM", "6:00 PM"), 
                   times = c(sum(data$Time == "6:00:00"), sum(data$Time == "18:00:00"))))
))
# Levene's Test 检验方差同质性
levene_result <- leveneTest(CO_GT ~ Time, data = data_combined)
print(levene_result)
# 独立样本 t 检验
data$Time <- as.character(data$Time)
data_6am <- na.omit(data$`CO(GT)`[data$Time == "6:00:00"])  # 6:00 的数据
data_6pm <- na.omit(data$`CO(GT)`[data$Time == "18:00:00"])  # 18:00 的数据
# 独立样本 t 检验（假设两组数据的方差不等）
t_test_result <- t.test(
  data_6am, data_6pm,
  paired = FALSE,        # 独立样本
  var.equal = FALSE,     # 假定方差不等
  mu = 0,                # 假设均值差为 0
  alternative = "two.sided" # 双尾检验
)
print(t_test_result)
# 计算描述性统计量进行功效分析
mean_6am <- mean(data_6am) 
mean_6pm <- mean(data_6pm) 
sd_6am <- sd(data_6am)     
sd_6pm <- sd(data_6pm)   
n_6am <- length(data_6am)  
n_6pm <- length(data_6pm)   
# 计算效应量 (Cohen's d)
pooled_sd <- sqrt(((n_6am - 1) * sd_6am^2 + (n_6pm - 1) * sd_6pm^2) / (n_6am + n_6pm - 2))
d <- (mean_6am - mean_6pm) / pooled_sd  # Cohen's d
library(pwr)
power_analysis <- pwr.t2n.test(
  d = d,                # 效应量
  n1 = n_6am,           # 第一组样本量
  n2 = n_6pm,           # 第二组样本量
  sig.level = 0.05,      # 显著性水平
  power = NULL,          # 计算功效
  alternative = "two.sided" # 双尾检验
)
print(power_analysis)
#Wilcoxon 秩和检验
# 使用 Wilcoxon 秩和检验比较 6:00 AM 和 6:00 PM 的数据
wilcox_result <- wilcox.test(
  data_6am, data_6pm,
  paired = FALSE,       # 两组是独立样本
  alternative = "two.sided" # 双尾检验
)
print("Wilcoxon Test Results for 6:00 AM vs 6:00 PM:")
print(wilcox_result)
# 进行置换检验
if (!requireNamespace("coin", quietly = TRUE)) {
  install.packages("coin")
}
library(coin)
# 组合 6:00 AM 和 6:00 PM 的数据
data_combined <- data.frame(
  CO_GT = c(data_6am, data_6pm),  # `CO(GT)` 浓度
  Time = factor(rep(c("6:00 AM", "6:00 PM"), 
                   times = c(length(data_6am), length(data_6pm))))  # 时间变量
)
perm_test_result <- oneway_test(CO_GT ~ Time, data = data_combined, distribution = "approximate")
print("Permutation Test Results for 6:00 AM vs 6:00 PM:")
print(perm_test_result)
#单因素方差分析
anova_result <- aov(`CO(GT)` ~ AH, data )
anova(anova_result)
# 去除包含 NA 的行，并确保数据框长度一致
data_clean <- data[!is.na(data[["CO(GT)"]]) & !is.na(data[["AH"]]), ]
# 重新创建清洗后的 CO 和 AH 列
CO_clean <- data_clean[["CO(GT)"]]
AH_clean <- data_clean[["AH"]]
# 添加清洗后的 CO 和 AH 列
data_clean$CO_clean <- CO_clean
data_clean$AH_clean <- AH_clean
# 添加分组变量
data_clean$group <- ifelse(data_clean$Time >= "18:00:00" & data_clean$Time <= "22:00:00", "实验组", "对照组")
ancova_model <- aov(CO_clean ~ AH_clean + group, data = data_clean)
# 显示结果
summary(ancova_model)
# 多因素方差分析
data_clean <- na.omit(data)

# 多因素方差分析模型，查看T, RH, AH对CO(GT)的影响
model <- aov(`NO2(GT)` ~ T + RH + AH, data = data_clean)
# 显示结果
anova(model)
#线性回归模型
#创建一个新的数据框 processed_data
processed_data <- data
# 将 Date 列转换为 Date 类型，并提取 Month
processed_data$Date <- as.Date(processed_data$Date, format = "%Y/%m/%d")
processed_data$Month <- as.numeric(format(processed_data$Date, "%m"))
# 将 Time 列转换为 POSIXct 类型，并提取 Hour
processed_data$Time <- strptime(processed_data$Time, format = "%H:%M:%S")
processed_data$Hour <- as.numeric(format(processed_data$Time, "%H"))
# 线性回归模型：NO2(GT) ~ T + AH + Month + Hour
model <- lm(`NO2(GT)` ~ T + AH + Month + Hour, data = processed_data)
# 查看模型系数
print(coefficients(model))
# 查看模型摘要
print(summary(model))
# 获取模型系数的置信区间（95% 置信区间）
conf_intervals <- confint(model, level = 0.95)
print(conf_intervals)
new_data <- data.frame(
  T = c(20.5, 22.3, 18.2),  # 新的温度数据
  AH = c(0.80, 0.75, 0.78), # 新的绝对湿度数据
  Date = as.Date(c("2024/11/27", "2024/11/27", "2024/11/27")),  # 新的日期
  Time = c("14:30:00", "15:00:00", "16:30:00")  # 新的时间
)
# 将新数据中的 Date 和 Time 转换为 Month 和 Hour
new_data$Month <- as.numeric(format(new_data$Date, "%m"))
new_data$Hour <- as.numeric(format(strptime(new_data$Time, format = "%H:%M:%S"), "%H"))
# 使用 predict() 函数生成新数据的预测值
predictions <- predict(model, newdata = new_data, type = "response")
# 查看预测结果
print(predictions)
#模型参数
hatvalues(model)
rstudent(model)
cooks.distance(model)
library(car)
vif(model)