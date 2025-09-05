# 确保已加载必要的包
library(ggplot2)
library(dplyr)
correlation <- cor.test(data$DIGM_score, data$metabolic_signature, method = "pearson")
ρ_value <- round(correlation$estimate, 3)
data$DIGM_score <- as.factor(data$DIGM_score)
box_plot <- ggplot(data, aes(x = DIGM_score, y = metabolic_signature)) +
  # 先绘制误差线（放在底层）
  stat_boxplot(
    geom = "errorbar", 
    width = 0.2, 
    coef = 1.5,
    color = "black"
  ) + 
  # 再绘制箱体（放在上层，覆盖误差线）
  geom_boxplot(
    outlier.shape = NA,
    alpha = 1,  
    fill = "#c8daf2",          # 箱体填充色
    color = "black"          # 箱体边框色
  ) + 
  labs(
    x = "DIGM",
    y = "Metabolic Signature"
  ) +
  coord_cartesian(ylim = c(2,5.5)) +
  scale_y_continuous(breaks = seq(2, 5.5, by = 0.5)) + 
  annotate(
    "text", 
    x = -Inf, 
    y = Inf, 
    label = paste("R =", ρ_value), 
    hjust = -0.1, 
    vjust = 1.2, 
    size = 5
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank()  # 移除纵向网格线减少干扰
  )

# 保存图形到文件
ggsave(
  filename = "boxplot_DIGM_metabolic.pdf",  # 文件名
  plot = box_plot,                          # 要保存的图形对象
  device = "pdf",                           # 文件格式
  width = 8,                                # 宽度（英寸）
  height = 6,                               # 高度（英寸）
  dpi = 300,                                # 分辨率
  bg = "white"                              # 背景色
)
