# 1. 加载所需包
library(mediation)  # 用于中介分析

# 2. 构建中介模型（自变量 -> 中介变量）
# 因变量：糖尿病状态（E11_status_cox）
# 自变量：AS_added（核心自变量）
# 控制变量：人口学特征、生活方式、饮食因素等
model_mediator <- glm(
  formula = E11_status_cox ~ AS_added + 
    Age.at.recruitment + Sex + Ethnicity + 
    Townsend.deprivation.index.at.recruitment + Edu_pro + 
    Smoking.status...Instance.0 + Alcohol.drinker.status...Instance.0 +
    Physical_activity_per_week +
    Energy_intake + Sugar_intake + Sodium_intake + 
    Red_meat_intake + Processed_meat_intake + 
    Vegetable_intake + Fruit_intake + Vitamin_supplemention + 
    Fiber_intake + Monounsaturated_fatty_acids + Saturated_fatty_acids,
  data = data
  # 注意：若E11_status_cox为二分类变量，需添加 family = binomial(link = "logit")
)

# 3. 构建结果模型（自变量 + 中介变量 -> 结局变量）
# 因变量：抑郁症的生存数据（时间+状态）
# 包含变量：AS_added（自变量）、E11_status_cox（中介变量）及控制变量
model_outcome <- survreg(
  formula = Surv(depressive_episode_time_cox, depressive_episode_status_cox > 0) ~ 
    AS_added + E11_status_cox + 
    Age.at.recruitment + Sex + Ethnicity + 
    Townsend.deprivation.index.at.recruitment + Edu_pro + 
    Smoking.status...Instance.0 + Alcohol.drinker.status...Instance.0 +
    Physical_activity_per_week +
    Energy_intake + Sugar_intake + Sodium_intake + 
    Red_meat_intake + Processed_meat_intake + 
    Vegetable_intake + Fruit_intake + Vitamin_supplemention + 
    Fiber_intake + Monounsaturated_fatty_acids + Saturated_fatty_acids,
  data = data
)

# 4. 执行中介分析
# 处理变量：AS_added
# 中介变量：E11_status_cox（糖尿病状态）
# 结局变量：depressive_episode_status_cox（抑郁症状态）
med_result <- mediate(
  model = model_mediator,        # 中介模型
  model2 = model_outcome,        # 结果模型
  treat = "AS_added",            # 处理变量名称
  mediator = "E11_status_cox",   # 中介变量名称
  outcome = "depressive_episode_status_cox",  # 结局事件状态变量（修正为状态变量）
  robustSE = TRUE,               # 稳健标准误（非参数bootstrap时会被忽略）
  boot = TRUE,                   # 使用bootstrap法估计置信区间
  sims = 100                     # bootstrap重复次数（实际分析可增至1000+提高稳定性）
)

# 查看中介分析结果
summary(med_result)
