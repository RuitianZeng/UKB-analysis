#输入模型
#糖尿病为中介，AS_added为自变量
model <- glm(E11_status_cox ~ AS_added+Age.at.recruitment+
              Sex+Ethnicity +Townsend.deprivation.index.at.recruitment+Edu_pro+Smoking.status...Instance.0+Alcohol.drinker.status...Instance.0+
              Physical_activity_per_week+
              Energy_intake+Sugar_intake+Sodium_intake+Red_meat_intake+Processed_meat_intake+Vegetable_intake+Fruit_intake+Vitamin_supplemention+Fiber_intake+
              Monounsaturated_fatty_acids+Saturated_fatty_acids,data=data)
#输出模型
#抑郁症为结局
model2 <- survreg(Surv(depressive_episode_time_cox,depressive_episode_status_cox>0) ~ AS_added+E11_status_cox+Age.at.recruitment+
                  Sex+Ethnicity +Townsend.deprivation.index.at.recruitment+Edu_pro+Smoking.status...Instance.0+Alcohol.drinker.status...Instance.0+
                  Physical_activity_per_week+
                  Energy_intake+Sugar_intake+Sodium_intake+Red_meat_intake+Processed_meat_intake+Vegetable_intake+Fruit_intake+Vitamin_supplemention+Fiber_intake+
                  Monounsaturated_fatty_acids+Saturated_fatty_acids,data = data)
#中介模型
library(mediation)
med <- mediate(model,model2,treat = "AS_added",
               mediator = "E11_status_cox",outcome = "depressive_episode_time_cox",robustSE = T,boot = T, sims = 100)
