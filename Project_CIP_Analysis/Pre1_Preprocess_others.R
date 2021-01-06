
# -------------------------------------------------------------------------
# Preprocess to extract time-series to a row
# -------------------------------------------------------------------------
library(e1071)
library(dplyr)
# -------------------------------------------------------------------------

rinse_training_labels <- read.csv(
  'sustainable-industry-rinse-over-run-training-labels.csv', sep=";")

rinse_training_values <- read.csv(
  'sustainable-industry-rinse-over-run-training-set-values.csv', sep=";")

prepocess_time <- read.csv('prepocess_time.csv')
##############################################################################
# ----------------------------------------------------------------------------
names(rinse_training_values)

# 移除變數 "object_id", "supply_flow", "return_turbidity", "return_flow", "tank_lsh_acid", "tank_lsh_pre_rinse"
# 移除所有Boolean變數
drop_col <- c("object_id", "supply_flow", "return_turbidity", "return_flow", "tank_lsh_acid", "tank_lsh_pre_rinse")
rinse_training_values_con <- rinse_training_values[ , !(names(rinse_training_values) %in% drop_col)]
rinse_training_values_con <- cbind(rinse_training_values_con[, sapply(rinse_training_values_con, is.integer) | 
                              sapply(rinse_training_values_con,is.numeric)], rinse_training_values_con$phase) 
colnames(rinse_training_values_con)[14] <- 'phase'

##############################################################################
# ----------------------------------------------------------------------------
# group by process, get summary
rinse_phase_summary <- 
  rinse_training_values_con %>%
  group_by(process_id, phase) %>%
  summarise_all(list(sum = sum, 
                     mean = mean,
                     max = max,
                     min = min,
                     var = var #,
                     # skewness = skewness,
                     # kurtosis = kurtosis
                     ))%>% as.data.frame()
rinse_phase_summary[is.na(rinse_phase_summary)] <- 0

# Reshape，將每個phase中的變數接轉為feature
d <- reshape(rinse_phase_summary, idvar = "process_id", timevar = "phase", direction = 'wide')
d[is.na(d)] <- 0

d_merge <- merge(x = d, y = prepocess_time, by = "process_id", all.x = TRUE)
d_merge <- merge(x = d_merge, y = rinse_training_labels, by = "process_id", all.x = TRUE)

write.csv(rinse_phase_summary, 'preprocess_v1_with_phase.csv', row.names = FALSE)
write.csv(d_merge, 'preprocess_v1.csv', row.names = FALSE)

##############################################################################
# -------------------------------------------------------------------------
# 包含偏度和峰態的檔案

# group by process, get summary
rinse_phase_summary_withKS <- 
  rinse_training_values_con %>%
  group_by(process_id, phase) %>%
  summarise_all(list(sum = sum, 
                     mean = mean,
                     max = max,
                     min = min,
                     var = var ,
                     skewness = skewness,
                     kurtosis = kurtosis
  ))%>% as.data.frame()
rinse_phase_summary_withKS[is.na(rinse_phase_summary_withKS)] <- 0

# Reshape，將每個phase中的變數接轉為feature
d_KS <- reshape(rinse_phase_summary_withKS, idvar = "process_id", timevar = "phase", direction = 'wide')
d_KS[is.na(d_KS)] <- 0

dKS_merge <- merge(x = d_KS, y = prepocess_time, by = "process_id", all.x = TRUE)
dKS_merge <- merge(x = dKS_merge, y = rinse_training_labels, by = "process_id", all.x = TRUE)

write.csv(rinse_phase_summary_withKS, 'preprocess_v1KS_with_phase.csv', row.names = FALSE)
write.csv(dKS_merge, 'preprocess_v1KS.csv', row.names = FALSE)
