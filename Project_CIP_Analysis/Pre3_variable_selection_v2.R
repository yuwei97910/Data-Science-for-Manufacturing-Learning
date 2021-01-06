
library(rpart)
library(partykit)
library(dplyr)
# -------------------------------------------------------------------------------------

rinse_turbidity <- read.csv("preprocess_v2-2.csv")
attach(rinse_turbidity)

# Remove X and final_rinse_total_turbidity_liter
rinse_turbidity <- rinse_turbidity[ , !names(rinse_turbidity) %in% c("X", "final_rinse_total_turbidity_liter")]

# V1
#######################################################################################
# 變數挑選
#######################################################################################
# -------------------------------------------------------------------------------------
# Correlation - 移除共線性之變數

corr_var = data.frame(v1 = '', v2 = '')
for (i in c(1:(ncol(rinse_turbidity)-1))) {
  for (j in c((i+1):ncol(rinse_turbidity))) {
    corr <- cor(rinse_turbidity[,i], rinse_turbidity[,j])
    if(corr >= 0.9){
      corr_var <- merge(corr_var, data.frame(v1 = colnames(rinse_turbidity)[i], v2 = colnames(rinse_turbidity)[j]), all = TRUE)
    }
  }
}

i = 2
while (i < ncol(rinse_turbidity)) {
  j = i + 1
  while (j < ncol(rinse_turbidity)) {
    corr <- cor(rinse_turbidity[,i], rinse_turbidity[,j])
    if(corr >= 0.9){
      rinse_turbidity <- rinse_turbidity[, -c(j)]
    }
    j = j + 1
  }
  i = i + 1
}

write.csv(rinse_turbidity, 'preprocess_v2_remove_cor.csv', row.names = FALSE)


# -------------------------------------------------------------------------------------
# Decision Tree - 挑選重要變數

cart.model<- rpart(log(turbidity) ~., 
                   data = rinse_turbidity)
rparty.tree <- as.party(cart.model) # 轉換cart決策樹
rparty.tree # 輸出各節點的細部資訊
plot(rparty.tree) 

important_var <- row.names(as.data.frame(cart.model$variable.importance))

out_put_var = append(important_var[1:20], c('process_id', 'turbidity')) # 取出樹當中的30個重要變數
rinse_turbidity_important <- rinse_turbidity[, names(rinse_turbidity) %in% (out_put_var)]

write.csv(rinse_turbidity_important, 'preprocess_v2_treeslect.csv', row.names = FALSE)

#######################################################################################
# -------------------------------------------------------------------------------------
# Scaled the data rinse_turbidity
rinse_turbidity_scaled <- rinse_turbidity
rinse_turbidity_scaled[,2:147] <- sapply(rinse_turbidity_scaled[,2:147], scale)

fit1 <- lm(log(turbidity) ~ ., data = rinse_turbidity_scaled)
summary(fit1)
fit2 <- lm(log(turbidity) ~ ., data = rinse_turbidity_important)
summary(fit2)
