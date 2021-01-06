
library(rpart)
library(partykit)
library(dplyr)
# -------------------------------------------------------------------------------------

rinse_total <- read.csv("preprocess_v1.csv")
attach(rinse_total)

rinse_totalKS <- read.csv("preprocess_v1KS.csv")
attach(rinse_totalKS)

# Remove X
rinse_total <- rinse_total[ , !names(rinse_total) %in% c("X")]
rinse_totalKS <- rinse_totalKS[ , !names(rinse_totalKS) %in% c("X")]

# V1 - with K & S
#######################################################################################
# 變數挑選
#######################################################################################
# -------------------------------------------------------------------------------------
# Correlation - 移除共線性之變數

corr_var = data.frame(v1 = '', v2 = '')
for (i in c(1:(ncol(rinse_totalKS)-1))) {
  for (j in c((i+1):ncol(rinse_totalKS))) {
    corr <- cor(rinse_totalKS[,i], rinse_totalKS[,j])
    if(corr >= 0.9){
      corr_var <- merge(corr_var, data.frame(v1 = colnames(rinse_totalKS)[i], v2 = colnames(rinse_totalKS)[j]), all = TRUE)
    }
  }
}

i = 2
while (i < ncol(rinse_totalKS)) {
  j = i + 1
  while (j < ncol(rinse_totalKS)) {
    corr <- cor(rinse_totalKS[,i], rinse_totalKS[,j])
    if(corr >= 0.9){
      rinse_totalKS <- rinse_totalKS[, -c(j)]
    }
    j = j + 1
  }
  i = i + 1
}

write.csv(rinse_totalKS, 'preprocess_v1KS_remove_cor.csv', row.names = FALSE)


# -------------------------------------------------------------------------------------
# Decision Tree - 挑選重要變數

cart.model<- rpart(log(final_rinse_total_turbidity_liter) ~., 
                   data = rinse_totalKS)
rparty.tree <- as.party(cart.model) # 轉換cart決策樹
rparty.tree # 輸出各節點的細部資訊
plot(rparty.tree) 

important_var <- row.names(as.data.frame(cart.model$variable.importance))

out_put_var = append(important_var[1:20], c('process_id', 'turbidity')) # 取出樹當中的20個重要變數
rinse_totalKS_important <- rinse_totalKS[, names(rinse_totalKS) %in% (out_put_var)]

#write.csv(rinse_totalKS_important, 'preprocess_v1KS_treeslect.csv', row.names = FALSE)

#######################################################################################
# -------------------------------------------------------------------------------------
# Scaled the data rinse_turbidity
rinse_totalKS_scaled <- rinse_totalKS
rinse_totalKS_scaled[,2:147] <- sapply(rinse_totalKS_scaled[,2:147], scale)

rinse_totalKS_important_scaled <- rinse_totalKS_important
rinse_totalKS_important_scaled[,2:21] <- sapply(rinse_totalKS_important_scaled[,2:21], scale)


fit1 <- lm(log(final_rinse_total_turbidity_liter) ~ ., data = rinse_totalKS_scaled)
summary(fit1)
fit2 <- lm(log(final_rinse_total_turbidity_liter) ~ ., data = rinse_totalKS_important)
summary(fit2)


#######################################################################################
#######################################################################################

# V1 - without K & S
#######################################################################################
# 變數挑選
#######################################################################################
# -------------------------------------------------------------------------------------
# Correlation - 移除共線性之變數

corr_var = data.frame(v1 = '', v2 = '')
for (i in c(1:(ncol(rinse_total)-1))) {
  for (j in c((i+1):ncol(rinse_total))) {
    corr <- cor(rinse_total[,i], rinse_total[,j])
    if(corr >= 0.9){
      corr_var <- merge(corr_var, data.frame(v1 = colnames(rinse_total)[i], v2 = colnames(rinse_total)[j]), all = TRUE)
    }
  }
}

i = 2
while (i < ncol(rinse_total)) {
  j = i + 1
  while (j < ncol(rinse_total)) {
    corr <- cor(rinse_total[,i], rinse_total[,j])
    if(corr >= 0.9){
      rinse_total <- rinse_total[, -c(j)]
    }
    j = j + 1
  }
  i = i + 1
}

write.csv(rinse_total, 'preprocess_v1_remove_cor.csv', row.names = FALSE)

# -------------------------------------------------------------------------------------
# Decision Tree - 挑選重要變數

cart.model<- rpart(log(final_rinse_total_turbidity_liter) ~., 
                   data = rinse_total)
rparty.tree <- as.party(cart.model) # 轉換cart決策樹
rparty.tree # 輸出各節點的細部資訊
plot(rparty.tree) 

important_var <- row.names(as.data.frame(cart.model$variable.importance))

out_put_var = append(important_var[1:20], c('process_id', 'turbidity')) # 取出樹當中的30個重要變數
rinse_total_important <- rinse_total[, names(rinse_total) %in% (out_put_var)]

write.csv(rinse_total_important, 'preprocess_v1_treeslect.csv', row.names = FALSE)

#######################################################################################
# -------------------------------------------------------------------------------------
# Scaled the data rinse_turbidity
rinse_total_scaled <- rinse_total
rinse_total_scaled[,2:147] <- sapply(rinse_total_scaled[,2:147], scale)

rinse_total_important_scaled <- rinse_total_important
rinse_total_important_scaled[,2:21] <- sapply(rinse_total_important_scaled[,2:21], scale)

fit1 <- lm(log(final_rinse_total_turbidity_liter) ~ ., data = rinse_total_scaled)
summary(fit1)
fit2 <- lm(log(final_rinse_total_turbidity_liter) ~ ., data = rinse_total_important_scaled)
summary(fit2)



null = lm(log(final_rinse_total_turbidity_liter) ~ 1, data = rinse_total_scaled)  
full = lm(log(final_rinse_total_turbidity_liter) ~ ., data = rinse_total_scaled)

# 2.使用step()，一個一個把變數丟進去
fit_forward = step(null, scope = list(lower=null, upper=full), direction = "forward")
summary(fit_forward)
