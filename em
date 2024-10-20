# 定义数据
Y1 <- 125
Y2 <- 18
Y3 <- 20
Y4 <- 34
n <- Y1 + Y2 + Y3 + Y4  # 总数n = 197

# 初始化参数theta
theta <- 0.5  # 初始值，可根据需要调整
tolerance <- 1e-6  # 收敛条件
max_iter <- 1000  # 最大迭代次数
log_likelihood <- c()  # 存储每次迭代的对数似然值

# EM算法
for (iter in 1:max_iter) {
  # E步：计算期望值Z1和Z2
  Z1 <- Y1 * (1/2) / (1/2 + theta/4)
  Z2 <- Y1 * (theta/4) / (1/2 + theta/4)
  
  # M步：更新theta的值
  theta_new <- (Z2 + Y4) / (Z1 + Z2 + Y2 + Y3 + Y4)
  
  # 计算对数似然值
  log_lik <- Y1 * log(1/2 + theta_new/4) + (Y2 + Y3) * log((1 - theta_new)/4) + Y4 * log(theta_new/4)
  log_likelihood <- c(log_likelihood, log_lik)
  
  # 检查是否收敛
  if (abs(theta_new - theta) < tolerance) {
    cat("算法收敛于第", iter, "次迭代\n")
    break
  }
  
  # 更新theta
  theta <- theta_new
}

# 输出结果
cat("估计的theta值为:", theta, "\n")
cat("最终的对数似然值为:", log_likelihood[length(log_likelihood)], "\n")

# 绘制对数似然值收敛情况
plot(log_likelihood, type = "l", xlab = "迭代次数", ylab = "对数似然值", main = "EM算法对数似然值收敛")