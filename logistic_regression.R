##========================
## 1. TIỀN XỬ LÝ DỮ LIỆU
##========================

# 1.1 Đọc dữ liệu
add_data <- read.csv("D:/BTL/xstk/add.csv")

# 1.2 Thay tất cả dấu “?” (có thể kèm khoảng trắng) thành NA
add_data[ add_data == "?"      ] <- NA
add_data[ add_data == "   ?"   ] <- NA
add_data[ add_data == "     ?" ] <- NA

# 1.3 Xóa cột ID (X) và các cột url+term còn lại,
#     chỉ giữ lại 3 biến liên tục và biến target
add_data <- add_data[ , -1 ]    # xóa cột X ID Ko cần 
add_data <- add_data[, -c(5:1558)]   # giữ cột height,width,ratio,local,target
head(add_data,10)
# 1.4 Đổi tên cột cho rõ
colnames(add_data) <- c("height", "width", "ratio", "local","target_raw")

# 1.5 Chuyển biến target thành 0/1
add_data$target <- ifelse(trimws(add_data$target_raw) == "ad.", 1, 0)

# 1.6 Chuyển các biến height,width,ratio,local về numeric
add_data$height <- as.numeric(add_data$height)
add_data$width  <- as.numeric(add_data$width)
add_data$ratio  <- as.numeric(add_data$ratio)
add_data$local  <- as.numeric(add_data$local)

# 1.7 Bỏ cột target_raw không cần thiết
add_data$target_raw <- NULL

# 1.8 Kiểm tra cấu trúc và tóm tắt sơ bộ
str(add_data)
summary(add_data)

# 1.9 Kiểm tra dữ liệu khuyết
library(questionr)
freq.na(add_data)

# Vẽ biểu đồ missing value
library(ggplot2)

# Tạo dataframe cho missing values
missing_data <- as.data.frame(sapply(add_data, function(x) sum(is.na(x))))

# Đổi tên cột
colnames(missing_data) <- c("Missing Count")

# Tính tỷ lệ phần trăm missing
missing_data$Percent <- (missing_data$`Missing Count` / nrow(add_data)) * 100

# Thêm cột tên biến
missing_data$Variable <- rownames(missing_data)

# Vẽ biểu đồ và ghi số liệu lên trên, phần trăm nằm trong cột
library(grid)  # Load thư viện grid

ggplot(missing_data, aes(x = reorder(Variable, -`Missing Count`), y = `Missing Count`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = `Missing Count`), vjust = -0.5, color = "black") +
  geom_text(aes(y = `Missing Count` / 2, label = paste0(round(Percent, 1), "%")), 
            color = "darkred", size = 3.5) +
  labs(title = "Missing Values per Variable", x = "Variable", y = "Missing Count") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = unit(c(20, 20, 20, 20), "pt")
  ) +
  expand_limits(y = max(missing_data$`Missing Count`) * 1.1)


##==========================================
## 2. NHẬP DỮ LIỆU THIẾU (IMPUTE) BẰNG MEDIAN
##==========================================

# 2.1 Tính median cho từng biến (loại bỏ NA)
med_height <- median(add_data$height, na.rm = TRUE)
med_width  <- median(add_data$width,  na.rm = TRUE)

# 2.2 Thay NA bằng median tương ứng
add_data$height[is.na(add_data$height)] <- med_height
add_data$width [is.na(add_data$width) ] <- med_width

# Hàm tính lại giá trị 'ratio' khi có dữ liệu của 'height' và 'width'
update_ratio <- function(df) {
  df$ratio[is.na(df$ratio) & !is.na(df$height) & !is.na(df$width)] <- df$height[is.na(df$ratio) & !is.na(df$height) & !is.na(df$width)] / df$width[is.na(df$ratio) & !is.na(df$height) & !is.na(df$width)]
  return(df)
}

# Áp dụng hàm update_ratio vào dữ liệu để thay thế giá trị 'ratio' thiếu
add_data <- update_ratio(add_data)

# 2.3 Kiểm tra lại số NA (phải đều về 0)
freq.na(add_data)
add_data <- na.omit(add_data)
freq.na(add_data)

##===================================
## 3. THỐNG KÊ MÔ TẢ (DESCRIPTIVE)
##===================================

# 3.1 Tổng quan summary() sau khi đã xử lý NA
summary(add_data)

# 3.2 Các chỉ số riêng biệt cho từng biến
vars <- c("height", "width", "ratio", "local")

for (v in vars) {
  cat("=== Variable:", v, "===\n")
  cat("  Mean   =", mean(add_data[[v]]), "\n")
  cat("  Median =", median(add_data[[v]]), "\n")
  cat("  SD     =", sd(add_data[[v]]), "\n")
  cat("  Min    =", min(add_data[[v]]), "\n")
  cat("  Max    =", max(add_data[[v]]), "\n\n")
}

# 3.3 Vẽ biểu đồ với ggplot2
# Cài thư viện nếu chưa có
# install.packages("ggplot2")
library(ggplot2)

# 3.3.1 Histogram cho từng biến
for (v in vars) {
  max_count <- max(hist(add_data[[v]], plot = FALSE, breaks = 30)$counts)
  upper_limit <- ceiling(max_count / 200) * 200 + 200  # +200 để cao hơn mức lớn nhất
  
  print(
    ggplot(add_data, aes_string(x = v)) +
      geom_histogram(aes(y = ..count..), bins = 30, fill = "lightblue", color = "black") +
      
      # Thêm số trên cột
      stat_bin(aes(y = ..count.., label = ..count..),
               bins = 30, geom = "text", vjust = -0.5, size = 3, color = "red", fontface = "bold") +
      
      geom_vline(aes_string(xintercept = paste0("mean(", v, ", na.rm = TRUE)")), 
                 color = "blue", linetype = "dashed") +
      scale_y_continuous(breaks = seq(0, upper_limit, by = 200), 
                         limits = c(0, upper_limit), 
                         expand = expansion(mult = c(0, 0.1))) +
      labs(title = paste("Histogram of", v), x = v, y = "Frequency") +
      theme_classic()
  )
}


qqnorm(add_data$height, main = "Q-Q plot for height")
qqline(add_data$height, col = "red")

# Shapiro-Wilk test kiểm định chuẩn
shapiro.test(add_data$height)

qqnorm(add_data$width, main = "Q-Q plot for width")
qqline(add_data$width, col = "red")

# Shapiro-Wilk test kiểm định chuẩn
shapiro.test(add_data$width)

rm.out <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)  # Q1 và Q3
  H <- 1.5 * IQR(x, na.rm = na.rm)                         # 1.5 * IQR
  
  y <- x  # Tạo bản sao để xử lý
  # Loại bỏ ngoại lai nhỏ hơn Q1 - 1.5*IQR
  y[x < (qnt[1] - H)] <- NA
  # Loại bỏ ngoại lai lớn hơn Q3 + 1.5*IQR
  y[x > (qnt[2] + H)] <- NA
  
  return(y)
}
new_data<-add_data
new_data$height = rm.out(new_data$height)
new_data$width = rm.out(new_data$width)

summary(new_data)
freq.na(new_data)

# Tính median cho từng biến (loại bỏ NA)
med_height <- median(new_data$height, na.rm = TRUE)
med_width  <- median(new_data$width,  na.rm = TRUE)

# Thay NA bằng median tương ứng
new_data$height[is.na(new_data$height)] <- med_height
new_data$width [is.na(new_data$width) ] <- med_width

summary(new_data)

# Hàm tính lại 'ratio' hoàn toàn dựa vào 'height' và 'width' mới
update_ratio <- function(df) {
  df$ratio <- df$height / df$width
  return(df)
}

# Áp dụng hàm update_ratio vào dữ liệu để thay thế giá trị 'ratio' thiếu
new_data <- update_ratio(new_data)

summary(new_data)
freq.na(new_data)

# Vẽ lại histogram
for (v in vars) {
  max_count <- max(hist(new_data[[v]], plot = FALSE, breaks = 30)$counts)
  upper_limit <- ceiling(max_count / 200) * 200 + 200  # +200 để cao hơn mức lớn nhất
  
  print(
    ggplot(new_data, aes_string(x = v)) +
      geom_histogram(aes(y = ..count..), bins = 30, fill = "lightblue", color = "black") +
      
      # Thêm số trên cột
      stat_bin(aes(y = ..count.., label = ..count..),
               bins = 30, geom = "text", vjust = -0.5, size = 3, color = "red", fontface = "bold") +
      
      geom_vline(aes_string(xintercept = paste0("mean(", v, ", na.rm = TRUE)")), 
                 color = "blue", linetype = "dashed") +
      scale_y_continuous(breaks = seq(0, upper_limit, by = 200), 
                         limits = c(0, upper_limit), 
                         expand = expansion(mult = c(0, 0.1))) +  # Mở rộng không gian trên
      labs(title = paste("Histogram of", v), x = v, y = "Frequency") +
      theme_classic()
  )
}

# 3.3.2 Boxplot cho từng biến
target_counts <- table(add_data$target)
# Vẽ Barplot cho 'target'
barplot(target_counts,
        main = "Barplot of Target",
        names.arg = c("Non-Ad", "Ad"),
        col = c("gray", "orange"),
        ylab = "Frequency")

# Vẽ Boxplot cho 'height' theo 'target'
boxplot(height ~ target, data = add_data,
        main = "Boxplot of Height by Target",
        xlab = "Target", ylab = "Height",
        col = c("lightgray", "lightblue"),
        names = c("Non-Ad (0)", "Ad (1)"))

# Vẽ Boxplot cho 'width' theo 'target'
boxplot(width ~ target, data = add_data,
        main = "Boxplot of Width by Target",
        xlab = "Target", ylab = "Width",
        col = c("lightgray", "lightgreen"),
        names = c("Non-Ad (0)", "Ad (1)"))

# Vẽ Boxplot cho 'ratio' theo 'target'
boxplot(ratio ~ target, data = add_data,
        main = "Boxplot of Ratio by Target",
        xlab = "Target", ylab = "Ratio",
        col = c("lightgray", "orange"),
        names = c("Non-Ad (0)", "Ad (1)"))

# Vẽ Boxplot cho 'local' theo 'target'
boxplot(local ~ target, data = add_data,
        main = "Boxplot of Local by Target",
        xlab = "Target", ylab = "local",
        col = c("lightgray", "pink"),
        names = c("Non-Ad (0)", "Ad (1)"))

# 3.3.3 Scatter plot: Height vs Width
ggplot(add_data, aes(x = height, y = width)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter: Height vs Width", x = "Height", y = "Width")

# 3.3.4 Scatterplot matrix cho tất cả biến số
pairs(add_data[, vars], main = "Scatterplot Matrix (height, width, ratio, local)")


##===================================
## 4. (TÙY CHỌN) HỒI QUY LOGISTIC
##===================================

# Mô hình hồi quy logistic theo 'height'
model_height <- glm(target ~ height, data = add_data, family = "binomial")

# Vẽ đồ thị phân tán giữa 'height' và 'target', sau đó vẽ đường cong logistic
plot(add_data$height, add_data$target,
     xlab = "Height", ylab = "Target",
     main = "Logistic Regression: Target vs Height",
     pch = 16, col = rgb(0, 0, 1, 0.3))  # Đồ thị phân tán

curve(predict(model_height, newdata = data.frame(height = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)  # Đường cong logistic

# Mô hình hồi quy logistic theo 'width'
model_width <- glm(target ~ width, data = add_data, family = "binomial")

plot(add_data$width, add_data$target,
     xlab = "Width", ylab = "Target",
     main = "Logistic Regression: Target vs Width",
     pch = 16, col = rgb(0, 0.5, 0, 0.3))

curve(predict(model_width, newdata = data.frame(width = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)

# Mô hình hồi quy logistic theo 'ratio'
model_ratio <- glm(target ~ ratio, data = add_data, family = "binomial")

plot(add_data$ratio, add_data$target,
     xlab = "Ratio", ylab = "Target",
     main = "Logistic Regression: Target vs Ratio",
     pch = 16, col = rgb(0.7, 0, 0, 0.3))

curve(predict(model_ratio, newdata = data.frame(ratio = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)

# Mô hình hồi quy logistic theo 'local'
model_local <- glm(target ~ local, data = add_data, family = "binomial")

plot(add_data$local, add_data$target,
     xlab = "Local", ylab = "Target",
     main = "Logistic Regression: Target vs Local",
     pch = 16, col = rgb(0, 0, 1, 0.3))  # Đồ thị phân tán

curve(predict(model_local, newdata = data.frame(local = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)

# Tính ma trận tương quan giữa các biến liên tục 'height', 'width', 'ratio', 'local'
library(corrplot)
cor_matrix <- cor(add_data[, c("height", "width", "ratio","local")], use = "complete.obs")

# Vẽ biểu đồ ma trận tương quan (kết hợp circle + local color)
corrplot(cor_matrix, method = "circle", 
         addCoef.col = "black", tl.col = "black", number.cex = 0.8,
         title = "Correlation Matrix", mar = c(0,0,1,0))

##===========================================
## 4. HỒI QUY LOGISTIC ĐƠN BIẾN (SINGLE VAR)
##===========================================

set.seed(123)
sample_index <- sample(1:nrow(add_data), size = 0.7 * nrow(add_data))
train_data <- add_data[sample_index, ]
test_data <- add_data[-sample_index, ]

# Huấn luyện mô hình đơn biến
model_height <- glm(target ~ height, data = train_data, family = binomial)
summary(model_height)
model_width  <- glm(target ~ width,  data = train_data, family = binomial)
summary(model_width)
model_ratio  <- glm(target ~ ratio,  data = train_data, family = binomial)
summary(model_ratio)
model_local  <- glm(target ~ local,  data = train_data, family = binomial)
summary(model_local)

# Hàm đánh giá mô hình
evaluate_model <- function(model, data, label) {
  pred_prob <- predict(model, newdata = data, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  actual <- data$target
  accuracy <- mean(pred_class == actual)
  
  cat(paste0(label, ":\n"))
  cat("Accuracy: ", round(accuracy, 4), "\n")
  cat("Confusion Matrix:\n")
  print(table(Predicted = pred_class, Actual = actual))
  cat("\n")
  
  return(pred_prob)
}

# Đánh giá từng mô hình đơn biến
prob_height <- evaluate_model(model_height, test_data, "Model 1: height")
prob_width  <- evaluate_model(model_width,  test_data, "Model 2: width")
prob_ratio  <- evaluate_model(model_ratio,  test_data, "Model 3: ratio")
prob_local  <- evaluate_model(model_local,  test_data, "Model 4: local")

# Tính và in AIC
cat("AIC - Model 1 (height):", AIC(model_height), "\n")
cat("AIC - Model 2 (width):",  AIC(model_width),  "\n")
cat("AIC - Model 3 (ratio):",  AIC(model_ratio),  "\n")
cat("AIC - Model 4 (local):",  AIC(model_local),  "\n")

# ROC và AUC
library(pROC)
roc_height <- roc(test_data$target, prob_height)
roc_width  <- roc(test_data$target, prob_width)
roc_ratio  <- roc(test_data$target, prob_ratio)
roc_local  <- roc(test_data$target, prob_local)

# Vẽ đường ROC
plot(roc_height, col = "blue", lwd = 2, main = "ROC Curve - Single Variable Models")
lines(roc_width,  col = "green", lwd = 2)
lines(roc_ratio,  col = "red", lwd = 2)
lines(roc_local,  col = "orange", lwd = 2)
legend("bottomright", legend = c("height", "width", "ratio", "local"),
       col = c("blue", "green", "red", "orange"), lwd = 2)

# AUC
cat("AUC - Model 1 (height):", auc(roc_height), "\n")
cat("AUC - Model 2 (width):",  auc(roc_width),  "\n")
cat("AUC - Model 3 (ratio):",  auc(roc_ratio),  "\n")
cat("AUC - Model 4 (local):",  auc(roc_local),  "\n")


##===========================================
## 5. HỒI QUY LOGISTIC ĐA BIẾN (MULTIVARIATE)
##===========================================

# Huấn luyện mô hình đa biến
model_full <- glm(target ~ height + width + ratio + local, data = train_data, family = binomial)
summary(model_full)
model_hwloc <- glm(target ~ height + width + local, data = train_data, family = binomial)
summary(model_hwloc)
model_rloc <- glm(target ~ ratio + local, data = train_data, family = binomial)
summary(model_rloc)
model_hwroc <- glm(target ~ height + width + ratio, data = train_data, family = binomial)
summary(model_hwroc)
model_wroc <- glm(target ~ width + ratio, data = train_data, family = binomial)
summary(model_wroc)
model_hroc <- glm(target ~ height + ratio, data = train_data, family = binomial)
summary(model_hroc)
# Đánh giá từng mô hình đa biến
prob_full <- evaluate_model(model_full, test_data, "Model 5: Full (height + width + ratio + local)")
prob_hwloc <- evaluate_model(model_hwloc, test_data, "Model 6: height + width + local")
prob_rloc <- evaluate_model(model_rloc, test_data, "Model 7: ratio + local")
prob_hwroc <- evaluate_model(model_hwroc, test_data, "Model 8: height + width + ratio")
prob_wroc <- evaluate_model(model_wroc, test_data, "Model 9: width + ratio")
prob_hroc <- evaluate_model(model_hroc, test_data, "Model 10: height + width + ratio")
# Tính và in AIC
cat("AIC - Model 5 (Full):", AIC(model_full), "\n")
cat("AIC - Model 6 (height + width + local):", AIC(model_hwloc), "\n")
cat("AIC - Model 7 (ratio + local):", AIC(model_rloc), "\n")
cat("AIC - Model 8 (height + width + ratio):", AIC(model_hwroc), "\n")
cat("AIC - Model 9 (width + ratio):", AIC(model_wroc), "\n")
cat("AIC - Model 10 (height + width + ratio):", AIC(model_hroc), "\n")
# ROC và AUC
library(pROC)
roc_full  <- roc(test_data$target, prob_full)
roc_hwloc <- roc(test_data$target, prob_hwloc)
roc_rloc  <- roc(test_data$target, prob_rloc)
roc_hwroc  <- roc(test_data$target, prob_hwroc)
roc_wroc  <- roc(test_data$target, prob_wroc)
roc_hroc  <- roc(test_data$target, prob_hroc)

# Vẽ đường ROC
plot(roc_full,  col = "blue",   lwd = 2, main = "ROC Curve - Multivariate Models")
lines(roc_hwloc, col = "purple", lwd = 2)
lines(roc_rloc,  col = "brown",  lwd = 2)
lines(roc_hwroc,  col = "orange",  lwd = 2)
lines(roc_wroc,  col = "orange",  lwd = 2)
legend("bottomright",
       legend = c("Model 5: Full", "Model 6: height + width + local", "Model 7: ratio + local ","Model 8: height + width + ratio"),
       col = c("blue", "purple", "brown", "orange"), lwd = 2)

# AUC
cat("AUC - Model 5 (Full):", auc(roc_full), "\n")
cat("AUC - Model 6 (height + width + local):", auc(roc_hwloc), "\n")
cat("AUC - Model 7 (ratio + local):", auc(roc_rloc), "\n")
cat("AUC - Model 8 (height + width + ratio):", auc(roc_hwroc), "\n")
cat("AUC - Model 9 (width + ratio):", auc(roc_wroc), "\n")
cat("AUC - Model 10 (height + ratio):", auc(roc_hroc), "\n")
