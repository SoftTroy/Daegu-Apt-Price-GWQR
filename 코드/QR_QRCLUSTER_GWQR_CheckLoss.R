# 패지키 불러오기
# install.packages("psych")
# install.packages("writexl")
# install.packages("car")
# install.packages("lmtest")
# install.packages("ggcorrplot")
# install.packages("quantreg")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("patchwork")
# install.packages("viridis")
# install.packages("geosphere")


library(psych)
library(writexl)
library(car)
library(lmtest)
library(ggcorrplot)
library(quantreg)
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)
library(viridis)
library(geosphere)

#------------------------------------------------------------------------------#

# 데이터 불러오기
data = read.csv("C:/Users/TAEEON/Downloads/1.진행중/공모전/통콘/data/merge데이터/merged_daegu_apartments_acad_comm.csv", header = TRUE)


col_list = c(
  "단지명","단지주소","전용면적...","거래금액.만원.","건축년도","lat","lon","walk_route_dist_tmap",
  "DONG_CNT_x","NMHSH_y",
  "GNRLZ_INSTUT_CNT","ETEX_INSTUT_CNT","FGGG_INSTUT_CNT","AAMAPE_INSTUT_CNT","READRM_CNT","ETC_INSTUT_CNT",
  "TRDAR_TO_DSTNC"
)


#"INFO_INSTUT_CNT", "SPCEDU_INSTUT_CNT" 는 모든 값이 0. 따라서 분석에서 제외함.
#"SSIZE_INSTUT_CNT", "MSIZE_INSTUT_CNT", "LGZ_INSTUT_CNT" 는 다변량자료분석을 위한 검정을 실시할 수 없어서 제외.

df = data[col_list]

#------------------------------------------------------------------------------#

# 이상점 또는 계수 추정에 문제가 있는 샘플 제거

# 공간상관성이 낮은 샘플(위도값이 제일 큰 샘플. 이거 하나때문에 bandwidth 가 너무 커짐)
outlier_idx1 <- which(df$lat == max(df$lat))
df <- df[-outlier_idx1, ]

# 좌하단 산업단지 3군집에 대한 데이터 제거. 이거 때문에 bandwidth 가 커짐
outlier_idx2 <- which(df$lat < 35.75)
df <- df[-outlier_idx2, ]

#------------------------------------------------------------------------------#

# 데이터 타입 설정

# 변수 "거래금액.만원." 데이터 타입 설정
df$`거래금액.만원.` <- as.numeric(gsub(",", "", df$`거래금액.만원.`))

# "단지명","단지주소"를 지우고, 모든 변수를 수치형변수로 변환
df[c("단지명","단지주소")] <- NULL
df[] <- lapply(df, as.numeric)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# 1. 변수 선택 및 정의

# 건물연령
df['건물연령'] <- (2025-df['건축년도'])

# 면적 당 거래금액
df['면적당거래금액'] <- (df$거래금액.만원./df$전용면적...)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# 1-1. log변환할 변수 선정 후, 로그 변환

# 면적당거래금액 변수 로그변환
df['log_면적당거래금액'] <- log(df$거래금액.만원./df$전용면적...+1)

# 거리 변수 로그변환
df['log_walk_route_dist_tmap'] <- log(df$"walk_route_dist_tmap"+1)
df['log_TRDAR_TO_DSTNC'] <- log(df$"TRDAR_TO_DSTNC"+1)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# 1-2. lat(위도), lon(경도) 를 제외한 모든 변수 표준화
# GWR 에 필요한 과정임.

target_cols <- setdiff(names(df), c("lat", "lon"))
df[target_cols] <- scale(df[target_cols])

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# 2. PCA할 변수 선정 후, PCA로 차원축소

fa_vars <- c("log_walk_route_dist_tmap", "GNRLZ_INSTUT_CNT", 
             "ETEX_INSTUT_CNT","FGGG_INSTUT_CNT", 
             "AAMAPE_INSTUT_CNT", "READRM_CNT", 
             "ETC_INSTUT_CNT", "log_TRDAR_TO_DSTNC",
             "DONG_CNT_x","NMHSH_y",
             "건물연령")

new_names <- c("log_지하철까지의거리", "종합학원수", 
               "입시학원수", "외국어학원수", 
               "예체능학원수", "독서실수", 
               "기타학원수", "log_상권까지의거리",
               "동수", "세대수",
               "건물연령")

names(df)[match(fa_vars, names(df))] <- new_names

sub_df <- df[ , new_names]

#------------------------------------------------------------------------------#
# "동수","종합학원수" 얘네는 그냥 버리는게 좋을 것 같다..!
drop_vars <- c("동수","종합학원수","log_지하철까지의거리","세대수","log_상권까지의거리", "건물연령")
sub_df <- sub_df[, setdiff(names(sub_df), drop_vars)]

# 2. PCA 실행 (표준화)
pca_result <- prcomp(sub_df, scale. = TRUE)

# 3. Scree plot - 주성분별 분산 설명량 시각화
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

scree_df <- data.frame(
  PC = seq_along(variance_explained),
  VarianceExplained = variance_explained,
  CumulativeVariance = cumsum(variance_explained)
)

ggplot(scree_df, aes(x = PC, y = VarianceExplained)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = scree_df$Factor) +
  labs(title = "Scree Plot", x = "Factor Number", y = "Eigenvalue") +
  theme_minimal()

# 4. 인자 적재표 (loading matrix)
loadings <- pca_result$rotation
print(round(loadings, 3))   # 변수별 주성분 적재값 출력

# 5. 인자적재 설명력 (분산 설명율)
print(round(variance_explained, 3))
print(round(scree_df$CumulativeVariance, 3))

# 첫 번째 주성분 점수 추출
pc1_scores <- pca_result$x[, 1]

# PCA변수를 새 컬럼으로 추가
df <- cbind(df, pc1_scores)

#------------------------------------------------------------------------------#

# pc1_scores 에 대해 asinh 변환 적용한 변수 생성
df['asinh_PC1'] <- asinh(df$pc1_scores)

# df의 열 제거
df[c("walk_route_dist_tmap", "TRDAR_TO_DSTNC")] <- NULL    #log_지하철까지의거리, log_상권까지의거리 로 대체
df[c("동수", "종합학원수")] <- NULL    #여훈이형이 PCA과정에서 필요없다고 판단한 열들
df[c("입시학원수","외국어학원수","예체능학원수","독서실수","기타학원수")] <- NULL    #PCA로 차원축소된 변수들
df['건축년도'] <- NULL                #건축년도
df[c("거래금액.만원.","전용면적...","면적당거래금액")] <- NULL    #log_면적당거래금액으로 대체
df[c("pc1_scores")] <- NULL      #asinh_PC1 로 대체됨.
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


# 5. QR 실시(5개 quantile로 나눔), 계수 확인, 성능평가지표 : , Check Loss

# 분위수회귀분석
qr_model <- rq(log_면적당거래금액 ~ . - lat - lon, data = df, 
               tau = c(0.25, 0.50, 0.75))
summary(qr_model)

qr_summary<- summary(qr_model)
plot(qr_summary)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# 6. Clustering 실시, LM와 QR 실시, 계수 확인 및 유의성 확인, 성능평가지표 : , Check Loss


# 지역 구분 추가
data2 = read.csv("C:/Users/TAEEON/Downloads/1.진행중/공모전/통콘/Rcode/여훈이형 보내준 자료/df_clean_cluster_v2.csv", header = TRUE)
# data2 <- data2[-outlier_idx1, ]    #df_clean_cluster 를 사용하면, 해당 인덱스 제거해야함. v2는 비활성화.
# data2 <- data2[-outlier_idx2, ]    #df_clean_cluster 를 사용하면, 해당 인덱스 제거해야함. v2는 비활성화.
col_region_distinct <- c("지역번호_1","지역번호_2","지역번호_3","지역번호_4")
df_region_distinct <- data2[, col_region_distinct]
df_region_distinct[] <- lapply(df_region_distinct, factor)
df2 <- cbind(df,df_region_distinct)

# 분위수회귀분석
qr_model_cluster <- rq(log_면적당거래금액 ~ 
                         (세대수 + 건물연령 + log_지하철까지의거리 
                          + log_상권까지의거리 + asinh_PC1) * (지역번호_1 + 지역번호_2 + 지역번호_3 + 지역번호_4), 
                       data = df2, 
                       tau = c(0.25, 0.50, 0.75))
summary(qr_model_cluster)

qr_summary_cluster<- summary(qr_model_cluster)
plot(qr_summary_cluster)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# 7. GWQR 실시 (Adaptive Kernel 적용)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# 함수 정의: run_custom_gwqr_adaptive
# 목적: Adaptive Kernel(이웃 수 기준)을 사용하여 GWQR 수행
#------------------------------------------------------------------------------#
run_custom_gwqr_adaptive <- function(data, dep_var, lat_var, lon_var, tau, num_neighbors) {
  
  # 1. Formula 생성
  f_char <- paste(dep_var, "~ . -", lat_var, " -", lon_var, sep="")
  formula_obj <- as.formula(f_char)
  
  # 2. 변수명 추출을 위한 더미 모델 실행
  dummy_fit <- rq(formula_obj, data = data[1:min(100, nrow(data)), ], tau = tau)
  var_names <- names(coef(dummy_fit))
  n_vars <- length(var_names)
  n_obs <- nrow(data)
  
  # 계수 저장 매트릭스 초기화
  coef_mat <- matrix(NA, nrow = n_obs, ncol = n_vars)
  colnames(coef_mat) <- var_names
  
  # 3. 진행 상황 표시
  cat("\n[GWQR - Adaptive Kernel] 분석 시작\n")
  cat("Target Quantile(tau):", tau, "\n")
  cat("Adaptive Bandwidth (Number of Neighbors):", num_neighbors, "\n")
  
  pb <- txtProgressBar(min = 0, max = n_obs, style = 3)
  
  # 위경도 매트릭스 생성 (Haversine 계산용)
  coords_matrix <- cbind(data[[lon_var]], data[[lat_var]])
  
  # 4. Main Loop
  for (i in 1:n_obs) {
    
    # (1) 타겟 지점 설정
    target_point <- c(data[[lon_var]][i], data[[lat_var]][i])
    
    # (2) 거리 계산 (모든 점과의 거리) -> km 단위
    dists_meter <- distHaversine(coords_matrix, target_point)
    dists_km <- dists_meter / 1000 
    
    # (3) Adaptive Bandwidth 결정 (h_i)
    # 거리를 오름차순 정렬하여 num_neighbors 번째 거리를 찾음
    # 이 거리가 해당 지점의 bandwidth(h)가 됨
    dist_sorted <- sort(dists_km)
    adaptive_h <- dist_sorted[num_neighbors]
    
    # 만약 중복 좌표 등으로 h가 0이면 아주 작은 값으로 대체하여 에러 방지
    if(adaptive_h == 0) adaptive_h <- 0.001
    
    # (4) Adaptive Bi-square Kernel 가중치 계산
    # 공식: w = (1 - (d/h)^2)^2  (단, d < h 일 때만. d >= h 이면 0)
    # Adaptive 방식에서는 Gaussian보다 Bi-square가 경계 처리에 더 유리함
    weights <- ifelse(dists_km < adaptive_h, 
                      (1 - (dists_km / adaptive_h)^2)^2, 
                      0)
    
    # (5) 가중 분위수 회귀 적합
    # 가중치가 0보다 큰 데이터만 추출하여 계산 속도 향상 가능하나, 
    # rq 함수 내부 처리를 위해 전체 데이터를 넘기되 weights로 조절
    
    # 유효 데이터 개수 확인 (자유도 확보)
    if(sum(weights > 1e-10) < (n_vars + 5)) {
      coef_mat[i, ] <- NA
    } else {
      tryCatch({
        # method="br" 또는 "fn" 사용 권장 (데이터 많을 시 "fn"이 빠름)
        fit <- rq(formula_obj, data = data, tau = tau, weights = weights, method="fn")
        coef_mat[i, ] <- coef(fit)
      }, error = function(e) {
        coef_mat[i, ] <- NA
      })
    }
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  # 5. 결과 반환
  coef_df <- as.data.frame(coef_mat)
  colnames(coef_df) <- paste0("coef_", colnames(coef_df))
  result_df <- cbind(data, coef_df)
  
  return(result_df)
}

#-----------------------------------------------------------------------------#
# 실행 파트 (Adaptive Bandwidth 적용)
#-----------------------------------------------------------------------------#

# [중요] 적절한 이웃 수(Neighbors) 설정
# 전체 데이터의 10% ~ 20% 정도를 권장합니다.
# 예를 들어 데이터가 2,000개라면 200~400개 정도로 설정.
# 너무 작으면(예: 30개) 국지적 다중공선성 문제가 해결되지 않습니다.

total_n <- nrow(df)
# neighbor_cnt <- round(total_n * 0.15) # 전체 데이터의 15%를 이웃으로 설정 (추천)
#------------------------------------------------------------------------------#
# 7. GWQR 실시 (Adaptive Kernel 적용)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# 함수 정의: run_custom_gwqr_adaptive
# 목적: Adaptive Kernel(이웃 수 기준)을 사용하여 GWQR 수행
#------------------------------------------------------------------------------#
run_custom_gwqr_adaptive <- function(data, dep_var, lat_var, lon_var, tau, num_neighbors) {
  
  # 1. Formula 생성
  f_char <- paste(dep_var, "~ . -", lat_var, " -", lon_var, sep="")
  formula_obj <- as.formula(f_char)
  
  # 2. 변수명 추출을 위한 더미 모델 실행
  dummy_fit <- rq(formula_obj, data = data[1:min(100, nrow(data)), ], tau = tau)
  var_names <- names(coef(dummy_fit))
  n_vars <- length(var_names)
  n_obs <- nrow(data)
  
  # 계수 저장 매트릭스 초기화
  coef_mat <- matrix(NA, nrow = n_obs, ncol = n_vars)
  colnames(coef_mat) <- var_names
  
  # 3. 진행 상황 표시
  cat("\n[GWQR - Adaptive Kernel] 분석 시작\n")
  cat("Target Quantile(tau):", tau, "\n")
  cat("Adaptive Bandwidth (Number of Neighbors):", num_neighbors, "\n")
  
  pb <- txtProgressBar(min = 0, max = n_obs, style = 3)
  
  # 위경도 매트릭스 생성 (Haversine 계산용)
  coords_matrix <- cbind(data[[lon_var]], data[[lat_var]])
  
  # 4. Main Loop
  for (i in 1:n_obs) {
    
    # (1) 타겟 지점 설정
    target_point <- c(data[[lon_var]][i], data[[lat_var]][i])
    
    # (2) 거리 계산 (모든 점과의 거리) -> km 단위
    dists_meter <- distHaversine(coords_matrix, target_point)
    dists_km <- dists_meter / 1000 
    
    # (3) Adaptive Bandwidth 결정 (h_i)
    # 거리를 오름차순 정렬하여 num_neighbors 번째 거리를 찾음
    # 이 거리가 해당 지점의 bandwidth(h)가 됨
    dist_sorted <- sort(dists_km)
    adaptive_h <- dist_sorted[num_neighbors]
    
    # 만약 중복 좌표 등으로 h가 0이면 아주 작은 값으로 대체하여 에러 방지
    if(adaptive_h == 0) adaptive_h <- 0.001
    
    # (4) Adaptive Bi-square Kernel 가중치 계산
    # 공식: w = (1 - (d/h)^2)^2  (단, d < h 일 때만. d >= h 이면 0)
    # Adaptive 방식에서는 Gaussian보다 Bi-square가 경계 처리에 더 유리함
    weights <- ifelse(dists_km < adaptive_h, 
                      (1 - (dists_km / adaptive_h)^2)^2, 
                      0)
    
    # (5) 가중 분위수 회귀 적합
    # 가중치가 0보다 큰 데이터만 추출하여 계산 속도 향상 가능하나, 
    # rq 함수 내부 처리를 위해 전체 데이터를 넘기되 weights로 조절
    
    # 유효 데이터 개수 확인 (자유도 확보)
    if(sum(weights > 1e-10) < (n_vars + 5)) {
      coef_mat[i, ] <- NA
    } else {
      tryCatch({
        # method="br" 또는 "fn" 사용 권장 (데이터 많을 시 "fn"이 빠름)
        fit <- rq(formula_obj, data = data, tau = tau, weights = weights, method="fn")
        coef_mat[i, ] <- coef(fit)
      }, error = function(e) {
        coef_mat[i, ] <- NA
      })
    }
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  # 5. 결과 반환
  coef_df <- as.data.frame(coef_mat)
  colnames(coef_df) <- paste0("coef_", colnames(coef_df))
  result_df <- cbind(data, coef_df)
  
  return(result_df)
}

#-----------------------------------------------------------------------------#
# 실행 파트 (Adaptive Bandwidth 적용)
#-----------------------------------------------------------------------------#

# [중요] 적절한 이웃 수(Neighbors) 설정
# 전체 데이터의 10% ~ 20% 정도를 권장합니다.
# 예를 들어 데이터가 2,000개라면 200~400개 정도로 설정.
# 너무 작으면(예: 30개) 국지적 다중공선성 문제가 해결되지 않습니다.

total_n <- nrow(df)
neighbor_cnt <- round(total_n * 0.15) # 전체 데이터의 15%를 이웃으로 설정 (추천)
# neighbor_cnt <- 66 #윤찬이가 추천해준 수

print(paste("설정된 이웃 수 (Adaptive Bandwidth):", neighbor_cnt))

# Q1 (tau = 0.25)
gwqr_result_q1 <- run_custom_gwqr_adaptive(
  data = df,
  dep_var = "log_면적당거래금액",
  lat_var = "lat",
  lon_var = "lon",
  tau = 0.25,
  num_neighbors = neighbor_cnt 
)

# Q2 (tau = 0.50)
gwqr_result_q2 <- run_custom_gwqr_adaptive(
  data = df,
  dep_var = "log_면적당거래금액",
  lat_var = "lat",
  lon_var = "lon",
  tau = 0.50,
  num_neighbors = neighbor_cnt
)

# Q3 (tau = 0.75)
gwqr_result_q3 <- run_custom_gwqr_adaptive(
  data = df,
  dep_var = "log_면적당거래금액",
  lat_var = "lat",
  lon_var = "lon",
  tau = 0.75,
  num_neighbors = neighbor_cnt
)

# 결과 확인
head(gwqr_result_q1)
head(gwqr_result_q2)
head(gwqr_result_q3)

print(paste("설정된 이웃 수 (Adaptive Bandwidth):", neighbor_cnt))

# 결과 확인
head(gwqr_result_q1)
head(gwqr_result_q2)
head(gwqr_result_q3)

#-----------------------------------------------------------------------------#

# -----------------------------------------------------------------------------
# [Step 1] 시각화할 변수(X) 자동 선택
# -----------------------------------------------------------------------------

# 1. 분석에서 제외할 변수명 정의 (Y 및 좌표)
exclude_vars <- c("log_면적당거래금액", "lat", "lon")

# 2. df의 전체 컬럼 중 제외 변수를 뺀 나머지를 X변수로 간주
#    (GWQR 결과에는 'coef_' 접두어가 붙으므로 이를 반영)
x_vars <- setdiff(names(df), exclude_vars)
target_coef_cols <- paste0("coef_", x_vars)

# (확인용 출력) 자동으로 선택된 변수명
cat("시각화할 계수 변수 목록:\n")
print(target_coef_cols)


# -----------------------------------------------------------------------------
# [Step 2] 범례 통일을 위한 Global Min/Max 계산
# -----------------------------------------------------------------------------
# Q1, Q2, Q3 전체 결과를 통틀어 각 변수의 최소/최대를 구해야
# 서로 다른 분위수에서도 색상이 동일한 값을 의미하게 됩니다.

global_limits <- list()

for (col in target_coef_cols) {
  # 3개 결과셋에서 해당 계수 컬럼의 값을 모두 모음
  all_values <- c(gwqr_result_q1[[col]], 
                  gwqr_result_q2[[col]], 
                  gwqr_result_q3[[col]])
  
  # NA 제외하고 범위 계산
  global_limits[[col]] <- range(all_values, na.rm = TRUE)
}


# -----------------------------------------------------------------------------
# [Step 3] 시각화 생성 함수 (Patchwork 이용)
# -----------------------------------------------------------------------------

create_patchwork_plot <- function(result_data, tau_label) {
  
  plot_list <- list()
  
  for (col in target_coef_cols) {
    
    # 현재 변수의 고정된 범위 가져오기
    my_limits <- global_limits[[col]]
    
    # 플롯 생성 (sf 객체가 아니므로 geom_point 사용)
    p <- ggplot(result_data, aes(x = lon, y = lat, color = .data[[col]])) +
      geom_point(size = 2, alpha = 0.8) + # 점 크기 및 투명도 조절
      scale_color_viridis(
        option = "viridis", 
        name = "Coef",
        limits = my_limits # [핵심] 범례 범위 고정
      ) +
      labs(title = col, # 변수명을 제목으로
           subtitle = paste("Tau =", tau_label)) +
      theme_void() + # 축과 배경 제거 (지도처럼 보이게)
      theme(
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5),
        legend.position = "right",
        legend.key.height = unit(0.8, "cm")
      )
    
    plot_list[[col]] <- p
  }
  
  # patchwork를 이용해 2x3 배열로 정렬 (변수가 6개라고 가정 시 ncol=3이면 2행 자동 생성)
  # wrap_plots는 리스트 형태의 plot들을 묶어줍니다.
  final_plot <- wrap_plots(plot_list, ncol = 3) + 
    plot_annotation(
      title = paste("GWQR Results: Tau =", tau_label),
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )
  
  return(final_plot)
}


# -----------------------------------------------------------------------------
# [Step 4] 결과 출력
# -----------------------------------------------------------------------------

# Q1 (Low Price) 시각화
plot_q1 <- create_patchwork_plot(gwqr_result_q1, "0.25 (Low Price)")
print(plot_q1)

# Q2 (Median Price) 시각화
plot_q2 <- create_patchwork_plot(gwqr_result_q2, "0.50 (Median Price)")
print(plot_q2)

# Q3 (High Price) 시각화
plot_q3 <- create_patchwork_plot(gwqr_result_q3, "0.75 (High Price)")
print(plot_q3)

# 이미지 저장
# setwd("C:/Users/TAEEON/Downloads/1.진행중/공모전/통콘/Rcode")
# ggsave("GWQR_Q1_adaptive.png", plot_q1, width = 16, height = 9)
# ggsave("GWQR_Q2_adaptive.png", plot_q2, width = 16, height = 9)
# ggsave("GWQR_Q3_adaptive.png", plot_q3, width = 16, height = 9)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#



#Check Loss




#------------------------------------------------------------------------------#
# QR, QR CLUSTER  rq 모델 객체에서 Check Loss 계산
#------------------------------------------------------------------------------#
calc_check_loss_from_model <- function(model_obj, taus) {
  
  # 모델 객체에서 잔차 추출 (matrix 형태: 행=관측치, 열=타우)
  # rq() 함수 결과물에는 resid(잔차)가 이미 저장되어 있음
  residuals_matrix <- residuals(model_obj)
  
  # 결과를 저장할 벡터 생성
  loss_values <- numeric(length(taus))
  
  # 각 Tau별로 순회하며 Loss 계산
  for (i in 1:length(taus)) {
    tau <- taus[i]
    
    # 해당 tau의 잔차 벡터 추출
    # 모델 학습 시 tau 순서대로 열이 생성됨
    u <- residuals_matrix[, i]
    
    # Check Loss 공식 적용: mean( rho_tau(u) )
    # rho_tau(u) = u * (tau - I(u < 0))
    rho <- u * (tau - (u < 0))
    
    loss_values[i] <- mean(rho, na.rm = TRUE)
  }
  
  return(loss_values)
}


# -----------------------------------------------------------------------------
# GWQR adaptive Check Loss
# -----------------------------------------------------------------------------

calculate_gwqr_check_loss <- function(result_df, dep_var, tau) {
  
  # 1. 계수 컬럼(coef_) 식별
  all_cols <- names(result_df)
  coef_cols <- grep("^coef_", all_cols, value = TRUE)
  
  # 2. 예측값(y_pred) 계산 초기화
  # 각 행별로: y_pred = coef_Intercept + (coef_var1 * var1) + (coef_var2 * var2) ...
  n <- nrow(result_df)
  y_pred <- numeric(n)
  
  for (c_col in coef_cols) {
    # "coef_" 접두사를 제거하여 원본 변수명 추출
    var_name <- sub("^coef_", "", c_col)
    
    # (Intercept) 처리
    if (var_name == "(Intercept)") {
      # 절편은 변수 곱하기 없이 계수만 더함
      y_pred <- y_pred + result_df[[c_col]]
    } else {
      # 해당 변수가 데이터프레임에 존재하는지 확인
      if (var_name %in% all_cols) {
        # 계수 * 변수값
        y_pred <- y_pred + (result_df[[c_col]] * result_df[[var_name]])
      } else {
        warning(paste("변수", var_name, "를 데이터에서 찾을 수 없어 계산에서 제외했습니다."))
      }
    }
  }
  
  # 3. 실제값(y_true) 추출
  y_true <- result_df[[dep_var]]
  
  # 4. 잔차(Residual) 계산
  residuals <- y_true - y_pred
  
  # 5. Check Loss 계산 (NA 제외)
  # 공식: mean( rho_tau(residual) )
  rho <- residuals * (tau - (residuals < 0))
  check_loss <- mean(rho, na.rm = TRUE)
  
  return(check_loss)
}


#------------------------------------------------------------------------------#
# QR, QR CLUSTER 각 모델별 Check Loss 산출
#------------------------------------------------------------------------------#

# 분석에 사용한 quantile 목록
taus_list <- c(0.25, 0.50, 0.75)

# 1. 일반 QR 모델 (qr_model) Loss 계산
loss_qr <- calc_check_loss_from_model(qr_model, taus_list)

# 2. QR + Cluster 모델 (qr_model_cluster) Loss 계산
loss_cluster <- calc_check_loss_from_model(qr_model_cluster, taus_list)

#------------------------------------------------------------------------------#
# GWQR 각 모델별 Check Loss 산출
#------------------------------------------------------------------------------#

# 결과가 저장된 데이터프레임이 메모리에 있다고 가정합니다.
# (gwqr_result_q1, gwqr_result_q2, gwqr_result_q3)

# 1. Loss 계산
loss_q1 <- calculate_gwqr_check_loss(gwqr_result_q1, "log_면적당거래금액", 0.25)
loss_q2 <- calculate_gwqr_check_loss(gwqr_result_q2, "log_면적당거래금액", 0.50)
loss_q3 <- calculate_gwqr_check_loss(gwqr_result_q3, "log_면적당거래금액", 0.75)

#------------------------------------------------------------------------------#
#통합 결과
#

# 라이브러리 로드 (이미 로드되어 있다면 생략 가능)
library(dplyr)
library(ggplot2)

# 1. 각 모델별 데이터프레임 생성
# (1) Standard QR
df_qr <- data.frame(
  Tau = taus_list,
  Model = "QR",
  Check_Loss = loss_qr
)

# (2) QR + Cluster
df_cluster <- data.frame(
  Tau = taus_list,
  Model = "QR + Cluster",
  Check_Loss = loss_cluster
)

# (3) GWQR (Adaptive) - 스칼라 값들을 벡터로 묶음
df_gwqr <- data.frame(
  Tau = c(0.25, 0.50, 0.75),
  Model = "GWQR (Adaptive)",
  Check_Loss = c(loss_q1, loss_q2, loss_q3)
)

# 2. 데이터프레임 병합 및 정렬
final_comparison_df <- bind_rows(df_qr, df_cluster, df_gwqr) %>%
  select(Tau, Model, Check_Loss) %>%  # 열 순서 지정
  arrange(Tau, Check_Loss)            # Tau 기준 오름차순, 그 안에서 Loss 낮은 순 정렬

# 3. 결과 출력
print("==== 최종 모델 성능 비교표 (Check Loss) ====")
print(final_comparison_df)

# 4. (선택사항) 시각화 - 막대그래프
ggplot(final_comparison_df, aes(x = as.factor(Tau), y = Check_Loss, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Check_Loss, 4)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  labs(title = "Model Performance Comparison by Quantile",
       subtitle = "Lower Check Loss indicates better performance",
       x = "Quantile (Tau)",
       y = "Check Loss") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom")
