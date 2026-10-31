# ============================================
# 1. Import Data dan Plot Deret Waktu
# ============================================

library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(forecast)
library(tseries)
library(zoo)
library(pracma)
library(fracdiff)
library(longmemo)
library(arfima)
library(MASS)
library(TSA)

# Baca data
data <- read_excel("C:/Users/acer/Downloads/Skripsi/nilaiekspor2.xlsx")

# Gabung Tahun dan Bulan jadi format tanggal
bulan_map <- c(
  "Januari" = "January", "Februari" = "February", "Maret" = "March",
  "April" = "April", "Mei" = "May", "Juni" = "June", "Juli" = "July",
  "Agustus" = "August", "September" = "September",
  "Oktober" = "October", "November" = "November", "Desember" = "December"
)

data$Bulan_Eng <- bulan_map[data$Bulan]
data$Tanggal <- as.Date(paste(data$Tahun, data$Bulan_Eng, "1"), format = "%Y %B %d")
data <- data[order(data$Tanggal), ]
head(data)
names(data)

ekspor_ts <- ts(
  data$Nilai_Ekspor,
  start = c(2015, 1),
  frequency = 12
)
ekspor_ts

#plot time series
plot(ekspor_ts,
     xlab = "Tahun",
     ylab = "Nilai Ekspor (juta US$)")
acf(ekspor_ts, lag.max = 36)

#stasioneritas ragam
model_awal <- lm(ekspor_ts ~ 1)
boxcox(model_awal)
lambda <- BoxCox.lambda(ekspor_ts)
lambda
ekspor_bc <- BoxCox(ekspor_ts, lambda=lambda)
ekspor_bc
model_awal1 <- lm(ekspor_bc ~ 1)
boxcox(model_awal1)
plot(ekspor_bc)
BoxCox.lambda(ekspor_bc)
# ===== CARI LAMBDA =====
bc <- boxcox(model_awal, plotit = FALSE)
lambda <- BoxCox.lambda(ekspor_ts)
lambda
# ===== TRANSFORMASI =====
ekspor_bc <- BoxCox(ekspor_ts, lambda = lambda)
ekspor_bc
BoxCox.lambda(ekspor_bc)
# ===== SESUDAH TRANSFORMASI =====
model_after <- lm(ekspor_bc ~ 1)
boxcox(model_after)
title("Setelah Box-Cox")
par(mfrow = c(1,2))
boxcox(model_awal)
title("Sebelum Box-Cox")
boxcox(model_after)
title("Setelah Box-Cox")
par(mfrow = c(1,1))



#======stasioneritas mean======#
acf(ekspor_bc,main =" ", lag.max=36 )
pacf(ekspor_bc, lag.max = 48,
     main = "PACF Nilai Ekspor Setelah Transformasi Box–Cox")
adf.test(ekspor_bc)


#======indikasi long memory======#
hurst <- hurstexp(ekspor_bc)
hurst


#======GPH======#
gph <- fdGPH(ekspor_bc, bandw.exp = 0.98)
d_gph <- gph$d
d_gph
gph <- fdGPH(ekspor_bc, bandw.exp = 0.9)
d_gph <- gph$d
d_gph
se_as    <- gph$sd.as     # asymptotic SE
se_reg   <- gph$sd.reg    # regression SE
se_as
se_reg
n <- length(ekspor_bc)
beta <- 0.98
m <- floor(n^beta)
m
j <- 1:m
j
omega <- 2*pi*j/n
omega
spec <- abs(fft(ekspor_bc))^2/(2*pi*n)
spec
periodogram <- spec[2:(m+1)]
periodogram
X <- log(4*(sin(omega/2))^2)
X
Y <- log(periodogram)
Y
model <- lm(Y ~ X)
model
beta_hat <- coef(model)[2]
beta_hat
d_hat <- -beta_hat
d_hat


# Hitung statistik t
t_stat <- d_gph / se_as
# Hitung p-value (dua sisi)
p_value <- 2 * (1 - pnorm(abs(t_stat)))

t_stat
p_value
differenced1 <- diffseries(ekspor_bc, d = d_gph)
adf.test(differenced1)
par(mfrow = c(1,2))
acf(differenced1, main = " ")
pacf(differenced1, main = " ")
par(mfrow = c(1,1))
model_gph1 <- arfima(
  z = ekspor_bc,
  order = c(0, 0, 1),
  fixed = list(frac = d_gph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_gph1)
model_gph2 <- arfima(
  z = ekspor_bc,
  order = c(0, 0, 2),
  fixed = list(frac = d_gph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_gph2)
model_gph3 <- arfima(
  z = ekspor_bc,
  order = c(1, 0, 0),
  fixed = list(frac = d_gph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_gph3)
model_gph4 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 0),
  fixed = list(frac = d_gph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_gph4)
model_gph5 <- arfima(
  z = ekspor_bc,
  order = c(1, 0, 1),
  fixed = list(frac = d_gph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_gph5)
model_gph6 <- arfima(
  z = ekspor_bc,
  order = c(1, 0, 2),
  fixed = list(frac = d_gph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_gph6)
model_gph7 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 1),
  fixed = list(frac = d_gph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_gph7)
model_gph8 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 2),
  fixed = list(frac = d_gph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_gph8)

model_gph1 <- arfima(ekspor_bc,
                     order = c(2,0,2),
                     fixed = list(frac = d_gph))
summary(model_gph1)
#model fixed yg diambil model (2,d,2)
resid_list <- residuals(model_gph1)
resid <- as.numeric(resid_list$Mode1)
resid <- resid[!is.na(resid)]
par(mfrow=c(1,2))
acf(resid, main="ACF Residual",lag.max = 36)
pacf(resid, main="PACF Residual")
par(mfrow=c(1,1))
Box.test(resid, lag = 24, type = "Ljung-Box")
jb <- jarque.bera.test(resid)
jb


#Peramalan 
# Jumlah periode ramalan
fc_gph <- predict(model_gph1, n.ahead = 9)
forecast_values <- as.numeric(fc_gph[[1]]$Forecast)
se_values       <- as.numeric(fc_gph[[1]]$exactSD)

lambda_bc <-  lambda
lambda_bc
forecast_gph <- InvBoxCox(forecast_values, lambda= lambda_bc,
                          biasadj=TRUE,
                          fvar = se_values^2)
forecast_gph
upper_asli    <- InvBoxCox(forecast_values + 1.96*se_values, lambda_bc)
lower_asli    <- InvBoxCox(forecast_values - 1.96*se_values, lambda_bc)
forecast_gphts <- ts(forecast_gph,
                     start = c(2025, 1),
                     frequency = 12)

aktual <- read_excel("C:/Users/acer/Downloads/Skripsi/aktualeks.xlsx")
aktual_values <- as.numeric(aktual$Nilai_Ekspor)
# Pastikan jumlah data sama dengan prediksi
length(aktual_values)
# MAPE, MSE, RMSE
MAPE <- mean(abs((aktual_values - forecast_gph) / aktual_values)) * 100
MSE  <- mean((aktual_values - forecast_gph)^2)
RMSE <- sqrt(MSE)
MAPE
MSE
RMSE


hasil_peramalangph <- data.frame(
  Bulan   = c("Jan","Feb","Mar","Apr","Mei","Jun","Jul","Agu","Sep"),
  Aktual  = aktual_values,
  Ramalan = forecast_gph,
  APE     = abs((aktual_values - forecast_gph) / aktual_values) * 100
)

hasil_peramalangph
# Asumsikan hasil_peramalan sudah ada
aktual_values <- hasil_peramalan$Aktual
ramalan_values <- hasil_peramalan$Ramalan
periode <- 1:9

# Plot garis aktual tipis
plot(periode, aktual_values, type = "l", col = "black", lwd = 1,
     ylim = range(c(aktual_values, ramalan_values)),
     xlab = "Bulan", ylab = "Nilai Ekspor",
     main = "Peramalan vs Aktual Ekspor Provinsi Lampung Metode GPH",
     xaxt = "n")
axis(1, at = 1:9, labels = hasil_peramalan$Bulan)

# Tambahkan garis prakiraan tipis
lines(periode, ramalan_values, col = "red", lwd = 1)

# Tambahkan legend
legend("topright", legend = c("aktual", "prakira"), 
       col = c("black", "red"), lwd = 1)


#======SGPH======#
sgph <- fdSperio(ekspor_bc, bandw.exp = 0.5, beta =0.9)
d_sgph <- sgph$d
d_sgph
se_as    <- sgph$sd.as     # asymptotic SE
se_reg   <- sgph$sd.reg    # regression SE
se_as
se_reg
#==============================
# SGPH Estimator dengan Parzen Kernel (Aman)
#==============================
x <- ekspor_bc
x <- x

n <- length(x)

beta <- 0.5
m <- floor(n^beta)

n
m
j <- 1:m
omega <- 2*pi*j/n

omega
fx <- fft(x)
Iomega <- (Mod(fx)^2)/(2*pi*n)
parzen <- function(k, m){
  ak <- abs(k)/m
  
  if(abs(k) <= m/2){
    return(1 - 6*ak^2 + 6*ak^3)
  } else if(abs(k) <= m){
    return(2*(1-ak)^3)
  } else{
    return(0)
  }
}
k_seq <- (-m):m
lambda_vec <- sapply(k_seq, parzen, m=m)
lambda_vec <- lambda_vec / sum(lambda_vec)
I_smooth_full <- numeric(length(Iomega))

for(jj in 1:length(Iomega)){
  
  sum_val <- 0
  
  for(k in (-m):m){
    
    idx <- jj + k
    
    # circular boundary
    if(idx < 1){
      idx <- length(Iomega) + idx
    }
    if(idx > length(Iomega)){
      idx <- idx - length(Iomega)
    }
    
    sum_val <- sum_val + 
      lambda_vec[k + m + 1] * Iomega[idx]
  }
  
  I_smooth_full[jj] <- sum_val
}
I_smooth <- I_smooth_full[2:(m+1)]
Y <- log(I_smooth)
X <- log(4*(sin(omega/2))^2)
model_sgph <- lm(Y ~ X)
summary(model_sgph)
d_manual <- -coef(model_sgph)[2]
d_manual

str(sgph)



tabel_sgph <- data.frame(
  j = j,
  omega = omega,
  I_smooth = I_smooth,
  X = X,
  Y = Y
)

tabel_sgph
tabel_sgph <- round(tabel_sgph, 4)
tabel_sgph
format(tabel_sgph, scientific = TRUE)

# ---------------------------
# 1. Persiapan Data
# ---------------------------
x <- ekspor_bc
x <- x - mean(x)   # demean data

n <- length(x)
beta <- 0.5

# bandwidth regresi
m <- floor(n^beta)

# bandwidth smoothing kernel
m_kernel <- m
m_kernel
# ---------------------------
# 2. Frekuensi Fourier
# ---------------------------
j <- 1:m
omega <- 2*pi*j/n
omega
# ---------------------------
# 3. Periodogram
# ---------------------------
I_full <- (1/(2*pi*n)) * Mod(fft(x))^2
I_full
# ---------------------------
# 4. Kernel Parzen
# ---------------------------
parzen_weight <- function(k, M){
  
  u <- abs(k)/M
  
  if(u <= 0.5){
    return(1 - 6*u^2 + 6*u^3)
  } else if(u <= 1){
    return(2*(1-u)^3)
  } else{
    return(0)
  }
}
parzen_weight
# ---------------------------
# 5. Smoothed Periodogram
# ---------------------------
smooth_I <- numeric(m)

for(jj in 1:m){
  
  sum_val <- 0
  weight_sum <- 0
  
  for(k in (-m_kernel):m_kernel){
    
    idx <- jj + k
    
    if(idx >= 1 && idx <= floor(n/2)){
      
      w <- parzen_weight(k, m_kernel)
      
      sum_val <- sum_val + w * I_full[idx+1]
      weight_sum <- weight_sum + w
    }
  }
  
  smooth_I[jj] <- sum_val / weight_sum
}
smooth_I
# ---------------------------
# 6. Trim Frekuensi Rendah
# ---------------------------
start <- floor(0.1*m)

omega_trim <- omega[start:m]
omega_trim
smooth_trim <- smooth_I[start:m]
smooth_trim
# ---------------------------
# 7. Regresi SGPH
# ---------------------------
X <- log(4*(sin(omega_trim/2))^2)
X
Y <- log(smooth_trim)
Y
model <- lm(Y ~ X)
model
# ---------------------------
# 8. Penduga d
# ---------------------------
d_sgph_manual <- -coef(model)[2]
d_sgph_manual


x <- as.numeric(ekspor_bc)
T <- length(x)

beta <- 0.5
m <- floor(T^beta)
c_band <- 0.9     # exponent bandwidth (bebas 0<c<1)
L <- floor(T^c_band)
spec_raw <- spec.pgram(x, log = "no", plot = FALSE)
I_raw <- spec_raw$spec
lambda_all <- 2*pi*(1:(T-1))/T
parzen_weight <- function(l, L){
  u <- abs(l)/L
  
  if(u <= 0.5){
    return(1 - 6*u^2 + 6*u^3)
  } else if(u <= 1){
    return(2*(1-u)^3)
  } else {
    return(0)
  }
}
I_smooth <- numeric(m)

for(j in 1:m){
  
  sum_val <- 0
  
  for(l in -L:L){
    
    idx <- j - l
    
    if(idx > 0 && idx <= length(I_raw)){
      w <- parzen_weight(l, L)
      sum_val <- sum_val + w * I_raw[idx]
    }
  }
  
  I_smooth[j] <- sum_val
}
lambda <- 2*pi*(1:m)/T
x_reg <- log(4*(sin(lambda/2))^2)
y_reg <- log(I_smooth)
model <- lm(y_reg ~ x_reg)
summary(model)

d_hat <- -coef(model)[2]
d_hat


# ==========================
# 1. DATA
# ==========================
x <- as.numeric(ekspor_bc)
x <- x - mean(x)          # demean

n <- length(x)

# ==========================
# 2. PARAMETER SGPH
# ==========================
beta <- 0.5               # trimming freq (GPH)
m <- floor(n^beta)

L <- floor(n^0.9)         # bandwidth smoothing (kernel)

# ==========================
# 3. FREKUENSI FOURIER
# ==========================
j <- 1:m
omega <- 2*pi*j/n

# ==========================
# 4. PERIODOGRAM (FULL)
# ==========================
fx <- fft(x)
I_full <- (Mod(fx)^2) / (2*pi*n)

# ambil positive frequencies saja
I_full <- I_full[1:(floor(n/2))]

# ==========================
# 5. KERNEL PARZEN
# ==========================
parzen <- function(u){
  au <- abs(u)
  
  if(au <= 0.5){
    return(1 - 6*u^2 + 6*au^3)
  } else if(au <= 1){
    return(2*(1 - au)^3)
  } else {
    return(0)
  }
}

# ==========================
# 6. SMOOTHED PERIODOGRAM
# ==========================
I_smooth <- numeric(m)

for(jj in 1:m){
  
  temp <- 0
  wsum <- 0
  
  for(k in (-L):L){
    
    idx <- jj + k
    
    if(idx >= 1 && idx <= length(I_full)){
      w <- parzen(k / L)
      temp <- temp + w * I_full[idx]
      wsum <- wsum + w
    }
  }
  
  I_smooth[jj] <- temp / wsum
}

# ==========================
# 7. TRIM LOW FREQUENCY
# ==========================
start <- floor(0.1 * m)

omega_trim <- omega[start:m]
I_trim <- I_smooth[start:m]

# ==========================
# 8. REGRESI SGPH
# ==========================
X <- log(4 * (sin(omega_trim / 2))^2)
Y <- log(I_trim)

model <- lm(Y ~ X)

summary(model)

# ==========================
# 9. ESTIMASI d
# ==========================
d_sgph <- -coef(model)[2]
d_sgph


# =========================
# SGPH FULL IMPLEMENTATION
# =========================

x <- ekspor_bc
n <- length(x)

# =========================
# 1. BANDWIDTH REGRESI (m)
# bandw.exp = 0.5
# =========================
eta <- 0.5
m <- floor(n^eta)

j <- 1:m
omega <- 2*pi*j/n

# =========================
# 2. PERIOD0GRAM (BASE GPH)
# =========================
spec <- abs(fft(x))^2 / (2*pi*n)
I <- spec[2:(m + 2*10 + 1)]   # extra padding utk smoothing

# =========================
# 3. PARZEN KERNEL
# =========================
parzen_kernel <- function(u){
  a <- abs(u)
  if(a <= 0.5){
    return(1 - 6*u^2 + 6*a^3)
  } else if(a <= 1){
    return(2*(1 - a)^3)
  } else {
    return(0)
  }
}

# =========================
# 4. BANDWIDTH KERNEL (beta = 0.9)
# =========================
beta <- 0.9
L <- floor(n^beta)

# safety check: jangan terlalu besar
L <- min(L, floor(length(I)/4))

# =========================
# 5. SMOOTHED PERIODOGRAM
# =========================
I_smooth <- rep(NA, length(I))

for(i in (L+1):(length(I)-L)){
  temp <- 0
  for(l in -L:L){
    temp <- temp + parzen_kernel(l/L) * I[i + l]
  }
  I_smooth[i] <- temp
}

# ambil bagian valid
I_sgph <- I_smooth[(L+1):(L+m)]
omega_sgph <- omega[1:length(I_sgph)]

# =========================
# 6. CLEANING (INI PENTING BIAR LM TIDAK ERROR)
# =========================
Y <- log(I_sgph)
X <- log(4 * (sin(omega_sgph/2))^2)

df <- data.frame(Y = Y, X = X)

# buang NA / Inf / -Inf
df <- df[is.finite(df$Y) & is.finite(df$X), ]

# =========================
# 7. REGRESI SGPH
# =========================
model_sgph <- lm(Y ~ X, data = df)
summary(model_sgph)

# =========================
# 8. ESTIMASI d
# =========================
beta_hat <- coef(model_sgph)[2]
d_hat_sgph <- -beta_hat

d_hat_sgph


differenced2 <- diffseries(ekspor_bc, d = d_sgph)
adf.test(differenced2)
par(mfrow = c(1,2))
acf(differenced2, main=" ")
pacf(differenced2, main=" ")
par(mfrow = c(1,1))

# Bangun model ARFIMA(0, d_SGPH, 0)
modelsgph0d1 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 0),
  fixed = list(frac = d_sgph)   # pakai nilai SGPH sebagai fixed d
)
summary(modelsgph0d1)

model_sgph1 <- arfima(
  z = ekspor_bc,
  order = c(1, 0, 1),
  fixed = list(frac = d_sgph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_sgph1)


model_sgph3 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 3),
  fixed = list(frac = d_sgph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_sgph3)

model_sgph4 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 0),
  fixed = list(frac = d_sgph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_sgph4)

model_sgph5 <- arfima(
  z = ekspor_ts,
  order = c(10, 0, 0),
  fixed = list(frac = d_sgph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_sgph5)

model_sgph6 <- arfima(
  z = ekspor_bc,
  order = c(1, 0, 2),
  fixed = list(frac = d_sgph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_sgph6)

model_sgph7 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 1),
  fixed = list(frac = d_sgph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_sgph7)

model_sgph8 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 2),
  fixed = list(frac = d_sgph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_sgph8)

model_sgph9 <- arfima(
  z = ekspor_ts,
  order = c(1, 0, 0),
  fixed = list(frac = d_sgph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_sgph9)

model_sgph10 <- arfima(
  z = ekspor_bc,
  order = c(3, 0, 3),
  fixed = list(frac = d_sgph)   # pakai nilai SGPH sebagai fixed d
)
summary(model_sgph10)


#model fixed yg diambil model (2,d,2)
resid_list <- residuals(model_sgph8)
residsgph <- as.numeric(resid_list$Mode1)
residsgph <- resid[!is.na(resid)]

par(mfrow=c(1,2))
acf(residsgph, main="ACF Residual",lag.max = 36)
pacf(residsgph, main="PACF Residual")
par(mfrow=c(1,1))
Box.test(residsgph, lag = 24, type = "Ljung-Box")
jb <- jarque.bera.test(residsgph)
jb

#Model (2,d,2)
fc_sgph <- predict(model_sgph8, n.ahead = 9)
fc_sgph
forecast_values <- as.numeric(fc_sgph[[1]]$Forecast)
se_values       <- as.numeric(fc_sgph[[1]]$exactSD)
lambda_bc <- lambda
forecast_sgph <- InvBoxCox(forecast_values, lambda,
                           biasadj=TRUE,
                           fvar = se_values^2)
forecast_sgph
upper_asli    <- InvBoxCox(forecast_values + 1.96*se_values, lambda_bc)
lower_asli    <- InvBoxCox(forecast_values - 1.96*se_values, lambda_bc)
forecast_ts <- ts(forecast_sgph,
                  start = c(2025, 1),
                  frequency = 12)
aktual <- read_excel("C:/Users/acer/Downloads/Skripsi/aktualeks.xlsx")
aktual_values <- as.numeric(aktual$Nilai_Ekspor)
# Pastikan jumlah data sama dengan prediksi
length(aktual_values)  # harus = 21
# MAPE, MSE, RMSE
MAPE <- mean(abs((aktual_values - forecast_sgph) / aktual_values)) * 100
MSE  <- mean((aktual_values - forecast_sgph)^2)
RMSE <- sqrt(MSE)
MAPE
MSE
RMSE
hasil_peramalansgph <- data.frame(
  Bulan   = c("Jan","Feb","Mar","Apr","Mei","Jun","Jul","Agu","Sep"),
  Aktual  = aktual_values,
  Ramalan = forecast_sgph,
  APE     = abs((aktual_values - forecast_sgph) / aktual_values) * 100
)
hasil_peramalansgph




#======EML======#
arf_eml <- fracdiff(ekspor_bc, nar = 1, nma = 1)
arf_eml
d_eml <- arf_eml$d
d_eml
differenced3 <- diffseries(ekspor_bc, d = d_eml)
par(mfrow = c(1,2))
acf(differenced3, main = "ACF EML")
pacf(differenced3, main = "PACF EML")
par(mfrow = c(1,1))
x_fd <- diffseries(ekspor_ts, d = d_eml)
eacf(x_fd)

#kalo d nya tetap
model_1d0 <- arfima(
  z = ekspor_bc,
  order = c(1, 0, 0),
  fixed = list(frac = d_eml)
)
summary(model_1d0)

model_0d1 <- arfima(
  z = ekspor_bc,
  order = c(0, 0, 2),
  fixed = list(frac = d_eml)
)
summary(model_0d1)

model_1d1 <- arfima(
  z = ekspor_bc,
  order = c(1, 0, 1),
  fixed = list(frac = d_eml)
)
summary(model_1d1)

model_0d2 <- arfima(
  z = ekspor_bc,
  order = c(0, 0, 2),
  fixed = list(frac = d_eml)
)
summary(model_0d2)

model_2d0 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 0),
  fixed = list(frac = d_eml)
)
summary(model_2d0)

model_1d2 <- arfima(
  z = ekspor_bc,
  order = c(1, 0, 2),
  fixed = list(frac = d_eml)
)
summary(model_1d2)

model_2d1 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 1),
  fixed = list(frac = d_eml)
)
summary(model_2d1)

model_2d2l <- arfima0(
  z = ekspor_bc,
  order = c(2, 0, 2)
)

model_2d2l
model2 <- arfima0(z=ekspor_bc, order = c(2, 0, 2), lmodel = c("FD", "FGN", "PLA", "NONE"))
model2
arfima_eml <- fracdiff(ekspor_bc, nar = 0, nma = 0)
arfima_eml
#Diagnostik Model EML (2,d,2)
resid_list <- residuals(model_2d2l)
resid <- as.numeric(resid_list$Mode1)
resid <- resid[!is.na(resid)]
par(mfrow=c(1,2))
acf(resid, main="ACF Residual")
pacf(resid, main="PACF Residual")
par(mfrow=c(1,1))
Box.test(resid, lag = 24, type = "Ljung-Box")
jb <- jarque.bera.test(resid)
jb

#Diagnostik Model EML (1,d,1)
resid_list <- residuals(model_1d1)
resid <- as.numeric(resid_list$Mode1)
resid <- resid[!is.na(resid)]
par(mfrow=c(1,2))
acf(resid, main="ACF Residual")
pacf(resid, main="PACF Residual")
par(mfrow=c(1,1))
Box.test(resid, lag = 24, type = "Ljung-Box")
jb <- jarque.bera.test(resid)
jb

#Diagnostik Model EML (1,d,2)
resid_list <- residuals(model_1d2)
resid <- as.numeric(resid_list$Mode1)
resid <- resid[!is.na(resid)]
par(mfrow=c(1,2))
acf(resid, main="ACF Residual")
pacf(resid, main="PACF Residual")
par(mfrow=c(1,1))
Box.test(resid, lag = 24, type = "Ljung-Box")
jb <- jarque.bera.test(resid)
jb

#Peramalan 
#Model EML (2,d,2)
fc <- predict(model_2d2l, n.ahead = 9)
forecast_values <- as.numeric(fc[[1]]$Forecast)
se_values       <- as.numeric(fc[[1]]$exactSD)
lambda_bc <-  lambda
forecast_asli <- InvBoxCox(forecast_values, lambda_bc)
forecast_asli
upper_asli    <- InvBoxCox(forecast_values + 1.96*se_values, lambda_bc)
lower_asli    <- InvBoxCox(forecast_values - 1.96*se_values, lambda_bc)
forecast_ts <- ts(forecast_asli,
                  start = c(2025, 1),
                  frequency = 12)
aktual <- read_excel("C:/Users/acer/Downloads/Skripsi/aktualeks.xlsx")
aktual_values <- as.numeric(aktual$Nilai_Ekspor)
# Pastikan jumlah data sama dengan prediksi
length(aktual_values)  # harus = 21
# MAPE, MSE, RMSE
MAPE <- mean(abs((aktual_values - forecast_asli) / aktual_values)) * 100
MSE  <- mean((aktual_values - forecast_asli)^2)
RMSE <- sqrt(MSE)
MAPE
MSE
RMSE
hasil_peramalaneml2d2 <- data.frame(
  Bulan   = c("Jan","Feb","Mar","Apr","Mei","Jun","Jul","Agu","Sep"),
  Aktual  = aktual_values,
  Ramalan = forecast_asli,
  APE     = abs((aktual_values - forecast_asli) / aktual_values) * 100
)
hasil_peramalaneml2d2

#Model EML (1,d,1)
fc <- predict(model_1d1, n.ahead = 9)
forecast_values <- as.numeric(fc[[1]]$Forecast)
se_values       <- as.numeric(fc[[1]]$exactSD)
lambda_bc <-  lambda
forecast_asli <- InvBoxCox(forecast_values, lambda_bc)
forecast_asli
upper_asli    <- InvBoxCox(forecast_values + 1.96*se_values, lambda_bc)
lower_asli    <- InvBoxCox(forecast_values - 1.96*se_values, lambda_bc)
forecast_ts <- ts(forecast_asli,
                  start = c(2025, 1),
                  frequency = 12)
aktual <- read_excel("C:/Users/acer/Downloads/Skripsi/aktualeks.xlsx")
aktual_values <- as.numeric(aktual$Nilai_Ekspor)
# Pastikan jumlah data sama dengan prediksi
length(aktual_values)  # harus = 21
# MAPE, MSE, RMSE
MAPE <- mean(abs((aktual_values - forecast_asli) / aktual_values)) * 100
MSE  <- mean((aktual_values - forecast_asli)^2)
RMSE <- sqrt(MSE)
MAPE
MSE
RMSE
hasil_peramalaneml1d1 <- data.frame(
  Bulan   = c("Jan","Feb","Mar","Apr","Mei","Jun","Jul","Agu","Sep"),
  Aktual  = aktual_values,
  Ramalan = forecast_asli,
  APE     = abs((aktual_values - forecast_asli) / aktual_values) * 100
)
hasil_peramalaneml1d1

#Model EML (1,d,2)
fc <- predict(model_1d2, n.ahead = 9)
forecast_values <- as.numeric(fc[[1]]$Forecast)
se_values       <- as.numeric(fc[[1]]$exactSD)
lambda_bc <-  lambda
forecast_asli <- InvBoxCox(forecast_values, lambda_bc)
forecast_asli
upper_asli    <- InvBoxCox(forecast_values + 1.96*se_values, lambda_bc)
lower_asli    <- InvBoxCox(forecast_values - 1.96*se_values, lambda_bc)
forecast_ts <- ts(forecast_asli,
                  start = c(2025, 1),
                  frequency = 12)
aktual <- read_excel("C:/Users/acer/Downloads/Skripsi/aktualeks.xlsx")
aktual_values <- as.numeric(aktual$Nilai_Ekspor)
# Pastikan jumlah data sama dengan prediksi
length(aktual_values)  # harus = 21
# MAPE, MSE, RMSE
MAPE <- mean(abs((aktual_values - forecast_asli) / aktual_values)) * 100
MSE  <- mean((aktual_values - forecast_asli)^2)
RMSE <- sqrt(MSE)
MAPE
MSE
RMSE
hasil_peramalaneml1d2 <- data.frame(
  Bulan   = c("Jan","Feb","Mar","Apr","Mei","Jun","Jul","Agu","Sep"),
  Aktual  = aktual_values,
  Ramalan = forecast_asli,
  APE     = abs((aktual_values - forecast_asli) / aktual_values) * 100
)
hasil_peramalaneml1d2


#kalo d nya berbeda tiap model aka scr simultan lgsg
model_0d1 <- fracdiff(ekspor_bc, nar = 2, nma = 2)
model_0d1$stderror.dpq
d_eml <- arf_eml$d
d_eml
eml_model <- fracdiff(ekspor_bc, nar = 2, nma = 2, h = 1e-4)
summary(eml_model)
arma_model <- arima(ekspor_bc, order=c(2,0,2))
AIC(arma_model)



# Estimasi ARFIMA(1,d,1)
model_0d1 <- fracdiff(ekspor_bc, nar = 0, nma = 2, h=1e-6)
model_0d1
d_hat  <- model_0d1$d
d_hat
ma_hat <- model_0d1$ma
ma_hat
se     <- model_0d1$stderror.dpq
se
se_d   <- se["d"]
se_ma  <- se["ma"]
se <- model_0d1$stderror.dpq
se_d  <- se[1]
se_ar <- se[2]
se_ma <- se[3]
z_d  <- d_hat / se_d
z_ar <- model_0d1$ar / se_ar
z_ma <- ma_hat / se_ma
p_d  <- 2 * (1 - pnorm(abs(z_d)))
p_ar <- 2 * (1 - pnorm(abs(z_ar)))
p_ma <- 2 * (1 - pnorm(abs(z_ma)))
hasil <- data.frame(
  Parameter = c("d","AR(1)","MA(1)"),
  Estimate  = c(d_hat, model_0d1$ar, ma_hat),
  Std_Error = c(se_d, se_ar, se_ma),
  z_value   = c(z_d, z_ar, z_ma),
  p_value   = c(p_d, p_ar, p_ma)
)
hasil


model_0d1 <- arfima(
  z = ekspor_bc,
  order = c(0, 0, 1),
  fixed = list(frac = d_eml1)
)
summary(model_0d1)

arf_eml <- fracdiff(ekspor_bc, nar = 0, nma = 1)
d_eml2 <- arf_eml$d
d_eml2
model_0d1 <- arfima(
  z = ekspor_bc,
  order = c(0, 0, 1),
  back = TRUE
)
summary(model_0d1)

model_0d2 <- arfima(
  z = ekspor_bc,
  order = c(0, 0, 2),
  back = TRUE
)
summary(model_0d2)

model_1d0 <- arfima(
  z = ekspor_bc,
  order = c(1, 0, 0),
  back = TRUE
)
summary(model_1d0)


model_2d0 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 0),
  back = TRUE
)
summary(model_2d0)

model_1d1 <- arfima(
  z = ekspor_bc,
  order = c(1, 0, 1),
  back = TRUE
)
summary(model_1d1)


model_1d2 <- arfima(
  z = ekspor_bc,
  order = c(1, 0, 2),
  back = TRUE
)
summary(model_1d2)

model_2d1 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 1),
  back = TRUE
)
summary(model_2d1)

model_2d2 <- arfima(
  z = ekspor_bc,
  order = c(2, 0, 2),
  back = TRUE
)
summary(model_2d2)



#model fixed yg diambil model (2,d,2)
resid_list <- residuals(model_2d2)
resid <- as.numeric(resid_list$Mode1)
resid <- resid[!is.na(resid)]
par(mfrow=c(1,2))
acf(resid, main="ACF Residual",lag.max = 36)
pacf(resid, main="PACF Residual")
par(mfrow=c(1,1))
Box.test(resid, lag = 24, type = "Ljung-Box")
jb <- jarque.bera.test(resid)
jb


#Peramalan 
# Jumlah periode ramalan
fc_eml<- predict(model_2d2, n.ahead = 9)
forecast_values <- as.numeric(fc_eml[[1]]$Forecast)
se_values       <- as.numeric(fc_eml[[1]]$exactSD)

lambda_bc <-  lambda
forecast_eml <- InvBoxCox(forecast_values, lambda,
                          biasadj=TRUE,
                          fvar = se_values^2)
forecast_eml
upper_asli    <- InvBoxCox(forecast_values + 1.96*se_values, lambda_bc)
lower_asli    <- InvBoxCox(forecast_values - 1.96*se_values, lambda_bc)
forecast_ts <- ts(forecast_eml,
                  start = c(2025, 1),
                  frequency = 12)

aktual <- read_excel("C:/Users/acer/Downloads/Skripsi/aktualeks.xlsx")
aktual_values <- as.numeric(aktual$Nilai_Ekspor)
# Pastikan jumlah data sama dengan prediksi
length(aktual_values)  # harus = 21
# MAPE, MSE, RMSE
MAPE <- mean(abs((aktual_values - forecast_eml) / aktual_values)) * 100
MSE  <- mean((aktual_values - forecast_eml)^2)
RMSE <- sqrt(MSE)
MAPE
MSE
RMSE


hasil_peramalan <- data.frame(
  Bulan   = c("Jan","Feb","Mar","Apr","Mei","Jun","Jul","Agu","Sep"),
  Aktual  = aktual_values,
  Ramalan = forecast_eml,
  APE     = abs((aktual_values - forecast_eml) / aktual_values) * 100
)

hasil_peramalan

mean(aktual_values)
mean(forecast_eml)
range(forecast_values)
range(ekspor_bc)

# Time series
aktual_ts <- ts(aktual_values, start=c(2025,1), frequency=12)
gph_ts    <- ts(forecast_gph,  start=c(2025,1), frequency=12)
sgph_ts   <- ts(forecast_sgph, start=c(2025,1), frequency=12)
eml_ts    <- ts(forecast_eml,  start=c(2025,1), frequency=12)

aktual_ts
gph_ts
sgph_ts
eml_ts

bulan <- c("Jan","Feb","Mar","Apr","Mei","Jun","Jul","Agu","Sep")

plot(aktual_ts,
     type="l",
     col="black",
     lwd=1,
     xaxt="n",
     xlab="2025",
     ylab="Nilai Ekspor")

axis(1, at=time(aktual_ts), labels=bulan)

lines(gph_ts,  col="blue",      lwd=1)
lines(sgph_ts, col="red",       lwd=1)
lines(eml_ts,  col="darkgreen", lwd=1)

legend("topleft",
       legend=c("Aktual","GPH (d = 0.47)","SGPH (d = 0.44)","EML (d = 0.39)"),
       col=c("black","blue","red","darkgreen"),
       lty=1,
       lwd=1,
       bty="n")

plot(aktual_ts,
     type="l",
     col="black",
     lwd=2,
     xaxt="n",
     xlab="2025",
     ylab="Nilai Ekspor",
     ylim=range(c(aktual_values, forecast_gph, forecast_sgph, forecast_eml)))

axis(1, at=time(aktual_ts), labels=bulan)

lines(gph_ts,  col="blue",      lwd=2)
lines(sgph_ts, col="red",       lwd=2)
lines(eml_ts,  col="darkgreen", lwd=2)

legend("topleft",
       legend=c("Aktual","GPH","SGPH","EML"),
       col=c("black","blue","red","darkgreen"),
       lty=1,
       lwd=2,
       bty="n")