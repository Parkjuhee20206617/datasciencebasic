getwd()
install.packages('dplyr')
library(dplyr)

#1번> csv파일 준비
#따릉이 사용량
bike <- read.csv("ddareung.csv",fileEncoding = "CP949",encoding="UTF-8",header=TRUE,na.strings = '.')
#서울시 미세먼지,초미세먼지량
microdust <- read.csv("대기오염.csv",fileEncoding = "CP949",encoding="UTF-8",header=FALSE,na.strings = '.')


#2,3> 데이터 정제 및 가공
#bike
bike<- subset(bike,select=-place_no) #place_no 제거
bike<- summarize(count = n(),group_by(bike,rental_month),rental_sum = sum(rentals)) 
count_target <- max(bike$count)  # count 열을 맞출 목표 값
bike$count <- ifelse(bike$count < count_target, count_target, bike$count) 
# count가 목표 값보다 작으면 목표 값으로 변경

# rentals의 평균 구하기
bike <- bike %>%mutate(rental_mean = rental_sum / count)  # rentals의 평균 계산
bike = bike %>% rename('month'= 'rental_month') #이름 재정의의
head(bike,10)


#microdust
microdust <- subset(microdust, select = -1) #기준1 삭제
microdust<-as_tibble(t(microdust)) #tibble형태로 변경
colnames(microdust) <- microdust[1,]
colnames(microdust)

microdust = microdust %>% rename('month'= '대기오염')
microdust = microdust[-1,]
microdust <- microdust[,c(1,6,7)]

microdust$month<- as.integer(microdust$month)
microdust$`미세먼지(㎍/㎥/년)`<- as.integer(microdust$`미세먼지(㎍/㎥/년)`) # 정수형으로 변환
microdust$`초미세먼지(㎍/㎥/년)`<- as.integer(microdust$`초미세먼지(㎍/㎥/년)`)
microdust


#데이터 합치기
bike_dust= left_join(bike,microdust,by="month") 
bike_dust<- bike_dust %>% select(month, rental_mean, `미세먼지(㎍/㎥/년)`,`초미세먼지(㎍/㎥/년)` )
bike_dust


#boxplot으로 이상값 확인
boxplot(bike_dust[,2])  #rental_mean
boxplot(bike_dust[,3:4]) #미세먼지량, 초미세먼지량


#summary() 통계량확인
summary(bike_dust)
bike_iqr <- IQR(bike_dust$rental_mean) #rental_mean의 IQR 구하기
bike_dust$rental_mean <- ifelse(bike_dust$rental_mean > summary(bike_dust$rental_mean)[5] + bike_dust_iqr*1.5, NA, bike_dust$rental_mean)

dust1_iqr <- IQR(bike_dust$`미세먼지(㎍/㎥/년)`, na.rm = TRUE) #미세먼지의 IQR 구하기
bike_dust$`미세먼지(㎍/㎥/년)` <- ifelse(bike_dust$`미세먼지(㎍/㎥/년)` > summary(bike_dust$`미세먼지(㎍/㎥/년)`)[5] + dust1_iqr*1.5, NA, bike_dust$`미세먼지(㎍/㎥/년)`)

dust2_iqr <- IQR(bike_dust$`초미세먼지(㎍/㎥/년)`, na.rm = TRUE) #초미세먼지의 IQR 구하기
bike_dust$`초미세먼지(㎍/㎥/년)` <- ifelse(bike_dust$`초미세먼지(㎍/㎥/년)` > summary(bike_dust$`초미세먼지(㎍/㎥/년)`)[5] + dust2_iqr*1.5, NA, bike_dust$`초미세먼지(㎍/㎥/년)`)


#na 확인 및 처리
table(is.na(bike_dust$rental_mean))#NA확인
table(is.na(bike_dust$`미세먼지(㎍/㎥/년)`))#NA확인
table(is.na(bike_dust$`초미세먼지(㎍/㎥/년)`))#NA확인
bike_dust<-na.omit(bike_dust) #결측값제거
bike_dust



#4번> 데이터시각화하기
install.packages("ggplot2")
library(ggplot2)

dev.off() #plots 팔레트 리셋해주기기

# bar, line그래프
p1<-bike_dust %>% ggplot(aes(month,rental_mean)) +
  geom_col(aes(y = `미세먼지(㎍/㎥/년)`, fill = "미세먼지량"))+  #막대그래프요약표
  geom_point(color = "orange", size = 3.5)+geom_line(size = 1.0)+ 
  labs(x = "2020년도 month", y = "미세먼지량에 따른 자전거 평균대여량") +
  ggtitle("월별 미세머지량 및 평균 자전거 대여량") +
  scale_fill_manual(values = c("미세먼지량" = "orange"))+theme_minimal()
  
p2 <-bike_dust %>% ggplot(aes(month,rental_mean)) +
  geom_col(aes(y = `초미세먼지(㎍/㎥/년)`, fill = "초세먼지량"))+
  geom_point(color = "tomato1", size = 3.5)+geom_line(size = 1.0)+
  labs(x = "2020년도 month", y = "초미세먼지에 따른 자전거 평균대여량")+
  ggtitle("월별 초미세머지량 및 평균 자전거 대여량") +
  scale_fill_manual(values = c("초세먼지량" = "tomato1"))+theme_minimal()

p1+p2



#자전거 사용량, 미세먼지, 초미세먼지 막대그래프시각화
ggplot(bike_dust, aes(x = month)) +
  geom_col(aes(y = rental_mean, fill = "자전거 대여량"), width = 0.6) +
  geom_col(aes(y = `미세먼지(㎍/㎥/년)`, fill = "미세먼지량"), width = 0.6) +
  geom_col(aes(y = `초미세먼지(㎍/㎥/년)`, fill = "초미세먼지량"), width = 0.6) +
  labs(x = "2020년도월별", y = "평균 대여량 및 미세먼지,초미세먼지량") +
  scale_fill_manual(values = c("자전거 대여량" = "green4", "미세먼지량" = "gold", "초미세먼지량" = "red")) +
  ggtitle("월별 평균 자전거 대여량 및 미세먼지,초미세먼지량") +
  theme_minimal()




#library(patchwork)

# 월별 평균 자전거 대여량 시각화
plot1 <- ggplot(bike_dust, aes(x = month, y = rental_mean)) +
  geom_line(color = "green4", size = 1.2) +
  labs(x = "월", y = "평균 자전거 대여량") +
  ggtitle("월별 평균 자전거 대여량") + geom_point()+
  theme_minimal()

# 월별 미세먼지량 시각화
plot2 <- ggplot(bike_dust, aes(x = month, y = `미세먼지(㎍/㎥/년)`)) +
  geom_line(color = "orange", size = 1.2) +
  labs(x = "월", y = "평균 미세먼지량") + 
  ggtitle("월별 평균 미세먼지량") + geom_point()+
  theme_minimal()

# 월별 초미세먼지량 시각화
plot3 <- ggplot(bike_dust, aes(x = month, y = `초미세먼지(㎍/㎥/년)`)) +
  geom_line(color = "red", size = 1.2) +
  labs(x = "월", y = "평균 초미세먼지량") +
  ggtitle("월별 평균 초미세먼지량") + geom_point()+
  theme_minimal()

plot1 + plot2 + plot3



#5번> 가설검정단계 수행

#가설검정1
#H0:미세먼지량이 높을 수록 월별 평균 따릉이사용량이 높다.(귀무가설)
#H1:미세먼지량이 높을 수록 월별 평균 따릉이사용량이 낮다.(대립가설)
#가설검정2
#H0:초미세먼지량이 높을 수록 월별 평균 따릉이사용량이 높다.(귀무가설)
#H1:초초미세먼지량이 높을 수록 월별 평균 따릉이사용량이 낮다.(대립가설)

#다중회귀모델생성
bike_bust_model = lm(rental_mean~`미세먼지(㎍/㎥/년)`+`초미세먼지(㎍/㎥/년)`,data = bike_dust)

#가설 검정
summary(bike_bust_model)
#residuals: 훈련집합에 대한 잔차(실제값과 선형회귀모델로 구한 예측값과의 차)

#6번> 모델링, 예측 판별
p_value1 <- summary(bike_bust_model)$coefficients[2, "Pr(>|t|)"]
p_value1
p_value2 <- summary(bike_bust_model)$coefficients[3, "Pr(>|t|)"]
p_value2
alpha <- 0.05  # 유의 수준 설정

if (p_value1 < alpha) {
  cat("귀무 가설(H0)을 기각합니다. 즉, 미세먼지량이 높을수록 월별 평균 따릉이 사용량이 낮을 가능성이 있습니다.")
} else {
  cat("귀무 가설(H0)을 채택합니다. 즉, 미세먼지량이 높을수록 월별 평균 따릉이 사용량이 높을 가능성이 있습니다.")
}

if (p_value2 < alpha) {
  cat("귀무 가설(H0)을 기각합니다. 즉, 초미세먼지량이 높을수록 월별 평균 따릉이 사용량이 낮을 가능성이 있습니다.")
} else {
  cat("귀무 가설(H0)을 채택합니다. 즉, 초미세먼지량이 높을수록 월별 평균 따릉이 사용량이 높을 가능성이 있습니다.")
}

#모두 유의 수준 0.05보다 커서 통계적으로 차이가 없다고 판정할 수 있음


#3차원 시각화
install.packages("scatterplot3d")
library(scatterplot3d)

sp3=scatterplot3d(bike_dust$`미세먼지(㎍/㎥/년)`, bike_dust$`초미세먼지(㎍/㎥/년)`,bike_dust$rental_mean, angle=50)


