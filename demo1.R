library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

install.packages("extrafont")
library(extrafont)
font_import()

# 데이터 로드
data <- read.csv("https://raw.githubusercontent.com/Surihub/stat_edu/main/data/%EB%94%B0%EB%A6%89%EC%9D%B4/dr_20240419_20_21.csv")

# 질문 1: 어느 시간대에 자전거 대여가 가장 활발할까?
# '기준_시간대' 별로 데이터 집계
time_agg <- data %>%
  group_by(기준_시간대) %>%
  summarise(전체_건수 = sum(전체_건수))

# 시각화
ggplot(time_agg, aes(x = 기준_시간대, y = 전체_건수)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "시간대별 자전거 대여 건수", x = "시간대 (시)", y = "대여 건수") +
  theme(axis.text.x = element_text(angle = 90))

# 시작 대여소와 종료 대여소별로 데이터 집계
start_station_agg <- data %>%
  count(시작_대여소명) %>%
  top_n(10, wt = n)  # 가장 많은 대여가 시작된 상위 10개 대여소

end_station_agg <- data %>%
  count(종료_대여소명) %>%
  top_n(10, wt = n)  # 가장 많은 대여가 종료된 상위 10개 대여소

# 시각화
ggplot(start_station_agg, aes(x = reorder(시작_대여소명, n), y = n)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "가장 인기 있는 시작 대여소 Top 10", x = "대여소 이름", y = "대여 건수") +
  coord_flip()

ggplot(end_station_agg, aes(x = reorder(종료_대여소명, n), y = n)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "가장 인기 있는 종료 대여소 Top 10", x = "대여소 이름", y = "대여 건수") +
  coord_flip()

# 시작 및 종료 동 별로 데이터 집계
data$시작_동 <- sub("_.*", "", data$시작_대여소명)
data$종료_동 <- sub("_.*", "", data$종료_대여소명)

start_dong_agg <- data %>%
  count(시작_동) %>%
  arrange(desc(n))

# 시각화
ggplot(start_dong_agg, aes(x = reorder(시작_동, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "전체 시작 동 별 대여 건수", x = "동 이름", y = "대여 건수") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90))

# 날짜 형식으로 변환 및 요일 추출
data$기준_날짜 <- ymd(data$기준_날짜)
data$요일 <- weekdays(data$기준_날짜, abbreviate = FALSE)

# 상자그림 그리기
ggplot(data[data$전체_이용_거리 < 20000, ], aes(x = 요일, y = 전체_이용_거리)) +
  geom_boxplot(fill = "dodgerblue") +
  labs(title = "자전거 전체 이용 거리 상자그림", x = "요일", y = "이용 거리 (미터)")