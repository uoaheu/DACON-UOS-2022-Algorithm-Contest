# 🏆 2022 UOS 빅데이터 알고리즘 경진대회 - 대상 (1등)
**서울시 지역구별 따릉이 대여량 예측**을 주제로 진행된 **2022 UOS 빅데이터 알고리즘 경진대회**에서 **대상 (1등)**을 수상한 프로젝트입니다.  

---

## 대회 개요
- **주최 :** 서울시립대학교  
- **주관 :** 데이콘 ([대회 링크](https://dacon.io/competitions/official/236029/overview/description))  
- **주제 :** 서울시 지역구별 일별 따릉이 대여량 예측  
- **기간 :** 2022.11.14 ~ 2022.12.12  
- **평가 지표 :** MAE (Mean Absolute Error)  
- **최종 결과 : 대상 (1등)**  

---

## 프로젝트 소개
- **사용 언어 :** R  
- **주요 기술 :** 시계열 분석 (Holt-Winters DES), 데이터 전처리 (`tidyverse`, `data.table`, `lubridate` 등), 시각화 (`ggplot2`)  
- **특징**  
  - 시계열 예측을 위한 **Holt-Winters 모델** 사용  
  - **Penalty Adjustment**를 통한 예측 오차 보정  
  - **주말과 평일**의 따릉이 대여 패턴 분석  

---

## 주요 기능 설명
- 데이터 전처리 : 결측치 처리 및 시계열 형식으로 변환
- 시계열 예측 : Holt-Winters DES 모델을 이용한 예측
- 오차 보정 : 특정 기간 동안의 Penalty Adjustment로 예측 성능 향상
- 검증 및 평가 : MAE를 기준으로 예측 성능 평가

---

## 주요 성과
- 대상 (1등) 수상 : 발표 평가 및 코드 심사를 통해 최종 1등 달성
- MAE 최적화 : Penalty Adjustment 기법으로 예측 오차 최소화
- 효율적인 예산 관리 : 정확한 수요 예측을 통해 따릉이 대여소 확장 및 신규 도입의 효율성 제고
