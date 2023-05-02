product_info<-read.csv("2023.03.31 신일피엔에스 01_품목정보.csv")
month_next<-read.csv("신일pns이월data.csv")
product_entrance<-read.csv("신일pns입고data.csv")
product_exit<-read.csv("신일pns출고data.csv")


library(ggplot2)
library(dplyr)
library(tidyr)

str(product_exit)
#기간: 2015/1 ~ 2023/3
#전체데이터: 75819

unique(product_exit$품명)#4099
unique(product_exit$거래처명)#684
product_2022<-product_exit %>% 
  filter(거래명세서일>="2022")
unique(product_2022$거래처명)#385
unique(product_2022$품명)#1680

product_exit$product_class<-substr(product_exit$품번,1,1)
product_exit$product_class<-as.integer(product_exit$product_class)

#상품, 제품군만 확인해달라고 부탁하였으니 다른 유형은 제거
product_focused<-product_exit %>% 
  filter(product_class==1| product_class==2)

sum(product_focused$product_class==1)
sum(product_focused$product_class==2)
#PP BAG: SW, LM, ST
#Sheet: SH
#Paper BAG: KO, KV, KP
#PE BAG: PE, PER
#AL?, LH?, KS?, RL?, 00?, AL?, RP?

sum(is.na(product_focused$품번))
sum(is.na(product_focused$품명))
sum(is.na(product_focused$출고수량))
sum(is.na(product_focused$거래처명))
sum(is.na(product_focused$거래명세서일))
#전부 결측값 없음



product_focused$거래명세서일<-as.Date(as.character(product_focused$거래명세서일),format="%Y%m%d")
sum(is.na(product_focused$거래명세서일))
#날짜 결측값 없음

#단순히 제품의 스펙을 변경하는 경우, 같은 품목이라고 봐도 될 것 같음
#새로운 제품이 나오는 경우는 새로운 품목이 생성된 것이라고 생각하고
#마지막 자리수가 증가하여 품번명이 달라진 경우는 통일
#같은 제품이지만 다른 고객사인 경우, 같은 제품을 판매했는지는 확인할 방법이 없는듯 함
product_focused$code_length<-nchar(product_focused$품번)
unique(product_focused$code_length)
#품번이 10,11,12,14자리가 있음
sum(product_focused$code_length==10)
sum(product_focused$code_length==11)
sum(product_focused$code_length==12)
sum(product_focused$code_length==14)

leng_11<-product_focused %>% 
  filter(code_length==11)
leng_12<-product_focused %>% 
  filter(code_length==12)
leng_14<-product_focused %>% 
  filter(code_length==14)
#12,14의 경우 7,5 밖에 없어 제거해도 될 듯함


product_focused<-product_focused %>% 
  filter(code_length==10|code_length==11)

product_focused$product_type<-substr(product_focused$품번,2,3)
product_focused$customer<-substr(product_focused$품번,4,6)
product_focused$product_num<-substr(product_focused$품번,nchar(product_focused$품번)-3,nchar(product_focused))
unique(product_focused$product_num)

str(product_focused)
str(product_focused$product_type)
unique(product_focused$product_type)



product_focused$product<-substr(product_focused$품번,1,nchar(product_focused$품번)-1)

product_group<-product_focused %>% 
  group_by(product,거래명세서일,waste) %>% 
  summarize(demand=sum(출고수량))

#{페기} 공통점?
#문의내역 및 실무내용 회의를 통해 크게 2가지 상황 예상
#1. 품목 폐기: 고객사와 커뮤니케이션을 통해, 품목을 폐기한 경우
#2. 고객사 상호 변경: 고객사의 상호가 바뀜에 따라, 품번, 품목이 바뀌는 경우 기존 품목명을 {폐기}로 표기


sum(substr(product_focused$품명,2,3)=="페기")
sum(substr(product_focused$품명,2,3)=="폐기")


product_focused$waste<-substr(product_focused$품명,2,3)
product_focused$waste<-ifelse(product_focused$waste=="페기",1,product_focused$waste)
product_focused$waste<-ifelse(product_focused$waste=="폐기",1,product_focused$waste)
product_focused$waste<-ifelse(product_focused$waste==1,1,0)

product_wasted<-product_focused %>% 
  filter(waste==1)


product_waste_group<-product_wasted %>% 
  group_by(product,거래명세서일) %>% 
  summarize(demand=sum(출고수량))

max(product_waste_group$거래명세서일)
#2023-03-31
#날짜에는 문제가 없는 듯함

#출고수량

#출고수량에 마이너스인게 있다.
#왜일까?
str(product_focused$출고수량)
product_minus_demand<-product_focused %>% 
  filter(출고수량<=0)
sum(product_minus_demand$출고수량==0)
#0인 건 없음

product_minus_demand
#출고량에 -1인 품목들이 꽤 있는데 이것이 의미하는 바가 있나?
#총630품목

sum(product_minus_demand$출고수량==-1)
#우선 다 제거하고 골라보자

product_select<-product_focused %>% 
  filter(waste==0) %>% 
  filter(출고수량>0)

write.csv(product_select,"product_select.csv")

product_8<-product_select %>% 
  filter(product=='2KPKRI001'|product=='2SWKHC007'|product=='2SWKHC003'|product=='2SWKHC005'|
           product=='2SWLGC019'|product=='2LMSIP002'|product=='2SWSIP002'|product=='2KVLTJ003') 
unique(product_8$품명)
