library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)


product_exit<-read.csv("신일pns출고data.csv")
sw_united<-read_excel("PNS_codename_united.xlsx",sheet = "PP BAG (SW 품목)- 이력 현황")
lm_united<-read_excel("PNS_codename_united.xlsx",sheet = "PP BAG(LM 품목)-이력현황")
paper_united<-read_excel("PNS_codename_united.xlsx",sheet = "PAPER BAG")
pe_united<-read_excel("PNS_codename_united.xlsx",sheet = "PE BAG")


str(product_exit)
#기간: 2015/1 ~ 2023/3
#전체데이터: 75819

unique(product_exit$품명)#4099
unique(product_exit$거래처명)#684
product_2022<-product_exit %>% 
  filter(거래명세서일>="2022")
unique(product_2022$거래처명)#385
unique(product_2022$품명)#1680



#SW

sum(is.na(sw_united$품번...12))
sum(is.na(sw_united$품명...13))
sum(is.na(sw_united$품번...14))
sum(is.na(sw_united$품명...15))
sum(is.na(sw_united$품번...16))
sum(is.na(sw_united$품명...17))

names(sw_united)<-c('no',"present_code","present_name",'code1','name1','code2','name2',
                    'code3','name3','code4','name4','code5','name5','code6','name6','code7','name7',
                    'code8','name8','code9','name9')

str(sw_united)



#1
sw_united<-sw_united %>% 
  select('present_code','present_name','code1','code2','code3','code4','code5','code6','code7')

product_old<-product_exit

sw_code1<-sw_united %>% 
  filter(code1!='NA')

sw_code1_present<-sw_code1$present_code
sw_code1_old<-sw_code1$code1

product_old$품번 <- ifelse(product_old$품번 %in% sw_code1_old, sw_code1_present[match(product_old$품번, sw_code1_old)], product_old$품번)

#2

sw_code2<-sw_united %>% 
  filter(code2!='NA')

sw_code2_present<-sw_code2$present_code
sw_code2_old<-sw_code2$code2

product_old$품번 <- ifelse(product_old$품번 %in% sw_code2_old, sw_code2_present[match(product_old$품번, sw_code2_old)], product_old$품번)

#3

sw_code3<-sw_united %>% 
  filter(code3!='NA')

sw_code3_present<-sw_code3$present_code
sw_code3_old<-sw_code3$code3

product_old$품번 <- ifelse(product_old$품번 %in% sw_code3_old, sw_code3_present[match(product_old$품번, sw_code3_old)], product_old$품번)

#4

sw_code4<-sw_united %>% 
  filter(code4!='NA')

sw_code4_present<-sw_code4$present_code
sw_code4_old<-sw_code4$code4

product_old$품번 <- ifelse(product_old$품번 %in% sw_code4_old, sw_code4_present[match(product_old$품번, sw_code4_old)], product_old$품번)

#5

sw_code5<-sw_united %>% 
  filter(code5!='NA')

sw_code5_present<-sw_code5$present_code
sw_code5_old<-sw_code5$code5

product_old$품번 <- ifelse(product_old$품번 %in% sw_code5_old, sw_code5_present[match(product_old$품번, sw_code5_old)], product_old$품번)

#6

sw_code6<-sw_united %>% 
  filter(code6!='NA')

sw_code6_present<-sw_code6$present_code
sw_code6_old<-sw_code6$code6

product_old$품번 <- ifelse(product_old$품번 %in% sw_code6_old, sw_code6_present[match(product_old$품번, sw_code6_old)], product_old$품번)

#7

sw_code7<-sw_united %>% 
  filter(code7!='NA')

sw_code7_present<-sw_code7$present_code
sw_code7_old<-sw_code7$code7

product_old$품번 <- ifelse(product_old$품번 %in% sw_code7_old, sw_code7_present[match(product_old$품번, sw_code7_old)], product_old$품번)

#lm


names(lm_united)<-c('no',"present_code","present_name",'code1','name1','code2','name2',
                    'code3','name3','code4','name4')

str(lm_united)


lm_united<-lm_united %>% 
  select('present_code','present_name','code1','code2','code3','code4')

#1


lm_code1<-lm_united %>% 
  filter(code1!='NA')

lm_code1_present<-lm_code1$present_code
lm_code1_old<-lm_code1$code1

product_old$품번 <- ifelse(product_old$품번 %in% lm_code1_old, lm_code1_present[match(product_old$품번, lm_code1_old)], product_old$품번)

#2

lm_code2<-lm_united %>% 
  filter(code2!='NA')

lm_code2_present<-lm_code2$present_code
lm_code2_old<-lm_code2$code2

product_old$품번 <- ifelse(product_old$품번 %in% lm_code2_old, lm_code2_present[match(product_old$품번, lm_code2_old)], product_old$품번)

#3

lm_code3<-lm_united %>% 
  filter(code3!='NA')

lm_code3_present<-lm_code3$present_code
lm_code3_old<-lm_code3$code3

product_old$품번 <- ifelse(product_old$품번 %in% lm_code3_old, lm_code3_present[match(product_old$품번, lm_code3_old)], product_old$품번)

#4

lm_code4<-lm_united %>% 
  filter(code4!='NA')

lm_code4_present<-lm_code4$present_code
lm_code4_old<-lm_code4$code4

product_old$품번 <- ifelse(product_old$품번 %in% lm_code4_old, lm_code4_present[match(product_old$품번, lm_code4_old)], product_old$품번)

#PP

str(paper_united)

pp_united<-paper_united[,1:10]

names(pp_united)<-c('no',"present_code","present_name",'code1','name1','code2','name2',
                    'code3','name3','code4')


pp_united<-pp_united %>% 
  select('present_code','present_name','code1','code2','code3','code4')


#1


pp_code1<-pp_united %>% 
  filter(code1!='NA')

pp_code1_present<-pp_code1$present_code
pp_code1_old<-pp_code1$code1

product_old$품번 <- ifelse(product_old$품번 %in% pp_code1_old, pp_code1_present[match(product_old$품번, pp_code1_old)], product_old$품번)

#2

pp_code2<-pp_united %>% 
  filter(code2!='NA')

pp_code2_present<-pp_code2$present_code
pp_code2_old<-pp_code2$code2

product_old$품번 <- ifelse(product_old$품번 %in% pp_code2_old, pp_code2_present[match(product_old$품번, pp_code2_old)], product_old$품번)

#3

pp_code3<-pp_united %>% 
  filter(code3!='NA')

pp_code3_present<-pp_code3$present_code
pp_code3_old<-pp_code3$code3

product_old$품번 <- ifelse(product_old$품번 %in% pp_code3_old, pp_code3_present[match(product_old$품번, pp_code3_old)], product_old$품번)

#4

pp_code4<-pp_united %>% 
  filter(code4!='NA')

pp_code4_present<-pp_code4$present_code
pp_code4_old<-pp_code4$code4

product_old$품번 <- ifelse(product_old$품번 %in% pp_code4_old, pp_code4_present[match(product_old$품번, pp_code4_old)], product_old$품번)

#extra

product_old$품번 <- ifelse(product_old$품번 %in% c('2KVKHC0640','2KVKHC0630','2KVKHC0680',
'2KVKHC0540','2KVKHC0660','2KVKHC0560','2KVKHC0580','2KVKHC0690','2KVKHC0510','2KVKHC0600',
'2KVKHC0511'),'2KVKHC0010' , product_old$품번)


#PE

str(pe_united)

pe_united<-pe_united[,1:10]

names(pe_united)<-c('no',"present_code","present_name",'code1','name1','code2','name2',
                    'code3','name3','code4')


pe_united<-pe_united %>% 
  select('present_code','present_name','code1','code2','code3','code4')


#1


pe_code1<-pe_united %>% 
  filter(code1!='NA')

pe_code1_present<-pe_code1$present_code
pe_code1_old<-pe_code1$code1

product_old$품번 <- ifelse(product_old$품번 %in% pe_code1_old, pe_code1_present[match(product_old$품번, pe_code1_old)], product_old$품번)

#2

pe_code2<-pe_united %>% 
  filter(code2!='NA')

pe_code2_present<-pe_code2$present_code
pe_code2_old<-pe_code2$code2

product_old$품번 <- ifelse(product_old$품번 %in% pe_code2_old, pe_code2_present[match(product_old$품번, pe_code2_old)], product_old$품번)

#3

pe_code3<-pe_united %>% 
  filter(code3!='NA')

pe_code3_present<-pe_code3$present_code
pe_code3_old<-pe_code3$code3

product_old$품번 <- ifelse(product_old$품번 %in% pe_code3_old, pe_code3_present[match(product_old$품번, pe_code3_old)], product_old$품번)

#4

pe_code4<-pe_united %>% 
  filter(code4!='NA')

pe_code4_present<-pe_code4$present_code
pe_code4_old<-pe_code4$code4

product_old$품번 <- ifelse(product_old$품번 %in% pe_code4_old, pe_code4_present[match(product_old$품번, pe_code4_old)], product_old$품번)

#extra

product_old$품번 <- ifelse(product_old$품번 %in% c('2PEINO0043','2PEINO0042','2PEINO0041',
                                               '2PEINO0040','2PEINO0030','2PEINO0021'),'2PEINO0064', product_old$품번)

product_old$품번 <- ifelse(product_old$품번 %in% c('2PELTC0061','2PELTC0060'),'2PELCC0060', product_old$품번)




#########################################################

unique(product_exit$품명)#4099
unique(product_exit$거래처명)#684
unique(product_exit$품번)#4003


product_2022<-product_exit %>% 
  filter(거래명세서일>="2022")
unique(product_2022$거래처명)#385
unique(product_2022$품명)#1680
unique(product_2022$품번)#1680


unique(product_old$품명)#4099
unique(product_old$거래처명)#684
unique(product_old$품번)#3797


product_old_2022<-product_old %>% 
  filter(거래명세서일>="2022")
unique(product_old_2022$거래처명)#385
unique(product_old_2022$품명)#1680
unique(product_old_2022$품번)#1582









#상품, 제품군만 확인해달라고 부탁하였으니 다른 유형은 제거

product_old$product_class<-substr(product_old$품번,1,1)
product_old$product_class<-as.integer(product_old$product_class)

product_focused<-product_old %>% 
  filter(product_class==1| product_class==2)

sum(product_focused$product_class==1)
sum(product_focused$product_class==2)

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

#PP BAG: SW, LM, ST
#Sheet: SH
#Paper BAG: KO, KV, KP
#PE BAG: PE
#AL?, LH?, KS?, RL?, 00?, AL?, RP?


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