## Body fat distribution and visceral fat mediate the effect of age with lower maximal oxygen consumption 
## Data Analysis: Luisa Fern·ndez-Chirino, Neftali Eduardo Antonio-Villa & Omar Yaxmehen Bello-Chavolla (oyaxbell@yahoo.com.mx)
## Latest version of Analysis 13-Feb-2023
## Any question regarding analysis, please contact Omar Yaxmehen Bello-Chavolla

#### Packages ####
library(haven); library(tidyverse); library(dplyr); library(mice); library(nhanesA); library(car)
library(readxl); library(corrplot); library(nortest);library(lmtest); library(dunn.test); 
library(mediation); library(bestNormalize); library(boot); library(simpleboot); library(randtests); 
library(pROC); library(OptimalCutpoints); library(caret); library(ggpubr); library(cowplot);
library(gridExtra);library(interactions); library(broom); library(gvlma); 
library(ppcor); library(sjPlot); library(survival); library(survminer); library(mitools);
library(miceadds); library(VIM); library(readr); library(lme4); library(polypoly);library(dummies); 
library(glmnet); library(jtools); library(epiR); library(reportROC); library(qcc); library(MatchIt)
library(nlme); library(splines); library(simPH); library(data.table); library(ggsci); library(ggpubr);
library(gtsummary); library(lavaan); library(lavaanPlot); library(corrr); library(PerformanceAnalytics)

#### Local WD (ignore)
setwd("C:/Users/verdu/OneDrive - UNIVERSIDAD NACIONAL AUT”NOMA DE M…XICO/CHIPLAB/CVFITNESS_BUENO")
setwd("/Users/luisa0224/Library/CloudStorage/OneDrive-UNIVERSIDADNACIONALAUTOÃÅNOMADEMEÃÅXICO/CHIPLAB/CVFITNESS_BUENO")

#### Load dataset and discretize variables ####
data<-fread("cvfitness.csv") 

data_female<-data%>%filter(RIAGENDR==0)
data_male<-data%>%filter(RIAGENDR==1)

data_female$stat_pfat<-as.factor(ifelse(data_female$DXDTOPF>=30,0,1))
data_female$stat_ice<-as.factor(ifelse(data_female$ICE>=0.5,0,1))
data_female$stat_trunkleg<-as.factor(ifelse(data_female$trunkleg>=0.8, 0, 1))

data_male$stat_age<-as.factor(cut(data_male$RIDAGEYR, breaks=c(17,25,35,45,55),
                                  labels=c("1","2","3","4")))
data_male$stat_pfat<-as.factor(ifelse(data_male$DXDTOPF>=25,0,1))
data_male$stat_ice<-as.factor(ifelse(data_male$ICE>=0.5,0,1))
data_male$stat_trunkleg<-as.factor(ifelse(data_male$trunkleg>=0.8, 0, 1))

stat_vo2<-c()
count<-1

for (i in data_female$CVDESVO2){
  if (data_female$stat_age[count]==1){
    if (i>=56){
      x<-6
    }else if (i>=47){
      x<-5
    }else if (i>=42){
      x<-4
    }else if (i>=38){
      x<-3
    }else if (i>=33){
      x<-2
    }else if (i>=28){
      x<-1
    }else if (i<28){
      x<-0
    }
  }else if (data_female$stat_age[count]==2){
    if (i>=52){
      x<-6
    }else if (i>=45){
      x<-5
    }else if (i>=39){
      x<-4
    }else if (i>=35){
      x<-3
    }else if (i>=31){
      x<-2
    }else if (i>=26){
      x<-1
    }else if (i<26){
      x<-0
    }
  }else if (data_female$stat_age[count]==3){
    if (i>=45){
      x<-6
    }else if (i>=38){
      x<-5
    }else if (i>=34){
      x<-4
    }else if (i>=31){
      x<-3
    }else if (i>=27){
      x<-2
    }else if (i>=22){
      x<-1
    }else if (i<22){
      x<-0
    }
  }else if (data_female$stat_age[count]==4){
    if (i>=40){
      x<-6
    }else if (i>=34){
      x<-5
    }else if (i>=31){
      x<-4
    }else if (i>=28){
      x<-3
    }else if (i>=25){
      x<-2
    }else if (i>=20){
      x<-1
    }else if (i<20){
      x<-0
    }
  }else if (data_female$stat_age[count]==5){
    if (i>=37){
      x<-6
    }else if (i>=32){
      x<-5
    }else if (i>=28){
      x<-4
    }else if (i>=25){
      x<-3
    }else if (i>=22){
      x<-2
    }else if (i>=18){
      x<-1
    }else if (i<18){
      x<-0
    }
  }
  count<-count+1
  stat_vo2<-c(stat_vo2,x)
}
table(stat_vo2)

data_female$stat_vo2<-as.factor(stat_vo2)

stat_vo2<-c()
count<-1

for (i in data_male$CVDESVO2){
  if (data_male$stat_age[count]==1){
    if (i>=60){
      x<-6
    }else if (i>=52){
      x<-5
    }else if (i>=47){
      x<-4
    }else if (i>=42){
      x<-3
    }else if (i>=37){
      x<-2
    }else if (i>=30){
      x<-1
    }else if (i<30){
      x<-0
    }
  }else if (data_male$stat_age[count]==2){
    if (i>=56){
      x<-6
    }else if (i>=49){
      x<-5
    }else if (i>=43){
      x<-4
    }else if (i>=40){
      x<-3
    }else if (i>=35){
      x<-2
    }else if (i>=30){
      x<-1
    }else if (i<30){
      x<-0
    }
  }else if (data_male$stat_age[count]==3){
    if (i>=51){
      x<-6
    }else if (i>=43){
      x<-5
    }else if (i>=39){
      x<-4
    }else if (i>=35){
      x<-3
    }else if (i>=31){
      x<-2
    }else if (i>=26){
      x<-1
    }else if (i<26){
      x<-0
    }
  }else if (data_male$stat_age[count]==4){
    if (i>=45){
      x<-6
    }else if (i>=39){
      x<-5
    }else if (i>=36){
      x<-4
    }else if (i>=32){
      x<-3
    }else if (i>=29){
      x<-2
    }else if (i>=25){
      x<-1
    }else if (i<25){
      x<-0
    }
  }else if (data_male$stat_age[count]==5){
    if (i>=41){
      x<-6
    }else if (i>=36){
      x<-5
    }else if (i>=32){
      x<-4
    }else if (i>=30){
      x<-3
    }else if (i>=26){
      x<-2
    }else if (i>=22){
      x<-1
    }else if (i<22){
      x<-0
    }
  }
  count<-count+1
  stat_vo2<-c(stat_vo2,x)
}

data_male$stat_vo2<-as.factor(stat_vo2)

set.seed(123)
bn_df_train_male<-data_male%>%sample_frac(0.80)
bn_df_test_male<-subset(data_male, !(data_male$SEQN %in% bn_df_train_male$SEQN))

bn_df_train<-data%>%sample_frac(0.80)
bn_df_test<-subset(bn_df, (bn_df$SEQN %in% bn_df_train$SEQN))

#### CV fitness and body composition ####

comparisons<-list(c("1","2"),c("1","3"),c("1","4"),c("2","3"),c("2","4"),c("3","4"))
f1<-data %>% mutate(Sex=factor(RIAGENDR, labels = c("Female", "Male")))%>%
  ggplot(aes(y=CVDESVO2, fill=stat_age, x=stat_age))+
  geom_boxplot()+
  theme_minimal()+
  ylab("Estimated maximal O2 uptake (ml/kg/min)")+
  xlab("Age groups")+
  stat_compare_means(label.x=1, label.y=100)+
  stat_compare_means(comparisons=comparisons, label="p.signif")+
  facet_wrap(~Sex)+
  theme(legend.position="none")


f2a<-data %>% mutate(Sex=factor(RIAGENDR, labels = c("Female", "Male")))%>%
  ggplot(aes(x=trunkleg, y=t_VO2, col=Sex))+geom_point(alpha=0.05, position = "jitter")+
  geom_smooth(method="lm")+theme_classic()+ylab("Estimated maximal O2 uptake (ml/kg/min)")+
  xlab("Trunk fat% vs. leg fat%")
f2b<-data %>% mutate(Sex=factor(RIAGENDR, labels = c("Female", "Male")))%>%
  ggplot(aes(x=trunkextremities, y=t_VO2, col=Sex))+geom_point(alpha=0.05, position = "jitter")+
  geom_smooth(method="lm")+theme_classic()+ylab("Estimated maximal O2 uptake (ml/kg/min)")+
  xlab("Trunk fat vs fat in extremities")
f2c<-data %>% mutate(Sex=factor(RIAGENDR, labels = c("Female", "Male")))%>%
  ggplot(aes(x=updown, y=t_VO2, col=Sex))+geom_point(alpha=0.05, position = "jitter")+
  geom_smooth(method="lm")+theme_classic()+ylab("Estimated maximal O2 uptake (ml/kg/min)")+
  xlab("Body fat distribution")
f2d<-data %>% mutate(Sex=factor(RIAGENDR, labels = c("Female", "Male")))%>%
  ggplot(aes(x=o_P.TFAT, y=t_VO2, col=Sex))+geom_point(alpha=0.05, position = "jitter")+
  geom_smooth(method="lm")+theme_classic()+ylab("Estimated maximal O2 uptake (ml/kg/min)")+
  xlab("Total fat %")

fig2<-ggarrange(f2a, f2b, f2c, f2d, labels = LETTERS[1:4], common.legend = T)
 
ggsave(file = "Figure1.jpg", f1, bg = "transparent",
       width =25, height = 15, units=c("cm"),
       dpi = 500, limitsize = FALSE)
 
ggsave(file = "Figure2.jpg", fig2, bg = "transparent",
       width = 20, height = 20, units=c("cm"),
       dpi = 500, limitsize = FALSE)

###Linear models ####

f.1<-lm(t_VO2~RIDAGEYR, data=data_female)
f.2<-lm(o_P.TFAT~RIDAGEYR, data=data_female)
f.3<-lm(t_VO2~RIDAGEYR+o_P.TFAT, data=data_female)
f.4<-lm(trunkleg~RIDAGEYR, data=data_female)
f.5<-lm(trunkextremities~RIDAGEYR, data=data_female)
f.6<-lm(updown~RIDAGEYR, data=data_female)
f.7<-lm(ICE~RIDAGEYR, data=data_female)
f.8<-lm(t_VO2~trunkleg, data=data_female)
f.9<-lm(t_VO2~trunkextremities, data=data_female)
f.10<-lm(t_VO2~updown, data=data_female)
f.11<-lm(t_VO2~ICE, data=data_female)
f.12<-lm(trunkleg~o_P.TFAT, data=data_female)
f.13<-lm(trunkextremities~o_P.TFAT, data=data_female)
f.14<-lm(updown~o_P.TFAT, data=data_female)
f.15<-lm(ICE~o_P.TFAT, data=data_female)
f.16<-lm(o_P.TFAT~ICE+trunkleg, data=data_female)
f.17<-lm(o_P.TFAT~ICE+trunkextremities, data=data_female)
f.18<-lm(o_P.TFAT~ICE+updown, data=data_female)
f.19<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+trunkleg, data=data_female)
f.20<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+trunkextremities, data=data_female)
f.21<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+updown, data=data_female)

f.22<-glm(t_VO2~stat_age*o_P.TFAT+ICE+trunkextremities, data=data_female)
summary(f.22)

t1<-f.19%>%tbl_regression()
t2<-f.20%>%tbl_regression()
t3<-f.21%>%tbl_regression()

tbl_merge_1<-tbl_merge((tbls = list(t1,t2,t3)), tab_spanner = c("**Grasa troncal/grasa en piernas**",
                                                                      "**Grasa troncal/grasa en extremidades**",
                                                                      "**Grasa segmento superior/inferior**"))

s.1<-plot_model(f.19, type="diag", terms = c("t_VO2", "o_P.TFAT"),
                 show.intercept = T, colors = "Set1", legend.title="Body composition", 
                 axis.title="VO2 max", title="")
print(s.1)
s.2<-plot_model(f.20, type="diag", terms = c("t_VO2", "o_P.TFAT"),
                show.intercept = T, colors = "Set1", legend.title="Body composition", 
                axis.title="VO2 max", title="")
print(s.2)
s.3<-plot_model(f.21, type="diag", terms = c("t_VO2", "o_P.TFAT"),
                show.intercept = T, colors = "Set1", legend.title="Body composition", 
                axis.title="VO2 max", title="")
print(s.3)


f.1<-lm(t_VO2~RIDAGEYR, data=data_female)
f.2<-lm(o_P.TFAT~RIDAGEYR, data=data_female)
f.3<-lm(t_VO2~RIDAGEYR+o_P.TFAT, data=data_female)
f.4<-lm(trunkleg~RIDAGEYR, data=data_female)
f.5<-lm(trunkextremities~RIDAGEYR, data=data_female)
f.6<-lm(updown~RIDAGEYR, data=data_female)
f.7<-lm(ICE~RIDAGEYR, data=data_female)
f.8<-lm(t_VO2~trunkleg, data=data_female)
f.9<-lm(t_VO2~trunkextremities, data=data_female)
f.10<-lm(t_VO2~updown, data=data_female)
f.11<-lm(t_VO2~ICE, data=data_female)
f.12<-lm(trunkleg~o_P.TFAT, data=data_female)
f.13<-lm(trunkextremities~o_P.TFAT, data=data_female)
f.14<-lm(updown~o_P.TFAT, data=data_female)
f.15<-lm(ICE~o_P.TFAT, data=data_female)
f.16<-lm(o_P.TFAT~ICE+trunkleg, data=data_female)
f.17<-lm(o_P.TFAT~ICE+trunkextremities, data=data_female)
f.18<-lm(o_P.TFAT~ICE+updown, data=data_female)
f.19<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+trunkleg, data=data_female)
f.20<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+trunkextremities, data=data_female)
f.21<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+updown, data=data_female)

f.22<-glm(t_VO2~stat_age*o_P.TFAT+ICE+trunkextremities, data=data_female)
summary(f.22)

t1<-f.19%>%tbl_regression()
t2<-f.20%>%tbl_regression()
t3<-f.21%>%tbl_regression()

tbl_merge_1<-tbl_merge((tbls = list(t1,t2,t3)), tab_spanner = c("**Grasa troncal/grasa en piernas**",
                                                                "**Grasa troncal/grasa en extremidades**",
                                                                "**Grasa segmento superior/inferior**"))

s.1<-plot_model(f.19, type="diag", terms = c("t_VO2", "o_P.TFAT"),
                show.intercept = T, colors = "Set1", legend.title="Body composition", 
                axis.title="VO2 max", title="")
print(s.1)
s.2<-plot_model(f.20, type="diag", terms = c("t_VO2", "o_P.TFAT"),
                show.intercept = T, colors = "Set1", legend.title="Body composition", 
                axis.title="VO2 max", title="")
print(s.2)
s.3<-plot_model(f.21, type="diag", terms = c("t_VO2", "o_P.TFAT"),
                show.intercept = T, colors = "Set1", legend.title="Body composition", 
                axis.title="VO2 max", title="")
print(s.3)

#Male dataset
m.1<-lm(t_VO2~RIDAGEYR, data=data_male)
m.2<-lm(o_P.TFAT~RIDAGEYR, data=data_male)
m.3<-lm(t_VO2~RIDAGEYR+o_P.TFAT, data=data_male)
m.4<-lm(trunkleg~RIDAGEYR, data=data_male)
m.5<-lm(trunkextremities~RIDAGEYR, data=data_male)
m.6<-lm(updown~RIDAGEYR, data=data_male)
m.7<-lm(ICE~RIDAGEYR, data=data_male)
m.8<-lm(t_VO2~trunkleg, data=data_male)
m.9<-lm(t_VO2~trunkextremities, data=data_male)
m.10<-lm(t_VO2~updown, data=data_male)
m.11<-lm(t_VO2~ICE, data=data_male)
m.12<-lm(trunkleg~o_P.TFAT, data=data_male)
m.13<-lm(trunkextremities~o_P.TFAT, data=data_male)
m.14<-lm(updown~o_P.TFAT, data=data_male)
m.15<-lm(ICE~o_P.TFAT, data=data_male)
m.16<-lm(o_P.TFAT~ICE+trunkleg, data=data_male)
m.17<-lm(o_P.TFAT~ICE+trunkextremities, data=data_male)
m.18<-lm(o_P.TFAT~ICE+updown, data=data_male)
m.19<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+trunkleg, data=data_male)
m.20<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+trunkextremities, data=data_male)
m.21<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+updown, data=data_male)

m.22<-glm(t_VO2~stat_age*o_P.TFAT+ICE+trunkextremities, data=data_male)
summary(m.22)

t4<-m.19%>%tbl_regression()
t5<-m.20%>%tbl_regression()
t6<-m.21%>%tbl_regression()

tbl_merge_2<-tbl_merge((tbls = list(t4,t5,t6)), tab_spanner = c("**Grasa troncal/grasa en piernas**",
                                                                "**Grasa troncal/grasa en extremidades**",
                                                                "**Grasa segmento superior/inferior**"))

s.4<-plot_model(m.19, type="diag", terms = c("t_VO2", "o_P.TFAT"),
                show.intercept = T, colors = "Set1", legend.title="Body composition", 
                axis.title="VO2 max", title="")
print(s.4)
s.5<-plot_model(m.20, type="diag", terms = c("t_VO2", "o_P.TFAT"),
                show.intercept = T, colors = "Set1", legend.title="Body composition", 
                axis.title="VO2 max", title="")
print(s.5)
s.6<-plot_model(m.21, type="diag", terms = c("t_VO2", "o_P.TFAT"),
                show.intercept = T, colors = "Set1", legend.title="Body composition", 
                axis.title="VO2 max", title="")
print(s.6)

#Causality models

model1 <- '
  # direct effect
    t_VO2 ~ g*RIDAGEYR
  # mediator
    o_P.TFAT~a*RIDAGEYR
    ICE~b*o_P.TFAT
    trunkleg~c*o_P.TFAT
    t_VO2~d*o_P.TFAT
    t_VO2~e*trunkleg
    t_VO2~f*ICE
  # indirect effect (a*b)
    ab:=a*d
    cd:=b*c*e*f
  # total effect
    total:=g+(a*d)+(b*c*e*f)

'

model2 <- '
  # direct effect
    t_VO2 ~ g*RIDAGEYR
  # mediator
    o_P.TFAT~a*RIDAGEYR
    ICE~b*o_P.TFAT
    trunkextremities~c*o_P.TFAT
    t_VO2~d*o_P.TFAT
    t_VO2~e*trunkextremities
    t_VO2~f*ICE
  # indirect effect (a*b)
    ab:=a*d
    cd:=b*c*e*f
  # total effect
    total:=g+(a*d)+(b*c*e*f)

'

model3 <- '
  # direct effect
    t_VO2 ~ g*RIDAGEYR
  # mediator
    o_P.TFAT~a*RIDAGEYR
    ICE~b*o_P.TFAT
    updown~c*o_P.TFAT
    t_VO2~d*o_P.TFAT
    t_VO2~e*updown
    t_VO2~f*ICE
  # indirect effect (a*b)
    ab:=a*d
    cd:=b*c*e*f
  # total effect
    total:=g+(a*d)+(b*c*e*f)

'

#1. SEM
fit1_f <- sem(model1, data = data_female)
summary(fit1_f, standardized = TRUE, modindices = TRUE)
interpret(fit1_f)
sem1<-parameterEstimates(fit1_f, standardized=T)
BIC(fit1_f)
vcov(fit1_f)
fitted(fit1_f)
labels <- list(RIDAGEYR = "Edad", o_P.TFAT = "%Grasa total", ICE = "ICE", trunkleg = "T/P", t_VO2 = "VO2m·x")
lavaanPlot(model = fit1_f, labels=labels, node_options=list(fontname="Helvetica"),coefs=T, stand=T, covs=T, stars=c("parameters"))
mi1<-summary(fit1_f, standardized = TRUE, modindices = TRUE)$mi%>%dplyr::select(lhs, op, rhs, mi, sepc.all)%>%
  rename("Efector"=lhs, "Operador"=op, "Resultado"=rhs, "IM"=mi, "Estimador"=sepc.all)%>%remove_rownames()


fit1_m <- sem(model1, data = data_male)
summary(fit1_m, standardized = TRUE, modindices = TRUE)
interpret(fit1_m)
sem2<-parameterEstimates(fit1_m, standardized=T)
BIC(fit1_m)
vcov(fit1_m)
fitted(fit1_m)
lavaanPlot(model = fit1_m, labels=labels, node_options=list(fontname="Helvetica"),coefs=T, stand=T, covs=T, stars=c("parameters"))
mi2<-summary(fit1_m, standardized = TRUE, modindices = TRUE)$mi%>%dplyr::select(lhs, op, rhs, mi, sepc.all)%>%
  rename("Efector"=lhs, "Operador"=op, "Resultado"=rhs, "IM"=mi, "Estimador"=sepc.all)%>%remove_rownames()


labels <- list(RIDAGEYR = "Edad", o_P.TFAT = "%Grasa total", trunkextremities = "T/E", ICE = "ICE", t_VO2 = "VO2m·x")
fit2_f<-sem(model2, data=data_female)
summary(fit2_f, standardized = TRUE, modindices = TRUE)
interpret(fit2_f)
sem3<-parameterEstimates(fit2_f, standardized=T)
BIC(fit2_f)
vcov(fit2_f)
fitted(fit2_f)
lavaanPlot(model = fit2_f, labels=labels, node_options=list(fontname="Helvetica"),coefs=T, stand=T, covs=T, stars=c("parameters"))
mi3<-summary(fit2_f, standardized = TRUE, modindices = TRUE)$mi%>%dplyr::select(lhs, op, rhs, mi, sepc.all)%>%
  rename("Efector"=lhs, "Operador"=op, "Resultado"=rhs, "IM"=mi, "Estimador"=sepc.all)%>%remove_rownames()


fit2_m<-sem(model2, data=data_male)
summary(fit2_m, standardized = TRUE, modindices = TRUE)
interpret(fit2_m)
sem4<-parameterEstimates(fit2_m, standardized=T)
BIC(fit2_m)
vcov(fit2_m)
fitted(fit2_m)
lavaanPlot(model = fit2_m, labels=labels, node_options=list(fontname="Helvetica"),coefs=T, stand=T, covs=T, stars=c("parameters"))
mi4<-summary(fit2_m, standardized = TRUE, modindices = TRUE)$mi%>%dplyr::select(lhs, op, rhs, mi, sepc.all)%>%
  rename("Efector"=lhs, "Operador"=op, "Resultado"=rhs, "IM"=mi, "Estimador"=sepc.all)%>%remove_rownames()

labels <- list(RIDAGEYR = "Edad", o_P.TFAT = "%Grasa total", updown = "S/I", ICE = "ICE", t_VO2 = "VO2m·x")
fit3_f<-sem(model3, data=data_female)
summary(fit3_f, standardized = TRUE, modindices = TRUE)
interpret(fit3_f)
sem5<-parameterEstimates(fit3_f, standardized=T)
BIC(fit3_f)
vcov(fit3_f)
fitted(fit3_f)
lavaanPlot(model = fit3_f, labels=labels, node_options=list(fontname="Helvetica"),coefs=T, stand=T, covs=T, stars=c("parameters"))
mi5<-summary(fit3_f, standardized = TRUE, modindices = TRUE)$mi%>%dplyr::select(lhs, op, rhs, mi, sepc.all)%>%
  rename("Efector"=lhs, "Operador"=op, "Resultado"=rhs, "IM"=mi, "Estimador"=sepc.all)%>%remove_rownames()

fit3_m<-sem(model3, data=data_male)
summary(fit3_m, standardized = TRUE, modindices = TRUE)
interpret(fit3_m)
sem6<-parameterEstimates(fit3_m, standardized=T)
BIC(fit3_m)
vcov(fit3_m)
fitted(fit3_m)
lavaanPlot(model = fit3_m, labels=labels, node_options=list(fontname="Helvetica"),coefs=T, stand=T, covs=T, stars=c("parameters"))
mi6<-summary(fit3_m, standardized = TRUE, modindices = TRUE)$mi%>%dplyr::select(lhs, op, rhs, mi, sepc.all)%>%
  rename("Efector"=lhs, "Operador"=op, "Resultado"=rhs, "IM"=mi, "Estimador"=sepc.all)%>%remove_rownames()

#SEM table

header<-c("Coeficiente", "Estimador", "95IC", "valor P", "Estimador", "95IC", "valor P","Estimador", "95IC", "valor P")
r1<-c(sem1$label[1], round(sem1$est[1],4), paste(round(sem1$ci.lower[1],4)," - ", round(sem1$ci.upper[1],4) ), round(sem1$pvalue[1],4),
      round(sem3$est[1],4), paste(round(sem3$ci.lower[1],4)," - ", round(sem3$ci.upper[1],4) ), round(sem3$pvalue[1],4),
      round(sem5$est[1],4), paste(round(sem5$ci.lower[1],4)," - ", round(sem5$ci.upper[1],4) ), round(sem5$pvalue[1],4))
r2<-c(sem1$label[2], round(sem1$est[2],4), paste(round(sem1$ci.lower[2],4)," - ", round(sem1$ci.upper[2],4) ), round(sem1$pvalue[2],4),
      round(sem3$est[2],4), paste(round(sem3$ci.lower[2],4)," - ", round(sem3$ci.upper[2],4) ), round(sem3$pvalue[2],4),
      round(sem5$est[2],4), paste(round(sem5$ci.lower[2],4)," - ", round(sem5$ci.upper[2],4) ), round(sem5$pvalue[2],4))
r3<-c(sem1$label[3], round(sem1$est[3],4), paste(round(sem1$ci.lower[3],4)," - ", round(sem1$ci.upper[3],4) ), round(sem1$pvalue[3],4),
      round(sem3$est[3],4), paste(round(sem3$ci.lower[3],4)," - ", round(sem3$ci.upper[3],4) ), round(sem3$pvalue[3],4),
      round(sem5$est[3],4), paste(round(sem5$ci.lower[3],4)," - ", round(sem5$ci.upper[3],4) ), round(sem5$pvalue[3],4))
r4<-c(sem1$label[4], round(sem1$est[4],4), paste(round(sem1$ci.lower[4],4)," - ", round(sem1$ci.upper[4],4) ), round(sem1$pvalue[4],4),
      round(sem3$est[4],4), paste(round(sem3$ci.lower[4],4)," - ", round(sem3$ci.upper[4],4) ), round(sem3$pvalue[4],4),
      round(sem5$est[4],4), paste(round(sem5$ci.lower[4],4)," - ", round(sem5$ci.upper[4],4) ), round(sem5$pvalue[4],4))
r5<-c(sem1$label[5], round(sem1$est[5],4), paste(round(sem1$ci.lower[5],4)," - ", round(sem1$ci.upper[5],4) ), round(sem1$pvalue[5],4),
      round(sem3$est[5],4), paste(round(sem3$ci.lower[5],4)," - ", round(sem3$ci.upper[5],4) ), round(sem3$pvalue[5],4),
      round(sem5$est[5],4), paste(round(sem5$ci.lower[5],4)," - ", round(sem5$ci.upper[5],4) ), round(sem5$pvalue[5],4))
r6<-c(sem1$label[6], round(sem1$est[6],4), paste(round(sem1$ci.lower[6],4)," - ", round(sem1$ci.upper[6],4) ), round(sem1$pvalue[6],4),
      round(sem3$est[6],4), paste(round(sem3$ci.lower[6],4)," - ", round(sem3$ci.upper[6],4) ), round(sem3$pvalue[6],4),
      round(sem5$est[6],4), paste(round(sem5$ci.lower[6],4)," - ", round(sem5$ci.upper[6],4) ), round(sem5$pvalue[6],4))
r7<-c(sem1$label[7], round(sem1$est[7],4), paste(round(sem1$ci.lower[7],4)," - ", round(sem1$ci.upper[7],4) ), round(sem1$pvalue[7],4),
      round(sem3$est[7],4), paste(round(sem3$ci.lower[7],4)," - ", round(sem3$ci.upper[7],4) ), round(sem3$pvalue[7],4),
      round(sem5$est[7],4), paste(round(sem5$ci.lower[7],4)," - ", round(sem5$ci.upper[7],4) ), round(sem5$pvalue[7],4))
r8<-c(sem1$label[13], round(sem1$est[13],4), paste(round(sem1$ci.lower[13],4)," - ", round(sem1$ci.upper[13],4) ), round(sem1$pvalue[13],4),
      round(sem3$est[13],4), paste(round(sem3$ci.lower[13],4)," - ", round(sem3$ci.upper[13],4) ), round(sem3$pvalue[13],4),
      round(sem5$est[13],4), paste(round(sem5$ci.lower[13],4)," - ", round(sem5$ci.upper[13],4) ), round(sem5$pvalue[13],4))
r9<-c(sem1$label[14], round(sem1$est[14],4), paste(round(sem1$ci.lower[14],4)," - ", round(sem1$ci.upper[14],4) ), round(sem1$pvalue[14],4),
      round(sem3$est[14],4), paste(round(sem3$ci.lower[14],4)," - ", round(sem3$ci.upper[14],4) ), round(sem3$pvalue[14],4),
      round(sem5$est[14],4), paste(round(sem5$ci.lower[14],4)," - ", round(sem5$ci.upper[14],4) ), round(sem5$pvalue[14],4))

tab_sem1<-rbind(header, r1,r2,r3,r4,r5,r6,r7,r8,r9)
knitr::kable(tab_sem1, "latex")

header<-c("Coeficiente", "Estimador", "95IC", "valor P", "Estimador", "95IC", "valor P","Estimador", "95IC", "valor P")
r1<-c(sem2$label[1], round(sem2$est[1],4), paste(round(sem2$ci.lower[1],4)," - ", round(sem2$ci.upper[1],4) ), round(sem2$pvalue[1],4),
      round(sem4$est[1],4), paste(round(sem4$ci.lower[1],4)," - ", round(sem4$ci.upper[1],4) ), round(sem4$pvalue[1],4),
      round(sem6$est[1],4), paste(round(sem6$ci.lower[1],4)," - ", round(sem6$ci.upper[1],4) ), round(sem6$pvalue[1],4))
r2<-c(sem2$label[2], round(sem2$est[2],4), paste(round(sem2$ci.lower[2],4)," - ", round(sem1$ci.upper[2],4) ), round(sem2$pvalue[2],4),
      round(sem4$est[2],4), paste(round(sem4$ci.lower[2],4)," - ", round(sem4$ci.upper[2],4) ), round(sem4$pvalue[2],4),
      round(sem6$est[2],4), paste(round(sem6$ci.lower[2],4)," - ", round(sem6$ci.upper[2],4) ), round(sem6$pvalue[2],4))
r3<-c(sem2$label[3], round(sem2$est[3],4), paste(round(sem2$ci.lower[3],4)," - ", round(sem1$ci.upper[3],4) ), round(sem2$pvalue[3],4),
      round(sem4$est[3],4), paste(round(sem4$ci.lower[3],4)," - ", round(sem4$ci.upper[3],4) ), round(sem4$pvalue[3],4),
      round(sem6$est[3],4), paste(round(sem6$ci.lower[3],4)," - ", round(sem6$ci.upper[3],4) ), round(sem6$pvalue[3],4))
r4<-c(sem2$label[4], round(sem2$est[4],4), paste(round(sem2$ci.lower[4],4)," - ", round(sem1$ci.upper[4],4) ), round(sem2$pvalue[4],4),
      round(sem4$est[4],4), paste(round(sem4$ci.lower[4],4)," - ", round(sem4$ci.upper[4],4) ), round(sem4$pvalue[4],4),
      round(sem6$est[4],4), paste(round(sem6$ci.lower[4],4)," - ", round(sem6$ci.upper[4],4) ), round(sem6$pvalue[4],4))
r5<-c(sem2$label[5], round(sem2$est[5],4), paste(round(sem2$ci.lower[5],4)," - ", round(sem1$ci.upper[5],4) ), round(sem2$pvalue[5],4),
      round(sem4$est[5],4), paste(round(sem4$ci.lower[5],4)," - ", round(sem4$ci.upper[5],4) ), round(sem4$pvalue[5],4),
      round(sem6$est[5],4), paste(round(sem6$ci.lower[5],4)," - ", round(sem6$ci.upper[5],4) ), round(sem6$pvalue[5],4))
r6<-c(sem2$label[6], round(sem2$est[6],4), paste(round(sem2$ci.lower[6],4)," - ", round(sem1$ci.upper[6],4) ), round(sem2$pvalue[6],4),
      round(sem4$est[6],4), paste(round(sem4$ci.lower[6],4)," - ", round(sem4$ci.upper[6],4) ), round(sem4$pvalue[6],4),
      round(sem6$est[6],4), paste(round(sem6$ci.lower[6],4)," - ", round(sem6$ci.upper[6],4) ), round(sem6$pvalue[6],4))
r7<-c(sem2$label[7], round(sem2$est[7],4), paste(round(sem2$ci.lower[7],4)," - ", round(sem1$ci.upper[7],4) ), round(sem2$pvalue[7],4),
      round(sem4$est[7],4), paste(round(sem4$ci.lower[7],4)," - ", round(sem4$ci.upper[7],4) ), round(sem4$pvalue[7],4),
      round(sem6$est[7],4), paste(round(sem6$ci.lower[7],4)," - ", round(sem6$ci.upper[7],4) ), round(sem6$pvalue[7],4))
r8<-c(sem2$label[13], round(sem2$est[13],4), paste(round(sem2$ci.lower[13],4)," - ", round(sem1$ci.upper[13],4) ), round(sem2$pvalue[13],4),
      round(sem4$est[13],4), paste(round(sem4$ci.lower[13],4)," - ", round(sem4$ci.upper[13],4) ), round(sem4$pvalue[13],4),
      round(sem6$est[13],4), paste(round(sem6$ci.lower[13],4)," - ", round(sem6$ci.upper[13],4) ), round(sem6$pvalue[13],4))
r9<-c(sem2$label[14], round(sem2$est[14],4), paste(round(sem2$ci.lower[14],4)," - ", round(sem1$ci.upper[14],4) ), round(sem2$pvalue[14],4),
      round(sem4$est[14],4), paste(round(sem4$ci.lower[14],4)," - ", round(sem4$ci.upper[14],4) ), round(sem4$pvalue[14],4),
      round(sem6$est[14],4), paste(round(sem6$ci.lower[14],4)," - ", round(sem6$ci.upper[14],4) ), round(sem6$pvalue[14],4))

tab_sem2<-rbind(header, r1,r2,r3,r4,r5,r6,r7,r8,r9)
knitr::kable(tab_sem2, "latex")

#3. Mediations

set.seed(123)
ac.1_m<-lm(ICE~o_P.TFAT+trunkleg, data=data_male)
summary(ac.1_m)

bac.1_m<-lm(t_VO2~o_P.TFAT+ICE+trunkleg, data=data_male)
summary(bac.1_m)

med.1_m<- mediation::mediate(ac.1_m, bac.1_m, treat = "o_P.TFAT",
                           mediator = "ICE", covariates = "trunkleg", sims = 100, boot.ci.type = "perc", boot = T)
summary(med.1_m)
a<-summary(med.1_m)

ac.2_m<-lm(ICE~o_P.TFAT+trunkextremities, data=data_male)
summary(ac.2_m)

bac.2_m<-lm(t_VO2~o_P.TFAT+ICE+trunkextremities, data=data_male)
summary(bac.2_m)

med.2_m<- mediation::mediate(ac.2_m, bac.2_m, treat = "o_P.TFAT",
                           mediator = "ICE", covariates = "trunkextremities", sims = 100, boot.ci.type = "perc", boot = T)
summary(med.2_m)
b<-summary(med.2_m)

ac.3_m<-lm(ICE~o_P.TFAT+updown, data=data_male)
summary(ac.3_m)

bac.3_m<-lm(t_VO2~o_P.TFAT+ICE+updown, data=data_male)
summary(bac.3_m)

med.3_m<- mediation::mediate(ac.3_m, bac.3_m, treat = "o_P.TFAT",
                           mediator = "ICE", covariates = "updown", sims = 100, boot.ci.type = "perc", boot = T)
summary(med.3_m)
c<-summary(med.3_m)

ac.1.1_m<-lm(o_P.TFAT~RIDAGEYR+ICE+trunkleg, data=data_male)
summary(ac.1.1_m)

bac.1.1_m<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+trunkleg, data=data_male)
summary(bac.1.1_m)

med.1.1_m<- mediation::mediate(ac.1.1_m, bac.1.1_m, treat = "RIDAGEYR", mediator = "o_P.TFAT", 
                             covariates=c("trunkleg","ICE"), sims = 100, boot.ci.type = "perc", boot = T)
summary(med.1.1_m)
d<-summary(med.1.1_m)


ac.1.2_m<-lm(o_P.TFAT~RIDAGEYR+ICE+trunkextremities, data=data_male)
summary(ac.1.2)

bac.1.2_m<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+trunkextremities, data=data_male)
summary(bac.1.2_m)

med.1.2_m<- mediation::mediate(ac.1.2_m, bac.1.2_m, treat = "RIDAGEYR", mediator = "o_P.TFAT", 
                             covariates=c("trunkextremities","ICE"), sims = 100, boot.ci.type = "perc", boot = T)
summary(med.1.2_m)
e<-summary(med.1.2_m)


ac.1.3_m<-lm(o_P.TFAT~RIDAGEYR+ICE+updown, data=data_male)
summary(ac.1.3_m)

bac.1.3_m<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+updown, data=data_male)
summary(bac.1.3_m)

med.1.3_m<- mediation::mediate(ac.1.3_m, bac.1.3_m, treat = "RIDAGEYR", mediator = "o_P.TFAT", 
                             covariates=c("updown","ICE"), sims = 100, boot.ci.type = "perc", boot = T)
summary(med.1.3_m)
f<-summary(med.1.3_m)

#Mediation table
header<-c("Modelo","Efector","Mediador","Covariable(s)","Resultado","ACME","ADE","Efecto total","Mediado")
r1<-c("A","Grasa total","ICE","T/P","VO2m·x",
      paste0(round(a$d1, 4),"\\(",round(a$d1.ci[1],4),"-",round(a$d1.ci[2],4),")"),
      paste0(round(a$z1, 4),"\\(",round(a$z1.ci[1],4),"-",round(a$z1.ci[2],4),")"),
      paste0(round(a$tau.coef, 4),"\\(",round(a$tau.ci[1],4),"-",round(a$tau.ci[2],4),")"),
      paste0(round(a$n1, 4)*100,"\\(",round(a$n1.ci[1],4)*100,"-",round(a$n1.ci[2],4)*100,")"))
r2<-c("B","Grasa total","ICE","T/EX","VO2m·x",
      paste0(round(b$d1, 4),"\\(",round(b$d1.ci[1],4),"-",round(b$d1.ci[2],4),")"),
      paste0(round(b$z1, 4),"\\(",round(b$z1.ci[1],4),"-",round(b$z1.ci[2],4),")"),
      paste0(round(b$tau.coef, 4),"\\(",round(b$tau.ci[1],4),"-",round(b$tau.ci[2],4),")"),
      paste0(round(b$n1, 4)*100,"\\(",round(b$n1.ci[1],4)*100,"-",round(b$n1.ci[2],4)*100,")")) 
r3<-c("C","Grasa total","ICE","S/I","VO2m·x",
      paste0(round(c$d1, 4),"\\(",round(c$d1.ci[1],4),"-",round(c$d1.ci[2],4),")"),
      paste0(round(c$z1, 4),"\\(",round(c$z1.ci[1],4),"-",round(c$z1.ci[2],4),")"),
      paste0(round(c$tau.coef, 4),"\\(",round(c$tau.ci[1],4),"-",round(c$tau.ci[2],4),")"),
      paste0(round(c$n1, 4)*100,"\\(",round(c$n1.ci[1],4)*100,"-",round(c$n1.ci[2],4)*100,")"))
r4<-c("D","Edad","Grasa total","T/P, ICE","VO2m·x",
      paste0(round(d$d1, 4),"\\(",round(d$d1.ci[1],4),"-",round(d$d1.ci[2],4),")"),
      paste0(round(d$z1, 4),"\\(",round(d$z1.ci[1],4),"-",round(d$z1.ci[2],4),")"),
      paste0(round(d$tau.coef, 4),"\\(",round(d$tau.ci[1],4),"-",round(d$tau.ci[2],4),")"),
      paste0(round(d$n1, 4)*100,"\\(",round(d$n1.ci[1],4)*100,"-",round(d$n1.ci[2],4)*100,")"))
r5<-c("E","Edad","Grasa total","T/EX, ICE","VO2m·x",
      paste0(round(e$d1, 4),"\\(",round(e$d1.ci[1],4),"-",round(e$d1.ci[2],4),")"),
      paste0(round(e$z1, 4),"\\(",round(e$z1.ci[1],4),"-",round(e$z1.ci[2],4),")"),
      paste0(round(e$tau.coef, 4),"\\(",round(e$tau.ci[1],4),"-",round(e$tau.ci[2],4),")"),
      paste0(round(e$n1, 4)*100,"\\(",round(e$n1.ci[1],4)*100,"-",round(e$n1.ci[2],4)*100,")"))
r6<-c("F","Edad","Grasa total","S/I, ICE","VO2m·x",
      paste0(round(f$d1, 4),"\\(",round(f$d1.ci[1],4),"-",round(f$d1.ci[2],4),")"),
      paste0(round(f$z1, 4),"\\(",round(f$z1.ci[1],4),"-",round(f$z1.ci[2],4),")"),
      paste0(round(f$tau.coef, 4),"\\(",round(f$tau.ci[1],4),"-",round(f$tau.ci[2],4),")"),
      paste0(round(f$n1, 4)*100,"\\(",round(f$n1.ci[1],4)*100,"-",round(f$n1.ci[2],4)*100,")"))

tab_med1<-rbind(header, r1,r2,r3,r4,r5,r6)
knitr::kable(tab_med1, "latex")

set.seed(123)
ac.1_f<-lm(ICE~o_P.TFAT+trunkleg, data=data_female)
summary(ac.1_f)

bac.1_f<-lm(t_VO2~o_P.TFAT+ICE+trunkleg, data=data_female)
summary(bac.1_f)

med.1_f<- mediation::mediate(ac.1_f, bac.1_f, treat = "o_P.TFAT",
                           mediator = "ICE", covariates = "trunkleg", sims = 100, boot.ci.type = "perc", boot = T)
summary(med.1_f)
a<-summary(med.1_f)

ac.2_f<-lm(ICE~o_P.TFAT+trunkextremities, data=data)
summary(ac.2_f)

bac.2_f<-lm(t_VO2~o_P.TFAT+ICE+trunkextremities, data=data)
summary(bac.2_f)

med.2_f<- mediation::mediate(ac.2_f, bac.2_f, treat = "o_P.TFAT",
                           mediator = "ICE", covariates = "trunkextremities", sims = 100, boot.ci.type = "perc", boot = T)
summary(med.2_f)
b<-summary(med.2_f)

ac.3_f<-lm(ICE~o_P.TFAT+updown, data=data_female)
summary(ac.3_f)

bac.3_f<-lm(t_VO2~o_P.TFAT+ICE+updown, data=data_female)
summary(bac.3_f)

med.3_f<- mediation::mediate(ac.3_f, bac.3_f, treat = "o_P.TFAT",
                           mediator = "ICE", covariates = "updown", sims = 100, boot.ci.type = "perc", boot = T)
summary(med.3_f)
c<-summary(med.3_f)

ac.1.1_f<-lm(o_P.TFAT~RIDAGEYR+ICE+trunkleg, data=data_female)
summary(ac.1.1_f)

bac.1.1_f<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+trunkleg, data=data_female)
summary(bac.1.1_f)

med.1.1_f<- mediation::mediate(ac.1.1_f, bac.1.1_f, treat = "RIDAGEYR", mediator = "o_P.TFAT", 
                             covariates=c("trunkleg","ICE"), sims = 100, boot.ci.type = "perc", boot = T)
summary(med.1.1_f)
d<-summary(med.1.1_f)

ac.1.2_f<-lm(o_P.TFAT~RIDAGEYR+ICE+trunkextremities, data=data_female)
summary(ac.1.2_f)

bac.1.2_f<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+trunkextremities, data=data_female)
summary(bac.1.2_f)

med.1.2_f<- mediation::mediate(ac.1.2_f, bac.1.2_f, treat = "RIDAGEYR", mediator = "o_P.TFAT", 
                             covariates=c("trunkextremities","ICE"), sims = 100, boot.ci.type = "perc", boot = T)
summary(med.1.2_f)
e<-summary(med.1.2_f)


ac.1.3_f<-lm(o_P.TFAT~RIDAGEYR+ICE+updown, data=data_female)
summary(ac.1.3_f)

bac.1.3_f<-lm(t_VO2~RIDAGEYR+o_P.TFAT+ICE+updown, data=data_female)
summary(bac.1.3_f)

med.1.3_f<- mediation::mediate(ac.1.3_f, bac.1.3_f, treat = "RIDAGEYR", mediator = "o_P.TFAT", 
                             covariates=c("updown","ICE"), sims = 100, boot.ci.type = "perc", boot = T)
summary(med.1.3_f)
f<-summary(med.1.1_f)

r1<-c("A","Grasa total","ICE","T/P","VO2m·x",
      paste0("makecell{",round(a$d1, 4),"(",round(a$d1.ci[1],4)," - ",round(a$d1.ci[2],4),")}"),
      paste0("makecell{",round(a$z1, 4),"(",round(a$z1.ci[1],4)," - ",round(a$z1.ci[2],4),")}"),
      paste0("makecell{",round(a$tau.coef, 4),"(",round(a$tau.ci[1],4)," - ",round(a$tau.ci[2],4),")}"),
      paste0("makecell{",round(a$n1, 4)*100,"% (",round(a$n1.ci[1],4)*100,"% - ",round(a$n1.ci[2],4)*100,"%)}"))
r2<-c("B","Grasa total","ICE","T/EX","VO2m·x",
      paste0("makecell{",round(b$d1, 4),"(",round(b$d1.ci[1],4)," - ",round(b$d1.ci[2],4),")}"),
      paste0("makecell{",round(b$z1, 4),"(",round(b$z1.ci[1],4)," - ",round(b$z1.ci[2],4),")}"),
      paste0("makecell{",round(b$tau.coef, 4),"(",round(b$tau.ci[1],4)," - ",round(b$tau.ci[2],4),")}"),
      paste0("makecell{",round(b$n1, 4)*100,"% (",round(b$n1.ci[1],4)*100,"% - ",round(b$n1.ci[2],4)*100,"%)}")) 
r3<-c("C","Grasa total","ICE","S/I","VO2m·x",
      paste0("makecell{",round(c$d1, 4),"(",round(c$d1.ci[1],4)," - ",round(c$d1.ci[2],4),")}"),
      paste0("makecell{",round(c$z1, 4),"(",round(c$z1.ci[1],4)," - ",round(c$z1.ci[2],4),")}"),
      paste0("makecell{",round(c$tau.coef, 4),"(",round(c$tau.ci[1],4)," - ",round(c$tau.ci[2],4),")}"),
      paste0("makecell{",round(c$n1, 4)*100,"% (",round(c$n1.ci[1],4)*100,"% - ",round(c$n1.ci[2],4)*100,"%)}"))
r4<-c("D","Edad","Grasa total","T/P, ICE","VO2m·x",
      paste0("makecell{",round(d$d1, 4),"(",round(d$d1.ci[1],4)," - ",round(d$d1.ci[2],4),")}"),
      paste0("makecell{",round(d$z1, 4),"(",round(d$z1.ci[1],4)," - ",round(d$z1.ci[2],4),")}"),
      paste0("makecell{",round(d$tau.coef, 4),"(",round(d$tau.ci[1],4)," - ",round(d$tau.ci[2],4),")}"),
      paste0("makecell{",round(d$n1, 4)*100,"% (",round(d$n1.ci[1],4)*100,"% - ",round(d$n1.ci[2],4)*100,"%)}"))
r5<-c("E","Edad","Grasa total","T/EX, ICE","VO2m·x",
      paste0("makecell{",round(e$d1, 4),"(",round(e$d1.ci[1],4)," - ",round(e$d1.ci[2],4),")}"),
      paste0("makecell{",round(e$z1, 4),"(",round(e$z1.ci[1],4)," - ",round(e$z1.ci[2],4),")}"),
      paste0("makecell{",round(e$tau.coef, 4),"(",round(e$tau.ci[1],4)," - ",round(e$tau.ci[2],4),")}"),
      paste0("makecell{",round(e$n1, 4)*100,"(",round(e$n1.ci[1],4)*100," - ",round(e$n1.ci[2],4)*100,")}"))
r6<-c("F","Edad","Grasa total","S/I, ICE","VO2m·x",
      paste0("makecell{",round(f$d1, 4),"(",round(f$d1.ci[1],4)," - ",round(f$d1.ci[2],4),")}"),
      paste0("makecell{",round(f$z1, 4),"(",round(f$z1.ci[1],4)," - ",round(f$z1.ci[2],4),")}"),
      paste0("makecell{",round(f$tau.coef, 4),"(",round(f$tau.ci[1],4)," - ",round(f$tau.ci[2],4),")}"),
      paste0("makecell{",round(f$n1, 4)*100,"% (",round(f$n1.ci[1],4)*100,"% - ",round(f$n1.ci[2],4)*100,"%)}"))

tab_med2<-rbind(header, r1,r2,r3,r4,r5,r6)
knitr::kable(tab_med2, "latex")

knitr::kable(r1, "latex")


#Tables

sum_data<-data%>%dplyr::select(DXDTOPF, RIDAGEYR, RIDRETH1, CVDESVO2, BMI, ICE, trunkleg,
                               trunkextremities,updown,RIAGENDR)%>%mutate(fatlegs=data$DXDLLPF+data$DXDRLPF, 
                                                                          fatarms=data$DXXRAFAT+data$DXXLAFAT)
colnames(sum_data)<-c("Grasa total (%)", "Edad (AÒos)", "Etnicidad", "Volumen m·ximo de oxÌgeno (mL/kg/min)", 
                      "IMC", "ICE", "Grasa en piernas/grasa troncal", "Grasa en extremidades/grasa troncal",
                      "DistribuciÛn grasa inferior vs superior","GENDER","Grasa en piernas","Grasa en brazos")

table1 <- tbl_summary(sum_data, by = GENDER, missing = "no") %>% add_n() %>% add_p()

table1 %>% as_gt() %>%  gt::gtsave(filename = "tab1.tex") 
tbl_merge_1 %>% as_gt() %>%  gt::gtsave(filename = "tab2.tex") 
tbl_merge_2 %>% as_gt() %>%  gt::gtsave(filename = "tab3.tex") 

sum_data1<-sum_data%>%dplyr::select(`Grasa total (%)`, `Edad (AÒos)`, `Volumen m·ximo de oxÌgeno (mL/kg/min)`,
                                    IMC, ICE, `Grasa en piernas/grasa troncal`, `Grasa en extremidades/grasa troncal`,
                                    `DistribuciÛn grasa inferior vs superior`)


plot(sum_data1)
pairs(sum_data1)
chart.Correlation(sum_data1, histogram=TRUE, pch=19)


