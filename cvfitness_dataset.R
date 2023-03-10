##Paquetes####
library(haven); library(tidyverse); library(dplyr); library(mice); library(nhanesA); 
library(readxl); library(corrplot); library(nortest);library(lmtest); library(dunn.test); 
library(mediation); library(bestNormalize); library(boot); library(simpleboot); library(randtests); 
library(pROC); library(OptimalCutpoints); library(caret); library(ggpubr); library(cowplot);
library(gridExtra); library(GmAMisc); library(interactions); library(broom); library(gvlma); 
library(ppcor); library(sjPlot); library(survival); library(survminer); library(mitools);
library(miceadds); library(VIM); library(readr); library(lme4); library(polypoly);library(dummies); 
library(glmnet); library(jtools); library(epiR); library(reportROC); library(qcc); library(MatchIt)
library(nlme); library(splines); library(simPH)

#setwd("C:/Users/B4-SUR-4/OneDrive - UNIVERSIDAD NACIONAL AUT?NOMA DE M?XICO/CVFITNESS")
setwd("C:/Users/verdu/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CHIPLAB/CVFITNESS_BUENO")

##Functions#### 
categorize<-function(x){
  #Categorizes discrete variable into 1 and 0, preserving 1. Missing data substitutes as 0.
  c1<-c()
  name<-deparse(substitute(x))
  for (value in x){
    if (is.na(value)){
      cat=0
    }else if (value==1){
      cat=1
    }else{
      cat=0
    }
    c1<-c(c1, cat)
  }
  return(name<-c1)
}

categorizeinv<-function(x){
  #Categorizes discrete variable into 1 and 0, preserving 0. Missing data substitutes as 0.
  c1<-c()
  name<-deparse(substitute(x))
  for (value in x){
    if (is.na(value)){
      cat=0
    }else if (value==0){
      cat=0
    }else{
      cat=1
    }
    c1<-c(c1, cat)
  }
  return(name<-c1)
}

categ<-function(x, lim){
  #With a given limit, dichotomizes continuous variables, with greater-than criteria. 
  #Missing data substitutes as 0.
  category<-c()
  for(value in x){
    if(is.na(value)){
      cat=0
    }else if (value<lim){
      cat=0
    }else{
      cat=1
    }
    category<-c(category,cat)
  }
  return(category)
}

inv.categ<-function(x, lim){
  #With a given limit, dichotomizes continuous variables, with less-than criteria.
  #missing data substitutes as 0.
  category<-c()
  for(value in x){
    if(is.na(value)){
      cat=0
    }else if (value>lim){
      cat=0
    }else{
      cat=1
    }
    category<-c(category,cat)
  }
  return(category)
}

cap<-function(x){
  #Substitutes outliers in continuous variable with value in third or first quartile, respectively.
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

pool.BIC<-function(data, x1, x2){
  #Averages BIC obtained from multiple models given by mice-imputed datasets and determines best model.
  BIC1<-c()
  m<-max(data$.imp)
  for(i in 1:m){
    model.imputed1 <- lm((x1), data = data[which(data$.imp == m),])
    BIC1[i] <- BIC(model.imputed1)
  }
  B1<-c(mean(BIC1))
  BIC2<-c()
  m<-max(data$.imp)
  for(i in 1:m){
    model.imputed2 <- lm((x2), data = data[which(data$.imp == m),])
    BIC2[i] <- BIC(model.imputed2)
  }
  B2<-c(mean(BIC2)) 
  if(abs(B2-B1)>2){
    if(B2>B1){
      return(c("Select first model. BIC: ",B1))
    }else{
      return(c("Select second model. BIC: ",B2))
    }
  }else{
    return(c("Select simpler model. BIC1: ",B1,". BIC2: ",B2))
  }
}

pool.BIC.r<-function(data, x1, x2){
  #Averages BIC obtained from multiple models with random effects 
  #given by mice-imputed datasets and determines best model.
  BIC1<-c()
  m<-max(data$.imp)
  for(i in 1:m){
    model.imputed1 <- lmer((x1), data = data[which(data$.imp == m),])
    BIC1[i] <- BIC(model.imputed1)
  }
  B1<-c(mean(BIC1))
  BIC2<-c()
  m<-max(data$.imp)
  for(i in 1:m){
    model.imputed2 <- lmer((x2), data = data[which(data$.imp == m),])
    BIC2[i] <- BIC(model.imputed2)
  }
  B2<-c(mean(BIC2)) 
  if(abs(B2-B1)>2){
    if(B2>B1){
      return(c("Select first model. BIC: ",B1))
    }else{
      return(c("Select second model. BIC: ",B2))
    }
  }else{
    return(c("Select simpler model. BIC1: ",B1,". BIC2: ",B2))
  }
}

BICS<-function(x){
  bics=c()
  mod=c()
  name=deparse(substitute(x))
  for(i in 1:length(x)){
    b=mean(pool(x[[i]])$glanced$BIC) 
    bics=c(bics,b)
  }
  name=bics 
  return(data.frame("Model"=1:length(x),"BIC"=name))}


####LABS####

lab06.2000<-nhanes("LAB06")%>%dplyr::select(SEQN, LBXHCY)
lab06.2002<-nhanes("L06_B")%>%dplyr::select(SEQN, LBDHCY)
lab06.2004<-nhanes("L06MH_C")%>%dplyr::select(SEQN, LBXHCY)

lab10.2000<-nhanes("LAB10")%>%dplyr::select(SEQN, LBXGH)
lab10.2002<-nhanes("L10_B")%>%dplyr::select(SEQN, LBXGH)
lab10.2004<-nhanes("L10_C")%>%dplyr::select(SEQN, LBXGH)

lab10AM.2000<-nhanes("LAB10AM")%>%dplyr::select(SEQN, LBXGLU, LBXIN)
lab10AM.2002<-nhanes("L10AM_B")%>%dplyr::select(SEQN, LBXGLU, LBXIN)
lab10AM.2004<-nhanes("L10AM_C")%>%dplyr::select(SEQN, LBXGLU, LBXIN)

lab11.2000<-nhanes("LAB11")%>%dplyr::select(SEQN, LBXCRP)
lab11.2002<-nhanes("L11_B")%>%dplyr::select(SEQN, LBXCRP)
lab11.2004<-nhanes("L11_C")%>%dplyr::select(SEQN, LBXCRP)

lab13ct.2000<-nhanes("Lab13")%>%dplyr::select(SEQN, LBXTC)
lab13ct.2002<-nhanes("l13_b")%>%dplyr::select(SEQN, LBXTC)
lab13ct.2004<-nhanes("l13_c")%>%dplyr::select(SEQN, LBXTC)

lab13hdl.2000<-nhanes("Lab13")%>%dplyr::select(SEQN, LBDHDL)
lab13hdl.2002<-nhanes("l13_b")%>%dplyr::select(SEQN, LBDHDL)
lab13hdl.2004<-nhanes("l13_c")%>%dplyr::select(SEQN, LBXHDD)

lab13.2000<-merge(lab13ct.2000, lab13hdl.2000, by="SEQN", all.x=TRUE)
lab13.2002<-merge(lab13ct.2002, lab13hdl.2002, by="SEQN", all.x=TRUE)
lab13.2004<-merge(lab13ct.2004, lab13hdl.2004, by="SEQN", all.x=TRUE)

lab13AM.2000<-nhanes("LAB13AM")%>%dplyr::select(SEQN, LBDLDL, LBXTR)
lab13AM.2002<-nhanes("L13AM_B")%>%dplyr::select(SEQN, LBDLDL, LBXTR)
lab13AM.2004<-nhanes("L13AM_C")%>%dplyr::select(SEQN, LBDLDL, LBXTR)

lab18.2000<-nhanes("LAB18")%>%dplyr::select(SEQN, LBXSUA, LBXSCLSI, LBXSNASI, LBXSKSI)
lab18.2002<-nhanes("L40_B")%>%dplyr::select(SEQN, LBXSUA, LBXSCLSI, LBXSNASI, LBXSKSI)
lab18.2004<-nhanes("L40_C")%>%dplyr::select(SEQN, LBXSUA, LBXSCLSI, LBXSNASI, LBXSKSI)

lab25.2000<-nhanes("LAB25")%>%dplyr::select(SEQN, LBXHCT, LBXHGB)
lab25.2002<-nhanes("L25_B")%>%dplyr::select(SEQN, LBXHCT, LBXHGB)
lab25.2004<-nhanes("L25_C")%>%dplyr::select(SEQN, LBXHCT, LBXHGB)

lab.2000<-list(lab25.2000, lab06.2000, lab11.2000, lab13ct.2000, lab13hdl.2000, lab18.2000,
               lab10.2000, lab13AM.2000, lab10AM.2000)
labs.2000<-Reduce(function(x,y) merge(x,y,by="SEQN",all.x=TRUE), lab.2000)

lab.2002<-list(lab25.2002, lab06.2002, lab11.2002, lab13ct.2002, lab13hdl.2002, lab18.2002,
               lab10.2002, lab13AM.2002, lab10AM.2002)
labs.2002<-Reduce(function(x,y) merge(x,y,by="SEQN",all.x=TRUE), lab.2002)

lab.2004<-list(lab25.2004, lab06.2004, lab11.2004, lab13ct.2004, lab13hdl.2004, lab18.2004,
               lab10.2004, lab13AM.2004, lab10AM.2004)
labs.2004<-Reduce(function(x,y) merge(x,y,by="SEQN",all.x=TRUE), lab.2004)

####ANTROPOMETRIA####

BMX.2000<-nhanes("BMX")%>%dplyr::select(SEQN, BMXWT, BMXHT, BMXWAIST)
BMX.2002<-nhanes("BMX_B")%>%dplyr::select(SEQN, BMXWT, BMXHT, BMXWAIST)
BMX.2004<-nhanes("BMX_C")%>%dplyr::select(SEQN, BMXWT, BMXHT, BMXWAIST)

BPX.2000<-nhanes("BPX")%>%dplyr::select(SEQN, BPXSY1, BPXDI1,
                                        BPXSY2, BPXDI2, BPXSY3, BPXDI3,
                                        BPXSY4, BPXDI4)
BPX.2002<-nhanes("BPX_B")%>%dplyr::select(SEQN, BPXSY1, BPXDI1,
                                          BPXSY2, BPXDI2, BPXSY3, BPXDI3,
                                          BPXSY4, BPXDI4)
BPX.2004<-nhanes("BPX_C")%>%dplyr::select(SEQN, BPXSY1, BPXDI1,
                                          BPXSY2, BPXDI2, BPXSY3, BPXDI3,
                                          BPXSY4, BPXDI4)

PLS.2000<-nhanes("BPX")%>%dplyr::select(SEQN, BPXPLS)
PLS.2002<-nhanes("BPX_B")%>%dplyr::select(SEQN, BPXPLS)
PLS.2004<-nhanes("BPX_C")%>%dplyr::select(SEQN, BPXPLS)

BPXSBP.2000<-BPX.2000%>%dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXSY4)
BPXSBP.2002<-BPX.2002%>%dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXSY4)
BPXSBP.2004<-BPX.2004%>%dplyr::select(SEQN, BPXSY1, BPXSY2, BPXSY3, BPXSY4)

BPXSBP.2000$BPXSAVG<-apply(BPXSBP.2000[c("BPXSY1", "BPXSY2", "BPXSY3", 
                                         "BPXSY4")], 1, mean, na.rm=TRUE)
BPXSBP.2002$BPXSAVG<-apply(BPXSBP.2002[c("BPXSY1", "BPXSY2", "BPXSY3", 
                                         "BPXSY4")], 1, mean, na.rm=TRUE)
BPXSBP.2004$BPXSAVG<-apply(BPXSBP.2004[c("BPXSY1", "BPXSY2", "BPXSY3", 
                                         "BPXSY4")], 1, mean, na.rm=TRUE)

BPXDBP.2000<-BPX.2000%>%dplyr::select(SEQN, BPXDI1, BPXDI2, BPXDI3, BPXDI4)
BPXDBP.2002<-BPX.2002%>%dplyr::select(SEQN, BPXDI1, BPXDI2, BPXDI3, BPXDI4)
BPXDBP.2004<-BPX.2004%>%dplyr::select(SEQN, BPXDI1, BPXDI2, BPXDI3, BPXDI4)

BPXDBP.2000$BPXDAVG<-apply(BPXDBP.2000[c("BPXDI1", "BPXDI2", "BPXDI3", 
                                         "BPXDI4")], 1, mean, na.rm=TRUE)
BPXDBP.2002$BPXDAVG<-apply(BPXDBP.2002[c("BPXDI1", "BPXDI2", "BPXDI3", 
                                         "BPXDI4")], 1, mean, na.rm=TRUE)
BPXDBP.2004$BPXDAVG<-apply(BPXDBP.2004[c("BPXDI1", "BPXDI2", "BPXDI3", 
                                         "BPXDI4")], 1, mean, na.rm=TRUE)

BPXT.2000<-merge(BPXDBP.2000, BPXSBP.2000, by="SEQN", 
                 all=TRUE)%>%dplyr::select(SEQN, BPXDAVG, BPXSAVG)
BPXT.2002<-merge(BPXDBP.2002, BPXSBP.2002, by="SEQN", 
                 all=TRUE)%>%dplyr::select(SEQN, BPXDAVG, BPXSAVG)
BPXT.2004<-merge(BPXDBP.2004, BPXSBP.2004, by="SEQN",
                 all=TRUE)%>%dplyr::select(SEQN, BPXDAVG, BPXSAVG)

BPXT.2000$BPXAVG<-(BPXT.2000$BPXDAVG+BPXT.2000$BPXSAVG)/2
BPXT.2002$BPXAVG<-(BPXT.2002$BPXDAVG+BPXT.2002$BPXSAVG)/2
BPXT.2004$BPXAVG<-(BPXT.2004$BPXDAVG+BPXT.2004$BPXSAVG)/2

BPXS.2000<-merge(PLS.2000, BPXT.2000, by="SEQN", all=TRUE)
BPXS.2002<-merge(PLS.2002, BPXT.2002, by="SEQN", all=TRUE)
BPXS.2004<-merge(PLS.2004, BPXT.2004, by="SEQN", all=TRUE)

LEXAB.2000<-nhanes("LEXABPI")%>%dplyr::select(SEQN, LEXBRPM, LEXLABPI,
                                              LEXRABPI)
LEXAB.2002<-nhanes("LEXAB_B")%>%dplyr::select(SEQN, LEXBRPM, LEXLABPI,
                                              LEXRABPI)
LEXAB.2004<-nhanes("LEXAB_C")%>%dplyr::select(SEQN, LEXBRPM, LEXLABPI,
                                              LEXRABPI)

CVX.2000<-nhanes("CVX")%>%dplyr::select(SEQN, CVDESVO2, CVDFITLV)
CVX.2002<-nhanes("CVX_B")%>%dplyr::select(SEQN, CVDESVO2, CVDFITLV)
CVX.2004<-nhanes("CVX_C")%>%dplyr::select(SEQN, CVDESVO2, CVDFITLV)

antro.2000<-list(BMX.2000, BPXS.2000, CVX.2000, LEXAB.2000)
antros.2000<-Reduce(function(x,y) merge(x,y,by="SEQN",all=TRUE), antro.2000)

antro.2002<-list(BMX.2002, BPXS.2002, CVX.2002, LEXAB.2002)
antros.2002<-Reduce(function(x,y) merge(x,y,by="SEQN",all=TRUE), antro.2002)

antro.2004<-list(BMX.2004, BPXS.2004, CVX.2004, LEXAB.2004)
antros.2004<-Reduce(function(x,y) merge(x,y,by="SEQN",all=TRUE), antro.2004)

####CUESTIONARIO####

BPQ.2000<-nhanes("BPQ")%>%dplyr::select(SEQN, BPQ020, BPQ040A)
BPQ.2002<-nhanes("BPQ_B")%>%dplyr::select(SEQN, BPQ020, BPQ040A)
BPQ.2004<-nhanes("BPQ_C")%>%dplyr::select(SEQN, BPQ020, BPQ040A)

MCQ.2000<-read_xpt("MCQ.xpt")%>%dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, 
                                              MCQ160E,  MCQ160F, MCQ160K, MCQ220, MCQ010, MCQ030)
MCQ.2002<-read_xpt("MCQ_B.xpt")%>%dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, 
                                                MCQ160E, MCQ160F, MCQ160K, MCQ220, MCQ010, 
                                                MCQ035)%>%rename(MCQ030=MCQ035)
MCQ.2004<-read_xpt("MCQ_C.xpt")%>%dplyr::select(SEQN, MCQ160B, MCQ160C, MCQ160D, 
                                                MCQ160E, MCQ160F, MCQ160K, MCQ220, MCQ010, 
                                                MCQ035)%>%rename(MCQ030=MCQ035)

SMQ.2000<-nhanes("SMQ")%>%dplyr::select(SEQN, SMQ040)
SMQ.2002<-nhanes("SMQ_B")%>%dplyr::select(SEQN, SMQ040)
SMQ.2004<-nhanes("SMQ_C")%>%dplyr::select(SEQN, SMQ040)

DIQ.2000<-nhanes("DIQ")%>%dplyr::select(SEQN, DIQ010, DIQ040G, DIQ050, DIQ070)
DIQ.2002<-nhanes("DIQ_B")%>%dplyr::select(SEQN, DIQ010, DID040G, DIQ050, DIQ070)%>%rename(DIQ040G=DID040G)
DIQ.2004<-nhanes("DIQ_C")%>%dplyr::select(SEQN, DIQ010, DID040G, DIQ050, DIQ070)%>%rename(DIQ040G=DID040G)

quest.2000<-list(BPQ.2000, MCQ.2000, DIQ.2000, SMQ.2000)
quests.2000<-Reduce(function(x,y) merge(x,y,by="SEQN",all=TRUE), quest.2000)

quest.2002<-list(BPQ.2002, MCQ.2002, DIQ.2002, SMQ.2002)
quests.2002<-Reduce(function(x,y) merge(x,y,by="SEQN",all=TRUE), quest.2002)

quest.2004<-list(BPQ.2004, MCQ.2004, DIQ.2004, SMQ.2004)
quests.2004<-Reduce(function(x,y) merge(x,y,by="SEQN",all=TRUE), quest.2004)

####DXA####

#DXA.2000<-nhanesDXA(1999)%>%dplyr::select(SEQN, DXDTOPF, DXDTOFAT, DXDTOLE, DXDTOLI,
#                                          DXXTRFAT, DXDTRPF, DXDTRLE, DXXTRLI, DXXHEFAT, 
#                                          DXDHEPF,DXXLAFAT, DXDLAPF, DXXLLFAT, DXDLLPF, 
#                                          DXXRAFAT, DXDRAPF, DXXRLFAT, DXDRLPF, DXDLALE, 
#                                          DXDLLLE, DXDRALE, DXDRLLE)
DXA.2000<-read_xpt("dxx.xpt")%>%dplyr::select(SEQN, DXDTOPF, DXDTOFAT, DXDTOLE, DXDTOLI,
                                              DXXTRFAT, DXDTRPF, DXDTRLE, DXXTRLI, DXXHEFAT, 
                                              DXDHEPF,DXXLAFAT, DXDLAPF, DXXLLFAT, DXDLLPF, 
                                              DXXRAFAT, DXDRAPF, DXXRLFAT, DXDRLPF, DXDLALE, 
                                              DXDLLLE, DXDRALE, DXDRLLE)

#DXA.2002<-nhanesDXA(2001)%>%dplyr::select(SEQN, DXDTOPF, DXDTOFAT, DXDTOLE, DXDTOLI,
#                                           DXXTRFAT, DXDTRPF, DXDTRLE, DXXTRLI, DXXHEFAT, 
#                                           DXDHEPF,DXXLAFAT, DXDLAPF, DXXLLFAT, DXDLLPF, 
#                                           DXXRAFAT, DXDRAPF, DXXRLFAT, DXDRLPF, DXDLALE, 
#                                           DXDLLLE, DXDRALE, DXDRLLE)
DXA.2002<-read_xpt("dxx_b.xpt")%>%dplyr::select(SEQN, DXDTOPF, DXDTOFAT, DXDTOLE, DXDTOLI,
                                                DXXTRFAT, DXDTRPF, DXDTRLE, DXXTRLI, DXXHEFAT, 
                                                DXDHEPF,DXXLAFAT, DXDLAPF, DXXLLFAT, DXDLLPF, 
                                                DXXRAFAT, DXDRAPF, DXXRLFAT, DXDRLPF, DXDLALE, 
                                                DXDLLLE, DXDRALE, DXDRLLE)

#DXA.2004<-nhanesDXA(2003)%>%dplyr::select(SEQN, DXDTOPF, DXDTOFAT, DXDTOLE, DXDTOLI,
#                                          DXXTRFAT, DXDTRPF, DXDTRLE, DXXTRLI, DXXHEFAT, 
#                                          DXDHEPF,DXXLAFAT, DXDLAPF, DXXLLFAT, DXDLLPF, 
#                                          DXXRAFAT, DXDRAPF, DXXRLFAT, DXDRLPF, DXDLALE, 
#                                         DXDLLLE, DXDRALE, DXDRLLE)

DXA.2004<-read_xpt("dxx_c.xpt")%>%dplyr::select(SEQN, DXDTOPF, DXDTOFAT, DXDTOLE, DXDTOLI,
                                                DXXTRFAT, DXDTRPF, DXDTRLE, DXXTRLI, DXXHEFAT, 
                                                DXDHEPF,DXXLAFAT, DXDLAPF, DXXLLFAT, DXDLLPF, 
                                                DXXRAFAT, DXDRAPF, DXXRLFAT, DXDRLPF, DXDLALE, 
                                                DXDLLLE, DXDRALE, DXDRLLE)

####DEMOGRAFICOS####

DEMO.2000<-nhanes("DEMO")%>%dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)
#DEMO.2000<-read_xpt("DEMO.xpt")%>%dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)

DEMO.2002<-nhanes("DEMO_B")%>%dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)
#DEMO.2002<-read_xpt("DEMO_B.xpt")%>%dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)

DEMO.2004<-nhanes("DEMO_C")%>%dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)
#DEMO.2004<-read_xpt("DEMO_C.xpt")%>%dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)

#### Dataset compilation ####

column_names<-c("SEQN" ,"RIAGENDR", "RIDAGEYR", "RIDRETH1", "BMXWT", "BMXHT", "BMXWAIST", 
                "BPXPLS", "BPXDAVG", "BPXSAVG", "BPXAVG", "CVDESVO2", "CVDFITLV", 
                "LEXBRPM", "LEXLABPI", "LEXRABPI", "LBXHCT", "LBXHGB", "LBXHCY", "LBXCRP", 
                "LBXTC", "LBDHDL", "LBXSUA", "LBXSCLSI", "LBXSNASI", "LBXSKSI", "LBXGH", 
                "LBDLDL", "LBXTR", "LBXGLU", "LBXIN", "BPQ020", "BPQ040A", "MCQ160B", "MCQ160C", 
                "MCQ160D", "MCQ160E", "MCQ160F","MCQ160K", "MCQ220", "MCQ010", "MCQ030","DIQ010", 
                "DID040G", "DIQ050", "DIQ070", "SMQ040")
#"DXDTOPF","DXDTOFAT", "DXDTOLE", "DXDTOLI", "DXXTRFAT", "DXDTRPF", 
#"DXDTRLE", "DXXTRLI", "DXXHEFAT", "DXDHEPF", "DXXLAFAT", "DXDLAPF", "DXXLLFAT", 
#"DXDLLPF", "DXXRAFAT", "DXDRAPF", "DXXRLFAT", "DXDRLPF", "DXDLALE", "DXDLLLE", 
#"DXDRALE", "DXDRLLE"
dn.2000<-list(DEMO.2000, antros.2000, labs.2000, quests.2000)
nhanes.2000<-Reduce(function(x,y) merge(x,y,by="SEQN", all=TRUE), dn.2000)
colnames(nhanes.2000)<-column_names

dn.2002<-list(DEMO.2002, antros.2002, labs.2002, quests.2002)
nhanes.2002<-Reduce(function(x,y) merge(x,y,by="SEQN", all=TRUE), dn.2002)
colnames(nhanes.2002)<-column_names

dn.2004<-list(DEMO.2004, antros.2004, labs.2004, quests.2004)
nhanes.2004<-Reduce(function(x,y) merge(x,y,by="SEQN", all=TRUE), dn.2004)
colnames(nhanes.2004)<-column_names

d.nhanes1<-rbind(nhanes.2000, nhanes.2002, nhanes.2004)

vo2max<-d.nhanes1[!is.na(d.nhanes1$CVDESVO2),]

DXA.2000$.imp<-rep(seq(1,5,1), nrow(DXA.2000)/5)
DXA.2002$.imp<-rep(seq(1,5,1), nrow(DXA.2002)/5)
DXA.2004$.imp<-rep(seq(1,5,1), nrow(DXA.2004)/5)

dxa<-rbind(DXA.2000, DXA.2002, DXA.2004)
dxa<-dxa[!is.na(dxa$DXDTOPF),]

#### Dataset management and multiple imputation####

vo2max[, c(1:45)]<-sapply(vo2max[, c(1:45)], as.numeric)

vo2max$RIAGENDR<-categorize(vo2max$RIAGENDR)
vo2max$BPQ020<-categorize(vo2max$BPQ020)
vo2max$BPQ040A<-categorize(vo2max$BPQ040A)
vo2max$MCQ160B<-categorize(vo2max$MCQ160B)
vo2max$MCQ160C<-categorize(vo2max$MCQ160C)
vo2max$MCQ160D<-categorize(vo2max$MCQ160D)
vo2max$MCQ160E<-categorize(vo2max$MCQ160E)
vo2max$MCQ160F<-categorize(vo2max$MCQ160F)
vo2max$MCQ160K<-categorize(vo2max$MCQ160K)
vo2max$MCQ220<-categorize(vo2max$MCQ220)
vo2max$MCQ010<-categorize(vo2max$MCQ010)
vo2max$MCQ030<-categorize(vo2max$MCQ030)
vo2max$DIQ010<-categorize(vo2max$DIQ010)
vo2max$DIQ050<-categorize(vo2max$DIQ050)
vo2max$DIQ070<-categorize(vo2max$DIQ070)
vo2max$DID040G<-categorize(vo2max$DID040G)

vo2max$BMI<-(vo2max$BMXWT/((vo2max$BMXHT)/100)^2)
vo2max$METSIR<-(log(2*vo2max$LBXGLU+vo2max$LBXTR)*vo2max$BMI)/log(vo2max$LBDHDL)
vo2max$WHR<-(vo2max$BMXWAIST)/(vo2max$BMXHT)
vo2max$o_WHR<-orderNorm(vo2max$WHR)$x.t
vo2max$METSVF<-(4.466+0.011*(log(vo2max$METSIR)^3)+3.239*(log(vo2max$WHR)^3)+(0.319*vo2max$RIAGENDR)
                +0.594*(log(vo2max$RIDAGEYR)))
vo2max$METSVF.C<-categ(vo2max$METSVF, 7.18)
vo2max$c_WHR<-categ(vo2max$WHR, 0.55)

##TransformaciÃ³n de variables

vo2max$t_VO2<-orderNorm(vo2max$CVDESVO2)$x.t
vo2max$MAXHR<-(208-(0.7*vo2max$RIDAGEYR))
vo2max$C.VO2<-orderNorm((vo2max$MAXHR*15.3)/vo2max$BPXPLS)$x.t
vo2max$o_METSVF<-orderNorm(vo2max$METSVF)$x.t

vo2max$R.FACTOR1<-(vo2max$BPQ020+vo2max$BPQ040A+vo2max$MCQ160B+vo2max$MCQ160C+vo2max$MCQ160D+vo2max$MCQ160E+
                     vo2max$MCQ160F+vo2max$MCQ010+vo2max$DIQ010+vo2max$MCQ160K)

dxa$o_P.TFAT<-orderNorm(dxa$DXDTOPF)$x.t
dxa$o_P.VFAT<-orderNorm(dxa$DXDTRPF)$x.t
dxa$o_P.SFAT<-orderNorm((dxa$DXDHEPF+dxa$DXDLAPF+dxa$DXDRAPF+dxa$DXDTRPF)/4)$x.t
dxa$o_P.IFAT<-orderNorm((dxa$DXDLLPF+dxa$DXDRLPF)/2)$x.t

dxa$c_TFAT<-categ(dxa$DXDTOPF, 30)
dxa$c_VFAT<-categ(dxa$DXDTRPF, 30)
dxa$c_SFAT<-categ(((dxa$DXDHEPF+dxa$DXDLAPF+dxa$DXDRAPF+dxa$DXDTRPF)/4), 30)
dxa$c_IFAT<-categ((dxa$DXDLLPF+dxa$DXDRLPF)/2, 30)

dxa$bcomp<-dxa$c_VFAT+2*dxa$c_SFAT+4*dxa$c_IFAT

#0: Lean
#1: Trunk obese
#2: Upper body obese
#3: Trunk and upper body obese
#4: Lower body obese
#5: Trunk and lower body obese
#6: Upper and lower body obese
#7: All obese
#0=0
#1=1, 2, 3
#2=4, 5
#3=6, 7

category<-c()
for(value in dxa$bcomp){
  if(value==0){
    cat=0
  }else{
    if (value==1||value==2||value==3){
      cat=1
    }else{
      if (value==4||value==5){
        cat=2
      }else{
        cat=3
      }
    }
  }
  category<-c(category,cat)
}

dxa$bcomp1<-category

dxa$AP.LEAN1<-(dxa$DXDLALE+dxa$DXDLLLE+dxa$DXDRALE+dxa$DXDRLLE)/1000

antropodxa<-vo2max%>%dplyr::select(SEQN, RIAGENDR, BMXWAIST, BMXWT, BMXHT, METSVF)

md.pattern(antropodxa)
imp<-mice(antropodxa, m=5, maxit=5, seed = 123)
antropodxa<-complete(imp, "long")
impantropodxa<-as.data.frame(imputationList(antropodxa)$imputations)
impantropodxa$ident<-paste(impantropodxa$.imp,impantropodxa$SEQN)

dxa2<-as.data.frame(imputationList(dxa)$imputations)

dxa2$ident<-paste(dxa2$.imp, dxa2$SEQN)

dxaesp<-merge(impantropodxa, dxa2, by="ident")

for (value in dxaesp$RIAGENDR){
  if (value==1){
    L.MASS1<-(0.47*dxaesp$BMXWAIST)+(0.03*dxaesp$BMXHT)+(0.012*dxaesp$AP.LEAN1)-(0.001*(dxaesp$AP.LEAN1^2))+(0.29*dxaesp$BMXWAIST)+13.5
  }else{
    if (value==0){
      L.MASS1<-(0.25*dxaesp$BMXWT)+(0.09*dxaesp$BMXHT)+(0.111*dxaesp$AP.LEAN1)-(0.0005*(dxaesp$AP.LEAN1^2))+(0.06*dxaesp$BMXWAIST)-4.5
    }
  }  
}

dxaesp$L.MASS<-L.MASS1
dxaesp$P.LMASS<-(L.MASS1/dxaesp$BMXWT)*100
dxaesp$o_L.MASS<-orderNorm(dxaesp$P.LMASS)$x.t

dxaesp$P.METSVF<-(exp(dxaesp$METSVF)*(dxaesp$DXDTOPF)/dxaesp$DXDTOFAT)
dxaesp$o_P.METSVF<-orderNorm(dxaesp$P.METSVF)$x.t

dxa<-dxaesp%>%dplyr::select(SEQN.y, .imp.y, DXDTOPF,DXDTOFAT, DXDTOLE, DXDTOLI, DXXTRFAT, DXDTRPF, DXDTRLE, 
                            DXXTRLI, DXXHEFAT, DXDHEPF, DXXLAFAT, DXDLAPF, DXXLLFAT, DXDLLPF, DXXRAFAT, DXDRAPF,
                            DXXRLFAT, DXDRLPF, DXDLALE, DXDLLLE, DXDRALE, DXDRLLE, .imp.y, o_P.TFAT, o_P.SFAT, 
                            o_P.IFAT, o_P.VFAT, L.MASS, P.LMASS, o_L.MASS, P.METSVF, o_P.METSVF, c_TFAT, c_VFAT, c_SFAT, 
                            c_IFAT, bcomp, bcomp1, AP.LEAN1)%>%rename(SEQN=SEQN.y, .imp=.imp.y)

aggr_plot<-aggr(dxa, col=c('navyblue','red'), 
                numbers=TRUE, sortVars=TRUE, labels=names(dxa), 
                cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
print(aggr_plot)

#========================================================#
md.pattern(vo2max)
aggr_plot<-aggr(vo2max, col=c('navyblue','red'), 
                numbers=TRUE, sortVars=TRUE, labels=names(vo2max), 
                cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
print(aggr_plot)
set.seed(123)
imp<-mice(vo2max, m=5, maxit=5)
data1<-complete(imp, "long", include=F)
impdata<-imputationList(data1)
impdata<-as.data.frame(impdata$imputations)
impdata$ident<-paste(impdata$.imp,impdata$SEQN)

dxa1<-as.data.frame(imputationList(dxa)$imputations)

dxa1$ident<-paste(dxa1$.imp, dxa1$SEQN)

data2<-merge(impdata, dxa1, by="ident")%>%dplyr::select(!c(SEQN.y, .imp.y))%>%rename(SEQN=SEQN.x, .imp=.imp.x)

aggr_plot<-aggr(data2, col=c('navyblue','red'), 
                numbers=TRUE, sortVars=TRUE, labels=names(data2), 
                cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
print(aggr_plot)

dxaimp<-dxa1%>%filter(.imp==1)%>%dplyr::select(!c(.imp))
datana<-merge(dxaimp, vo2max, by="SEQN")
datana$.imp<-rep(0,nrow(datana))

data4<-rbind(data2[,names(datana)], datana)

data4<-data4%>%filter(.imp==1)

data<-data4%>%filter(RIDAGEYR>=18&.imp==1)

data$fat_mass_index<-(data$DXDTOFAT/1000)/((data$BMXHT/100)^2)
data$trunk_fat<-((data$DXXTRFAT)/1000)/((data$BMXHT/100)^2)
data$upper_fat<-((data$DXXHEFAT+data$DXXLAFAT+data$DXXRAFAT+data$DXXTRFAT)/1000)/((data$BMXHT/100)^2)
data$lower_fat<-((data$DXXLLFAT+data$DXXRLFAT)/1000)/((data$BMXHT/100)^2)
data$ob_subtypes<-data$ob_upper+2*data$ob_lower
data$METSVF_exp<-exp(data$METSVF)
data$trunk_fat_i<-(data$DXXTRFAT)/(data$DXDTRLE)
data$visceral_fat_index<-(data$METSVF_exp/1000)/((data$BMXHT/100)^2)
data$trunkleg<-data$DXDTRPF/(data$DXDLLPF+data$DXDRLPF)
data$trunkextremities<-data$DXXTRFAT/(data$DXXRAFAT+data$DXXRLFAT+data$DXXLLFAT+data$DXXLAFAT)
data$updown<-(data$DXDTRPF+data$DXDLAPF+data$DXDRAPF)/(data$DXDLLPF+data$DXDRLPF)
data$ICE<-data$BMXWAIST/data$BMXHT
data<-subset(data, data$trunkleg<3&data$trunkextremities<3&data$updown<3&data$CVDESVO2<70)
data$stat_age<-as.factor(cut(data$RIDAGEYR, breaks=c(17,25,35,45,55), labels=c("1","2","3","4")))
data<-data%>%filter(R.FACTOR1<1)

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

write.csv(data, "cvfitness.csv")

