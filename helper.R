#This script contains all the helper functions used in README.Rmd.
#Bhan, Lam (2022) [Nanyang Technological University]
#ORCID: 0000-0001-5193-6560
#GitHub: https://github.com/bhanlam/

#compute CLAR, ORTH, NCON, IBAL for main axes
mainForm <- function(x) {
        x %>% 
                mutate(APPR=APPR/10) %>%
                mutate(UNDR=UNDR/10) %>%
                mutate(ANTO=ANTO/10) %>%
                mutate(CLAR=1-0.5*ASSOCW/10-0.5*ASSOCCW/10) %>%
                mutate(ORTH=1-2*abs(BIAS/10-0.5)) %>%
                mutate(NCON=1-0.5*(IMPCW/10+IMPCCW/10)) %>%
                mutate(IBAL=1-abs(IMPCCW/10-IMPCW/10))
}

#compute CLAR, CONN, IBAL for derived axes
derForm <- function(x) {
        x %>% 
                mutate(APPR=APPR/10) %>%
                mutate(UNDR=UNDR/10) %>%
                mutate(CLAR=1-0.5*ASSOCW/10-0.5*ASSOCCW/10) %>%
                mutate(CONN=0.5*(IMPCW/10+IMPCCW/10)) %>%
                mutate(IBAL=1-abs(IMPCCW/10-IMPCW/10))
}

mainAxSummary <- function(x) {
        x %>% 
                group_by(CANDIDATE) %>%       
                dplyr::summarize(APPR=mean(APPR),UNDR=mean(UNDR),
                                 CLAR=mean(CLAR),ANTO=mean(ANTO),
                                 ORTH=mean(ORTH),NCON=mean(NCON),
                                 IBAL=mean(IBAL))
}

mainAxSummarySPLIT <- function(x) {
        x %>% 
        group_by(COUNTRY,CANDIDATE) %>%       
        dplyr::summarize(APPR=mean(APPR),UNDR=mean(UNDR),
                         CLAR=mean(CLAR),ANTO=mean(ANTO),
                         ORTH=mean(ORTH),NCON=mean(NCON),
                         IBAL=mean(IBAL))
}

derAxSummary <- function(x) {
        x %>% 
                group_by(CANDIDATE) %>%       
                dplyr::summarize(APPR=mean(APPR),UNDR=mean(UNDR),
                                 CLAR=mean(CLAR),CONN=mean(CONN),
                                 IBAL=mean(IBAL))
}

derAxSummarySPLIT <- function(x) {
        x %>% 
                group_by(COUNTRY,CANDIDATE) %>%       
                dplyr::summarize(APPR=mean(APPR),UNDR=mean(UNDR),
                                 CLAR=mean(CLAR),CONN=mean(CONN),
                                 IBAL=mean(IBAL))
}

#Kruskal Wallis Test
kwTest<-function(df,type){
        mainCrit<-c("APPR","UNDR","CLAR","IBAL","ANTO","ORTH","NCON")
        derCrit<-c("APPR","UNDR","CLAR","IBAL","CONN")
        
        data<-data.frame(CRITERION=character(),pvalue=numeric(),effect=numeric())
        
        if(type=="main"){
                for(crit in mainCrit){
                        kwt<-kruskal.test(x=df[,crit],g=as.factor(df$COUNTRY),data=df)
                        kwteff<-kruskal_effsize(formula = as.formula(paste(crit,"~COUNTRY"))
                                                ,data=df)
                        data<-rbind(data,c(CRITERION=crit,pvalue=kwt$p.value,effect=kwteff$effsize))
                }}
        if(type=="derived"){
                for(crit in derCrit){
                        kwt<-kruskal.test(x=df[,crit],g=as.factor(df$COUNTRY),data=df)
                        kwteff<-kruskal_effsize(formula = as.formula(paste(crit,"~COUNTRY"))
                                                ,data=df)
                        data<-rbind(data,c(CRITERION=crit,pvalue=kwt$p.value,effect=kwteff$effsize))
                }}
        
        return(data %>% setNames(c("CRITERION","pvalue","effect")))
}

#Prentice Test
prenticeTest<-function(df,type){
        mainCrit<-c("APPR","UNDR","CLAR","IBAL","ANTO","ORTH","NCON")
        derCrit<-c("APPR","UNDR","CLAR","IBAL","CONN")
        
        data<-data.frame(CRITERION=character(),pvalue=numeric())
        
        if(type=="main"){
                for(crit in mainCrit){
                        pt<-prentice.test(df[,crit],as.factor(df$COUNTRY),as.factor(df$CANDIDATE))
                        data<-rbind(data,c(CRITERION=crit,pvalue=pt$p.value))
                }}
        if(type=="derived"){
                for(crit in derCrit){
                        pt<-prentice.test(df[,crit],as.factor(df$COUNTRY),as.factor(df$CANDIDATE))
                        data<-rbind(data,c(CRITERION=crit,pvalue=pt$p.value))
                }}
        
        return(data %>% setNames(c("CRITERION","pvalue")))
}

#Mann-Whitney-Wilcoxon Test
mwwTest <- function(df,criterion,PAQ){
        #df is the data
        #criterion is a vector of criterion for which MWWT is conducted
        #PAQ attribute under test
        
        #initialise dataframe
        wt<-data.frame(PAQ=character(),
                       CRITERION=character(),
                       CANDIDATE=character(),
                       pvalue=numeric(),
                       adjpval=numeric())
        
        for (c in criterion){
                for (cand in unique(df$CANDIDATE)){
                        data<-df %>% filter(CANDIDATE==cand) %>% 
                                mutate(COUNTRY=as.factor(COUNTRY))
                        wt_cand<-wilcox.test(as.formula(data[,c] ~ data$COUNTRY))
                        wt<-rbind(wt,data.frame(PAQ=PAQ,
                                                CRITERION=c,
                                                CANDIDATE=cand,
                                                pvalue=wt_cand$p.value,
                                                adjval=p.adjust(wt_cand$p.value,
                                                                method = "bonferroni",
                                                                n=2)))
                }
        }
        return(wt %>% setNames(c("PAQ","CRITERION","CANDIDATE","pvalue","adjval")))
}