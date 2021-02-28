library(tidyverse)
library(plotrix)
library(lme4)
library(brms)
library(plotly)
library(ggthemes)

setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/MOT_DSP/")

options(contrasts = rep("contr.sdif",2))

data = read_csv(file = "data/clean/all_data_clean.csv") %>%
    mutate(val = factor(val,levels=c("zero","low","med","high")),
           grp = factor(group,levels=c("non","rew","pen")),
           tim = factor(time, levels=c("early","mid","late","test")),
           blk = ifelse(sess=="reward",block,block+9))
data_long = pivot_longer(data,
                  cols = ends_with("_IKI"),
                  names_to = "key",
                  values_to = "IKI") %>%
    mutate(key = ifelse(key=="one_IKI",1,
                        ifelse(key=="two_IKI",2,
                               ifelse(key=="three_IKI",3,
                                      ifelse(key=="four_IKI",4,
                                             ifelse(key=="five_IKI",5,
                                                    ifelse(key=="six_IKI",6,
                                                           ifelse(key=="seven_IKI",7,8))))))))

plt_data = data_long %>% 
    filter(kerr==FALSE & val !="med" & tim %in% c("late","test")) %>%
    mutate(IKI_mu = mean(IKI,na.rm=T)) %>%
    group_by(id) %>% 
    mutate(IKI_id_mu = mean(IKI,na.rm=T)) %>% ungroup() %>%
    group_by(id,grp,key,seq_keys,tim) %>% 
    summarise(IKI = mean(IKI,na.rm=T), 
              IKI_id_mu = mean(IKI_id_mu,na.rm=T),
              IKI_mu=mean(IKI_mu,na.rm=T)) %>% 
    mutate(IKI_new = IKI - IKI_id_mu + IKI_mu) %>%
    group_by(grp,key,seq_keys,tim) %>% 
    summarise(IKI = mean(IKI,na.rm=T),
              se=std.error(IKI_new,na.rm=T)) 
plt_data %>% ggplot(aes(x=key,y=IKI,ymax=IKI+se,ymin=IKI-se,colour=grp,linetype=tim)) +
    geom_point(position=position_dodge(.1),size=1) +
    geom_line(position=position_dodge(.1)) +
    geom_errorbar(position=position_dodge(.1),width=0) +
    scale_colour_colorblind() +
    facet_wrap(seq_keys~grp,ncol=3,nrow=4)
ggsave("visuals/figures/IKI_ret.pdf",units="in",dpi=150,height=6,width=6)


plt_data = data_long %>% 
    filter(kerr==FALSE & val !="med" & tim %in% c("early","late")) %>%
    mutate(IKI_mu = mean(IKI,na.rm=T)) %>%
    group_by(id) %>% 
    mutate(IKI_id_mu = mean(IKI,na.rm=T)) %>% ungroup() %>%
    group_by(id,grp,key,seq_keys,tim) %>% 
    summarise(IKI = mean(IKI,na.rm=T), 
              IKI_id_mu = mean(IKI_id_mu,na.rm=T),
              IKI_mu=mean(IKI_mu,na.rm=T)) %>% 
    mutate(IKI_new = IKI - IKI_id_mu + IKI_mu) %>%
    group_by(grp,key,seq_keys,tim) %>% 
    summarise(IKI = mean(IKI,na.rm=T),
              se=std.error(IKI_new,na.rm=T)) 
plt_data %>% ggplot(aes(x=key,y=IKI,ymax=IKI+se,ymin=IKI-se,colour=grp,linetype=tim)) +
    geom_point(position=position_dodge(.1),size=1) +
    geom_line(position=position_dodge(.1)) +
    geom_errorbar(position=position_dodge(.1),width=0) +
    scale_colour_colorblind() +
    facet_wrap(seq_keys~grp,ncol=3,nrow=4)
ggsave("visuals/figures/IKI_acq.pdf",units="in",dpi=150,height=6,width=6)




a#