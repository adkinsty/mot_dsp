library(tidyverse)
library(ggthemes)
library(bayesplot)

# ******************************** Data ***********************************
setwd("/Users/adkinsty/Box/LeeLab/Experiments/Exp_files/mot_dsp/")

dat <- read_csv(file = "data/clean/all_data_clean.csv") %>%
  mutate(size = ifelse(group == "non","zero",size),
         value = factor(size,c("low","med","high","zero")),
         group = factor(group,levels = c("rew","pen","non")),
         kerr = ifelse(kerr == TRUE, 1, 0),
         v = ifelse(size == "low", -.5, ifelse(size == "med", 0, .5)),
         g = ifelse(group=="pen",-.5,ifelse(group=="rew",.5,0)))

dat1 <- filter(dat,sess == "reward")
dat1.1 <- filter(dat1,kerr == 0) 
dat2 <- filter(dat,sess == "test")
dat2.1 <- filter(dat2,kerr == 0)


# ******************************** Plot parameters ***********************************

kps_title <- "Movement Speed (kps)"; rt_title <- "Initiation Time (ms)"; acc_title <- "Keypress Accuracy (%)"
ret_title <- "Skill decay (%)"; sess_title <- "Session"; sess_lab <- c("Training", "Test")
blk_title <- "Training Block"; val_title <- "Value ($)"; grp_title <- "Valence (-/+)"
blk_lab <- c(1,"","","","","","","",9); val_lab <- c("5","10","30","Control"); grp_lab <- c("Reward","Punishment","Control")
pos <- position_dodge(.25)
ebar_width <- .25
dot_size <- .5

# Colors:      Black      Orange     Skyblue   BluGreen    Yellow      Blue    Vermillion  RedPurple
wong_pal <- c("#000000", "#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00", "#cc79a7")
val_pal <- wong_pal[c(7,5,4,1)]
grp_pal <- wong_pal[c(6,2,1)]

col_plot <- function(x) {
  x + 
  geom_col(position=pos,width=.6,alpha=.4) +
  geom_errorbar(position = pos, width = ebar_width) +
  geom_point(position=pos,shape=21,fill="white") +
  scale_colour_manual(val_title, labels = val_lab, values = val_pal) +
  scale_fill_manual(val_title, labels = val_lab, values = val_pal) +
  scale_x_discrete(grp_title, labels = grp_lab)
}
dot_plot <- function(x) {
  x + 
  geom_line(position=pos) +
  geom_errorbar(position = pos, width = ebar_width) +
  geom_errorbar(aes(ymin=mu-seb,ymax=mu+seb),width=0,alpha=.5,position=pos) +
  geom_point(position=pos,shape=21,fill="white") +
  scale_colour_manual(grp_title, labels = grp_lab, values = grp_pal) +
  scale_x_discrete(val_title, labels = val_lab)
}
lin_plot <- function(x) {
  pos <- position_dodge(.5)
  x + 
  geom_errorbar(position=pos,width=0) +
  geom_line(position=pos) +
  geom_point(position=pos,shape=21,fill="white",size=dot_size) +
  facet_wrap(group ~ .,labeller = labeller(group = c("rew"="Reward","pen"="Punishment","non"="Control"))) +
  scale_colour_manual(val_title, labels = val_lab, values = val_pal) +
  scale_fill_manual(val_title, labels = val_lab, values = val_pal) +
  scale_x_continuous(blk_title, breaks = 1:9, labels=blk_lab)
}
lin_plot2 <- function(x) {
  pos <- position_dodge(.5)
  x + 
  geom_line(position=pos) +
  geom_errorbar(position = pos, width = ebar_width) +
  geom_point(position=pos,shape=21,fill="white") +
  facet_wrap(group ~ .,labeller = labeller(group = c("rew"="Reward","pen"="Punishment","non"="Control"))) +
  scale_colour_manual(val_title, labels = val_lab, values = val_pal) +
  scale_fill_manual(val_title, labels = val_lab, values = val_pal) +
  scale_x_discrete(sess_title, labels=sess_lab)
}

ax_size <- .4; ax_color <- "grey"
my_theme <- theme_tufte(base_size = 10, base_family = "sans") + 
  theme(legend.position = "none", 
        axis.line.x = element_line(size = ax_size,colour=ax_color),
        panel.grid.major.y = element_line(linetype="dotted",size=ax_size,colour=ax_color),
        axis.ticks = element_line(size=ax_size,colour=ax_color))



# ******************************** Train rt ***********************************
dat1.1 %>%
  group_by(id,group,value) %>% summarise(rt = mean(rt)) %>% 
  ungroup %>% mutate(gmu = mean(rt)) %>% 
  group_by(id) %>% mutate(smu = mean(rt)) %>% 
  ungroup() %>% mutate(rt_ = rt - smu + gmu) %>% 
  group_by(group,value) %>% summarise(mu = mean(rt), se = sd(rt_)/sqrt(n()), seb = sd(rt)/sqrt(n())) %>% 
  ggplot(aes(x=value, y=mu, ymax=mu+se, ymin=mu-se, colour=group, group=group)) %>%
  dot_plot() +
  scale_y_continuous(rt_title) +
  my_theme
ggsave("visuals/figures/train_rt.pdf",height=2.5,width=2.5)


dat1.1 %>%
  group_by(id,group,value,block) %>%
  summarise(rt = mean(rt)) %>% 
  ungroup() %>% mutate(gmu = mean(rt)) %>% 
  group_by(id) %>% mutate(smu = mean(rt)) %>% 
  ungroup() %>% mutate(rt_ = rt - smu + gmu) %>% 
  group_by(group,value,block) %>% summarise(rt = mean(rt), se = sd(rt_)/sqrt(n())) %>% 
  ggplot(aes(x=block, y=rt, ymax=rt+se, ymin=rt-se, colour=value, group=value, fill=value)) %>%
  lin_plot() +
  scale_y_continuous(rt_title) +
  my_theme
ggsave("visuals/figures/train_block_rt.pdf",height=2,width=5)

bm_rt <- read_rds("modeling/rds_files/bm_rt.rds")
pars <- rev(c("b_v","b_g","b_v:g","b_bL:v","b_bL:g","b_bL:v:g"))
mcmc_areas(bm_rt,pars=pars) +
  scale_x_continuous("Parameter Estimate") +
  my_theme + 
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave("visuals/figures/train_par_rt.pdf",height=2.5,width=2)



# ******************************** Train kps ***********************************

dat1.1 %>%
  mutate(kps = 7/(mt/1000)) %>%
  group_by(id,group,value) %>% summarise(kps = mean(kps)) %>% 
  ungroup %>% mutate(gmu = mean(kps)) %>% 
  group_by(id) %>% mutate(smu = mean(kps)) %>% 
  ungroup() %>% mutate(kps_ = kps - smu + gmu) %>% 
  group_by(group,value) %>% summarise(mu = mean(kps), se = sd(kps_)/sqrt(n()), seb = sd(kps) / sqrt(n())) %>% 
  ggplot(aes(x=value, y=mu, ymax=mu+se, ymin=mu-se, colour=group, group=group)) %>%
  dot_plot() +
  scale_y_continuous(kps_title) +
  my_theme
ggsave("visuals/figures/train_kps.pdf",height=2.5,width=2.5)

dat1.1 %>%
  mutate(kps = 7/(mt/1000)) %>%
  group_by(id,group,value,block) %>% summarise(kps = mean(kps)) %>% 
  ungroup %>% mutate(gmu = mean(kps)) %>% 
  group_by(id) %>% mutate(smu = mean(kps)) %>% ungroup() %>%
  mutate(kps_ = kps - smu + gmu) %>% 
  group_by(group,value,block) %>% summarise(kps = mean(kps), se = sd(kps_)/sqrt(n())) %>% 
  ggplot(aes(x=block, y=kps, ymax=kps+se, ymin=kps-se, colour=value, group=value, fill=value))  %>%
  lin_plot() +
  scale_y_continuous(kps_title,breaks=0:7) +
  my_theme
ggsave("visuals/figures/train_block_kps.pdf",height=2,width=5)

bm_kps <- read_rds("modeling/rds_files/bm_kps.rds")
pars <- rev(c("b_v","b_g","b_v:g","b_bL:v","b_bL:g","b_bL:v:g"))
mcmc_areas(bm_kps,pars=pars) +
  scale_x_continuous("Parameter Estimate") +
  my_theme + 
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave("visuals/figures/train_par_kps.pdf",height=2.5,width=2)



# ******************************** Train acc ***********************************

dat1 %>%
  mutate(acc = ifelse(kerr==0,1,0)) %>% 
  group_by(id,group,value) %>% summarise(acc = mean(acc)*100) %>% 
  ungroup() %>% mutate(gmu = mean(acc)) %>% 
  group_by(id) %>% mutate(smu = mean(acc)) %>% 
  ungroup() %>% mutate(acc_ = acc - smu + gmu) %>% 
  group_by(group,value) %>% summarise(mu = mean(acc), se = sd(acc_)/sqrt(n()), seb = sd(acc) / sqrt(n())) %>% 
  ggplot(aes(x=value, y=mu, ymax=mu+se, ymin=mu-se, colour=group, group=group)) %>%
  dot_plot() +
  scale_y_continuous(acc_title) +
  my_theme
ggsave("visuals/figures/train_acc.pdf",height=2.5,width=2.5)

dat1 %>%
  mutate(acc = ifelse(kerr==0,1,0)) %>% 
  group_by(id,group,value,block) %>% summarise(acc = mean(acc)*100) %>% 
  ungroup() %>% mutate(gmu = mean(acc)) %>% 
  group_by(id) %>% mutate(smu = mean(acc)) %>% 
  ungroup() %>% mutate(acc_ = acc - smu + gmu) %>% 
  group_by(group,value,block) %>% summarise(se = sd(acc)/sqrt(n()), acc = mean(acc), se_ = sd(acc_)/sqrt(n())) %>% 
  ggplot(aes(x=block, y=acc, ymax=acc+se_, ymin=acc-se_, colour=value, group=value, fill=value))  %>%
  lin_plot() +
  scale_y_continuous(acc_title,breaks=seq(0,80,30)) +
  my_theme
ggsave("visuals/figures/train_block_acc.pdf",height=2,width=5)

bm_acc <- read_rds("modeling/rds_files/bm_acc.rds")
pars <- rev(c("b_v","b_g","b_v:g","b_bL:v","b_bL:g","b_bL:v:g"))
mcmc_areas(bm_acc,pars=pars) +
  scale_x_continuous("Parameter Estimate") +
  my_theme + 
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave("visuals/figures/train_par_acc.pdf",height=2.5,width=2)



# ******************************** Test plots ***********************************

dat2.1 %>%
  filter(sequence != "random") %>%
  group_by(id,group,value) %>% summarise(rt = mean(rt)) %>% 
  ungroup %>% mutate(gmu = mean(rt)) %>% 
  group_by(id) %>% mutate(smu = mean(rt)) %>% 
  ungroup() %>% mutate(rt_ = rt - smu + gmu) %>% 
  group_by(group,value) %>% summarise(mu = mean(rt), se = sd(rt_)/sqrt(n()), seb = sd(rt)/sqrt(n())) %>% 
  ggplot(aes(x=value, y=mu, ymax=mu+se, ymin=mu-se, colour=group, group=group)) %>%
  dot_plot() +
  scale_y_continuous(rt_title) +
  my_theme
ggsave("visuals/figures/test_rt.pdf",height=2,width=2)

dat2.1 %>%
  filter(sequence != "random") %>%
  mutate(kps = 7/(mt/1000)) %>%
  group_by(id,group,value) %>% summarise(kps = mean(kps)) %>% 
  ungroup %>% mutate(gmu = mean(kps)) %>% 
  group_by(id) %>% mutate(smu = mean(kps)) %>% 
  ungroup() %>% mutate(kps_ = kps - smu + gmu) %>% 
  group_by(group,value) %>% summarise(mu = mean(kps), se = sd(kps_)/sqrt(n()), seb = sd(kps)/sqrt(n())) %>% 
  ggplot(aes(x=value, y=mu, ymax=mu+se, ymin=mu-se, colour=group, group=group)) %>%
  dot_plot() +
  scale_y_continuous(kps_title) +
  my_theme
ggsave("visuals/figures/test_kps.pdf",height=2,width=2)

dat2 %>%
  filter(sequence != "random") %>%
  mutate(acc = ifelse(kerr==0,1,0)) %>% 
  group_by(id,group,value) %>% summarise(acc = mean(acc)*100) %>% 
  ungroup() %>% mutate(gmu = mean(acc)) %>% 
  group_by(id) %>% mutate(smu = mean(acc)) %>% 
  ungroup() %>% mutate(acc_ = acc - smu + gmu) %>% 
  group_by(group,value) %>% summarise(mu = mean(acc), se = sd(acc_)/sqrt(n()), seb = sd(acc)/sqrt(n())) %>% 
  ggplot(aes(x=value, y=mu, ymax=mu+se, ymin=mu-se, colour=group, group=group)) %>%
  dot_plot() +
  scale_y_continuous(acc_title) +
  my_theme
ggsave("visuals/figures/test_acc.pdf",height=2,width=2)


# RETENTION
dat %>% 
  filter(kerr == 0 & sess != "training" & sequence != "random") %>%
  mutate(kps = 7/(mt/1000)) %>%
  mutate(block = ifelse(sess=="test",block+6,block)) %>%
  filter(block %in% 7:9) %>% 
  group_by(group,id,sess,value) %>%
  summarise(kps = mean(kps)) %>%
  ungroup %>% mutate(gmu = mean(kps)) %>% 
  group_by(id) %>% mutate(smu = mean(kps)) %>% ungroup() %>%
  mutate(kps_ = kps - smu + gmu) %>% 
  group_by(group,value,sess) %>% summarise(se = sd(kps)/sqrt(n()), kps = mean(kps), se_ = sd(kps_)/sqrt(n())) %>% 
  ggplot(aes(x=sess, y=kps, ymax=kps+se_, ymin=kps-se_, colour=value, group=value, fill=value))  %>%
  lin_plot2() +
  scale_y_continuous(kps_title,breaks=seq(0,6,.5)) +
  my_theme
ggsave("visuals/figures/ret_mt.pdf",height=2,width=4)

color_scheme_set("brightblue")
bm_ret <- read_rds("modeling/rds_files/bm_ret_mt.rds")
pars <- rev(c("b_g:s","b_v:s","b_v:g:s"))
mcmc_areas(bm_ret,pars=pars) +
  scale_x_continuous("Parameter Estimate") +
  my_theme + 
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave("visuals/figures/train_par_ret.pdf",height=2,width=2)


