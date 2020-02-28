library(tidyverse)
library(lmerTest)

#read data
dat <- read_csv("/Users/carlpearson/Documents/r_github/RHEL-Customization-Tickets/do_not_upload/cust_ranking_dat.csv",col_names = T)
#remove commas from colnames
colnames(dat) <- gsub(",",";", colnames(dat))
colnames(dat) <- gsub("-","", colnames(dat))

df_long <- dat %>% 
  rename(id=`Response ID`) %>%
  pivot_longer(-id) %>%
  mutate(name_s=str_trunc(name,width=18,side="right",ellipsis = ""),
         name_s=gsub(" ","_",name_s)) %>%
  na.omit()

df_long %>%
  mutate(name=as.factor(name)) %>%
  ggplot(aes(x=fct_rev(name),y=value ,fill=name)) +
  geom_violin(alpha=.5,color="white")+
  stat_summary(fun=mean,size=1) +
  ggthemes::theme_tufte(base_family = "sans",base_size = 15) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  ) + 
  scale_y_continuous(breaks = c(1,4,7)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  coord_flip(ylim=c(1,7)) +
  labs(y="Rank Average",
       subtitle = "Point = mean, line = SD, color = response distributions")

ggsave("/Users/carlpearson/Documents/r_github/RHEL-Customization-Ticket/plost/conf_violin.png",width = 12,height = 8)


kruskal.test(value~ name, data = df_long)
pairwise.wilcox.test(df_long$value, df_long$name,
                     p.adjust.method = "BH")

ggboxplot(df_long, x = "name", y = "rank", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")


library(PMCMRplus)
fri.mod <- frdAllPairsExactTest(y=df_long$value,groups= df_long$name_s,blocks=df_long$id,
                 method = "fdr") 

barPlot(fri.mod)
plot(fri.mod)
fri.sum <- capture.output(PMCMRplus::summaryGroup(fri.mod)) #get summary output

#turn fri.sum into structured output
df_fri.sum <- as.matrix(fri.sum) %>%
  gsub("\\s+", " ", .) %>%
  gsub("Sig\\. group","Sig_group",.) %>%
  gsub("median","name_s median",.) %>%
  trimws(.,which="both") %>%
  as.data.frame() %>%
  separate(V1, into = c(paste0("v",1:6)), sep=" ") 

colnames(df_fri.sum) <- df_fri.sum[1,] #get colnames from first row
df_fri.sum <- df_fri.sum[-1,] #remove first row



df_long %>%
  left_join(df_fri.sum) %>%
  ungroup() %>%
  mutate(Sig_group=as.character(Sig_group)) %>%
  arrange(median,Q25) %>%
  ggplot(aes(value, fill=Sig_group)) +
  geom_histogram(binwidth = 1) +
  geom_label(aes(label=name_s,y=3,x=4),fill="lightgray")+
  geom_label(aes(label=Sig_group,y=3,x=6),fill="lightgray")+
  facet_wrap(~name_s,ncol=1) +
  ggthemes::theme_tufte(base_family = "sans",base_size = 15) +
  theme(
    legend.position = "none",
    strip.text.x = element_blank()
  )  +
  scale_x_continuous(breaks = c(1,4,7)) +
  labs(y="Rank Count",x="Rank") 
  
kable(as.matrix(fri.sum$p.value)) %>% kable_styling(bootstrap_options = c("striped", "hover"))

library(rstatix)
library(ggpubr)


#friedman test
res.fried <- df_long %>%
  friedman_test(value ~ name_s | id)
res.fried

#effect size
df_long %>%
  friedman_effsize(value ~ name_s | id)

mod.pairwise <- df_long %>%
  arrange(id) %>% #order needs to be by ID
  rstatix::wilcox_test(value ~ name_s, p.adjust.method = "holm")

mod.pairwise <- mod.pairwise %>% 
  add_xy_position(x = "name_s") 
  ggboxplot(df_long, x = "name_s", y = "value", add = "point") +
    stat_pvalue_manual(mod.pairwise, hide.ns = TRUE) +
    labs(
      subtitle = get_test_label(res.fried,  detailed = TRUE),
      caption = get_pwc_label(mod.pairwise)
    )
  
df_long %>%
ggplot(aes(x = Species, y = Petal.Width)) +
  geom_boxplot() +
  # Use the prepared table of test results as data for the geom
  geom_text(aes(label = Sig, y = MaxWidth + 0.2), size = 6,
            data = t_tests)



