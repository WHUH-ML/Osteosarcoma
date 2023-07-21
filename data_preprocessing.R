library(survival)
library(magrittr)
library(gtsummary)
library(forestplot)
library(data.table)
library(flextable)


#### 1. Read data ####
source('preprocessing.R')
data <- get_data() %>% data.frame(check.names = T) %>% subset(select=-c(Histological.type,Radiotherapy,Chemotherapy))
#### 2.1 Overall distrubution ####
overall <- tbl_summary(data,statistic = list(all_continuous() ~ "{mean} ({sd})"))
#### 2.2 Univariate Cox regression analysis ####
data_cox = data %>% mutate(Status= Status %>% as.numeric() %>% -1) 
unicox <- data_cox %>% 
  tbl_uvregression(
    method = coxph,
    y = Surv(Survival.months,Status),
    exponentiate = TRUE,
    pvalue_fun = function(x) style_pvalue(x, digits = 2),
    hide_n = T
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.050) %>%
  bold_labels() %>%
  italicize_levels()

##### 2.3 Multivariate Cox regression analysis ####
fml = as.formula(Surv(Survival.months, Status) ~ .)
multi_cox <- coxph(fml, data = data_cox) %>%
  tbl_regression(exponentiate=T,
                 pvalue_fun = ~style_pvalue(.x, digits = 2),
                 hide_n = T
  )%>%
  add_global_p() %>%
  bold_p(t = 0.050) %>%
  bold_labels() %>%
  italicize_levels()

#### 2.4 Merge tables ####
tbl_merge(
  tbls = list(overall,unicox, multi_cox),
  tab_spanner = c("**Overall**", "**Univariate Cox**", "**Multivariate Cox**")
) %>% 
  bold_labels() %>%
  italicize_levels()%>%
  as_flex_table() %>%
  autofit()%>%
  bold(bold = TRUE, part = "header")%>%
  flextable::save_as_docx(path = 'Tables/Merge_Unicox_Multicox.docx')

include_cols <- colnames(data)
exclude_cols <- c('Race','Primary.site',"Chemotherapy")

#### Impute data ####
library(missForest)
data_imputed <- missForest(
  data[colnames(data) %>% setdiff(exclude_cols) %>% setdiff(c('Status','Survival.months'))]
)[["ximp"]] %>% cbind(
  data[c('Status','Survival.months')]
)

#### Feature Selecting -- Correlation plot ------------------------------------
library(corrplot)
corr_data <- data_imputed
corr_data <- as.data.frame(lapply(corr_data,as.numeric),check.names = F)
colnames(corr_data) <- lapply(colnames(corr_data), function(x){gsub('\\.',' ',x)})
corMatMy <- cor(corr_data%>%.[,setdiff(names(.),c('Status','Survival months'))])
col <- colorRampPalette(c("darkorange", "white", "steelblue"))(20)
corrplot(corMatMy, type = "upper", order = "hclust", col = col,tl.col="black", tl.cex=0.8, tl.srt=70)
tiff(paste('Figures','correation plot.tiff',sep = '/'),width=2000,height = 2000,res=300)
corrplot(corMatMy, type = "upper", order = "hclust", col = col,tl.col="black", tl.cex=1, tl.srt=70)
dev.off()

# exclude_cols %<>% append(c('Stage')) 

#### Save data ####
data_imputed %>% #.[setdiff(names(.),exclude_cols)] %>% 
  mutate_at(.,sapply(.,function(x) length(levels(x))) %>% .[.<10] %>% names,~as.numeric(.)-1) %>%
  data.frame() %>% 
  write.csv('data/data_surv.csv',row.names = F)

data.frame(
  include_cols = include_cols %>% setdiff(exclude_cols)
) %>% write.csv('data/include_cols.csv')

plot_distribution <- function(data_){
  plot_list <- list()
  for (colname in colnames(data_)){
    print(colname)
    if (data_[[colname]] %>% is.numeric){
      p <- ggplot(data_,mapping=aes(x=.data[[colname]])) + geom_histogram()
    }else{
      p <- ggplot(data_,aes(x=.data[[colname]])) + geom_bar() + 
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 
    }
    p <- p +
      theme_bw()+ scale_color_npg()+ theme(axis.text.x = element_text(angle = 0,hjust=0.5,size = 5))
    plot_list <- append(plot_list,list(p))
  }
  grid.arrange(grobs = plot_list, ncol = 4)
}

data_display <- data[colnames(data) %>% setdiff(exclude_cols)] 
data_display %>% plot_distribution()




