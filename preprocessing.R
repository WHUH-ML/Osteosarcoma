library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(ggsci)
get_data <- function(){
  origin_data <- readxl::read_xlsx('../data/Osteosarcoma AYA _ 4.1.xlsx')
  data <- origin_data %>% 
    filter(`Primary Site` %in% c(400:403)) %>%
    # filter(`First malignant primary indicator`== 'Yes') %>%
    # filter(`RX Summ--Surg Prim Site (1998+)` %in% c(30, 41, 40, 26, 0, 42, 25, 53, 50, 51, 52, 54, 19)) %>%
    filter(!grepl('Blank|NA|UNK|IVNOS',`Derived AJCC Stage Group, 6th ed (2004-2015)`)) %>%
    filter(!grepl('Paget',`ICD-O-3 Hist/behav`)) %>%
    filter(!grepl('Blank|NA|UNK|unknow|^Surgery',`RX Summ--Systemic/Sur Seq`))%>%
    filter(`Survival months` != 0) %>%
    # filter(`Marital status at diagnosis`!='Unknown') %>%
    # filter(`Race recode (W, B, AI, API)`!='Unknown')%>%
    # filter(`Grade (thru 2017)` != 'Unknown') %>%
    # filter(`CS tumor size (2004-2015)`<=989 && `CS tumor size (2004-2015)`>0) %>%
    # filter(`CS mets at dx (2004-2015)` %in% c(0,30,35,40,50)) %>%
    # filter(!grepl('999',`CS extension (2004-2015)`)) %>%
    mutate(`Age recode with single ages and 100+`=gsub('years','',`Age recode with single ages and 100+`))%>%
    subset(select = c(
      `Age recode with single ages and 100+`,
      `Sex`,
      `Marital status at diagnosis`,
      `Race recode (White, Black, Other)`,
      `ICD-O-3 Hist/behav`,
      `Primary Site - labeled`,
      `Derived AJCC Stage Group, 6th ed (2004-2015)`,
      `Grade (thru 2017)`,
      `RX Summ--Surg Prim Site (1998+)`,
      `RX Summ--Systemic/Sur Seq`,
      `RX Summ--Surg/Rad Seq`,
      `Chemotherapy recode (yes, no/unk)`,
      `CS tumor size (2004-2015)`,
      `Total number of in situ/malignant tumors for patient`,
      `CS extension (2004-2015)`,
      `CS mets at dx (2004-2015)`,
      `Survival months`,
      `Vital status recode (study cutoff used)`
      )) %>%
    setNames(
      c(
        'Age','Gender','Marital status','Race','Histological type','Primary site','Stage','Grade','Surgery','Systemic therapy',
        'Radiotherapy','Chemotherapy','Tumor size','Number of tumors','Tumor extension','Distant metastasis',
        'Survival months','Status'
      )
    ) %>%
    mutate(
      `Tumor size` = replace(`Tumor size`,`Tumor size` %in% append(seq(989,999,1),'Blank(s)'),NA),
      Stage = replace(Stage,grepl('UNK|Blank',Stage),NA),
      Grade = replace(Grade,grepl('Unknown|Blank',Grade),NA),
      Surgery = replace(Surgery,grepl('90|99',Surgery),NA) ,
      `Tumor extension` = replace(`Tumor extension`,grepl('999|Blanks',`Tumor extension`),NA) ,
      `Distant metastasis` = replace(`Distant metastasis`,grepl('99|Blank',`Distant metastasis`),NA) 
    ) %>%
    mutate(
      Age = as.numeric(Age),
      Gender = factor(Gender,levels = c('Female', 'Male')),
      `Marital status` = grepl('^Married',`Marital status`) %>% factor(labels = c('Not married','Married')),
      Race = Race %>% factor(levels = c('White', 'Black', 'Other (American Indian/AK Native, Asian/Pacific Islander)'),
                             labels = c('White', 'Black', 'Other')),
      `Histological type` = `Histological type` %>% factor(levels = c("9180/3: Osteosarcoma, NOS", 
                                                                      "9181/3: Chondroblastic osteosarcoma", "9182/3: Fibroblastic osteosarcoma", 
                                                                      "9183/3: Telangiectatic osteosarcoma", "9185/3: Small cell osteosarcoma", 
                                                                      "9186/3: Central osteosarcoma", "9187/3: Intraosseous well differentiated osteosarcoma", 
                                                                      "9192/3: Parosteal osteosarcoma", "9193/3: Periosteal osteosarcoma", 
                                                                      "9194/3: High grade surface osteosarcoma"),
                                                           labels = c("9180", 
                                                             "9181", "9182/3", 
                                                             "9183/3", "9185/3", 
                                                             "9186/3", "9187/3", 
                                                             "9192/3", "9193/3", 
                                                             "9194/3")),
      `Primary site` = `Primary site` %>% factor(
        levels = c("C40.2-Long bones of lower limb and associated joints", "C40.0-Long bones: upper limb, scapula, and associated joints", 
                   "C40.1-Short bones of upper limb and associated joints", "C40.3-Short bones of lower limb and associated joints"
        ),
        labels = c('Long bones','Long bones','Short bones','Short bones')
      ),
      Stage = Stage %>% factor(
        levels = c("IA", "IB","IIA", "IIB","III",  "IVA",  "IVB"),
        labels = c("IA", "IB","IIA", "IIB","III",  "IVA",  "IVB")
      ),
      Grade = Grade %>% factor(
        levels = c("Well differentiated; Grade I","Moderately differentiated; Grade II",  
                   "Poorly differentiated; Grade III", "Undifferentiated; anaplastic; Grade IV"),
        labels = c("Well differentiated","Moderately differentiated",  
                   "Poorly differentiated", "Undifferentiated")
        ),
      Surgery = Surgery %>% factor(
        levels = c(0, 
                   40, 41, 42, 50, 51, 52, 53, 54,
                   19, 25, 26,
                   30
                   ),
        labels = c('None',
                   rep('Amputation',8),
                   rep('Local destruction or excision',3),
                   'Radical excision with limb salvage'
                   )
      ),
      `Systemic therapy` = `Systemic therapy` %>% factor(
        levels = c("No systemic therapy and/or surgical procedures", "Systemic therapy both before and after surgery", 
                   "Systemic therapy before surgery", "Systemic therapy after surgery"),
        labels = c("Not", "before surgery", "after surgery", "both before and after surgery")
      ),
      Radiotherapy = grepl('^No',Radiotherapy) %>% `!` %>% factor(labels = c('Not','Yes')),
      Chemotherapy = grepl('^Yes',Chemotherapy) %>% factor(labels = c('Not','Yes')),
      `Tumor size` = `Tumor size` %>% as.numeric(),
      `Number of tumors` = `Number of tumors` %>% as.numeric() %>% cut(breaks=c(-Inf,1,Inf),labels=c('1','> 1')),
      `Tumor extension` = `Tumor extension` %>% factor(
        levels = c("100", "200",
                   "600", "400",  "300", "350", "310",
                   "700", "800", "820", "850"
                   ),
        labels = c(
          rep('No break in periosteum',2),
          rep('Extension beyond periosteum',5),
          rep('Further extension',4)
        )
      ),
      `Distant metastasis` = `Distant metastasis` %>% factor(
        levels = c("0",
                   "30",
                   "35", "40", "50"),
        labels = c(
          'None',
          'Lung only',
          rep('Other distant organ',3)
        )
      ),
      `Survival months` = `Survival months` %>% as.numeric,
      Status = Status %>% factor(levels = c('Alive','Dead'))
      )
  
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
  # data %>% plot_distribution()
  # write.csv(data,'data1.csv')
  data
    
}
  
