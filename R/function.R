library(devtools)
library(data.table)

library(R.utils)
library(mlr)
library(randomForest)
library(reticulate)
#use_python(gsub('lib/R','bin/python3.8',R.home()))
#py_config()

# conda create -n AOZORAtools -c conda-forge R r-devtools r-data.table r-reticulate r-r.utils r-mlr r-randomforest python=3.8 unidic-lite mecab-python3 
# conda activate AOZORAtools
# pip install asari
# R

# remove.packages('AOZORAtools')
# devtools::install_github('bigbrolv/AOZORAtools')
# library(AOZORAtools)
# sentiment_analysis('広告多すぎる')
# install.packages(c("devtools","roxygen2","testthat","knitr"))
# reticulate::install_miniconda()
# use_miniconda('/Users/slv/Library/r-miniconda')
# reticulate::py_install('mecab-python3==1.0.2',pip = T)
# reticulate::py_install('unidic-lite',pip = T)
# reticulate::py_install('fugashi',pip = T)
# reticulate::py_install('asari',pip = T)
# reticulate::conda_install(packages = 'libcxx',forge = T)
# mac 需要安装xcode （mecab need）
#需要的包  1.data.table 2.reticulate 3.R.utils

load('data/fiction_info.rda')

# 构建R下载函数
download_fiction <- function(作品名字, 作家, fiction_dir = 'fiction_download') {
  # 创建下载作品路径
  if(!dir.exists(fiction_dir)){dir.create(fiction_dir)}

  # 检查作品名字是否存在于数据框中
  if (!(作品名字 %in% fiction_info$作品名)) {
    stop("作品名字不存在于数据库中。")
  }

  # 检查作家是否存在于数据框中
  if (!(作家 %in% fiction_info$author)) {
    stop("作家不存在于数据库中。")
  }
  print('输入的作品在数据库中存在，可以下载！')

  # 获取小说下载的url列表 --可能一个或多个
  url_ls = fiction_info[(fiction_info$作品名 %in% 作品名字)&(fiction_info$author %in%作家),'テキストファイルURL']

  # 下载文件
  #url = url_ls[1]
  for (url in url_ls){
    download_file = paste0(fiction_dir,'/',basename(url))
    download.file(url, download_file, mode = "wb")

    # 解压文件
    txt_pre_ls = list.files(path = fiction_dir,pattern = 'txt$',full.names = T)
    unzip(download_file,exdir =fiction_dir  ,overwrite = T)
    txt_post_ls = list.files(path = fiction_dir,pattern = 'txt$',full.names = T)
    info_df = file.info(txt_post_ls)
    info_df  = info_df[order(info_df$mtime, decreasing = TRUE),]
    unzip_file = setdiff(txt_post_ls,txt_pre_ls) # 获取解压文件路径
    if(length(unzip_file) == 0){
      unzip_file = rownames(info_df)[1]}
    # 删除原文件
    file.remove(download_file)

    # 解压文件名改为规定格式
    newname = fiction_info[fiction_info$`テキストファイルURL` ==url,'fiction_new_name']
    file.rename(from = unzip_file,to = paste0(fiction_dir,'/',newname))
  }
}
#dir.create('AOZORAtools')
#setwd('AOZORAtools')
#download_fiction(作品名字 ='樹木とその葉',作家 = '若山牧水' )
#a = mecab_process('fiction_download/2210_樹木とその葉_若山牧水_16-酒の讃と苦笑.txt')
#b =word_packet_from_mecab(a,output_prefix = '1')
#sentiment_analysis('広告多すぎる')
#school_classify(b)

# Python 函数  funcMecab funcAsari
# mecab = import('MeCab')
# tagger = mecab$Tagger()
# funcMecab = function(text_file){
#   fiction_text = readLines(text_file,encoding= 'Shift-JIS')
#   fiction_text <- iconv(fiction_text, from = "Shift-JIS", to = "UTF-8")
#   fiction_text_all = paste0(fiction_text,collapse = '')
#   return(tagger$parse(fiction_text_all))
# }
# 
# asari = import('asari')
# Sonar = asari$api$Sonar()
# funcAsari = function(txt){
#   return(Sonar$ping(text = txt))
# }
# funcAsari('広告多すぎる')
# 分词
mecab_process  = function(fiction_file = NULL,
                          fiction_dir = 'fiction_download',
                          mecab_result_dir = 'mecab_result',
                          output = F){
  # 禁止warning
  options (warn = -1)
  source_python(system.file('data/mecab2.py', package = "AOZORAtools"))
  #分词
  #source_python('mecab.py')
  print(paste0('Output file is in ',mecab_result_dir,'.'))
  if(!dir.exists(mecab_result_dir)){dir.create(mecab_result_dir)}

  # Mecab Output
  mecab_res = fread(
    #py_call("funcMecab", args = list(fiction_file)),
    funcMecab(fiction_file),
    data.table = F
    )
  return(mecab_res)
  if(output ==T){fwrite(mecab_res,file = paste0(mecab_result_dir,'/',gsub('.txt','.mecab_res.txt',fiction_file)),row.names = F,quote = F,sep = '\t')}

  # 恢复warning
  options (warn = 1)
}

# 输出词袋
word_packet_from_mecab = function(processed_mecab_result,
                                  word_packet_dir ='word_packet_result',output_prefix){
  source_python(system.file('data/mecab2.py', package = "AOZORAtools")) #测试包

  # 创建文件夹
  print(paste0('Output file is in ',word_packet_dir,'.'))
  if(!dir.exists(word_packet_dir)){dir.create(word_packet_dir)}
  # 如果输入是对象
  if (class(processed_mecab_result)=="data.frame"  ){
    print('监测到输入R object,进行分词。')
    work_packet_df = as.data.frame(table(processed_mecab_result[,1]))
    colnames(work_packet_df) = c('word','frequence')
    work_packet_df = work_packet_df[order(work_packet_df[,2],decreasing = T),]
    fwrite(work_packet_df,file = paste0(word_packet_dir,'/',output_prefix,'.txt'),sep = '\t',quote = F)
    print(paste0('分词结果输出路径为',word_packet_dir,'/',output_prefix,'.txt'))
    return(work_packet_df)
  }
  # 如果输入是字符串
  if (class(processed_mecab_result)=="character"  ){
    print('监测到输入为字符串,进行读取文件。')
    work_packet_df = fread(processed_mecab_result,data.table = F)
    if(dim(df)[2] != 8){stop('不是mecab的输出，中止！')}
    colnames(work_packet_df) = c('word','frequence')
    work_packet_df = work_packet_df[order(work_packet_df[,2],decreasing = T),]
    fwrite(work_packet_df,file = paste0(word_packet_dir,'/',output_prefix,'.txt'),sep = '\t',quote = F)
    print(paste0('分词结果输出路径为',word_packet_dir,'/',output_prefix,'.txt'))
    return(work_packet_df)
  }
}

#b =word_packet_from_mecab(a,output_name = '1')

#情感分析

sentiment_analysis = function(text){
  #source_python('mecab2.py') #测试包
  source_python(system.file('data/asari2.py', package = "AOZORAtools"))
  asari_res = funcAsari2(text)
  print(paste0('输入文字的情感是：',asari_res$top_class))
  return(asari_res)
}

#a = sentiment_analysis('広告多すぎる')
#a = funcAsari("広告多すぎる♡")
#all_words = readRDS('data/classifier_word_list.rds')
#model = readRDS('data/model.rds')
# 分类器    --需要提前载入model和word_list
school_classify = function(word_packet_result){
  if (class(word_packet_result)=="data.frame"  ){
    print('监测到输入R object,进行分类器。')
    word_packet_df = word_packet_result}
  if (class(word_packet_result)=="character"  ){
    print('监测到输入字符串,进行读取文本后分类器。')
    word_packet_df =  data.table::fread(word_packet_result,data.table =F)
  }


  # 词袋重合 intersect_df  合并  词袋不重合setdiff_df
  all_words = readRDS(system.file('data/classifier_word_list.rds', package = "AOZORAtools"))
  match_df = data.frame(word = all_words,code = paste0('W',1:length(all_words)))

  model = readRDS(system.file('data/model.rds', package = "AOZORAtools"))
  intersect_df = word_packet_df[word_packet_df[,1] %in% all_words,]
  setdiff_df = data.frame(word = setdiff(all_words,word_packet_df[,1]),frequence = 0)
  need_predict_df = rbind(intersect_df,setdiff_df)
  need_predict_df2 = data.frame(predict_txt = need_predict_df[,2])
  rownames(need_predict_df2) = need_predict_df[,1]
  rownames(need_predict_df2) = match_df[match(rownames(need_predict_df2),match_df$word),'code']
  predictions <- predict(model, newdata = as.data.frame(t(need_predict_df2)))
  school = predictions[['data']][1,1]
  school = as.character(school)
  print(paste0('小说流派分类器的结果为：',school))
  return(school)
}
#school_classify(word_packet_df)
