library(data.table)
# load_all('.')

l_transcripts <- read.table(
  'D:/WEIXIN/WeChat Files/wxid_mkzug8s7hewb22/FileStorage/File/2022-09/list', col.names = 'col_1'
  )$col_1

res_data <-
  rbindlist(
    lapply(l_transcripts[1], get_protein_homologs),
    fill = T
  )

save(res_data, file = 'inst/extdata/processed/dt_protein_homologs.rda')


get_protein_homologs('Cre01.g005534.t1.2')
