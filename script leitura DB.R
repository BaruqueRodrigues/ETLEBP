library(DBI)
library(dplyr)
library(ETLEBP)
library(RSQLite)

# Importando os dados sqlite
filename <- "data/DB_EIP/EIP_20210415.db"

# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(),
                 ":memory:",
                 dbname = filename)

# Checar os elementos presentes dentro do SQLITE
dbListTables(con)

# Chamando um dataset dentro desse sqlite
mytbl1 <- dbReadTable(con,"dm_agente_empresa")
mytbl2 <- dbReadTable(con,"dm_categoria")
mytbl3 <- dbReadTable(con,"dm_formentador")
mytbl4 <- dbReadTable(con,"dm_mod_finan")
mytbl5 <- dbReadTable(con,"dm_nat_disp")
mytbl6 <- dbReadTable(con,"dm_projeto")
mytbl7 <- dbReadTable(con,"ft_dispendio")
names(mytbl1)



#inserir categorias IEA do sqlite na base

consulta <- select(mytbl7, id_item, id_cat2, id_formnt)
m2<-mytbl2 %>% select(id,cat2)
m3 <- mytbl3 %>% select(id_formentador,nme_form)
consulta <- left_join(consulta, m2,by = c("id_cat2" = "id"))
consulta <- left_join(consulta, m3, by = c("id_formnt"= "id_formentador"))

consulta <- consulta %>% mutate(id_projeto = paste(nme_form, id_item , sep = "-"))

consulta<-consulta %>% select(id_projeto,cat2)
left_join(a, consulta, by = c("id"= "id_projeto"))


a<-merge(a, consulta[,c("cat2","id_projeto")],
      by.x = "id",
      by.y = "id_projeto")
a<-cria_base_intermediaria_aneel()
b<- cria_base_intermediaria_bndes()




fim:nrow()
#Quebrando a base
{
  inicio<-(max(mytbl1$id_agente)+1)

  fim<-(inicio+nrow(b)-1)
  dm_agente_empresa <- b %>% select(nome_agente_executor,nome_agente_financiador,uf_ag_executor,fonte_de_dados,
                                    natureza_agente_executor)

  dm_agente_empresa <- dm_agente_empresa %>%
  mutate(
    id_agente = inicio:fim,
    nme_agente = nome_agente_executor,
    uf = uf_ag_executor,
    municipio = NA,
    cnpj = NA,
    ntz_agente = natureza_agente_executor
  ) %>%
  select(id_agente,
         nme_agente,
         ntz_agente,
         uf,
         municipio,
         cnpj)

  dbExecute(con, 'INSERT INTO dm_agente_empresa (id_agente, nme_agente,ntz_agente, uf, municipio, cnpj)
          VALUES (:id_agente, :nme_agente, :ntz_agente, :uf, :municipio, :cnpj);', dm_agente_empresa)
  #Fim da carga
  a <-a %>% filter(!titulo_projeto %in% mytbl6$título )

#Carga dm_projeto
dm_projeto <- b %>%
              select(id, data_assinatura,data_limite,titulo_projeto,status_projeto)


inicio<-(max(mytbl6$id_projeto)+1)

fim<-(inicio+nrow(b)-1)

dm_projeto <- dm_projeto %>%
                        mutate(id_projeto = inicio:fim,
                               id_item = id,
                               dta_inicio = as.character(data_assinatura),
                               dta_limite = as.character(data_limite),
                               'título'     = titulo_projeto,
                               'situação'   = status_projeto) %>%
                        select(id_projeto,id_item,
                               dta_inicio,dta_limite,'título',
                               'situação')


dbExecute(con, 'INSERT INTO dm_projeto (id_projeto, id_item,dta_inicio, dta_limite, título, situação)
          VALUES (:id_projeto, :id_item, :dta_inicio, :dta_limite, :título, :situação);', dm_projeto)


##Carga ft_dispendio
vlr_res <- e %>% select(id, valor_executado_2013:valor_executado_2020) %>%
                tidyr::gather(ano, vlr, -id) %>%
                mutate(ano = recode(ano,
                                    "valor_executado_2013" = 2013,
                                    "valor_executado_2014" = 2014,
                                    "valor_executado_2015" = 2015,
                                    "valor_executado_2016" = 2016,
                                    "valor_executado_2017" = 2017,
                                    "valor_executado_2018" = 2018,
                                    "valor_executado_2019" = 2019,
                                    "valor_executado_2020" = 2020)) %>%
                rename(id_item = id)



bs_res <- e %>% select(id, natureza_agente_financiador,data_assinatura,categorias,nome_agente_executor,
                             fonte_de_dados, modalidade_financiamento)

outra<- bs_res %>% select(nome_agente_executor)%>%na.omit(nome_agente_executor)
outra<- left_join(outra, mytbl1[,c(1,2)],
                    by = c("nome_agente_executor"="nme_agente"))%>%
                    rename(id_exec = id_agente)

bs_res <- left_join(bs_res, outra) %>% unique()


bs_res <- left_join(bs_res, mytbl2[,c(1,3)],
          by =  c("categorias" = "cat2")) %>%
          rename(id_item = id.x,
                 id_cat2 = id.y,
                 dta_inicio = data_assinatura)


inicio<-(max(mytbl7$id_disp)+1)

fim<-(inicio+nrow(bs_res)-1)

inicio:fim
bs_res<-bs_res %>% mutate(fonte_de_dados = recode(fonte_de_dados,
                                       "ANEEL"  = 5,
                                       "BNDES"  = 6,
                                       "FINEP"  = 7,
                                       "Finep"  = 7,
                                       "FNDCT"  = 8,
                                       "CNPq"   = 9,
                                       "FAPESP" = 10,
                                       "ANP"    = 11,
                                       "CNEN"   = 12),
                  modalidade_financiamento = recode(modalidade_financiamento,
                                                    "Reembolsável"  = 1,
                                                    "Não-reembolsável"= 2,
                                                    "Não Reembolsável" =2,
                                                    "Subvenção" =3,
                                                    "Não se Aplica" = 4,
                                                    "Não informado" =5),
                  natureza_agente_financiador = recode(natureza_agente_financiador,
                                                       "Empresa Privada" = 0,
                                                       "empresa pública" = 1,
                                                       "Empresa pública" = 1,
                                                       "Empresa Pública" = 1,
                                                       "Empresa privada" = 0,
                                                       "Empresa economia mista" =1,
                                                       "Fundação de Amparo (FAP)" = 1,
                                                       "ICT pública" =1,
                                                       "ONU" =0),
                  chamada = NA,
                  id_disp = inicio:fim
                  ) %>%
          rename(id_formnt = fonte_de_dados,
                 mod_finan = modalidade_financiamento,
                 ntz_finan = natureza_agente_financiador)
#id_prop e id_finan e id_exec medem a mesma coisa
bs_res<-left_join(vlr_res, bs_res )
bs_res<-bs_res %>% select(-nome_agente_executor,-categorias)

dbExecute(con, 'INSERT INTO ft_dispendio (id_item, ano, vlr, ntz_finan, dta_inicio,
                                          id_exec, id_formnt, mod_finan, id_cat2,chamada, id_disp)
          VALUES (:id_item, :ano, :vlr, :ntz_finan, :dta_inicio,
                  :id_exec, :id_formnt, :mod_finan, :id_cat2, :chamada, :id_disp);', bs_res)
names(mytbl7)
names(bs_res)
}
