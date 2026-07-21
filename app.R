library(shiny)
library(dplyr)
library(DT)
library(writexl)

# ==============================================================================
# 1) ESCOPO GLOBAL: CARGA E PREPARAÇÃO DOS DADOS
# ==============================================================================

base_bruta_escolas <- read.csv2("autonomia_financeira_dinamica_2026.csv",
                                check.names = FALSE)

# ==============================================================================
# 2) INTERFACE DO USUÁRIO (UI)
# ==============================================================================
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Simulador de Autonomia Financeira Escolar (Cenário 2026)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      actionButton("calcular", "Calcular Novo Cenário", class = "btn-primary btn-lg w-100"),
      hr(),
      
      tabsetPanel(
        # ---- ABA ORÇAMENTO ----
        tabPanel("Orçamento", 
                 br(),
                 numericInput("dotacao_total", "Dotação Total (R$)", value = 172621092.63, min = 0),
                 sliderInput("perc_fixo", "Percentual Fixo (%)", min = 0, max = 100, value = 45, step = 1),
                 sliderInput("perc_variavel", "Percentual Variável (%)", min = 0, max = 100, value = 45, step = 1),
                 sliderInput("perc_equidade", "Percentual Equidade (%)", min = 0, max = 100, value = 10, step = 1)
        ),
        
        # ---- ABA PESOS FIXOS ----
        tabPanel("Pesos Fixo",
                 br(),
                 h5("Ponderação por Turnos"),
                 numericInput("p_turno1", "1 Turno", value = 1.0, step = 0.05),
                 numericInput("p_turno2", "2 Turnos", value = 1.25, step = 0.05),
                 numericInput("p_turno3", "3 Turnos", value = 1.5, step = 0.05),
                 hr(),
                 h5("Ponderação por Área (fx_area)"),
                 numericInput("p_area1", "Faixa 1 (ln < 8)", value = 1.0, step = 0.1),
                 numericInput("p_area2", "Faixa 2 (ln 8-10)", value = 1.5, step = 0.1),
                 numericInput("p_area3", "Faixa 3 (ln 10-12)", value = 2.0, step = 0.1),
                 numericInput("p_area4", "Faixa 4 (ln >= 12)", value = 2.5, step = 0.1),
                 hr(),
                 h5("Ponderação por Salas (fx_salas)"),
                 numericInput("p_sala1", "Faixa 1 (0 a 5)", value = 1.0, step = 0.05),
                 numericInput("p_sala2", "Faixa 2 (6 a 10)", value = 1.25, step = 0.05),
                 numericInput("p_sala3", "Faixa 3 (11 a 15)", value = 1.5, step = 0.05),
                 numericInput("p_sala4", "Faixa 4 (Mais de 15)", value = 2.0, step = 0.05),
                 hr(),
                 h5("Ponderação por Ambientes Especiais"),
                 numericInput("p_amb1", "1 Ambiente Esp.", value = 1.0, step = 0.1),
                 numericInput("p_amb2", "2 Ambientes Esp.", value = 2.0, step = 0.1),
                 numericInput("p_amb3", "3 Ambientes Esp.", value = 3.0, step = 0.1)
        ),
        
        # ---- ABA PESOS VARIÁVEIS ----
        tabPanel("Pesos Variável",
                 br(),
                 h5("Infantil e Fundamental"),
                 numericInput("pv_inf_int", "Infantil - Integral", value = 1.4, step = 0.05),
                 numericInput("pv_inf_parc", "Infantil - Parcial", value = 1.0, step = 0.05),
                 numericInput("pv_fund_int", "Fundamental - Integral", value = 1.4, step = 0.05),
                 numericInput("pv_fund_parc", "Fundamental - Parcial", value = 1.0, step = 0.05),
                 hr(),
                 h5("Ensino Médio e Técnico"),
                 numericInput("pv_med_int", "Médio - Integral", value = 1.8, step = 0.05),
                 numericInput("pv_med_parc", "Médio - Parcial", value = 1.4, step = 0.05),
                 numericInput("pv_med_art", "Médio Articulado / IFTP", value = 1.8, step = 0.05),
                 numericInput("pv_ept", "Técnico Profissional", value = 1.0, step = 0.05),
                 hr(),
                 h5("Modalidades e Diversidade"),
                 numericInput("pv_esp", "Educação Especial", value = 1.4, step = 0.05),
                 numericInput("pv_ind_qui", "Indígenas / Quilombolas", value = 1.25, step = 0.05),
                 numericInput("pv_eja", "EJA", value = 1.0, step = 0.05),
                 numericInput("pv_neeja", "NEEJA", value = 0.50, step = 0.05)
        )
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        # ---- ABA RESUMOS DA REDE ----
        tabPanel("Resumos de Auditoria (Rede)", 
                 br(), 
                 verbatimTextOutput("valores_base"),
                 hr(),
                 h4("Resumo do Impacto do Cenário (Ganho / Perda por Faixas)"),
                 tableOutput("resumo_impacto"), 
                 hr(),
                 h4("Resumo Completo da Parcela Fixa"), 
                 tableOutput("resumo_fixo"),
                 hr(),
                 h4("Resumo Completo da Parcela Variável (Matrículas e Etapas)"), 
                 tableOutput("resumo_variavel"),
                 hr(),
                 h4("Resumo Completo da Parcela de Equidade (Sexo, Raça e Vulnerabilidade)"), 
                 tableOutput("resumo_equidade")
        ),
        
        # ---- ABA CONSULTA INDIVIDUAL POR ESCOLA ----
        tabPanel("Consulta por Escola",
                 br(),
                 h4("Buscar Escola"),
                 selectizeInput("escola_selecionada", "Selecione ou digite o IDT ou Nome da Escola:", choices = NULL, width = "100%"),
                 hr(),
                 verbatimTextOutput("valores_base_escola"),
                 hr(),
                 h4("Detalhamento da Parcela Fixa Calculada para esta Escola"),
                 tableOutput("resumo_fixo_escola"),
                 hr(),
                 h4("Detalhamento da Parcela Variável (Matrículas desta Escola)"),
                 tableOutput("resumo_variavel_escola"),
                 hr(),
                 h4("Detalhamento da Parcela de Equidade (Estudantes desta Escola)"),
                 tableOutput("resumo_equidade_escola")
        ),
        
        # ---- ABA BASE DE DADOS ----
        tabPanel("Base de Dados por Escola", 
                 br(),
                 downloadButton("baixar_relatorio_completo", "Baixar Relatório Completo (.xlsx)", class = "btn-success"),
                 br(), br(),
                 DTOutput("tabela_resultados")
        )
      )
    )
  )
)

# ==============================================================================
# 3) SERVER (LÓGICA REATIVA DO SISTEMA)
# ==============================================================================
server <- function(input, output, session) {
  
  # Alerta de validação do orçamento total (deve somar 100%)
  observe({
    soma_perc <- input$perc_fixo + input$perc_variavel + input$perc_equidade
    if(soma_perc != 100) {
      showNotification("Atenção: A soma dos percentuais da dotação não fecha em 100%!", type = "warning", duration = NULL, id = "aviso_perc")
    } else {
      removeNotification("aviso_perc")
    }
  })
  
  # Alimenta o campo de busca de escolas dinamicamente assim que o app inicia
  observe({
    df <- base_bruta_escolas
    escolhas <- df$idt_estab
    names(escolhas) <- paste0(df$idt_estab, " - ", df$nome)
    updateSelectizeInput(session, "escola_selecionada", choices = escolhas, server = TRUE)
  })
  
  dados_simulados <- eventReactive(input$calcular, {
    
    # 1. Resgata parâmetros de orçamento do painel lateral
    dot_total <- input$dotacao_total
    p_fixo    <- input$perc_fixo / 100
    p_var     <- input$perc_variavel / 100
    p_eq      <- input$perc_equidade / 100
    
    # 2. Resgata parâmetros dinâmicos da parcela Fixa
    t1 <- input$p_turno1; t2 <- input$p_turno2; t3 <- input$p_turno3
    a1 <- input$p_area1;  a2 <- input$p_area2;  a3 <- input$p_area3;  a4 <- input$p_area4
    s1 <- input$p_sala1;  s2 <- input$p_sala2;  s3 <- input$p_sala3;  s4 <- input$p_sala4
    am1 <- input$p_amb1;  am2 <- input$p_amb2;  am3 <- input$p_amb3
    
    # 2.5 Resgata parâmetros dinâmicos da parcela Variável
    v_inf_int  <- input$pv_inf_int;  v_inf_parc  <- input$pv_inf_parc
    v_fund_int <- input$pv_fund_int; v_fund_parc <- input$pv_fund_parc
    v_med_int  <- input$pv_med_int;  v_med_parc  <- input$pv_med_parc
    v_med_art  <- input$pv_med_art;  v_ept       <- input$pv_ept
    v_esp      <- input$pv_esp;      v_ind_qui   <- input$pv_ind_qui
    v_eja      <- input$pv_eja;      v_neeja     <- input$pv_neeja
    
    # 3. Recalcula Ponderadores por Escola com base nos Inputs do usuário
    res <- base_bruta_escolas %>%
      mutate(
        fx_salas = case_when(
          qt_salas_utilizadas >= 0 & qt_salas_utilizadas <= 5 ~ 1,
          qt_salas_utilizadas >= 6 & qt_salas_utilizadas <= 10 ~ 2,
          qt_salas_utilizadas >= 11 & qt_salas_utilizadas <= 15 ~ 3,
          qt_salas_utilizadas >= 16 ~ 4,
          TRUE ~ NA_real_
        ),
        ambientes_aprendizagem_esp = rowSums(across(c(in_laboratorio_ciencias, in_quadra_esportes, in_sala_atendimento_especial)), na.rm = TRUE),
        area_aux = if_else(is.na(area_terreno), 100, area_terreno),
        ln_area  = log(area_aux),
        fx_area = case_when(
          ln_area >= 12 ~ 4, ln_area >= 10 & ln_area < 12 ~ 3,
          ln_area >= 8 & ln_area < 10 ~ 2, ln_area < 8 ~ 1, TRUE ~ NA_real_
        ),
        
        turno_pond    = case_when(qt_turnos == 1 ~ t1, qt_turnos == 2 ~ t2, qt_turnos == 3 ~ t3, TRUE ~ NA_real_),
        ambiente_pond = case_when(
          ambientes_aprendizagem_esp == 1 ~ am1,
          ambientes_aprendizagem_esp == 2 ~ am2,
          ambientes_aprendizagem_esp == 3 ~ am3,
          TRUE ~ 0
        ),
        area_pond     = case_when(fx_area == 1 ~ a1, fx_area == 2 ~ a2, fx_area == 3 ~ a3, fx_area == 4 ~ a4, TRUE ~ NA_real_),
        sala_pond     = case_when(fx_salas == 1 ~ s1, fx_salas == 2 ~ s2, fx_salas == 3 ~ s3, fx_salas == 4 ~ s4, TRUE ~ NA_real_),
        
        # Consolidação reativa do peso Fixo
        fator_fixo = rowSums(across(c(turno_pond, ambiente_pond, area_pond, sala_pond)), na.rm = TRUE),
        
        # Consolidação dinâmica do peso Variável usando os novos inputs
        num_indigenas   = replace_na(num_indigenas, 0),
        num_quilombolas = replace_na(num_quilombolas, 0),
        fator_variavel = if_else(
          !is.na(qt_mat_neeja) & qt_mat_neeja > 0, qt_mat_neeja * v_neeja,
          qt_mat_inf_pre_int * v_inf_int + qt_mat_inf_pre_parc * v_inf_parc +
            qt_mat_fund_int * v_fund_int + qt_mat_fund_parc * v_fund_parc +
            qt_mat_med_int * v_med_int + qt_mat_med_parc * v_med_parc +
            qt_mat_med_arti_iftp_ct * v_med_art +
            (qt_mat_prof_tec_conc + qt_mat_prof_tec_subs + qt_mat_prof_fic_conc) * v_ept +
            qt_mat_esp * v_esp + (num_quilombolas + num_indigenas) * v_ind_qui + qt_mat_eja * v_eja
        ),
        
        # Consolidação do peso de Equidade Dinâmica
        alunos_semcad_ppi = pmax(0, qt_mat_bas_ppi - alunos_cad_F_PPI - alunos_cad_M_PPI),
        fator_equidade = if_else(
          !is.na(qt_mat_neeja) & qt_mat_neeja > 0, 0,
          alunos_cad_F_BA  * fe_nota_f_ba +
            alunos_cad_F_PPI * fe_nota_f_ppi +
            alunos_cad_M_BA  * fe_nota_m_ba +
            alunos_cad_M_PPI * fe_nota_m_ppi +
            alunos_semcad_ppi * ((fe_nota_semcad_f + fe_nota_semcad_m) / 2)
        )
      )
    
    # 4. Divisão do orçamento dinâmico por subcomponente
    orcamento_fixo     <- dot_total * p_fixo
    orcamento_variavel <- dot_total * p_var
    orcamento_equidade <- dot_total * p_eq
    
    total_fator_fixo     <- sum(res$fator_fixo[res$suepro == 0], na.rm = TRUE)
    total_fator_variavel <- sum(res$fator_variavel[res$suepro == 0], na.rm = TRUE)
    total_fator_equidade <- sum(res$fator_equidade, na.rm = TRUE)
    
    valor_base_fixo     <- if_else(total_fator_fixo > 0, orcamento_fixo / total_fator_fixo, 0)
    valor_base_variavel <- if_else(total_fator_variavel > 0, orcamento_variavel / total_fator_variavel, 0)
    valor_base_equidade <- if_else(total_fator_equidade > 0, orcamento_equidade / total_fator_equidade, 0)
    
    # 5. Distribuição financeira por escola e cálculo de variações
    res <- res %>%
      mutate(
        vl_fixo_dinamico = if_else(suepro == 0, fator_fixo * valor_base_fixo, NA_real_),
        vl_fixo_dinamico = if_else(suepro == 1, fator_fixo * 11843.18, vl_fixo_dinamico), 
        
        vl_variavel_dinamico = if_else(suepro == 0, fator_variavel * valor_base_variavel, NA_real_),
        vl_variavel_dinamico = if_else(suepro == 1, fator_variavel * 127.04, vl_variavel_dinamico), 
        
        vl_equidade_dinamico = fator_equidade * valor_base_equidade,
        
        af_novo_dinamico = rowSums(across(c(vl_fixo_dinamico, vl_variavel_dinamico, vl_equidade_dinamico)), na.rm = TRUE),
        
        # Cálculos de comparação com o orçamento vigente
        var_simples     = af_novo_dinamico - af_vigente,
        var_porcentagem = if_else(af_vigente > 0, (var_simples / af_vigente) * 100, 0)
      ) %>%
      arrange(desc(var_porcentagem))
    
    # Retorna lista estruturada
    list(
      dados = res,
      v_base_fixo = valor_base_fixo,
      v_base_variavel = valor_base_variavel,
      v_base_equidade = valor_base_equidade,
      orc_fixo = orcamento_fixo,       
      orc_var = orcamento_variavel,    
      orc_eq = orcamento_equidade      
    )
  }, ignoreNULL = FALSE)
  
  # ---- REATIVO PARA FILTRAR A ESCOLA SELECIONADA ----
  escola_filtrada <- reactive({
    req(input$escola_selecionada)
    res_list <- dados_simulados()
    df_esc <- res_list$dados %>% filter(idt_estab == input$escola_selecionada)
    list(
      escola = df_esc,
      v_base_fixo = if_else(df_esc$suepro == 1, 11843.18, res_list$v_base_fixo),
      v_base_variavel = if_else(df_esc$suepro == 1, 127.04, res_list$v_base_variavel),
      v_base_equidade = res_list$v_base_equidade
    )
  })
  
  # ==============================================================================
  # EXPRESSÕES REATIVAS DAS TABELAS E TEXTOS (INCLUI SLIDERS E INPUTS)
  # ==============================================================================
  
  # Tabela: Resultados dos Orçamentos Globais e Valores Base Calculados
  tb_valores_base_react <- reactive({
    res <- dados_simulados()
    
    tibble(
      `Indicador / Parcela` = c(
        "Dotação Total Definida",
        "Percentual Parcela Fixa",
        "Orçamento Global Fixo",
        "Valor Base Fixo",
        "Percentual Parcela Variável",
        "Orçamento Global Variável",
        "Valor Base Variável",
        "Percentual Parcela Equidade",
        "Orçamento Global Equidade",
        "Valor Base Equidade",
        "DOTAÇÃO TOTAL APURADA"
      ),
      `Valor` = c(
        paste0("R$ ", format(round(input$dotacao_total, 2), big.mark=".", decimal.mark=",")),
        paste0(input$perc_fixo, " %"),
        paste0("R$ ", format(round(res$orc_fixo, 2), big.mark=".", decimal.mark=",")),
        paste0("R$ ", format(round(res$v_base_fixo, 4), big.mark=".", decimal.mark=",")),
        paste0(input$perc_variavel, " %"),
        paste0("R$ ", format(round(res$orc_var, 2), big.mark=".", decimal.mark=",")),
        paste0("R$ ", format(round(res$v_base_variavel, 4), big.mark=".", decimal.mark=",")),
        paste0(input$perc_equidade, " %"),
        paste0("R$ ", format(round(res$orc_eq, 2), big.mark=".", decimal.mark=",")),
        paste0("R$ ", format(round(res$v_base_equidade, 4), big.mark=".", decimal.mark=",")),
        paste0("R$ ", format(round(res$orc_fixo + res$orc_var + res$orc_eq, 2), big.mark=".", decimal.mark=","))
      )
    )
  })
  
  # Texto formatado dos Valores Base
  txt_valores_base_react <- reactive({
    res <- dados_simulados()
    paste0(
      "Resultados dos Orçamentos Globais e Valores Base Calculados:\n",
      "Dotação Total Definida: R$ ", format(round(input$dotacao_total, 2), big.mark=".", decimal.mark=","), "\n",
      "-----------------------------------------------------------\n",
      "PARCELA FIXA (", input$perc_fixo, "%):\n",
      "  > Orçamento Global Fixo: R$ ", format(round(res$orc_fixo, 2), big.mark=".", decimal.mark=","), "\n",
      "  > Valor Base Fixo:        R$ ", format(round(res$v_base_fixo, 4), big.mark=".", decimal.mark=","), "\n\n",
      "PARCELA VARIÁVEL (", input$perc_variavel, "%):\n",
      "  > Orçamento Global Var.: R$ ", format(round(res$orc_var, 2), big.mark=".", decimal.mark=","), "\n",
      "  > Valor Base Variável:   R$ ", format(round(res$v_base_variavel, 4), big.mark=".", decimal.mark=","), "\n\n",
      "PARCELA EQUIDADE (", input$perc_equidade, "%):\n",
      "  > Orçamento Global Eq.:  R$ ", format(round(res$orc_eq, 2), big.mark=".", decimal.mark=","), "\n",
      "  > Valor Base Equidade:   R$ ", format(round(res$v_base_equidade, 4), big.mark=".", decimal.mark=","), "\n",
      "-----------------------------------------------------------\n",
      "DOTAÇÃO TOTAL APURADA:     R$ ", format(round(res$orc_fixo + res$orc_var + res$orc_eq, 2), big.mark=".", decimal.mark=",")
    )
  })
  
  # Tabela: Resumo de Impacto
  tb_impacto_react <- reactive({
    df <- dados_simulados()$dados
    total_escolas <- nrow(df)
    
    tibble(
      `Resumo Cenário` = c(
        "Perda acima de 50%", "Perda acima de 25%", "Perda acima de 10%", "Perda", 
        "Ganho", "Ganho acima de 10%", "Ganho acima de 25%", "Ganho acima de 50%", 
        "Ganho acima de 75%", "Ganho acima de 100%"
      ),
      Qtd = c(
        sum(df$var_porcentagem <= -50, na.rm = TRUE),
        sum(df$var_porcentagem <= -25, na.rm = TRUE),
        sum(df$var_porcentagem <= -10, na.rm = TRUE),
        sum(df$var_porcentagem < 0, na.rm = TRUE),
        sum(df$var_porcentagem > 0, na.rm = TRUE),
        sum(df$var_porcentagem >= 10, na.rm = TRUE),
        sum(df$var_porcentagem >= 25, na.rm = TRUE),
        sum(df$var_porcentagem >= 50, na.rm = TRUE),
        sum(df$var_porcentagem >= 75, na.rm = TRUE),
        sum(df$var_porcentagem >= 100, na.rm = TRUE)
      )
    ) %>%
      mutate(
        `%` = paste0(round((Qtd / total_escolas) * 100, 0), "%"),
        Qtd = format(Qtd, big.mark = ".", decimal.mark = ",")
      )
  })
  
  # Tabela: Parcela Fixa
  tb_fixo_react <- reactive({
    res_list    <- dados_simulados()
    df          <- res_list$dados
    v_base_fixo <- res_list$v_base_fixo
    df_reg      <- df %>% filter(suepro == 0)
    
    tibble(
      `Categoria Parcela Fixa` = c(
        "Escolas de 1 Turno", "Escolas de 2 Turnos", "Escolas de 3 Turnos",
        "Escolas Área - Faixa 1", "Escolas Área - Faixa 2", "Escolas Área - Faixa 3", "Escolas Área - Faixa 4",
        "Escolas Salas - Até 5", "Escolas Salas - 6 a 10", "Escolas Salas - 11 a 15", "Escolas Salas - Mais de 15",
        "Escolas com 0 Ambientes Esp.", "Escolas com 1 Ambiente Esp.", "Escolas com 2 Ambientes Esp.", "Escolas com 3 Ambientes Esp."
      ),
      `Quantidade de Escolas` = c(
        sum(df_reg$qt_turnos == 1, na.rm = TRUE),
        sum(df_reg$qt_turnos == 2, na.rm = TRUE),
        sum(df_reg$qt_turnos == 3, na.rm = TRUE),
        sum(df_reg$fx_area == 1, na.rm = TRUE),
        sum(df_reg$fx_area == 2, na.rm = TRUE),
        sum(df_reg$fx_area == 3, na.rm = TRUE),
        sum(df_reg$fx_area == 4, na.rm = TRUE),
        sum(df_reg$fx_salas == 1, na.rm = TRUE),
        sum(df_reg$fx_salas == 2, na.rm = TRUE),
        sum(df_reg$fx_salas == 3, na.rm = TRUE),
        sum(df_reg$fx_salas == 4, na.rm = TRUE),
        sum(df_reg$ambientes_aprendizagem_esp == 0, na.rm = TRUE),
        sum(df_reg$ambientes_aprendizagem_esp == 1, na.rm = TRUE),
        sum(df_reg$ambientes_aprendizagem_esp == 2, na.rm = TRUE),
        sum(df_reg$ambientes_aprendizagem_esp == 3, na.rm = TRUE)
      ),
      `Peso Ponderador` = c(
        input$p_turno1, input$p_turno2, input$p_turno3,
        input$p_area1, input$p_area2, input$p_area3, input$p_area4,
        input$p_sala1, input$p_sala2, input$p_sala3, input$p_sala4,
        0, input$p_amb1, input$p_amb2, input$p_amb3
      )
    ) %>%
      mutate(
        `Valor Base Fixo (R$)` = v_base_fixo,
        `Total Estimado Categoria (R$)` = `Quantidade de Escolas` * `Peso Ponderador` * v_base_fixo
      ) %>%
      mutate(
        `Quantidade de Escolas` = format(`Quantidade de Escolas`, big.mark = ".", decimal.mark = ","),
        `Peso Ponderador` = format(round(`Peso Ponderador`, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
        `Valor Base Fixo (R$)` = format(round(`Valor Base Fixo (R$)`, 4), nsmall = 4, big.mark = ".", decimal.mark = ","),
        `Total Estimado Categoria (R$)` = format(round(`Total Estimado Categoria (R$)`, 2), nsmall = 2, big.mark = ".", decimal.mark = ",")
      )
  })
  
  # Tabela: Parcela Variável
  tb_variavel_react <- reactive({
    res_list   <- dados_simulados()
    df         <- res_list$dados
    v_base_var <- res_list$v_base_variavel
    
    df %>%
      filter(suepro == 0) %>%
      summarise(
        `Pré-Escola Integral`       = sum(qt_mat_inf_pre_int, na.rm = TRUE),
        `Pré-Escola Parcial`        = sum(qt_mat_inf_pre_parc, na.rm = TRUE),
        `Ens. Fundamental Integral` = sum(qt_mat_fund_int, na.rm = TRUE),
        `Ens. Fundamental Parcial`  = sum(qt_mat_fund_parc, na.rm = TRUE),
        `Ens. Médio Integral`       = sum(qt_mat_med_int, na.rm = TRUE),
        `Ens. Médio Parcial`        = sum(qt_mat_med_parc, na.rm = TRUE),
        `Médio Articulado/Iftp/Ct`  = sum(qt_mat_med_arti_iftp_ct, na.rm = TRUE),
        `Educação Especial`         = sum(qt_mat_esp, na.rm = TRUE),
        `Indígenas e Quilombolas`   = sum(num_quilombolas + num_indigenas, na.rm = TRUE),
        `EJA`                       = sum(qt_mat_eja, na.rm = TRUE),
        `EPT Profissional`          = sum(qt_mat_prof_tec_conc + qt_mat_prof_tec_subs + qt_mat_prof_fic_conc, na.rm = TRUE),
        `NEEJA`                     = sum(qt_mat_neeja, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Etapa de Matrícula", values_to = "Matrículas Brutas") %>%
      mutate(
        `Peso Ponderador` = case_when(
          `Etapa de Matrícula` == "Pré-Escola Integral"       ~ input$pv_inf_int,
          `Etapa de Matrícula` == "Pré-Escola Parcial"        ~ input$pv_inf_parc,
          `Etapa de Matrícula` == "Ens. Fundamental Integral" ~ input$pv_fund_int,
          `Etapa de Matrícula` == "Ens. Fundamental Parcial"  ~ input$pv_fund_parc,
          `Etapa de Matrícula` == "Ens. Médio Integral"       ~ input$pv_med_int,
          `Etapa de Matrícula` == "Ens. Médio Parcial"        ~ input$pv_med_parc,
          `Etapa de Matrícula` == "Médio Articulado/Iftp/Ct"  ~ input$pv_med_art,
          `Etapa de Matrícula` == "Educação Especial"         ~ input$pv_esp,
          `Etapa de Matrícula` == "Indígenas e Quilombolas"   ~ input$pv_ind_qui,
          `Etapa de Matrícula` == "EJA"                       ~ input$pv_eja,
          `Etapa de Matrícula` == "EPT Profissional"          ~ input$pv_ept,
          `Etapa de Matrícula` == "NEEJA"                     ~ input$pv_neeja,
          TRUE                                                ~ 0
        ),
        `Valor Base Variável` = v_base_var,
        `Valor por Aluno (R$)` = `Peso Ponderador` * v_base_var,
        `Total Estimado Etapa (R$)` = `Matrículas Brutas` * `Valor por Aluno (R$)`
      ) %>%
      mutate(
        `Matrículas Brutas` = format(`Matrículas Brutas`, big.mark = ".", decimal.mark = ","),
        `Peso Ponderador` = format(round(`Peso Ponderador`, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
        `Valor Base Variável` = format(round(`Valor Base Variável`, 4), nsmall = 4, big.mark = ".", decimal.mark = ","),
        `Valor por Aluno (R$)` = format(round(`Valor por Aluno (R$)`, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
        `Total Estimado Etapa (R$)` = format(round(`Total Estimado Etapa (R$)`, 2), nsmall = 2, big.mark = ".", decimal.mark = ",")
      )
  })
  
  # Tabela: Parcela Equidade
  tb_equidade_react <- reactive({
    res_list  <- dados_simulados()
    df        <- res_list$dados
    v_base_eq <- res_list$v_base_equidade
    
    df_elegivel <- df %>%
      mutate(
        across(c(alunos_cad_F_BA, alunos_cad_F_PPI, alunos_cad_M_BA, alunos_cad_M_PPI, alunos_semcad_ppi),
               ~ if_else(!is.na(qt_mat_neeja) & qt_mat_neeja > 0, 0, as.numeric(.)))
      )
    
    f_f_ba   <- if("fe_nota_f_ba" %in% names(df)) df$fe_nota_f_ba else fe_nota_f_ba
    f_f_ppi  <- if("fe_nota_f_ppi" %in% names(df)) df$fe_nota_f_ppi else fe_nota_f_ppi
    f_m_ba   <- if("fe_nota_m_ba" %in% names(df)) df$fe_nota_m_ba else fe_nota_m_ba
    f_m_ppi  <- if("fe_nota_m_ppi" %in% names(df)) df$fe_nota_m_ppi else fe_nota_m_ppi
    f_semcad <- if("fe_nota_semcad_f" %in% names(df)) ((df$fe_nota_semcad_f + df$fe_nota_semcad_m) / 2) else ((fe_nota_semcad_f + fe_nota_semcad_m) / 2)
    
    tot_f_ba   <- sum(df_elegivel$alunos_cad_F_BA * f_f_ba * v_base_eq, na.rm = TRUE)
    tot_f_ppi  <- sum(df_elegivel$alunos_cad_F_PPI * f_f_ppi * v_base_eq, na.rm = TRUE)
    tot_m_ba   <- sum(df_elegivel$alunos_cad_M_BA * f_m_ba * v_base_eq, na.rm = TRUE)
    tot_m_ppi  <- sum(df_elegivel$alunos_cad_M_PPI * f_m_ppi * v_base_eq, na.rm = TRUE)
    tot_semcad <- sum(df_elegivel$alunos_semcad_ppi * f_semcad * v_base_eq, na.rm = TRUE)
    
    tibble(
      `Grupo / Categoria de Equidade` = c(
        "Feminino - No CadÚnico (Branca / Amarela)",
        "Feminino - No CadÚnico (Preta / Parda / Indígena)",
        "Masculino - No CadÚnico (Branca / Amarela)",
        "Masculino - No CadÚnico (Preta / Parda / Indígena)",
        "Fora do CadÚnico (Preta / Parda / Indígena)"
      ),
      `Estudantes Elegíveis` = c(
        sum(df_elegivel$alunos_cad_F_BA, na.rm = TRUE),
        sum(df_elegivel$alunos_cad_F_PPI, na.rm = TRUE),
        sum(df_elegivel$alunos_cad_M_BA, na.rm = TRUE),
        sum(df_elegivel$alunos_cad_M_PPI, na.rm = TRUE),
        sum(df_elegivel$alunos_semcad_ppi, na.rm = TRUE)
      ),
      `Total Estimado Categoria (R$)` = c(tot_f_ba, tot_f_ppi, tot_m_ba, tot_m_ppi, tot_semcad)
    ) %>%
      mutate(
        `Valor Base Equidade` = v_base_eq,
        `Peso Médio Ponderador` = if_else(`Estudantes Elegíveis` > 0, (`Total Estimado Categoria (R$)` / v_base_eq) / `Estudantes Elegíveis`, 0),
        `Valor Médio por Aluno (R$)` = `Peso Médio Ponderador` * v_base_eq
      ) %>%
      select(
        `Grupo / Categoria de Equidade`, `Estudantes Elegíveis`, `Peso Médio Ponderador`, 
        `Valor Base Equidade`, `Valor Médio por Aluno (R$)`, `Total Estimado Categoria (R$)`
      ) %>%
      mutate(
        `Estudantes Elegíveis` = format(`Estudantes Elegíveis`, big.mark = ".", decimal.mark = ","),
        `Peso Médio Ponderador` = format(round(`Peso Médio Ponderador`, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
        `Valor Base Equidade` = format(round(`Valor Base Equidade`, 4), nsmall = 4, big.mark = ".", decimal.mark = ","),
        `Valor Médio por Aluno (R$)` = format(round(`Valor Médio por Aluno (R$)`, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
        `Total Estimado Categoria (R$)` = format(round(`Total Estimado Categoria (R$)`, 2), nsmall = 2, big.mark = ".", decimal.mark = ",")
      )
  })
  
  # ==============================================================================
  # OUTPUTS DA ABA: RESUMOS DE AUDITORIA (REDE COMPLETA)
  # ==============================================================================
  
  output$valores_base <- renderText({
    txt_valores_base_react()
  })
  
  output$resumo_impacto <- renderTable({
    tb_impacto_react()
  }, align = 'lrr')
  
  output$resumo_fixo <- renderTable({
    tb_fixo_react()
  }, align = 'lrrrr')
  
  output$resumo_variavel <- renderTable({
    tb_variavel_react()
  }, align = 'lrrrrr')
  
  output$resumo_equidade <- renderTable({
    tb_equidade_react()
  }, align = 'lrrrrr')
  
  # ==============================================================================
  # OUTPUTS DA ABA: CONSULTA INDIVIDUAL POR ESCOLA
  # ==============================================================================
  
  output$valores_base_escola <- renderText({
    esc_list <- escola_filtrada()
    df_esc <- esc_list$escola
    
    tipo_rede <- if_else(df_esc$suepro == 1, "SUEPRO (Valores Regulamentados Fixos)", "Regular / Técnica")
    
    paste0(
      "Demonstrativo de Impacto Orçamentário Individual da Escola:\n",
      "-----------------------------------------------------------\n",
      "Escola Selecionada: ", df_esc$nome, " (IDT: ", df_esc$idt_estab, ")\n",
      "Segmento da Rede:   ", tipo_rede, "\n\n",
      "RESUMO FINANCEIRO DA ESCOLA NO NOVO CENÁRIO:\n",
      "  > Repasse Atual (Vigente):  R$ ", format(round(df_esc$af_vigente, 2), big.mark=".", decimal.mark=","), "\n",
      "  > Novo Repasse Calculado:   R$ ", format(round(df_esc$af_novo_dinamico, 2), big.mark=".", decimal.mark=","), "\n",
      "  > Variação Absoluta:        R$ ", format(round(df_esc$var_simples, 2), big.mark=".", decimal.mark=","), "\n",
      "  > Impacto Percentual:       ", format(round(df_esc$var_porcentagem, 2), big.mark=".", decimal.mark=","), " %\n\n",
      "VALORES BASE EFETIVOS NESTA SIMULAÇÃO:\n",
      "  > Vl. Base Fixo Aplicado:   R$ ", format(round(esc_list$v_base_fixo, 4), big.mark=".", decimal.mark=","), "\n",
      "  > Vl. Base Var. Aplicado:   R$ ", format(round(esc_list$v_base_variavel, 4), big.mark=".", decimal.mark=","), "\n",
      "  > Vl. Base Equidade Aplic.: R$ ", format(round(esc_list$v_base_equidade, 4), big.mark=".", decimal.mark=",")
    )
  })
  
  output$resumo_fixo_escola <- renderTable({
    esc_list <- escola_filtrada()
    df_esc <- esc_list$escola
    v_base_f <- esc_list$v_base_fixo
    
    tibble(
      `Indicador Fixo` = c("Turnos Funcionamento", "Área Terreno (Log)", "Salas Utilizadas", "Ambientes Especiais"),
      `Métrica na Escola` = c(
        paste0(df_esc$qt_turnos, " turno(s)"),
        paste0(round(df_esc$ln_area, 2), " (Faixa ", df_esc$fx_area, ")"),
        paste0(df_esc$qt_salas_utilizadas, " sala(s) (Faixa ", df_esc$fx_salas, ")"),
        paste0(df_esc$ambientes_aprendizagem_esp, " ambiente(s)")
      ),
      `Peso Ponderador` = c(df_esc$turno_pond, df_esc$area_pond, df_esc$sala_pond, df_esc$ambiente_pond)
    ) %>%
      mutate(
        `Valor Base Fixo (R$)` = v_f <- v_base_f,
        `Total Parcela (R$)` = `Peso Ponderador` * v_base_f
      ) %>%
      mutate(
        `Peso Ponderador` = format(round(`Peso Ponderador`, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
        `Valor Base Fixo (R$)` = format(round(`Valor Base Fixo (R$)`, 4), nsmall = 4, big.mark = ".", decimal.mark = ","),
        `Total Parcela (R$)` = format(round(`Total Parcela (R$)`, 2), nsmall = 2, big.mark = ".", decimal.mark = ",")
      )
  }, align = 'llrrr')
  
  output$resumo_variavel_escola <- renderTable({
    esc_list <- escola_filtrada()
    df_esc <- esc_list$escola
    v_base_v <- esc_list$v_base_variavel
    
    tibble(
      `Etapa de Matrícula` = c(
        "Pré-Escola Integral", "Pré-Escola Parcial", "Ens. Fundamental Integral", "Ens. Fundamental Parcial",
        "Ens. Médio Integral", "Ens. Médio Parcial", "Médio Articulado/Iftp/Ct", "Educação Especial",
        "Indígenas e Quilombolas", "EJA", "EPT Profissional", "NEEJA"
      ),
      `Matrículas da Escola` = c(
        df_esc$qt_mat_inf_pre_int, df_esc$qt_mat_inf_pre_parc, df_esc$qt_mat_fund_int, df_esc$qt_mat_fund_parc,
        df_esc$qt_mat_med_int, df_esc$qt_mat_med_parc, df_esc$qt_mat_med_arti_iftp_ct, df_esc$qt_mat_esp,
        (df_esc$num_quilombolas + df_esc$num_indigenas), df_esc$qt_mat_eja,
        (df_esc$qt_mat_prof_tec_conc + df_esc$qt_mat_prof_tec_subs + df_esc$qt_mat_prof_fic_conc), df_esc$qt_mat_neeja
      ),
      `Peso Ponderador` = c(
        input$pv_inf_int, input$pv_inf_parc, input$pv_fund_int, input$pv_fund_parc,
        input$pv_med_int, input$pv_med_parc, input$pv_med_art, input$pv_esp,
        input$pv_ind_qui, input$pv_eja, input$pv_ept, input$pv_neeja
      )
    ) %>%
      mutate(
        `Valor Base Variável` = v_base_v,
        `Valor por Aluno (R$)` = `Peso Ponderador` * v_base_v,
        `Total Etapa (R$)` = `Matrículas da Escola` * `Valor por Aluno (R$)`
      ) %>%
      mutate(
        `Matrículas da Escola` = format(`Matrículas da Escola`, big.mark = ".", decimal.mark = ","),
        `Peso Ponderador` = format(round(`Peso Ponderador`, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
        `Valor Base Variável` = format(round(`Valor Base Variável`, 4), nsmall = 4, big.mark = ".", decimal.mark = ","),
        `Valor por Aluno (R$)` = format(round(`Valor por Aluno (R$)`, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
        `Total Etapa (R$)` = format(round(`Total Etapa (R$)`, 2), nsmall = 2, big.mark = ".", decimal.mark = ",")
      )
  }, align = 'lrrrrr')
  
  output$resumo_equidade_escola <- renderTable({
    esc_list <- escola_filtrada()
    df_esc <- esc_list$escola
    v_base_eq <- esc_list$v_base_equidade
    
    f_f_ba   <- if("fe_nota_f_ba" %in% names(df_esc)) df_esc$fe_nota_f_ba else fe_nota_f_ba
    f_f_ppi  <- if("fe_nota_f_ppi" %in% names(df_esc)) df_esc$fe_nota_f_ppi else fe_nota_f_ppi
    f_m_ba   <- if("fe_nota_m_ba" %in% names(df_esc)) df_esc$fe_nota_m_ba else fe_nota_m_ba
    f_m_ppi  <- if("fe_nota_m_ppi" %in% names(df_esc)) df_esc$fe_nota_m_ppi else fe_nota_m_ppi
    f_semcad <- if("fe_nota_semcad_f" %in% names(df_esc)) ((df_esc$fe_nota_semcad_f + df_esc$fe_nota_semcad_m) / 2) else ((fe_nota_semcad_f + fe_nota_semcad_m) / 2)
    
    is_neeja <- !is.na(df_esc$qt_mat_neeja) && df_esc$qt_mat_neeja > 0
    
    est_f_ba   <- if(is_neeja) 0 else df_esc$alunos_cad_F_BA
    est_f_ppi  <- if(is_neeja) 0 else df_esc$alunos_cad_F_PPI
    est_m_ba   <- if(is_neeja) 0 else df_esc$alunos_cad_M_BA
    est_m_ppi  <- if(is_neeja) 0 else df_esc$alunos_cad_M_PPI
    est_semcad <- if(is_neeja) 0 else df_esc$alunos_semcad_ppi
    
    tibble(
      `Grupo / Categoria de Equidade` = c(
        "Feminino - No CadÚnico (Branca / Amarela)",
        "Feminino - No CadÚnico (Preta / Parda / Indígena)",
        "Masculino - No CadÚnico (Branca / Amarela)",
        "Masculino - No CadÚnico (Preta / Parda / Indígena)",
        "Fora do CadÚnico (Preta / Parda / Indígena)"
      ),
      `Estudantes Elegíveis` = c(est_f_ba, est_f_ppi, est_m_ba, est_m_ppi, est_semcad),
      `Peso Ponderador` = c(f_f_ba, f_f_ppi, f_m_ba, f_m_ppi, f_semcad)
    ) %>%
      mutate(
        `Valor Base Equidade` = v_base_eq,
        `Valor por Aluno (R$)` = `Peso Ponderador` * v_base_eq,
        `Total Categoria (R$)` = `Estudantes Elegíveis` * `Valor por Aluno (R$)`
      ) %>%
      mutate(
        `Estudantes Elegíveis` = format(`Estudantes Elegíveis`, big.mark = ".", decimal.mark = ","),
        `Peso Ponderador` = format(round(`Peso Ponderador`, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
        `Valor Base Equidade` = format(round(`Valor Base Equidade`, 4), nsmall = 4, big.mark = ".", decimal.mark = ","),
        `Valor Médio por Aluno (R$)` = format(round(`Valor por Aluno (R$)`, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
        `Total Categoria (R$)` = format(round(`Total Categoria (R$)`, 2), nsmall = 2, big.mark = ".", decimal.mark = ",")
      ) %>% 
      select(
        `Grupo / Categoria de Equidade`, 
        `Estudantes Elegíveis`, 
        `Peso Ponderador`, 
        `Valor Base Equidade`, 
        `Valor Médio por Aluno (R$)`, 
        `Total Categoria (R$)`
      )
  }, align = 'lrrrrr')
  
  # ==============================================================================
  # OUTPUTS DA ABA: BASE DE DADOS COMPLETA (DT)
  # ==============================================================================
  output$tabela_resultados <- renderDT({
    df_exibicao <- dados_simulados()$dados %>%
      select(
        idt_estab, co_entidade, nome, suepro, 
        af_vigente, af_novo_dinamico, var_simples, var_porcentagem,
        vl_fixo_dinamico, vl_variavel_dinamico, vl_equidade_dinamico
      )
    
    datatable(
      df_exibicao,
      options = list(
        pageLength = 15, 
        order = list(list(7, 'desc')), 
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(c("af_vigente", "af_novo_dinamico", "var_simples", "vl_fixo_dinamico", "vl_variavel_dinamico", "vl_equidade_dinamico"), currency = "R$ ", mark = ".", dec.mark = ",") %>%
      formatRound("var_porcentagem", digits = 2, mark = ".", dec.mark = ",")
  })
  
  # Exportação do relatório em Excel (com todas as abas reativas)
  output$baixar_relatorio_completo <- downloadHandler(
    filename = function() {
      paste0("Relatorio_Autonomia_Financeira_Rede_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      lista_de_abas <- list(
        "Escolas Simuladas"     = dados_simulados()$dados %>% mutate(across(where(is.character), ~ gsub('"', '', .))),
        "Orçamentos e Vlr Base" = tb_valores_base_react(),
        "Resumo de Impacto"     = tb_impacto_react(),
        "Resumo Parcela Fixa"   = tb_fixo_react(),
        "Resumo Parcela Var"    = tb_variavel_react(),
        "Resumo Equidade"       = tb_equidade_react()
      )
      
      writexl::write_xlsx(lista_de_abas, path = file)
    }
  )
}

# Inicializa a aplicação
shinyApp(ui = ui, server = server)