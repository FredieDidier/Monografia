# =============================================================================
# build_dictionary.R
#
# Generates the variable dictionary for the project's micro-data as an Excel
# workbook: dictionary/variable_dictionary.xlsx
#
#   Files            what each data file is, how many rows, where it lives
#   main_data        build output: one row per worker employed in quarter t
#   analysis_sample  analysis input: matched origins, the estimation sample
#   Legend           value labels for every coded variable
#   Provenance       how the data were built, and the measurement decisions
#
# Row counts and column types are read from the parquet files themselves, so the
# dictionary cannot drift from the data. Run after build/code/00_master_build.R
# and analysis/code/01_prepare_analysis_data.R:
#
#   Rscript dictionary/build_dictionary.R
# =============================================================================

suppressPackageStartupMessages({
  library(openxlsx)
  library(arrow)
  library(data.table)
})

if (!exists("ROOT")) source(file.path("analysis", "code", "_config.R"))

OUT <- file.path(ROOT, "dictionary", "variable_dictionary.xlsx")
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)

ORIGINS_PQ <- file.path(DIR_DATA, "analysis_origins.parquet")

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
pq_info <- function(path) {
  if (!file.exists(path)) return(NULL)
  ds <- open_dataset(path)
  s  <- schema(ds)
  list(nrow  = nrow(ds),
       size  = file.size(path),
       types = setNames(
         vapply(s$fields, function(f) {
           t <- f$type$ToString()
           if (grepl("^dictionary", t)) "factor"
           else if (t == "string") "character"
           else if (grepl("^int", t)) "integer"
           else "numeric"
         }, character(1)),
         names(s)))
}

info_main <- pq_info(RAW_PARQUET)
info_est  <- pq_info(ANALYSIS_PQ)
info_org  <- pq_info(ORIGINS_PQ)

#' Attach the observed type to a dictionary block, and fail loudly if the
#' dictionary and the data disagree about which columns exist.
attach_types <- function(dict, info, label) {
  if (is.null(info)) { dict$Type <- NA_character_; return(dict) }
  missing_in_dict <- setdiff(names(info$types), dict$Variable)
  missing_in_data <- setdiff(dict$Variable, names(info$types))
  if (length(missing_in_dict))
    warning(label, ": columns in the data but not documented: ",
            paste(missing_in_dict, collapse = ", "))
  if (length(missing_in_data))
    warning(label, ": documented but absent from the data: ",
            paste(missing_in_data, collapse = ", "))
  dict$Type <- unname(info$types[dict$Variable])
  dict
}

d <- function(...) data.frame(..., stringsAsFactors = FALSE)

# =============================================================================
# main_data.parquet
# =============================================================================
main <- rbind(
  d(Variable = "id_rs3", Source = "datazoom.social",
    PT = "Identificador do indivíduo no painel rotativo, estágio 3 (advanced_3). String hexadecimal cujo primeiro dígito codifica o grupo de rotação, portanto globalmente única. NA quando o algoritmo não conseguiu vincular a pessoa entre trimestres.",
    EN = "Stage-3 individual identifier in the rotating panel (advanced_3). Hexadecimal string whose leading digit encodes the rotation group, hence globally unique. NA when the algorithm could not link the person across quarters.",
    Notes = "Nunca converter para numérico: valores como \"1ffff\" viram NA. Ids que aparecem duas vezes no mesmo trimestre (~0,01%) são anulados no build."),
  d(Variable = "id_dom", Source = "datazoom.social",
    PT = "Identificador do domicílio, único apenas DENTRO de cada grupo de rotação V1014.",
    EN = "Household identifier, unique only WITHIN each V1014 rotation group.",
    Notes = "Prefixado com V1014 em analysis_sample para virar global."),
  d(Variable = "UPA", Source = "PNADC",
    PT = "Unidade primária de amostragem.", EN = "Primary sampling unit.",
    Notes = "Dimensão principal de clusterização da inferência."),
  d(Variable = "Estrato", Source = "PNADC",
    PT = "Estrato do desenho amostral.", EN = "Sampling stratum.", Notes = ""),
  d(Variable = "V1008", Source = "PNADC",
    PT = "Número de seleção do domicílio dentro da UPA.",
    EN = "Household selection number within the PSU.", Notes = ""),
  d(Variable = "V1014", Source = "PNADC",
    PT = "Grupo de rotação (painel). 13 grupos entre 2012Q1 e 2026Q1.",
    EN = "Rotation group (panel). 13 groups between 2012Q1 and 2026Q1.", Notes = ""),
  d(Variable = "V1016", Source = "PNADC",
    PT = "Número da entrevista do domicílio, de 1 a 5.",
    EN = "Household interview number, 1 to 5.",
    Notes = "Entrevista 5 é a última: não pode ser pareada adiante por desenho."),
  d(Variable = "V1022", Source = "PNADC",
    PT = "Situação do domicílio: 1 urbana, 2 rural.",
    EN = "Household location: 1 urban, 2 rural.", Notes = ""),
  d(Variable = "V1023", Source = "PNADC",
    PT = "Tipo de área (capital, região metropolitana, resto).",
    EN = "Area type (capital, metropolitan area, rest).", Notes = ""),
  d(Variable = "V1028", Source = "PNADC",
    PT = "Peso amostral da pessoa, com pós-estratificação.",
    EN = "Person sampling weight, post-stratified.",
    Notes = "Usado em toda estatística e regressão do paper."),
  d(Variable = "UF", Source = "PNADC",
    PT = "Unidade da Federação.", EN = "Federation unit (state).", Notes = ""),
  d(Variable = "Ano", Source = "PNADC", PT = "Ano de referência.", EN = "Reference year.", Notes = ""),
  d(Variable = "Trimestre", Source = "PNADC", PT = "Trimestre de referência.", EN = "Reference quarter.", Notes = ""),
  d(Variable = "qtr", Source = "Derivada",
    PT = "Trimestre como inteiro AAAAT (ex.: 20201 = 2020Q1).",
    EN = "Quarter as integer YYYYQ (e.g. 20201 = 2020Q1).", Notes = "= Ano * 10 + Trimestre."),
  d(Variable = "qidx", Source = "Derivada",
    PT = "Índice sequencial de trimestre, 2012Q1 = 1.",
    EN = "Sequential quarter index, 2012Q1 = 1.", Notes = "Usado para localizar t+1."),
  d(Variable = "V2003", Source = "PNADC", PT = "Número de ordem da pessoa no domicílio.", EN = "Person order number in the household.", Notes = ""),
  d(Variable = "V2005", Source = "PNADC", PT = "Condição no domicílio (responsável, cônjuge, filho...).", EN = "Position in the household (head, spouse, child...).", Notes = "Usada pelo estágio 3 para doar datas de nascimento."),
  d(Variable = "V2007", Source = "PNADC", PT = "Sexo: 1 homem, 2 mulher.", EN = "Sex: 1 male, 2 female.", Notes = ""),
  d(Variable = "V2009", Source = "PNADC", PT = "Idade em anos completos.", EN = "Age in completed years.", Notes = ""),
  d(Variable = "V2010", Source = "PNADC", PT = "Cor ou raça.", EN = "Colour or race.", Notes = "Ver aba Legend."),
  d(Variable = "VD3004", Source = "PNADC", PT = "Nível de instrução mais elevado alcançado.", EN = "Highest education level attained.", Notes = "= 7 é superior completo, a definição de college no paper."),
  d(Variable = "VD3005", Source = "PNADC", PT = "Anos de estudo.", EN = "Years of schooling.", Notes = ""),
  d(Variable = "VD4001", Source = "PNADC", PT = "Condição em relação à força de trabalho: 1 na força, 2 fora.", EN = "Labour force status: 1 in the labour force, 2 out of it.", Notes = ""),
  d(Variable = "VD4002", Source = "PNADC", PT = "Condição de ocupação: 1 ocupado, 2 desocupado.", EN = "Occupation status: 1 employed, 2 unemployed.", Notes = "Combinada com VD4001 define os três estados de destino."),
  d(Variable = "VD4009", Source = "PNADC", PT = "Posição na ocupação e categoria do emprego.", EN = "Position in employment and job category.", Notes = "Entra na classificação de formalidade."),
  d(Variable = "VD4010", Source = "PNADC", PT = "Grupamento de atividade principal (12 grupos).", EN = "Main activity grouping (12 groups).", Notes = "Colapsado em 5 setores; ver Legend."),
  d(Variable = "VD4012", Source = "PNADC", PT = "Contribuinte de instituto de previdência no trabalho principal.", EN = "Contributes to social security in the main job.", Notes = ""),
  d(Variable = "VD4017", Source = "PNADC", PT = "Rendimento habitual do trabalho principal, em reais.", EN = "Usual income from the main job, in reais.", Notes = ""),
  d(Variable = "VD4031", Source = "PNADC", PT = "Horas habitualmente trabalhadas por semana em todos os trabalhos.", EN = "Usual weekly hours worked across all jobs.", Notes = ""),
  d(Variable = "V4010", Source = "PNADC", PT = "Código da ocupação no trabalho principal (COD, 4 dígitos).", EN = "Occupation code in the main job (COD, four digits).", Notes = "Primeiro dígito = grande grupo; ver Legend."),
  d(Variable = "V4012", Source = "PNADC", PT = "Posição na ocupação no trabalho principal.", EN = "Job function in the main job.", Notes = "Ver Legend."),
  d(Variable = "V4013", Source = "PNADC", PT = "Código da atividade principal do empreendimento (CNAE).", EN = "Activity code of the establishment (CNAE).", Notes = ""),
  d(Variable = "V4019", Source = "PNADC", PT = "Empreendimento registrado no CNPJ.", EN = "Business registered with a CNPJ.", Notes = "AUSENTE de 2012Q1 a 2015Q1: o IBGE só passou a coletar em 2015. Ver aba Provenance."),
  d(Variable = "V4025", Source = "PNADC", PT = "Trabalho tinha caráter temporário.", EN = "Job was temporary.", Notes = ""),
  d(Variable = "V4029", Source = "PNADC", PT = "Empregado com carteira de trabalho assinada.", EN = "Employee with a signed work card.", Notes = ""),
  d(Variable = "V4032", Source = "PNADC", PT = "Contribuinte de previdência no trabalho principal.", EN = "Contributes to social security in the main job.", Notes = "Marcador de formalidade para conta própria e, quando falta o CNPJ, para empregadores."),
  d(Variable = "V4039", Source = "PNADC", PT = "Horas habitualmente trabalhadas por semana no trabalho principal.", EN = "Usual weekly hours in the main job.", Notes = ""),
  d(Variable = "V4040", Source = "PNADC", PT = "Tempo no trabalho principal, agrupado em 4 faixas.", EN = "Time in the main job, four grouped bands.", Notes = "Ver Legend."),
  d(Variable = "lf_state", Source = "Derivada", PT = "Estado na força de trabalho em t: Employed, Unemployed, Out of labour force.", EN = "Labour force state in t.", Notes = "De VD4001 e VD4002. Todas as origens são Employed."),
  d(Variable = "position", Source = "Derivada", PT = "Posição no mercado de trabalho, código 3 a 10.", EN = "Labour market position, code 3 to 10.", Notes = "Ver Legend."),
  d(Variable = "formal", Source = "Derivada", PT = "1 se o vínculo em t é formal, 0 se informal.", EN = "1 if the job in t is formal, 0 if informal.", Notes = "Derivada de position."),
  d(Variable = "dest_state", Source = "Derivada", PT = "Estado na força de trabalho em t+1.", EN = "Labour force state in t+1.", Notes = "NA quando não pareado."),
  d(Variable = "dest_formal", Source = "Derivada", PT = "Formalidade do vínculo em t+1, se ainda ocupado.", EN = "Formality of the job in t+1, if still employed.", Notes = ""),
  d(Variable = "matched_next", Source = "Derivada", PT = "1 se o algoritmo vinculou a pessoa ao trimestre t+1.", EN = "1 if the algorithm linked the person into t+1.", Notes = "É a seleção que gera a amostra de estimação; base da análise de atrito."),
  d(Variable = "exit", Source = "Derivada", PT = "1 se ocupado em t e não ocupado em t+1.", EN = "1 if employed in t and not employed in t+1.", Notes = "Desfecho principal. NA se matched_next = 0."),
  d(Variable = "exit_to_unemployment", Source = "Derivada", PT = "1 se ocupado em t e desocupado em t+1 (E->U).", EN = "1 if employed in t and unemployed in t+1 (E->U).", Notes = "Painel B da Tabela 1."),
  d(Variable = "exit_to_nonpart", Source = "Derivada", PT = "1 se ocupado em t e fora da força em t+1 (E->N).", EN = "1 if employed in t and out of the labour force in t+1 (E->N).", Notes = "Painel C da Tabela 1. E->U + E->N = exit."),
  d(Variable = "to_informal", Source = "Derivada", PT = "1 se ocupado informalmente em t+1.", EN = "1 if informally employed in t+1.", Notes = "Estado de destino, não transição: inclui quem já era informal.")
)
main <- attach_types(main, info_main, "main_data.parquet")

# =============================================================================
# analysis_sample.parquet / analysis_origins.parquet
# =============================================================================
est <- rbind(
  d(Variable = "pid", Source = "Derivada", PT = "Identificador da pessoa. Igual a id_rs3 quando pareada; \"u<linha>\" quando não.", EN = "Person identifier. Equals id_rs3 when matched; \"u<row>\" otherwise.", Notes = "Ids distintos para não pareados evitam agrupá-los num efeito fixo espúrio."),
  d(Variable = "psu", Source = "PNADC (UPA)", PT = "Unidade primária de amostragem.", EN = "Primary sampling unit.", Notes = "Dimensão de clusterização."),
  d(Variable = "household", Source = "Derivada", PT = "Domicílio globalmente único: \"<V1014>_<id_dom>\".", EN = "Globally unique household: \"<V1014>_<id_dom>\".", Notes = ""),
  d(Variable = "strata", Source = "PNADC (Estrato)", PT = "Estrato amostral.", EN = "Sampling stratum.", Notes = ""),
  d(Variable = "panel_grp", Source = "PNADC (V1014)", PT = "Grupo de rotação.", EN = "Rotation group.", Notes = ""),
  d(Variable = "interview", Source = "PNADC (V1016)", PT = "Número da entrevista, 1 a 5.", EN = "Interview number, 1 to 5.", Notes = "A análise de atrito restringe a 1-4."),
  d(Variable = "qtr", Source = "Derivada", PT = "Trimestre AAAAT.", EN = "Quarter YYYYQ.", Notes = ""),
  d(Variable = "matched_next", Source = "Derivada", PT = "1 se vinculado a t+1.", EN = "1 if linked into t+1.", Notes = "Sempre 1 em analysis_sample; 0 ou 1 em analysis_origins."),
  d(Variable = "dest_state", Source = "Derivada", PT = "Estado em t+1.", EN = "State in t+1.", Notes = ""),
  d(Variable = "exit", Source = "Derivada", PT = "Saída do emprego entre t e t+1.", EN = "Employment exit between t and t+1.", Notes = "Desfecho principal."),
  d(Variable = "exit_to_unemployment", Source = "Derivada", PT = "Saída para desemprego (E->U).", EN = "Exit into unemployment (E->U).", Notes = ""),
  d(Variable = "exit_to_nonpart", Source = "Derivada", PT = "Saída para fora da força (E->N).", EN = "Exit into non-participation (E->N).", Notes = ""),
  d(Variable = "exit_to_informal", Source = "Derivada", PT = "Ocupado informalmente em t+1.", EN = "Informally employed in t+1.", Notes = "Estado de destino, não transição."),
  d(Variable = "college", Source = "PNADC (VD3004)", PT = "1 se superior completo.", EN = "1 if completed tertiary education.", Notes = "VD3004 == 7. Variável de interesse do paper."),
  d(Variable = "female", Source = "PNADC (V2007)", PT = "1 se mulher.", EN = "1 if woman.", Notes = ""),
  d(Variable = "white", Source = "PNADC (V2010)", PT = "1 se branca.", EN = "1 if white.", Notes = ""),
  d(Variable = "nonwhite", Source = "PNADC (V2010)", PT = "1 se declara cor/raça diferente de branca.", EN = "1 if reports a colour/race other than white.", Notes = "Usada na figura de heterogeneidade por raça."),
  d(Variable = "black_brown", Source = "PNADC (V2010)", PT = "1 se preta ou parda.", EN = "1 if black or brown.", Notes = "Definição alternativa, mantida para comparação."),
  d(Variable = "race5", Source = "PNADC (V2010)", PT = "Cor ou raça em 5 categorias mais não declarado.", EN = "Colour or race in five categories plus not reported.", Notes = "Efeito fixo na especificação principal."),
  d(Variable = "urban", Source = "PNADC (V1022)", PT = "1 se domicílio urbano.", EN = "1 if urban household.", Notes = ""),
  d(Variable = "age", Source = "PNADC (V2009)", PT = "Idade em anos completos.", EN = "Age in completed years.", Notes = "Amostra restrita a 14 anos ou mais."),
  d(Variable = "hours", Source = "PNADC (V4039)", PT = "Horas habituais por semana no trabalho principal.", EN = "Usual weekly hours in the main job.", Notes = ""),
  d(Variable = "income", Source = "PNADC (VD4017)", PT = "Rendimento habitual do trabalho principal, em reais.", EN = "Usual income from the main job, in reais.", Notes = "Ausente codificado como 0."),
  d(Variable = "log_income", Source = "Derivada", PT = "log(1 + income).", EN = "log(1 + income).", Notes = "Controle na especificação principal."),
  d(Variable = "formal", Source = "Derivada", PT = "1 se o vínculo em t é formal.", EN = "1 if the job in t is formal.", Notes = "Sem valores ausentes: ver aba Provenance."),
  d(Variable = "temporary", Source = "PNADC (V4025)", PT = "1 se o trabalho é temporário.", EN = "1 if the job is temporary.", Notes = ""),
  d(Variable = "social_security", Source = "PNADC (V4032)", PT = "1 se contribui para a previdência.", EN = "1 if contributes to social security.", Notes = ""),
  d(Variable = "signed_card", Source = "PNADC (V4029)", PT = "1 se empregado com carteira assinada.", EN = "1 if employee with a signed work card.", Notes = ""),
  d(Variable = "tenure", Source = "PNADC (V4040)", PT = "Tempo no trabalho atual, 4 faixas.", EN = "Time in the current job, four bands.", Notes = "Efeito fixo. Ver Legend."),
  d(Variable = "sector", Source = "PNADC (VD4010)", PT = "Setor de atividade em 5 grupos.", EN = "Activity sector in five groups.", Notes = "Efeito fixo. Ver Legend."),
  d(Variable = "occupation", Source = "PNADC (V4010)", PT = "Grande grupo ocupacional da COD (10 grupos).", EN = "COD occupational major group (ten groups).", Notes = "Primeiro dígito do código de 4 posições. Efeito fixo."),
  d(Variable = "position_grp", Source = "Derivada", PT = "Posição no mercado de trabalho, rotulada.", EN = "Labour market position, labelled.", Notes = "Base dos recortes de heterogeneidade."),
  d(Variable = "state", Source = "PNADC (UF)", PT = "Unidade da Federação.", EN = "Federation unit.", Notes = "Efeito fixo."),
  d(Variable = "w", Source = "PNADC (V1028)", PT = "Peso amostral da pessoa.", EN = "Person sampling weight.", Notes = "Toda estimativa do paper é ponderada.")
)
est <- attach_types(est, info_est, "analysis_sample.parquet")

# =============================================================================
# Files
# =============================================================================
fmt_n  <- function(x) if (is.null(x)) "--" else formatC(x, format = "d", big.mark = ",")
fmt_mb <- function(x) if (is.null(x)) "--" else sprintf("%.0f MB", x / 1024^2)

files <- d(
  File = c("main_data.parquet", "analysis_sample.parquet", "analysis_origins.parquet"),
  Rows = c(fmt_n(info_main$nrow), fmt_n(info_est$nrow), fmt_n(info_org$nrow)),
  Size = c(fmt_mb(info_main$size), fmt_mb(info_est$size), fmt_mb(info_org$size)),
  `Produced by` = c("build/code/12_build_main_data.R",
                    "analysis/code/01_prepare_analysis_data.R",
                    "analysis/code/01_prepare_analysis_data.R"),
  PT = c("Uma linha por trabalhador ocupado no trimestre t, pareado ou não ao t+1. Guarda o estado de destino completo e as variáveis brutas da PNADC.",
         "Amostra de estimação: apenas origens pareadas, com as variáveis de análise construídas. Lida por 02-06 e 08.",
         "Todas as origens, pareadas ou não. Lida por 07 para modelar a seleção na amostra de estimação."),
  EN = c("One row per worker employed in quarter t, matched into t+1 or not. Carries the full destination state and the raw PNADC variables.",
         "Estimation sample: matched origins only, with the analysis variables built. Read by 02-06 and 08.",
         "Every origin, matched or not. Read by 07 to model selection into the estimation sample."),
  check.names = FALSE
)

# =============================================================================
# Legend
# =============================================================================
legend <- rbind(
  d(Variable = "position / position_grp", Value = "3", Label = "Formal private employee / Empregado privado formal"),
  d(Variable = "", Value = "4", Label = "Informal private employee / Empregado privado informal"),
  d(Variable = "", Value = "5", Label = "Formal self-employed / Conta própria formal"),
  d(Variable = "", Value = "6", Label = "Informal self-employed / Conta própria informal"),
  d(Variable = "", Value = "7", Label = "Formal employer / Empregador formal"),
  d(Variable = "", Value = "8", Label = "Informal employer / Empregador informal"),
  d(Variable = "", Value = "9", Label = "Formal public sector / Setor público formal"),
  d(Variable = "", Value = "10", Label = "Informal public sector / Setor público informal"),
  d(Variable = "formal", Value = "1 / 0", Label = "Formal (position 3,5,7,9) / Informal (position 4,6,8,10)"),
  d(Variable = "dest_state", Value = "Employed", Label = "Ocupado em t+1 / Employed in t+1"),
  d(Variable = "", Value = "Unemployed", Label = "Desocupado em t+1 / Unemployed in t+1"),
  d(Variable = "", Value = "Out of labour force", Label = "Fora da força de trabalho em t+1 / Out of the labour force in t+1"),
  d(Variable = "V2010 / race5", Value = "1", Label = "Branca / White"),
  d(Variable = "", Value = "2", Label = "Preta / Black"),
  d(Variable = "", Value = "3", Label = "Amarela / Asian"),
  d(Variable = "", Value = "4", Label = "Parda / Brown"),
  d(Variable = "", Value = "5", Label = "Indígena / Indigenous"),
  d(Variable = "", Value = "9", Label = "Ignorado / Not reported"),
  d(Variable = "V4012", Value = "1", Label = "Trabalhador doméstico / Domestic worker"),
  d(Variable = "", Value = "2", Label = "Militar / Military"),
  d(Variable = "", Value = "3", Label = "Empregado do setor privado / Private sector employee"),
  d(Variable = "", Value = "4", Label = "Empregado do setor público / Public sector employee"),
  d(Variable = "", Value = "5", Label = "Empregador / Employer"),
  d(Variable = "", Value = "6", Label = "Conta própria / Own-account worker"),
  d(Variable = "", Value = "7", Label = "Trabalhador familiar auxiliar / Unpaid family worker"),
  d(Variable = "tenure (V4040)", Value = "1", Label = "Menos de 1 mês / Under 1 month"),
  d(Variable = "", Value = "2", Label = "1 a 11 meses / 1-11 months"),
  d(Variable = "", Value = "3", Label = "1 a 2 anos / 1-2 years"),
  d(Variable = "", Value = "4", Label = "2 anos ou mais / 2 years or more"),
  d(Variable = "sector (VD4010)", Value = "1", Label = "Agricultura / Agriculture"),
  d(Variable = "", Value = "2", Label = "Indústria / Industry"),
  d(Variable = "", Value = "3", Label = "Construção / Construction"),
  d(Variable = "", Value = "4", Label = "Comércio / Trade"),
  d(Variable = "", Value = "5 a 12", Label = "Serviços (12 grupos colapsados em um) / Services"),
  d(Variable = "occupation (V4010)", Value = "0", Label = "Forças armadas, policiais e bombeiros militares / Armed forces, police and military firefighters"),
  d(Variable = "", Value = "1", Label = "Dirigentes e gerentes / Managers"),
  d(Variable = "", Value = "2", Label = "Profissionais das ciências e intelectuais / Professionals"),
  d(Variable = "", Value = "3", Label = "Técnicos de nível médio / Technicians and associate professionals"),
  d(Variable = "", Value = "4", Label = "Apoio administrativo / Clerical support workers"),
  d(Variable = "", Value = "5", Label = "Serviços e vendedores / Service and sales workers"),
  d(Variable = "", Value = "6", Label = "Agropecuária qualificada / Skilled agricultural, forestry and fishery workers"),
  d(Variable = "", Value = "7", Label = "Operários e artesãos / Craft and related trades workers"),
  d(Variable = "", Value = "8", Label = "Operadores de instalações e máquinas / Plant and machine operators"),
  d(Variable = "", Value = "9", Label = "Ocupações elementares / Elementary occupations"),
  d(Variable = "VD4001", Value = "1 / 2", Label = "Na força de trabalho / Fora da força de trabalho"),
  d(Variable = "VD4002", Value = "1 / 2", Label = "Ocupado / Desocupado"),
  d(Variable = "VD3004", Value = "7", Label = "Superior completo (definição de college) / Completed tertiary")
)

# =============================================================================
# Provenance: how the data were built and the measurement decisions
# =============================================================================
prov <- d(
  Topic = c(
    "Fonte", "Identificação do painel", "Construção",
    "Janela amostral", "Definição do desfecho",
    "Formalidade dos empregadores", "Ids ambíguos",
    "Não pareados", "Ocupação", "Pesos", "Versão anterior"
  ),
  Description = c(
    "PNAD Contínua trimestral do IBGE, microdados públicos, 2012Q1 a 2026Q1. Baixados por build/code/10_download_pnadc_quarters.R.",
    "datazoom.social, build_pnadc_panel(panel = \"advanced_3\"), estágio 3: vínculo por domicílio, sexo e data de nascimento; doação de datas de nascimento entre entrevistas; e correspondência difusa por teoria dos grafos para sequências fragmentadas.",
    "Cada trimestre é baixado uma vez e cacheado; a identificação roda separadamente em cada um dos 13 grupos de rotação. Como um domicílio pertence a exatamente um grupo, isso é equivalente a rodar sobre o arquivo inteiro.",
    "Origens vão até 2024Q4. Trimestres posteriores são baixados apenas para não truncar a identificação dos grupos de rotação mais recentes.",
    "Saída do emprego = ocupado em t e não ocupado em t+1. Desagregada em E->U (desocupado) e E->N (fora da força), que somam ao total por construção.",
    "Formal se o empreendimento tem CNPJ (V4019). O IBGE só passou a coletar V4019 em meados de 2015, então para 2012Q1-2015Q1 usa-se a contribuição previdenciária (V4032). Sem esse fallback, 1,2% das pessoas-trimestre ocupadas ficariam sem formalidade e seriam descartadas em silêncio pelas regressões.",
    "Cerca de 0,01% das pessoas-trimestre recebem um id_rs3 que aparece duas vezes no mesmo trimestre, indicando que o passo de grafo fundiu duas pessoas. Esses ids são anulados: as linhas permanecem na população de origens, mas contam como não rastreáveis.",
    "Origens que o algoritmo não encontra em t+1 são mantidas em analysis_origins.parquet com matched_next = 0 e desfecho ausente. São a base da análise de atrito.",
    "Classificação de Ocupações para Pesquisas Domiciliares (COD) do IBGE, código de 4 dígitos em V4010; o primeiro dígito é o grande grupo.",
    "Todas as estatísticas e regressões usam o peso pessoal V1028. Nenhum resultado do paper é não ponderado, exceto a linha de robustez que declara sê-lo.",
    "A versão anterior do paper foi construída com o pareamento clássico por domicílio e data de nascimento, colapsava o destino em t+1 num único estado e descartava os não pareados. Está preservada em build/code/legacy/ e no arquivo main_data.dta."
  )
)

# =============================================================================
# Write
# =============================================================================
hdr <- createStyle(textDecoration = "bold", fgFill = "#E8E8E8",
                   halign = "left", valign = "top", border = "TopBottomLeftRight",
                   borderColour = "#999999", fontName = "Arial", fontSize = 10)
body <- createStyle(wrapText = TRUE, valign = "top",
                    fontName = "Arial", fontSize = 10)

wb <- createWorkbook()
modifyBaseFont(wb, fontName = "Arial", fontSize = 10)

add_sheet <- function(name, df, widths) {
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = hdr)
  addStyle(wb, name, body, rows = 2:(nrow(df) + 1), cols = seq_along(df),
           gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, name, cols = seq_along(df), widths = widths)
  freezePane(wb, name, firstActiveRow = 2)
}

add_sheet("Files", files, c(26, 12, 10, 40, 70, 70))
add_sheet("main_data", main[, c("Variable", "Source", "Type", "PT", "EN", "Notes")],
          c(22, 18, 12, 70, 70, 55))
add_sheet("analysis_sample", est[, c("Variable", "Source", "Type", "PT", "EN", "Notes")],
          c(22, 20, 12, 70, 70, 55))
add_sheet("Legend", legend, c(24, 14, 85))
add_sheet("Provenance", prov, c(28, 120))

saveWorkbook(wb, OUT, overwrite = TRUE)
msg("wrote ", OUT, " (", round(file.size(OUT) / 1024, 1), " KB, ",
    length(names(wb)), " sheets)")
