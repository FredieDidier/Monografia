# Verificação das citações — `latex/paper.tex` e `latex/refs.bib`

**Data desta verificação: 18/09/2026.** **Atualizado no mesmo dia:** as
pendências da §5 foram resolvidas pelo Fredie (itens 1, 2 e 4) e o
`paper_anpec.tex` foi corrigido; ver §7. Primeira rodada do projeto. Confere
**existência, metadado e conteúdo** de cada citação: para cada `\cite` do
manuscrito, a frase em volta foi lida contra o abstract (ou o texto completo)
da obra, não só contra a ficha bibliográfica. Método copiado da terceira rodada
do WorldCupHealth (06/09/2026).

**Escopo.** `latex/paper.tex` (a fonte de verdade): **33 comandos de citação,
48 chaves distintas**. `latex/refs.bib`: 50 entradas. Nenhuma tabela ou figura
gerada em `analysis/output/` carrega `\cite`, então o manuscrito é a única
superfície. ✅ `latex/paper_anpec.tex` (versão ANPEC, commit `576a794`)
recebeu as mesmas correções na mesma passada (§7); compila em 20 páginas
identificado e 19 cego, como antes.

---

## 1. Resumo

- **Estrutura:** 48 chaves citadas, todas no `.bib`. Duas entradas órfãs,
  `Berge2018` e `Rcore2024` (software), nunca citadas; o `elsarticle-harv` só
  imprime o que é citado, então não aparecem no PDF. Inofensivo; podem sair.
- **Metadado:** 46 entradas resolvidas contra Crossref (por DOI quando havia,
  por título quando não), OpenAlex, RePEc e páginas de editora; `MTE2020` e
  `Rcore2024` não são indexáveis. **Uma entrada fabricada por atribuição
  (`Neri2020`), corrigida.** Duas divergências registradas na §4.
- **Conteúdo:** **um erro de sinal (Maurizio), uma omissão de escopo
  (Beuermann), um número errado (Pew/BLS), uma quimera (Neri), quatro
  descrições mais fortes que a obra (Lopez, Ganong–Marinescu, Alon, OCDE), uma
  citação no lugar errado (Cajner).** Tudo corrigido em `paper.tex`.
- **Direção inversa** (substantivos órfãos em listas antes de `\citep`
  múltiplo): contadas as quatro listas coordenadas do texto; nenhuma tem mais
  substantivos que citações.
- **Build após as correções:** 0 citações indefinidas, 0 referências
  indefinidas, 0 overfull hbox, **46 páginas** (eram 45).

---

## 2. O que foi encontrado e corrigido

| chave / sítio | o manuscrito dizia | a obra é | correção |
|---|---|---|---|
| `Neri2020` (2 sítios: cobertura do AE; 68 milhões) | Neri, Marcelo (2021), *Auxílio Emergencial: Lessons from the Brazilian experience responding to COVID-19*, World Bank | O título, a instituição e o ano são do relatório principal do Banco Mundial de **Lara de Arruda, Lazarotto de Andrade, Falcão, Teixeira Barbosa e Morgandi (2021)**, DOI 10.1596/37254. Marcelo Neri não é autor de nada com esse título | Entrada substituída por `Arruda2021` com os autores reais; os dois sítios agora citam `Arruda2021, Arruda2022` |
| `Maurizio2023`, sítio da introdução | "operou principalmente via maiores taxas de saída, não menor entrada", para os seis países | Texto completo (Europe PMC, PMC10189224): *"the increased exit rate was the main driver of the employment drop, **apart from in Brazil**"*; *"Exit rates for both informal and formal occupations increased significantly in 2020, **except in Brazil**"*; *"Brazil experienced a **decrease** in exit rates from both formal and informal jobs"*. Informalidade respondeu por **55% (Brasil) a 85%** da queda total | Frase reescrita: cinco dos seis países via saída, **Brasil a exceção, com saídas em queda** — o que corrobora a queda das saídas que este paper encontra no meio da pandemia; "main cause" virou "accounted for most of the overall contraction (55% of it in Brazil)" |
| `Beuermann2024` | "additional schooling did reduce the probability of losing a job" | O efeito é para **mulheres**: *"females (but not males) who score just above the admission threshold..."* | "find that, **for women**, additional schooling did reduce..." |
| `Pew2020, Montenovo2022, Cajner2020` | "at most a high school diploma rose by **11** pp ... **three times** the increase for college graduates" | BLS/CPS, fev→mai 2020, 25+, dessazonalizado: ensino médio sem faculdade 3,7→15,9 (**+12,2 pp**); bacharelado ou mais 1,9→7,4 (+5,5 pp): razão **2,2**, não 3. A nota da Pew dá 7,2% em maio para graduados e não dá a variação por grupo. `Cajner2020` é por **faixa salarial** (folha de pagamento ADP), não por escolaridade | "high school diploma and no college rose by 12 percentage points ... **more than twice**", com nota de rodapé dando as séries do BLS; `Cajner2020` movido para a frase "employment losses by wage level", ao lado de `Chetty2024` |
| `Prassl2020, Albanesi2021, Aum2021` | "distributed very unequally across skill levels" | Albanesi–Kim é por **ocupação, gênero e família**, não por escolaridade | "across skill levels **and occupations**" |
| `Forsythe2020` | "vacancy postings and separations" | O paper usa *vacancy postings and UI claims* | "vacancy postings and unemployment claims" |
| `Lopez2023` | "raised incomes substantially at the bottom of the distribution and changed labour supply behaviour, including hours worked" | Lopez, Malde e McQuillin acham **redução de horas** e efeitos sobre crenças; não estimam renda | Renda passou para `Arruda2021, Andrade2025` (redução de pobreza e desigualdade); `Lopez2023` fica só com horas |
| `Ganong2020, Marinescu2021` | "illustrates how strongly such programmes can alter **measured transitions**" | Ganong et al.: 76% dos elegíveis com taxa de reposição **acima de 100%**; Marinescu et al.: benefícios 10% maiores reduzem candidaturas 3,6% **sem reduzir vagas** nem emprego | Cada obra citada pelo que mostra; "alter labour-market behaviour" |
| `Alon2020` | "changed labour supply of **graduates** with children" | É sobre **mães** (e pais), fechamento de escolas e creches | "of parents, and of mothers in particular" |
| `Davis1987, Brainard1993` | "education directs workers ... into activities less exposed to reallocation shocks" | Nenhum dos dois é sobre educação: Davis (perturbações alocativas e capital específico) e Brainard–Cutler (~40% do desemprego agregado é realocação) estabelecem que choques de realocação geram desemprego | "the reallocation shocks **that account for part of cyclical unemployment**" — a citação passa a sustentar a parte da frase que é dela |
| OECD (2023), nota de rodapé | "unemployment among **workers** with tertiary education averages 4--5%, against 12.8%" | EAG 2023, indicador A3: **25–34 anos**, terciário **4,9%**, sem ensino médio 12,8% | Faixa etária acrescentada, 4,9% em vez de "4--5%", indicador nomeado |
| `Andrade2025` | sem `number` | *REN* v. 56 **n. 2** (2025), 64--83 | `number = {2}` |

---

## 3. O que passou intacto — não precisa ser refeito

**Conteúdo conferido contra o abstract ou o texto:** Maurizio et al. (2023) no
sítio de Resultados (*"inactivity was the most likely destination"*, 68% no
Brasil — exato); Bottan–Hoffmann–Vera-Cossio (17 países, exposição desigual);
Giupponi–Landais (mantém emprego em choques temporários, custo de realocação em
choques persistentes — exato); Cameron–Gelbach–Miller 2011 (two-way);
Abadie–Athey–Imbens–Wooldridge (agrupar quando a amostragem é agrupada);
Montiel Olea–Plagborg-Møller (banda sup-*t* por bootstrap);
Cameron–Gelbach–Miller 2008 e Roodman et al. (wild cluster bootstrap, poucos
clusters, Rademacher com nulo imposto); Kitagawa (padronização),
Oaxaca e Blinder (análogo de regressão), Fortin–Lemieux–Firpo (revisão);
Ribas–Soares (regra domicílio × sexo × data de nascimento com tolerância a erro
de registro); Dingel–Neiman (viabilidade de trabalho remoto); Barrero–Bloom–Davis;
La Porta–Shleifer, Meghir–Narita–Robin, Ulyssea 2018 e Dix-Carneiro et al.
(seleção de firmas, salários, resposta do emprego a choques: três substantivos,
quatro citações); Adams-Prassl et al. e Aum–Lee–Shin (menos escolarizados mais
atingidos); Montenovo et al. (quedas maiores para ensino médio e alguma
faculdade); Chetty–Friedman–Stepner (perdas por faixa salarial); Farber 2005 e
Hoynes–Miller–Schaller (menos escolarizados sofrem mais no ciclo, explicado por
exposição setorial); Ben-Porath e Mincer (capital humano e produtividade);
Arruda et al. 2022 (nota técnica 4, cobertura e grupos vulneráveis); Andrade–Souza
(perfil dos beneficiários); Salto–Couri–Pellegrini (RAF 50, IFI, 22/03/2021);
MTE (página do BEm responde 200 em 18/09/2026).

**Metadado conferido contra índice ou editora, sem divergência:** Albanesi2021,
Prassl2020, Beuermann2024, Montenovo2022, BenPorath1967, Lise2020, Davis1987,
Brainard1993, Farber2005, Hoynes2012, Alon2020, Bottan2020, Aum2021, Dingel2020,
Lopez2023, Ganong2020, Marinescu2021, Cajner2020, Giupponi2023, Maurizio2023,
Ulyssea2020, GerardGonzaga2021, Cameron2011, Cameron2008, Roodman2019,
MontielOlea2019, Blinder1973, Kitagawa1955, Oaxaca1973, Ulyssea2018, Meghir2015,
LaPorta2014, DixCarneiro2026, Abadie2023, Fortin2011, Chetty2024, Forsythe2020,
Autor2003, Mincer1974, Pew2020 (Kochhar, 11/06/2020), Ribas2008 (IPEA TD 1348),
Parente2024 (IMF WP 2024/159), Arruda2022, Salto2021, Andrade2025.

---

## 4. Divergências registradas — não "corrigir"

- **`Barrero2023`**: o Crossref deposita `23-49`; a página da AEA diz **pp.
  23–50**. Mantido 23–50, que é a forma da editora.
- **`Andrade2025`**: o Crossref traz data de emissão 28/07/2023 para um DOI
  `ren.2025.1629`; a página da revista diz **v. 56 n. 2 (2025)**. Mantido 2025.
- **`Abadie2023`**: Crossref dá 2022 (online first); a versão impressa é *QJE*
  138(1), 2023. Mantido 2023.
- **`Salto2021`**: o autor corporativo é a IFI e a biblioteca do Senado lista dez
  "outros autores"; o `.bib` traz os três diretores (Salto, Couri, Pellegrini),
  que abrem a lista. Mantido.
- **`Farber2005`**: *Economic Perspectives* é a revista do Federal Reserve Bank
  of Chicago, vol. 29(Q II); não está no Crossref, confirmado no RePEc.

---

## 5. Deixado como está, e por quê — decisões para o Fredie

1. **`Ulyssea2018, Ulyssea2020, Parente2024`** para *"schooling and formality are
   strongly correlated"* e **`Ulyssea2020, GerardGonzaga2021`** para
   *"informality is far more common among non-graduates"*. Nenhum desses
   papers tem essa correlação como objeto: Parente é salário mínimo e
   desigualdade, Gerard–Gonzaga é seguro-desemprego, Ulyssea 2018 é um modelo
   de firmas. A revisão de Ulyssea 2020 registra o fato e a Tabela descritiva
   do próprio paper o mostra. **Sugestão:** deixar só `Ulyssea2020` nos dois
   sítios (`GerardGonzaga2021` só é citado ali e sairia do `.bib`; `Parente2024`
   também). Não cortei porque remove entradas.
2. **`Lise2020`** para *"human capital ... lowers displacement risk"*: o paper é
   sobre acumulação e depreciação de habilidades multidimensionais; está na
   família, mas o objeto não é risco de desligamento. Aceitável como está.
3. **`Autor2003`** para *"tasks more compatible with remote work"*: é a origem
   da abordagem por tarefas, não sobre trabalho remoto; Dingel–Neiman e
   Barrero et al. carregam a frase. Aceitável como está.
4. `Berge2018` e `Rcore2024` órfãos no `.bib`. Se `fixest` e R forem
   citados na seção de dados, entram; senão, podem sair.

---

## 6. Armadilhas de método desta rodada, para a próxima

- **A busca por título no Crossref devolveu obra errada em 3 de 46** —
  `Pew2020` casou com uma resenha de "Kochhar, Anjali", `Neri2020` com "Neri
  Nobre, Luciana", `Berge2018` com um paper de sísmica — e **working paper em
  vez de artigo em 8** (NBER/SSRN). O limiar de similaridade de título (0,72)
  pegou os três primeiros; os oito foram reconferidos com filtro de periódico.
  Casar e conferir o casamento continuam sendo dois passos.
- **O parser do `.bib` quebrou em entradas que fecham com `}}` na mesma
  linha** (`Barrero2023`, `Salto2021`): absorveram os campos da entrada
  seguinte e "resolveram" com o DOI errado. Foi o DOI resolvido diretamente
  que expôs o problema. Um `.bib` com uma entrada por bloco `\n}` evita isso.
- **O abstract não bastou para o Maurizio.** O abstract diz *"significant
  increase in exit rates"* para os seis países; a exceção brasileira só está no
  corpo. Quando a obra é o antecedente mais próximo do resultado, ler o texto.
- **Editoras que bloqueiam robô:** Springer, IMF, OCDE e OKR do Banco Mundial
  devolveram 403 ou HTML de verificação. Rotas que funcionaram: Europe PMC
  (`fullTextXML` pelo PMCID), a API `search.worldbank.org/api/v3/wds`, o
  RePEc, e o painel de navegador embutido para a biblioteca do Senado e a OCDE.

---

## 7. Resolução das pendências, 18/09/2026 (mesma sessão)

- **`Parente2024` e `GerardGonzaga2021` saíram do `.bib`** e foram substituídos
  nos dois sítios por obras cujo objeto é a correlação escolaridade–formalidade,
  conferidas na fonte:
  - **Haanwinckel e Soares (2021)**, *Review of Economic Studies* 88(6),
    2970--3010, DOI 10.1093/restud/rdab017: modelo estimado com dados do Brasil
    de 2003; *"changes in workforce composition appear as the main drivers of the
    reduction in informality"* entre 2003 e 2012 — a composição é escolaridade.
    Cita-se nos dois sítios.
  - **Gasparini e Tornarolli (2009)**, *Desarrollo y Sociedad* 63, 13--80, DOI
    10.13043/dys.63.1: mais de 100 pesquisas domiciliares da América Latina,
    Brasil incluído; texto (CEDLAS WP 46): *"The probability of being informal is
    decreasing in the worker's education (table 3.3)"*. Cita-se no sítio
    "schooling and formality are strongly correlated".
  - `Ulyssea2018` saiu desse mesmo sítio (modelo de firmas sem escolaridade do
    trabalhador); continua citado na frase sobre seleção de firmas.
- **`Berge2018` e `Rcore2024` removidos** do `.bib` (órfãos).
- **`paper_anpec.tex`** recebeu todas as correções da §2, adaptadas à redação
  dele (que difere da do `paper.tex` em várias frases). ⚠️ A versão submetida à
  ANPEC é a do commit `576a794`; esta é a que vale se houver recompilação.
- **Três papers do Gustavo Gonzaga entraram**, a pedido do Fredie, cada um
  onde a frase é dele e com metadado conferido no Crossref:
  - **Gonzaga (2003)**, *Economía* 4(1), 165--222: *"impressively high job and
    worker turnover rates"* no Brasil, com dados da PME — citado na frase da
    Seção de Dados sobre a taxa de saída bruta de \RawExit\% por trimestre.
  - **Gerard e Gonzaga (2021)**, *AEJ: Economic Policy* 13(3), 167--206:
    informalidade e o custo de eficiência do seguro-desemprego — citado na
    lista da introdução, que ganhou o substantivo "the cost of social
    insurance".
  - **Engbom, Gonzaga, Moser e Olivieri (2022)**, *Quantitative Economics*
    13(4), 1405--1446: penalidade e volatilidade de ganhos no setor informal,
    *"informal employment is an imperfect insurance mechanism"* — mesma lista,
    substantivo "earnings dynamics". A lista ficou com 5 substantivos para 6
    citações.
- Build final: `paper.tex` **46 páginas**, 0 indefinidas, **1 overfull hbox
  pré-existente** (6,2 pt em `tab_decomp_variants.tex`, tabela gerada; está
  igual no commit `ba8f6f9` e não é desta rodada). `paper_anpec.tex` **21
  páginas** — ⚠️ uma acima do limite de 20 que o próprio arquivo declara; a
  página 21 são 11 linhas de referências. `paper_anpec_blind.tex` (a versão que
  o comitê lê) fecha em **20**. Ambos 0 indefinidas, 0 overfull.
- `.bib` final: **51 entradas, 51 citadas**, nenhuma órfã, nenhuma ausente.
