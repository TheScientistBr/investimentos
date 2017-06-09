Introdução
----------

O objetivo deste projeto é dizer qual a probabilidade de um cliente passar com sucesso para análise de crédito dado que ele foi pré-aprovado para o empréstimo com garantia de automóvel.

Neste estudo de caso, vamos tentar discutir algumas maneiras de prever a chance de um cliente específico, que quer candidatar-se a um empréstimo, será padrão em seus pagamentos (ou não).

Em primeiro lugar, "prever" algo é uma expressão realmente estranha, não é? É provável que as pessoas comuns liguem a "previsão" com o relatório meteorológico diário - e todos sabemos que esse tipo de previsão não é confiável o tempo todo.

Então, por que fazer o esforço tentar olhar para o futuro para estimar a probabilidade de inadimplência? Por que os bancos simplesmente não dão a alguém que precise de algum dinheiro sabendo que eles não podem fazer nada para reduzir os mal devedores?

É claro que não é assim que isso funciona - a maneira mais intuitiva (e ainda muito popular) é que um oficial de empréstimo qualificado e experiente faça o melhor para decidir se um candidato é aprovado - ou não.

### Carregando o Dataset

A carga do dataset no formato `csv` deve passar por uma transformação inicial quanto a coluna `bi_lead_id` simplesmente por simples inconsistência quanto a coerção. Transformamos em caractere.

``` r
df <- read.csv("data/auto_refi_loan_data.csv",encoding = "UTF-8", stringsAsFactors = FALSE) 
head(df[,1:6])
```

    ##   X  bi_lead_id age monthly_income collateral_value loan_amount
    ## 1 1 1.00001e+11  61           1300               NA       10000
    ## 2 2 1.73466e+05  50           3500            40000       23936
    ## 3 3 1.73465e+05  31           2600            18000        5000
    ## 4 4 1.73464e+05  51           3600            21000       10000
    ## 5 5 1.73463e+05  51           1500            10000        8000
    ## 6 6 1.73462e+05  25           4000            39000       10000

**Estrutura do Dataset**

A coluna de descrição foi truncada para melhor visualização

``` r
structure <- read.csv("data/dataset_description.csv", header = FALSE, col.names = c("coluna","descricao"),encoding = "UTF-8")
structure$descricao <- strtrim(structure$descricao,width = 45)
structure
```

    ##                      coluna                                     descricao
    ## 1                bi_lead_id     Chave única de uma solicitação de cliente
    ## 2                       age                             Idade do cliente 
    ## 3            monthly_income Renda mensal informada pelo cliente no moment
    ## 4          collateral_value  Valor do automóvel que será dado em garantia
    ## 5               loan_amount Valor solicitado pelo cliente para empréstimo
    ## 6                      city                             Cidade do cliente
    ## 7                     state                             Estado do cliente
    ## 8    collateral_debt_amount Valor que o automovel do cliente tem de dívid
    ## 9        serasa_restriction Indica se o cliente possui alguma pendência n
    ## 10 serasa_dishonored_checks Indica se o cliente possui cheques sem fundo 
    ## 11     serasa_expired_debts Indica se o cliente possui dívidas vencidas (
    ## 12     serasa_banking_debts Indica se o cliente possui divídas bancárias 
    ## 13  serasa_commercial_debts Indica se o cliente possui dividas comerciais
    ## 14          serasa_protests Indica se o cliente possui protestos (dado da
    ## 15           marital_status                                 Estado civil 
    ## 16          cpf_restriction     Restrição no cpf (informado pelo cliente)
    ## 17                loan_term                           Prazo do empréstimo
    ## 18          monthly_payment                Pagamento mensal do empréstimo
    ## 19                  purpose Motivo pelo qual o cliente deseja o empréstim
    ## 20               auto_brand                                Marca do carro
    ## 21               auto_model                               Modelo do carro
    ## 22          auto_model_year                                  Ano do carro
    ## 23             covered_lead Lead pré-aprovado (apenas leads pré-aprovados
    ## 24           form_completed         Ficha cadastral completa pelo cliente
    ## 25         sent_to_analysis               Enviado para análise de crédito
    ## 26           discard_reason Motivo de descarte de um lead (leads pré-apro
    ## 27                  channel                      Canal de entrada do lead
    ## 28         created_at_safra                       Data de criação do lead
    ## 29                 zip_code                                CEP do cliente
    ## 30             landing_page  Página inicial que o cliente acessou no site
    ## 31     landing_page_product          Produto da página inicial do cliente
    ## 32                   gender                             Gênero do cliente
    ## 33          education_level                  Grau de instrução do cliente
    ## 34                 utm_term Tipo de dispositivo do cliente (c = computer,

Limpeza e transformação dos dados
---------------------------------

Algumas ações são necessárias, como por exemplo, a limpeza e transformação de algumas variáveis categóricas para inteiros ou numéricas de ponto flutuante. Também excluimos todas as observações cujo valor da variáel `covered_lead` seja igual a zero e os formulários incompletos.

``` r
library(gmodels)
df$bi_lead_id <- as.character(df$bi_lead_id) # adequando a variável (transofrmação)
ndf <- subset(df,covered_lead != "0")
ndf <- subset(ndf,form_completed != "0")
## trocando valores não disponíveis por zero nas colunas 
## collateral_debt_amount e monthly_payment
ndf$collateral_debt_amount[is.na(ndf$collateral_debt_amount)] <- 0 
ndf$monthly_payment[is.na(ndf$monthly_payment)] <- 0 
```

A coluna `purpose` é uma string com muitas váriações, o ideal é que o dado tivesse sido padronizado em sua coleta, já que isso aconteceu via portal na Internet. Vamos tentar melhorar com alguns ajustes. Vamos olhar um pequeno exemplo:

``` r
ndf$purpose[20:30]
```

    ##  [1] "NEGOCIAR DIVIDAS"             "Quitar dívidas"              
    ##  [3] "para pagar contas"            "Pagar dívidas "              
    ##  [5] "quitar dividas"               "Pagar dividas"               
    ##  [7] "Quitar empréstimo bancário "  "Para fins de saúde."         
    ##  [9] "Pagamento de dividas"         "Terminar a minha lanchonete "
    ## [11] "PAGAMENTO DÍVIDAS "

*quitar dividas* é um bom exemplo, vamos retirar os espaços duplos ou maiores, deixar todo o texto em caixa baixa e remover toda a pontuação do texto. Vamos remover as acentuações, já que alguns clientes colocam e outros não.

``` r
ndf$purpose <- factor(tolower(ndf$purpose))
ndf$purpose <- gsub("^ *|(?<= ) | *$", "", ndf$purpose, perl = TRUE)
rm_accent <- function(str,pattern="all") {
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)

  pattern <- unique(pattern)

  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"

  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )

  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )

  accentTypes <- c("´","`","^","~","¨","ç")

  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))

  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)

  return(str)
}

ndf$purpose <- rm_accent(ndf$purpose)
ndf$purpose[20:30]
```

    ##  [1] "negociar dividas"            "quitar dividas"             
    ##  [3] "para pagar contas"           "pagar dividas"              
    ##  [5] "quitar dividas"              "pagar dividas"              
    ##  [7] "quitar emprestimo bancario"  "para fins de saude."        
    ##  [9] "pagamento de dividas"        "terminar a minha lanchonete"
    ## [11] "pagamento dividas"

Agora podemos transformar essa coluna em numérica devido aos seus nívels (fatores). O mesmo para outras variáveis.

``` r
ndf$purpose <- as.factor(ndf$purpose)
ndf$purpose <- unclass(ndf$purpose)
ndf$education_level <- as.factor(ndf$education_level)
ndf$education_level <- unclass(ndf$education_level)

# transformando marital_status
ndf$marital_status <- as.integer(ndf$marital_status)
```

Vamos então retirar algumas colunas para um novo dataset para o processamento, como por exemplo a cidade onde o cliente mora, esses valores não interferem no resultado final.

``` r
credit <- ndf
credit[,c(1,2,7,8,17,18,21:25,27:33,35,36)] <- NULL
dim(credit)[1]
```

    ## [1] 4172

``` r
remove(ndf)
```

``` r
summary(credit[,c(1:5,13,14)])
```

    ##       age         monthly_income     collateral_value    loan_amount    
    ##  Min.   : 18.00   Min.   :     800   Min.   :      27   Min.   :  2500  
    ##  1st Qu.: 30.00   1st Qu.:    2800   1st Qu.:   18000   1st Qu.:  7000  
    ##  Median : 37.00   Median :    4000   Median :   24000   Median : 12310  
    ##  Mean   : 38.84   Mean   :   14541   Mean   :   53686   Mean   : 15550  
    ##  3rd Qu.: 46.00   3rd Qu.:    6700   3rd Qu.:   34000   3rd Qu.: 20000  
    ##  Max.   :115.00   Max.   :11000000   Max.   :27000000   Max.   :288000  
    ##  collateral_debt_amount monthly_payment      purpose      
    ##  Min.   :       0       Min.   :    0.0   Min.   :   1.0  
    ##  1st Qu.:       0       1st Qu.:  365.9   1st Qu.: 609.8  
    ##  Median :       0       Median :  580.6   Median :1105.0  
    ##  Mean   :    4880       Mean   :  725.6   Mean   :1188.9  
    ##  3rd Qu.:      10       3rd Qu.:  891.1   3rd Qu.:1888.2  
    ##  Max.   :10100000       Max.   :12000.0   Max.   :2471.0

Nesta fase vamos apenas entender se existe valores do tipo outliers. Não vamos avaliar as medidas de tendência central da coluna `sent_to_analysis`. Observando os valores acima é fácil perceber que alguns clientes digitaram informações aparentemente inconsistentes, como por exemplo, o `monthly_payment`. Valores zero e outros muito acima da média, o que podemos considerar outliers. O mesmo vale para a idade. O mesmo ocorre com `loan_term`, vamos retirar esses valores faltantes.

**Normalizando as variáveis**

A solução abaixo permite escalar apenas nomes de variáveis específicas, preservando outras variáveis inalteradas (e os nomes das variáveis podem ser gerados dinamicamente), mas optamos aqui por seu indice específico.

``` r
standValue <- function(x){(x-min(x))/(max(x)-min(x))}
credit$monthly_income <- standValue(credit$monthly_income)
credit$collateral_value <- standValue(credit$collateral_value)
credit$loan_amount <- standValue(credit$loan_amount)
credit$collateral_debt_amount <- standValue(credit$collateral_debt_amount)
credit$monthly_payment <- standValue(credit$monthly_payment)
credit$purpose <- standValue(credit$purpose)
credit$age <- standValue(credit$age)
credit$education_level <- standValue(credit$education_level)
credit$marital_status <- standValue(credit$marital_status)

summary(credit[,c(1:5,13,14)])
```

    ##       age         monthly_income      collateral_value   
    ##  Min.   :0.0000   Min.   :0.0000000   Min.   :0.0000000  
    ##  1st Qu.:0.1237   1st Qu.:0.0001818   1st Qu.:0.0006657  
    ##  Median :0.1959   Median :0.0002909   Median :0.0008879  
    ##  Mean   :0.2148   Mean   :0.0012493   Mean   :0.0019874  
    ##  3rd Qu.:0.2887   3rd Qu.:0.0005364   3rd Qu.:0.0012583  
    ##  Max.   :1.0000   Max.   :1.0000000   Max.   :1.0000000  
    ##   loan_amount      collateral_debt_amount monthly_payment  
    ##  Min.   :0.00000   Min.   :0.0000000      Min.   :0.00000  
    ##  1st Qu.:0.01576   1st Qu.:0.0000000      1st Qu.:0.03049  
    ##  Median :0.03436   Median :0.0000000      Median :0.04838  
    ##  Mean   :0.04571   Mean   :0.0004832      Mean   :0.06046  
    ##  3rd Qu.:0.06130   3rd Qu.:0.0000010      3rd Qu.:0.07426  
    ##  Max.   :1.00000   Max.   :1.0000000      Max.   :1.00000  
    ##     purpose      
    ##  Min.   :0.0000  
    ##  1st Qu.:0.2465  
    ##  Median :0.4470  
    ##  Mean   :0.4809  
    ##  3rd Qu.:0.7641  
    ##  Max.   :1.0000

Agora com os dados normalizados passamos para a próxima fase.

### Fase exploratória

Agora vamos identificar os clientes que já foram enviados para análise de crédito. Esses clientes serão usados para testar o modelo.

``` r
dim(subset(credit,credit$sent_to_analysis > 0))[1]
```

    ## [1] 1502

Observa-se que apenas 4172 clientes que não tem restrições no Serasa, sendo que desses 1502 ainda não foram enviados para a análise de crédito.

### Aprimorando a limpeza e tratamento dos dados

Registros sem valores de Pagamento mensal do empréstimo são retirados.

**monthly\_payment**

``` r
head(credit[order(credit$monthly_payment,decreasing = TRUE),c(1:4)])
```

    ##             age monthly_income collateral_value loan_amount
    ## 2816  0.2371134    0.003563896     0.0166656881   1.0000000
    ## 11054 0.2577320    0.009018838     0.0062953074   0.5271454
    ## 8632  0.3505155    0.002427449     0.0079619757   0.6690018
    ## 22595 0.3092784    0.001745581     0.0021841922   0.1772329
    ## 25826 0.2268041    0.000381846     0.0009249317   0.0262697
    ## 34693 0.2989691    0.001745581     0.0030568597   0.1663748

``` r
credit <- subset(credit,credit$monthly_payment != 0)
```

``` r
head(credit[order(credit$collateral_value,decreasing = TRUE),c(1:4)])
```

    ##              age monthly_income collateral_value loan_amount
    ## 33736 0.24742268   1.701033e-04        1.0000000 0.026269702
    ## 5507  0.05154639   2.909302e-04        0.9074073 0.026269702
    ## 22443 0.14432990   3.636628e-05        0.6666663 0.008756567
    ## 27610 0.19587629   2.909302e-04        0.6111107 0.008756567
    ## 2907  0.23711340   1.090988e-04        0.5926197 0.015761821
    ## 27593 0.43298969   8.364245e-04        0.2111103 0.230784799

Os valores acima ordenados por `collateral_value` mostram que o valor máximo é um outlier. o gráfico de dispersão confirma essa suspeita. Devemos observa que o segundo item da lista foi enviado para análise de crédito, então não podemos considerá-lo como outlier.

Comparando todas as variáveis com e sem os outliers

``` r
boxplot(credit[,c(1:5,13:14)])
```

![](README_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
boxplot(credit[,c(1:5,13:14)],outline = FALSE)
```

![](README_files/figure-markdown_github/unnamed-chunk-12-2.png)

``` r
par(mfrow = c(1,5))
plot(credit$age, ylab = structure[2,2], xlab = "Clientes")
plot(credit$collateral_value, ylab = structure[4,2], xlab = "Clientes")
plot(credit$collateral_debt_amount, ylab = structure[8,2], xlab = "Clientes")
plot(credit$monthly_payment, ylab = structure[18,2], xlab = "Clientes")
plot(credit$monthly_income, ylab = structure[3,2], xlab = "Clientes")
```

![](README_files/figure-markdown_github/unnamed-chunk-13-1.png)

Apesar dos valores estarem longe dos demais, há coerência entre `collateral_value`e `monthly_income`, pois os valores são muito distante dos demais.

Vamos então retirar todos os outliers para verificar o seu comportamento, mas antes vamos fazer uma cópia dos nossos dados.

``` r
## Função para remover outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

credit2 <- credit
credit$collateral_value <- remove_outliers(credit$collateral_value)
credit$collateral_debt_amount <- remove_outliers(credit$collateral_debt_amount)
credit$monthly_payment <- remove_outliers(credit$monthly_payment)
credit$monthly_income <- remove_outliers(credit$monthly_income)

par(mfrow = c(1,5))
plot(credit$age, ylab = structure[2,2], xlab = "Clientes")
plot(credit$collateral_value, ylab = structure[4,2], xlab = "Clientes")
plot(credit$collateral_debt_amount, ylab = structure[8,2], xlab = "Clientes")
plot(credit$monthly_payment, ylab = structure[18,2], xlab = "Clientes")
plot(credit$monthly_income, ylab = structure[3,2], xlab = "Clientes")
```

![](README_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
credit <- credit2
remove(credit2)
```

Agora temos uma dispersão sem outliers, mas isso é apenas para demonstrar, pois não vamos retirar esses valores de nosso modelo.

``` r
head(credit[order(credit$monthly_income,decreasing = TRUE),c(1:4)],10)
```

    ##              age monthly_income collateral_value loan_amount
    ## 3322  0.04123711      1.0000000     0.0005545609 0.038528897
    ## 10677 0.39175258      0.5737872     0.0004434497 0.029071804
    ## 22362 0.36082474      0.4872354     0.0009249317 0.070052539
    ## 2053  0.28865979      0.3817732     0.0004434497 0.029071804
    ## 29190 0.23711340      0.2272165     0.0012212283 0.095271454
    ## 33164 0.17525773      0.1635755     0.0005545609 0.026269702
    ## 33538 0.34020619      0.1635755     0.0008693761 0.008756567
    ## 28685 0.11340206      0.1363008     0.0016286361 0.129947461
    ## 28712 0.02061856      0.1181179     0.0009249317 0.008756567
    ## 15446 0.09278351      0.1090261     0.0002582643 0.008756567

O terceiro item da tabela acima já enviado para análise, então não vamos considerar esses elementos distantes como outliers.

Por último, os valores ausentes e zerados de `collateral_debt_amount` devem ser trocados por zero, pois o carro é o bem que será dado como garantias do emprestimo e esse valor é a dívida do automóvel.

``` r
credit[is.na(credit)] <- 0
```

Agora temos nosso banco de dados e nós sabemos como parece.

Método Um: Regressão Logística
------------------------------

O primeiro passo é criar nosso conjunto de dados de treinamento e nosso conjunto de dados de teste. O conjunto de teste é usado para avaliar a precisão do modelo.

Vamos criar vários modelos, por isso é necessário dar-lhes designações numéricas (1, 2, 3, etc.). Podemos quebrar os conjuntos de dados em todos os tamanhos que desejamos, até 50-50, mas aqui usamos uma divisão de um terço e dois terços, pois é o mais recomendado pela literatura.

Nesta fase, realizaremos uma regressão logística usando a função glm(). Começamos com o conjunto de práticas, i\_calibration1. Aqui, seremos seletivos com as variáveis que usamos no modelo. Vamos mudar isso um pouco, mas, por enquanto, usaremos apenas cinco para determinar o valor da credibilidade.

**OBS:** retiramos do modelo as variáveis `education_level` e `marital_status`, essas variáveis deixam o modelo instável e *uma predição a partir de um ajuste `rank-deficient` pode ser enganoso*.

``` r
i_test1 <- sort(sample(1:nrow(credit), size = dim(credit)[1]*.7))

i_calibration1 <- (1:nrow(credit))[-i_test1]
set.seed(1)
LogisticModel.1 <- glm(sent_to_analysis ~ . -education_level -marital_status, 
                       family = poisson(link = "log"),data = credit[i_calibration1, ])
```

    ## Warning: glm.fit: fitted rates numerically 0 occurred

Com isso, podemos avançar para ajustar o modelo que acabamos de criar para o conjunto de testes, i\_test1, e nos preparar para fazer nossa primeira previsão.

``` r
fitLog1 <- predict(LogisticModel.1, type = 'response', newdata = credit[i_test1, ])
```

Nós montamos nosso modelo. Agora, usaremos o pacote ROCR para criar previsões e medir o desempenho em termos de área sob a curva (AUC). Quanto maior a medida da AUC, melhor será o nosso modelo.

``` r
library(ROCR)
pred1 <- prediction(fitLog1, credit$sent_to_analysis[i_test1])
perf1 <- performance(pred1, 'tpr', 'fpr')
plot(perf1,colorize=TRUE,print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
```

![](README_files/figure-markdown_github/unnamed-chunk-17-1.png)

E iremos envolver esta parte ao encontrar a AUC.

``` r
AUCLog1 <- performance(pred1, measure = 'auc')@y.values[[1]]
AUCLog1
```

    ## [1] 0.7500528

Esse não é um resultado ruim, mas vamos ver se podemos fazer melhor com um método diferente.

Método dois: árvore de regressão
--------------------------------

Em seguida, vamos tentar analisar os dados usando uma abordagem de árvore de regressão. Grande parte do nosso código é semelhante ao que foi usado nos modelos de logística acima, mas precisamos fazer alguns ajustes.

Observe novamente que estamos analisando todas as variáveis em nosso modelo para encontrar seu impacto em nossa variável de interesse, credibilidade (sent\_to\_analysis).

O pacote `rpart`é usado para Árvores de particionamento e regressão recursivas. Completando os recursos necessário, o pacote `rpart.plot`será usado para plotar o gráfico.

Temporáriamente a coluna `sent_to_analysis`será transofmrada em categórica, pois a biblioteca rpart usa dessa forma.

``` r
library(rpart)
set.seed(1)
credit$sent_to_analysis <- as.factor(credit$sent_to_analysis)
TreeModel <- rpart(sent_to_analysis ~ ., data = credit[i_calibration1, ])
library(rpart.plot)
prp(TreeModel, type = 2, extra = 1)
```

![](README_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
fitTree <- predict(TreeModel, newdata = credit[i_test1, ], type = 'prob')[, 2]
pred3 <- prediction(fitTree, credit$sent_to_analysis[i_test1])
perf3 <- performance(pred3, 'tpr', 'fpr')
plot(perf3,colorize=TRUE,print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
```

![](README_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
AUCTree <- performance(pred3, measure = 'auc')@y.values[[1]]
AUCTree
```

    ## [1] 0.7155061

O resultado foi pior do que o anterior. E ambos não são resultados satisfatórios, dada a complexidade do nosso modelo de árvore, então, novamente, temos que nos perguntar se não estamos melhor usando o modelo de Regressão Logística mais simples do primeiro modelo.

Método três: floresta aleatória (Random Forest)
-----------------------------------------------

Em vez de construir uma árvore de decisão, podemos usar o método Random Forest para criar uma "floresta" metafóra de árvores de decisão. Neste método, o resultado final é o modo das classes (se trabalharmos em um modelo de classificação) ou a média das previsões (se estiver trabalhando com regressões).

A idéia por trás da floresta aleatória é que as árvores de decisão são propensas a superação, então encontrar a árvore "média" na floresta pode ajudar a evitar esse problema.

``` r
library(randomForest)
set.seed(1)
RF <- randomForest(sent_to_analysis ~ ., data = credit[i_calibration1, ])
fitForest1 <- predict(RF, newdata = credit[i_test1, ], type = 'prob')[, 2]
pred4 <- prediction(fitForest1, credit$sent_to_analysis[i_test1])
perf4 <- performance(pred4, 'tpr', 'fpr')
plot(perf4,colorize=TRUE,print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
```

![](README_files/figure-markdown_github/unnamed-chunk-22-1.png)

Agora vamos avaliar o desempenho

``` r
AUCRF <- performance(pred4, measure = 'auc')@y.values[[1]]
AUCRF
```

    ## [1] 0.7496038

Com o esforço extra, ainda assim não obtemos um resultado um tanto melhorado. O modelo de Regressão logística é o melhor desempenho até o momento.

Método quarto: Comparando florestas aleatórias com modelos logísticos
---------------------------------------------------------------------

Analisamos vários resultados usando dois métodos básicos de análise - regressões logísticas e árvores de decisão. Verificamos apenas resultados individuais expressos como AUC.

A abordagem da floresta aleatória exige que criemos uma floresta de árvores de decisão e assumimos o modo ou a média. Por que não fazer uso de todos esses dados? Como eles se parecem?

O código a seguir cria um gráfico que representa centenas de combinações de pontuações AUC para cada árvore em nossa floresta aleatória e para modelos logísticos.

Primeiro precisamos de uma função para realizar a análise.

**OBS: para esse modelo vamos deixar todas as variáveis no modelo.**

``` r
AUC <- function(i){
        set.seed(i)
        i_test2 <<- sample(1:nrow(credit), size = dim(credit)[1]*.7)
        i_calibration2 <<- (1:nrow(credit))[-i_test2]
        # vamos criar esse modelo no ambiente global, pois vamos usa-lo mais na frente
        LogisticModel.3 <<- glm(sent_to_analysis ~ ., family = binomial, data = credit[i_calibration2, ])
        summary(LogisticModel.3)
        fitLog3 <- predict(LogisticModel.3, type = 'response', newdata = credit[i_test2, ])
        library(ROCR)
        pred5 <- prediction(fitLog3, credit$sent_to_analysis[i_test2])
        AUCLog3 <- performance(pred5, measure = 'auc')@y.values[[1]]
        RF <- randomForest(sent_to_analysis ~ ., data = credit[i_calibration2, ])
        fitForest2 <- predict(RF, newdata = credit[i_test2, ], type = 'prob')[, 2]
        pred6 <- prediction(fitForest2, credit$sent_to_analysis[i_test2])
        AUCRF <- performance(pred6, measure = 'auc')@y.values[[1]]
        return(c(AUCLog3, AUCRF))
}
```

Esta parte do código leva um tempo para ser executado porque estamos tabulando e gravando centenas de resultados individuais. Você pode ajustar o número de resultados no modelo alterando a contagem no objeto VAUC. Aqui, escolhemos calcular 50 x-y pares, ou 100 resultados individuais.

Normalmente `markdown` não é usado para relatórios longos ou demorados, mas para essa demanda o processo foi realizado aqui mesmo.

``` r
VAUC <- Vectorize(AUC)(1:100)
plot(t(VAUC), xlab = 'Logistic', ylab = 'Random Forest')
```

![](README_files/figure-markdown_github/VAUC-1.png)

É possível verificar que os resultados que temos dos primeiros quatro modelos estão bem no meio da distribuição.

Isso confirma para nós os modelos são bastante comparáveis. O melhor que podemos esperar é uma AUC de 0,77 e a maioria nos dá resultados semelhantes ao que já calculamos.

Mas vamos tentar visualizar um pouco melhor. Um par de pacotes R pode ser usado para melhorar a representação gráfica dos resultados. Isso nos mostrará exatamente onde o conjunto de resultados mais provável reside.

Primeiro, precisamos converter nosso objeto VAUC em um Data Frame.

``` r
AA <- as.data.frame(t(VAUC))
```

Vamos usar as bibliotecas `ggplot2` e `hdrcde` para criar alguns gráficos. O primeiro é um gráfico de contorno de densidade.

``` r
library(ggplot2)
data(AA, package = 'MASS')
ggplot(AA, aes(x = V1, y = V2)) + geom_point() + geom_density2d() + xlab('Logistic') + ylab('Random Forest')
```

![](README_files/figure-markdown_github/unnamed-chunk-26-1.png)

O segundo é um gráfico de contorno de alta densidade que nos dá as regiões de probabilidade para os dados.

``` r
library("hdrcde")
par(mar = c(3.1, 4.1, 1.1, 2.1))
with(AA, hdr.boxplot.2d(V1, V2, show.points = TRUE, prob = c(.01, .05, .5, .75), xlab = 'Logistic', ylab = 'Random Forest'))
```

![](README_files/figure-markdown_github/unnamed-chunk-27-1.png)

De qualquer maneira que descrevemos nossos resultados, temos que usar os dados para tomar uma decisão de empréstimo, há um problema aqui?

Estas podem ser as melhores pontuações que podemos encontrar com esses modelos, mas os resultados são aceitáveis para determinar o mérito do credor? Isso depende dos padrões de crédito utilizados pela instituição de crédito.

Na melhor das hipóteses, parece que nossos modelos nos dão 82% de chance de emprestar a bons riscos de crédito. Por cada R$ 1 milhão em empréstimos, na melhor das hipóteses, podemos esperar que seja reembolsado R$ 820.000. Em média, esperamos recuperar cerca de R$ 780 mil em principal. Em outras palavras, de acordo com nossa análise, há entre 75% e 80% de chance de recuperar nosso empréstimo de US R$ 1 milhão, dependendo do método de modelagem que usamos.

À medida que adicionamos candidatos de empréstimo a nossas bases de dados, gostaríamos que eles se agrupassem na área mais escura do gráfico de alta densidade se considerarmos bons riscos de crédito.

A menos que cobramos muito interesse para cobrir nossas perdas, talvez precisemos de melhores modelos.

Como há muitas possíveis variáveis para o modelo, podemos proceder com a abordagem de Stepwise para selecionar o modelo com a "melhor" combinação de variáveis explicativas:

### Outro teste (AIC)

O critério de informação Akaike (AIC) é uma medida da qualidade relativa dos modelos estatísticos para um dado conjunto de dados. Dada uma coleção de modelos para os dados, AIC estima a qualidade de cada modelo, em relação a cada um dos outros modelos. Assim, a AIC fornece um meio para a seleção do modelo.

O AIC é baseado na teoria da informação: oferece uma estimativa relativa da informação perdida quando um determinado modelo é usado para representar o processo que gera os dados. Ao fazê-lo, trata-se do trade-off entre a bondade do ajuste do modelo e a complexidade do modelo.

O Critério de Informação de Akaike (AIC) é definido como

*A**I**C*<sub>*p*</sub> = −2*l**o**g*(*L*<sub>*p*</sub>)+2\[(*p* + 1)+1\],

em que *L*<sub>*p*</sub> é a função de máxima verossimilhança do modelo e p é o número de variáveis explicativas consideradas no modelo.

O Critério de Informação Bayesiano (BIC) é definido como

*B**I**C*<sub>*p*</sub> = −2*l**o**g*(*L*<sub>*p*</sub>)+\[(*p* + 1)+1\]*l**o**g*(*n*).

Tanto o AIC quanto o BIC aumentam conforme SQE aumenta. Além disso, ambos critérios penalizam modelos com muitas variáveis sendo que valores menores de AIC e BIC são preferíveis.

Como modelos com mais variáveis tendem a produzir menor SQE mas usam mais parâmetros, a melhor escolha é balancear o ajuste com a quantidade de variáveis.

**Criando outro modelo para teste**

Mais uma vez vamos usar todas as variáveis no modelo.

``` r
#Índices obtidos após a aleatorização
ordena <- sort(sample(nrow(credit), size = dim(credit)[1]*.7))

#Dados para o treinamento
treinamento<-credit[ordena,]
#Dados para a validação
validacao<-credit[-ordena,]
#Regressão Logística
modelo.completo <- glm(sent_to_analysis ~ . ,family=binomial,data=treinamento)
```

Vamos fazer isso com o melhor modelo, mas antes temos que voltar aos dados originais (nao normalizados), pois estamos em busca de sucesso ou fracasso, então a distribuição típica é Poison.

Vamos usar a biblioteca MASS para calcular intervalos de confiança a partir de modelos de regressão logística.

``` r
library("MASS")
stepwise <- stepAIC(LogisticModel.3,direction="both")
```

    ## Start:  AIC=1398.45
    ## sent_to_analysis ~ age + monthly_income + collateral_value + 
    ##     loan_amount + collateral_debt_amount + serasa_restriction + 
    ##     serasa_dishonored_checks + serasa_expired_debts + serasa_banking_debts + 
    ##     serasa_commercial_debts + serasa_protests + marital_status + 
    ##     monthly_payment + purpose + education_level
    ## 
    ## 
    ## Step:  AIC=1398.45
    ## sent_to_analysis ~ age + monthly_income + collateral_value + 
    ##     loan_amount + collateral_debt_amount + serasa_restriction + 
    ##     serasa_dishonored_checks + serasa_expired_debts + serasa_banking_debts + 
    ##     serasa_commercial_debts + serasa_protests + marital_status + 
    ##     monthly_payment + purpose
    ## 
    ## 
    ## Step:  AIC=1398.45
    ## sent_to_analysis ~ age + monthly_income + collateral_value + 
    ##     loan_amount + collateral_debt_amount + serasa_restriction + 
    ##     serasa_dishonored_checks + serasa_expired_debts + serasa_banking_debts + 
    ##     serasa_commercial_debts + serasa_protests + monthly_payment + 
    ##     purpose
    ## 
    ##                            Df Deviance    AIC
    ## - loan_amount               1   1370.5 1396.5
    ## - monthly_income            1   1370.8 1396.8
    ## - serasa_protests           1   1370.9 1396.9
    ## - serasa_expired_debts      1   1371.1 1397.1
    ## - purpose                   1   1371.8 1397.8
    ## <none>                          1370.5 1398.5
    ## - serasa_commercial_debts   1   1373.2 1399.2
    ## - collateral_value          1   1373.9 1399.9
    ## - serasa_dishonored_checks  1   1375.7 1401.7
    ## - serasa_restriction        1   1375.8 1401.8
    ## - serasa_banking_debts      1   1378.2 1404.2
    ## - age                       1   1379.7 1405.7
    ## - monthly_payment           1   1390.5 1416.5
    ## - collateral_debt_amount    1   1444.2 1470.2
    ## 
    ## Step:  AIC=1396.53
    ## sent_to_analysis ~ age + monthly_income + collateral_value + 
    ##     collateral_debt_amount + serasa_restriction + serasa_dishonored_checks + 
    ##     serasa_expired_debts + serasa_banking_debts + serasa_commercial_debts + 
    ##     serasa_protests + monthly_payment + purpose
    ## 
    ##                            Df Deviance    AIC
    ## - monthly_income            1   1370.9 1394.9
    ## - serasa_protests           1   1371.0 1395.0
    ## - serasa_expired_debts      1   1371.1 1395.1
    ## - purpose                   1   1371.9 1395.9
    ## <none>                          1370.5 1396.5
    ## - serasa_commercial_debts   1   1373.2 1397.2
    ## + loan_amount               1   1370.5 1398.5
    ## - collateral_value          1   1374.6 1398.6
    ## - serasa_dishonored_checks  1   1375.8 1399.8
    ## - serasa_restriction        1   1376.0 1400.0
    ## - serasa_banking_debts      1   1378.3 1402.3
    ## - age                       1   1379.7 1403.7
    ## - monthly_payment           1   1420.4 1444.4
    ## - collateral_debt_amount    1   1444.7 1468.7
    ## 
    ## Step:  AIC=1394.91
    ## sent_to_analysis ~ age + collateral_value + collateral_debt_amount + 
    ##     serasa_restriction + serasa_dishonored_checks + serasa_expired_debts + 
    ##     serasa_banking_debts + serasa_commercial_debts + serasa_protests + 
    ##     monthly_payment + purpose
    ## 
    ##                            Df Deviance    AIC
    ## - serasa_protests           1   1371.3 1393.3
    ## - serasa_expired_debts      1   1371.5 1393.5
    ## - purpose                   1   1372.2 1394.2
    ## <none>                          1370.9 1394.9
    ## - serasa_commercial_debts   1   1373.6 1395.6
    ## + monthly_income            1   1370.5 1396.5
    ## + loan_amount               1   1370.8 1396.8
    ## - collateral_value          1   1374.9 1396.9
    ## - serasa_dishonored_checks  1   1376.2 1398.2
    ## - serasa_restriction        1   1376.3 1398.3
    ## - serasa_banking_debts      1   1378.7 1400.7
    ## - age                       1   1379.9 1401.9
    ## - monthly_payment           1   1420.8 1442.8
    ## - collateral_debt_amount    1   1445.5 1467.5
    ## 
    ## Step:  AIC=1393.33
    ## sent_to_analysis ~ age + collateral_value + collateral_debt_amount + 
    ##     serasa_restriction + serasa_dishonored_checks + serasa_expired_debts + 
    ##     serasa_banking_debts + serasa_commercial_debts + monthly_payment + 
    ##     purpose
    ## 
    ##                            Df Deviance    AIC
    ## - serasa_expired_debts      1   1371.9 1391.9
    ## - purpose                   1   1372.7 1392.7
    ## <none>                          1371.3 1393.3
    ## - serasa_commercial_debts   1   1373.7 1393.7
    ## + serasa_protests           1   1370.9 1394.9
    ## + monthly_income            1   1371.0 1395.0
    ## + loan_amount               1   1371.2 1395.2
    ## - collateral_value          1   1375.3 1395.3
    ## - serasa_dishonored_checks  1   1376.7 1396.7
    ## - serasa_banking_debts      1   1378.8 1398.8
    ## - serasa_restriction        1   1379.2 1399.2
    ## - age                       1   1380.2 1400.2
    ## - monthly_payment           1   1421.3 1441.3
    ## - collateral_debt_amount    1   1445.9 1465.9
    ## 
    ## Step:  AIC=1391.88
    ## sent_to_analysis ~ age + collateral_value + collateral_debt_amount + 
    ##     serasa_restriction + serasa_dishonored_checks + serasa_banking_debts + 
    ##     serasa_commercial_debts + monthly_payment + purpose
    ## 
    ##                            Df Deviance    AIC
    ## - purpose                   1   1373.3 1391.3
    ## <none>                          1371.9 1391.9
    ## - serasa_commercial_debts   1   1374.1 1392.1
    ## + serasa_expired_debts      1   1371.3 1393.3
    ## + monthly_income            1   1371.5 1393.5
    ## + serasa_protests           1   1371.5 1393.5
    ## + loan_amount               1   1371.8 1393.8
    ## - collateral_value          1   1375.9 1393.9
    ## - serasa_dishonored_checks  1   1377.2 1395.2
    ## - serasa_banking_debts      1   1379.3 1397.3
    ## - serasa_restriction        1   1380.5 1398.5
    ## - age                       1   1380.8 1398.8
    ## - monthly_payment           1   1421.8 1439.8
    ## - collateral_debt_amount    1   1446.3 1464.3
    ## 
    ## Step:  AIC=1391.27
    ## sent_to_analysis ~ age + collateral_value + collateral_debt_amount + 
    ##     serasa_restriction + serasa_dishonored_checks + serasa_banking_debts + 
    ##     serasa_commercial_debts + monthly_payment
    ## 
    ##                            Df Deviance    AIC
    ## <none>                          1373.3 1391.3
    ## - serasa_commercial_debts   1   1375.5 1391.5
    ## + purpose                   1   1371.9 1391.9
    ## + serasa_expired_debts      1   1372.7 1392.7
    ## + serasa_protests           1   1372.8 1392.8
    ## + monthly_income            1   1372.9 1392.9
    ## - collateral_value          1   1377.1 1393.1
    ## + loan_amount               1   1373.2 1393.2
    ## - serasa_dishonored_checks  1   1378.7 1394.7
    ## - serasa_banking_debts      1   1380.8 1396.8
    ## - age                       1   1381.8 1397.8
    ## - serasa_restriction        1   1381.8 1397.8
    ## - monthly_payment           1   1423.7 1439.7
    ## - collateral_debt_amount    1   1450.5 1466.5

Após algumas iterações, observa-se que o conjunto de variáveis com o menor valor para o Critério de Informação de Akaike é:

``` r
summary(stepwise)
```

    ## 
    ## Call:
    ## glm(formula = sent_to_analysis ~ age + collateral_value + collateral_debt_amount + 
    ##     serasa_restriction + serasa_dishonored_checks + serasa_banking_debts + 
    ##     serasa_commercial_debts + monthly_payment, family = binomial, 
    ##     data = credit[i_calibration2, ])
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0198  -0.9776  -0.3851   1.1451   3.0388  
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -0.9830     0.1793  -5.482 4.21e-08 ***
    ## age                          1.5641     0.5380   2.907  0.00364 ** 
    ## collateral_value          -298.4433   156.3562  -1.909  0.05630 .  
    ## collateral_debt_amount   -1492.7110   250.2715  -5.964 2.46e-09 ***
    ## serasa_restriction          -1.1533     0.4215  -2.736  0.00621 ** 
    ## serasa_dishonored_checks   -13.9593   357.5786  -0.039  0.96886    
    ## serasa_banking_debts        -2.2237     1.0638  -2.090  0.03658 *  
    ## serasa_commercial_debts     -0.8448     0.5769  -1.464  0.14306    
    ## monthly_payment             12.9010     2.1108   6.112 9.84e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1630.6  on 1247  degrees of freedom
    ## Residual deviance: 1373.3  on 1239  degrees of freedom
    ## AIC: 1391.3
    ## 
    ## Number of Fisher Scoring iterations: 15

Percebemos que nem todas as variáveis são significativas. Podemos excluir do modelo `serasa_dishonored_checks`, `serasa_commercial_debts` e todas as outras com p-value maior que 0.05. Na verdade as restrições ao crédito não são tão importantes para o modelo.

Uma medida interessante para interpretar o modelo é a medida de Razão de chances (Odds Ratio) que calcula a razão de chances.

Em estatística, o odds ratio (OR) é uma das três principais maneiras de quantificar a intensidade da presença ou ausência da propriedade *A* associada à presença ou ausência da propriedade *B* em uma determinada população.

``` r
exp(cbind(OR = coef(stepwise), confint(stepwise)))
```

    ##                                     OR         2.5 %        97.5 %
    ## (Intercept)               3.742054e-01  2.626273e-01  5.306509e-01
    ## age                       4.778351e+00  1.668670e+00  1.377305e+01
    ## collateral_value         2.441869e-130 3.160640e-265  2.408640e-18
    ## collateral_debt_amount    0.000000e+00  0.000000e+00  0.000000e+00
    ## serasa_restriction        3.155797e-01  1.308029e-01  6.927503e-01
    ## serasa_dishonored_checks  8.661014e-07  2.733914e-88 2.367183e-102
    ## serasa_banking_debts      1.082062e-01  5.801335e-03  5.819761e-01
    ## serasa_commercial_debts   4.296293e-01  1.312155e-01  1.302425e+00
    ## monthly_payment           4.007067e+05  6.929441e+03  2.734586e+07

A ideia agora é construir o(s) modelo(s) de Credit Scoring com o DataFrame "credit" e em seguida avaliar o ajuste com o DataFrame "validacao".

``` r
#Abordagem Stepwise para seleção de variáveis
        
#Faz a previsão para a base de validação (probabilidade)
predito <- predict(stepwise,validacao,type="response")

#Escolhe quem vai ser "1" e quem vai ser "0"
predito <- ifelse(predito>=0.8,1,0)
  
#Compara os resultados
table(predito,validacao$sent_to_analysis)
```

    ##        
    ## predito   0   1
    ##       0 798 429
    ##       1   5  16

A taxa de acerto é:

$$
Tx = \\frac {798+16}{1248}
$$

``` r
(table(predito,validacao$sent_to_analysis)[1]+table(predito,validacao$sent_to_analysis)[4])/ (table(predito,validacao$sent_to_analysis)[1]+table(predito,validacao$sent_to_analysis)[2]+table(predito,validacao$sent_to_analysis)[3]+table(predito,validacao$sent_to_analysis)[4])
```

    ## [1] 0.6522436

Podemos melhorar a taxa de acerto refinando o modelo por meio da exclusão de variáveis não significantes e pela inclusão de componentes estatisticamente significantes.

Usando o AIC e Maximum likelihood
---------------------------------

``` r
logLik(modelo.completo)
```

    ## 'log Lik.' -1574.788 (df=14)

``` r
logLik(LogisticModel.1)
```

    ## 'log Lik.' -813.27 (df=14)

``` r
logLik(LogisticModel.3)
```

    ## 'log Lik.' -685.2237 (df=14)

Então temos o valor do modelo que sabemos que é o correto de −1590.07, enquanto o modelo mais simples sem as variáveis `education_level` e `marital_status` (e errado) foi de -801.05. O modelo, também completo, vemos então que quanto melhor o ajuste (e teoricamente melhor o modelo) temos um valor maior de logLik.

``` r
extractAIC(modelo.completo)
```

    ## [1]   14.000 3177.577

``` r
extractAIC(LogisticModel.1)
```

    ## [1]   14.00 1654.54

``` r
extractAIC(LogisticModel.3)
```

    ## [1]   14.000 1398.447

Agora o modelo 1, com 2 parâmetros a menos tem um valor mais alto de 3247 enquanto o modelo 3 (que sabemos que é o correto) tem um parâmetro e o valor de 1630 de AIC, mas o *modelo completo* tem o menor valor, então valores menores devem ser bons. Somos inclinados a usar o modelo completo que tem os 2 parâmetros, intercepto e inclinação.

Mas podemos ainda fazer um teste F, vendo a razão entre os logLik para saber se os modelos são diferentes.

``` r
anova(LogisticModel.1,LogisticModel.3)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: sent_to_analysis ~ (age + monthly_income + collateral_value + 
    ##     loan_amount + collateral_debt_amount + serasa_restriction + 
    ##     serasa_dishonored_checks + serasa_expired_debts + serasa_banking_debts + 
    ##     serasa_commercial_debts + serasa_protests + marital_status + 
    ##     monthly_payment + purpose + education_level) - education_level - 
    ##     marital_status
    ## Model 2: sent_to_analysis ~ age + monthly_income + collateral_value + 
    ##     loan_amount + collateral_debt_amount + serasa_restriction + 
    ##     serasa_dishonored_checks + serasa_expired_debts + serasa_banking_debts + 
    ##     serasa_commercial_debts + serasa_protests + marital_status + 
    ##     monthly_payment + purpose + education_level
    ##   Resid. Df Resid. Dev Df Deviance
    ## 1      1234     720.54            
    ## 2      1234    1370.45  0  -649.91

Existem diferenças entre os modelos, então ficamos com aquele com mais parâmetros, o modelo 1, o mais complexo, não podemos abandonar ele pelo mais simples, ja que ele explica muita coisa que o modelo 2 mais simples não deu conta de explicar, mas não podemos esquecer que:

**OBS:** retiramos do modelo as variáveis `education_level` e `marital_status`, essas variáveis deixam o modelo instável e *uma predição a partir de um ajuste `rank-deficient` pode ser enganoso*. Entãoi vamos ficar com o modelo 3, cujos valores estão no meio termo qusando comparado aos dois outros models.

Podemos ainda fazer isso usando o comando *s**t**e**p*(), que vai pegar o modelo inicial que você fornecer, e se você mandar ele simplificar com o argumento *d**i**r**e**c**t**i**o**n* = ”*b**a**c**k**w**a**r**d*”, ele vai tirando um por um dos parâmetros, e ve se o modelo fica diferente, se continuar explicando bem os dados, ele descarta esse parâmetro, so deixar o *t**r**a**c**e* = 1 para ver o processo, como o modelo mais complexo aqui, o modelo 1, ele faz isso quantas vezes for o núemro de variáveis menos um.

Então temos:

ps(trace=1 e nesse caso o direction=”backward” são os default da função)

``` r
mod.final3 <- step(LogisticModel.3)
```

    ## Start:  AIC=1398.45
    ## sent_to_analysis ~ age + monthly_income + collateral_value + 
    ##     loan_amount + collateral_debt_amount + serasa_restriction + 
    ##     serasa_dishonored_checks + serasa_expired_debts + serasa_banking_debts + 
    ##     serasa_commercial_debts + serasa_protests + marital_status + 
    ##     monthly_payment + purpose + education_level
    ## 
    ## 
    ## Step:  AIC=1398.45
    ## sent_to_analysis ~ age + monthly_income + collateral_value + 
    ##     loan_amount + collateral_debt_amount + serasa_restriction + 
    ##     serasa_dishonored_checks + serasa_expired_debts + serasa_banking_debts + 
    ##     serasa_commercial_debts + serasa_protests + marital_status + 
    ##     monthly_payment + purpose
    ## 
    ## 
    ## Step:  AIC=1398.45
    ## sent_to_analysis ~ age + monthly_income + collateral_value + 
    ##     loan_amount + collateral_debt_amount + serasa_restriction + 
    ##     serasa_dishonored_checks + serasa_expired_debts + serasa_banking_debts + 
    ##     serasa_commercial_debts + serasa_protests + monthly_payment + 
    ##     purpose
    ## 
    ##                            Df Deviance    AIC
    ## - loan_amount               1   1370.5 1396.5
    ## - monthly_income            1   1370.8 1396.8
    ## - serasa_protests           1   1370.9 1396.9
    ## - serasa_expired_debts      1   1371.1 1397.1
    ## - purpose                   1   1371.8 1397.8
    ## <none>                          1370.5 1398.5
    ## - serasa_commercial_debts   1   1373.2 1399.2
    ## - collateral_value          1   1373.9 1399.9
    ## - serasa_dishonored_checks  1   1375.7 1401.7
    ## - serasa_restriction        1   1375.8 1401.8
    ## - serasa_banking_debts      1   1378.2 1404.2
    ## - age                       1   1379.7 1405.7
    ## - monthly_payment           1   1390.5 1416.5
    ## - collateral_debt_amount    1   1444.2 1470.2
    ## 
    ## Step:  AIC=1396.53
    ## sent_to_analysis ~ age + monthly_income + collateral_value + 
    ##     collateral_debt_amount + serasa_restriction + serasa_dishonored_checks + 
    ##     serasa_expired_debts + serasa_banking_debts + serasa_commercial_debts + 
    ##     serasa_protests + monthly_payment + purpose
    ## 
    ##                            Df Deviance    AIC
    ## - monthly_income            1   1370.9 1394.9
    ## - serasa_protests           1   1371.0 1395.0
    ## - serasa_expired_debts      1   1371.1 1395.1
    ## - purpose                   1   1371.9 1395.9
    ## <none>                          1370.5 1396.5
    ## - serasa_commercial_debts   1   1373.2 1397.2
    ## - collateral_value          1   1374.6 1398.6
    ## - serasa_dishonored_checks  1   1375.8 1399.8
    ## - serasa_restriction        1   1376.0 1400.0
    ## - serasa_banking_debts      1   1378.3 1402.3
    ## - age                       1   1379.7 1403.7
    ## - monthly_payment           1   1420.4 1444.4
    ## - collateral_debt_amount    1   1444.7 1468.7
    ## 
    ## Step:  AIC=1394.91
    ## sent_to_analysis ~ age + collateral_value + collateral_debt_amount + 
    ##     serasa_restriction + serasa_dishonored_checks + serasa_expired_debts + 
    ##     serasa_banking_debts + serasa_commercial_debts + serasa_protests + 
    ##     monthly_payment + purpose
    ## 
    ##                            Df Deviance    AIC
    ## - serasa_protests           1   1371.3 1393.3
    ## - serasa_expired_debts      1   1371.5 1393.5
    ## - purpose                   1   1372.2 1394.2
    ## <none>                          1370.9 1394.9
    ## - serasa_commercial_debts   1   1373.6 1395.6
    ## - collateral_value          1   1374.9 1396.9
    ## - serasa_dishonored_checks  1   1376.2 1398.2
    ## - serasa_restriction        1   1376.3 1398.3
    ## - serasa_banking_debts      1   1378.7 1400.7
    ## - age                       1   1379.9 1401.9
    ## - monthly_payment           1   1420.8 1442.8
    ## - collateral_debt_amount    1   1445.5 1467.5
    ## 
    ## Step:  AIC=1393.33
    ## sent_to_analysis ~ age + collateral_value + collateral_debt_amount + 
    ##     serasa_restriction + serasa_dishonored_checks + serasa_expired_debts + 
    ##     serasa_banking_debts + serasa_commercial_debts + monthly_payment + 
    ##     purpose
    ## 
    ##                            Df Deviance    AIC
    ## - serasa_expired_debts      1   1371.9 1391.9
    ## - purpose                   1   1372.7 1392.7
    ## <none>                          1371.3 1393.3
    ## - serasa_commercial_debts   1   1373.7 1393.7
    ## - collateral_value          1   1375.3 1395.3
    ## - serasa_dishonored_checks  1   1376.7 1396.7
    ## - serasa_banking_debts      1   1378.8 1398.8
    ## - serasa_restriction        1   1379.2 1399.2
    ## - age                       1   1380.2 1400.2
    ## - monthly_payment           1   1421.3 1441.3
    ## - collateral_debt_amount    1   1445.9 1465.9
    ## 
    ## Step:  AIC=1391.88
    ## sent_to_analysis ~ age + collateral_value + collateral_debt_amount + 
    ##     serasa_restriction + serasa_dishonored_checks + serasa_banking_debts + 
    ##     serasa_commercial_debts + monthly_payment + purpose
    ## 
    ##                            Df Deviance    AIC
    ## - purpose                   1   1373.3 1391.3
    ## <none>                          1371.9 1391.9
    ## - serasa_commercial_debts   1   1374.1 1392.1
    ## - collateral_value          1   1375.9 1393.9
    ## - serasa_dishonored_checks  1   1377.2 1395.2
    ## - serasa_banking_debts      1   1379.3 1397.3
    ## - serasa_restriction        1   1380.5 1398.5
    ## - age                       1   1380.8 1398.8
    ## - monthly_payment           1   1421.8 1439.8
    ## - collateral_debt_amount    1   1446.3 1464.3
    ## 
    ## Step:  AIC=1391.27
    ## sent_to_analysis ~ age + collateral_value + collateral_debt_amount + 
    ##     serasa_restriction + serasa_dishonored_checks + serasa_banking_debts + 
    ##     serasa_commercial_debts + monthly_payment
    ## 
    ##                            Df Deviance    AIC
    ## <none>                          1373.3 1391.3
    ## - serasa_commercial_debts   1   1375.5 1391.5
    ## - collateral_value          1   1377.1 1393.1
    ## - serasa_dishonored_checks  1   1378.7 1394.7
    ## - serasa_banking_debts      1   1380.8 1396.8
    ## - age                       1   1381.8 1397.8
    ## - serasa_restriction        1   1381.8 1397.8
    ## - monthly_payment           1   1423.7 1439.7
    ## - collateral_debt_amount    1   1450.5 1466.5

``` r
mod.final3
```

    ## 
    ## Call:  glm(formula = sent_to_analysis ~ age + collateral_value + collateral_debt_amount + 
    ##     serasa_restriction + serasa_dishonored_checks + serasa_banking_debts + 
    ##     serasa_commercial_debts + monthly_payment, family = binomial, 
    ##     data = credit[i_calibration2, ])
    ## 
    ## Coefficients:
    ##              (Intercept)                       age  
    ##                  -0.9830                    1.5641  
    ##         collateral_value    collateral_debt_amount  
    ##                -298.4433                -1492.7110  
    ##       serasa_restriction  serasa_dishonored_checks  
    ##                  -1.1533                  -13.9593  
    ##     serasa_banking_debts   serasa_commercial_debts  
    ##                  -2.2237                   -0.8448  
    ##          monthly_payment  
    ##                  12.9010  
    ## 
    ## Degrees of Freedom: 1247 Total (i.e. Null);  1239 Residual
    ## Null Deviance:       1631 
    ## Residual Deviance: 1373  AIC: 1391

Precisamos de uma boa linha de base que crie "o melhor modelo simples" que traga um equilíbrio entre a melhor precisão possível com um modelo que ainda é simples o suficiente para entender.

``` r
coefficients(LogisticModel.1) * 20/log(2)
```

    ##              (Intercept)                      age           monthly_income 
    ##            -3.243942e+01             3.198618e+01            -2.702324e+02 
    ##         collateral_value              loan_amount   collateral_debt_amount 
    ##            -2.188574e+03             3.408626e+01            -8.703873e+04 
    ##       serasa_restriction serasa_dishonored_checks     serasa_expired_debts 
    ##            -1.996582e+01            -4.392602e+02            -3.965658e+02 
    ##     serasa_banking_debts  serasa_commercial_debts          serasa_protests 
    ##            -5.059458e+01            -4.212423e+00             9.623641e+00 
    ##          monthly_payment                  purpose 
    ##             6.952592e+01             3.049794e-01

Uma nova abordagem de um novo modelo
------------------------------------

O pacote OneR serve para encontrar esse ponto e, assim, estabelecer uma nova linha de base Para modelos de classificação em Aprendizado de Máquinas (ML).

O pacote `OneR` está preenchendo uma lacuna de longa data porque apenas uma implementação baseada em JAVA estava disponível até agora (pacote RWeka como uma interface para a classe OneR JAVA). Além disso, vários aprimoramentos foram feitos.

Agora vamos usar o pacote `OneR` e comparar. Mas na criação do modelo vamos ignorar a coluna `educarion_level`, essa coluna apenas prejudica o modelo. Em uma versão de teste os resultados foram muito abaixo do esperado. Veja abaixo o exemplo com essa variável.

``` r
library("OneR")
modelOne <- OneR(credit, verbose = TRUE)
```

    ## 
    ##     Attribute                Accuracy
    ## 1 * age                      100%    
    ## 1   monthly_income           100%    
    ## 1   collateral_value         100%    
    ## 1   loan_amount              100%    
    ## 1   collateral_debt_amount   100%    
    ## 1   serasa_restriction       100%    
    ## 1   serasa_dishonored_checks 100%    
    ## 1   serasa_expired_debts     100%    
    ## 1   serasa_banking_debts     100%    
    ## 1   serasa_commercial_debts  100%    
    ## 1   serasa_protests          100%    
    ## 1   marital_status           100%    
    ## 1   monthly_payment          100%    
    ## 1   purpose                  100%    
    ## 1   sent_to_analysis         100%    
    ## ---
    ## Chosen attribute due to accuracy
    ## and ties method (if applicable): '*'

Agora sem a variável:

``` r
library("OneR")
modelOne <- OneR(credit[,-16], verbose = TRUE)
```

    ## 
    ##     Attribute                Accuracy
    ## 1 * monthly_payment          64.25%  
    ## 2   loan_amount              64.01%  
    ## 3   age                      63.98%  
    ## 3   monthly_income           63.98%  
    ## 3   collateral_value         63.98%  
    ## 3   collateral_debt_amount   63.98%  
    ## 3   serasa_restriction       63.98%  
    ## 3   serasa_dishonored_checks 63.98%  
    ## 3   serasa_expired_debts     63.98%  
    ## 3   serasa_banking_debts     63.98%  
    ## 3   serasa_commercial_debts  63.98%  
    ## 3   serasa_protests          63.98%  
    ## 3   marital_status           63.98%  
    ## 3   purpose                  63.98%  
    ## ---
    ## Chosen attribute due to accuracy
    ## and ties method (if applicable): '*'

Vamos mostrar as regras aprendidas e diagnósticos do modelo.

``` r
summary(modelOne)
```

    ## 
    ## Call:
    ## OneR.data.frame(x = credit[, -16], verbose = TRUE)
    ## 
    ## Rules:
    ## If monthly_payment = (0.0044,0.204] then sent_to_analysis = 0
    ## If monthly_payment = (0.204,0.403]  then sent_to_analysis = 1
    ## If monthly_payment = (0.403,0.602]  then sent_to_analysis = 1
    ## If monthly_payment = (0.602,0.801]  then sent_to_analysis = 1
    ## If monthly_payment = (0.801,1]      then sent_to_analysis = 0
    ## 
    ## Accuracy:
    ## 2672 of 4159 instances classified correctly (64.25%)
    ## 
    ## Contingency table:
    ##                 monthly_payment
    ## sent_to_analysis (0.0044,0.204] (0.204,0.403] (0.403,0.602] (0.602,0.801]
    ##              0           * 2632            28             0             0
    ##              1             1459          * 30           * 8           * 1
    ##              Sum           4091            58             8             1
    ##                 monthly_payment
    ## sent_to_analysis (0.801,1]  Sum
    ##              0         * 1 2661
    ##              1           0 1498
    ##              Sum         1 4159
    ## ---
    ## Maximum in each column: '*'
    ## 
    ## Pearson's Chi-squared test:
    ## X-squared = 22.982, df = 4, p-value = 0.0001277

Agora sim temos um modelo mais performático.

Então vamos plotar o Diagnóstico do modelo

``` r
plot(modelOne)
```

![](README_files/figure-markdown_github/unnamed-chunk-43-1.png)

**Usando o modelo para prever dados**

``` r
prediction <- predict(modelOne, credit[,c(1:5,12:14,16)])
```

**Avaliando as estatísticas de previsão**

``` r
eval_model(prediction, credit[,c(1:5,12:14,16)])
```

    ## 
    ## Confusion matrix (absolute):
    ##           Actual
    ## Prediction    0    1  Sum
    ##        0   4092    0 4092
    ##        1     67    0   67
    ##        Sum 4159    0 4159
    ## 
    ## Confusion matrix (relative):
    ##           Actual
    ## Prediction    0    1  Sum
    ##        0   0.98 0.00 0.98
    ##        1   0.02 0.00 0.02
    ##        Sum 1.00 0.00 1.00
    ## 
    ## Accuracy:
    ## 0.9839 (4092/4159)
    ## 
    ## Error rate:
    ## 0.0161 (67/4159)
    ## 
    ## Error rate reduction (vs. base rate):
    ## -Inf (p-value = 1)

Sim, esse modelo com 98,4% é o modelo ideal para classificação de crédito com garantial de automóvel. Vale lembrar que a cada novo dado o modelo precisa ser treinado novamente par uma melhor previsão.
