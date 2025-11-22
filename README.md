# Trabalho-Geostatistica


Banco de dados referente à produção da cana-de-açúcar, anos de 2016 e
atributos do solo.

### Carregando pacotes

``` r
library(tidyverse)
library(sp)
library(vegan)
library(readxl)
library(gstat)
library(plotly)
library(nortest)
library(car)
library(corrplot)
library(GGally)
library(psych)
theme_set(theme_bw())
```

### Arrumando o banco de dados no excel.

``` r
# list_files <- list.files("data-raw", full.names = TRUE)
# 
# my_read_xl <- function(path){
#   read_excel(path) |> 
#     mutate(
#   ano = str_extract(path,"[0-9]+")
#   ) |> relocate(ano)
# }
# my_read_xl(list_files[3])
# dff <- map_df(list_files,my_read_xl)
# write_rds("C:/Trabalho_Gener/Data/sugarcane-soil-production.rds")
```

### Lendo o banco em rds

``` r
data_set <- read_rds("C:/Trabalho_Gener/Data/sugarcane-soil-production.rds")
contorno <- read.table("C:/Trabalho_Gener/Data/coordenadas-contorno.txt",sep=",",h=TRUE)
p <- Polygon(contorno)
ps <- Polygons(list(p),1)
contorno_ps <- SpatialPolygons(list(ps))
def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}
```

``` r
x<-data_set$x
y<-data_set$y
dis <- 0.005 #Distância entre pontos
grid <- expand.grid(X=seq(min(x),max(x),dis), Y=seq(min(y),max(y),dis)) |> 
  mutate(flag = def_pol(X,Y,contorno)) |>  
  filter(flag) |> select(-flag)
gridded(grid) = ~ X + Y
plot(grid) 
points(x,y,col="red",pch=4)
```

### Conhecendo o Banco de dados

``` r
data_set |> 
  filter(ano == 2016) |> 
  ggplot(aes(x=x, y=y)) +
  geom_point()
# ggplotly(plot_graph)
```

``` r
glimpse(data_set)
```

``` r
df_aux <- data_set |> 
  filter(ano == 2016) |> 
  select(tch_real:m_1) 
for(i in 1:length(df_aux)){
  x_vari <- as.data.frame(df_aux)[,i]
  print(paste("Variável:",names(df_aux[i])))
  print(lillie.test(x_vari))
}
estat_names <- c("Min","Q1","Med","Média","Q3",
                 "Max","DP","CV","Skn","Krt")
estat_desc <- function(x){
  m <- mean(x,na.rm = TRUE)
  md <- median(x)
  mini <- min(x,na.rm = TRUE)
  q1 <- quantile(x,.25)
  q3 <- quantile(x,.75)
  maxi <- max(x,na.rm = TRUE)
  dp <- sd(x,na.rm = TRUE)
  cv <- 100*dp/m
  ass <- agricolae::skewness(x)
  curt <- agricolae::kurtosis(x)
  c(mini,q1,md,m,q3,maxi,dp,cv,ass,curt)
}

data_set |> 
  filter(ano == 2016) |> 
  group_by(x,y) |> 
  summarise(
    across(
      tch_real:m_1,
      ~mean(.,na.rm=TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) |> 
  reframe(
    across(
      tch_real:m_1,
      estat_desc
    ),
  ) |> add_column(estat_names) |> relocate(estat_names) |> 
  writexl::write_xlsx("saida/estat-desc.xlsx")
```

### Tranformação para adequação das variáveis

``` r
data_set |> 
  mutate(
    s_1 = log(s_1),
    ca_1 = log(ca_1),
    p_resina_1 = log(p_resina_1),
    sb_1 = log(sb_1),
    m_1 = log(m_1+1),
    ctc_1 = log(ctc_1),
  )|> 
  filter(ano == 2016) |> 
  group_by(x,y) |> 
  summarise(
    across(
      tch_real:m_1,
      ~mean(.,na.rm=TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) |> 
  reframe(
    across(
      tch_real:m_1,
      estat_desc
    ),
  ) |> add_column(estat_names) |> 
  relocate(estat_names)

df_aux <- data_set |> 
  mutate(
    s_1 = log(s_1),
    ca_1 = log(ca_1),
    p_resina_1 = log(p_resina_1+1),
    sb_1 = log(sb_1),
    m_1 = log(m_1+1)) |> 
  filter(ano == 2016) |> 
  select(tch_real:m_1)
```

# PASSO 1

definir o ano e a variável

### Separa o banco de dados por ano e por variáveis

``` r
ano_analise <- 2016
variavel <- "tch_real"
data_set_aux <- data_set |> 
  filter(ano == ano_analise) |> 
  select(x,y,variavel)
names(data_set_aux) <- c("x","y","z")
glimpse(data_set_aux)
```

### Calcular a média da variável por ponto

``` r
data_set_aux <- data_set_aux |> 
  group_by(x,y) |> 
  summarise(
    z = mean(z,na.rm = TRUE),
    .groups = "drop"
  )
```

### Análise exoploratória

``` r
data_set_aux |> 
  pull(z) |> 
  summary()
```

``` r
data_set_aux |> 
  ggplot(aes(y=z)) +
  geom_boxplot(fill="gray") +
  xlim(-1,1) +
  labs(y = variavel)
```

``` r
data_set_aux |> 
  ggplot(aes(x=z)) +
  geom_histogram(fill="gray",color="black",
                 bins = 10) +
  labs(x = variavel)
```

Testes de normalidade

``` r
y <- data_set_aux |> pull(z)
shapiro.test(y)
cvm.test(y)
lillie.test(y)
ad.test(y)
```

### verificar colinearidade

``` r
cor_matrix <- cor(df_aux, use = "pairwise.complete.obs", method = "pearson")
print(round(cor_matrix, 2))

# Visualizar a matriz de correlação
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.8)
```

``` r
# Análise de multicolinearidade com VIF
# É necessário ajustar um modelo linear com todas as variáveis independentes
# Exemplo: escolha uma variável dependente qualquer (ex: y)
modelo <- lm(tch_real~ ., data = df_aux)
vif_valores <- vif(modelo)
print(vif_valores)
```

``` r
# Identificar variáveis com VIF alto (>5 ou >10, dependendo do critério)
colineares <- vif_valores[vif_valores > 10]
print("Variáveis com possível multicolinearidade:")
print(colineares)
```

## Regressão Linear Múltipla

``` r
variaveis_validas <- df_aux |> 
  select(-c(ca_1, h_al_1,sb_1,v_1))

# Criar fórmula dinâmica para regressão
form <- as.formula(paste("tch_real ~", paste(names(variaveis_validas[-1]), collapse = " + ")))

# Ajustar o modelo de regressão múltipla
modelo_final <- lm(form, data = df_aux)

# Resumo do modelo
summary(modelo_final)
```

``` r
plot(modelo_final)
hist(residuals(modelo_final))
lmtest::bptest(modelo_final)  # Teste de Breusch-Pagan
lmtest::dwtest(modelo_final)
```

## Stepwise Forward usando

``` r
# Modelo nulo (apenas intercepto)
modelo_nulo <- lm(tch_real ~ 1, data = variaveis_validas)

# Modelo completo (com todas as variáveis explicativas)
modelo_completo <- lm(tch_real ~ ., data = variaveis_validas)

# Aplicar Stepwise Forward com base no critério AIC
modelo_step_forward <- step(
  object = modelo_nulo,
  scope = list(lower = modelo_nulo, upper = modelo_completo),
  direction = "forward",
  trace = TRUE
)

# Ver resumo do modelo final selecionado
summary(modelo_step_forward)
```

## Análise de Componentes Principais

``` r
da <- df_aux[-c(2,10)]
mc <- cor(da)
rownames(mc) <-c("TCH","pH","MO","P","S","Ca","Mg","K","H+Al","SB",
                 "CTC","V%","m%")
colnames(mc) <-c("TCH","pH","MO","P","S","Ca","Mg","K","H+Al","SB",
                 "CTC","V%","m%")
# corrplot::corrplot.mixed(mc,upper = "square",
#                          tl.col = "black",
#                          tl.cex = .9,
#                          lower.col = "black",
#                          number.cex = 0.7)

corrplot(mc,method = "color",
         outline = T,,
         addgrid.col = "darkgray",cl.pos = "r", tl.col = "black",
         tl.cex = 1, cl.cex = 1, type = "upper", bg="azure2",
         diag = FALSE,
         addCoef.col = "black",
         # cl.ratio = 0.2,
         cl.length = 5,
         number.cex = 0.8) 
```

``` r
print("======== Análise de Agrupamento Hierárquico ========== ")
da_pad<-decostand(da, 
                  method = "standardize",
                  na.rm=TRUE)
da_pad_euc<-vegdist(da_pad,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
plot(da_pad_euc_ward, 
     ylab="Distância Euclidiana",
     xlab="Acessos", hang=-1,
     col="blue", las=1,
     cex=.6,lwd=1.5);box()

grupo<-cutree(da_pad_euc_ward,2)
```

``` r
print("======== Análise de Componentes Principais ========== ")
pca <-  prcomp(da_pad,scale.=TRUE)
# Autovalores
eig<-pca$sdev^2
print("==== Autovalores ====")
print(round(eig,3))
print("==== % da variância explicada ====")
ve<-eig/sum(eig)
print(round(ve,4))
print("==== % da variância explicada acumulada ====")
print(round(cumsum(ve),4)*100)
print("==== Poder Discriminante ====")
mcor<-cor(da_pad,pca$x)
corrplot(mcor)
print("==== screeplot ====")
screeplot(pca)
abline(h=1)
```

``` r
pc1V<-cor(da_pad,pca$x)[,1]/sd(cor(da_pad,pca$x)[,1])
pc2V<-cor(da_pad,pca$x)[,2]/sd(cor(da_pad,pca$x)[,2])
pc3V<-cor(da_pad,pca$x)[,3]/sd(cor(da_pad,pca$x)[,3])
pc1c<-pca$x[,1]/sd(pca$x[,1])
pc2c<-pca$x[,2]/sd(pca$x[,2])
pc3c<-pca$x[,3]/sd(pca$x[,3])
nv<-ncol(da) # número de variáveis utilizadas na análise
```

``` r
# gráfico biplot
bip<-data.frame(pc1c,pc2c,pc3c,grupo)
texto <- data.frame(
  x = pc1V,
  y = pc2V,
  z = pc3V,
  label = rownames(mc)
)
c("TCH","pH","MO","P","S","Ca","Mg","K","H+Al","SB",
  "CTC","V%","m%")
bi_plot <- bip |> 
  ggplot(aes(x=pc1c,y=pc2c))+
  geom_point(size = 1,color="gray") + 
  theme_minimal() +
  # scale_shape_manual(values=16:18)+
  # scale_color_manual(values=c("#009E73", "#999999","#D55E00")) +
  #annotate(geom="text", x=pc1V, y=pc2V, label=names(pc1V),
  #            color="black",font=3)+
  geom_vline(aes(xintercept=0),
             color="black", size=1)+
  geom_hline(aes(yintercept=0),
             color="black", size=1)+
  annotate(geom="segment",
           x=rep(0,length(da)),
           xend=texto$x,
           y=rep(0,length(da)),
           yend=texto$y,color="black",lwd=.5)+
  geom_label(data=texto,aes(x=x,y=y,label=label),
             color="black",angle=0,fontface="bold",size=4,fill="white")+
  labs(x=paste("CP1 (",round(100*ve[1],2),"%)",sep=""),
       y=paste("CP2 (",round(100*ve[2],2),"%)",sep=""),
       color="",shape="")+
  theme(legend.position = "top")
bi_plot
```

``` r
bi_plot +
  coord_cartesian(
    xlim = c(-1.8,1.5),
    ylim = c(-2.5,1.5)
  )
```

``` r
data_set |> 
  filter(ano == 2016) |> 
  add_column(grupo) |> 
  ggplot(aes(x=x,y=y,color=as_factor(grupo))) +
  geom_point() +
  labs(color = "Grupo")
```

``` r
print("==== Tabela da correlação dos atributos com cada PC ====")
ck<-sum(pca$sdev^2>=0.98)
tabelapca<-vector()
for( l in 1:ck) tabelapca<-cbind(tabelapca,mcor[,l])
colnames(tabelapca)<-paste(rep(c("PC"),ck),1:ck,sep="")
pcat<-round(tabelapca,3)
tabelapca<-tabelapca[order(abs(tabelapca[,1])),]
print(tabelapca)
writexl::write_xlsx(data.frame(tabelapca) |> 
                      add_column(nome = row.names(tabelapca)), "saida/pca-table.xlsx")
```

## Modelagem do semivariograma

``` r
data_set_aux |> 
  ggplot(aes(x=x,y=y,color=z)) +
  geom_point() +
  labs(color = variavel)
```

### Análise geoestatística

Criar o arquivo para análise

``` r
coordinates(data_set_aux) = ~ x + y  
form <- z ~ 1 # fórmula da função variogram
```

## PASSO 2

Construir o semivariograma experimental

``` r
vari_exp <- variogram(form, data = data_set_aux,
                      cressie = FALSE,
                      cutoff = 0.20, # distância máxima do semivariograma
                      width = .008) # distancia entre pontos
vari_exp  |>  
  ggplot(aes(x=dist, y=gamma)) +
  geom_point() +
  labs(x="lag (º)",
       y=expression(paste(gamma,"(h)")))
```

#### Escolha do melhor modelo

``` r
patamar=400
alcance=0.05
epepita=0
modelo_1 <- fit.variogram(vari_exp,vgm(patamar,"Sph",alcance,epepita))
modelo_2 <- fit.variogram(vari_exp,vgm(patamar,"Exp",alcance,epepita))
modelo_3 <- fit.variogram(vari_exp,vgm(patamar,"Gau",alcance,epepita))
sqr.f1<-round(attr(modelo_1, "SSErr"),4); c01<-round(modelo_1$psill[[1]],4); c0_c11<-round(sum(modelo_1$psill),4);a1<-round(modelo_1$range[[2]],2)
sqr.f2<-round(attr(modelo_2, "SSErr"),4); c02<-round(modelo_2$psill[[1]],4); c0_c12<-round(sum(modelo_2$psill),4);a2<-round(3*modelo_2$range[[2]],2)
sqr.f3<-round(attr(modelo_3, "SSErr"),4); c03<-round(modelo_3$psill[[1]],4); c0_c13<-round(sum(modelo_3$psill),4);a3<-round(modelo_3$range[[2]]*(3^.5),2)

df_aux <- vari_exp |> 
  mutate(
    gamma_m1 = ifelse(dist <= a1, c01 + (c0_c11-c01)*(3/2*(dist/a1)-1/2*(dist/a1)^3),c0_c11),
    gamma_m2 = c02 + (c0_c12-c02)*(1-exp(-3*(dist/a2))),
    gamma_m3 = c03 + (c0_c13-c03)*(1-exp(-3*(dist/a3)^2)),
    residuo_total = (gamma-mean(gamma))^2,
    residuo_mod_1 = (gamma - gamma_m1)^2,
    residuo_mod_2 = (gamma - gamma_m2)^2,
    residuo_mod_3 = (gamma - gamma_m3)^2
  ) |> 
  summarise(
    r2_1=(sum(residuo_total) - sum(residuo_mod_1))/sum(residuo_total), 
    r2_2=(sum(residuo_total) - sum(residuo_mod_2))/sum(residuo_total), 
    r2_3=(sum(residuo_total) - sum(residuo_mod_3))/sum(residuo_total), 
  )
r21<-as.vector(round(df_aux[1],4))
r22<-as.vector(round(df_aux[2],4))
r23<-as.vector(round(df_aux[3],4))

plot(vari_exp,model=modelo_1, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Esf(C0= ",c01,"; C0+C1= ", c0_c11, "; a= ", a1,"; r2 = ", r21,")",sep=""))
plot(vari_exp,model=modelo_2, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Exp(C0= ",c02,"; C0+C1= ", c0_c12, "; a= ", a2,"; r2 = ", r22,")",sep=""))
plot(vari_exp,model=modelo_3, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Gau(C0= ",c03,"; C0+C1= ", c0_c13, "; a= ", a3,"; r2 = ", r23,")",sep=""))
```

## Validação Cruzada

``` r
conjunto_validacao <- data_set_aux |> 
  as_tibble() |> 
  sample_n(300)
coordinates(conjunto_validacao) = ~x + y
modelos<-list(modelo_1,modelo_2,modelo_3)
for(j in 1:3){
  est<-0
  # vari<-as.character(form)[2]
  for(i in 1:nrow(conjunto_validacao)){
    valid <- krige(formula=form, conjunto_validacao[-i,], conjunto_validacao, model=modelos[[j]])
    est[i]<-valid$var1.pred[i]
  }
  obs<-as.data.frame(conjunto_validacao)[,3] 
  RMSE<-round((sum((obs-est)^2)/length(obs))^.5,3)
  mod<-lm(obs~est)
  b<-round(mod$coefficients[2],3)
  se<-round(summary(mod)$coefficients[4],3)
  r2<-round(summary(mod)$r.squared,3) 
  a<-round(mod$coefficients[1],3)
  plot(est,obs,xlab="Estimado", ylab="Observado",pch=j,col="blue",
       main=paste("Modelo = ",modelos[[j]][2,1],"; Coef. Reg. = ", b, " (SE = ",se, ", r2 = ", r2,")\ny intersept = ",a,"RMSE = ",RMSE ))
  abline(lm(obs~est));
  abline(0,1,lty=3)
}
```

## PASSO 3

selecionar o melhor modelo Modelar o semivariograma

``` r
modelo <- modelo_1 ## sempre modificar
plot(vari_exp,model=modelo, col=1,pl=F,pch=16)
```

## PASSO 4

### Krigragem ordinária (KO)

Utilizando o algorítmo de KO, vamos estimar xco2 nos locais não
amostrados.

``` r
ko_variavel <- krige(formula=form, data_set_aux, grid, model=modelo, 
                     block=c(0,0),
                     nsim=0,
                     na.action=na.pass,
                     debug.level=-1,  
)
```

Mapa de padrão espacial

``` r
mapa <- as.tibble(ko_variavel) |> 
  ggplot(aes(x=X, y=Y)) + 
  geom_tile(aes(fill = var1.pred)) +
  # scale_fill_gradient(low = "yellow", high = "blue") + 
  scale_fill_viridis_c() +
  coord_equal() + 
  labs(fill=variavel,
       x="Longitude",
       y="Latitude")
mapa
ggsave(paste0("mapas/krigagem-",variavel,"-",ano_analise,".png"))
```

``` r
# Salvando o arquivo krigado
df <- ko_variavel |> 
  as.tibble() |> 
  mutate(var1.var = sqrt(var1.var)) |> 
  rename(
    !!variavel := var1.pred,
    !!paste0(variavel,"_sd") := var1.var,
  )
write_rds(df,paste0("saida/",variavel,"-",ano_analise,".rds"))
