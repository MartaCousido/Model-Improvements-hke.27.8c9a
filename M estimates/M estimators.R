# Estimates of natural mortality for hke.8c9a

# Read SS output------------------------------------------------------------

dir<- "D:/Usuarios/mcousido/Nextcloud/Mervex Core/ICES GROUPS/WGBIE/WD PRE WGBIE25/Modelos WD/6_rec_dev_1985_sigmaR_RW_new_bias"
#dir<-paste0(getwd(),"5. New rec devs")

replist <- r4ss::SSgetoutput( dirvec = dir, getcovar = F, verbose = FALSE)[[1]]

# Packages ------------------------------------------------------------------
library(FLife)
library(FLBRP)
library(FSA)
library(ggplot2)
library(dplyr)
library(tidyr)

# length-at-age - VB growth model -------------------------------------------

al0<-0
al0_m<-0
al0_f<-0
lh_pars_Female<-lhPar(FLPar(linf = 1))
lh_pars_Female["linf"]<-110
lh_pars_Female["k"]<-0.191
lh_pars_Female["t0"]<-al0_f

lh_pars_Male<-lhPar(FLPar(linf = 1))
lh_pars_Male["linf"]<-80
lh_pars_Male["k"]<-0.294
lh_pars_Male["t0"]<-al0_m

a <- 0.00377
b <-  3.16826



# M estimators --------------------------------------------------------------

## Females -----------------------------------------------------------------------
lvec.vb <- as.numeric(vonB(FLQuant(0:15+0.5), lh_pars_Female))
# Gislason
m.gis.vb_female <- sapply(lvec.vb, function(x){metaM( c("Gislason"), Linf=lh_pars_Female["linf"], K=lh_pars_Female["k"], t0=lh_pars_Female["t0"], L=x) })
# Charnov 
m.cha.vb_female <- sapply(lvec.vb, function(x){metaM( c("Charnov"), Linf=lh_pars_Female["linf"], K=lh_pars_Female["k"], t0=lh_pars_Female["t0"], L=x) })

## Males -------------------------------------------------------------------------
lvec.vb <- as.numeric(vonB(FLQuant(0:15+0.5), lh_pars_Male))
# Gislason
m.gis.vb_male <- sapply(lvec.vb, function(x){metaM( c("Gislason"), Linf=lh_pars_Male["linf"], K=lh_pars_Male["k"], t0=lh_pars_Male["t0"], L=x) })
# Charnov u Landa and Piñeiro (2000)
m.cha.vb_male <- sapply(lvec.vb, function(x){metaM( c("Charnov"), Linf=lh_pars_Male["linf"], K=lh_pars_Male["k"], t0=lh_pars_Male["t0"], L=x) })


# Current M from SS --------------------------------------------------------

M <-replist$Natural_Mortality_Bmark
M_female <-as.numeric( subset( M, Seas==2 & Settlement==1 & Sex==1)[-(1:4)] )  # Females (Season 1 Settlement 1)
M_male <- as.numeric(subset( M, Seas==2 & Settlement==1 & Sex==2)[-(1:4)] )


# Plot ----------------------------------------------------------------------



edad <- 0:15

datos <- data.frame(
  edad = edad,
  gis_female = m.gis.vb_female,
  cha_female = m.cha.vb_female,

  gis_male = m.gis.vb_male,
  cha_male = m.cha.vb_male,

  M_female = M_female,
  M_male = M_male
)

datos_largos <- datos %>%
  pivot_longer(-edad, names_to = "serie", values_to = "valor")

datos_M_female <- datos_largos %>% filter(serie == "M_female")
datos_M_male <- datos_largos %>% filter(serie == "M_male")
datos_otros <- datos_largos %>%
  filter(!serie %in% c("M_female", "M_male"))

colores_personalizados <- c(
  "gis_female" = "#e41a1c",
  "cha_female" = "#fb8072",
  "gis_male" = "#377eb8",
  "cha_male" = "#80b1d3"
)

ggplot() +
  geom_line(data = datos_otros, aes(x = edad, y = valor, color = serie), size = 1.2) +
  
  geom_line(data = datos_M_female, aes(x = edad, y = valor),
            color = "black", linetype = "dashed", size = 1) +
  geom_point(data = datos_M_female, aes(x = edad, y = valor),
             color = "black", size = 2) +
  
  geom_line(data = datos_M_male, aes(x = edad, y = valor),
            color = "black", linetype = "dashed", size = 1) +
  geom_point(data = datos_M_male, aes(x = edad, y = valor),
             color = "black", size = 2) +
  
  scale_color_manual(values = colores_personalizados) +
  labs(title = "M",
       x = "Age",
       y = "M",
       color = "Estimator") +
  theme_minimal()

# To move to control.ss
aux<-datos[c(1,2,6,16),]

for (i in 1:4){
  M<-aux[i,4]
  F<-aux[i,2]
  
  r<-log(M/F);print(r)
  print(F*exp(r)-M)}

