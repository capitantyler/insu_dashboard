# librerías ----
library(graphics, quietly = T)
library(stats, quietly = T)
library(readxl, quietly = T) # para leer xlsx. Es más rápido que open_xlsx
library(openxlsx, quietly = T)
library(lubridate, quietly = T)
library(scales, quietly = T)
library(rlang, quietly = T)
library(tidyverse, quietly = T)
library(data.table, quietly = T)
library(ggplot2, quietly = T)
library(gt, quietly = T)
library(glue, quietly = T)
library(gtable, quietly = T)
library(grid, quietly = T)
library(cowplot, quietly = T)
library(gridExtra, quietly = T)
library(shiny, quietly = T)
library(shinyjs, quietly = T)
library(shinydashboard, quietly = T)
library(shinyWidgets, quietly = T)
library(shinyBS, quietly = T)
library(shinyFeedback, quietly = T)
library(DT, quietly = T)
library(assertthat, quietly = T)
library(waiter, quietly = T)

# para el pdf
library(rmarkdown, quietly = T)
library(tinytex, quietly = T)
library(webshot, quietly = T)

if(!webshot::is_phantomjs_installed()) webshot::install_phantomjs()

# variables primarias ---

# datafolder          <- Sys.getenv("datafolder")
# repositorio         <- Sys.getenv("repositorio")
# output              <- Sys.getenv("output")
# contratos_x_cluster <- as.integer(Sys.getenv("contratos_x_cluster"))
# 

datafolder          <- "data/"
repositorio         <- ""
output              <- "output/"
contratos_x_cluster <- 100

# funciones ----

source(paste0(repositorio, "utils/misc/obtiene_utils.R"), encoding = "UTF-8")
repositorio <- obtiene_utils(paste0(repositorio, "utils/"))

source(paste0(repositorio["contratos"], "filtrar_tablas.R"), encoding = "UTF-8")
source(paste0(repositorio["contratos"], "filtrar_nros_contrato.R"), encoding = "UTF-8")

source(paste0(repositorio["emision"], "emision_con_movimientos.R"), encoding = "UTF-8")
source(paste0(repositorio["emision"], "factor_estacionalidad.R"), encoding = "UTF-8")

source(paste0(repositorio["fechas"], "AAAAMM_diferido.R"), encoding = "UTF-8")
source(paste0(repositorio["fechas"], "AAAAMM_ejercicio.R"), encoding = "UTF-8")
source(paste0(repositorio["fechas"], "completa_huecos_MES.R"), encoding = "UTF-8")
source(paste0(repositorio["fechas"], "proporcion_mes.R"), encoding = "UTF-8")

source(paste0(repositorio["siniestros"], "calcula_siniestralidad.R"), encoding = "UTF-8")
source(paste0(repositorio["siniestros"], "genera_reportes_siniestros.R"), encoding = "UTF-8")
source(paste0(repositorio["siniestros"], "arma_liquidaciones_reservas.R"), encoding = "UTF-8")
source(paste0(repositorio["siniestros"], "ultimate_DFM.R"), encoding = "UTF-8")
source(paste0(repositorio["siniestros"], "IBNER_DFM.R"), encoding = "UTF-8")
source(paste0(repositorio["siniestros"], "ultimate_BF.R"), encoding = "UTF-8")
source(paste0(repositorio["siniestros"], "completa_periodos_IBNER.R"), encoding = "UTF-8")
  #IBNR_siniestros_tasa.R obsoleta. Reemplazar
source(paste0(repositorio["siniestros"], "IBNR_siniestros_tasa.R"), encoding = "UTF-8")

source(paste0(repositorio["plot"], "eje_y_doble.R"), encoding = "UTF-8")
source(paste0(repositorio["plot"], "roundUpNice.R"), encoding = "UTF-8")
source(paste0(repositorio["plot"], "rescaleUp.R"), encoding = "UTF-8")

source(paste0(repositorio["estilos"], "stlExcel_droidgrey.R"), encoding = "UTF-8")
source(paste0(repositorio["estilos"], "formatear_columnas.R"), encoding = "UTF-8")

source(paste0(repositorio["misc"], "funciones_comunes.R"), encoding = "UTF-8")
source(paste0(repositorio["misc"], "comprimir_num.R"), encoding = "UTF-8")

source(paste0(repositorio["tests"], "chk_cols.R"), encoding = "UTF-8")
source(paste0(repositorio["excel"], "excel_utils.R"), encoding = "UTF-8")

# funciones locales
source("R/calcula_reporte_siniestros.R", encoding = "UTF-8")
source("R/calcula_EmisionConMovimientos.R", encoding = "UTF-8")
source("R/calcula_frsa_ju_final.R", encoding = "UTF-8")
source("R/arma_gt_periodo.R", encoding = "UTF-8")
source('R/agrega_periodo_frecuencias.R', encoding = 'UTF-8')
source('R/agrega_periodo_cobertura.R', encoding = 'UTF-8')
source("R/gt_periodo.R", encoding = "UTF-8")
source("R/plot_frec_total.R", encoding = "UTF-8")
source("R/plot_frec_jud.R", encoding = "UTF-8")
source("R/plot_frec_grmu_porinc.R", encoding = "UTF-8")
source("R/plot_sdad_prima.R", encoding = "UTF-8")
source("R/plot_sdad_rvas.R", encoding = "UTF-8")
source("R/escribe_cabecera_tablero_HTML.R", encoding = "UTF-8")

source("mods/boton_do.R", encoding = "UTF-8")
source("mods/seleccionar_grupo.R", encoding = "UTF-8")
source("mods/cabecera.R", encoding = "UTF-8")
source("mods/cuadro.R", encoding = "UTF-8")
source("mods/listado_descargable.R", encoding = "UTF-8")
source("mods/graf_sdad_rvas.R", encoding = "UTF-8")
source("mods/graf_sdad_prima.R", encoding = "UTF-8")
source("mods/graf_frec_total.R", encoding = "UTF-8")
source("mods/graf_frec_jud.R", encoding = "UTF-8")
source("mods/graf_frec_grmu_porinc.R", encoding = "UTF-8")


message("cargando bases en formato data.table")

contratos.data <- readRDS(paste0(datafolder,"contratos/contratos.Rds"))
contratos.data <- data.table(contratos.data, key = "CONTRATO")

estado_consecuencia.data <- readRDS(paste0(datafolder,"maestros/estado_consecuencia.Rds"))
estado_consecuencia.data <- data.table(estado_consecuencia.data, key = "EDOCONS")

estado_siniestro.data <- readRDS(paste0(datafolder,"maestros/estado_siniestro.Rds"))
estado_siniestro.data <- data.table(estado_siniestro.data, key = "ESTADO_STRO")

cie10.data <- readRDS(paste0(datafolder,"maestros/cie10.Rds"))
cie10.data <- data.table(cie10.data, key = "ID")
cie10.data <- cie10.data[,
 CIE10_DESCR := paste0(
   COD_ASO, ": ", str_trunc(`DESCRIPCION COD3`, 50)
 )
][,
  .(ID, COD_ASO, CIE10_DESCR)
]
setnames(cie10.data, "COD_ASO", "CIE10")

siniestros.data <- readRDS(paste0(datafolder,"siniestros/siniestros.Rds"))
siniestros.data <- data.table(siniestros.data, key = "DENUNCIA")
siniestros.data <- estado_consecuencia.data[,
  .(EDOCONS, ESTADO_CONSECUENCIA)
][
  siniestros.data,
  on = .(EDOCONS),
  allow.cartesian = TRUE
][,
  EDOCONS := NULL
]
siniestros.data <- estado_siniestro.data[,
  .(ESTADO_STRO, ESTADO_SINIESTRO)
][
  siniestros.data,
  on = .(ESTADO_STRO),
  allow.cartesian = TRUE
]
siniestros.data <- cie10.data[
  siniestros.data,
  on = .(ID = CIE10_id),
  allow.cartesian = TRUE
][,
  ID := NULL
]

juicios.data <- readRDS(paste0(datafolder,"juicios/juicios.Rds"))
juicios.data <- data.table(juicios.data, key = c("JUICIO", "CONTRATO"))[
  ULTIMO_JUICIO == 1
]

liquidaciones.data <- readRDS(paste0(datafolder,"siniestros/liquidaciones.Rds"))
liquidaciones.data <- data.table(
  liquidaciones.data, key = c("DENUNCIA", "MESLIQ", "CONTRATO")
)
# eliminar cuando se mejore la división entre renta GI e ILP
# liquidaciones.data[,
#   IND_LIQ := ILP_LIQ + RENTA_LIQ
# ][,
#   c("ILP_LIQ", "RENTA_LIQ") := NULL
# ]

reservas.data <- readRDS(paste0(datafolder,"reservas/reservas.Rds"))
reservas.data <- data.table(reservas.data, key = c("DENUNCIA", "MES", "CONTRATO"))

emiorig.data <- readRDS(paste0(datafolder,"emision/emiorig.Rds"))
emiorig.data <- data.table(emiorig.data, key = c("CONTRATO", "MES"))

rectificativa.data <- readRDS(paste0(datafolder,"emision/rectificativa.Rds"))
rectificativa.data <- data.table(rectificativa.data,
                                 key = c("CONTRATO", "MESEMI", "MESRECTIFICATIVA")
)

domesticas.data <- readRDS(paste0(datafolder,"emision/domesticas.Rds"))
domesticas.data <- data.table(domesticas.data, key = c("CONTRATO", "MES"))

CIIU.data <- readRDS(paste0(datafolder,"maestros/CIIU_R2.Rds"))
CIIU.data <- data.table(CIIU.data, key = "CIIU_R2")

sucursal.data <- readRDS(paste0(datafolder,"maestros/sucursal.Rds"))
sucursal.data <- data.table(sucursal.data, key = "SUC_ID")

provincia.data <- readRDS(paste0(datafolder,"maestros/provincia.Rds"))
provincia.data <- data.table(provincia.data, key = "ID")

origen.data <- readRDS(paste0(datafolder,"maestros/origen.Rds"))
origen.data <- data.table(origen.data, key = "ORIGEN")

pas.data <- readRDS(paste0(datafolder,"maestros/PAS.Rds"))
pas.data <- data.table(pas.data, key = "CUIT")

pasUC.data <- readRDS(paste0(datafolder,"maestros/PAS_UC.Rds"))
pasUC.data <- data.table(pasUC.data, key = c("UC", "CUIT"))

titulo_clase.data <- readRDS(paste0(datafolder,"maestros/titulo_clase.Rds"))
titulo_clase.data <- dplyr::select(
  titulo_clase.data, CLASE1, CLASE1_ETIQUETA, CLASE3, CLASE3_ETIQUETA
)
titulo_clase.data <- data.table(titulo_clase.data, key = "CLASE3")

ripte.data <- readRDS(
  paste0(datafolder,"mercado/MTyDS/ripte_sipa.Rds")
)
ripte.data <- data.table(
  ripte.data,
  key = c("MES")
)[,
  c("MES", "IRIPTE")
][,
  IRIPTE_medio := sqrt(IRIPTE * lead(IRIPTE, 1L))
]

# obtengo fdas y BC a priori
fda.data <- readRDS(paste0(datafolder, "ultimates/fda.Rds"))
fda.data <- data.table(fda.data, key = "d")

ultimates_GSC.data <- readRDS(paste0(datafolder, "ultimates/ultimates_GSC.Rds"))
ultimates_GSC.data <- data.table(
  ultimates_GSC.data, key = c("TRIM", "MES_INI", "MES_FIN")
)

#calculo mes corte de datos
mes_corte_datos_max <- max(emiorig.data$MES)
mes_corte_datos_min <- min(emiorig.data$MES)
