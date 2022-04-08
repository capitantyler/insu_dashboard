#' filtrar_tablas
#'
#' filtra tablas de datos según el conjunto de contratos que se determinó.
#' Requiere que las tablas pasadas sean de tipo data.table
#'
#' @param filtroContratos
#' @param heredarContrato logical utiliza la columna CONTRATO de cada tabla,
#'                                o sea, no usa la clave principal de las tablas
#'
#' @return
#' @export
#'
#' @examples
#'
#' @note
#' TODO: armar method para data.frame y tibble
#' TODO: buescar otra forma de simplificar la cantidad de try's. Quizas mejor con
#' !exists
#' TODO: agregar de argumento opcional las tablas que se pasan
#'
#'
filtrar_tablas <- function(
  filtroContratos = NA,
  heredarContrato = TRUE
){
  library(data.table)
  library(rlang)

  nombres_lista <- c(
    "contratos", "emiorig", "rectificativa", "domesticas",
    "comunicaciones", "cobranza", "comisiones",
    "CIIU", "sucursal", "provincia",
    "siniestros", "liquidaciones", "juicios", "reservas", "ibner", "cie10",
    "mercado", "tarifa_clase", "titulo_clase",
    "UC", "pasUC", "pas", "ROL"
  )

  lista <- vector(mode = "list", length = length(nombres_lista))
  names(lista) <- nombres_lista

  if(is_na(filtroContratos)){
    lapply(nombres_lista, function(x) try(`<-`(lista[[x]], get(paste0(x, ".data")))))
  } else {
    try({lista$contratos <- contratos.data[CONTRATO %in% filtroContratos, ]}, silent = TRUE)
    try({lista$emiorig <- emiorig.data[CONTRATO %in% filtroContratos, ]}, silent = TRUE)
    try({lista$rectificativa <- rectificativa.data[CONTRATO %in% filtroContratos, ]}, silent = TRUE)
    try({lista$domesticas <- domesticas.data[CONTRATO %in% filtroContratos, ]}, silent = TRUE)
    try({lista$comunicaciones <- comunicaciones.data[CONTRATO %in% filtroContratos, ]}, silent = TRUE)
    try({lista$cobranza <- cobranza.data[CONTRATO %in% filtroContratos, ]}, silent = TRUE)
    try({lista$comisiones <- comisiones.data[CONTRATO %in% filtroContratos, ]}, silent = TRUE)
    try({lista$siniestros <- siniestros.data[CONTRATO %in% filtroContratos, ]}, silent = TRUE)
    try({
      if(heredarContrato == TRUE){
        lista$liquidaciones <- liquidaciones.data[CONTRATO %in% filtroContratos, ]
        lista$juicios <- juicios.data[CONTRATO %in% filtroContratos, ]
        lista$reservas <- reservas.data[CONTRATO %in% filtroContratos, ]
      } else {
        lista$liquidaciones <- liquidaciones.data[DENUNCIA %in% lista$siniestros$DENUNCIA, ]
        lista$juicios <- juicios.data[DENUNCIA %in% lista$siniestros$DENUNCIA, ]
        lista$reservas <- reservas.data[DENUNCIA %in% lista$siniestros$DENUNCIA, ]
      }
    }, silent = TRUE)
    try({lista$cie10 <- cie10.data[COD_ASO %in% lista$siniestros$CIE10, ]}, silent = TRUE)
    try({lista$ibner <- ibner.data[CLASE %in% lista$contratos$CLASE, ]}, silent = TRUE)
    try({lista$UC <- UC.data[UC %in% lista$contratos$UC, ]}, silent = TRUE)
    try({lista$pasUC <- pasUC.data[UC %in% lista$contratos$UC, ]}, silent = TRUE)
    try({lista$pas <- pas.data[CUIT %in% lista$pasUC$CUIT, ]}, silent = TRUE)
    try({lista$ROL <- ROL.data[ROL_ID %in% lista$pasUC$ROL_ID, ]}, silent = TRUE)
    try({lista$CIIU <- CIIU.data[CIIU_R2 %in% lista$contratos$CIIU_R2, ]}, silent = TRUE)
    try({lista$mercado <- mercado.data[CIIU_R2 %in% lista$CIIU$CIIU_R2, ]}, silent = TRUE)
    try({lista$tarifa_clase <- tarifa_clase.data[CLASE %in% lista$contratos$CLASE, ]}, silent = TRUE)
    try({lista$titulo_clase <- titulo_clase.data[CLASE3 %in% lista$tarifa_clase$CLASE, ]}, silent = TRUE)
    try({lista$sucursal <- sucursal.data[SUC_ID %in% lista$contratos$SUC, ]}, silent = TRUE)
    try({lista$provincia <- provincia.data[ID %in% lista$contratos$PCIA, ]}, silent = TRUE)
    
  }
  
  lista[!sapply(lista,is.null)]

}
