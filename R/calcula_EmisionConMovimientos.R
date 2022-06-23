#' wrapper de EmisiónconMovimientos
#'
#' este código debería insertarse en la misma función.
#' Se itera por cluster y se eliminan columnas innecesarias
#'
#' @param reportes  vector con nombres de reportes
#' @param data  list con todos las tablas necesarias para EmisionConMovimientos
#' @param contratos_x_cluster  integer estimado de contratos por cluster
#' @param columnas_reportes  character-list  lista nominada, con el nombre de las columnas
#' @param ...  restantes parámetros de EmisiónconMovimientos
#'
#' @return
#' @export
#'
#' @examples
#'
#' filtroContratos <- filtrar_nros_contrato(
#' "provincias", 5
#' )
#'
#' grupo <- filtrar_tablas(
#'   filtroContratos = filtroContratos
#' )
#'
#' rdo <- calcula_emisionConMovimientos(
#'  reportes = c("rpt1", "rpt2"),
#'   contratos_x_cluster = contratos_x_cluster,
#'  data = grupo,
#'   columnas_reportes = list(
#'     rpt1 = c(
#'       "CONTRATO", "CLIENTE", "INIVIG", "MES", "PER", "SALPROM", "PRIMA_r", "COMI_r"
#'    ),
#'     rpt2 = c(
#'      "CONTRATO", "MES", "PER", "MESES", "TRABAJADORES", "TRABAJADORESMESsinDOM",
#'       "TRABAJADORESMES", "MASA_desest", "TRABAJADORESMES_r", "MASA_r", "PREMIO_r",
#'       "PRIMA_r", "COMI_r"
#'     )
#'   ),
#'   modoRectificativa = "A"
#' )
#'
calcula_emisionConMovimientos <- function(
  reportes,
  data,
  contratos_x_cluster,
  columnas_reportes = NULL,
  ...
){

  tablas_faltantes <- setdiff(
    c("emiorig", "contratos", "sucursal"),
    names(data)
  )

  if(length(tablas_faltantes) != 0) stop(glue(
    "calcula_emisionConMovimientos: falta alguna de las siguientes tablas: ",
    tablas_faltantes
  ))


  if(
    length(data$contratos$CONTRATO) < contratos_x_cluster
  ){

    pars <- list(...)

    rpt <- do.call(
      EmisionConMovimientos,
      c(
        list(
          reportes = reportes,
          emiorig = data$emiorig,
          rectificativa = data$rectificativa,
          domesticas = data$domesticas,
          contratos = data$contratos,
          sucursal = data$sucursal
        ),
        pars
      )
    )

  } else {

    clusters_contratos <- split(
      data$contratos$CONTRATO,
      ceiling(seq_along(data$contratos$CONTRATO)/contratos_x_cluster)
    )

    n_cluster <- length(clusters_contratos)

    rpt <- lapply(
      clusters_contratos,
      function(x){
        EmisionConMovimientos(
          reportes = reportes,
          emiorig = data$emiorig[CONTRATO %in% x],
          rectificativa = data$rectificativa[CONTRATO %in% x],
          domesticas = data$domesticas[CONTRATO %in% x],
          contratos = data$contratos[CONTRATO %in% x],
          sucursal = data$sucursal,
          ...
        )
      }
    )

    rpt <- do.call(Map, c(list, rpt))[reportes]

    rpt <- map(rpt, rbindlist)

  }

  # achico reportes
  if(!is.null(columnas_reportes)){
    rpt[] <- lapply(
      reportes,
      function(x){
        if(!is.null(columnas_reportes[[x]])){
          if(inherits_any(rpt[[x]], "data.table")){
            rpt[[x]][, .SD, .SDcols = columnas_reportes[[x]]]
          } else {
            select(rpt[[x]], all_of(columnas_reportes[[x]]))
          }
        } else {
          rpt[[x]]
        }
      }
    )
  }

  return(rpt)

}
