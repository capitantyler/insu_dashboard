#' wrapper de genera_reporte_siniestros
#'
#' este código debería insertarse en la misma función.
#' Se itera por cluster y se eliminan columnas innecesarias
#'
#' @param reportes  vector con nombres de reportes
#' @param data  list con todos las tablas necesarias para Egenera_reportes_siniestros
#' @param contratos_x_cluster  integer estimado de denuncias por cluster
#' @param columnas_reportes  character-list  lista nominada, con el nombre de las columnas
#' @param ...  restantes parámetros de genera_reportes_siniestros
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
#' rdo <- calcula_reporte_siniestros(
#'   reportes = c("rpt1", "rpt2"),
#'   denuncias_x_cluster = contratos_x_cluster,
#'   data = grupo,
#'   columnas_reportes = list(
#'     rpt1 = c(
#'       "CONTRATO", "CLIENTE", "INIVIG",  "MESACC",
#'      "ILT_RVA", "ESP_RVA", "ILP_RVA", "JUI_RVA", "TOT_RVA",
#'      "ILT_LIQ", "ESP_LIQ", "ILP_LIQ", "JUI_LIQ", "TOT_LIQ",
#'      "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC", "TOT_INC",
#'      "ILT_IBNER", "ESP_IBNER", "ILP_IBNER", "JUI_IBNER", "TOT_IBNER",
#'      "ILT_ULT", "ESP_ULT", "ILP_ULT", "JUI_ULT", "TOT_ULT"
#'    ),
#'    rpt2 = c(
#'      "CONTRATO",
#'      "ILT_RVA", "ESP_RVA", "ILP_RVA", "JUI_RVA", "TOT_RVA",
#'      "ILT_LIQ", "ESP_LIQ", "ILP_LIQ", "JUI_LIQ", "TOT_LIQ",
#'      "ILT_INC", "ESP_INC", "ILP_INC", "JUI_INC", "TOT_INC",
#'      "ILT_IBNER", "ESP_IBNER", "ILP_IBNER", "JUI_IBNER", "TOT_IBNER",
#'      "ILT_ULT", "ESP_ULT", "ILP_ULT", "JUI_ULT", "TOT_ULT"
#'     )
#'   ),
#'   mesMin = 201901,
#'   mesMax = 202201
#' )
#'
calcula_reporte_siniestros <- function(
    reportes,
    data,
    denuncias_x_cluster,
    columnas_reportes = NULL,
    ...
){

  tablas_faltantes <- setdiff(
    c("emiorig", "contratos", "sucursal"),
    names(data)
  )

  if(length(tablas_faltantes) != 0) stop(glue(
    "calcula_reporte_siniestros: falta alguna de las siguientes tablas: ",
    tablas_faltantes
  ))


  if(
    length(data$siniestros$DENUNCIA) < denuncias_x_cluster
  ){

    rpt <- do.call(
      genera_reportes_siniestros,
      c(
        list(
          reportes = reportes,
          siniestros = data$siniestros,
          liquidaciones = data$liquidaciones,
          reservas = data$reservas,
          juicios = data$juicios,
          contratos = data$contratos
        ),
        ...
      )
    )

  } else {

    clusters <- data$siniestros[,
      .N, by = .(CONTRATO)
    ][ order(-N)][,
      freq_cto_acum := cumsum(N) / sum(N)
    ][,
      id := cut(
        freq_cto_acum,
        breaks = ceiling(sum(N)/denuncias_x_cluster),
        labels = FALSE
      )
    ][,
      .(CONTRATO, id)
    ]

    clusters_contratos <- split(
      clusters,
      clusters$id
    )

    n_cluster <- length(clusters_contratos)

    # progressSweetAlert(
    #   session = session,
    #   id = "ui_progress_bar_clusters",
    #   title = "Procesando...",
    #   status = "info",
    #   display_pct = TRUE, value = 0
    # )

    rpt <- lapply(
      clusters_contratos,
      function(x){

        denuncias_en_cluster <- unique(
          data$siniestro[CONTRATO %in% x[["CONTRATO"]]][["DENUNCIA"]]
        )

        # updateProgressBar(
        #   session = session,
        #   id = "ui_progress_bar_clusters",
        #   value = cluster / n_cluster * 100,
        # )

        do.call(
          genera_reportes_siniestros,
          c(
            list(
              reportes = reportes,
              siniestros = data$siniestros[DENUNCIA %in% denuncias_en_cluster],
              liquidaciones = data$liquidaciones[DENUNCIA %in% denuncias_en_cluster],
              reservas = data$reservas[DENUNCIA %in% denuncias_en_cluster],
              juicios = data$juicios[DENUNCIA %in% denuncias_en_cluster],
              contratos = data$contratos[CONTRATO %in% x[["CONTRATO"]]]
            ),
            ...
          )
        )

      }
    )

    rpt <- do.call(Map, c(list, rpt))[reportes]

    rpt <- map(rpt, rbindlist, use.names=TRUE)

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
