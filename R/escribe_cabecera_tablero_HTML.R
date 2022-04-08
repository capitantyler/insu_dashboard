#' escribe_cabecera_tablero_HTML
#'
#' generación de cabecera del tablero integral
#'
#' @param grupo        list           conjunto de datos asociados
#' @param grupo_tipo   character      tipo de filtro
#'
#' @return
#' @export
#'
#' @note cuando grupo_tipo refiere a un cliente, se traen todos los contratos
#' asociados a sus cuits.
#' TODO. falta agregar unos puntos suspensivos si supera
#'       el número límite de contratos a describir
#'
#' @examples
escribe_cabecera_tablero_HTML <- function(
  grupo,
  grupo_tipo
){

  # funciones auxiliares y constantes----

  combina_tablas_contrato <- function(
    grupo,
    ...,
    limite_contratos = 20L
  ){

    lenCIIU <- 50L  #largo de cadena de descripción de CIIU
    by_quosure <- quos(...)

    # forma descripciones de los campos de contrato.
    tabla <- grupo$contratos %>%
      rename(
        SUC_ID = SUC,
        TRABAJADORES = CAPITAS
      ) %>%
      inner_join(
        grupo$sucursal %>% select(SUC_ID, SUC),
        by = "SUC_ID"
      ) %>%
      inner_join(
        grupo$CIIU %>% select(CIIU_R2, CIIU_R2_DESC),
        by = "CIIU_R2"
      ) %>%
      mutate(
        CIIU_R2 = paste(
          CIIU_R2,
          str_trunc(CIIU_R2_DESC, width = !!lenCIIU, ellipsis = "..."),
          sep = "-"
        )
      )

    tabla <- tabla %>%
      arrange(
        desc(TRABAJADORES)
      ) %>%
      mutate(
        # DESCRIPCION: como va con colores y títulos, formateo previamente
        # cada línea con div_contratos
        # tuve que usar broom::pmap porque mutate directo no funciona
        # esto es porque las funciones que agregan HTML muchas de ellas
        # no estan vectorizadas.
        # si se quiere guardar una columna con un resultado concreto, hay que
        # vectorizarla.
        # En todo momento hay que pasar las funciones de htmltools con purrr:map

        # Igual es importante aprender broom. Basicamente lo que hace es
        # como un for o apply sobre una función .f
        # ..1, ..2, ..3, ..4 los paso en la lista

        DESCRIPCION = pmap(
          .l = list(
            CONTRATO, CLIENTE, SUC, CIIU_R2,
            INIVIG, RESCISION,
            TRABAJADORES, ESTADO, VBLE
          ),
          .f = ~ div_contratos(
            CONTRATO = ..1, CLIENTE = ..2, SUC = ..3, CIIU = ..4,
            INIVIG = ..5, RESCISION = ..6,
            TRABAJADORES = ..7, ESTADO = ..8, VBLE = ..9
          )
        )
      ) %>%
      group_by(
        !!!by_quosure
      ) %>%
      nest()

    tabla <- tabla %>%
      mutate(
        vigentes = map(
          .x = data,
          .f = ~ filter(., ESTADO %in% c(2, 3)) %>%
            summarise(
              TRABAJADORES = sum(TRABAJADORES),
              VBLE = if_else(
                sum(MASA) == 0,
                0,
                sum(MASA * VBLE) / sum (MASA)
              )
            )
        ),
        DESCRIPCION = map(
          .x = data,
          .f = ~ div_append_divs(
            # falta agregar unos puntos suspensivos si supera
            # el número límite de contratos a describir
            .x[["DESCRIPCION"]][
              1:min(length(.x[["DESCRIPCION"]]),limite_contratos)
            ]
          )
        )
      ) %>%
      unnest_wider(vigentes) %>%
      arrange(
        desc(TRABAJADORES)
      ) %>%
      mutate(
        RESCINDIDO = if_else(
          sum(TRABAJADORES) == 0, TRUE, FALSE
        ),
        VBLE = percent(VBLE / 100, accuracy = 0.01),
        TRABAJADORES = number(
          TRABAJADORES, big.mark = ".", decimal.mark = ",",
          suffix = " TR"
        )
      )

    return(tabla)

  }


  div_contratos <- function(
    CONTRATO, CLIENTE, GRUPO_EC,
    SUC, CIIU, INIVIG, RESCISION,
    TRABAJADORES, ESTADO, VBLE, ...
  ){
    #' Crea una cadena con datos del contrato, coloacando en rojo si estpa rescindido
    #'
    #'
    mydiv <- div(
      class = "parent", id = "contrato",
      strong(CONTRATO),
      HTML("&rarr;"), CLIENTE, HTML(" &diams;"),
      if(!missing("GRUPO_EC"))
        if(GRUPO_EC != "SD") span(
        span(GRUPO_EC, style="font-style:italic"), HTML(" &diams;")
      ),
      span(SUC, style="color:Teal"), HTML(" &diams;"),
      span(CIIU, style="color:MediumVioletRed"), HTML(" &diams;"),
      span(
        "VIG:", INIVIG,
        if(RESCISION != date("1900-01-01")) span(HTML(" &ndash;"), RESCISION),
        style="color:OrangeRed"
      ),
      HTML(" &diams;"),
      number(TRABAJADORES, big.mark = ".", decimal.mark = ",", suffix = " TR"),
      HTML(" &diams;"),
      percent(VBLE/100, accuracy = 0.01)
    )

    if(ESTADO != 2) {
      mydiv <- tagAppendAttributes(
        mydiv,
        style = 'color:red'
      )
    }
    return(mydiv)
  }

  div_append_divs <- function(
    divs
    #...
  ){
    #' div_append_divs
    #'
    #' une conjunto de divs en una sola div contenedor
    #'
    #' @param divs
    #' @param ...
    #'
    #' @return
    #' @export
    #'
    #' @examples

    mydiv <- div(
      class = "parent", id = "contenedor"
    )

    mydiv <- tagSetChildren(
      mydiv,
      # list = `[`(divs)
      list = divs
    )

    return(mydiv)
  }
  # div_descripcion <- Vectorize(div_descripcion, c("DESCRIPCION"))


  out <- tryCatch ({

    if (grupo_tipo == "Asociart") {
      cabecera <- tags$span(
        tags$strong("ASOCIART ART")
      )
    }

    ## UC/Productor/Asesor/broker ----

    if (grupo_tipo == "pas" | grupo_tipo == "uc") {

      # agrego CUIT del PAS grupo$contrato
      grupo$contratos[
        grupo$pasUC, on = 'UC',
        `:=`(
          CUIT_PAS = i.CUIT,
          PRODUCTOR = i.PRODUCTOR
        )
      ]

      cabecera <- combina_tablas_contrato(
        grupo,
        CUIT_PAS, PRODUCTOR, UC
      )

      if(grupo_tipo == "uc"){
        cabecera <- cabecera %>%
          mutate(
            CABECERA = glue(
              '{if_else(RESCINDIDO == TRUE, "*RESCINDIDO - ", "")}',
              '{UC} con PAS {CUIT_PAS}:{PRODUCTOR}'
            )
          )
      } else {
        cabecera <- cabecera %>%
          mutate(
            CABECERA = glue(
              '{if_else(RESCINDIDO == TRUE, "*RESCINDIDO - ", "")}',
              '{CUIT_PAS}:{PRODUCTOR} en UC {UC}'
            )
          )
      }

      cabecera <- cabecera %>%
        ungroup() %>%
        select(
          CABECERA, DESCRIPCION, TRABAJADORES, VBLE
        )

    }

    ## Grupo Económico ----
    if (grupo_tipo == "grupo_ec") {

      # en una primera parte selecciono los datos y los acomodo para trabajar
      cabecera <- combina_tablas_contrato(
        grupo,
        GRUPO_EC,
        limite_contratos = 200L
      )

      cabecera <- cabecera %>%
        ungroup() %>%
        rename(
          CABECERA = GRUPO_EC
        ) %>%
        select(
          CABECERA, DESCRIPCION, TRABAJADORES, VBLE
        )

    }

    ## sucursal ----

    if (grupo_tipo == "sucursales") {

      grupo$contratos[
        grupo$sucursal, on = c(SUC = "SUC_ID"),
        `:=`(
          SUCURSAL = i.SUCURSAL,
          RESPONSABLE = i.RESPONSABLE
        )
      ]

      cabecera <- combina_tablas_contrato(
        grupo,
        SUC, SUCURSAL, RESPONSABLE,
        limite_contratos = 50L
      )

      cabecera <- cabecera %>%
        mutate(
          CABECERA = glue('{SUC}-{SUCURSAL}, jefe:{RESPONSABLE}')
        ) %>%
        ungroup() %>%
        select(
          CABECERA, DESCRIPCION, TRABAJADORES, VBLE
        )

    }

    if (grupo_tipo == "regiones") {
      # contratos región
      grupo$contratos[
        grupo$sucursal[
          SUC_TIPO == "Sucursal"
        ], on = c(REG = "ID_REG"),
        `:=`(
          REGION = i.REGION,
          RESPONSABLE = i.RESPONSABLE
        )
      ]

      cabecera <- combina_tablas_contrato(
        grupo,
        REG, REGION, RESPONSABLE,
        limite_contratos = 50L
      )

      cabecera <- cabecera %>%
        ungroup() %>%
        mutate(
          CABECERA = glue('{REG}-{REGION}, jefe:{RESPONSABLE}')
        ) %>%
        select(
          CABECERA, DESCRIPCION, TRABAJADORES, VBLE
        )

    }

    if (grupo_tipo == "provincias") {

      grupo$contratos[
        grupo$provincia, on = c(PCIA = "ID"),
        `:=`(
          PROVINCIA = i.PROVINCIA
        )
      ]

      cabecera <- combina_tablas_contrato(
        grupo,
        PCIA, PROVINCIA,
        limite_contratos = 50L
      )

      cabecera <- cabecera %>%
        ungroup() %>%
        mutate(
          CABECERA = glue('{PCIA}-{PROVINCIA}')
        ) %>%
        select(
          CABECERA, DESCRIPCION, TRABAJADORES, VBLE
        )

    }


    if (grupo_tipo == "CIIUR2") {

      grupo$contratos[
        CIIU.data, on = 'CIIU_R2',
        `:=`(
          CIIU_R2_DESCR = i.CIIU_R2_DESC
        )
      ]

      cabecera <- combina_tablas_contrato(
        grupo,
        CIIU_R2, CIIU_R2_DESCR,
        limite_contratos = 50L
      )

      cabecera <- cabecera %>%
        ungroup() %>%
        mutate(
          CABECERA = paste(
            CIIU_R2,
            substr(CIIU_R2_DESCR, 1, 50),
            sep = " - "
          )
        ) %>%
        select(
          CABECERA, DESCRIPCION, TRABAJADORES, VBLE
        )

    }

    if (grupo_tipo == "actividad x20") {

      grupo$contratos[
        grupo$titulo_clase, on = c(CLASE = "CLASE3"),
        `:=`(
          CLASE1 = i.CLASE1,
          CLASE1_ETIQUETA = i.CLASE1_ETIQUETA
        )
      ]

      cabecera <- combina_tablas_contrato(
        grupo,
        CLASE1
      )

      cabecera <- cabecera %>%
        ungroup() %>%
        mutate(
          CABECERA = substr(CLASE1_ETIQUETA, 1, 50)
        ) %>%
        select(
          CABECERA, DESCRIPCION, TRABAJADORES, VBLE
        )

    }

    if (grupo_tipo == "actividad xCIIU_V1") {

      grupo$contratos[,
        CIIU_R2_1d := CIIU_R2 %/% 100000
      ][
        unique(CIIU.data, by = "CIIU_R2_1d_DESC"), on = 'CIIU_R2_1d',
      ]

      cabecera <- combina_tablas_contrato(
        grupo,
        CIIU_R2_1d, CIIU_R2_1d_DESC,
        limite_contratos = 50L
      )

      cabecera <- cabecera %>%
        ungroup() %>%
        mutate(
          CABECERA = CIIU_R2_1d_DESC
        ) %>%
        select(
          CABECERA, DESCRIPCION, TRABAJADORES, VBLE
        )

    }

    ## Clase de actividad ----
    if (grupo_tipo == "clase_3") {

      cabecera <- combina_tablas_contrato(
        grupo,
        CLASE,
        limite_contratos = 30L
      )

      cabecera <- cabecera %>%
        ungroup() %>%
        mutate(
          CABECERA = substr(CLASE3_ETIQUETA, 1, 50)
        ) %>%
        select(
          CABECERA, DESCRIPCION, TRABAJADORES, VBLE
        )

    }

    ## contratos y CUITs ----
    if (grupo_tipo == "contratos" | grupo_tipo == "cuit") {

      # en una primera parte selecciono los datos y los acomodo para trabajar
      cabecera <- combina_tablas_contrato(
        grupo,
        CUIT,
        limite_contratos = 500L
      )

      cabecera <- cabecera %>%
        mutate(
          CABECERA = if_else(
            RESCINDIDO == TRUE,
            paste(
              "RESCINDIDO",
              CUIT,
              map(
                .x = data,
                .f = ~ slice_head(.) %>% pull(CLIENTE)
              ),
              sep = " - "
            ),
            paste(
              CUIT,
              map(
                .x = data,
                .f = ~ filter(., ESTADO %in% c(2, 3)) %>%
                  slice_head() %>% pull(CLIENTE)
              ),
              sep = " - "
            )
          )
        ) %>%
        ungroup() %>%
        select(
          CABECERA, DESCRIPCION, TRABAJADORES, VBLE
        )

    }

    # parte render HTML ----
    if (grupo_tipo != "Asociart"){
      cabecera <- map(seq_len(nrow(cabecera)), function(i) {
        fluidRow(
          column(10,
             box(width = 12,
                 id = "box_detalle", class = "box_cabecera",
                 background = "aqua",
                 solidHeader = TRUE,
                 title = cabecera[["CABECERA"]][[i]],
                 lapply(
                   cabecera[i, 2][[1]],
                   function(x) HTML(as.character(x))
                 )
             )
          ),
          column(1, box(width = 12,
            id = "box_trabajadores", class = "box_cabecera",
            cabecera[["TRABAJADORES"]][[i]]
          )
          ),
          column(1, box(width = 12,
            id = "box_tasa", class = "box_cabecera",
            cabecera[["VBLE"]][[i]]
          )
          )
        )
      })

    }

    return(cabecera)

  },
  error = function (x) {
    message ("Ocurrió un error en escribe_cabecera_tablero_HTML.R: ")
    message (x)
    # retorno un mensaje en caso de error
    return (NA)
  })

  return (out)

}
