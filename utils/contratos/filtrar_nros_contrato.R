#' filtrar_nros_contrato
#'
#' devuelve un vector de números de contrato filtrados por la condición
#' grupo.tipo = grupo.valor.
#'
#' @param grupo.tipo   character  tipo de filtro
#' @param grupo.valor  indeterminado  lista de valores a machear
#'
#' @return
#' @export
#'
#' @note cuando grupo.tipo refiere a un cliente, se traen todos los contrtos
#' asociados a sus cuits
#'
#' @examples
filtrar_nros_contrato <- function(
  grupo.tipo,
  grupo.valor
){

  split_grupovalor <- function(x, pattern = " |,|;"){
    x <- unique(
      str_split(as.character(x), pattern, simplify = TRUE)
    )
  }

  filtroContratos <- tryCatch({
    switch (
      grupo.tipo,
      "Asociart" = contratos.data$CONTRATO,
      "cuit" = contratos.data$CONTRATO[
          contratos.data$CUIT %in% split_grupovalor(grupo.valor)
        ],
      "contratos" = {
        contratos.data$CONTRATO[
          contratos.data$CUIT %in% contratos.data$CUIT[
            contratos.data$CONTRATO %in% as.numeric(split_grupovalor(grupo.valor))
          ]
        ]
      },
      "grupo_ec" = contratos.data$CONTRATO[
        contratos.data$GRUPO_EC %in% grupo.valor
        ],
      "uc" = contratos.data$CONTRATO[
        contratos.data$UC %in% as.numeric(split_grupovalor(grupo.valor))
        ],
      "sucursales" = contratos.data$CONTRATO[
        contratos.data$SUC %in% as.numeric(grupo.valor)
        ],
      "regiones" = contratos.data$CONTRATO[
        contratos.data$REG %in% as.numeric(grupo.valor)
        ],
      "provincias" = contratos.data$CONTRATO[
        contratos.data$PCIA %in% as.numeric(grupo.valor)
        ],
      "CIIUR2" = contratos.data$CONTRATO[
        contratos.data$CIIU_R2 %in% as.numeric(grupo.valor)
        ],
      "CIIUR2_1d" = contratos.data$CONTRATO[
        contratos.data$CIIU_R2 %/% 100000 %in% as.numeric(grupo.valor)
      ],
      "clase_3" = contratos.data$CONTRATO[
        contratos.data$CLASE %in% grupo.valor
        ],
      "clase_1" = contratos.data$CONTRATO[
        contratos.data$CLASE %in% titulo_clase.data$CLASE3[
          titulo_clase.data$CLASE1 %in% grupo.valor
          ]
        ],
      "pas" = contratos.data$CONTRATO[
        contratos.data$UC %in% pasUC.data$UC[
          pasUC.data$CUIT %in% as.numeric(split_grupovalor(grupo.valor))
          ]
        ],
      stop()
    )

  },

  error = function(cond) {
    message(paste(
      "Ocurrió un error al pasar estos argumentos a filtrar_nros_contrato",
      "grupo.tipo", grupo.tipo,
      "grupo.valor", grupo.valor
    ))
    message("Se devuelve 0. Mensaje original de error:")
    message(cond)

    # Choose a return value when such a type of condition occurs
    return(NA_integer_)
  }

  )

  return(filtroContratos)

}
