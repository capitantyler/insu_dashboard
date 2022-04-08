#' Función agrega_periodo_frecuencias ----
#'
#' Calcula FRSA total, de graves y de juicios, sin y con IBNR
#'
#' @param emision       data.frame  emisión original
#' @param siniestros    data.frame  siniestros agregados por período y contrato
#' @param columnas_agrupar    character   columnas para agrupar
#'
#' @return              data.frame  frecuencias por período
#' @export
#'
#' @examples
agrega_periodo_frecuencias <- function(
  emision,
  siniestros
){

  frecuencias <- emision %>%
      select(
        PER, TRABAJADORESMES_r
      ) %>%
      left_join(
        siniestros %>% select(PER, N, N_ULT, GR, MU, JU, JN),
        by = "PER"
      ) %>%
      mutate(
        across(
          c(N, GR, JU, JN),
          .fns = ~ if_else(TRABAJADORESMES_r == 0L, 0, . / TRABAJADORESMES_r * 12),
          .names = "FRSA_{col}"
        ),
        across(
          c(starts_with("FRSA"), GR, MU, JU),
          .fns = ~ . * N_ULT / N,
          .names = "{col}_FINAL"
        ),
        FRSA_N_IBNR = FRSA_N_FINAL - FRSA_N,
        FRSA_GR_IBNR = FRSA_GR_FINAL - FRSA_GR,
        ## Agrego desarrollo de juicios. Buscar la función de calcula_frsa_ju_final
        JU_FINAL = calcula_frsa_ju_final(JU_FINAL, JN),
        FRSA_JU_FINAL = calcula_frsa_ju_final(FRSA_JU_FINAL, FRSA_JN),
        FRSA_JU_IBNR = FRSA_JU_FINAL - FRSA_JU
      ) %>%
      rename(
        N_FINAL = N_ULT
      ) %>%
      select(
        PER,
        # N, GR, MU, JU,
        N_FINAL, GR_FINAL, MU_FINAL, JU_FINAL,
        FRSA_N_FINAL, FRSA_GR_FINAL, FRSA_JU_FINAL
      )

  return(frecuencias)

}
