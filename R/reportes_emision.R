reportes_emision <- function(
  data,
  contratos_x_cluster,
  mes_min,
  mes_corte_datos,
  mes_rolling
){

  ## reportes siniestralidad y emision ----
  emision <- calcula_emisionConMovimientos(
    reportes = c("rpt1", "rpt2"),
    data = data(),
    contratos_x_cluster = contratos_x_cluster,
    columnas_reportes = list(
      rpt1 = c(
        "CONTRATO", "CLIENTE", "INIVIG", "MES", "PER", "TRABAJADORES_r",
        "SALPROM", "PRIMA_r", "COMI_r"
      ),
      rpt2 = c(
        "CONTRATO", "MES", "PER", "MESES",
        "TRABAJADORES", "TRABAJADORESMESsinDOM", "TRABAJADORESMES",
        "MASA_desest", "TRABAJADORESMES_r", "MASA_r",
        "PREMIO_r", "PRIMA_r", "COMI_r"
      )
    ),
    modoRectificativa = "A",
    mesMin = mes_min(),
    mesMax = mes_corte_datos,
    cierreDeMes = mes_rolling()
  )

  return(emision)

}
