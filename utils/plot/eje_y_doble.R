#' eje_y_doble
#' 
#' Combina dos gráficos en uno solo con dos ejes de ordenadas.
#' Toma del gráfico p1 todos los elementos gráficos. 
#' Del gráfico p2 toma su eje y todods sus elementos gráficos que
#' no entren en conclicto con el gráfico p1.
#' Las leyendas y título principal se combinan.
#' 
#' Fuente principal: 
#' https://stackoverflow.com/questions/37984000/how-to-manage-the-t-b-l-r-coordinates-of-gtable-to-plot-the-secondary-y-axi
#' 
#' Fuente de ayuda (no pficial): 
#' https://cran.r-project.org/web/packages/gridExtra/vignettes/gtable.html#components-of-a-gtable
#'
#' @param p1 gráfico principal y los principales formatos
#' @param p2 gráfico con el eje secundario
#'
#' @return
#' @export
#'
#' @examples
eje_y_doble <- function(
  p1, 
  p2
){
  
  library(ggplot2)
  library(gtable)
  library(grid)
  library(scales)
  library(dplyr)
  
  out <- tryCatch(
    expr = {
      
      # elimino elementos de p2 qu ese superpondrán con p1
      
      p2 <- p2 + theme(
        # panel.background = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(fill = NA)
      )
      
      # obtengo los grobs de cada uno de los gráficos
      g1 <- ggplotGrob(p1)
      g2 <- ggplotGrob(p2)
      
      # obtengo la localización de los paneles de grob g2 
      # y la superpongo sobre grab g2
      pp <- c(
        subset(g1$layout, grepl("panel", g1$layout$name), select = t:r)
      )
      g <- gtable_add_grob(
        g1, 
        g2$grobs[grepl("panel", g1$layout$name)], 
        pp$t, pp$l, pp$b, pp$l
      )
      
      # función para invertir etiquetas
      hinvert_title_grob <- function(grob){
        if(grob$name != "NULL"){
          widths <- grob$widths
          grob$widths[1] <- widths[3]
          grob$widths[3] <- widths[1]
          grob$vp[[1]]$layout$widths[1] <- widths[3]
          grob$vp[[1]]$layout$widths[3] <- widths[1]
          
          grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
          grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
          grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
        }
        grob 
      }
      
      # obtengo la etiqueta del eje y del grob g2, y la invierto
      index <- which(g2$layout$name == "ylab-l") 
      ylab <- g2$grobs[[index]]           # etiqueta_y extraída
      ylab <- hinvert_title_grob(ylab)    
      
      # colocar la etiqueta del eje_y en el nuevo grob g, a la derecha
      # del panel más echado a la derecha
      # Nota: sólo una columna y una etiqueta
      g <- gtable_add_cols(
        g, 
        g2$widths[g2$layout[index, ]$l], 
        pos = max(pp$r)
      )
      
      g <- gtable_add_grob(
        g,
        ylab, 
        t = min(pp$t), l = max(pp$r)+1, 
        b = max(pp$b), r = max(pp$r)+1,
        clip = "off", name = "ylab-r"
      )
      
      # obtener el eje_y del grob g2, invertir las marcas (ticks) y 
      # sus correspondientes etiquetas
      
      # index <- which(g2$layout$name == "axis-l-1-1")  # Which grob
      
      index <- g2$layout %>% 
        rowid_to_column() %>% 
        filter(
          grepl("axis-l.*", name)
        ) %>% 
        filter(
          l == min(l) & t == min(t)
        ) %>% 
        pull(rowid)            # Which grob
      
      
      yaxis <- g2$grobs[[index]]                    # Extract the grob
      
      ticks <- yaxis$children[[2]]
      ticks$widths <- rev(ticks$widths)
      ticks$grobs <- rev(ticks$grobs)
      
      plot_theme <- function(p) {
        plyr::defaults(p$theme, theme_get())
      }
      
      tml <- plot_theme(p1)$axis.ticks.length   # Tick mark length
      ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
      
      ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
      yaxis$children[[2]] <- ticks
      
      # Put the y axis into g, to the right of the right-most panel
      # Note: Only one column, but two y axes - one for each row of 
      # the facet_wrap plot
      g <- gtable_add_cols(
        g, 
        g2$widths[g2$layout[index, ]$l], 
        pos = max(pp$r)
      )
      
      nrows = length(unique(pp$t)) # Número de filas únicas de facet
      g <- gtable_add_grob(  
        g, 
        rep(list(yaxis), nrows), 
        t = unique(pp$t), l = max(pp$r)+1,
        b = unique(pp$b), r = max(pp$r)+1, 
        clip = "off", name = if_else(
          nrows == 1,
          "axis-r",
          paste0("axis-r-", 1:nrows)
        )
      )
      
      # obtiene las leyendas
      if(is.element("guide-box", g1$layout$name)){
        leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
        # Combina las leyendas
        g$grobs[[which(g$layout$name == "guide-box")]] <- leg1
        if(is.element("guide-box", g1$layout$name)){
          leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]
          # Combina las leyendas
          g$grobs[[which(g$layout$name == "guide-box")]] <-
            gtable:::cbind_gtable(leg1, leg2, "first")
        }
      }
      g
    },
    error = function(e) {
      message(
        paste(
          "error al realizar doble eje_y en gráfico", 
          paste(format(p1$mapping), collapse = ""),
          paste(format(p2$mapping), collapse = ""),
          sep = " y "
        )
      )
      message("Mensaje de error en R:")
      message(e)
      # valor de retorno en caso de error
      return(NA)
    },
    warning = function(w) {
      message(
        paste(
          "warning al realizar doble eje_y en gráfico", 
          paste(format(p1$mapping), collapse = ""),
          paste(format(p2$mapping), collapse = ""),
          sep = " y "
        )
      )
      message("Mensaje de warning en R:")
      message(w)
      # valor de retorno en caso de error
      return(NA)
    }
  )
  return(out)
}

