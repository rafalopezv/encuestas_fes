library(highcharter)

morales %<>% 
  arrange(fecha_conclusion_encuesta) %>% 
  mutate(num = 1:nrow(.))



models <- loess(valor ~ num, data = morales)
fit <- arrange(augment(models), num)
fit %<>% mutate_if(is.numeric, round, 3) 

highchart() %>%
  hc_add_series(morales, type = "scatter", name = "Evo Morales", 
                color = 'blue',
                hcaes(x = fecha_1, y = valor),
                tooltip = list(pointFormat = paste("<b>Evo Morales:<b> {point.valor} %<br>
                                                         <b>Fecha de cierre encuesta:<b> {point.fecha_1}<br>
                                                         <b>Encuestadora:<b> {point.encuestadora_1}<br>
                                                         <b>Margen de error real:<b> {point.margen_error} %<br>
                                                         <b>Tamaño de la muestra:<b> {point.alcance_muestra}"
                ), headerFormat = "")) %>%
  hc_add_series(fit, type = "line", hcaes(x = num, y = .fitted),
                name = "Predicción", id = "fit", color = 'black') %>%
  hc_add_series(fit, type = "arearange",
                hcaes(x = num, low = .fitted - 3.03,
                      high = .fitted + 3.03),
                linkedTo = "fit") %>% 
  hc_xAxis(categories = morales$fecha_1,
           tickmarkPlacement = "on",
           title = list(enabled = T)) %>% 
  hc_yAxis(title = list(text = "Intención de voto")) %>% 
  hc_title(text = "Predicción voto a Evo Morales") %>% 
  hc_subtitle(text = "subtitulo del gráfico x") %>% 
  hc_plotOptions(line = list(
    marker = list(
      lineWidth = 1/100,
      lineColor = "#ffffff",
      enabled = F
    ))
  ) %>% 
  hc_plotOptions(arearange = list(
    marker = list(
      lineWidth = 1/100,
      lineColor = "#ffffff",
      enabled = F
    ))
  ) %>% 
  hc_add_theme(hc_theme_gridlight()) %>% 
  htmlwidgets::saveWidget(here::here("img", "prototipo_prediccion.html"))
  

morales %>% View
