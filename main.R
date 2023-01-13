
# 0) setup ----------------------------------------------------------------

library(lubridate)
library(tidyverse)
options(scipen = 999)

# 1) data -----------------------------------------------------------------

gastos <- readr::read_csv2("data/Planilha12003a2022.csv", locale = readr::locale(encoding = "latin1")) %>%
	janitor::clean_names() %>% 
	slice(1:(n() - 2)) %>% 
	mutate(
		data_pgto = dmy(data_pgto),
		ano_mes_data = floor_date(data_pgto, unit = "1 month"),
		ano = year(data_pgto),
		presidente = case_when(
			ano <= 2010 ~ "Lula",
			ano <= 2016 ~ "Dilma",
			ano <= 2018 ~ "Temer",
			ano <= 2022 ~ "Biroliro"
		)
	) %>%
	mutate(valor = valor %>% str_remove("R\\$ ") %>% 
		   	parse_number(locale = readr::locale(decimal_mark = ",")),
		   valor_corrigido = deflateBR::deflate(valor, data_pgto, "01/2003", "ipca"))
	


# 2) gráficos -------------------------------------------------------------

gastos %>%
	group_by(presidente, ano) %>%
	summarise(valor = sum(valor_corrigido, na.rm = TRUE)/1000000) %>%
	ggplot(aes(x = factor(ano), y = valor, fill = presidente)) +
	geom_col() +
	labs(x = "Ano", y = "Gasto (em milhões de Reais - IPCA)") +
	scale_fill_manual(name = "Presidente",
					  values = viridisLite::viridis(n = 4)) +
	theme_linedraw()


