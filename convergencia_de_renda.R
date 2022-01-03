library(tidyverse)
library(devtools)
install.packages('gridExtra')
library(gridExtra)
library(grid)

df <- read_csv('ipeadata[09-11-2021-08-32].csv', 
               skip=2, col_names = c('sigla','cod', 'mun', 'renda_0', 'renda_t'), 
               col_types = c(col_character(), col_character(), col_character(), col_double(), col_double()),
               locale= locale(encoding='UTF-8'))


df2 <- df %>% mutate(var = renda_t/renda_0, log_0 = log(renda_0), log_var = log(var)/9)

estados <- c('SC', 'RJ', 'SP', 'AM', 'MA', 'PE', 'MT', 'GO', 'PA', 'AL')

df_final <- df2 %>% filter(sigla %in% estados) %>% split(.$sigla)
 

modelos <- map(df_final, ~lm(log_var ~ log_0, data = .x))
coeficientes <- modelos %>% map(~.$coefficients)


graficos <- df_final %>% map2(names(.), ~ggplot(mapping = aes(x= log_0, y= log_var), data=.x) + 
                   geom_point(color = 'red') + 
                   geom_smooth(method = 'lm') +
                   labs(title = paste('Convergência de renda -',.y)) + 
                     xlab('ln(Renda1991)') + 
                     ylab('Taxa de crescimento médio'))
                 
r_squared <- modelos %>% map(summary) %>% map_dbl(~.$r.squared) 
summaries <- modelos %>% map(summary)
p_valor_beta <- summaries %>% map(~.$coefficients[2 , 4])

view(r_squared)


