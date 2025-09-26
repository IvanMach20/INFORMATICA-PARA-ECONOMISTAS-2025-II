*========================================================================
*Facultad de Economía
*Universidad Nacional del Centro del Perú
*Curso: Informática para economistas - III ciclo
*SEMANA 6: STATA para Economistas 1 (Conociendo y jugando con comandos básicos)
*Docente: MSc. Ciro Ivan Machacuay Meza
*Correo: cmachacuay@uncp.edu.pe
*========================================================================

*========================================================================
*1. Area de trabajo
*========================================================================
*ctrl + d    >> ejecutar lineas
clear all    

*Al aplicar este comando se le esta indicando a STATA que todo lo trabajado
*estará en la ruta indicada
cd "C:\Users\USER\Desktop\2025 II\INFORMATICA PARA ECONOMISTAS\UNIDAD 1\SEMANA 6"

*========================================================================
*cargamos la base de datos
*========================================================================

*Alternativa 1:Usando el comando "use" que te va cargar datos en archivo dta.
use "base_ninis.dta", clear

*Alternativa 2:Se puede hacer a traves de pestañas pero el archivo debe estar en 
*formato excel (caso aplicativo data EGP)

*Alternativa 3: Se puede hacer de otras formas (investigar cuales son esas formas)

*para cambiar el tipo en propiedades de datos
recast byte nini, force

*para guardar los resultados trabajados en STATA.
save resultados.dta

*Para mostrar la base de datos
browse

*Tambien se puede mostrar la base de datos a traves de pestañas
*Se puede mostrar la base de datos a la cantidad de observaciones que uno desee
list in 1/50

*Si hay variables categoricas se puede conocer las categroias
label list nini
label list region

*Práctica para el alumno, hacer bucles con todas las variables categoricos

*========================================================================
*Comandos útiles para trabajar con tipos de datos:
*========================================================================
*Muestra los tipos de todas las variables
describe 

*Proporciona información detallada sobre las variables
codebook 

**Generar nuevas variables
* Variable numérica simple
generate nueva_var = 10

* Copiar valores de otra variable
generate copia = sexo

* Operaciones matemáticas
generate suma = gasto + fac500a
generate producto = gasto * fac500a
generate division = gasto / fac500a

**Eliminar variables
drop division

* Eliminar varias variables
drop ariable1 variable2 variable3


* Crear variable de cualitativa a cuantitativa
generate sex_num = 0 if sexo == 0
replace sex_num = 1 if sexo == 1

* Crear variable de cuantitativa a cualitativa
generate sex_cat = 1 if sexo == 1  // Hombre = 1
replace sex_cat = 0 if sexo == 0   // Mujer = 0

* Etiquetar los codigos
label define sex_cat_label 1 "Hombre" 0 "Mujer"
label values sex_cat sex_cat_label

* Verificar
tab sex_cat
tab sexo sex_cat, missing

*PRACTICA CALIFICADA ADICIONAL: a traves de una data que proporcionará el docente, aplicar los casos (un 1 do file, 1 base de datos, y 1 archvo de resultados)


*========================================================================
*2. Analisis estadística descriptiva
*========================================================================
*Resumen de todas las variables numéricas
summarize       

*Resumen de variables específicas
summarize gasto  

*Estadísticas más detalladas (incluye percentiles, etc.)
summarize, detail  
summarize gasto, detail

*========================================================================
*Estadísticas por grupos
*========================================================================
*Resumen por categorías de "grupo"
bysort civil: summarize gasto
bysort region: summarize gasto

*Frecuencias simples
tabulate sexo

*Tabla cruzada
tabulate sexo region

*Tabla cruzada con porcentajes
tabulate sexo region, row col cell

*Generación de tablas y tablas cruzadas o de contingencia
tab nini
tab nini [iw = fac500a]
tab nini sexo [iw = fac500a], col

*Media con intervalo de confianza
mean gasto  

*Percentiles específicos
centile gasto, centile(25 50 75)

*========================================================================
*Gráficos descriptivos básicos
*========================================================================

*Histograma con curva normal
histogram gasto, frequency normal

*gráfico de barras
graph bar (mean) gasto, over(sexo) 

*gráfico de barras más elaborado
graph bar gasto fac500a, over(sexo) ///
    legend(label(1 "gasto") label(2 "fac500a")) 
	
*grafico de barras horizontal
graph hbar gasto, over(sexo)
graph hbar gasto fac500a, over(sexo) ///
    legend(label(1 "gasto") label(2 "fac500a"))
	

*Activa colores intensos
set scheme s2color  

graph bar gasto, over(sexo) ///
    title("Gasto por sexo") ///
    ytitle("Soles") ///
    bar(1, color(blue)) ///
    bar(2, color(blue))
	
graph bar gasto, over(sexo) ///
    title("Gasto por sexo") ///
    ytitle("Soles") ///
    bar(1, fcolor(red)) ///
    bar(2, fcolor(red)) ///
    blabel(bar, format(%9.0f))
	
*grafico de pastel
*grafico basico
graph pie gasto, over(sexo)

*grafico con porcentaje
graph pie gasto, over(sexo) plabel(_all percent)

*grafico con porcentaje y más elaborado
graph pie gasto, over(sexo) plabel(_all percent) ///
    pie(1, color(red)) ///
    pie(2, color(blue)) 

*Diagrama de caja
graph box gasto

*Diagrama de dispersión con línea de ajuste
twoway (scatter gasto fac500a) (lfit gasto fac500a) 

*PRACTICA CALIFICADA: a traves de la data que proporcionará el docente , aplicar los casos sobre estadistica descriptiva (un 1 do file, 1 base de datos, y 1 archvo de resultados)

*========================================================================
*2. Analisis estadística inferencial
*========================================================================

*========================================================================
*Prueba de Districución normal
*========================================================================

*Shapiro-Wilk (para n < 50)
*Si el valor p ≤ α: Se rechaza la hipótesis nula. Se concluye que los datos no siguen una distribución normal o que la desviación de la normalidad es significativa. 
*Si el valor p > α: No se rechaza la hipótesis nula. Se concluye que los datos siguen una distribuidos normalmente. 

swilk gasto

*Kolmogorov-Smirnov (para n ≥ 50)
*Si el valor p ≤ α: Se rechaza la hipótesis nula. Se concluye que los datos no siguen una distribución normal o que la desviación de la normalidad es significativa. 
*Si el valor p > α: No se rechaza la hipótesis nula. Se concluye que los datos siguen una distribuidos normalmente.

summarize    

*ksmirnov gasto = normal((gasto - r(mean))/r(sd))

ksmirnov gasto = normal((gasto - 2927.022)/1908.901)

*jarque bera

* Instalar el comando jb (si no está instalado)
ssc install jb
jb gasto


*Gráfico básico de distribución normal
histogram gasto, normal title("Distribución del gasto") xtitle("Gasto") ytitle("Densidad")

*hacer bucles con las variables cuantitativas

*Gráfico Q-Q para evaluar normalidad
qnorm gasto, title("Gráfico Q-Q de normalidad para gasto")

*Gráfico de densidad con curva normal superpuesta sin barras
kdensity gasto, normal title("Función de densidad") xtitle("Gasto") ytitle("Densidad")

**Gráfico de densidad con curva normal superpuesta con barras
histogram gasto, frequency normal

*PRACTICA CALIFICADA: a traves de la data que proporcionará el docente , aplicar los casos de estadística inferencial (un 1 do file, 1 base de datos, y 1 archvo de resultados)

