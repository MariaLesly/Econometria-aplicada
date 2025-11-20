* ================================================================
* TAREA 1 - GRUPO 6: Construcción de la base de datos y análisis descriptivo

* OBJETIVO: En este script se construye la base de datos de corte trasnversal para el año 2022 y se obtiene estadísticos descriptivos, gráficos y estimación OLS para realizar el análisis de línea base y sesgo de selección.

* INTEGRANTES:
* Durga Valentina Linares Herrera - F1482478
* María Lesly León Huamán - 20166498
* Mirzha Brizeth Rivera Zubieta - F1429415
* Piero Beretta Vidal - 20193347
* Margarita Mamani Condori - 20133573
* ================================================================
clear all
ssc install estout

global direct "C:\Users\lesly\OneDrive\Documentos\DiplomadoQLAB\Intro_eco_aplicada\trabajo_final"
cd "$direct\datos"

* =========================================================================
* 1. Construcción de la base de datos (corte transversal 2022)
* =========================================================================

* 1.1 Creación de la base temporal a nivel de Jefe de hogar. 
* -------------------------------------------------------------------------
* Se crea una base de datos con una fila por hogar, con información sobre la edad, sexo, educación y tipo de empleo del jefe de hogar. 
 
* 1.1.1 Seleccionamos a los jefes de hogar (Módulo 200: edad y sexo)
import delimited using "Enaho01-2022-200.csv", clear stringcols(_all) encoding("latin1")

destring p203 p207 p208a, replace force
keep if p203 == 1 //selecciona al jefe de hogar
keep conglome vivienda hogar codperso p207 p208a
rename p207 sexo_jefe
rename p208a edad_jefe
save "temp_jefes.dta", replace


* 1.1.2 Nivel educativo del jefe de hogar (Módulo 300: nivel educativo)
import delimited using "Enaho01a-2022-300.csv", clear stringcols(_all) encoding("latin1")
destring p301a, replace force

* Generamos la variable categórica de nivel educativo
gen educ_jefe_nivel = .
replace educ_jefe_nivel = 0 if inlist(p301a, 1, 2) // Grupo 0: sin nivel/inicial
replace educ_jefe_nivel = 1 if inlist(p301a, 3, 4) // Grupo 1: primaria
replace educ_jefe_nivel = 2 if inlist(p301a, 5, 6) // Grupo 2: secundaria
replace educ_jefe_nivel = 3 if inlist(p301a, 7, 8) // Grupo 3: superior no universitaria 
replace educ_jefe_nivel = 4 if inlist(p301a, 9, 10, 11) // Grupo 4: superior universitaria completa/incompleta/postgrado

* Añadimos las etiquetas
label define educ_label 0 "0. Sin nivel/inicial" ///
                       1 "1. Primaria"         ///
                       2 "2. Secundaria"       ///
                       3 "3. Superior no universitaria"     ///
                       4 "4. Superior universitario/Postgrado"
					                          
label values educ_jefe_nivel educ_label
label variable educ_jefe_nivel "Nivel Educativo (Agrupado)"
keep conglome vivienda hogar codperso educ_jefe_nivel
save "temp_jefes_educ.dta", replace


* 1.1.3. Empleo del jefe de hogar (Módulo 500: Informalidad) ---
import delimited using "Enaho01a-2022-500.csv", clear stringcols(_all) encoding("latin1")
destring ocupinf, replace force

* Generamos una dummy de informalidad 
gen ocupinf_jefe = .
replace ocupinf_jefe = 1 if ocupinf == 1  // 1 = Empleo informal
replace ocupinf_jefe = 0 if ocupinf == 2  // 0 = Empleo formal

* Añadimos las etiquetas
label define ocupinf_label 0 "Formal" 1 "Informal"
label values ocupinf_jefe ocupinf_label
label variable ocupinf_jefe "Empleo informal del jefe (1=Sí)"
keep conglome vivienda hogar codperso ocupinf_jefe

save "temp_jefes_emp.dta", replace

* 1.1.4 Merge de las bases temporales para crear la base final de jefes
use "temp_jefes.dta", clear
merge 1:1 conglome vivienda hogar codperso using "temp_jefes_educ.dta"
keep if _merge == 3 
drop _merge
merge 1:1 conglome vivienda hogar codperso using "temp_jefes_emp.dta"
keep if _merge == 1 | _merge == 3 // se mantienen jefes aunque no trabajen
drop _merge
drop codperso
save "temp_jefe_2022.dta", replace

* 1.2 Creación de la base a nivel de persona (niños)
*--------------------------------------------------------------------------
* Esta base contiene la variable dependiente (asistencia_escolar) y los controles a nivel del niño (edad y sexo)

* 1.2.1 Edad y sexo del niño (Módulo 200)
import delimited using "Enaho01-2022-200.csv", clear stringcols(_all) encoding("latin1")
destring codperso p207 p208a, replace force
keep conglome vivienda hogar codperso p207 p208a
save "dta_miembros_2022.dta", replace

* 1.2.2 Asistencia escolar (Módulo 300: p307)
import delimited using "Enaho01a-2022-300.csv", clear stringcols(_all) encoding("latin1")
destring codperso p307, replace force
keep conglome vivienda hogar codperso p307

* Merge con los datos de miembros
merge 1:1 conglome vivienda hogar codperso using "dta_miembros_2022.dta"
keep if _merge == 3
drop _merge

* renombrar, etiquetar y limpiar las variables (missing values)
rename p208a edad_nino
rename p207 sexo_nino
replace edad_nino = . if edad_nino == 99
label variable edad_nino "Edad del niño/a"

* Etiqeutar la variable sexo
label define sexo_label 1 "Hombre" 2 "Mujer"
label values sexo_nino sexo_label
label variable sexo_nino "Sexo del niño/a"

save "base_2022_persona.dta", replace

* 1.3 Merge de las bases a nivel jefe de hogar, niños y hogar
*--------------------------------------------------------------------------

* 1.3.1 Información a nivel de hogar (Sumaria-2022)
import delimited using "Sumaria-2022.csv", clear stringcols(_all) encoding("latin1")
destring ingtpu01 ingtpu03 mieperho estrato dominio gashog2d, replace force
keep conglome vivienda hogar ingtpu01 ingtpu03 mieperho estrato dominio gashog2d
save "temp_sumaria_2022.dta", replace

* 1.3.2 Merge de las bases de datos creadas anteriormente
use "base_2022_persona.dta", clear
merge m:1 conglome vivienda hogar using "temp_sumaria_2022.dta" // merge con la base de hogar
keep if _merge == 3
drop _merge
merge m:1 conglome vivienda hogar using "temp_jefe_2022.dta" // merge con la base de jefe de hogar
keep if _merge == 3
drop _merge

* 1.3.3 Generamos la variable año y etiquetamos variables finales
gen AÑO = 2022
label variable AÑO "Año de la encuesta"

egen id_hogar = group(conglome vivienda hogar)
label variable id_hogar "ID númerico del hogar"

label variable ingtpu01 "Monto S/. Juntos (Hogar, anual)"
label variable ingtpu03 "Monto S/. Pensión 65 (Hogar, anual)"
label variable mieperho "Número de miembros en el hogar"

gen log_gasto = log(gashog2d/mieperho)
label variable log_gasto "Log del gasto per cápita'"

label define dom_label 1 "Costa Norte" 2 "Costa Centro" 3 "Costa Sur" ///
                     4 "Sierra Norte" 5 "Sierra Centro" 6 "Sierra Sur" ///
                     7 "Selva" 8 "Lima Metropolitana", replace
label values dominio dom_label
label variable dominio "Dominio Geográfico (8 regiones)"

* (Etiquetas para ESTRATO)
label define est_label 1 "Lima Metro" 2 "Resto Costa Urb" 3 "Sierra Urb" ///
                     4 "Selva Urb" 5 "Pueblos 2k-20k" 6 "Costa Rural" ///
                     7 "Sierra Rural" 8 "Selva Rural", replace
label values estrato est_label
label variable estrato "Estrato Geográfico (8 estratos)"

order AÑO id_hogar conglome vivienda hogar codperso

save "base_2022_completa.dta", replace


*==========================================================================
* 2. Análisis descriptivo (Tabla de balance)
* =========================================================================
* En esta sección se corre un t-test para las variables de control entre el grupo de tratamiento (Juntos) y control con la finalidad de probar empíricamente la existencia de sesgo de selección
use "base_2022_completa.dta", clear

* Generamos la variable dependiente (Y): asistencia_escolar (1=Sí, 0=No)
gen asistencia_escolar = 0
replace asistencia_escolar = 1 if p307 == 1
label variable asistencia_escolar "Asistencia Escolar (1=Sí)"

* Generamos la variable de tratamiento (X): beneficio_juntos (1=Sí, 0=No)
gen beneficio_juntos = (ingtpu01 > 0 & ingtpu01 != .)
label variable beneficio_juntos "Hogar recibe Juntos (1=Sí)"

* Filtramos a los niños en edad escolar, 6 a 16 años, en la muestra
keep if edad_nino >= 6 & edad_nino <= 16
order AÑO id_hogar codperso asistencia_escolar beneficio_juntos

* 2.1 Ejecutamos las pruebas de medias (ttest)

* Variable de Resultado (Y)
ttest asistencia_escolar, by(beneficio_juntos)

* Controles del niño/a
ttest edad_nino, by(beneficio_juntos)
tab sexo_nino beneficio_juntos, row chi2

* Controles del jefe/a de hogar
ttest edad_jefe, by(beneficio_juntos)
tab sexo_jefe beneficio_juntos, row chi2
ttest educ_jefe_nivel, by(beneficio_juntos) 
ttest ocupinf_jefe, by(beneficio_juntos)    

* Controles del hogar
gen p65 = (ingtpu03 > 0 & ingtpu03 != .)
label variable p65 "Hogar recibe Pensión 65 (1=Sí)"

ttest mieperho, by(beneficio_juntos)
ttest p65, by(beneficio_juntos)            
tab estrato beneficio_juntos, row chi2
tab dominio beneficio_juntos, row chi2


* =========================================================================
* 3. Análisis gráfico, regresión OLS y diagnósticos
* =========================================================================
use "base_2022_completa.dta", clear

* En caso se utilice la base_2022_completa independientemente de ejecutar 
* la sección anterior se debe generar las siguientes variables:

gen asistencia_escolar = 0
replace asistencia_escolar = 1 if p307 == 1
replace asistencia_escolar = . if p307 > 2 | p307 == .


gen beneficio_juntos = (ingtpu01 > 0 & ingtpu01 != .) 
replace beneficio_juntos = 0 if beneficio_juntos == .

gen p65 = (ingtpu03 > 0 & ingtpu03 != .) 
replace p65 = 0 if p65 == .


keep if edad_nino >= 6 & edad_nino <= 16

* 3.1 Gráfico de Barras
graph bar (mean) asistencia_escolar, over(beneficio_juntos) ///
    ytitle("Tasa de asistencia escolar (mean)") ///
    title("Gráfico 1: Asistencia escolar por grupo de tratamiento (2022)") ///
    legend(label(1 "Control (No Juntos)") label(2 "Tratamiento (Juntos)"))
	
*Como observamos en el Gráfico 1, la diferencia en la asistencia escolar entre el grupo de tratamiento y control es casi nula y estadísticamente no significativa. Sin embargo, los test de las otras variables, desarrolladas en la seccion anterior, muestran que ambos grupos son drásticamente diferentes. Por lo tanto, incluso la comparación simple está contaminada por un fuerte sesgo de selección. 

* 3.2 Gráfico de Cajas 
graph box educ_jefe_nivel, over(beneficio_juntos) ///
    ytitle("Nivel Educativo del jefe de hogar") ///
    title("Gráfico 2: Sesgo de selección en Nivel Educativo") ///
    legend(label(1 "Control (No Juntos)") label(2 "Tratamiento (Juntos)"))
	
* El Gráfico 2 muestra el sesgo de selección en la variable de educación del jefe de hogar. El grupo de tratamiento se concentra en el Nivel 1 (Primaria) a comparación del grupo de control, cuya mediana es el Nivel 2 (Secundaria). Es decir, el nivel de educacción del jefe/a de hogar que pertenecen al programa es bajo. Esto confirma el resultado en el ttest educ_jefe_nivel, by(beneficio_juntos) y demuestra que las comparaciones entre ambos grupos está fuertemente sesgada.

* 3.3 Gráfico de densidad Kernel 
twoway (kdensity log_gasto if beneficio_juntos == 0, color(blue%30) recast(area)) ///
       (kdensity log_gasto if beneficio_juntos == 1, color(red%30) recast(area)), ///
       legend(label(1 "Control") label(2 "Tratamiento")) ///
       title("Gráfico 3: Densidad de Gasto (Focalización del programa)") ///
       xtitle("Log Gasto per cápita") ytitle("Densidad") ///
       name(g3_densidad, replace)
	   
* El Gráfico 3 presenta la función de densidad de probailidad del logaritmo del gasto per cápita, el cual es un proxy de bienestar, para el grupo de control y tratamiento. Se observa que la distribuciòn del grupo de tratamiento se desplaza hacia la izquierda, lo cual hace referencia a que este grupo tiene menor gasto a diferencia del grupo de control. Por lo tanto, hay una buena focalización del programa. Sin embargo, los grupos no son comparables, porque el grupo de control es más rico ex ante, lo cual confirma el sesgo de selección.
	  
* 3.4 Gráfico de dispersión y ajuste lineal
twoway (scatter log_gasto educ_jefe_nivel, mcolor(black%5) msize(tiny)) ///
       (lfit log_gasto educ_jefe_nivel, lcolor(red) lwidth(thick)), ///
       title("Gráfico 4: Retornos a la educación del Jefe") ///
       xtitle("Nivel educativo Jefe (Categoría)") ytitle("Log Gasto per cápita") ///
       name(g5_scat, replace)
	   
* El Gráfico 4 presenta la relación entre el nivel educativo del jefe de hogar y el bienestar económico (logaritmo del gasto per cápita). La línea de ajuste lineal muestra una pendiente positiva y pronunciada, confirmando que mayores niveles educativos están fuertemente correlacionados con un mayor bienestar en el hogar. Dado que previamente establecimos que los beneficiarios del programa Juntos se concentran en los niveles educativos más bajos (0 y 1) (Gráfico 2), este gráfico ilustra el mecanismo de transmisión de la pobreza: la baja escolaridad del jefe se traduce en menores recursos económicos. Esto refuerza la necesidad de controlar rigurosamente por el nivel educativo y el gasto en nuestro análisis multivariado para evitar atribuir al programa efectos que en realidad provienen de la pobreza estructural del hogar.

* 3.5 Análisis asociativo multivaridado

* Definimos la lista de controles 
global controles "i.sexo_nino c.edad_nino c.educ_jefe_nivel i.sexo_jefe c.edad_jefe i.ocupinf_jefe c.mieperho i.p65 i.estrato i.dominio"

* Modelo 1: correlación simple
reg asistencia_escolar i.beneficio_juntos, vce(cluster id_hogar)
est store modelo_1

* Modelo 2: Regresión OLS con controles
reg asistencia_escolar i.beneficio_juntos $controles, vce(cluster id_hogar)
est store modelo_2

estout modelo_1 modelo_2, cells(b(star fmt(3)) se(par fmt(2))) ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("Tabla 2: Análisis de Asociación (OLS 2022)") ///
    legend varlabels(_cons Constant)

* En conclusión, el análisis descriptivo demuestra un fuerte sesgo de selección, por lo que en el análisis asociativo se observa que, usando el modelo OLS, no se encuentra una asociación estadísticamente significativa. Este resultado no tiene robustez, debido a la alta probabilidad de sesgo de selección. Por lo tanto, para estimar el efecto causal del programa es necesario utilizar una metodología más robusta como Efectos fijos, utilizando datos de panel, que es el objetivo de nuestro proyecto.

* 3.6 Diagnóstico del modelo // Para correr los test de diagnóstico (post estimación) ejecutamos el modelo 'quietly' sin cluster
quietly reg asistencia_escolar i.beneficio_juntos sexo_nino edad_nino educ_jefe_nivel sexo_jefe edad_jefe ocupinf_jefe mieperho p65 i.estrato i.dominio

* A. Test de Multicolinealidad (VIF)
display "Test de multicolinealidad (VIF)"
estat vif
* Mean VIF = 2.18 --> No existe problema de multicolinealidad

* B. Test de Heterocedasticidad (White y Breusch-Pagan)
display "Test de heterocedasticidad"
estat hettest
estat imtest, white
* H0: Homocedasticidad. Si p-value < 0.05, hay heterocedasticidad, por lo que se justifica uso de cluster.

* C. Test de Especificación (Ramsey RESET)
display "Test de variables omitidas"
ovtest
* H0: El modelo no tiene variables omitidas. Si p-value < 0.05, rechazamos la hipótesis nula. El modelo OLS de corte transversal tiene sesgo de variables omitidas, por lo que amerita modelo de datos de panel.

* D. Test de normalidad de residuos
display "Test de normalidad de residuos"
predict residuos, resid
sktest residuos
* Los residuos de una variable dependiente binaria no pueden tener distribución normal; sin embargo, debido al tamaño de la muestra N, el TLC garantiza la normalidad asintótica de los estimadores de OLS.

* Gráfico de normalidad de residuos
kdensity residuos, normal title("Normalidad de residuos") name(g6_norm, replace)
* La distribución bimodal es característica de los residuos en modelos de variable dependiente binaria.
