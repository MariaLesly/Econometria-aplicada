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
global direct "C:\Users\Lenovo\OneDrive\Documentos\DiplomadoQLAB\Intro_eco_aplicada\trabajo_final"
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
destring ingtpu01 ingtpu03 mieperho estrato dominio, replace force
keep conglome vivienda hogar ingtpu01 ingtpu03 mieperho estrato dominio
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
* 3. Análisis gráfico y regresión OLS
* =========================================================================
use "base_2022_completa.dta", clear

* En caso se utilice la base_2022_completa independientemente de ejecutar la sección anterior se debe generar las siguientes variables: 

*gen asistencia_escolar = 0
*replace asistencia_escolar = 1 if p307 == 1
*replace asistencia_escolar = . if p307 > 2 | p307 == .
*gen beneficio_juntos = (ingtpu01 > 0 & ingtpu01 != .) 
*replace beneficio_juntos = 0 if beneficio_juntos == .
*gen p65 = (ingtpu03 > 0 & ingtpu03 != .) 
*replace p65 = 0 if p65 == .
*keep if edad_nino >= 6 & edad_nino <= 16

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


* 3.3 Análisis asociativo multivaridado

* Definimos la lista de controles 
global controles "i.sexo_nino c.edad_nino c.educ_jefe_nivel i.sexo_jefe c.edad_jefe i.ocupinf_jefe c.mieperho i.p65 i.estrato i.dominio"

* Modelo 1: correlación simpre
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