clear all
ssc install estout, replace

global direct "E:\Econometria"
cd "$direct\base2024"

* =========================================================================
* 1. Construcción de la base de datos (corte transversal 2024)
* =========================================================================

* 1.1 Creación de la base temporal a nivel de Jefe de hogar. 
* -------------------------------------------------------------------------
* Se crea una base de datos con una fila por hogar, con información sobre la edad, sexo y nivel educativo del jefe de hogar. 
     
* 1.1.1 Seleccionamos a los jefes de hogar (Módulo 200: edad y sexo)
import delimited using "Enaho01-2024-200.csv", clear stringcols(_all) encoding("latin1")

destring p203 p207 p208a, replace force
keep if p203 == 1 // selecciona al jefe de hogar
keep conglome vivienda hogar codperso p207 p208a
rename p207 sexo_jefe
rename p208a edad_jefe
save "temp_jefes.dta", replace

* 1.1.2 Nivel educativo del jefe de hogar (Módulo 300: nivel educativo)
import delimited using "Enaho01a-2024-300.csv", clear stringcols(_all) encoding("latin1")
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

* 1.1.3 (SE OMITIÓ) Empleo del jefe de hogar (Módulo 500) ----
* Nota: Se ha eliminado la importación y creación de la variable ocupinf_jefe
* porque el año 2024 no se tiene esta variable

* 1.1.4 Merge de las bases temporales para crear la base final de jefes
use "temp_jefes.dta", clear
merge 1:1 conglome vivienda hogar codperso using "temp_jefes_educ.dta"
keep if _merge == 3 
drop _merge
* Ya no hacemos merge con temp_jefes_emp.dta porque la variable ocupinf fue eliminada
drop codperso
save "temp_jefe_2024.dta", replace

* 1.2 Creación de la base a nivel de persona (niños)
*--------------------------------------------------------------------------

* Esta base contiene la variable dependiente (asistencia_escolar) y los controles a nivel del niño (edad y sexo)

* 1.2.1 Edad y sexo del niño (Módulo 200)
import delimited using "Enaho01-2024-200.csv", clear stringcols(_all) encoding("latin1")
destring codperso p207 p208a, replace force
keep conglome vivienda hogar codperso p207 p208a
save "dta_miembros_2024.dta", replace

* 1.2.2 Asistencia escolar (Módulo 300: p307)
import delimited using "Enaho01a-2024-300.csv", clear stringcols(_all) encoding("latin1")
destring codperso p307, replace force
keep conglome vivienda hogar codperso p307

* Merge con los datos de miembros
merge 1:1 conglome vivienda hogar codperso using "dta_miembros_2024.dta"
keep if _merge == 3
drop _merge

* renombrar, etiquetar y limpiar las variables (missing values)
rename p208a edad_nino
rename p207 sexo_nino
replace edad_nino = . if edad_nino == 99
label variable edad_nino "Edad del niño/a"

* Etiquetar la variable sexo
label define sexo_label 1 "Hombre" 2 "Mujer"
label values sexo_nino sexo_label
label variable sexo_nino "Sexo del niño/a"

save "base_2024_persona.dta", replace

* 1.3 Merge de las bases a nivel jefe de hogar, niños y hogar
*--------------------------------------------------------------------------

* 1.3.1 Información a nivel de hogar (Sumaria-2024)
import delimited using "Sumaria-2024.csv", clear stringcols(_all) encoding("latin1")
destring ingtpu01 ingtpu03 mieperho estrato dominio, replace force
keep conglome vivienda hogar ingtpu01 ingtpu03 mieperho estrato dominio
save "temp_sumaria_2024.dta", replace

* 1.3.2 Merge de las bases de datos creadas anteriormente
use "base_2024_persona.dta", clear
merge m:1 conglome vivienda hogar using "temp_sumaria_2024.dta" // merge con la base de hogar
keep if _merge == 3
drop _merge
merge m:1 conglome vivienda hogar using "temp_jefe_2024.dta" // merge con la base de jefe de hogar
keep if _merge == 3
drop _merge

* 1.3.3 Generamos la variable año y etiquetamos variables finales
gen AÑO = 2024
label variable AÑO "Año de la encuesta"

* --- Crear ID del hogar usando tostring + encode ---
tostring conglome vivienda hogar, replace force
gen str key = conglome + "_" + vivienda + "_" + hogar
encode key, gen(id_hogar)
drop key
label variable id_hogar "ID numérico del hogar"

label variable ingtpu01 "Monto S/. Juntos (Hogar, anual)"
label variable ingtpu03 "Monto S/. Pensión 65 (Hogar, anual)"
label variable mieperho "Número de miembros en el hogar"

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

save "base_2024_completa.dta", replace

*==========================================================================
* 2. Análisis descriptivo (Tabla de balance)
* =========================================================================
use "base_2024_completa.dta", clear

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
* Nota: Se omitió ttest ocupinf_jefe porque la variable ocupinf fue eliminada

* Controles del hogar
gen p65 = (ingtpu03 > 0 & ingtpu03 != .)
label variable p65 "Hogar recibe Pensión 65 (1=Sí)"

ttest mieperho, by(beneficio_juntos)
ttest p65, by(beneficio_juntos)            
tab estrato beneficio_juntos, row chi2
tab dominio beneficio_juntos, row chi2
