%{
  #include <stdlib.h>
  #include <stdio.h>
  extern FILE *yyin;
  //extern int linea;
  //extern char yytext;
  #define YYDEBUG 1

%}

%token ABSTRACT BASE BOOLEAN BREAK CASE CATCH CHAR CLASS CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN FINALLY FLOAT FOR GOTO IF INT INTERFACE INTERNAL LONG NAMESPACE NEW OVERRIDE PRIVATE PROTECTED PUBLIC RETURN SEALED SHORT SIGNED STATIC STRUCT SWITCH THIS THROW TRY TYPEDEF UNION UNSIGNED USING VIRTUAL VOID WHILE IDENTIFICADOR ENTERO CADENA REAL CARACTER BOOLEANO SIZEOF PTR_ACCESO INC DEC DESPI DESPD LE GE EQ NEQ AND OR MULT_ASIG DIV_ASIG MOD_ASIG SUMA_ASIG RESTA_ASIG DESPI_ASIG DESPD_ASIG AND_ASIG XOR_ASIG OR_ASIG

%%

/************/
/* PROGRAMA */
/************/

modulo
    : lista_declaracion                                                       { printf ("\tmodulo -> list_decl\n"); }
    | lista_directivas_uso lista_declaracion                                  { printf ("\tmodulo -> lista_dir_uso list_decl\n"); }
    ;

lista_declaracion
    : declaracion                                                             { printf ("\tlist_decl -> decl\n"); }
    | lista_declaracion declaracion                                           { printf ("\tlist_decl -> list_decl decl\n"); }
    ;

declaracion
    : declaracion_espacio_nombres                                             { printf ("\tdecl -> decl_esp_nom\n"); }
    | declaracion_variable                                                    { printf ("\tdecl -> decl_var\n"); }
    | declaracion_tipo                                                        { printf ("\tdecl -> decl_tipo\n"); }
    | declaracion_funcion                                                     { printf ("\tdecl -> decl_func\n"); }
    ;

lista_directivas_uso
    : directiva_uso                                                           { printf ("\tlista_dir_uso -> dir_uso\n"); }
    | lista_directivas_uso directiva_uso                                      { printf ("\tlista_dir_uso -> lista_dir_uso dir_uso\n"); }
    ;

directiva_uso
    : USING IDENTIFICADOR '=' nombre_tipo_o_espacio_nombres ';'               { printf ("\tdir_uso -> USING ID = nom_tipo_o_esp_noms\n"); }
    | USING nombre_tipo_o_espacio_nombres ';'                                 { printf ("\tdir_uso -> nom_tipo_o_esp_noms\n"); }
    ;

nombre_tipo_o_espacio_nombres
    : identificador_con_tipos                                                 { printf ("\tnom_tipo_o_esp_noms -> id_tipos\n"); }
    | nombre_tipo_o_espacio_nombres '.' identificador_con_tipos               { printf ("\tnom_tipo_o_esp_noms -> nom_tipo_o_esp_noms . id_tipos\n"); }
    ;

identificador_con_tipos
    : IDENTIFICADOR                                                           { printf ("\tid_tipos -> ID\n"); }
    | IDENTIFICADOR '(' lista_nombres_tipo_o_espacio_nombres ')'              { printf ("\tid_tipos -> ID ( list_tipos )\n"); }
    ;

lista_nombres_tipo_o_espacio_nombres
    : nombre_tipo_o_espacio_nombres                                           { printf ("\tlist_nom_tipo_o_esp_noms -> nom_tipo_o_esp_noms\n"); }
    | lista_nombres_tipo_o_espacio_nombres ',' nombre_tipo_o_espacio_nombres  { printf ("\tlist_nom_tipo_o_esp_noms -> list_nom_tipo_o_esp_noms nom_tipo_o_esp_noms\n"); }
    ;

/*******************/
/* ESPACIO NOMBRES */
/*******************/
declaracion_espacio_nombres 
    : NAMESPACE identificador_anidado bloque_espacio_nombres                  { printf ("\tdecl_esp_nom -> NMSPACE id_anid bloq_esp_nom\n"); }
    ;

identificador_anidado 
    : IDENTIFICADOR                                                           { printf ("\tid_anid -> ID\n"); }
    | identificador_anidado '.' IDENTIFICADOR                                 { printf ("\tid_anid -> id_anid . ID\n"); }
    ;

bloque_espacio_nombres 
    : '{' lista_declaracion '}'                                               { printf ("\tbloq_esp_noms -> { list_decl }\n"); } 
    | '{' lista_directivas_uso lista_declaracion '}'                          { printf ("\tbloq_esp_noms -> { lista_dir_uso list_decl }\n"); }
    ;

/*************/
/* VARIABLES */
/*************/
declaracion_variable 
    : tipo lista_nombre ';'                                                   { printf ("\tdecl_var -> tipo list_nom\n"); }
    ;

tipo 
    : '<' nombre_tipo_o_espacio_nombres '>'                                   { printf ("\ttipo -> < nom_tipo_o_esp_noms >\n"); }
    | tipo_escalar                                                            { printf ("\ttipo -> tipo_escalar\n"); }
    ;

tipo_escalar 
    : tipo_basico                                                             { printf ("\ttipo_escalar -> tipo_basico\n"); }
    | longitud tipo_basico                                                    { printf ("\ttipo_escalar -> long tipo_basico\n"); }
    | signo tipo_basico                                                       { printf ("\ttipo_escalar -> signo tipo_basico\n"); }
    | signo longitud tipo_basico                                              { printf ("\ttipo_escalar -> signo long tipo_basico\n"); }
    ;

longitud 
    : SHORT                                                                   { printf ("\tlong -> SHORT\n"); }
    | LONG                                                                    { printf ("\tlong -> LONG\n"); }
    ;

signo 
    : SIGNED                                                                  { printf ("\tsigno -> SIGNED\n"); }
    | UNSIGNED                                                                { printf ("\tsigno -> UNSIGNED\n"); }
    ;

tipo_basico 
    : CHAR                                                                    { printf ("\ttipo_basico -> CHARD\n"); }
    | INT                                                                     { printf ("\ttipo_basico -> INT\n"); }
    | FLOAT                                                                   { printf ("\ttipo_basico -> FLOAT\n"); }
    | DOUBLE                                                                  { printf ("\ttipo_basico -> DOUBLE\n"); }
    | BOOLEAN                                                                 { printf ("\ttipo_basico -> BOOLEAN\n"); }
    ;

nombre 
    : dato                                                                    { printf ("\tnom -> dato\n"); }
    | dato '=' valor                                                          { printf ("\tnom -> dato = valor\n"); }
    ;

lista_nombre 
    : nombre                                                                  { printf ("\tlist_nom -> nom\n"); }
    | lista_nombre nombre                                                     { printf ("\tlist_nom -> list_nom nom\n"); }
    ;

dato 
    : dato_indexado                                                           { printf ("\tdato -> dato_indexado\n"); }
    | '*' dato                                                                { printf ("\tdato -> * dato\n"); }
    ;

dato_indexado 
    : IDENTIFICADOR                                                           { printf ("\tdato_indexado -> ID\n"); }
    | IDENTIFICADOR '[' ']'                                                   { printf ("\tdato_indexado -> ID []\n"); }
    | IDENTIFICADOR '[' lista_expresion ']'                                   { printf ("\tdato_indexado -> ID [ list_exp ]\n"); }
    ;

lista_expresion 
    : expresion                                                               { printf ("\tlist_exp -> exp\n"); }
    | lista_expresion ',' expresion                                           { printf ("\tlist_exp -> list_exp exp\n"); }
    ;

valor 
    : expresion                                                               { printf ("\tvalor -> exp\n"); }
    | '{' lista_valor '}'                                                     { printf ("\tvalor -> { list_valor }\n"); }
    ;

lista_valor 
    : valor                                                                   { printf ("\tlist_valor -> valor\n"); }
    | lista_valor ',' valor                                                   { printf ("\tlist_valor -> list_valor valor\n"); }

/*********/
/* TIPOS */
/*********/

declaracion_tipo 
    : nombramiento_tipo                                                       { printf ("\tdecl_tipo -> nombramiento_tipo\n"); }
    | declaracion_struct_union                                                { printf ("\tdecl_tipo -> decl_struct_union\n"); }
    | declaracion_interfaz                                                    { printf ("\tdecl_tipo -> decl_interfaz\n"); }
    | declaracion_enum                                                        { printf ("\tdecl_tipo -> decl_enum\n"); }
    | declaracion_clase                                                       { printf ("\tdecl_tipo -> decl_clase\n"); }
    ;

nombramiento_tipo 
    : TYPEDEF tipo IDENTIFICADOR ';'                                          { printf ("\tnombramiento_tipo -> TYPEDEF tipo ID\n"); }
    ;

declaracion_struct_union 
    : struct_union '{' lista_declaracion_campo '}'                            { printf ("\tdecl_struct_union -> struct_union { list_decl_campo }\n"); }
    | lista_modificador struct_union '{' lista_declaracion_campo '}'          { printf ("\tdecl_struct_union -> list_mod struct_union { list_decl_campo }\n"); }
    | lista_modificador struct_union IDENTIFICADOR '{' lista_declaracion_campo '}' { printf ("  decl_struct_union -> list_mod struct_union ID { list_decl_campo }\n"); }
    ;

modificador 
    : NEW                                                                     { printf ("\tmod -> NEW\n"); }
    | PUBLIC                                                                  { printf ("\tmod -> PUBLIC\n"); }
    | PROTECTED                                                               { printf ("\tmod -> PROTECTED\n"); }
    | INTERNAL                                                                { printf ("\tmod -> INTERNAL\n"); }
    | PRIVATE                                                                 { printf ("\tmod -> PRIVATE\n"); }
    | STATIC                                                                  { printf ("\tmod -> STATIC\n"); }
    | VIRTUAL                                                                 { printf ("\tmod -> VIRTUAL\n"); }
    | SEALED                                                                  { printf ("\tmod -> SEALED\n"); }
    | OVERRIDE                                                                { printf ("\tmod -> OVERRIDE\n"); }
    | ABSTRACT                                                                { printf ("\tmod -> ABSTRACT\n"); }
    | EXTERN                                                                  { printf ("\tmod -> EXTERN\n"); }
    ;

lista_modificador
    : modificador                                                             { printf ("\tlist_mod -> mod\n"); }
    | lista_modificador modificador                                           { printf ("\tlist_mod -> list_mod mod\n"); }
    ;

struct_union 
    : STRUCT                                                                  { printf ("\tstruct_union -> STRUCT\n"); }
    | UNION                                                                   { printf ("\tstruct_union -> UNION\n"); }
    ;

declaracion_campo 
    : tipo lista_nombre ';'                                                   { printf ("\tdecl_campo -> tipo list_nom\n"); }
    | declaracion_struct_union lista_nombre ';'                               { printf ("\tdecl_campo -> decl_struct_union list_nom\n"); }
    ;

lista_nombre 
    : nombre                                                                  { printf ("\tlist_nom -> nom\n"); }
    | lista_nombre ',' nombre                                                 { printf ("\tlist_nom -> list_nom nom\n"); }
    ;

lista_declaracion_campo
    : declaracion_campo                                                       { printf ("\tlist_decl_campo -> decl_campo\n"); }
    | lista_declaracion_campo declaracion_campo                               { printf ("\tlist_decl_campo -> list_decl_campo decl_campo\n"); }
    ;

declaracion_interfaz 
    : INTERFACE IDENTIFICADOR herencia cuerpo_interfaz                        { printf ("\tdecl_interfaz -> INTERFACE ID herencia cuerpo_interfaz\n"); }
    | lista_modificador INTERFACE IDENTIFICADOR herencia cuerpo_interfaz      { printf ("\tdecl_interfaz -> list_mod INTERFACE ID herencia cuerpo_interfaz\n"); }
    ;

herencia 
    : ':' nombre_tipo_o_espacio_nombres                                       { printf ("\therencia -> nom_tipo_o_esp_noms\n"); }
    ;

lista_nombre_tipo_o_espacio_nombres 
    : nombre_tipo_o_espacio_nombres                                           { printf ("\tlist_nom_tipo_o_esp_noms -> nom_tipo_o_esp_noms\n"); }
    | lista_nombre_tipo_o_espacio_nombres ',' nombre_tipo_o_espacio_nombres   { printf ("\tlist_nom_tipo_o_esp_noms -> list_nom_tipo_o_esp_noms nom_tipo_o_esp_noms\n"); }
    ;

cuerpo_interfaz 
    : '{' '}'                                                                 { printf ("\tcuerpo_interfaz -> { }\n"); }
    | '{' lista_declaracion_metodo_interfaz '}'                               { printf ("\tcuerpo_interfaz -> { list_decl_metodo_inferfaz }\n"); }
    ;

declaracion_metodo_interfaz 
    : firma_funcion ';'                                                       { printf ("\tdecl_metodo_inferfaz -> firma_funcion\n"); }
    | NEW firma_funcion ';'                                                   { printf ("\tdecl_metodo_inferfaz -> NEW firma_funcion\n"); }
    ;

lista_declaracion_metodo_interfaz 
    : declaracion_metodo_interfaz                                             { printf ("\tlist_decl_metodo_inferfaz -> decl_metodo_inferfaz\n"); }
    | lista_declaracion_metodo_interfaz declaracion_metodo_interfaz           { printf ("\tlist_decl_metodo_inferfaz -> list_decl_metodo_inferfaz decl_metodo_inferfaz\n"); }
    ;

declaracion_enum 
    : ENUM IDENTIFICADOR cuerpo_enum                                          { printf ("\tdecl_enum -> ENUM ID cuerpo_enum\n"); }
    | ENUM IDENTIFICADOR ':' tipo_escalar cuerpo_enum                         { printf ("\tdecl_enum -> ENUM ID tipo_escalar cuerpo_enum\n"); }
    | lista_modificador ENUM IDENTIFICADOR cuerpo_enum                        { printf ("\tdecl_enum -> list_mod ENUM ID cuerpo_enum\n"); }
    | lista_modificador ENUM IDENTIFICADOR ':' tipo_escalar cuerpo_enum       { printf ("\tdecl_enum -> list_mod ENUM ID tipo_escalar cuerpo_enum\n"); }
    ;

cuerpo_enum 
    : '{' lista_declaracion_miembro_enum '}'                                  { printf ("\tcuerpo_enum -> { list_decl_miembro_enum }\n"); }
    ;

declaracion_miembro_enum 
    : IDENTIFICADOR                                                           { printf ("\tdecl_miembro_enum -> ID\n"); }
    | IDENTIFICADOR '=' expresion                                             { printf ("\tdecl_miembro_enum -> ID = exp\n"); }
    ;

lista_declaracion_miembro_enum 
    : declaracion_miembro_enum                                                { printf ("\tlist_decl_miembro_enum -> decl_miembro_enum\n"); }
    | lista_declaracion_miembro_enum ',' declaracion_miembro_enum             { printf ("\tlist_decl_miembro_enum -> list_decl_miembro_enum decl_miembro_enum\n"); }
    ;

/**********/
/* CLASES */
/**********/
declaracion_clase 
    : CLASS IDENTIFICADOR herencia cuerpo_clase                               { printf ("\tdecl_clase -> CLASS ID herencia cuerpo_clase\n"); }     
    | lista_modificador CLASS IDENTIFICADOR herencia cuerpo_clase             { printf ("\tdecl_clase -> list_mod CLASS ID herencia cuerpo_clase\n"); }
    ;

cuerpo_clase 
    : '{' lista_declaracion_elemento_clase '}'                                { printf ("\tcuerpo_clase -> list_decl_elem_clase\n"); }
    ;

declaracion_elemento_clase 
    : declaracion_tipo                                                        { printf ("\tdecl_elem_clase -> decl_tipo\n"); }
    | declaracion_atributo                                                    { printf ("\tdecl_elem_clase -> declaracion_atributo\n"); }
    | declaracion_metodo                                                      { printf ("\tdecl_elem_clase -> decl_metodo\n"); }
    | declaracion_constructor                                                 { printf ("\tdecl_elem_clase -> decl_constructor\n"); }
    | declaracion_destructor                                                  { printf ("\tdecl_elem_clase -> decl_destructor\n"); }
    | declaracion_atributo                                                    { printf ("\tdecl_elem_clase -> decl_atributo\n"); }
    ;

lista_declaracion_elemento_clase 
    : declaracion_elemento_clase                                              { printf ("\tlist_decl_elem_clase -> decl_elem_clase\n"); }
    | lista_declaracion_elemento_clase declaracion_elemento_clase             { printf ("\tlist_decl_elem_clase -> list_decl_elem_clase decl_elem_clase\n"); }
    ;

declaracion_atributo 
    : declaracion_variable                                                    { printf ("\tdecl_atributo -> decl_var\n"); }
    | lista_modificador declaracion_variable                                  { printf ("\tdecl_atributo -> list_mod decl_var\n"); }
    ;

declaracion_metodo 
    : firma_funcion bloque_instrucciones                                      { printf ("\tdecl_metodo -> firma_funcion bloque_instrucciones\n"); }
    | lista_modificador firma_funcion bloque_instrucciones                    { printf ("\tdecl_metodo -> list_mod firma_funcion bloque_instrucciones\n"); }
    ;

declaracion_constructor 
    : cabecera_constructor bloque_instrucciones                               { printf ("\tdecl_constructor -> cabecera_constructor bloque_instrucciones\n"); }
    | lista_modificador cabecera_constructor bloque_instrucciones             { printf ("\tdecl_constructor -> list_mod cabecera_constructor bloque_instrucciones\n"); }
    ;

cabecera_constructor 
    : IDENTIFICADOR                                                           { printf ("\tcabecera_constructor -> ID\n"); }
    | IDENTIFICADOR parametros inicializador_constructor                      { printf ("\tcabecera_constructor -> ID parametros inicializador_constructor\n"); }
    | IDENTIFICADOR parametros                                                { printf ("\tcabecera_constructor -> ID parametros\n"); }
    | IDENTIFICADOR inicializador_constructor                                 { printf ("\tcabecera_constructor -> ID inicializador_constructor\n"); }
    ;

inicializador_constructor 
    : ':' BASE parametros                                                     { printf ("\tinicializador_constructor -> BASE parametros\n"); }
    | ':' THIS parametros                                                     { printf ("\tinicializador_constructor -> THIS parametros\n"); }
    ;

declaracion_destructor 
    : cabecera_destructor bloque_instrucciones                                { printf ("\tdecl_destructor -> cabecera_destructor bloque_instrucciones\n"); }
    | lista_modificador cabecera_destructor bloque_instrucciones              { printf ("\tdecl_destructor -> list_mod cabecera_destructor bloque_instrucciones\n"); }
    ;

cabecera_destructor 
    : '~' IDENTIFICADOR '(' ')'                                               { printf ("\tcabecera_destructor -> ~ ID ()\n"); }
    ;

/*************/
/* FUNCIONES */
/*************/
declaracion_funcion 
    : firma_funcion bloque_instrucciones                                      { printf ("\tdecl_func -> firma_funcion bloque_instrucciones\n"); }
    ;

firma_funcion 
    : VOID IDENTIFICADOR parametros                                           { printf ("\tfirma_funcion -> VOID ID parametros\n"); }
    | tipo IDENTIFICADOR parametros                                           { printf ("\tfirma_funcion -> tipo ID parametros\n"); }
    | tipo lista_asterisco IDENTIFICADOR parametros                           { printf ("\tfirma_funcion -> tipo list_asterisco ID parametros\n"); }
    ;

lista_asterisco
    : '*'                                                                     { printf ("\tlist_asterisco -> *\n"); } 
    | lista_asterisco '*'                                                     { printf ("\tlist_asterisco -> list_asterisco *\n"); }
    ;

parametros 
    : '(' ')'                                                                 { printf ("\tparametros -> ()\n"); }
    | '(' lista_argumentos ')'                                                { printf ("\tparametros -> ( list_argumentos )\n"); }
    ;

argumentos 
    : nombre_tipo lista_variable                                              { printf ("\targumentos -> nom_tipo list_var\n"); }
    ;

lista_argumentos 
    : argumentos                                                              { printf ("\tlist_argumentos -> argumentos\n"); }
    | lista_argumentos ';' argumentos                                         { printf ("\tlist_argumentos -> list_argumentos argumentos\n"); }
    ;

nombre_tipo 
    : tipo                                                                    { printf ("\tnom_tipo -> tipo\n"); }
    | tipo lista_asterisco                                                    { printf ("\tnom_tipo -> tipo list_asterisco\n"); }
    ;

variable
    : IDENTIFICADOR                                                           { printf ("\tvar -> ID\n"); }
    | IDENTIFICADOR '=' expresion                                             { printf ("\tvar -> ID = exp\n"); }
    ;

lista_variable 
    : variable                                                                { printf ("\tlist_var -> var\n"); }
    | lista_variable ',' variable                                             { printf ("\tlist_var -> list_var var\n"); }
    ;

/*****************/
/* INSTRUCCIONES */
/*****************/
instruccion 
    : bloque_instrucciones                                                    { printf ("\tinstruccion -> bloque_instrucciones\n"); }
    | instruccion_expresion                                                   { printf ("\tinstruccion -> instruccion_exp\n"); }
    | instruccion_bifurcacion                                                 { printf ("\tinstruccion -> instruccion_bifurcacion\n"); }
    | instruccion_bucle                                                       { printf ("\tinstruccion -> instruccion_bucle\n"); }
    | instruccion_salto                                                       { printf ("\tinstruccion -> instruccion_salto\n"); }
    | instruccion_destino_salto                                               { printf ("\tinstruccion -> instruccion_destino_salto\n"); }
    | instruccion_retorno                                                     { printf ("\tinstruccion -> instruccion_retorno\n"); }
    | instruccion_lanzamiento_excepcion                                       { printf ("\tinstruccion -> instruccion_lanzamiento_excepcion\n"); }
    | instruccion_captura_excepcion                                           { printf ("\tinstruccion -> instruccion_captura_excepcion\n"); }
    | instruccion_vacia                                                       { printf ("\tinstruccion -> instruccion_vacia\n"); }
    ;

lista_instruccion 
    : instruccion                                                             { printf ("\tlist_instruccion -> instruccion\n"); }
    | lista_instruccion instruccion                                           { printf ("\tlist_instruccion -> list_instruccion instruccion\n"); }
    ;

bloque_instrucciones 
    : '{' '}'                                                                 { printf ("\tbloque_instrucciones -> {}\n"); }
    | '{' lista_declaracion '}'                                               { printf ("\tbloque_instrucciones -> { list_decl }\n"); }
    | '{' lista_instruccion '}'                                               { printf ("\tbloque_instrucciones -> { list_instruccion }\n"); }
    | '{' lista_declaracion lista_instruccion '}'                             { printf ("\tbloque_instrucciones -> { list_decl list_instruccion }\n"); }
    ;

instruccion_expresion 
    : expresion_funcional ';'                                                 { printf ("\tinstruccion_expresion -> exp_funcional\n"); }
    | asignacion ';'                                                          { printf ("\tinstruccion_expresion -> asignacion\n"); }
    ;

asignacion 
    : expresion_indexada operador_asignacion expresion                        { printf ("\tasignacion -> exp_index op_asignacion exp\n"); }
    ;

operador_asignacion 
    : '='                                                                     { printf ("\top_asignacion -> =\n"); }
    | MULT_ASIG                                                               { printf ("\top_asignacion -> MULT_ASIG\n"); }
    | DIV_ASIG                                                                { printf ("\top_asignacion -> DIV_ASIG\n"); }
    | MOD_ASIG                                                                { printf ("\top_asignacion -> MOD_ASIG\n"); }
    | SUMA_ASIG                                                               { printf ("\top_asignacion -> SUMA_ASIG\n"); }
    | RESTA_ASIG                                                              { printf ("\top_asignacion -> RESTA_ASIG\n"); }
    | DESPI_ASIG                                                              { printf ("\top_asignacion -> DESPI_ASIG\n"); }
    | DESPD_ASIG                                                              { printf ("\top_asignacion -> DESPD_ASIG\n"); }
    | AND_ASIG                                                                { printf ("\top_asignacion -> AND_ASIG\n"); }
    | XOR_ASIG                                                                { printf ("\top_asignacion -> XOR_ASIG\n"); }
    | OR_ASIG                                                                 { printf ("\top_asignacion -> OR_ASIG\n"); }
    ;

instruccion_bifurcacion 
    : IF '(' expresion ')' instruccion                                        { printf ("\tinstruccion_bifurcacion -> IF ( exp ) instruccion\n"); }
    | IF '(' expresion ')' instruccion ELSE instruccion                       { printf ("\tinstruccion_bifurcacion -> IF ( exp ) instruccion ELSE instruccion\n"); }
    | SWITCH '(' expresion ')' '{' lista_instruccion_caso '}'                 { printf ("\tinstruccion_bifurcacion -> SWITCH ( exp ) { list_instruccion_caso }\n"); }
    ;

instruccion_caso 
    : CASE expresion ':' instruccion                                          { printf ("\tinstruccion_caso -> CASE exp instruccion\n"); }
    | DEFAULT ':' instruccion                                                 { printf ("\tinstruccion_caso -> DEFAULT instruccion\n"); }
    ;

lista_instruccion_caso
    : instruccion_caso                                                        { printf ("\tlist_instruccion_caso -> instruccion_caso\n"); }
    | lista_instruccion_caso instruccion_caso                                 { printf ("\tlist_instruccion_caso -> list_instruccion_caso instruccion_caso\n"); }
    ;

instruccion_bucle 
    : WHILE '(' expresion ')' instruccion                                     { printf ("\tinstruccion_bucle -> WHILE ( exp ) instruccion\n"); } 
    | DO instruccion WHILE '(' expresion ')' ';'                              { printf ("\tinstruccion_bucle -> DO instruccion WHILE ( exp )\n"); } 
    | FOR '(' ';' expresion ';' lista_expresion ')' instruccion               { printf ("\tinstruccion_bucle -> FOR ( exp list_exp ) instruccion\n"); } 
    | FOR '(' lista_asignacion ';' expresion ';' lista_expresion ')' instruccion { printf ("\tinstruccion_bucle -> FOR ( list_asignacion exp list_exp ) instruccion\n"); } 
    ;

lista_asignacion 
    : asignacion                                                              { printf ("\tlist_asignacion -> asignacion\n"); } 
    | lista_asignacion asignacion                                             { printf ("\tlist_asignacion -> list_asignacion asignacion\n"); } 
    ;

instruccion_salto 
    : GOTO IDENTIFICADOR ';'                                                  { printf ("\tinstruccion_salto -> GOTO ID\n"); } 
    | CONTINUE ';'                                                            { printf ("\tinstruccion_salto -> CONTINUE\n"); } 
    | BREAK ';'                                                               { printf ("\tinstruccion_salto -> BREAK\n"); } 
    ;

instruccion_destino_salto 
    : IDENTIFICADOR ':' instruccion ';'                                       { printf ("\tinstruccion_destino_salto -> ID instruccion\n"); } 
    ;

instruccion_retorno 
    : RETURN ';'                                                              { printf ("\tinstruccion_retorno -> RETURN\n"); } 
    | RETURN expresion ';'                                                    { printf ("\tinstruccion_retorno -> RETURN exp\n"); } 
    ;

instruccion_lanzamiento_excepcion 
    : THROW expresion ';'                                                     { printf ("\tinstruccion_lanzamiento_excepcion -> THROW exp\n"); } 
    ;

instruccion_captura_excepcion 
    : TRY bloque_instrucciones clausulas-catch                                { printf ("\tinstruccion_captura_excepcion -> TRY  bloque_instrucciones clausulas-catch\n"); } 
    | TRY bloque_instrucciones clausula_finally                               { printf ("\tinstruccion_captura_excepcion -> TRY  bloque_instrucciones clausula_finally\n"); } 
    | TRY bloque_instrucciones clausulas-catch clausula_finally               { printf ("\tinstruccion_captura_excepcion -> TRY  bloque_instrucciones clausulas-catch clausula_finally\n"); } 
    ;

clausulas-catch 
    : lista_clausula_catch_especifica                                         { printf ("\tclausulas-catch -> list_clausula_catch_especifica\n"); } 
    | clausula_catch_general                                                  { printf ("\tclausulas-catch -> clausula_catch_general\n"); } 
    | lista_clausula_catch_especifica clausula_catch_general                  { printf ("\tclausulas-catch -> list_clausula_catch_especifica clausula_catch_general\n"); } 
    ;

clausula_catch_especifica 
    : CATCH '(' nombre_tipo ')' bloque_instrucciones                          { printf ("\tclausula_catch_especifica -> CATCH '(' nom_tipo ')' bloque_instrucciones\n"); } 
    ;

lista_clausula_catch_especifica 
    : lista_clausula_catch_especifica clausula_catch_especifica               { printf ("\tlist_clausula_catch_especifica -> list_clausula_catch_especifica clausula_catch_especifica\n"); } 
    ;

clausula_catch_general 
    : CATCH bloque_instrucciones                                              { printf ("\tclausula_catch_general -> CATCH bloque_instrucciones\n"); } 
    ;

clausula_finally 
    : FINALLY bloque_instrucciones                                            { printf ("\tclausula_finally -> FINALLY bloque_instrucciones\n"); } 
    ;

instruccion_retorno 
    : RETURN ';'                                                              { printf ("\tinstruccion_retorno -> RETURN\n"); } 
    | RETURN expresion ';'                                                    { printf ("\tinstruccion_retorno -> RETURN exp\n"); } 
    ;

instruccion_vacia
    : ';'                                                                     { printf ("\tinstruccion_vacia -> ;\n"); } 
    ;
/***************/
/* EXPRESIONES */
/***************/

expresion
    : expresion_constante                                                     { printf ("\texp -> exp_cons\n"); } 
    | expresion_parentesis                                                    { printf ("\texp -> exp_parentesis\n"); } 
    | expresion_funcional                                                     { printf ("\texp -> exp_funcional\n"); } 
    | expresion_creacion_objeto                                               { printf ("\texp -> exp_creacion_objeto\n"); } 
    | expresion_indexada                                                      { printf ("\texp -> exp_index\n"); } 
    | expresion_postfija                                                      { printf ("\texp -> exp_posfija\n"); } 
    | expresion_prefija                                                       { printf ("\texp -> exp_prefija\n"); } 
    | expresion_cast                                                          { printf ("\texp -> exp_cast\n"); } 
    ;

expresion_constante 
    : ENTERO                                                                  { printf ("\texpresion_constante -> ENTERO\n"); } 
    | REAL                                                                    { printf ("\texpresion_constante -> REAL\n"); } 
    | CADENA                                                                  { printf ("\texpresion_constante -> CADENA\n"); } 
    | CARACTER                                                                { printf ("\texpresion_constante -> CARACTER\n"); } 
    | BOOLEANO                                                                { printf ("\texpresion_constante -> BOOLEANO\n"); } 
    ;

expresion_parentesis 
    : '(' expresion ')'                                                       { printf ("\texpresion_parentesis -> ( exp )\n"); } 
    ;

expresion_funcional 
    : identificador_anidado '(' ')'                                           { printf ("\texpresion_funcional -> id_anid ()\n"); } 
    | identificador_anidado '(' lista_expresion ')'                           { printf ("\texpresion_funcional -> id_anid ( list_exp )\n"); } 
    ;

expresion_creacion_objeto 
    : NEW identificador_anidado '(' ')'                                       { printf ("\texpresion_creacion_objeto -> NEW id_anid ()\n"); } 
    | NEW identificador_anidado '(' lista_expresion ')'                       { printf ("\texpresion_creacion_objeto -> NEW id_anid ( list_exp )\n"); } 
    ;

expresion_indexada 
    : identificador_anidado                                                   { printf ("\texpresion_indexada -> id_anid\n"); } 
    | expresion_indexada '[' expresion ']'                                    { printf ("\texpresion_indexada -> exp_index [ exp ]\n"); } 
    | expresion_indexada PTR_ACCESO identificador_anidado                     { printf ("\texpresion_indexada -> exp_index -> id_anid\n"); } 
    ;

expresion_postfija 
    : expresion_constante                                                     { printf ("\texpresion_postfija -> exp_cons\n"); } 
    | expresion_parentesis                                                    { printf ("\texpresion_postfija -> exp_parentesis\n"); } 
    | expresion_funcional                                                     { printf ("\texpresion_postfija -> exp_funcional\n"); } 
    | expresion_creacion_objeto                                               { printf ("\texpresion_postfija -> exp_creacion_objeto\n"); } 
    | expresion_indexada                                                      { printf ("\texpresion_postfija -> exp_index\n"); } 
    | expresion_postfija INC                                                  { printf ("\texpresion_postfija -> exp_postfija INC\n"); } 
    | expresion_postfija DEC                                                  { printf ("\texpresion_postfija -> exp_posfija DEC\n"); } 
    ;

expresion_prefija 
    : expresion_postfija                                                      { printf ("\texpresion_prefija -> exp_posfija\n"); } 
    | SIZEOF expresion_prefija                                                { printf ("\texpresion_prefija -> SIZEOF exp_prefija\n"); } 
    | SIZEOF '(' nombre_tipo ')'                                              { printf ("\texpresion_prefija -> SIZEOF ( nom_tipo )\n"); } 
    | operador_prefijo expresion_cast                                         { printf ("\texpresion_prefija -> op_prefijo exp_cast\n"); } 
    ;

operador_prefijo 
    : INC                                                                     { printf ("\top_prefijo -> INC\n"); } 
    | DEC                                                                     { printf ("\top_prefijo -> DEC\n"); } 
    | '&'                                                                     { printf ("\top_prefijo -> &\n"); } 
    | '*'                                                                     { printf ("\top_prefijo -> *\n"); } 
    | '+'                                                                     { printf ("\top_prefijo -> +\n"); }  
    | '-'                                                                     { printf ("\top_prefijo -> -\n"); } 
    | '~'                                                                     { printf ("\top_prefijo -> ~\n"); } 
    | '!'                                                                     { printf ("\top_prefijo -> !\n"); } 
    ;

expresion_cast 
    : expresion_prefija                                                       { printf ("\texpresion_cast -> exp_prefija\n"); } 
    | '(' nombre_tipo ')' expresion_prefija                                   { printf ("\texpresion_cast -> ( nom_tipo ) exp_prefija\n"); } 
    ;

%%

void yyerror(char *s) {
  fflush(stdout);
  printf("Error FATAL %s\n", s);
  }

int yywrap() {
  return 1;
  }

int main(int argc, char *argv[]) {

  //yydebug = 0;

  if (argc < 2) {
    printf("Uso: ./c-dull NombreArchivo\n");
    }
  else {
    yyin = fopen(argv[1],"r");
    yyparse();
    }
  }