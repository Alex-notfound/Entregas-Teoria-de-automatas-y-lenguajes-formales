%{
  #include <stdlib.h>
  #include <stdio.h>
  extern FILE *yyin;
  extern int yylex();
  extern int yyclearin();
  #define YYDEBUG 1

%}

%token ABSTRACT BASE BOOLEAN BREAK CASE CATCH CHAR CLASS CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN FINALLY FLOAT FOR GOTO IF INT INTERFACE INTERNAL LONG NAMESPACE NEW OVERRIDE PRIVATE PROTECTED PUBLIC RETURN SEALED SHORT SIGNED STATIC STRUCT SWITCH THIS THROW TRY TYPEDEF UNION UNSIGNED USING VIRTUAL VOID WHILE IDENTIFICADOR ENTERO CADENA REAL CARACTER BOOLEANO SIZEOF PTR_ACCESO INC DEC DESPI DESPD LE GE EQ NEQ AND OR MULT_ASIG DIV_ASIG MOD_ASIG SUMA_ASIG RESTA_ASIG DESPI_ASIG DESPD_ASIG AND_ASIG XOR_ASIG OR_ASIG

%%

/************/
/* PROGRAMA */
/************/

modulo
    : lista_declaracion                                                       { printf ("\n\tmodulo -> list_decl"); }
    | lista_directivas_uso lista_declaracion                                  { printf ("\n\tmodulo -> lista_dir_uso list_decl"); }
    ;

lista_declaracion
    : declaracion                                                             { printf ("\n\tlist_decl -> decl"); }
    | lista_declaracion declaracion                                           { printf ("\n\tlist_decl -> list_decl decl"); }
    ;

declaracion
    : declaracion_espacio_nombres                                             { printf ("\n\tdecl -> decl_esp_nom"); }
    | declaracion_variable                                                    { printf ("\n\tdecl -> decl_var"); }
    | declaracion_tipo                                                        { printf ("\n\tdecl -> decl_tipo"); }
    | declaracion_funcion                                                     { printf ("\n\tdecl -> decl_func"); }
    ;

lista_directivas_uso
    : directiva_uso                                                           { printf ("\n\tlista_dir_uso -> dir_uso"); }
    | lista_directivas_uso directiva_uso                                      { printf ("\n\tlista_dir_uso -> lista_dir_uso dir_uso"); }
    ;

directiva_uso
    : USING IDENTIFICADOR '=' nombre_tipo_o_espacio_nombres ';'               { printf ("\n\tdir_uso -> USING ID = nom_tipo_o_esp_noms"); }
    | USING nombre_tipo_o_espacio_nombres ';'                                 { printf ("\n\tdir_uso -> nom_tipo_o_esp_noms"); }
    ;

nombre_tipo_o_espacio_nombres
    : identificador_con_tipos                                                 { printf ("\n\tnom_tipo_o_esp_noms -> id_tipos"); }
    | nombre_tipo_o_espacio_nombres '.' identificador_con_tipos               { printf ("\n\tnom_tipo_o_esp_noms -> nom_tipo_o_esp_noms . id_tipos"); }
    ;

identificador_con_tipos
    : IDENTIFICADOR                                                           { printf ("\n\tid_tipos -> ID"); }
    | IDENTIFICADOR '(' lista_nombres_tipo_o_espacio_nombres ')'              { printf ("\n\tid_tipos -> ID ( list_tipos )"); }
    ;

lista_nombres_tipo_o_espacio_nombres
    : nombre_tipo_o_espacio_nombres                                           { printf ("\n\tlist_nom_tipo_o_esp_noms -> nom_tipo_o_esp_noms"); }
    | lista_nombres_tipo_o_espacio_nombres ',' nombre_tipo_o_espacio_nombres  { printf ("\n\tlist_nom_tipo_o_esp_noms -> list_nom_tipo_o_esp_noms nom_tipo_o_esp_noms"); }
    ;

/*******************/
/* ESPACIO NOMBRES */
/*******************/
declaracion_espacio_nombres 
    : NAMESPACE identificador_anidado bloque_espacio_nombres                  { printf ("\n\tdecl_esp_nom -> NMSPACE id_anid bloq_esp_nom"); }
    ;

identificador_anidado 
    : IDENTIFICADOR                                                           { printf ("\n\tid_anid -> ID"); }
    | identificador_anidado '.' IDENTIFICADOR                                 { printf ("\n\tid_anid -> id_anid . ID"); }
    ;

bloque_espacio_nombres 
    : '{' lista_declaracion '}'                                               { printf ("\n\tbloq_esp_noms -> { list_decl }"); } 
    | '{' lista_directivas_uso lista_declaracion '}'                          { printf ("\n\tbloq_esp_noms -> { lista_dir_uso list_decl }"); }
    ;

/*************/
/* VARIABLES */
/*************/
declaracion_variable 
    : tipo lista_nombre ';'                                                   { printf ("\n\tdecl_var -> tipo list_nom"); }
    ;

tipo 
    : '<' nombre_tipo_o_espacio_nombres '>'                                   { printf ("\n\ttipo -> < nom_tipo_o_esp_noms >"); }
    | tipo_escalar                                                            { printf ("\n\ttipo -> tipo_escalar"); }
    ;

tipo_escalar 
    : tipo_basico                                                             { printf ("\n\ttipo_escalar -> tipo_basico"); }
    | longitud tipo_basico                                                    { printf ("\n\ttipo_escalar -> long tipo_basico"); }
    | signo tipo_basico                                                       { printf ("\n\ttipo_escalar -> signo tipo_basico"); }
    | signo longitud tipo_basico                                              { printf ("\n\ttipo_escalar -> signo long tipo_basico"); }
    ;

longitud 
    : SHORT                                                                   { printf ("\n\tlong -> SHORT"); }
    | LONG                                                                    { printf ("\n\tlong -> LONG"); }
    ;

signo 
    : SIGNED                                                                  { printf ("\n\tsigno -> SIGNED"); }
    | UNSIGNED                                                                { printf ("\n\tsigno -> UNSIGNED"); }
    ;

tipo_basico 
    : CHAR                                                                    { printf ("\n\ttipo_basico -> CHARD"); }
    | INT                                                                     { printf ("\n\ttipo_basico -> INT"); }
    | FLOAT                                                                   { printf ("\n\ttipo_basico -> FLOAT"); }
    | DOUBLE                                                                  { printf ("\n\ttipo_basico -> DOUBLE"); }
    | BOOLEAN                                                                 { printf ("\n\ttipo_basico -> BOOLEAN"); }
    ;

nombre 
    : dato                                                                    { printf ("\n\tnom -> dato"); }
    | dato '=' valor                                                          { printf ("\n\tnom -> dato = valor"); }
    ;

dato 
    : dato_indexado                                                           { printf ("\n\tdato -> dato_indexado"); }
    | lista_asterisco dato_indexado                                           { printf ("\n\tdato -> * dato"); }
    ;

dato_indexado 
    : IDENTIFICADOR                                                           { printf ("\n\tdato_indexado -> ID"); }
    | IDENTIFICADOR '[' ']'                                                   { printf ("\n\tdato_indexado -> ID []"); }
    | IDENTIFICADOR lista_corchetes                                   		  { printf ("\n\tdato_indexado -> ID list_corchetes"); }
    ;

lista_corchetes
	: '[' lista_expresion ']'												  { printf ("\n\tlista_corchetes -> [ list_exp ]"); }
	|  lista_corchetes '[' ']' 												  { printf ("\n\tlista_corchetes -> lista_corchetes []"); }
	;

lista_expresion 
    : expresion                                                               { printf ("\n\tlist_exp -> exp"); }
    | lista_expresion ',' expresion                                           { printf ("\n\tlist_exp -> list_exp exp"); }
    ;

valor 
    : expresion                                                               { printf ("\n\tvalor -> exp"); }
    | '{' lista_valor '}'                                                     { printf ("\n\tvalor -> { list_valor }"); }
    ;

lista_valor 
    : valor                                                                   { printf ("\n\tlist_valor -> valor"); }
    | lista_valor ',' valor                                                   { printf ("\n\tlist_valor -> list_valor valor"); }

/*********/
/* TIPOS */
/*********/

declaracion_tipo 
    : nombramiento_tipo                                                       { printf ("\n\tdecl_tipo -> nombramiento_tipo"); }
    | declaracion_struct_union                                                { printf ("\n\tdecl_tipo -> decl_struct_union"); }
    | declaracion_interfaz                                                    { printf ("\n\tdecl_tipo -> decl_interfaz"); }
    | declaracion_enum                                                        { printf ("\n\tdecl_tipo -> decl_enum"); }
    | declaracion_clase                                                       { printf ("\n\tdecl_tipo -> decl_clase"); }
    ;

nombramiento_tipo 
    : TYPEDEF tipo IDENTIFICADOR ';'                                          { printf ("\n\tnombramiento_tipo -> TYPEDEF tipo ID"); }
    ;

declaracion_struct_union 
    : struct_union '{' lista_declaracion_campo '}'                            { printf ("\n\tdecl_struct_union -> struct_union { list_decl_campo }"); }
    | struct_union IDENTIFICADOR '{' lista_declaracion_campo '}'              { printf ("\n\tdecl_struct_union -> struct_union ID { list_decl_campo }"); }
    | lista_modificador struct_union '{' lista_declaracion_campo '}'          { printf ("\n\tdecl_struct_union -> list_mod struct_union { list_decl_campo }"); }
    | lista_modificador struct_union IDENTIFICADOR '{' lista_declaracion_campo '}' { printf ("  decl_struct_union -> list_mod struct_union ID { list_decl_campo }"); }
    ;

modificador 
    : NEW                                                                     { printf ("\n\tmod -> NEW"); }
    | PUBLIC                                                                  { printf ("\n\tmod -> PUBLIC"); }
    | PROTECTED                                                               { printf ("\n\tmod -> PROTECTED"); }
    | INTERNAL                                                                { printf ("\n\tmod -> INTERNAL"); }
    | PRIVATE                                                                 { printf ("\n\tmod -> PRIVATE"); }
    | STATIC                                                                  { printf ("\n\tmod -> STATIC"); }
    | VIRTUAL                                                                 { printf ("\n\tmod -> VIRTUAL"); }
    | SEALED                                                                  { printf ("\n\tmod -> SEALED"); }
    | OVERRIDE                                                                { printf ("\n\tmod -> OVERRIDE"); }
    | ABSTRACT                                                                { printf ("\n\tmod -> ABSTRACT"); }
    | EXTERN                                                                  { printf ("\n\tmod -> EXTERN"); }
    ;

lista_modificador
    : modificador                                                             { printf ("\n\tlist_mod -> mod"); }
    | lista_modificador modificador                                           { printf ("\n\tlist_mod -> list_mod mod"); }
    ;

struct_union 
    : STRUCT                                                                  { printf ("\n\tstruct_union -> STRUCT"); }
    | UNION                                                                   { printf ("\n\tstruct_union -> UNION"); }
    ;

declaracion_campo 
    : tipo lista_nombre ';'                                                   { printf ("\n\tdecl_campo -> tipo list_nom"); }
    | declaracion_struct_union lista_nombre ';'                               { printf ("\n\tdecl_campo -> decl_struct_union list_nom"); }
    ;

lista_nombre 
    : nombre                                                                  { printf ("\n\tlist_nom -> nom"); }
    | lista_nombre ',' nombre                                                 { printf ("\n\tlist_nom -> list_nom nom"); }
    ;

lista_declaracion_campo
    : declaracion_campo                                                       { printf ("\n\tlist_decl_campo -> decl_campo"); }
    | lista_declaracion_campo declaracion_campo                               { printf ("\n\tlist_decl_campo -> list_decl_campo decl_campo"); }
    ;

declaracion_interfaz 
    : INTERFACE IDENTIFICADOR herencia cuerpo_interfaz                        { printf ("\n\tdecl_interfaz -> INTERFACE ID herencia cuerpo_interfaz"); }
    | lista_modificador INTERFACE IDENTIFICADOR herencia cuerpo_interfaz      { printf ("\n\tdecl_interfaz -> list_mod INTERFACE ID herencia cuerpo_interfaz"); }
    ;

herencia 
    : ':' nombre_tipo_o_espacio_nombres                                       { printf ("\n\therencia -> nom_tipo_o_esp_noms"); }
    |                                                                         { printf ("\n\therencia"); }
    ;

lista_nombre_tipo_o_espacio_nombres 
    : nombre_tipo_o_espacio_nombres                                           { printf ("\n\tlist_nom_tipo_o_esp_noms -> nom_tipo_o_esp_noms"); }
    | lista_nombre_tipo_o_espacio_nombres ',' nombre_tipo_o_espacio_nombres   { printf ("\n\tlist_nom_tipo_o_esp_noms -> list_nom_tipo_o_esp_noms nom_tipo_o_esp_noms"); }
    ;

cuerpo_interfaz 
    : '{' '}'                                                                 { printf ("\n\tcuerpo_interfaz -> { }"); }
    | '{' lista_declaracion_metodo_interfaz '}'                               { printf ("\n\tcuerpo_interfaz -> { list_decl_metodo_inferfaz }"); }
    ;

declaracion_metodo_interfaz 
    : firma_funcion ';'                                                       { printf ("\n\tdecl_metodo_inferfaz -> firma_funcion"); }
    | NEW firma_funcion ';'                                                   { printf ("\n\tdecl_metodo_inferfaz -> NEW firma_funcion"); }
    ;

lista_declaracion_metodo_interfaz 
    : declaracion_metodo_interfaz                                             { printf ("\n\tlist_decl_metodo_inferfaz -> decl_metodo_inferfaz"); }
    | lista_declaracion_metodo_interfaz declaracion_metodo_interfaz           { printf ("\n\tlist_decl_metodo_inferfaz -> list_decl_metodo_inferfaz decl_metodo_inferfaz"); }
    ;

declaracion_enum 
    : ENUM IDENTIFICADOR cuerpo_enum                                          { printf ("\n\tdecl_enum -> ENUM ID cuerpo_enum"); }
    | ENUM IDENTIFICADOR ':' tipo_escalar cuerpo_enum                         { printf ("\n\tdecl_enum -> ENUM ID tipo_escalar cuerpo_enum"); }
    | lista_modificador ENUM IDENTIFICADOR cuerpo_enum                        { printf ("\n\tdecl_enum -> list_mod ENUM ID cuerpo_enum"); }
    | lista_modificador ENUM IDENTIFICADOR ':' tipo_escalar cuerpo_enum       { printf ("\n\tdecl_enum -> list_mod ENUM ID tipo_escalar cuerpo_enum"); }
    ;

cuerpo_enum 
    : '{' lista_declaracion_miembro_enum '}'                                  { printf ("\n\tcuerpo_enum -> { list_decl_miembro_enum }"); }
    ;

declaracion_miembro_enum 
    : IDENTIFICADOR                                                           { printf ("\n\tdecl_miembro_enum -> ID"); }
    | IDENTIFICADOR '=' expresion                                             { printf ("\n\tdecl_miembro_enum -> ID = exp"); }
    ;

lista_declaracion_miembro_enum 
    : declaracion_miembro_enum                                                { printf ("\n\tlist_decl_miembro_enum -> decl_miembro_enum"); }
    | lista_declaracion_miembro_enum ',' declaracion_miembro_enum             { printf ("\n\tlist_decl_miembro_enum -> list_decl_miembro_enum decl_miembro_enum"); }
    ;

/**********/
/* CLASES */
/**********/
declaracion_clase 
    : CLASS IDENTIFICADOR herencia cuerpo_clase                               { printf ("\n\tdecl_clase -> CLASS ID herencia cuerpo_clase"); }     
    | lista_modificador CLASS IDENTIFICADOR herencia cuerpo_clase             { printf ("\n\tdecl_clase -> list_mod CLASS ID herencia cuerpo_clase"); }
    ;

cuerpo_clase 
    : '{' lista_declaracion_elemento_clase '}'                                { printf ("\n\tcuerpo_clase -> list_decl_elem_clase"); }
    ;

declaracion_elemento_clase 
    : declaracion_tipo                                                        { printf ("\n\tdecl_elem_clase -> decl_tipo"); }
    | declaracion_metodo                                                      { printf ("\n\tdecl_elem_clase -> decl_metodo"); }
    | declaracion_constructor                                                 { printf ("\n\tdecl_elem_clase -> decl_constructor"); }
    | declaracion_destructor                                                  { printf ("\n\tdecl_elem_clase -> decl_destructor"); }
    | declaracion_atributo                                                    { printf ("\n\tdecl_elem_clase -> decl_atributo"); }
    ;

lista_declaracion_elemento_clase 
    : declaracion_elemento_clase                                              { printf ("\n\tlist_decl_elem_clase -> decl_elem_clase"); }
    | lista_declaracion_elemento_clase declaracion_elemento_clase             { printf ("\n\tlist_decl_elem_clase -> list_decl_elem_clase decl_elem_clase"); }
    ;

declaracion_atributo 
    : declaracion_variable                                                    { printf ("\n\tdecl_atributo -> decl_var"); }
    | lista_modificador declaracion_variable                                  { printf ("\n\tdecl_atributo -> list_mod decl_var"); }
    ;

declaracion_metodo 
    : firma_funcion bloque_instrucciones                                      { printf ("\n\tdecl_metodo -> firma_funcion bloque_instrucciones"); }
    | lista_modificador firma_funcion bloque_instrucciones                    { printf ("\n\tdecl_metodo -> list_mod firma_funcion bloque_instrucciones"); }
    ;

declaracion_constructor 
    : cabecera_constructor bloque_instrucciones                               { printf ("\n\tdecl_constructor -> cabecera_constructor bloque_instrucciones"); }
    | lista_modificador cabecera_constructor bloque_instrucciones             { printf ("\n\tdecl_constructor -> list_mod cabecera_constructor bloque_instrucciones"); }
    ;

cabecera_constructor 
    : IDENTIFICADOR                                                           { printf ("\n\tcabecera_constructor -> ID"); }
    | IDENTIFICADOR parametros inicializador_constructor                      { printf ("\n\tcabecera_constructor -> ID parametros inicializador_constructor"); }
    | IDENTIFICADOR parametros                                                { printf ("\n\tcabecera_constructor -> ID parametros"); }
    | IDENTIFICADOR inicializador_constructor                                 { printf ("\n\tcabecera_constructor -> ID inicializador_constructor"); }
    ;

inicializador_constructor 
    : ':' BASE parametros                                                     { printf ("\n\tinicializador_constructor -> BASE parametros"); }
    | ':' THIS parametros                                                     { printf ("\n\tinicializador_constructor -> THIS parametros"); }
    ;

declaracion_destructor 
    : cabecera_destructor bloque_instrucciones                                { printf ("\n\tdecl_destructor -> cabecera_destructor bloque_instrucciones"); }
    | lista_modificador cabecera_destructor bloque_instrucciones              { printf ("\n\tdecl_destructor -> list_mod cabecera_destructor bloque_instrucciones"); }
    ;

cabecera_destructor 
    : '~' IDENTIFICADOR '(' ')'                                               { printf ("\n\tcabecera_destructor -> ~ ID ()"); }
    ;

/*************/
/* FUNCIONES */
/*************/
declaracion_funcion 
    : firma_funcion bloque_instrucciones                                      { printf ("\n\tdecl_func -> firma_funcion bloque_instrucciones"); }
    ;

firma_funcion 
    : VOID IDENTIFICADOR parametros                                           { printf ("\n\tfirma_funcion -> VOID ID parametros"); }
    | tipo IDENTIFICADOR parametros                                           { printf ("\n\tfirma_funcion -> tipo ID parametros"); }
    | tipo lista_asterisco IDENTIFICADOR parametros                           { printf ("\n\tfirma_funcion -> tipo list_asterisco ID parametros"); }
    ;

asterisco
    : '*'
    ;

lista_asterisco
    : asterisco                                                               { printf ("\n\tlist_asterisco -> *"); } 
    | lista_asterisco asterisco                                               { printf ("\n\tlist_asterisco -> list_asterisco *"); }
    ;

parametros 
    : '(' ')'                                                                 { printf ("\n\tparametros -> ()"); }
    | '(' lista_argumentos ')'                                                { printf ("\n\tparametros -> ( list_argumentos )"); }
    ;

argumentos 
    : nombre_tipo lista_variable                                              { printf ("\n\targumentos -> nom_tipo list_var"); }
    ;

lista_argumentos 
    : argumentos                                                              { printf ("\n\tlist_argumentos -> argumentos"); }
    | lista_argumentos ';' argumentos                                         { printf ("\n\tlist_argumentos -> list_argumentos argumentos"); }
    ;

nombre_tipo 
    : tipo                                                                    { printf ("\n\tnom_tipo -> tipo"); }
    | tipo lista_asterisco                                                    { printf ("\n\tnom_tipo -> tipo list_asterisco"); }
    ;

variable
    : IDENTIFICADOR                                                           { printf ("\n\tvar -> ID"); }
    | IDENTIFICADOR '=' expresion                                             { printf ("\n\tvar -> ID = exp"); }
    ;

lista_variable 
    : variable                                                                { printf ("\n\tlist_var -> var"); }
    | lista_variable ',' variable                                             { printf ("\n\tlist_var -> list_var var"); }
    ;

/*****************/
/* INSTRUCCIONES */
/*****************/
instruccion 
    : bloque_instrucciones                                                    { printf ("\n\tinstruccion -> bloque_instrucciones"); }
    | instruccion_expresion                                                   { printf ("\n\tinstruccion -> instruccion_exp"); }
    | instruccion_bifurcacion                                                 { printf ("\n\tinstruccion -> instruccion_bifurcacion"); }
    | instruccion_bucle                                                       { printf ("\n\tinstruccion -> instruccion_bucle"); }
    | instruccion_salto                                                       { printf ("\n\tinstruccion -> instruccion_salto"); }
    | instruccion_destino_salto                                               { printf ("\n\tinstruccion -> instruccion_destino_salto"); }
    | instruccion_retorno                                                     { printf ("\n\tinstruccion -> instruccion_retorno"); }
    | instruccion_lanzamiento_excepcion                                       { printf ("\n\tinstruccion -> instruccion_lanzamiento_excepcion"); }
    | instruccion_captura_excepcion                                           { printf ("\n\tinstruccion -> instruccion_captura_excepcion"); }
    | instruccion_vacia                                                       { printf ("\n\tinstruccion -> instruccion_vacia"); }
    ;

lista_instruccion 
    : instruccion                                                             { printf ("\n\tlist_instruccion -> instruccion"); }
    | lista_instruccion instruccion                                           { printf ("\n\tlist_instruccion -> list_instruccion instruccion"); }
    ;

bloque_instrucciones 
    : '{' '}'                                                                 { printf ("\n\tbloque_instrucciones -> {}"); }
    | '{' lista_declaracion '}'                                               { printf ("\n\tbloque_instrucciones -> { list_decl }"); }
    | '{' lista_instruccion '}'                                               { printf ("\n\tbloque_instrucciones -> { list_instruccion }"); }
    | '{' lista_declaracion lista_instruccion '}'                             { printf ("\n\tbloque_instrucciones -> { list_decl list_instruccion }"); }
    ;

instruccion_expresion 
    : expresion_funcional ';'                                                 { printf ("\n\tinstruccion_expresion -> exp_funcional"); }
    | asignacion ';'                                                          { printf ("\n\tinstruccion_expresion -> asignacion"); }
    ;

asignacion 
    : expresion_indexada operador_asignacion expresion                        { printf ("\n\tasignacion -> exp_index op_asignacion exp"); }
    ;

operador_asignacion 
    : '='                                                                     { printf ("\n\top_asignacion -> ="); }
    | MULT_ASIG                                                               { printf ("\n\top_asignacion -> MULT_ASIG"); }
    | DIV_ASIG                                                                { printf ("\n\top_asignacion -> DIV_ASIG"); }
    | MOD_ASIG                                                                { printf ("\n\top_asignacion -> MOD_ASIG"); }
    | SUMA_ASIG                                                               { printf ("\n\top_asignacion -> SUMA_ASIG"); }
    | RESTA_ASIG                                                              { printf ("\n\top_asignacion -> RESTA_ASIG"); }
    | DESPI_ASIG                                                              { printf ("\n\top_asignacion -> DESPI_ASIG"); }
    | DESPD_ASIG                                                              { printf ("\n\top_asignacion -> DESPD_ASIG"); }
    | AND_ASIG                                                                { printf ("\n\top_asignacion -> AND_ASIG"); }
    | XOR_ASIG                                                                { printf ("\n\top_asignacion -> XOR_ASIG"); }
    | OR_ASIG                                                                 { printf ("\n\top_asignacion -> OR_ASIG"); }
    ;

instruccion_bifurcacion 
    : IF '(' expresion ')' instruccion                                        { printf ("\n\tinstruccion_bifurcacion -> IF ( exp ) instruccion"); }
    | IF '(' expresion ')' instruccion ELSE instruccion                       { printf ("\n\tinstruccion_bifurcacion -> IF ( exp ) instruccion ELSE"); }
    | SWITCH '(' expresion ')' '{' lista_instruccion_caso '}'                 { printf ("\n\tinstruccion_bifurcacion -> SWITCH ( exp ) { list_instruccion_caso }");}
    ;

instruccion_caso 
    : CASE expresion ':' instruccion                                          { printf ("\n\tinstruccion_caso -> CASE exp instruccion"); }
    | DEFAULT ':' instruccion                                                 { printf ("\n\tinstruccion_caso -> DEFAULT instruccion"); }
    ;

lista_instruccion_caso
    : instruccion_caso                                                        { printf ("\n\tlist_instruccion_caso -> instruccion_caso"); }
    | lista_instruccion_caso instruccion_caso                                 { printf ("\n\tlist_instruccion_caso -> list_instruccion_caso instruccion_caso"); }
    ;

instruccion_bucle 
    : WHILE '(' expresion ')' instruccion                                     { printf ("\n\tinstruccion_bucle -> WHILE ( exp ) instruccion"); } 
    | DO instruccion WHILE '(' expresion ')' ';'                              { printf ("\n\tinstruccion_bucle -> DO instruccion WHILE ( exp )"); } 
    | FOR '(' ';' expresion ';' lista_expresion ')' instruccion               { printf ("\n\tinstruccion_bucle -> FOR ( exp list_exp ) instruccion"); } 
    | FOR '(' lista_asignacion ';' expresion ';' lista_expresion ')' instruccion { printf ("\n\tinstruccion_bucle -> FOR ( list_asignacion exp list_exp ) instruccion"); } 
    ;

lista_asignacion 
    : asignacion                                                              { printf ("\n\tlist_asignacion -> asignacion"); } 
    | lista_asignacion asignacion                                             { printf ("\n\tlist_asignacion -> list_asignacion asignacion"); } 
    ;

instruccion_salto 
    : GOTO IDENTIFICADOR ';'                                                  { printf ("\n\tinstruccion_salto -> GOTO ID"); } 
    | CONTINUE ';'                                                            { printf ("\n\tinstruccion_salto -> CONTINUE"); } 
    | BREAK ';'                                                               { printf ("\n\tinstruccion_salto -> BREAK"); } 
    ;

instruccion_destino_salto 
    : IDENTIFICADOR ':' instruccion ';'                                       { printf ("\n\tinstruccion_destino_salto -> ID instruccion"); } 
    ;

instruccion_retorno 
    : RETURN ';'                                                              { printf ("\n\tinstruccion_retorno -> RETURN"); } 
    | RETURN expresion ';'                                                    { printf ("\n\tinstruccion_retorno -> RETURN exp"); } 
    ;

instruccion_lanzamiento_excepcion 
    : THROW expresion ';'                                                     { printf ("\n\tinstruccion_lanzamiento_excepcion -> THROW exp"); } 
    ;

instruccion_captura_excepcion 
    : TRY bloque_instrucciones clausulas-catch                                { printf ("\n\tinstruccion_captura_excepcion -> TRY  bloque_instrucciones clausulas-catch"); } 
    | TRY bloque_instrucciones clausula_finally                               { printf ("\n\tinstruccion_captura_excepcion -> TRY  bloque_instrucciones clausula_finally"); } 
    | TRY bloque_instrucciones clausulas-catch clausula_finally               { printf ("\n\tinstruccion_captura_excepcion -> TRY  bloque_instrucciones clausulas-catch clausula_finally"); } 
    ;

clausulas-catch 
    : lista_clausula_catch_especifica                                         { printf ("\n\tclausulas-catch -> list_clausula_catch_especifica"); } 
    | clausula_catch_general                                                  { printf ("\n\tclausulas-catch -> clausula_catch_general"); } 
    | lista_clausula_catch_especifica clausula_catch_general                  { printf ("\n\tclausulas-catch -> list_clausula_catch_especifica clausula_catch_general"); } 
    ;

clausula_catch_especifica 
    : CATCH '(' nombre_tipo ')' bloque_instrucciones                          { printf ("\n\tclausula_catch_especifica -> CATCH '(' nom_tipo ')' bloque_instrucciones"); } 
    ;

lista_clausula_catch_especifica 
    : lista_clausula_catch_especifica clausula_catch_especifica               { printf ("\n\tlist_clausula_catch_especifica -> list_clausula_catch_especifica clausula_catch_especifica"); } 
    ;

clausula_catch_general 
    : CATCH bloque_instrucciones                                              { printf ("\n\tclausula_catch_general -> CATCH bloque_instrucciones"); } 
    ;

clausula_finally 
    : FINALLY bloque_instrucciones                                            { printf ("\n\tclausula_finally -> FINALLY bloque_instrucciones"); } 
    ;

instruccion_vacia
    : ';'                                                                     { printf ("\n\tinstruccion_vacia -> ;"); } 
    ;

/***************/
/* EXPRESIONES */
/***************/

expresion_constante 
    : ENTERO                                                                  { printf ("\n\texp_constante -> ENTERO"); } 
    | REAL                                                                    { printf ("\n\texp_constante -> REAL"); } 
    | CADENA                                                                  { printf ("\n\texp_constante -> CADENA"); } 
    | CARACTER                                                                { printf ("\n\texp_constante -> CARACTER"); } 
    | BOOLEANO                                                                { printf ("\n\texp_constante -> BOOLEANO"); } 
    ;

expresion_parentesis 
    : '(' expresion ')'                                                       { printf ("\n\texp_parentesis -> ( exp )"); } 
    ;

expresion_funcional 
    : identificador_anidado '(' ')'                                           { printf ("\n\texp_funcional -> id_anid ()"); } 
    | identificador_anidado '(' lista_expresion ')'                           { printf ("\n\texp_funcional -> id_anid ( list_exp )"); } 
    ;

expresion_creacion_objeto 
    : NEW identificador_anidado '(' ')'                                       { printf ("\n\texp_creacion_objeto -> NEW id_anid ()"); } 
    | NEW identificador_anidado '(' lista_expresion ')'                       { printf ("\n\texp_creacion_objeto -> NEW id_anid ( list_exp )"); } 
    ;

expresion_indexada 
    : identificador_anidado                                                   { printf ("\n\texp_indexada -> id_anid"); } 
    | expresion_indexada '[' expresion ']'                                    { printf ("\n\texp_indexada -> exp_index [ exp ]"); } 
    | expresion_indexada PTR_ACCESO identificador_anidado                     { printf ("\n\texp_indexada -> exp_index -> id_anid"); } 
    ;

expresion_postfija 
    : expresion_constante                                                     { printf ("\n\texp_postfija -> exp_cons"); } 
    | expresion_parentesis                                                    { printf ("\n\texp_postfija -> exp_parentesis"); } 
    | expresion_funcional                                                     { printf ("\n\texp_postfija -> exp_funcional"); } 
    | expresion_creacion_objeto                                               { printf ("\n\texp_postfija -> exp_creacion_objeto"); } 
    | expresion_indexada                                                      { printf ("\n\texp_postfija -> exp_index"); } 
    | expresion_postfija INC                                                  { printf ("\n\texp_postfija -> exp_postfija INC"); } 
    | expresion_postfija DEC                                                  { printf ("\n\texp_postfija -> exp_posfija DEC"); } 
    ;

expresion_prefija 
    : expresion_postfija                                                      { printf ("\n\texp_prefija -> exp_posfija"); } 
    | SIZEOF expresion_prefija                                                { printf ("\n\texp_prefija -> SIZEOF exp_prefija"); } 
    | SIZEOF '(' nombre_tipo ')'                                              { printf ("\n\texp_prefija -> SIZEOF ( nom_tipo )"); } 
    | operador_prefijo expresion_cast                                         { printf ("\n\texp_prefija -> op_prefijo exp_cast"); } 
    ;

operador_prefijo 
    : INC                                                                     { printf ("\n\top_prefijo -> INC"); } 
    | DEC                                                                     { printf ("\n\top_prefijo -> DEC"); } 
    | '&'                                                                     { printf ("\n\top_prefijo -> &"); } 
    | '*'                                                                     { printf ("\n\top_prefijo -> *"); } 
    | '+'                                                                     { printf ("\n\top_prefijo -> +"); }  
    | '-'                                                                     { printf ("\n\top_prefijo -> -"); } 
    | '~'                                                                     { printf ("\n\top_prefijo -> ~"); } 
    | '!'                                                                     { printf ("\n\top_prefijo -> !"); } 
    ;

expresion_cast 
    : expresion_prefija                                                       { printf ("\n\texp_cast -> exp_prefija"); } 
    | '(' nombre_tipo ')' expresion_prefija                                   { printf ("\n\texp_cast -> ( nom_tipo ) exp_prefija"); } 
    ;

expresion_logica 
    : expresion_logica OR expresion_logica1                                   { printf ("\n\texp_logica -> exp_logica OR exp_logica1"); }
    | expresion_logica1                                                       { printf ("\n\texp_logica -> exp_logica1 "); }
    ;

expresion_logica1
    : expresion_logica1 AND expresion_logica2                                 { printf ("\n\texp_logica1 -> exp_logica1 AND exp_logica2"); }
    | expresion_logica2                                                       { printf ("\n\texp_logica1 -> exp_logica2 "); }
    ;

expresion_logica2
    : expresion_logica2 EQ expresion_logica3                                  { printf ("\n\texp_logica2 -> exp_logica2 EQ exp_logica3"); }
    | expresion_logica2 NEQ expresion_logica3                                 { printf ("\n\texp_logica2 -> exp_logica2 NEQ exp_logica3"); }
    | expresion_logica3
    ;

expresion_logica3
    : expresion_logica3 '<' expresion_logica4                                 { printf ("\n\texp_logica3 -> exp_logica3 < exp_logica4"); }
    | expresion_logica3 '>' expresion_logica4                                 { printf ("\n\texp_logica3 -> exp_logica3 > exp_logica4"); }
    | expresion_logica3 GE expresion_logica4                                  { printf ("\n\texp_logica3 -> exp_logica3 GE exp_logica4"); }
    | expresion_logica3 LE expresion_logica4                                  { printf ("\n\texp_logica3 -> exp_logica3 LE exp_logica4"); }
    | expresion_logica4
    ;

expresion_logica4
    : expresion_logica4 '|' expresion_logica5                                 { printf("\n\texp_logica4 -> exp_logica4 | exp_logica5"); }
    | expresion_logica5                                                       { printf("\n\texp_logica4 -> exp_logica5 "); }
    ;

expresion_logica5
    : expresion_logica5 '^' expresion_logica6                                 { printf ("\n\texp_logica5 -> exp_logica5 '^' exp_logica6"); }
    | expresion_logica6                                                       { printf ("\n\texp_logica5 -> exp_logica6 "); }
    ;

expresion_logica6
    : expresion_logica6 '&' expresion_logica7                                 { printf ("\n\texp_logica6 -> exp_logica7 & exp_logica6"); }
    | expresion_logica7                                                       { printf ("\n\texp_logica6 -> exp_logica7 "); }
    ;

expresion_logica7
    : expresion_logica7 DESPI expresion_logica8                               { printf ("\n\texp_logica7 -> exp_logica7 DESPI exp_logica8"); }
    | expresion_logica7 DESPD expresion_logica8                               { printf ("\n\texp_logica7 -> exp_logica7 DESPD exp_logica8"); }
    | expresion_logica8                                                       { printf ("\n\texp_logica7 -> exp_logica8 "); }
    ;

expresion_logica8
    : expresion_logica8 '+' expresion_logica9                                 { printf ("\n\texp_logica8 -> exp_logica8 + exp_logica9"); }
    | expresion_logica8 '-' expresion_logica9                                 { printf ("\n\texp_logica8 -> exp_logica8 - exp_logica9"); }
    | expresion_logica9                                                       { printf ("\n\texp_logica8 -> exp_logica9"); }
    ;

expresion_logica9
    : expresion_logica9 '*' expresion_cast                                    { printf ("\n\texp_logica9 -> exp_logica9 * exp_cast"); }
    | expresion_logica9 '/' expresion_cast                                    { printf ("\n\texp_logica9 -> exp_logica9 / exp_cast"); }
    | expresion_logica9 '%' expresion_cast                                    { printf ("\n\texp_logica9 -> exp_logica9 % exp_cast"); }
    | expresion_cast                                                          { printf ("\n\texp_logica9 -> exp_cast"); }
    ;

expresion
    : expresion_logica                                                        { printf ("\n\texp -> exp_logica"); }
    | expresion_logica '?' expresion ':' expresion                            { printf ("\n\texp -> exp_logica ? exp : exp"); }
    ;

%%

void yyerror(char *s) {
  fflush(stdout);
  printf("\nError FATAL: %s", s);
  }

int yywrap() {
  return 1;
  }

int main(int argc, char *argv[]) {

  yydebug = 0;

  if (argc < 2) {
    printf("Uso: ./c-dull NombreArchivo");
    }
  else {
    yyin = fopen(argv[1],"r");
    yyparse();
    }
  }