%{
  #include <stdlib.h>
  #include <stdio.h>
  extern FILE *yyin;
  extern int linea;

  #define YYDEBUG 1

%}

%token ABSTRACT BASE BOOLEAN BREAK CASE CATCH CHAR CLASS CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN
%token FINALLY FLOAT FOR GOTO IF INT INTERFACE INTERNAL LONG NAMESPACE NEW OVERRIDE PRIVATE PROTECTED PUBLIC RETURN
%token SEALED SHORT SIGNED STATIC STRUCT SWITCH THIS THROW TRY TYPEDEF UNION UNSIGNED USING VIRTUAL VOID WHILE

%token IDENTIFICADOR ENTERO CADENA REAL CARACTER BOOLEANO SIZEOF PTR_ACCESO INC DEC DESPI DESPD LE GE EQ NEQ AND
%token OR MULT_ASIG DIV_ASIG MOD_ASIG SUMA_ASIG RESTA_ASIG DESPI_ASIG DESPD_ASIG AND_ASIG XOR_ASIG OR_ASIG

%%

/************/
/* PROGRAMA */
/************/

modulo
    : lista_declaracion                                                       { printf ("  modulo -> list_decl\n"); }
    | lista_directivas_uso lista_declaracion                                  { printf ("  modulo -> lista_dir_uso list_decl\n"); }
    ;

lista_declaracion
    : declaracion                                                             { printf ("  list_decl -> decl\n"); }
    | lista_declaracion declaracion                                           { printf ("  list_decl -> list_decl decl\n"); }
    ;

declaracion
    : declaracion_espacio_nombres                                             { printf ("  decl -> decl_esp_nom\n"); }
    | declaracion_variable                                                    { printf ("  decl -> decl_var\n"); }
    | declaracion_tipo                                                        { printf ("  decl -> decl_tipo\n"); }
    | declaracion_funcion                                                     { printf ("  decl -> decl_func\n"); }
    ;

lista_directivas_uso
    : directiva_uso                                                           { printf ("  lista_dir_uso -> dir_uso\n"); }
    | lista_directivas_uso directiva_uso                                      { printf ("  lista_dir_uso -> lista_dir_uso dir_uso\n"); }
    ;

directiva_uso
    : USING IDENTIFICADOR '=' nombre_tipo_o_espacio_nombres ';'               { printf ("  dir_uso -> USING ID = nom_tipo_o_esp_noms\n"); }
    | USING nombre_tipo_o_espacio_nombres ';'                                 { printf ("  dir_uso -> nom_tipo_o_esp_noms\n"); }
    ;

nombre_tipo_o_espacio_nombres
    : identificador_con_tipos                                                 { printf ("  nom_tipo_o_esp_noms -> id_tipos\n"); }
    | nombre_tipo_o_espacio_nombres '.' identificador_con_tipos               { printf ("  nom_tipo_o_esp_noms -> nom_tipo_o_esp_noms . id_tipos\n"); }
    ;

identificador_con_tipos
    : IDENTIFICADOR                                                           { printf ("  id_tipos -> ID\n"); }
    | IDENTIFICADOR '(' lista_nombres_tipo_o_espacio_nombres ')'              { printf ("  id_tipos -> ID ( list_tipos )\n"); }
    ;

lista_nombres_tipo_o_espacio_nombres
    : nombre_tipo_o_espacio_nombres                                           { printf ("  list_nom_tipo_o_esp_noms -> nom_tipo_o_esp_noms\n"); }
    | lista_nombres_tipo_o_espacio_nombres ',' nombre_tipo_o_espacio_nombres  { printf ("  list_nom_tipo_o_esp_noms -> list_nom_tipo_o_esp_noms nom_tipo_o_esp_noms\n"); }
    ;

/*******************/
/* ESPACIO NOMBRES */
/*******************/
declaracion_espacio_nombres 
    : NAMESPACE identificador_anidado bloque_espacio_nombres                  { printf ("  decl_esp_nom -> NMSPACE id_anid bloq_esp_nom\n"); }
    ;

identificador_anidado 
    : IDENTIFICADOR                                                           { printf ("  id_anid -> ID\n"); }
    | identificador_anidado '.' IDENTIFICADOR                                 { printf ("  id_anid -> id_anid . ID\n"); }
    ;

bloque_espacio_nombres 
    : '{' lista_declaracion '}'                                               { printf ("  bloq_esp_noms -> { list_decl }\n"); } 
    | '{' lista_directivas_uso lista_declaracion '}'                           { printf ("  bloq_esp_noms -> { lista_dir_uso list_decl }\n"); }
    ;

/*************/
/* VARIABLES */
/*************/
declaracion_variable 
    : tipo lista_nombre ';'                                                   { printf ("  decl_var -> tipo list_nom\n"); }
    ;

tipo 
    : '<' nombre_tipo_o_espacio_nombres '>'                                   { printf ("  tipo -> < nom_tipo_o_esp_noms >\n"); }
    | tipo_escalar                                                            { printf ("  tipo -> tipo_escalar\n"); }
    ;

tipo_escalar 
    : tipo_basico                                                             { printf ("  tipo_escalar -> tipo_basico\n"); }
    | longitud tipo_basico                                                    { printf ("  tipo_escalar -> long tipo_basico\n"); }
    | signo tipo_basico                                                       { printf ("  tipo_escalar -> signo tipo_basico\n"); }
    | signo longitud tipo_basico                                              { printf ("  tipo_escalar -> signo long tipo_basico\n"); }
    ;

longitud 
    : SHORT                                                                   { printf ("  long -> SHORT\n"); }
    | LONG                                                                    { printf ("  long -> LONG\n"); }
    ;

signo 
    : SIGNED                                                                  { printf ("  signo -> SIGNED\n"); }
    | UNSIGNED                                                                { printf ("  signo -> UNSIGNED\n"); }
    ;

tipo_basico 
    : CHAR                                                                    { printf ("  tipo_basico -> CHARD\n"); }
    | INT                                                                     { printf ("  tipo_basico -> INT\n"); }
    | FLOAT                                                                   { printf ("  tipo_basico -> FLOAT\n"); }
    | DOUBLE                                                                  { printf ("  tipo_basico -> DOUBLE\n"); }
    | BOOLEAN                                                                 { printf ("  tipo_basico -> BOOLEAN\n"); }
    ;

nombre 
    : dato                                                                    { printf ("  nom -> dato\n"); }
    | dato '=' valor                                                          { printf ("  nom -> dato = valor\n"); }
    ;

lista_nombre 
    : nombre                                                                  { printf ("  list_nom -> nom\n"); }
    | lista_nombre nombre                                                     { printf ("  list_nom -> list_nom nom\n"); }
    ;
//Posiblemente mal:
dato 
    : dato_indexado                                                           { printf ("  dato -> dato_indexado\n"); }
    | '*' dato                                                                { printf ("  dato -> * dato\n"); }
    ;

dato_indexado 
    : IDENTIFICADOR                                                           { printf ("  dato_indexado -> ID\n"); }
    | IDENTIFICADOR '[' ']'                                                   { printf ("  dato_indexado -> ID []\n"); }
    | IDENTIFICADOR '[' lista_expresion ']'                                   { printf ("  dato_indexado -> ID [ list_exp ]\n"); }
    ;

lista_expresion 
    : expresion                                                               { printf ("  list_exp -> exp\n"); }
    | lista_expresion ',' expresion                                           { printf ("  list_exp -> list_exp exp\n"); }
    ;

valor 
    : expresion                                                               { printf ("  valor -> exp\n"); }
    | '{' lista_valor '}'                                                     { printf ("  valor -> { list_valor }\n"); }
    ;

lista_valor 
    : valor                                                                   { printf ("  list_valor -> valor\n"); }
    | lista_valor ',' valor                                                   { printf ("  list_valor -> list_valor valor\n"); }

/*********/
/* TIPOS */
/*********/

declaracion_tipo 
    : nombramiento_tipo                                                       { printf ("  decl_tipo -> nombramiento_tipo\n"); }
    | declaracion_struct_union                                                { printf ("  decl_tipo -> decl_struct_union\n"); }
    | declaracion_interfaz                                                    { printf ("  decl_tipo -> decl_interfaz\n"); }
    | declaracion_enum                                                        { printf ("  decl_tipo -> decl_enum\n"); }
    | declaracion_clase                                                       { printf ("  decl_tipo -> decl_clase\n"); }
    ;

nombramiento_tipo 
    : TYPEDEF tipo IDENTIFICADOR ';'                                          { printf ("  nombramiento_tipo -> TYPEDEF tipo ID\n"); }
    ;

declaracion_struct_union 
    : struct_union '{' lista_declaracion_campo '}'                            { printf ("  decl_struct_union -> struct_union { list_decl_campo }\n"); }
    | lista_modificador struct_union '{' lista_declaracion_campo '}'          { printf ("  decl_struct_union -> list_mod struct_union { list_decl_campo }\n"); }
    | lista_modificador struct_union IDENTIFICADOR '{' lista_declaracion_campo '}' { printf ("  decl_struct_union -> list_mod struct_union ID { list_decl_campo }\n"); }
    ;

modificador 
    : NEW                                                                     { printf ("  mod -> NEW\n"); }
    | PUBLIC                                                                  { printf ("  mod -> PUBLIC\n"); }
    | PROTECTED                                                               { printf ("  mod -> PROTECTED\n"); }
    | INTERNAL                                                                { printf ("  mod -> INTERNAL\n"); }
    | PRIVATE                                                                 { printf ("  mod -> PRIVATE\n"); }
    | STATIC                                                                  { printf ("  mod -> STATIC\n"); }
    | VIRTUAL                                                                 { printf ("  mod -> VIRTUAL\n"); }
    | SEALED                                                                  { printf ("  mod -> SEALED\n"); }
    | OVERRIDE                                                                { printf ("  mod -> OVERRIDE\n"); }
    | ABSTRACT                                                                { printf ("  mod -> ABSTRACT\n"); }
    | EXTERN                                                                  { printf ("  mod -> EXTERN\n"); }
    ;

lista_modificador
    : modificador                                                             { printf ("  list_mod -> mod\n"); }
    | lista_modificador modificador                                           { printf ("  list_mod -> list_mod mod\n"); }
    ;

struct_union 
    : STRUCT                                                                  { printf ("  struct_union -> STRUCT\n"); }
    | UNION                                                                   { printf ("  struct_union -> UNION\n"); }
    ;

declaracion_campo 
    : tipo lista_nombre ';'                                                   { printf ("  decl_campo -> tipo list_nom\n"); }
    | declaracion_struct_union lista_nombre ';'                               { printf ("  decl_campo -> decl_struct_union list_nom\n"); }
    ;

lista_nombre 
    : nombre                                                                  { printf ("  list_nom -> nom\n"); }
    | lista_nombre ',' nombre                                                 { printf ("  list_nom -> list_nom nom\n"); }
    ;

lista_declaracion_campo
    : declaracion_campo                                                       { printf ("  list_decl_campo -> decl_campo\n"); }
    | lista_declaracion_campo declaracion_campo                               { printf ("  list_decl_campo -> list_decl_campo decl_campo\n"); }
    ;

declaracion_interfaz 
    : INTERFACE IDENTIFICADOR herencia cuerpo_interfaz                        { printf ("  decl_interfaz -> INTERFACE ID herencia cuerpo_interfaz\n"); }
    | lista_modificador INTERFACE IDENTIFICADOR herencia cuerpo_interfaz      { printf ("  decl_interfaz -> list_mod INTERFACE ID herencia cuerpo_interfaz\n"); }
    ;

herencia 
    : ':' nombre_tipo_o_espacio_nombres                                       { printf ("  herencia -> nom_tipo_o_esp_noms\n"); }
    ;

lista_nombre_tipo_o_espacio_nombres 
    : nombre_tipo_o_espacio_nombres                                           { printf ("  list_nom_tipo_o_esp_noms -> nom_tipo_o_esp_noms\n"); }
    | lista_nombre_tipo_o_espacio_nombres ',' nombre_tipo_o_espacio_nombres   { printf ("  list_nom_tipo_o_esp_noms -> list_nom_tipo_o_esp_noms nom_tipo_o_esp_noms\n"); }
    ;

cuerpo_interfaz 
    : '{' '}'                                                                 { printf ("  cuerpo_interfaz -> { }\n"); }
    | '{' lista_declaracion_metodo_interfaz '}'                               { printf ("  cuerpo_interfaz -> { list_decl_metodo_inferfaz }\n"); }
    ;

declaracion_metodo_interfaz 
    : firma_funcion ';'                                                       { printf ("  decl_metodo_inferfaz -> firma_funcion\n"); }
    | NEW firma_funcion ';'                                                   { printf ("  decl_metodo_inferfaz -> NEW firma_funcion\n"); }
    ;

lista_declaracion_metodo_interfaz 
    : declaracion_metodo_interfaz                                             { printf ("  list_decl_metodo_inferfaz -> decl_metodo_inferfaz\n"); }
    | lista_declaracion_metodo_interfaz declaracion_metodo_interfaz           { printf ("  list_decl_metodo_inferfaz -> list_decl_metodo_inferfaz decl_metodo_inferfaz\n"); }
    ;

declaracion_enum 
    : ENUM IDENTIFICADOR cuerpo_enum                                          { printf ("  decl_enum -> ENUM ID cuerpo_enum\n"); }
    | ENUM IDENTIFICADOR ':' tipo_escalar cuerpo_enum                         { printf ("  decl_enum -> ENUM ID tipo_escalar cuerpo_enum\n"); }
    | lista_modificador ENUM IDENTIFICADOR cuerpo_enum                        { printf ("  decl_enum -> list_mod ENUM ID cuerpo_enum\n"); }
    | lista_modificador ENUM IDENTIFICADOR ':' tipo_escalar cuerpo_enum       { printf ("  decl_enum -> list_mod ENUM ID tipo_escalar cuerpo_enum\n"); }
    ;

cuerpo_enum 
    : '{' lista_declaracion_miembro_enum '}'                                  { printf ("  cuerpo_enum -> { list_decl_miembro_enum }\n"); }
    ;

declaracion_miembro_enum 
    : IDENTIFICADOR                                                           { printf ("  decl_miembro_enum -> ID\n"); }
    | IDENTIFICADOR '=' expresion                                             { printf ("  decl_miembro_enum -> ID = exp\n"); }
    ;

lista_declaracion_miembro_enum 
    : declaracion_miembro_enum                                                { printf ("  list_decl_miembro_enum -> decl_miembro_enum\n"); }
    | lista_declaracion_miembro_enum ',' declaracion_miembro_enum             { printf ("  list_decl_miembro_enum -> list_decl_miembro_enum decl_miembro_enum\n"); }
    ;

/**********/
/* CLASES */
/**********/
declaracion_clase 
    : CLASS IDENTIFICADOR herencia cuerpo_clase                               { printf ("  decl_clase -> CLASS ID herencia cuerpo_clase\n"); }     
    | lista_modificador CLASS IDENTIFICADOR herencia cuerpo_clase             { printf ("  decl_clase -> list_mod CLASS ID herencia cuerpo_clase\n"); }
    ;

cuerpo_clase 
    : '{' lista_declaracion_elemento_clase '}'                                { printf ("  cuerpo_clase -> list_decl_elem_clase\n"); }
    ;

declaracion_elemento_clase 
    : declaracion_tipo                                                        { printf ("  decl_elem_clase -> decl_tipo\n"); }
    | declaracion_atributo                                                    { printf ("  decl_elem_clase -> declaracion_atributo\n"); }
    | declaracion_metodo                                                      { printf ("  decl_elem_clase -> decl_metodo\n"); }
    | declaracion_constructor                                                 { printf ("  decl_elem_clase -> decl_constructor\n"); }
    | declaracion_destructor                                                  { printf ("  decl_elem_clase -> decl_destructor\n"); }
    | declaracion_atributo                                                    { printf ("  decl_elem_clase -> decl_atributo\n"); }
    ;

lista_declaracion_elemento_clase 
    : declaracion_elemento_clase                                              { printf ("  list_decl_elem_clase -> decl_elem_clase\n"); }
    | lista_declaracion_elemento_clase declaracion_elemento_clase             { printf ("  list_decl_elem_clase -> list_decl_elem_clase decl_elem_clase\n"); }
    ;

declaracion_atributo 
    : declaracion_variable                                                    { printf ("  decl_atributo -> decl_var\n"); }
    | lista_modificador declaracion_variable                                  { printf ("  decl_atributo -> list_mod decl_var\n"); }
    ;

declaracion_metodo 
    : firma_funcion bloque_instrucciones                                      { printf ("  decl_metodo -> firma_funcion bloque_instrucciones\n"); }
    | lista_modificador firma_funcion bloque_instrucciones                    { printf ("  decl_metodo -> list_mod firma_funcion bloque_instrucciones\n"); }
    ;

declaracion_constructor 
    : cabecera_constructor bloque_instrucciones                               { printf ("  decl_constructor -> cabecera_constructor bloque_instrucciones\n"); }
    | lista_modificador cabecera_constructor bloque_instrucciones             { printf ("  decl_constructor -> list_mod cabecera_constructor bloque_instrucciones\n"); }
    ;

cabecera_constructor 
    : IDENTIFICADOR                                                           { printf ("  cabecera_constructor -> ID\n"); }
    | IDENTIFICADOR parametros inicializador_constructor                      { printf ("  cabecera_constructor -> ID parametros inicializador_constructor\n"); }
    | IDENTIFICADOR parametros                                                { printf ("  cabecera_constructor -> ID parametros\n"); }
    | IDENTIFICADOR inicializador_constructor                                 { printf ("  cabecera_constructor -> ID inicializador_constructor\n"); }
    ;

inicializador_constructor 
    : ':' BASE parametros                                                     { printf ("  inicializador_constructor -> BASE parametros\n"); }
    | ':' THIS parametros                                                     { printf ("  inicializador_constructor -> THIS parametros\n"); }
    ;

declaracion_destructor 
    : cabecera_destructor bloque_instrucciones                                { printf ("  decl_destructor -> cabecera_destructor bloque_instrucciones\n"); }
    | lista_modificador cabecera_destructor bloque_instrucciones              { printf ("  decl_destructor -> list_mod cabecera_destructor bloque_instrucciones\n"); }
    ;

cabecera_destructor 
    : '~' IDENTIFICADOR '(' ')'                                               { printf ("  cabecera_destructor -> ~ ID ()\n"); }
    ;

/*************/
/* FUNCIONES */
/*************/
declaracion_funcion 
    : firma_funcion bloque_instrucciones                                      { printf ("  decl_func -> firma_funcion bloque_instrucciones\n"); }
    ;

firma_funcion 
    : VOID IDENTIFICADOR parametros                                           { printf ("  firma_funcion -> VOID ID parametros\n"); }
    | tipo IDENTIFICADOR parametros                                           { printf ("  firma_funcion -> tipo ID parametros\n"); }
    | tipo lista_asterisco IDENTIFICADOR parametros                           { printf ("  firma_funcion -> tipo list_asterisco ID parametros\n"); }
    ;

lista_asterisco
    : '*'                                                                     { printf ("  list_asterisco -> *\n"); } 
    | lista_asterisco '*'                                                     { printf ("  list_asterisco -> list_asterisco *\n"); }
    ;

parametros 
    : '(' ')'                                                                 { printf ("  parametros -> ()\n"); }
    | '(' lista_argumentos ')'                                                { printf ("  parametros -> ( list_argumentos )\n"); }
    ;

argumentos 
    : nombre_tipo lista_variable                                              { printf ("  argumentos -> nom_tipo list_var\n"); }
    ;

lista_argumentos 
    : argumentos                                                              { printf ("  list_argumentos -> argumentos\n"); }
    | lista_argumentos ';' argumentos                                         { printf ("  list_argumentos -> list_argumentos argumentos\n"); }
    ;

nombre_tipo 
    : tipo                                                                    { printf ("  nom_tipo -> tipo\n"); }
    | tipo lista_asterisco                                                    { printf ("  nom_tipo -> tipo list_asterisco\n"); }
    ;

variable
    : IDENTIFICADOR                                                           { printf ("  var -> ID\n"); }
    | IDENTIFICADOR '=' expresion                                             { printf ("  var -> ID = exp\n"); }
    ;

lista_variable 
    : variable                                                                { printf ("  list_var -> var\n"); }
    | lista_variable ',' variable                                             { printf ("  list_var -> list_var var\n"); }
    ;

/*****************/
/* INSTRUCCIONES */
/*****************/
instruccion 
    : bloque_instrucciones                                                    { printf ("  instruccion -> bloque_instrucciones\n"); }
    | instruccion_expresion                                                   { printf ("  instruccion -> instruccion_exp\n"); }
    | instruccion_bifurcacion                                                 { printf ("  instruccion -> instruccion_bifurcacion\n"); }
    | instruccion_bucle                                                       { printf ("  instruccion -> instruccion_bucle\n"); }
    | instruccion_salto                                                       { printf ("  instruccion -> instruccion_salto\n"); }
    | instruccion_destino_salto                                               { printf ("  instruccion -> instruccion_destino_salto\n"); }
    | instruccion_retorno                                                     { printf ("  instruccion -> instruccion_retorno\n"); }
    | instruccion_lanzamiento_excepcion                                       { printf ("  instruccion -> instruccion_lanzamiento_excepcion\n"); }
    | instruccion_captura_excepcion                                           { printf ("  instruccion -> instruccion_captura_excepcion\n"); }
    | instruccion_vacia                                                       { printf ("  instruccion -> instruccion_vacia\n"); }
    ;

lista_instruccion 
    : instruccion                                                             { printf ("  list_instruccion -> instruccion\n"); }
    | lista_instruccion instruccion                                           { printf ("  list_instruccion -> list_instruccion instruccion\n"); }
    ;

bloque_instrucciones 
    : '{' '}'                                                                 { printf ("  bloque_instrucciones -> {}\n"); }
    | '{' lista_declaracion '}'                                               { printf ("  bloque_instrucciones -> { list_decl }\n"); }
    | '{' lista_instruccion '}'                                               { printf ("  bloque_instrucciones -> { list_instruccion }\n"); }
    | '{' lista_declaracion lista_instruccion '}'                             { printf ("  bloque_instrucciones -> { list_decl list_instruccion }\n"); }
    ;

instruccion_expresion 
    : expresion_funcional ';'                                                 { printf ("  instruccion_expresion -> exp_funcional\n"); }
    | asignacion ';'                                                          { printf ("  instruccion_expresion -> asignacion\n"); }
    ;

asignacion 
    : expresion_indexada operador_asignacion expresion                        { printf ("  asignacion -> exp_index op_asignacion exp\n"); }
    ;

operador_asignacion 
    : '='                                                                     { printf ("  op_asignacion -> =\n"); }
    | MULT_ASIG                                                               { printf ("  op_asignacion -> MULT_ASIG\n"); }
    | DIV_ASIG                                                                { printf ("  op_asignacion -> DIV_ASIG\n"); }
    | MOD_ASIG                                                                { printf ("  op_asignacion -> MOD_ASIG\n"); }
    | SUMA_ASIG                                                               { printf ("  op_asignacion -> SUMA_ASIG\n"); }
    | RESTA_ASIG                                                              { printf ("  op_asignacion -> RESTA_ASIG\n"); }
    | DESPI_ASIG                                                              { printf ("  op_asignacion -> DESPI_ASIG\n"); }
    | DESPD_ASIG                                                              { printf ("  op_asignacion -> DESPD_ASIG\n"); }
    | AND_ASIG                                                                { printf ("  op_asignacion -> AND_ASIG\n"); }
    | XOR_ASIG                                                                { printf ("  op_asignacion -> XOR_ASIG\n"); }
    | OR_ASIG                                                                 { printf ("  op_asignacion -> OR_ASIG\n"); }
    ;

instruccion_bifurcacion 
    : IF '(' expresion ')' instruccion                                        { printf ("  instruccion_bifurcacion -> IF ( exp ) instruccion\n"); }
    | IF '(' expresion ')' instruccion ELSE instruccion                       { printf ("  instruccion_bifurcacion -> IF ( exp ) instruccion ELSE instruccion\n"); }
    | SWITCH '(' expresion ')' '{' lista_instruccion_caso '}'                 { printf ("  instruccion_bifurcacion -> SWITCH ( exp ) { list_instruccion_caso }\n"); }
    ;

instruccion_caso 
    : CASE expresion ':' instruccion                                          { printf ("  instruccion_caso -> CASE exp instruccion\n"); }
    | DEFAULT ':' instruccion                                                 { printf ("  instruccion_caso -> DEFAULT instruccion\n"); }
    ;

lista_instruccion_caso
    : instruccion_caso                                                        { printf ("  list_instruccion_caso -> instruccion_caso\n"); }
    | lista_instruccion_caso instruccion_caso                                 { printf ("  list_instruccion_caso -> list_instruccion_caso instruccion_caso\n"); }
    ;

instruccion_bucle 
    : WHILE '(' expresion ')' instruccion                                     { printf ("  instruccion_bucle -> WHILE ( exp ) instruccion\n"); } 
    | DO instruccion WHILE '(' expresion ')' ';'                              { printf ("  instruccion_bucle -> DO instruccion WHILE ( exp )\n"); } 
    | FOR '(' ';' expresion ';' lista_expresion ')' instruccion               { printf ("  instruccion_bucle -> FOR ( exp list_exp ) instruccion\n"); } 
    | FOR '(' lista_asignacion ';' expresion ';' lista_expresion ')' instruccion { printf ("  instruccion_bucle -> FOR ( list_asignacion exp list_exp ) instruccion\n"); } 
    ;

lista_asignacion 
    : asignacion                                                              { printf ("  list_asignacion -> asignacion\n"); } 
    | lista_asignacion asignacion                                             { printf ("  list_asignacion -> list_asignacion asignacion\n"); } 
    ;

instruccion_salto 
    : GOTO IDENTIFICADOR ';'                                                  { printf ("  instruccion_salto -> GOTO ID\n"); } 
    | CONTINUE ';'                                                            { printf ("  instruccion_salto -> CONTINUE\n"); } 
    | BREAK ';'                                                               { printf ("  instruccion_salto -> BREAK\n"); } 
    ;

instruccion_destino_salto 
    : IDENTIFICADOR ':' instruccion ';'                                       { printf ("  instruccion_destino_salto -> ID instruccion\n"); } 
    ;

instruccion_retorno 
    : RETURN ';'                                                              { printf ("  instruccion_retorno -> RETURN\n"); } 
    | RETURN expresion ';'                                                    { printf ("  instruccion_retorno -> RETURN exp\n"); } 
    ;

instruccion_lanzamiento_excepcion 
    : THROW expresion ';'                                                     { printf ("  instruccion_lanzamiento_excepcion -> THROW exp\n"); } 
    ;

instruccion_captura_excepcion 
    : TRY bloque_instrucciones clausulas-catch                                { printf ("  instruccion_captura_excepcion -> TRY  bloque_instrucciones clausulas-catch\n"); } 
    | TRY bloque_instrucciones clausula_finally                               { printf ("  instruccion_captura_excepcion -> TRY  bloque_instrucciones clausula_finally\n"); } 
    | TRY bloque_instrucciones clausulas-catch clausula_finally               { printf ("  instruccion_captura_excepcion -> TRY  bloque_instrucciones clausulas-catch clausula_finally\n"); } 
    ;

clausulas-catch 
    : lista_clausula_catch_especifica                                         { printf ("  clausulas-catch -> list_clausula_catch_especifica\n"); } 
    | clausula_catch_general                                                  { printf ("  clausulas-catch -> clausula_catch_general\n"); } 
    | lista_clausula_catch_especifica clausula_catch_general                  { printf ("  clausulas-catch -> list_clausula_catch_especifica clausula_catch_general\n"); } 
    ;

clausula_catch_especifica 
    : CATCH '(' nombre_tipo ')' bloque_instrucciones                          { printf ("  clausula_catch_especifica -> CATCH '(' nom_tipo ')' bloque_instrucciones\n"); } 
    ;

lista_clausula_catch_especifica 
    : lista_clausula_catch_especifica clausula_catch_especifica               { printf ("  list_clausula_catch_especifica -> list_clausula_catch_especifica clausula_catch_especifica\n"); } 
    ;

clausula_catch_general 
    : CATCH bloque_instrucciones                                              { printf ("  clausula_catch_general -> CATCH bloque_instrucciones\n"); } 
    ;

clausula_finally 
    : FINALLY bloque_instrucciones                                            { printf ("  clausula_finally -> FINALLY bloque_instrucciones\n"); } 
    ;

instruccion_retorno 
    : RETURN ';'                                                              { printf ("  instruccion_retorno -> RETURN\n"); } 
    | RETURN expresion ';'                                                    { printf ("  instruccion_retorno -> RETURN exp\n"); } 
    ;

instruccion_vacia
    : ';'                                                                     { printf ("  instruccion_vacia -> ;\n"); } 
    ;
/***************/
/* EXPRESIONES */
/***************/

expresion
    : expresion_constante                                                     { printf ("  exp -> exp_cons\n"); } 
    | expresion_parentesis                                                    { printf ("  exp -> exp_parentesis\n"); } 
    | expresion_funcional                                                     { printf ("  exp -> exp_funcional\n"); } 
    | expresion_creacion_objeto                                               { printf ("  exp -> exp_creacion_objeto\n"); } 
    | expresion_indexada                                                      { printf ("  exp -> exp_index\n"); } 
    | expresion_postfija                                                      { printf ("  exp -> exp_posfija\n"); } 
    | expresion_prefija                                                       { printf ("  exp -> exp_prefija\n"); } 
    | expresion_cast                                                          { printf ("  exp -> exp_cast\n"); } 
    ;

expresion_constante 
    : ENTERO                                                                  { printf ("  expresion_constante -> ENTERO\n"); } 
    | REAL                                                                    { printf ("  expresion_constante -> REAL\n"); } 
    | CADENA                                                                  { printf ("  expresion_constante -> CADENA\n"); } 
    | CARACTER                                                                { printf ("  expresion_constante -> CARACTER\n"); } 
    | BOOLEANO                                                                { printf ("  expresion_constante -> BOOLEANO\n"); } 
    ;

expresion_parentesis 
    : '(' expresion ')'                                                       { printf ("  expresion_parentesis -> ( exp )\n"); } 
    ;

expresion_funcional 
    : identificador_anidado '(' ')'                                           { printf ("  expresion_funcional -> id_anid ()\n"); } 
    | identificador_anidado '(' lista_expresion ')'                           { printf ("  expresion_funcional -> id_anid ( list_exp )\n"); } 
    ;

expresion_creacion_objeto 
    : NEW identificador_anidado '(' ')'                                       { printf ("  expresion_creacion_objeto -> NEW id_anid ()\n"); } 
    | NEW identificador_anidado '(' lista_expresion ')'                       { printf ("  expresion_creacion_objeto -> NEW id_anid ( list_exp )\n"); } 
    ;

expresion_indexada 
    : identificador_anidado                                                   { printf ("  expresion_indexada -> id_anid\n"); } 
    | expresion_indexada '[' expresion ']'                                    { printf ("  expresion_indexada -> exp_index [ exp ]\n"); } 
    | expresion_indexada PTR_ACCESO identificador_anidado                           { printf ("  expresion_indexada -> exp_index -> id_anid\n"); } 
    ;

expresion_postfija 
    : expresion_constante                                                     { printf ("  expresion_postfija -> exp_cons\n"); } 
    | expresion_parentesis                                                    { printf ("  expresion_postfija -> exp_parentesis\n"); } 
    | expresion_funcional                                                     { printf ("  expresion_postfija -> exp_funcional\n"); } 
    | expresion_creacion_objeto                                               { printf ("  expresion_postfija -> exp_creacion_objeto\n"); } 
    | expresion_indexada                                                      { printf ("  expresion_postfija -> exp_index\n"); } 
    | expresion_postfija INC                                                  { printf ("  expresion_postfija -> exp_postfija INC\n"); } 
    | expresion_postfija DEC                                                  { printf ("  expresion_postfija -> exp_posfija DEC\n"); } 
    ;

expresion_prefija 
    : expresion_postfija                                                      { printf ("  expresion_prefija -> exp_posfija\n"); } 
    | SIZEOF expresion_prefija                                                { printf ("  expresion_prefija -> SIZEOF exp_prefija\n"); } 
    | SIZEOF '(' nombre_tipo ')'                                              { printf ("  expresion_prefija -> SIZEOF ( nom_tipo )\n"); } 
    | operador_prefijo expresion_cast                                         { printf ("  expresion_prefija -> op_prefijo exp_cast\n"); } 
    ;

operador_prefijo 
    : INC                                                                     { printf ("  op_prefijo -> INC\n"); } 
    | DEC                                                                     { printf ("  op_prefijo -> DEC\n"); } 
    | '&'                                                                     { printf ("  op_prefijo -> &\n"); } 
    | '*'                                                                     { printf ("  op_prefijo -> *\n"); } 
    | '+'                                                                     { printf ("  op_prefijo -> +\n"); }  
    | '-'                                                                     { printf ("  op_prefijo -> -\n"); } 
    | '~'                                                                     { printf ("  op_prefijo -> ~\n"); } 
    | '!'                                                                     { printf ("  op_prefijo -> !\n"); } 
    ;

expresion_cast 
    : expresion_prefija                                                       { printf ("  expresion_cast -> exp_prefija\n"); } 
    | '(' nombre_tipo ')' expresion_prefija                                   { printf ("  expresion_cast -> ( nom_tipo ) exp_prefija\n"); } 
    ;

%%

void yyerror(char *s) {
  fflush(stdout);
  printf("Error linea %d, %s\n", linea,s);
  }

int yywrap() {
  return 1;
  }

int main(int argc, char *argv[]) {

  yydebug = 0;

  if (argc < 2) {
    printf("Uso: ./c-dull NombreArchivo\n");
    }
  else {
    yyin = fopen(argv[1],"r");
    yyparse();
    }
  }
