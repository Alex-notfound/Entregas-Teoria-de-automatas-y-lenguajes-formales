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
    : lista_declaraciones                                                     { printf ("  modulo -> list_decl\n"); }
    | lista_directivas_uso lista_declaraciones                                { printf ("  modulo -> lista_dir_uso list_decl\n"); }
    ;

lista_declaraciones
    : declaracion                                                             { printf ("  list_decl -> decl\n"); }
    | lista_declaraciones declaracion                                         { printf ("  list_decl -> list_decl decl\n"); }
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
    | '{' lista_directiva_uso lista_declaracion '}'                           { printf ("  bloq_esp_noms -> { lista_dir_uso list_decl }\n"); }
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
    : nombramiento_tipo
    | declaracion_struct_union
    | declaracion_interfaz
    | declaracion_enum
    | declaracion_clase
    ;

nombramiento_tipo 
    : TYPEDEF tipo ID ';'
    ;

declaracion_struct_union 
    : struct_union '{' lista_declaracion_campo '}'
    | lista_modificador struct_union '{' lista_declaracion_campo '}'
    | lista_modificador struct_union IDENTIFICADOR '{' lista_declaracion_campo '}'
    ;

modificador 
    : NEW 
    | PUBLIC 
    | PROTECTED 
    | INTERNAL 
    | PRIVATE 
    | STATIC
    | VIRTUAL 
    | SEALED 
    | OVERRIDE 
    | ABSTRACT 
    | EXTERN
    ;

lista_modificador
    : modificador 
    | lista_modificador modificador
    ;

struct_union 
    : STRUCT 
    | UNION
    ;

declaracion_campo 
    : tipo lista_nombre ';' 
    | declaracion_struct_union lista_nombre ';'
    ;

lista_nombre 
    : nombre 
    | lista_nombre ',' nombre
    ;

lista_declaracion_campo
    : declaracion_campo 
    | lista_declaracion_campo declaracion_campo
    ;

declaracion_interfaz 
    : INTERFACE IDENTIFICADOR herencia cuerpo_interfaz
    | lista_modificador INTERFACE IDENTIFICADOR herencia cuerpo_interfaz
    ;

herencia 
    : ':' nombre_tipo_o_espacio_nombres
    ;

lista_nombre_tipo_o_espacio_nombres 
    : nombre_tipo_o_espacio_nombres 
    | lista_nombre_tipo_o_espacio_nombres ',' nombre_tipo_o_espacio_nombres
    ;

cuerpo_interfaz 
    : '{' '}' 
    | '{' lista_declaracion_metodo_interfaz '}'
    ;

declaracion_metodo_interfaz 
    : firma_funcion ';' 
    | NEW firma_funcion ';'
    ;

lista_declaracion_metodo_interfaz 
    : declaracion_metodo_interfaz 
    | lista_declaracion_metodo_interfaz declaracion_metodo_interfaz
    ;

declaracion_enum 
    : ENUM IDENTIFICADOR cuerpo_enum
    | ENUM IDENTIFICADOR ':' tipo_escalar cuerpo_enum
    | lista_modificador ENUM IDENTIFICADOR cuerpo_enum
    | lista_modificador ENUM IDENTIFICADOR ':' tipo_escalar cuerpo_enum
    ;

cuerpo_enum 
    : '{' lista_declaracion_miembro_enum '}'
    ;

declaracion_miembro_enum 
    : IDENTIFICADOR 
    | IDENTIFICADOR '=' expresion
    ;

lista_declaracion_miembro_enum 
    : declaracion_miembro_enum 
    | lista_declaracion_miembro_enum ',' declaracion_miembro_enum
    ;

/**********/
/* CLASES */
/**********/
declaracion_clase 
    : CLASS IDENTIFICADOR herencia cuerpo_clase 
    | lista_modificador CLASS IDENTIFICADOR herencia cuerpo_clase
    ;

cuerpo clase 
    : '{' lista_declaracion_elemento_clase '}'
    ;

declaracion_elemento_clase 
    : declaracion_tipo
    | declaracion_atributo
    | declaracion_metodo
    | declaracion_constructor
    | declaracion_destructor
    | declaracion_atributo
    ;

lista_declaracion_elemento_clase 
    : declaracion_elemento_clase 
    | lista_declaracion_elemento_clase declaracion_elemento_clase
    ;

declaracion_atributo 
    : declaracion_variable 
    | lista_modificador declaracion_variable
    ;

declaracion_metodo 
    : firma_funcion bloque_instrucciones 
    | lista_modificador firma_funcion bloque_instrucciones
    ;

declaracion_constructor 
    : cabecera_constructor bloque_instrucciones 
    | lista_modificador cabecera_constructor bloque_instrucciones
    ;

cabecera_constructor 
    : IDENTIFICADOR 
    | IDENTIFICADOR parametros inicializador_constructor 
    | IDENTIFICADOR parametros 
    | IDENTIFICADOR inicializador_constructor
    ;

inicializador_constructor 
    : ':' BASE parametros 
    | ':' THIS parametros
    ;

declaracion_destructor 
    : cabecera_destructor bloque_instrucciones 
    | lista_modificador cabecera_destructor bloque_instrucciones
    ;

cabecera_destructor 
    : '~' IDENTIFICADOR '(' ')'
    ;

/*************/
/* FUNCIONES */
/*************/
declaracion_funcion 
    : firma_funcion bloque_instrucciones
    ;

firma_funcion 
    : VOID IDENTIFICADOR parametros 
    | tipo IDENTIFICADOR parametros 
    | tipo lista_asterisco IDENTIFICADOR parametros
    ;

lista_asterisco
    : '*' 
    | lista_asterisco '*'
    ;

parametros 
    : '(' ')' 
    | '(' lista_argumentos ')'
    ;

argumentos 
    : nombre_tipo lista_variable
    ;

lista_argumentos 
    : argumentos 
    | lista_argumentos ';' argumentos
    ;

nombre_tipo 
    : tipo 
    | tipo lista_asterisco
    ;

variable
    : IDENTIFICADOR 
    | IDENTIFICADOR '=' expresion
    ;

lista_variable 
    : variable 
    | lista_variable ',' variable
    ;

/*****************/
/* INSTRUCCIONES */
/*****************/
instruccion 
    : bloque_instrucciones
    | instruccion_expresion
    | instruccion_bifurcacion
    | instruccion_bucle
    | instruccion_salto
    | instruccion_destino_salto
    | instruccion_retorno
    | instruccion_lanzamiento_excepcion
    | instruccion_captura_excepcion
    | instruccion_vacia
    ;

lista_instruccion 
    : instruccion 
    | lista_instruccion instruccion
    ;

bloque_instrucciones 
    : '{' '}' 
    | '{' lista_declaracion '}' 
    | '{' lista_instruccion '}' 
    | '{' lista_declaracion lista_instruccion '}'
    ;

instruccion_expresion 
    : expresion_funcional ';' 
    | asignacion ';'
    ;

asignacion 
    : expresion_indexada operador_asignacion expresion
    ;

operador_asignacion 
    : '=' 
    | '*=' 
    | '/=' 
    | '%=' 
    | '+=' 
    | '-=' 
    | '<<=' 
    | '>>=' 
    | '&=' 
    | '^=' 
    | '|='
    ;

instruccion_bifurcacion 
    : IF '(' expresion ')' instruccion 
    | IF '(' expresion ')' instruccion ELSE instruccion 
    | SWITCH '(' expresion ')' '{' lista_instruccion_caso '}'
    ;

instruccion_caso 
    : CASE expresion ':' instruccion 
    | DEFAULT ':' instruccion
    ;

lista_instruccion_caso
    : instruccion_caso 
    | lista_instruccion_caso instruccion_caso
    ;

instruccion_bucle 
    : WHILE '(' expresion ')' instruccion 
    | DO instruccion WHILE '(' expresion ')' ';'
    | FOR '(' ';' expresion ';' lista_expresion ')' instruccion
    | FOR '(' lista_asignacion ';' expresion ';' lista_expresion ')' instruccion
    ;

lista_asignacion 
    : asignacion 
    | lista_asignacion asignacion
    ;

instruccion_salto 
    : GOTO IDENTIFICADOR ';' 
    | CONTINUE ';' 
    | BREAK ';'
    ;

instruccion_destino_salto 
    : IDENTIFICADOR ':' instruccion ';'
    ;

instruccion_retorno 
    : RETURN ';' 
    | RETURN expresion ';'
    ;

instruccion_lanzamiento_excepcion 
    : THROW expresion ';'
    ;

instruccion_captura_excepcion 
    : TRY bloque_instrucciones clausulas-catch
    | TRY bloque_instrucciones clausula_finally
    | TRY bloque_instrucciones clausulas-catch clausula_finally
    ;

clausulas-catch 
    : lista_clausula_catch_especifica 
    | clausula_catch_general 
    | lista_clausula_catch_especifica clausula_catch_general
    ;

clausula_catch_especifica 
    : CATCH '(' nombre_tipo ')' bloque_instrucciones
    ;

lista_clausula_catch_especifica 
    : lista_clausula_catch_especifica clausula_catch_especifica
    ;

clausula_catch_general 
    : CATCH bloque_instrucciones
    ;

clausula-finally 
    : FINALLY bloque_instrucciones
    ;

instruccion_retorno 
    : RETURN ';' 
    | RETURN expresion ';'
    ;

/***************/
/* EXPRESIONES */
/***************/
expresion_constante 
    : ENTERO 
    | REAL 
    | CADENA 
    | CARACTER 
    | BOOLEANO
    ;

expresion_parentesis 
    : '(' expresion ')'
    ;

expresion_funcional 
    : identificador_anidado '(' ')' 
    | identificador_anidado '(' lista_expresion ')'
    ;

expresion_creacion_objeto 
    : NEW identificador_anidado '(' ')' 
    | NEW identificador_anidado '(' lista_expresion ')'
    ;

expresion_indexada 
    : identificador_anidado
    | expresion_indexada '[' expresion ']'
    | expresion_indexada '->' identificador_anidado
    ;

expresion_postfija 
    : expresion_constante
    | expresion_parentesis
    | expresion_funcional
    | expresion_creacion_objeto
    | expresion_indexada
    | expresion_postfija INC
    | expresion_postfija DEC
    ;

expresion_prefija 
    : expresion_postfija
    | SIZEOF expresion_prefija
    | SIZEOF '(' nombre_tipo ')'
    | operador_prefijo expresion_cast
    ;

operador_prefijo 
    : INC 
    | DEC 
    | '&' 
    | '*' 
    | '+' 
    | '-' 
    | '~' 
    | '!'
    ;

expresion_cast 
    : expresion_prefija 
    | '(' nombre_tipo ')' expresion_prefija
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
