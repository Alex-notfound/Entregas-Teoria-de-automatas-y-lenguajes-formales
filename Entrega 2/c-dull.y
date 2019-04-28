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
declaracion_espacio_nombres ::= ’namespace’ identificador_anidado bloque_espacio_nombres
identificador_anidado ::= [ IDENTIFICADOR ’.’ ]* IDENTIFICADOR
bloque_espacio_nombres ::= ’{’ [ directiva_uso ]* [ declaracion ]+ ’}’

/*************/
/* VARIABLES */
/*************/
declaracion_variable ::= tipo ( nombre )+ ’;’
tipo ::= ’<’ nombre_tipo_o_espacio_nombres ’>’
| tipo_escalar
tipo_escalar ::= [ signo ]? [ longitud ]? tipo_basico
longitud ::= SHORT | LONG
signo ::= SIGNED | UNSIGNED
tipo_basico ::= CHAR | INT | FLOAT | DOUBLE | BOOLEAN
nombre ::= dato [ ’=’ valor ]?
dato ::= [ ’*’ ]* dato_indexado
dato_indexado ::= IDENTIFICADOR [ ’[’ ( expresion )* ’]’ ]*
valor ::= expresion
| ’{’ ( valor )+ ’}’

/*********/
/* TIPOS */
/*********/
declaracion_tipo ::= nombramiento_tipo
| declaracion_struct_union
| declaracion_interfaz
| declaracion_enum
| declaracion_clase
nombramiento_tipo ::= ’typedef’ tipo ID ’;’
declaracion_struct_union ::= [ modificador ]* struct_union [ IDENTIFICADOR ]?
’{’ [ declaracion_campo ]+ ’}’
modificador ::= ’new’ | ’public’ | ’protected’ | ’internal’ | ’private’ | ’static’
| ’virtual’ | ’sealed’ | ’override’ | ’abstract’ | ’extern’
struct_union ::= ’struct’ | ’union’
declaracion_campo ::= tipo ( nombre )+ ’;’
| declaracion_struct_union ( nombre )+ ’;’
declaracion_interfaz ::= [ modificador ]* ’interface’ IDENTIFICADOR herencia cuerpo_interfaz
herencia ::= [ ’:’ ( nombre_tipo_o_espacio_nombres )+ ]?
cuerpo_interfaz ::= ’{’ [ declaracion_metodo_interfaz ]* ’}’
declaracion_metodo_interfaz ::= [ ’new’ ]? firma_funcion ’;’
declaracion_enum ::= [ modificador ]* ’enum’ IDENTIFICADOR [ ’:’ tipo_escalar ]? cuerpo_enum
cuerpo_enum ::= ’{’ ( declaracion_miembro_enum )+ ’}’
declaracion_miembro_enum ::= IDENTIFICADOR [ ’=’ expresion ]?


/**********/
/* CLASES */
/**********/
declaracion_clase ::= [ modificador ]* ’class’ IDENTIFICADOR herencia cuerpo_clase
cuerpo clase ::= ’{’ [ declaracion_elemento_clase ]+ ’}’
declaracion_elemento_clase ::= declaracion_tipo
| declaracion_atributo
| declaracion_metodo
| declaracion_constructor
| declaracion_destructor
| declaracion_atributo
declaracion_atributo ::= [ modificador ]* declaracion_variable
declaracion_metodo ::= [ modificador ]* firma_funcion bloque_instrucciones
declaracion_constructor ::= [ modificador ]* cabecera_constructor bloque_instrucciones
cabecera_constructor ::= IDENTIFICADOR [ parametros ]? [ inicializador_constructor ]?
inicializador_constructor ::= ’:’ BASE parametros
| ’:’ THIS parametros
declaracion_destructor ::= [ modificador ]* cabecera_destructor bloque_instrucciones
cabecera_destructor ::= ’~’ IDENTIFICADOR ’(’ ’)’

/*************/
/* FUNCIONES */
/*************/
declaracion_funcion ::= firma_funcion bloque_instrucciones
firma_funcion ::= VOID IDENTIFICADOR parametros
| tipo [ ’*’ ]* IDENTIFICADOR parametros
parametros ::= ’(’ [ argumentos ’;’ ]* [ argumentos ]? ’)’
argumentos ::= nombre_tipo ( variable )+
nombre_tipo ::= tipo [ ’*’ ]*
variable ::= IDENTIFICADOR [ ’=’ expresion ]?


/*****************/
/* INSTRUCCIONES */
/*****************/
instruccion ::= bloque_instrucciones
| instruccion_expresion
| instruccion_bifurcacion
| instruccion_bucle
| instruccion_salto
| instruccion_destino_salto
| instruccion_retorno
| instruccion_lanzamiento_excepcion
| instruccion_captura_excepcion
| instruccion_vacia
bloque_instrucciones ::= ’{’ [ declaracion ]* [ instruccion ]* ’}’
instruccion_expresion ::= expresion_funcional ’;’ | asignacion ’;’
asignacion ::= expresion_indexada operador_asignacion expresion
operador_asignacion ::= ’=’ | ’*=’ | ’/=’ | ’%=’ | ’+=’ | ’-=’ | ’<<=’ | ’>>=’ | ’&=’ | ’^=’ | ’|=’
instruccion_bifurcacion ::= ’if’ ’(’ expresion ’)’ instruccion [ ’else’ instruccion ]?
| ’switch’ ’(’ expresion ’)’ ’{’ [ instruccion_caso ]+ ’}’
instruccion_caso ::= ’case’ expresion ’:’ instruccion
| ’default’ ’:’ instruccion
instruccion_bucle ::= ’while’ ’(’ expresion ’)’ instruccion
| ’do’ instruccion ’while’ ’( expresion ’)’ ’;’
| ’for’ ’(’ ( asignacion )* ’;’ expresion ’;’ ( expresion )+ ’)’ instruccion
instruccion_salto ::= ’goto’ IDENTIFICADOR ’;’ | ’continue’ ’;’ | ’break’ ’;’
instruccion_destino_salto ::= IDENTIFICADOR ’:’ instruccion ’;’
instruccion_retorno ::= ’return’ [ expresion ]? ’;’
instruccion_lanzamiento_excepcion ::= ’throw’ expresion ’;’
instruccion_captura_excepcion ::= ’try’ bloque_instrucciones clausulas-catch
| ’try’ bloque_instrucciones clausula_finally
| ’try’ bloque_instrucciones clausulas-catch clausula_finally
clausulas-catch ::= [ clausula_catch_especifica ]+
| clausula_catch_general
| [ clausula_catch_especifica ]+ clausula_catch_general
clausula_catch_especifica ::= ’catch’ ’(’ nombre_tipo ’)’ bloque_instrucciones
clausula_catch_general ::= ’catch’ bloque_instrucciones
clausula-finally ::= ’finally’ bloque_instrucciones
instruccion_retorno ::= ’return’ [ expresion ]? ’;’

/***************/
/* EXPRESIONES */
/***************/

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
