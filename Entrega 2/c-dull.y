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
modulo : mas_directiva_uso mas_declaracion | mas_declaracion {printf("\tlista_dir_uso -> dir_uso\n");} 
;
mas_directiva_uso : directiva_uso | mas_directiva_uso directiva_uso 
;
mas_declaracion : declaracion | mas_declaracion declaracion 
;
declaracion : declaracion_espacio_nombres | declaracion_variables | declaracion_tipo | declaracion_funcion 
;
directiva_uso : USING IDENTIFICADOR '=' nombre_tipo_o_espacio_nombres ';' | USING nombre_tipo_o_espacio_nombres ';' 
{printf("\tdir_uso -> USING ID = nom_tipo_o_esp_noms\n");} ;

nombre_tipo_o_espacio_nombres : mas_identificador_con_tipos 
{printf("\tnom_tipo_o_esp_noms -> id_tipos\n");} ;

identificador_con_tipos : IDENTIFICADOR | IDENTIFICADOR '(' nombre_tipo_o_espacio_nombres ')'
{printf("\tid_tipos -> ID\n");} ;

/*******************/
/* ESPACIO NOMBRES */
/*******************/
declaracion_espacio_nombres : 'namespace' identificador_anidado bloque_espacio_nombres
;
identificador_anidado : IDENTIFICADOR | identificador_anidado '.' IDENTIFICADOR 
;
bloque_espacio_nombres : '{' mas_declaracion '}' | '{' mas_directiva_uso mas_declaracion '}'
;

/*************/
/* VARIABLES */
/*************/
declaracion_variable : tipo mas_nombre ';'
;
tipo : '<' nombre_tipo_o_espacio_nombres '>' | tipo_escalar
;
tipo_escalar : tipo_basico | longitud tipo_basico | signo tipo_basico | signo longitud tipo_basico
;
longitud : SHORT {printf("\t" $$ " -> " $1 "\n");}| LONG {printf("\t" $$ " -> " $2 "\n");}
;
signo : SIGNED {printf("\t" $$ " -> " $1 "\n");}| UNSIGNED {printf("\t" $$ " -> " $2 "\n");}
;
tipo_basico : CHAR {printf("\t" $$ " -> " $1 "\n");} | INT {printf("\t" $$ " -> " $2 "\n");} 
            | FLOAT {printf("\t" $$ " -> " $3 "\n");}| DOUBLE {printf("\t" $$ " -> " $4 "\n");}
            | BOOLEAN{printf("\t" $$ " -> " $5 "\n");}
;
nombre : dato | dato '=' valor
;
mas_nombre : nombre | mas_nombre nombre
;
//Posiblemente mal:
dato : dato_indexado | '*' dato
;
dato_indexado : IDENTIFICADOR | IDENTIFICADOR '[' ']' | IDENTIFICADOR '[' mas_expresion ']' 
;
mas_expresion : expresion | mas_expresion ',' expresion
;
valor : expresion | '{' mas_valor '}'
;
mas_valor : valor | mas_valor ',' valor

/*********/
/* TIPOS */
/*********/

declaracion_tipo : nombramiento_tipo
                  | declaracion_struct_union
                  | declaracion_interfaz
                  | declaracion_enum
                  | declaracion_clase
;
nombramiento_tipo : 'typedef' tipo ID ';'
;
declaracion_struct_union : struct_union '{' mas_declaracion_campo '}'
                          | mas_modificador struct_union '{' mas_declaracion_campo '}'
                          | mas_modificador struct_union IDENTIFICADOR '{' mas_declaracion_campo '}'
;
modificador : 'new' | 'public' | 'protected' | 'internal' | 'private' | 'static'
| 'virtual' | 'sealed' | 'override' | 'abstract' | 'extern'
;
mas_modificador: modificador | mas_modificador modificador
;
struct_union : 'struct' | 'union'
;
declaracion_campo : tipo mas_nombre ';' | declaracion_struct_union mas_nombre ';'
;
mas_nombre : nombre | mas_nombre ',' nombre
;
mas_declaracion_campo: declaracion_campo | mas_declaracion_campo declaracion_campo
;
declaracion_interfaz : 'interface' IDENTIFICADOR herencia cuerpo_interfaz
                      | mas_modificador 'interface' IDENTIFICADOR herencia cuerpo_interfaz
;
herencia : ':' nombre_tipo_o_espacio_nombres
;
mas_nombre_tipo_o_espacio_nombres : nombre_tipo_o_espacio_nombres | mas_nombre_tipo_o_espacio_nombres ',' nombre_tipo_o_espacio_nombres
;
cuerpo_interfaz : '{' '}' | '{' mas_declaracion_metodo_interfaz '}'
;
declaracion_metodo_interfaz : firma_funcion ';' | 'new' firma_funcion ';'
;
mas_declaracion_metodo_interfaz : declaracion_metodo_interfaz | mas_declaracion_metodo_interfaz declaracion_metodo_interfaz
;
declaracion_enum : 'enum' IDENTIFICADOR cuerpo_enum
                  | 'enum' IDENTIFICADOR ':' tipo_escalar cuerpo_enum
                  | mas_modificador 'enum' IDENTIFICADOR cuerpo_enum
                  | mas_modificador 'enum' IDENTIFICADOR ':' tipo_escalar cuerpo_enum
;
cuerpo_enum : '{' mas_declaracion_miembro_enum '}'
;
declaracion_miembro_enum : IDENTIFICADOR | IDENTIFICADOR '=' expresion
;
mas_declaracion_miembro_enum : declaracion_miembro_enum | mas_declaracion_miembro_enum ',' declaracion_miembro_enum

/**********/
/* CLASES */
/**********/
declaracion_clase : 'class' IDENTIFICADOR herencia cuerpo_clase | mas_modificador 'class' IDENTIFICADOR herencia cuerpo_clase
;
cuerpo clase : '{' mas_declaracion_elemento_clase '}'
;
declaracion_elemento_clase : declaracion_tipo
| declaracion_atributo
| declaracion_metodo
| declaracion_constructor
| declaracion_destructor
| declaracion_atributo
;
mas_declaracion_elemento_clase : declaracion_elemento_clase | mas_declaracion_elemento_clase declaracion_elemento_clase
;
declaracion_atributo : declaracion_variable | mas_modificador declaracion_variable
;
declaracion_metodo : firma_funcion bloque_instrucciones | mas_modificador firma_funcion bloque_instrucciones
;
declaracion_constructor : cabecera_constructor bloque_instrucciones | mas_modificador cabecera_constructor bloque_instrucciones
;
cabecera_constructor : IDENTIFICADOR | IDENTIFICADOR parametros inicializador_constructor 
                      | IDENTIFICADOR parametros | IDENTIFICADOR inicializador_constructor
;
inicializador_constructor : ':' BASE parametros | ':' THIS parametros
;
declaracion_destructor : cabecera_destructor bloque_instrucciones | mas_modificador cabecera_destructor bloque_instrucciones
;
cabecera_destructor : '~' IDENTIFICADOR '(' ')'
;

/*************/
/* FUNCIONES */
/*************/
declaracion_funcion : firma_funcion bloque_instrucciones
;
firma_funcion : VOID IDENTIFICADOR parametros | tipo IDENTIFICADOR parametros | tipo mas_asterisco IDENTIFICADOR parametros
;
mas_asterisco: '*' | mas_asterisco '*'
;
parametros : '(' ')' | '(' mas_argumentos ')'
;
argumentos : nombre_tipo mas_variable
;
mas_argumentos : argumentos | mas_argumentos ';' argumentos
;
nombre_tipo : tipo | tipo mas_asterisco
;
variable : IDENTIFICADOR | IDENTIFICADOR '=' expresion
;
mas_variable : variable | mas_variable ',' variable
;

/*****************/
/* INSTRUCCIONES */
/*****************/
instruccion : bloque_instrucciones
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
mas_instruccion : instruccion | mas_instruccion instruccion
;
bloque_instrucciones : '{' '}' | '{' mas_declaracion '}' | '{' mas_instruccion '}' | '{' mas_declaracion mas_instruccion '}'
;
instruccion_expresion : expresion_funcional ';' | asignacion ';'
;
asignacion : expresion_indexada operador_asignacion expresion
;
operador_asignacion : '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
;
instruccion_bifurcacion : 'if' '(' expresion ')' instruccion | 'if' '(' expresion ')' instruccion 'else' instruccion 
                        | 'switch' '(' expresion ')' '{' mas_instruccion_caso '}'
;
instruccion_caso : 'case' expresion ':' instruccion | 'default' ':' instruccion
;
mas_instruccion_caso: instruccion_caso | mas_instruccion_caso instruccion_caso
;
instruccion_bucle : 'while' '(' expresion ')' instruccion | 'do' instruccion 'while' '(' expresion ')' ';'
                  | 'for' '(' ';' expresion ';' mas_expresion ')' instruccion
                  | 'for' '(' mas_asignacion ';' expresion ';' mas_expresion ')' instruccion
;
mas_asignacion : asignacion | mas_asignacion asignacion
;
instruccion_salto : 'goto' IDENTIFICADOR ';' | 'continue' ';' | 'break' ';'
;
instruccion_destino_salto : IDENTIFICADOR ':' instruccion ';'
;
instruccion_retorno : 'return' ';' |'return' expresion ';'
;
instruccion_lanzamiento_excepcion : 'throw' expresion ';'
;
instruccion_captura_excepcion : 'try' bloque_instrucciones clausulas-catch
                              | 'try' bloque_instrucciones clausula_finally
                              | 'try' bloque_instrucciones clausulas-catch clausula_finally
;
clausulas-catch : mas_clausula_catch_especifica | clausula_catch_general | mas_clausula_catch_especifica clausula_catch_general
;
clausula_catch_especifica : 'catch' '(' nombre_tipo ')' bloque_instrucciones
;
mas_clausula_catch_especifica : mas_clausula_catch_especifica clausula_catch_especifica
;
clausula_catch_general : 'catch' bloque_instrucciones
;
clausula-finally : 'finally' bloque_instrucciones
;
instruccion_retorno : 'return' ';' | 'return' expresion ';'
;

/***************/
/* EXPRESIONES */
/***************/
expresion_constante : ENTERO | REAL | CADENA | CARACTER | BOOLEANO
;
expresion_parentesis : '(' expresion ')'
;
expresion_funcional : identificador_anidado '(' ')' | identificador_anidado '(' mas_expresion ')'
;
expresion_creacion_objeto : 'new' identificador_anidado '(' ')' | 'new' identificador_anidado '(' mas_expresion ')'
;
expresion_indexada : identificador_anidado
                    | expresion_indexada '[' expresion ']'
                    | expresion_indexada '->' identificador_anidado
;
expresion_postfija : expresion_constante
                    | expresion_parentesis
                    | expresion_funcional
                    | expresion_creacion_objeto
                    | expresion_indexada
                    | expresion_postfija '++'
                    | expresion_postfija '--'
;
expresion_prefija : expresion_postfija
                  | 'sizeof' expresion_prefija
                  | 'sizeof' '(' nombre_tipo ')'
                  | operador_prefijo expresion_cast
;
operador_prefijo : '++' | '--' | '&' | '*' | '+' | '-' | '~' | '!'
;
expresion_cast : expresion_prefija | '(' nombre_tipo ')' expresion_prefija
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
