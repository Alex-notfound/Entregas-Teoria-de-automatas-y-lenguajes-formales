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


/*************/
/* VARIABLES */
/*************/


/*********/
/* TIPOS */
/*********/


/**********/
/* CLASES */
/**********/


/*************/
/* FUNCIONES */
/*************/


/*****************/
/* INSTRUCCIONES */
/*****************/


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
