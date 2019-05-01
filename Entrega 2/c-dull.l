%{
	#include <stdio.h>
	#include <stdlib.h> 
	#include <ctype.h>
	#include "c-dull.tab.h"
   	int linea = 1;
   	int i = 0;
   	char cadena[256] = ""; 

%}

%x COMENTARIO
%x CADENA

RESERVADAS (abstract|base|boolean|break|case|catch|class|char|continue|default|do|double|else|enum|extern|false|finally|float|for|goto|if|int|internal|interface|long|namespace|new|override|private|protected|public|return|sealed|short|signed|sizeof|static|struct|switch|this|throw|true|try|typedef|union|unsigned|using|virtual|void|while)
IDENTIFICADORES ([A-Za-z_][A-Za-z_0-9]*)
CARACTERES_ESCAPADOS (\\'|\\\"|\\?|\\\\|\\n|\\r|\\t|\\v)
CONSTANTE_ENTERA_OCTAL (0[0-7]+)
CONSTANTE_ENTERA_DECIMAL ([0-9]+)
CONSTANTE_ENTERA_HEXADECIMAL (0[xX][0-9a-fA-F]+)
CONSTANTE_REAL_SIN_EXPONENTE ([0-9]*\.[0-9]+|0[xX][0-9a-fA-f]*\.[0-9a-fA-f]+)
CONSTANTE_REAL_EXPONENTE (([0-9]*\.[0-9]+|0[xX][0-9a-fA-f]*\.[0-9a-fA-f]+)[eE][+-]?[0-9]*\.?[0-9]+)
DELIMITADORES ([(){}?:;,])
OPERADORES_ACCESO_MEMORIA ([.*&\[\]]|"->")
OPERADORES_BITS ([~&|^]|"<<"|">>"|"<<="|">>="|"&="|"^="|"|=")
OPERADORES_RELACIONALES ([<>]|"<="|"=>"|"=="|"!=")
OPERADORES_LOGICOS (!|"&&"|"||")
OPERADORES_VARIOS (=|sizeof)
COMENTARIOS_UNA_LINEA (\/\/.*\n)
COMENTARIOS_MULTILINEA (\/\*)
CONSTANTE_CADENA ([^\\\n"]*|{CARACTERES_ESCAPADOS}|\\[0-3]?[0-7]{1,2}|\\x[0-9a-fA-f]{1,2})
CONSTANTE_CARACTER '([^\\'\n]|{CARACTERES_ESCAPADOS}|\\[0-3]?[0-7]{1,2}|\\x[0-9a-fA-f]{1,2})'
OPERADORES_ARITMETICOS ([*/%+-]|"--"|"++"|"*="|"/="|"%="|"+="|"-=")

%%

\n  linea++; 
(?i:ABSTRACT)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(ABSTRACT);}
(?i:BASE)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(BASE);}
(?i:BOOLEAN)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(BOOLEAN);}
(?i:BREAK)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(BREAK);}
(?i:CASE)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CASE);}
(?i:CATCH)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CATCH);}
(?i:CHAR)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CHAR);}
(?i:CLASS)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CLASS);}
(?i:CONTINUE)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CONTINUE);}
(?i:DEFAULT)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DEFAULT);}
(?i:DO)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DO);}
(?i:DOUBLE)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DOUBLE);}
(?i:ELSE)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(ELSE);}
(?i:ENUM)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(ENUM);}
(?i:EXTERN)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(EXTERN);}
(?i:FINALLY)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(FINALLY);}
(?i:FLOAT)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(FLOAT);}
(?i:FOR)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(FOR);}
(?i:GOTO)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(GOTO);}
(?i:IF)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(IF);}
(?i:INT)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(INT);}
(?i:INTERFACE)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(INTERFACE);}
(?i:INTERNAL)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(INTERNAL);}
(?i:LONG)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(LONG);}
(?i:NAMESPACE)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(NAMESPACE);}
(?i:NEW)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(NEW);}
(?i:OVERRIDE)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(OVERRIDE);}
(?i:PRIVATE)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(PRIVATE);}
(?i:PROTECTED)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(PROTECTED);}
(?i:PUBLIC)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(PUBLIC);}
(?i:RETURN)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(RETURN);}
(?i:SEALED)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(SEALED);}
(?i:SHORT)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(SHORT);}
(?i:SIGNED)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(SIGNED);}
(?i:STATIC)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(STATIC);}
(?i:STRUCT)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(STRUCT);}
(?i:SWITCH)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(SWITCH);}
(?i:THIS)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(THIS);}
(?i:THROW)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(THROW);}
(?i:TRY)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(TRY);}
(?i:TYPEDEF)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(TYPEDEF);}
(?i:UNION)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(UNION);}
(?i:UNSIGNED)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(UNSIGNED);}
(?i:USING)				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(USING);}




{DELIMITADORES} printf("\nlinea %d, delimitador: %s", linea, yytext); return (yytext[0]);
{COMENTARIOS_UNA_LINEA} linea++; return (yytext[0]);
{IDENTIFICADORES} printf("\nlinea %d, identificador: %s", linea, yytext); return (IDENTIFICADOR);
{CONSTANTE_ENTERA_OCTAL} printf("\nlinea %d, constante entero octal: %s", linea, yytext); return (ENTERO);
{CONSTANTE_ENTERA_DECIMAL} printf("\nlinea %d, constante entera decimal: %s", linea, yytext); return (ENTERO);
{CONSTANTE_ENTERA_HEXADECIMAL} printf("\nlinea %d, constante entera hexadecimal: %s", linea, yytext); return (ENTERO);
{CONSTANTE_REAL_SIN_EXPONENTE} printf("\nlinea %d, constante real sin exponente: %s", linea, yytext); return (REAL);
{CONSTANTE_REAL_EXPONENTE} printf("\nlinea %d, constante real con exponente: %s", linea, yytext); return (REAL);
{CONSTANTE_CARACTER} printf("\nlinea %d, constante caracter: %s", linea, yytext); return (CARACTER);
{OPERADORES_ACCESO_MEMORIA} printf("\nlinea %d, operador: %s", linea, yytext); return (yytext[0]);
{OPERADORES_BITS} printf("\nlinea %d, operador: %s", linea, yytext); return (yytext[0]);
{OPERADORES_RELACIONALES} printf("\nlinea %d, operador: %s", linea, yytext); return (yytext[0]);
{OPERADORES_LOGICOS} printf("\nlinea %d, operador logico: %s", linea, yytext); return (yytext[0]);
{OPERADORES_VARIOS} printf("\nlinea %d, operador: %s", linea, yytext); return (yytext[0]);
{OPERADORES_ARITMETICOS} printf("\nlinea %d, operador: %s", linea, yytext); return (yytext[0]);

{COMENTARIOS_MULTILINEA}	BEGIN(COMENTARIO);
<COMENTARIO>\n			linea++;
<COMENTARIO>.			;
<COMENTARIO>\*\/		BEGIN 0;

\"		BEGIN(CADENA);strcat(cadena, yytext);
<CADENA>\\\n			i++;linea++;strcat(cadena, yytext);
<CADENA>\"	BEGIN 0;strcat(cadena, yytext); printf("\nlinea %d, constante cadena: %s", linea-i, cadena);i=0;strcpy(cadena,""); return (CADENA);
<CADENA>{CONSTANTE_CADENA}		strcat(cadena,yytext);

%%

void minus(char cadena[]){
	for (int indice = 0; cadena[indice] != '\0'; ++indice){
		cadena[indice] = toupper(cadena[indice]);
	}
	printf("Cadena: %s", cadena);
}