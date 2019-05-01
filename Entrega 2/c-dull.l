%{
	#include <stdio.h>
	#include <stdlib.h> 
	#include <ctype.h>
	#include "c-dull.tab.h"
   	int linea = 1;
   	int i = 0;
   	char cadena[256] = ""; 
   	char reserv[20];
%}

%x COMENTARIO
%x STRING

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

\"		BEGIN(STRING);strcat(cadena, yytext);
<STRING>\\\n			i++;linea++;strcat(cadena, yytext);
<STRING>\"	BEGIN 0;strcat(cadena, yytext); printf("\nlinea %d, constante cadena: %s", linea-i, cadena);i=0;strcpy(cadena,""); return (CADENA);
<STRING>{CONSTANTE_CADENA}		strcat(cadena,yytext);

"abstract"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(ABSTRACT);}
"base"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(BASE);}
"boolean"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(BOOLEAN);}
"break"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(BREAK);}
"case"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CASE);}
"catch"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CATCH);}
"char"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CHAR);}
"class"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CLASS);}
"continue"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CONTINUE);}
"default"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DEFAULT);}
"do"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DO);}
"double"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DOUBLE);}
"else"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(ELSE);}
"enum"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(ENUM);}
"extern"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(EXTERN);}
"finally"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(FINALLY);}
"float"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(FLOAT);}
"for"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(FOR);}
"goto"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(GOTO);}
"if"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(IF);}
"int"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(INT);}
"interface"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(INTERFACE);}
"internal"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(INTERNAL);}
"long"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(LONG);}
"namespace"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(NAMESPACE);}
"new"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(NEW);}
"override"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(OVERRIDE);}
"private"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(PRIVATE);}
"protected"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(PROTECTED);}
"public"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(PUBLIC);}
"return"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(RETURN);}
"sealed"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(SEALED);}
"short"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(SHORT);}
"signed"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(SIGNED);}
"static"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(STATIC);}
"struct"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(STRUCT);}
"switch"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(SWITCH);}
"this"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(THIS);}
"throw"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(THROW);}
"try"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(TRY);}
"typedef"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(TYPEDEF);}
"union"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(UNION);}
"unsigned"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(UNSIGNED);}
"using"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(USING);}             
"virtual"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(VIRTUAL);}
"void"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(VOID);}
"while"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(WHILE);}
"identificador"		{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(IDENTIFICADOR);}
"entero"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(ENTERO);}
"cadena"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CADENA);}
"real"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(REAL);}
"caracter"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(CARACTER);}
"booleano"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(BOOLEANO);}
"sizeof"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(SIZEOF);}
"ptr_acceso"		{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(PTR_ACCESO);}
"inc"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(INC);}
"dec"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DEC);}
"despi"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DESPI);}
"despd"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DESPD);}
"le"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(LE);}
"ge"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(GE);}
"eq"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(EQ);}             
"neq"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(NEQ);}
"and"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(AND);}
"or"				{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(OR);}
"mult_asig"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(MULT_ASIG);}
"div_asig"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DIV_ASIG);}
"mod_asig"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(MOD_ASIG);}
"suma_asig"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(SUMA_ASIG);}
"resta_asig"		{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(RESTA_ASIG);}
"despi_asig"		{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DESPI_ASIG);}
"despd_asig"		{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(DESPD_ASIG);}
"and_asig"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(AND_ASIG);}
"xor_asig"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(XOR_ASIG);}
"or_asig"			{printf("\nlinea %d, palabra reservada: %s", linea, yytext); return(OR_ASIG);}

{DELIMITADORES} printf("\nlinea %d, delimitador: %s", linea, yytext); return (yytext[0]);
{COMENTARIOS_UNA_LINEA} linea++;
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


%%