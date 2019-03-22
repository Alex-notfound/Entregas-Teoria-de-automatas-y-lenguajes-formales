%{
	#include <string.h>
	#include <stdlib.h> 
   	int linea = 1;
   	int i = 0;
   	char cadena[256] = ""; 
%} 

%x COMENTARIO
%x CADENA

RESERVADAS (auto|base|boolean|break|case|catch|class|char|continue|default|do|double|else|enum|extern|false|finally|float|for|goto|if|include|int|interface|long|namespace|new|override|private|protected|public|return|sealed|short|signed|sizeof|static|struct|switch|this|throw|true|typedef|union|unsigned|using|virtual|void|while)
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
{DELIMITADORES} printf("\nlinea %d, delimitador: %s", linea, yytext);
{RESERVADAS} printf("\nlinea %d, palabra reservada: %s", linea, yytext);
{COMENTARIOS_UNA_LINEA} linea++;
{IDENTIFICADORES} printf("\nlinea %d, identificador: %s", linea, yytext);
{CONSTANTE_ENTERA_OCTAL} printf("\nlinea %d, constante entero octal: %s", linea, yytext);
{CONSTANTE_ENTERA_DECIMAL} printf("\nlinea %d, constante entera decimal: %s", linea, yytext);
{CONSTANTE_ENTERA_HEXADECIMAL} printf("\nlinea %d, constante entera hexadecimal: %s", linea, yytext);
{CONSTANTE_REAL_SIN_EXPONENTE} printf("\nlinea %d, constante real sin exponente: %s", linea, yytext);
{CONSTANTE_REAL_EXPONENTE} printf("\nlinea %d, constante real con exponente: %s", linea, yytext);
{CONSTANTE_CARACTER} printf("\nlinea %d, constante caracter: %s", linea, yytext);
{OPERADORES_ACCESO_MEMORIA} printf("\nlinea %d, operador: %s", linea, yytext);
{OPERADORES_BITS} printf("\nlinea %d, operador: %s", linea, yytext);
{OPERADORES_RELACIONALES} printf("\nlinea %d, operador: %s", linea, yytext);
{OPERADORES_LOGICOS} printf("\nlinea %d, operador logico: %s", linea, yytext);
{OPERADORES_VARIOS} printf("\nlinea %d, otro operador: %s", linea, yytext);
{OPERADORES_ARITMETICOS} printf("\nlinea %d, operador: %s", linea, yytext);

{COMENTARIOS_MULTILINEA}	BEGIN(COMENTARIO);
<COMENTARIO>\n			linea++;
<COMENTARIO>.			;
<COMENTARIO>\*\/		BEGIN 0;

\"		BEGIN(CADENA);strcat(cadena, yytext);
<CADENA>\\\n			i++;linea++;strcat(cadena, yytext);
<CADENA>\"	BEGIN 0;strcat(cadena, yytext); printf("\nlinea %d, constante cadena: %s", linea-i, cadena);i=0;strcpy(cadena,"");
<CADENA>{CONSTANTE_CADENA}		strcat(cadena,yytext);

%%

int yywrap() 
{ 
    return 1; 
}

int main(int argc, char *argv []) 
{
  yyin = fopen(argv[1],"r");
  yylex();
  printf("\n\n");
  fclose(yyin);
} 
