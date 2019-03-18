%{
   #include <string.h>
   #include <stdlib.h> 
   int linea = 1; 
%} 

RESERVADAS (auto|base|boolean|break|case|catch|class|char|continue|default|do|double|else|enum|extern|false|finally|float|for|goto|if|include|int|interface|long|namespace|new|override|private|protected|public|return|sealed|short|signed|sizeof|static|struct|switch|this|throw|true|typedef|union|unsigned|using|virtual|void|while)
IDENTIFICADORES ([A-Za-z_][A-Za-z_0-9]*)
CARACTERES_ESCAPADOS (\\'|\\?|\\\\|\\n|\\r|\\t|\\v)
CONSTANTE_ENTERA_OCTAL (0[0-7]+)
CONSTANTE_ENTERA_DECIMAL ([0-9]+)
CONSTANTE_ENTERA_HEXADECIMAL (0[xX][0-9a-fA-F]+)
CONSTANTE_REAL_SIN_EXPONENTE ([0-9]*\.[0-9]+|0[xX][0-9a-fA-f]*\.[0-9a-fA-f]+)
CONSTANTE_REAL_EXPONENTE (([0-9]*\.[0-9]+|0[xX][0-9a-fA-f]*\.[0-9a-fA-f]+)[eE][+-]?[0-9]+)
DELIMITADORES ([(){}?:;,])
OPERADORES_ACCESO_MEMORIA ([.*&\[\]]|"->")
OPERADORES_BITS ([~&|^]|"<<"|">>"|"<<="|">>="|"&="|"^="|"|=")
OPERADORES_RELACIONALES ([<>]|"<="|"=>"|"=="|"!=")
OPERADORES_LOGICOS (!|"&&"|"||")
OPERADORES_VARIOS (=|sizeof)
COMENTARIOS (\/\/.*\n|\/\*)
CONSTANTE_CADENA (\"([^\\\"\n]*|{CARACTERES_ESCAPADOS}|\\[0-3]?[0-7]{1,2}|\\x[0-9a-fA-f]{1,2}|([^\n\\\"]*\\\n)*)\")
CONSTANTE_CARACTER (\'([^\\\'\n]|{CARACTERES_ESCAPADOS}|\\[0-3]?[0-7]{1,2}|\\x[0-9a-fA-f]{1,2})\')
OPERADORES_ARITMETICOS ([*/%+-]|"--"|"++"|"*="|"/="|"%="|"+="|"-=")

%%

\n  linea++;  

{DELIMITADORES} printf("\nLinea %d, delimitador: %s", linea, yytext);
{RESERVADAS} printf("\nLinea %d, palabra reservada: %s", linea, yytext);
{CONSTANTE_ENTERA_OCTAL} printf("\nLinea %d, constante entero octal: %s", linea, yytext);
{CONSTANTE_ENTERA_DECIMAL} printf("\nLinea %d, palabra reservada: %s", linea, yytext);
{CONSTANTE_ENTERA_HEXADECIMAL} printf("\nLinea %d, CONSTANTE_ENTERA_HEXADECIMAL: %s", linea, yytext);
{CONSTANTE_REAL_SIN_EXPONENTE} printf("\nLinea %d, CONSTANTE_REAL_SIN_EXPONENTE: %s", linea, yytext);
{CONSTANTE_REAL_EXPONENTE} printf("\nLinea %d, CONSTANTE_REAL_EXPONENTE: %s", linea, yytext);
{OPERADORES_ACCESO_MEMORIA} printf("\nLinea %d, operador de acceso a memoria: %s", linea, yytext);
{OPERADORES_BITS} printf("\nLinea %d, operador de bits: %s", linea, yytext);
{OPERADORES_RELACIONALES} printf("\nLinea %d, operador relacional: %s", linea, yytext);
{OPERADORES_LOGICOS} printf("\nLinea %d, operador logico: %s", linea, yytext);
{OPERADORES_VARIOS} printf("\nLinea %d, otro operador: %s", linea, yytext);
{CONSTANTE_CADENA} printf("\nLinea %d, CONSTANTE_CADENA: %s", linea, yytext);
{CONSTANTE_CARACTER} printf("\nLinea %d, caracter: %s", linea, yytext);
{COMENTARIOS} printf("\nLinea %d, comentario: %s", linea, yytext);
{OPERADORES_ARITMETICOS} printf("\nLinea %d, operador aritmetico: %s", linea, yytext);
{IDENTIFICADORES} printf("\nLinea %d, identificador: %s", linea, yytext);

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
