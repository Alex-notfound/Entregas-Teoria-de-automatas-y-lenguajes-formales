< sección de declaraciones >
%{ 
   int linea = 0; 
%} 

RESERVADAS (auto|base|boolean|break|case|catch|class|char|continue|default|do|double|else|enum|extern|false|finally|float|for|goto|if|include|int|interface|long|namespace|new|override|private|protected|public|return|sealed|short|signed|sizeof|static|struct|switch|this|throw|true|typedef|union|unsigned|using|virtual|void|while)
IDENTIFICADORES [A-Za-z_][0-9]*

CARACTERES_ESCAPADOS (\\'|\\"|\\?|\\\\|\\n|\\r|\\t|\\v)

<El octal y el decimal pueden errar seguramente>

CONSTANTE_ENTERA_DECIMAL [0-9]+
CONSTANTE_ENTERA_OCTAL 0[0-7]+
CONSTANTE_ENTERA_HEXADECIMAL 0[xX][0-9a-fA-F]+

CONSTANTE_REAL_SIN_EXPONENTE ([0-9]*.[0-9]+|0[xX][0-9a-fA-f]*.[0-9a-fA-f]+)
CONSTANTE_REAL_EXPONENTE ([0-9]*.[0-9]+|0[xX][0-9a-fA-f]*.[0-9a-fA-f]+)[eE][+-]?[0-9]+

CONSTANTE_CARACTER \'([^\\\'\n]|{CARACTERES_ESCAPADOS}|\\[0-3]?[0-7]{1,2}|\\x[0-9a-fA-f]{1,2})\'
CONSTANTE_CADENA \"([^\\\"\n]*|{CARACTERES_ESCAPADOS}|\\[0-3]?[0-7]{1,2}|\\x[0-9a-fA-f]{1,2}|([^\n\\\"]*\\\n)*)\"

DELIMITADORES [(){}?:;,]

OPERADORES_ARITMETICOS ([+-*/%]|"--"|"++"|"*="|"/="|"%="|"+="|"-=")
OPERADORES_ACCESO_MEMORIA ([.*&\[\]]|"->")
OPERADORES_BITS ([~&|^]|"<<"|">>"|"<<="|">>="|"&="|"^="|"|=")
OPERADORES_RELACIONALES ([<>]|"<="|"=>"|"=="|"!=")
OPERADORES_LOGICOS (!|"&&"|"||")
OPERADORES_VARIOS (=|sizeof)

%%

< sección de reglas y acciones >
\n  num_lineas++;  

{RESERVADAS} printf("\nLinea %d, palabra reservada: %s" linea, yytext);
%%
< sección de rutinas de usuario >

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