< sección de declaraciones >
% %

RESERVADAS (auto|base|boolean|break|case|catch|class|char|continue|default|do|double|else|enum|extern|false|finally|float|for|goto|if|include|int|interface|long|namespace|new|override|private|protected|public|return|sealed|short|signed|sizeof|static|struct|switch|this|throw|true|typedef|union|unsigned|using|virtual|void|while)
IDENTIFICADORES [A-Za-z_][0-9]*

<El octal y el decimal pueden errar seguramente>
CONSTANTE_ENTERA_DECIMAL [0-9]+
CONSTANTE_ENTERA_OCTAL 0[0-7]+
CONSTANTE_ENTERA_HEXADECIMAL 0[xX][0-9a-fA-F]+

CONSTANTE_REAL_SIN_EXPONENTE ([0-9]*.[0-9]+|0[xX][0-9a-fA-f]*.[0-9a-fA-f]+)
CONSTANTE_REAL_EXPONENTE ([0-9]*.[0-9]+|0[xX][0-9a-fA-f]*.[0-9a-fA-f]+)[eE][+-]?[0-9]+


< sección de reglas y acciones >
% %
< sección de rutinas de usuario >

int main(int argc, char *argv []) 
{
  yyin = fopen(argv[1],"r");
  yylex();
  printf("\n\n");
  fclose(yyin);
}