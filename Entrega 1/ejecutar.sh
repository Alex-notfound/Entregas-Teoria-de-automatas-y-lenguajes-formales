#!/bin/bash
rm salida.txt
#Realiza varios saltos de linea
clear
#Compila la especificacion del analizador y genera un fichero C
flex especificacionAnalizador.l
#Compila ficheros C
gcc -o especificacionAnalizador lex.yy.c diccionario.c
#Ejecuta
./especificacionAnalizador prueba.txt >> salida.txt
#Muestra la salida almacenada en salida.txt
#cat salida.txt
#Sirve para comparar las diferencias entre el fichero de salida y el fichero con la salida esperada
sdiff salida.txt salidaObjetivo.txt
