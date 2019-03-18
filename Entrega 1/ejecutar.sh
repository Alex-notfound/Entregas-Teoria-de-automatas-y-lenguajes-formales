#!/bin/bash
rm salida.txt
clear
flex especificacionAnalizador.l
gcc -o especificacionAnalizador lex.yy.c diccionario.c
./especificacionAnalizador prueba.txt >> salida.txt
#cat salida.txt
sdiff salida.txt salidaObjetivo.txt
