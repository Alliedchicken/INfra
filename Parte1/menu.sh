#!/bin/bash

# Funciones de ejemplo
funcion1() {
    echo "Ejecutando funcion 1..."
}

funcion2() {
    echo "Ejecutando funcio≥n 2..."
}

menu() {
    echo "Opcion 1"
    echo "Opcion 2"
    echo "Opcion 3 - Salir"
    read -p "Elija una opcion: " opcion

    if [ "$opcion" -eq 1 ]; then
        funcion1
    fi

    if [ "$opcion" -eq 2 ]; then
        funcion2
    fi

    if [ "$opcion" -eq 3 ]; then
        echo "Saliendo..."
        exit 0
    fi
}

# Llamamos al menu√∫
menu

