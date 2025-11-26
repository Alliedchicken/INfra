#!/bin/bash

USUARIO_LOGUEADO=""
ARCH_USUARIOS="usuarios.txt"
ARCH_PRODUCTOS="productos.txt"
mkdir -p Datos

# ============================================
# Inicializar archivos necesarios
# ============================================
if [ ! -f "$ARCH_USUARIOS" ]; then
    echo "admin:admin" > "$ARCH_USUARIOS"
fi

if [ ! -f "$ARCH_PRODUCTOS" ]; then
    touch "$ARCH_PRODUCTOS"
fi


# ============================================
# 1) CREAR USUARIO
# ============================================
crear_usuario() {
    echo "Ingrese el nombre del nuevo usuario:"
    read nuevo_user

    if [ -z "$nuevo_user" ]; then
        echo "El usuario no puede estar vacío."
        return
    fi

    if grep -q "^$nuevo_user:" "$ARCH_USUARIOS"; then
        echo "El usuario ya existe."
        return
    fi

    echo "Ingrese la contraseña:"
    read -s pass
    echo "Repita la contraseña:"
    read -s pass2

    if [ -z "$pass" ]; then
        echo "La contraseña no puede ser vacía."
        return
    fi

    if [ "$pass" != "$pass2" ]; then
        echo "Las contraseñas no coinciden."
        return
    fi

    echo "$nuevo_user:$pass" >> "$ARCH_USUARIOS"
    echo "Usuario creado correctamente."
}


# ============================================
# 2) CAMBIAR CONTRASEÑA
# ============================================
cambiar_contrasena() {
    if [ -z "$USUARIO_LOGUEADO" ]; then
        echo "Debe estar logueado para cambiar la contraseña."
        return
    fi

    echo "Ingrese su contraseña actual:"
    read -s actual

    if ! grep -q "^$USUARIO_LOGUEADO:$actual$" "$ARCH_USUARIOS"; then
        echo "Contraseña incorrecta."
        return
    fi

    echo "Ingrese nueva contraseña:"
    read -s nueva
    echo "Repita nueva contraseña:"
    read -s nueva2

    if [ "$nueva" != "$nueva2" ]; then
        echo "Las contraseñas no coinciden."
        return
    fi

    # actualizar en archivo
    sed -i "s/^$USUARIO_LOGUEADO:.*/$USUARIO_LOGUEADO:$nueva/" "$ARCH_USUARIOS"
    echo "Contraseña actualizada."
}


# ============================================
# 3) LOGIN
# ============================================
login() {
    if [ -n "$USUARIO_LOGUEADO" ]; then
        echo "Ya estás logueado como $USUARIO_LOGUEADO."
        return
    fi

    echo "Usuario:"
    read user
    echo "Contraseña:"
    read -s pass

    if grep -q "^$user:$pass$" "$ARCH_USUARIOS"; then
        USUARIO_LOGUEADO="$user"
        echo "Login exitoso. Bienvenido $user!"
    else
        echo "Usuario o contraseña incorrectos."
    fi
}


# ============================================
# 4) LOGOUT
# ============================================
logout() {
    if [ -z "$USUARIO_LOGUEADO" ]; then
        echo "No hay usuario logueado."
    else
        echo "Usuario $USUARIO_LOGUEADO deslogueado."
        USUARIO_LOGUEADO=""
    fi
}


# ============================================
# 5) INGRESAR PRODUCTO
# ============================================
ingresar_producto() {
    if [ -z "$USUARIO_LOGUEADO" ]; then
        echo "Debe loguearse primero."
        return
    fi

    echo "Tipo (base/layer/shade/dry/contrast/technical/texture/mediums):"
    read tipo
    echo "Modelo:"
    read modelo
    echo "Descripción:"
    read desc
    echo "Cantidad:"
    read cant
    echo "Precio:"
    read precio

    codigo=$(echo "$tipo" | cut -c1-3 | tr '[:lower:]' '[:upper:]')

    echo "$codigo:$tipo:$modelo:$desc:$cant:$precio" >> "$ARCH_PRODUCTOS"

    echo "Producto ingresado correctamente:"
    echo "$codigo - $tipo - $modelo - $desc - $cant - \$ $precio"
}


# ============================================
# 6) VENDER PRODUCTO
# ============================================
vender_producto() {
    if [ -z "$USUARIO_LOGUEADO" ]; then
        echo "Debe loguearse primero."
        return
    fi

    echo "Lista de productos:"
    echo "------------------------------------"

    num=1
    while IFS=":" read -r cod tipo modelo desc cant precio; do
        echo "$num) $tipo - $modelo - Precio: \$ $precio - Stock: $cant"
        lista[$num]="$cod:$tipo:$modelo:$desc:$cant:$precio"
        num=$((num + 1))
    done < "$ARCH_PRODUCTOS"

    echo "Ingrese número de producto a comprar:"
    read elegido
    echo "Cantidad a comprar:"
    read cantidad

    seleccionado="${lista[$elegido]}"
    IFS=":" read -r cod tipo modelo desc stock precio <<< "$seleccionado"

    if (( cantidad > stock )); then
        echo "No hay stock suficiente."
        return
    fi

    nuevo_stock=$((stock - cantidad))

    # actualizar stock
    sed -i "${elegido}s/:$stock:/:$nuevo_stock:/" "$ARCH_PRODUCTOS"

    total=$((cantidad * precio))

    echo "============ RESUMEN COMPRA ============"
    echo "Producto: $tipo - $modelo"
    echo "Cantidad: $cantidad"
    echo "Total: \$ $total"
    echo "========================================"
}


# ============================================
# 7) FILTRAR PRODUCTOS
# ============================================
filtrar_productos() {
    if [ -z "$USUARIO_LOGUEADO" ]; then
        echo "Debe loguearse primero."
        return
    fi

    echo "Ingrese tipo a filtrar (o vacío para todos):"
    read filtro

    echo "------------------------------------"
    while IFS=":" read -r cod tipo modelo desc cant precio; do
        if [ -z "$filtro" ] || [[ "$tipo" == "$filtro" ]]; then
            echo "$cod - $tipo - $modelo - $desc - Stock: $cant - \$ $precio"
        fi
    done < "$ARCH_PRODUCTOS"
    echo "------------------------------------"
}


# ============================================
# 8) CREAR REPORTE CSV
# ============================================
generar_csv() {
    if [ -z "$USUARIO_LOGUEADO" ]; then
        echo "Debe loguearse primero."
        return
    fi

    echo "codigo,tipo,modelo,descripcion,cantidad,precio" > Datos/datos.csv
    cat "$ARCH_PRODUCTOS" >> Datos/datos.csv

    echo "Reporte generado en Datos/datos.csv"
}


# ============================================
# MENU PRINCIPAL
# ============================================
menu() {
    while true; do
        echo ""
        echo "========== INVENTARIO CITADEL =========="
        echo "Usuario actual: ${USUARIO_LOGUEADO:-Ninguno}"
        echo "----------------------------------------"
        echo "1) Crear usuario"
        echo "2) Cambiar contraseña"
        echo "3) Login"
        echo "4) Logout"
        echo "5) Ingresar producto"
        echo "6) Vender producto"
        echo "7) Filtro de productos"
        echo "8) Generar reporte CSV"
        echo "9) Salir"
        echo "----------------------------------------"
        echo "Seleccione una opción:"
        read opcion

        case $opcion in
            1) crear_usuario ;;
            2) cambiar_contrasena ;;
            3) login ;;
            4) logout ;;
            5) ingresar_producto ;;
            6) vender_producto ;;
            7) filtrar_productos ;;
            8) generar_csv ;;
            9) echo "Saliendo..."; exit ;;
            *) echo "Opción inválida." ;;
        esac
    done
}

menu
