#!/bin/bash
DIRECTORIO_BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

ARCHIVO_USUARIOS="$DIRECTORIO_BASE/usuarios.db"
ARCHIVO_PRODUCTOS="$DIRECTORIO_BASE/productos.db"
CARPETA_DATOS="$DIRECTORIO_BASE/Datos"
ARCHIVO_CSV="$CARPETA_DATOS/datos.CSV"

USUARIO_ACTUAL=""

# Tipos de pintura válidos (según enunciado)
TIPOS_VALIDOS=(
  "Base"
  "Layer"
  "Shade"
  "Dry"
  "Contrast"
  "Technical"
  "Texture"
  "Mediums"
)

# --------------------------------------------------------------
#  Funciones generales de utilidad
# --------------------------------------------------------------

pausar() {
  printf "Presione Enter para continuar..." >&2
  read -r
}

# Asegura que existan archivos de usuarios y productos,
# y que el usuario admin:admin esté presente.
asegurar_almacenamiento() {
  # Archivo de usuarios
  if [[ ! -f "$ARCHIVO_USUARIOS" ]]; then
    printf "admin:admin\n" >"$ARCHIVO_USUARIOS"
  fi

  # Asegurar que admin exista siempre
  if ! awk -F':' '$1=="admin"{found=1} END{exit found?0:1}' "$ARCHIVO_USUARIOS" >/dev/null 2>&1; then
    printf "admin:admin\n" >>"$ARCHIVO_USUARIOS"
  fi

  # Archivo de productos (vacío si no existe)
  [[ -f "$ARCHIVO_PRODUCTOS" ]] || : >"$ARCHIVO_PRODUCTOS"
}

# Requiere que haya un usuario logueado para continuar.
requerir_sesion() {
  if [[ -z "$USUARIO_ACTUAL" ]]; then
    echo "Debe iniciar sesión para usar esta opción."
    pausar
    return 1
  fi
  return 0
}

# Elimina espacios en blanco al inicio y al final
recortar() {
  local texto="$1"
  texto="${texto#"${texto%%[![:space:]]*}"}"
  texto="${texto%"${texto##*[![:space:]]}"}"
  printf "%s" "$texto"
}

# Pide un valor por teclado que no puede ser vacío
pedir_no_vacio() {
  local mensaje="$1"
  local entrada
  while true; do
    read -r -p "$mensaje" entrada
    entrada="$(recortar "$entrada")"
    if [[ -n "$entrada" ]]; then
      printf "%s\n" "$entrada"
      return 0
    fi
    echo "El valor no puede ser vacío."
  done
}

# Pide contraseña no vacía con confirmación
pedir_contrasenia() {
  local mensaje="$1"
  local pass confirm
  while true; do
    read -rs -p "$mensaje" pass
    echo
    pass="$(recortar "$pass")"
    if [[ -z "$pass" ]]; then
      echo "La contraseña no puede quedar en blanco."
      continue
    fi
    read -rs -p "Confirmar contraseña: " confirm
    echo
    confirm="$(recortar "$confirm")"
    if [[ "$pass" == "$confirm" ]]; then
      printf "%s\n" "$pass"
      return 0
    fi
    echo "Las contraseñas no coinciden."
  done
}

# Pide un entero positivo (> 0)
pedir_entero_positivo() {
  local mensaje="$1"
  local valor
  while true; do
    read -r -p "$mensaje" valor
    valor="$(recortar "$valor")"
    if [[ "$valor" =~ ^[0-9]+$ ]] && ((10#$valor > 0)); then
      printf "%d\n" "$valor"
      return 0
    fi
    echo "Ingrese un número entero positivo."
  done
}

# --------------------------------------------------------------
#  Manejo de usuarios
# --------------------------------------------------------------

existe_usuario() {
  local usuario="$1"
  [[ -f "$ARCHIVO_USUARIOS" ]] || return 1
  awk -F':' -v buscado="$usuario" '$1==buscado{found=1; exit} END{exit found?0:1}' "$ARCHIVO_USUARIOS"
}

actualizar_contrasenia_usuario() {
  local usuario_objetivo="$1"
  local nueva_pass="$2"
  local tmp
  tmp="$(mktemp)"
  awk -F':' -v buscado="$usuario_objetivo" -v pass="$nueva_pass" '
    BEGIN{OFS=":"}
    $1==buscado {print $1, pass; next}
    {print $0}
  ' "$ARCHIVO_USUARIOS" >"$tmp" && mv "$tmp" "$ARCHIVO_USUARIOS"
}

crear_usuario() {
  if ! requerir_sesion; then
    return
  fi

  echo "=== Crear usuario ==="
  local usuario nuevo_pass
  usuario="$(pedir_no_vacio 'Nuevo usuario: ')"

  if existe_usuario "$usuario"; then
    echo "El usuario '$usuario' ya existe."
    pausar
    return
  fi

  nuevo_pass="$(pedir_contrasenia 'Ingrese contraseña: ')"

  # Sanitizar por si hubiera CR/LF embebidos
  nuevo_pass="${nuevo_pass//$'\r'/}"
  nuevo_pass="${nuevo_pass//$'\n'/}"

  printf "%s:%s\n" "$usuario" "$nuevo_pass" >>"$ARCHIVO_USUARIOS"
  echo "Usuario '$usuario' creado correctamente."
  pausar
}

cambiar_contrasenia() {
  if ! requerir_sesion; then
    return
  fi

  echo "=== Cambiar contraseña ==="
  local objetivo="$USUARIO_ACTUAL"

  # Si es admin, puede cambiar la contraseña de otro usuario
  if [[ "$USUARIO_ACTUAL" == "admin" ]]; then
    local entrada
    read -r -p "Usuario a modificar (Enter para 'admin'): " entrada
    entrada="$(recortar "$entrada")"
    if [[ -n "$entrada" ]]; then
      objetivo="$entrada"
    fi
  fi

  if ! existe_usuario "$objetivo"; then
    echo "El usuario '$objetivo' no existe."
    pausar
    return
  fi

  local nueva_pass
  nueva_pass="$(pedir_contrasenia 'Nueva contraseña: ')"
  actualizar_contrasenia_usuario "$objetivo" "$nueva_pass"
  echo "Contraseña actualizada para '$objetivo'."
  pausar
}

iniciar_sesion() {
  if [[ -n "$USUARIO_ACTUAL" ]]; then
    echo "Ya hay una sesión activa con el usuario: $USUARIO_ACTUAL"
    pausar
    return
  fi

  echo "=== Login ==="
  local usuario pass almacenada
  usuario="$(pedir_no_vacio 'Usuario: ')"
  read -rs -p "Contraseña: " pass
  echo

  if ! existe_usuario "$usuario"; then
    echo "Usuario o contraseña incorrectos."
    pausar
    return
  fi

  almacenada="$(awk -F':' -v buscado="$usuario" '$1==buscado{print $2; exit}' "$ARCHIVO_USUARIOS")"
  if [[ "$pass" != "$almacenada" ]]; then
    echo "Usuario o contraseña incorrectos."
    pausar
    return
  fi

  USUARIO_ACTUAL="$usuario"
  echo "Bienvenido/a, $USUARIO_ACTUAL."
  pausar
}

cerrar_sesion() {
  if [[ -z "$USUARIO_ACTUAL" ]]; then
    echo "No hay sesión activa."
  else
    echo "Hasta luego, $USUARIO_ACTUAL."
    USUARIO_ACTUAL=""
  fi
  pausar
}

# --------------------------------------------------------------
#  Manejo de tipos de pintura
# --------------------------------------------------------------

mostrar_tipos_validos() {
  printf "Tipos de pintura disponibles:\n" >&2
  for tipo in "${TIPOS_VALIDOS[@]}"; do
    printf "  - %s\n" "$tipo" >&2
  done
}

pedir_tipo_valido() {
  local entrada normalizado tipo
  while true; do
    mostrar_tipos_validos
    printf "Ingrese tipo: " >&2
    read -r entrada
    entrada="$(recortar "$entrada")"
    normalizado="${entrada,,}"
    for tipo in "${TIPOS_VALIDOS[@]}"; do
      if [[ "${tipo,,}" == "$normalizado" ]]; then
        printf "%s\n" "$tipo"
        return 0
      fi
    done
    printf "Tipo inválido. Debe ser uno de la lista.\n" >&2
  done
}

# --------------------------------------------------------------
#  Manejo de productos
# --------------------------------------------------------------

ingresar_producto() {
  if ! requerir_sesion; then
    return
  fi

  echo "=== Ingresar producto ==="
  local tipo modelo descripcion cantidad precio codigo

  tipo="$(pedir_tipo_valido)"
  modelo="$(pedir_no_vacio 'Modelo: ')"
  descripcion="$(pedir_no_vacio 'Descripción: ')"
  cantidad="$(pedir_entero_positivo 'Cantidad: ')"
  precio="$(pedir_entero_positivo 'Precio unitario ($, entero): ')"

  codigo="$(printf "%s" "$tipo" | cut -c1-3 | tr '[:lower:]' '[:upper:]')"

  printf "%s|%s|%s|%s|%s|%s\n" "$codigo" "$tipo" "$modelo" "$descripcion" "$cantidad" "$precio" >>"$ARCHIVO_PRODUCTOS"
  echo "Producto ingresado:"
  echo "$codigo - $tipo - $modelo - $descripcion - $cantidad - \$ $precio"
  pausar
}

listar_productos() {
  if [[ ! -s "$ARCHIVO_PRODUCTOS" ]]; then
    echo "No hay productos cargados."
    return 1
  fi

  mapfile -t productos <"$ARCHIVO_PRODUCTOS"
  local indice=1
  for linea in "${productos[@]}"; do
    IFS='|' read -r codigo tipo modelo descripcion cantidad precio <<<"$linea"
    printf "%d) %s - %s - %s - Stock: %s - $ %s\n" \
      "$indice" "$tipo" "$modelo" "$descripcion" "$cantidad" "$precio"
    ((indice++))
  done
  return 0
}

vender_producto() {
  if ! requerir_sesion; then
    return
  fi

  if [[ ! -s "$ARCHIVO_PRODUCTOS" ]]; then
    echo "No hay productos disponibles."
    pausar
    return
  fi

  mapfile -t productos <"$ARCHIVO_PRODUCTOS"
  # "carrito" -> índice de producto => cantidad seleccionada
  declare -A selecciones=()

  while true; do
    echo "=== Catálogo de productos ==="
    local indice=1
    for linea in "${productos[@]}"; do
      IFS='|' read -r codigo tipo modelo descripcion cantidad precio <<<"$linea"
      printf "%d) %s - %s - $ %s (Stock: %s)\n" \
        "$indice" "$tipo" "$modelo" "$precio" "$cantidad"
      ((indice++))
    done

    read -r -p "Seleccione número de producto (Enter para terminar compra): " opcion
    opcion="$(recortar "$opcion")"

    if [[ -z "$opcion" ]]; then
      break
    fi

    if ! [[ "$opcion" =~ ^[0-9]+$ ]]; then
      echo "Debe ingresar un número válido."
      continue
    fi

    local posicion=$((10#$opcion))
    if (( posicion < 1 || posicion > ${#productos[@]} )); then
      echo "Selección fuera de rango."
      continue
    fi

    local indice_array=$((posicion - 1))
    IFS='|' read -r codigo tipo modelo descripcion cantidad precio <<<"${productos[$indice_array]}"

    local ya_tomadas=${selecciones[$indice_array]:-0}
    local disponibles=$((cantidad - ya_tomadas))

    if (( disponibles <= 0 )); then
      echo "No queda stock disponible para ese producto."
      continue
    fi

    local cant_compra
    while true; do
      read -r -p "Cantidad a comprar (máximo $disponibles): " cant_compra
      cant_compra="$(recortar "$cant_compra")"
      if ! [[ "$cant_compra" =~ ^[0-9]+$ ]]; then
        echo "Ingrese un número entero."
        continue
      fi
      cant_compra=$((10#$cant_compra))
      if (( cant_compra <= 0 || cant_compra > disponibles )); then
        echo "Cantidad fuera de rango."
        continue
      fi
      break
    done

    selecciones[$indice_array]=$((ya_tomadas + cant_compra))
    echo "Producto agregado al carrito."
  done

  if (( ${#selecciones[@]} == 0 )); then
    echo "No se realizaron compras."
    pausar
    return
  fi

  # Resumen y actualización de stock
  local total=0
  echo
  echo "=== Resumen de compra ==="
  printf "%-12s %-20s %-10s %-10s\n" "Tipo" "Modelo" "Cantidad" "Subtotal"

  for indice_array in "${!selecciones[@]}"; do
    IFS='|' read -r codigo tipo modelo descripcion cantidad precio <<<"${productos[$indice_array]}"
    local cantidad_comprada=${selecciones[$indice_array]}
    local subtotal=$((cantidad_comprada * precio))
    total=$((total + subtotal))

    printf "%-12s %-20s %-10s $ %s\n" "$tipo" "$modelo" "$cantidad_comprada" "$subtotal"

    # Actualizar stock en el array
    cantidad=$((cantidad - cantidad_comprada))
    productos[$indice_array]="$codigo|$tipo|$modelo|$descripcion|$cantidad|$precio"
  done

  printf "TOTAL A PAGAR: $ %s\n" "$total"

  # Volcar cambios al archivo de productos
  local tmp
  tmp="$(mktemp)"
  for linea in "${productos[@]}"; do
    echo "$linea" >>"$tmp"
  done
  mv "$tmp" "$ARCHIVO_PRODUCTOS"

  pausar
}

filtrar_productos() {
  if ! requerir_sesion; then
    return
  fi

  if [[ ! -s "$ARCHIVO_PRODUCTOS" ]]; then
    echo "No hay productos cargados."
    pausar
    return
  fi

  echo "=== Filtro de productos por tipo ==="
  echo "Si deja el filtro vacío, se mostrarán todos los productos."
  local filtro
  read -r -p "Tipo a filtrar (ej: Base, Contrast, etc.): " filtro
  filtro="$(recortar "$filtro")"
  local filtro_min="${filtro,,}"

  local encontrado=0
  while IFS='|' read -r codigo tipo modelo descripcion cantidad precio; do
    [[ -z "$codigo" ]] && continue
    if [[ -n "$filtro_min" && "${tipo,,}" != "$filtro_min" ]]; then
      continue
    fi
    encontrado=1
    echo "$codigo - $tipo - $modelo - $descripcion - $cantidad - \$ $precio"
  done <"$ARCHIVO_PRODUCTOS"

  if [[ "$encontrado" -eq 0 ]]; then
    echo "No se encontraron productos para el filtro indicado."
  fi
  pausar
}

# --------------------------------------------------------------
#  Generación de reporte CSV
# --------------------------------------------------------------

escapar_csv() {
  local valor="$1"
  valor="${valor//\"/\"\"}"
  printf "\"%s\"" "$valor"
}

generar_reporte() {
  if ! requerir_sesion; then
    return
  fi

  if [[ ! -s "$ARCHIVO_PRODUCTOS" ]]; then
    echo "No hay productos para generar el reporte."
    pausar
    return
  fi

  mkdir -p "$CARPETA_DATOS"

  {
    echo "codigo,tipo,modelo,descripcion,cantidad,precio"
    while IFS='|' read -r codigo tipo modelo descripcion cantidad precio; do
      [[ -z "$codigo" ]] && continue
      printf "%s,%s,%s,%s,%s,%s\n" \
        "$(escapar_csv "$codigo")" \
        "$(escapar_csv "$tipo")" \
        "$(escapar_csv "$modelo")" \
        "$(escapar_csv "$descripcion")" \
        "$cantidad" \
        "$precio"
    done <"$ARCHIVO_PRODUCTOS"
  } >"$ARCHIVO_CSV"

  echo "Reporte generado en: $ARCHIVO_CSV"
  pausar
}

# --------------------------------------------------------------
#  Menú principal
# --------------------------------------------------------------

menu_principal() {
  while true; do
    clear
    echo "================= Inventario de Pinturas Citadel ================="
    if [[ -n "$USUARIO_ACTUAL" ]]; then
      echo "Usuario actual: $USUARIO_ACTUAL"
    else
      echo "Usuario actual: (ninguno)"
    fi
    cat <<'MENU'

1) Iniciar sesión
2) Cerrar sesión
3) Crear usuario
4) Cambiar contraseña
5) Ingresar producto
6) Vender producto
7) Filtro de productos
8) Generar reporte CSV
9) Salir
MENU
    read -r -p "Seleccione opción: " opcion
    opcion="$(recortar "$opcion")"

    case "$opcion" in
      1) iniciar_sesion ;;
      2) cerrar_sesion ;;
      3) crear_usuario ;;
      4) cambiar_contrasenia ;;
      5) ingresar_producto ;;
      6) vender_producto ;;
      7) filtrar_productos ;;
      8) generar_reporte ;;
      9)
        echo "Saliendo del sistema..."
        exit 0
        ;;
      *)
        echo "Opción inválida."
        pausar
        ;;
    esac
  done
}

# --------------------------------------------------------------
#  Programa principal
# --------------------------------------------------------------
asegurar_almacenamiento
menu_principal
