SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
USERS_FILE="$SCRIPT_DIR/usuarios.db"
PRODUCTS_FILE="$SCRIPT_DIR/productos.db"
DATOS_DIR="$SCRIPT_DIR/Datos"
DATOS_FILE="$DATOS_DIR/datos.csv"

LOGGED_IN_USER=""

VALID_TYPES=(
  "Base"
  "Layer"
  "Shade"
  "Dry"
  "Contrast"
  "Technical"
  "Texture"
  "Mediums"
)

pause() {
  read -rp "Presione Enter para continuar..."
}

ensure_storage() {
  if [[ ! -f "$USERS_FILE" ]]; then
    printf "admin:admin\n" >"$USERS_FILE"
  fi

  if ! awk -F':' '$1=="admin"{found=1} END{exit found?0:1}' "$USERS_FILE" >/dev/null 2>&1; then
    printf "admin:admin\n" >>"$USERS_FILE"
  fi

  [[ -f "$PRODUCTS_FILE" ]] || : >"$PRODUCTS_FILE"
}

require_login() {
  if [[ -z "$LOGGED_IN_USER" ]]; then
    echo "Debe iniciar sesión para usar esta opción."
    pause
    return 1
  fi
  return 0
}

trim() {
  local input="$1"
  input="${input#"${input%%[![:space:]]*}"}"
  input="${input%"${input##*[![:space:]]}"}"
  printf "%s" "$input"
}

prompt_non_empty() {
  local prompt="$1"
  local input
  while true; do
    read -r -p "$prompt" input
    input="$(trim "$input")"
    if [[ -n "$input" ]]; then
      printf "%s\n" "$input"
      return 0
    fi
    echo "El valor no puede ser vacío."
  done
}

prompt_password() {
  local prompt="$1"
  local pass confirm
  while true; do
    read -rs -p "$prompt" pass
    echo
    pass="$(trim "$pass")"
    if [[ -z "$pass" ]]; then
      echo "La contraseña no puede quedar en blanco."
      continue
    fi
    read -rs -p "Confirmar contraseña: " confirm
    echo
    confirm="$(trim "$confirm")"
    if [[ "$pass" == "$confirm" ]]; then
      printf "%s\n" "$pass"
      return 0
    fi
    echo "Las contraseñas no coinciden."
  done
}

user_exists() {
  local user="$1"
  [[ -f "$USERS_FILE" ]] || return 1
  awk -F':' -v buscado="$user" '$1==buscado{found=1; exit} END{exit found?0:1}' "$USERS_FILE"
}

update_user_password() {
  local target="$1"
  local new_pass="$2"
  local tmp
  tmp="$(mktemp)"
  awk -F':' -v buscado="$target" -v pass="$new_pass" '
    BEGIN{OFS=":"}
    $1==buscado {print $1, pass; next}
    {print $0}
  ' "$USERS_FILE" >"$tmp" && mv "$tmp" "$USERS_FILE"
}

create_user() {
  if ! require_login; then
    return
  fi

  local username password
  username="$(prompt_non_empty "Nuevo usuario: ")"

  if user_exists "$username"; then
    echo "El usuario '$username' ya existe."
    pause
    return
  fi

  password="$(prompt_password "Ingrese contraseña: ")"
  printf "%s:%s\n" "$username" "$password" >>"$USERS_FILE"
  echo "Usuario '$username' creado correctamente."
  pause
}

change_password() {
  if ! require_login; then
    return
  fi

  local target="$LOGGED_IN_USER"
  if [[ "$LOGGED_IN_USER" == "admin" ]]; then
    read -r -p "Usuario a modificar (Enter para admin): " input
    input="$(trim "$input")"
    if [[ -n "$input" ]]; then
      target="$input"
    fi
  fi

  if ! user_exists "$target"; then
    echo "El usuario '$target' no existe."
    pause
    return
  fi

  local new_pass
  new_pass="$(prompt_password "Nueva contraseña: ")"
  update_user_password "$target" "$new_pass"
  echo "Contraseña actualizada para '$target'."
  pause
}

login() {
  if [[ -n "$LOGGED_IN_USER" ]]; then
    echo "Ya hay un usuario logueado: $LOGGED_IN_USER"
    pause
    return
  fi

  local username password stored_pass
  username="$(prompt_non_empty "Usuario: ")"
  read -rs -p "Contraseña: " password
  echo

  if ! user_exists "$username"; then
    echo "Usuario o contraseña incorrectos."
    pause
    return
  fi

  stored_pass="$(awk -F':' -v buscado="$username" '$1==buscado{print $2; exit}' "$USERS_FILE")"
  if [[ "$password" != "$stored_pass" ]]; then
    echo "Usuario o contraseña incorrectos."
    pause
    return
  fi

  LOGGED_IN_USER="$username"
  echo "Bienvenido, $LOGGED_IN_USER."
  pause
}

logout() {
  if [[ -z "$LOGGED_IN_USER" ]]; then
    echo "No hay sesión activa."
  else
    echo "Hasta luego, $LOGGED_IN_USER."
    LOGGED_IN_USER=""
  fi
  pause
}

show_types() {
  echo "Tipos disponibles:"
  for type in "${VALID_TYPES[@]}"; do
    echo "  - $type"
  done
}

prompt_type() {
  local input normalized type
  while true; do
    show_types
    read -r -p "Ingrese tipo: " input
    input="$(trim "$input")"
    normalized="${input,,}"
    for type in "${VALID_TYPES[@]}"; do
      if [[ "${type,,}" == "$normalized" ]]; then
        printf "%s\n" "$type"
        return 0
      fi
    done
    echo "Tipo inválido. Debe ser uno de la lista."
  done
}

prompt_positive_int() {
  local prompt="$1"
  local value
  while true; do
    read -r -p "$prompt" value
    value="$(trim "$value")"
    if [[ "$value" =~ ^[0-9]+$ ]] && ((10#$value > 0)); then
      printf "%d\n" "$value"
      return 0
    fi
    echo "Ingrese un número entero positivo."
  done
}

add_product() {
  if ! require_login; then
    return
  fi

  local type model description quantity price code
  type="$(prompt_type)"
  model="$(prompt_non_empty "Modelo: ")"
  description="$(prompt_non_empty "Descripción: ")"
  quantity="$(prompt_positive_int "Cantidad: ")"
  price="$(prompt_positive_int "Precio unitario ($ enteros): ")"
  code="$(printf "%s" "$type" | cut -c1-3 | tr '[:lower:]' '[:upper:]')"

  printf "%s|%s|%s|%s|%s|%s\n" "$code" "$type" "$model" "$description" "$quantity" "$price" >>"$PRODUCTS_FILE"
  echo "Producto agregado: $code - $type - $model - $description - $quantity - \$ $price"
  pause
}

list_products() {
  if [[ ! -s "$PRODUCTS_FILE" ]]; then
    echo "No hay productos cargados."
    return 1
  fi

  mapfile -t products <"$PRODUCTS_FILE"
  local idx=1
  for entry in "${products[@]}"; do
    IFS='|' read -r code type model description quantity price <<<"$entry"
    printf "%d) %s - %s - %s - Stock: %s - $ %s\n" "$idx" "$type" "$model" "$description" "$quantity" "$price"
    ((idx++))
  done
  return 0
}

sell_product() {
  if ! require_login; then
    return
  fi

  if [[ ! -s "$PRODUCTS_FILE" ]]; then
    echo "No hay productos disponibles."
    pause
    return
  fi

  mapfile -t products <"$PRODUCTS_FILE"
  declare -A selections=()

  while true; do
    echo "Catálogo:"
    local idx=1
    for entry in "${products[@]}"; do
      IFS='|' read -r code type model description quantity price <<<"$entry"
      printf "%d) %s - %s - $ %s (Stock: %s)\n" "$idx" "$type" "$model" "$price" "$quantity"
      ((idx++))
    done

    read -r -p "Seleccione número de producto (Enter para terminar): " choice
    choice="$(trim "$choice")"
    if [[ -z "$choice" ]]; then
      break
    fi

    if ! [[ "$choice" =~ ^[0-9]+$ ]]; then
      echo "Debe ingresar un número válido."
      continue
    fi

    local position=$((10#$choice))
    if (( position < 1 || position > ${#products[@]} )); then
      echo "Selección fuera de rango."
      continue
    fi

    local index=$((position - 1))
    IFS='|' read -r code type model description quantity price <<<"${products[$index]}"
    local taken=${selections[$index]:-0}
    local available=$((quantity - taken))
    if (( available <= 0 )); then
      echo "No queda stock disponible para ese producto."
      continue
    fi

    local amount
    while true; do
      read -r -p "Cantidad (máximo $available): " amount
      amount="$(trim "$amount")"
      if ! [[ "$amount" =~ ^[0-9]+$ ]]; then
        echo "Ingrese un número entero."
        continue
      fi
      amount=$((10#$amount))
      if (( amount <= 0 || amount > available )); then
        echo "Cantidad fuera de rango."
        continue
      fi
      break
    done

    selections[$index]=$((taken + amount))
    echo "Producto agregado al carrito."
  done

  if (( ${#selections[@]} == 0 )); then
    echo "No se realizaron compras."
    pause
    return
  fi

  local total=0
  printf "\nResumen de compra:\n"
  printf "%-12s %-20s %-10s %-10s\n" "Tipo" "Modelo" "Cantidad" "Subtotal"
  for i in "${!selections[@]}"; do
    IFS='|' read -r code type model description quantity price <<<"${products[$i]}"
    local count=${selections[$i]}
    local subtotal=$((count * price))
    total=$((total + subtotal))
    printf "%-12s %-20s %-10s $ %s\n" "$type" "$model" "$count" "$subtotal"
    quantity=$((quantity - count))
    products[$i]="$code|$type|$model|$description|$quantity|$price"
  done
  printf "TOTAL: $ %s\n" "$total"

  local tmp
  tmp="$(mktemp)"
  for entry in "${products[@]}"; do
    echo "$entry" >>"$tmp"
  done
  mv "$tmp" "$PRODUCTS_FILE"

  pause
}

filter_products() {
  local filter
  read -r -p "Filtrar por tipo (Enter para todos): " filter
  filter="$(trim "$filter")"
  local filter_lower="${filter,,}"

  if [[ ! -s "$PRODUCTS_FILE" ]]; then
    echo "No hay productos cargados."
    pause
    return
  fi

  local found=0
  while IFS='|' read -r code type model description quantity price; do
    [[ -z "$code" ]] && continue
    if [[ -n "$filter_lower" && "${type,,}" != "$filter_lower" ]]; then
      continue
    fi
    found=1
    echo "$code - $type - $model - $description - $quantity - \$ $price"
  done <"$PRODUCTS_FILE"

  if [[ "$found" -eq 0 ]]; then
    echo "No se encontraron productos para el filtro indicado."
  fi
  pause
}

csv_escape() {
  local value="$1"
  value="${value//\"/\"\"}"
  printf "\"%s\"" "$value"
}

generate_report() {
  if [[ ! -s "$PRODUCTS_FILE" ]]; then
    echo "No hay productos para generar el reporte."
    pause
    return
  fi

  mkdir -p "$DATOS_DIR"
  {
    echo "codigo,tipo,modelo,descripcion,cantidad,precio"
    while IFS='|' read -r code type model description quantity price; do
      [[ -z "$code" ]] && continue
      printf "%s,%s,%s,%s,%s,%s\n" \
        "$(csv_escape "$code")" \
        "$(csv_escape "$type")" \
        "$(csv_escape "$model")" \
        "$(csv_escape "$description")" \
        "$quantity" \
        "$price"
    done <"$PRODUCTS_FILE"
  } >"$DATOS_FILE"

  echo "Reporte generado en $DATOS_FILE"
  pause
}

main_menu() {
  while true; do
    clear
    echo "================= Inventario Citadel ================="
    if [[ -n "$LOGGED_IN_USER" ]]; then
      echo "Usuario actual: $LOGGED_IN_USER"
    else
      echo "Usuario actual: (ninguno)"
    fi
    cat <<'MENU'

1) Login
2) Logout
3) Crear usuario
4) Cambiar contraseña
5) Ingresar producto
6) Vender producto
7) Filtro de productos
8) Crear reporte CSV
9) Salir
MENU
    read -r -p "Seleccione opción: " option
    option="$(trim "$option")"
    case "$option" in
      1) login ;;
      2) logout ;;
      3) create_user ;;
      4) change_password ;;
      5) add_product ;;
      6) sell_product ;;
      7) filter_products ;;
      8) generate_report ;;
      9)
        echo "Saliendo del sistema."
        exit 0
        ;;
      *)
        echo "Opción inválida."
        pause
        ;;
    esac
  done
}

ensure_storage
main_menu
