with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Characters.Latin_1;    use Ada.Characters.Latin_1;

procedure Tambo is

   -------------------------------------------------
   --  TIPOS Y CONSTANTES BASICAS
   -------------------------------------------------
   subtype Id_Vaca is Positive range 1 .. 100;

   -- Random para tiempos de espera (en segundos)
   package Random_Duracion renames Ada.Numerics.Float_Random;
   -- Random para elegir si la vaca se vacuna primero u ordena primero
   package Random_Eleccion is new Ada.Numerics.Discrete_Random (Boolean);

   Capacidad_Ordenie      : constant Natural := 15;
   Capacidad_Vacunacion  : constant Natural := 5;
   Capacidad_Camion      : constant Natural := 50;

   -- Tipos de evento para colorear la salida
   type Tipo_Evento is (Evento_Ordenie, Evento_Vacunacion, Evento_Camion1, Evento_Camion2);

   -------------------------------------------------
   --  UTILIDADES DE IMPRESION (CON COLORES)
   -------------------------------------------------
   -- Codigos ANSI de color
   Color_Reset      : constant String := ESC & "[0m";
   Color_Ordenie     : constant String := ESC & "[33m";  -- amarillo
   Color_Vacunacion : constant String := ESC & "[34m";  -- azul
   Color_Camion1    : constant String := ESC & "[32m";  -- verde
   Color_Camion2    : constant String := ESC & "[35m";  -- magenta
   Color_Final      : constant String := ESC & "[1;36m"; -- cian brillante

   function Etiqueta_Vaca (Id : Id_Vaca) return String is
   begin
      -- Convierte el Id a String sin espacios a la izquierda
      return Trim (Integer'Image (Integer (Id)), Ada.Strings.Left);
   end Etiqueta_Vaca;

   procedure Imprimir_Mensaje (Texto : String; Id : Id_Vaca; Evento : Tipo_Evento) is
      Color   : constant String :=
        (case Evento is
            when Evento_Ordenie      => Color_Ordenie,
            when Evento_Vacunacion  => Color_Vacunacion,
            when Evento_Camion1     => Color_Camion1,
            when Evento_Camion2     => Color_Camion2);

      Prefijo : constant String :=
        (case Evento is
            when Evento_Ordenie      => "[ORDENIE] ",
            when Evento_Vacunacion  => "[VACUNACION] ",
            when Evento_Camion1     => "[CAMION 1] ",
            when Evento_Camion2     => "[CAMION 2] ");
   begin
      Put_Line
        (Color &
         Prefijo &
         "La vaca " & Etiqueta_Vaca (Id) & " " & Texto &
         Color_Reset);
   end Imprimir_Mensaje;

   -------------------------------------------------
   --  SERVIDOR: SALA DE ORDENIE (CAPACIDAD 15)
   -------------------------------------------------
   task Sala_Ordenie is
      entry Entrar (Id : Id_Vaca);  -- entra a la sala de ordenie
      entry Salir  (Id : Id_Vaca);  -- sale de la sala de ordenie
   end Sala_Ordenie;

   -------------------------------------------------
   --  SERVIDOR: AREA DE VACUNACION
   --  - Hasta 5 vacas en las mangas
   --  - La task garantiza que solo una operacion
   --    (Entrar/Salir) usa el "pasillo" a la vez
   -------------------------------------------------
   task Area_Vacunacion is
      entry Entrar (Id : Id_Vaca);  -- entra al area de vacunacion
      entry Salir  (Id : Id_Vaca);  -- sale del area de vacunacion
   end Area_Vacunacion;

   -------------------------------------------------
   --  SERVIDOR: CAMIONES (2 CAMIONES DE 50 VACAS)
   -------------------------------------------------
   task Gestion_Camiones is
      entry Subir (Id : Id_Vaca);
   end Gestion_Camiones;

   -------------------------------------------------
   --  FUNCION AUXILIAR PARA TIEMPOS RANDOM (0 .. Max_Segundos)
   -------------------------------------------------
   function Duracion_Aleatoria
     (Gen : in out Random_Duracion.Generator;
      Max_Segundos : Positive)
      return Duration
   is
   begin
      return Duration (Random_Duracion.Random (Gen) * Float (Max_Segundos));
   end Duracion_Aleatoria;

   -------------------------------------------------
   --  OPERACIONES DE UNA VACA: ORDENAR Y VACUNAR
   -------------------------------------------------
   procedure Ordenar (Id : Id_Vaca; Gen : in out Random_Duracion.Generator) is
   begin
      Sala_Ordenie.Entrar (Id);
      delay Duracion_Aleatoria (Gen, 3);  -- hasta 3 segundos ordenandose
      Sala_Ordenie.Salir (Id);
   end Ordenar;

   procedure Vacunar (Id : Id_Vaca; Gen : in out Random_Duracion.Generator) is
   begin
      Area_Vacunacion.Entrar (Id);
      delay Duracion_Aleatoria (Gen, 2);  -- hasta 2 segundos vacunandose
      Area_Vacunacion.Salir (Id);
   end Vacunar;

   -------------------------------------------------
   --  CADA VACA ES UNA TAREA (TASK)
   -------------------------------------------------
   task type Tarea_Vaca is
      entry Comenzar (Id : Id_Vaca);
   end Tarea_Vaca;

   task body Tarea_Vaca is
      Mi_Id              : Id_Vaca;
      Generador_Eleccion : Random_Eleccion.Generator;
      Generador_Duracion : Random_Duracion.Generator;
      Vacuna_Primero     : Boolean;
   begin
      -- La vaca espera a que le pasen su numero
      accept Comenzar (Id : Id_Vaca) do
         Mi_Id := Id;
      end Comenzar;

      -- Inicializamos generadores de random usando el Id como semilla
      Random_Eleccion.Reset (Generador_Eleccion, Integer (Mi_Id));
      Random_Duracion.Reset (Generador_Duracion, Integer (Mi_Id) * 13 + 7);

      -- Decide aleatoriamente si se vacuna primero u ordena primero
      Vacuna_Primero := Random_Eleccion.Random (Generador_Eleccion);

      if Vacuna_Primero then
         Vacunar (Mi_Id, Generador_Duracion);
         Ordenar (Mi_Id, Generador_Duracion);
      else
         Ordenar (Mi_Id, Generador_Duracion);
         Vacunar (Mi_Id, Generador_Duracion);
      end if;

      -- Luego sube a algun camion
      Gestion_Camiones.Subir (Mi_Id);
   end Tarea_Vaca;

   -------------------------------------------------
   --  ARREGLO DE 100 VACAS (TAREAS)
   -------------------------------------------------
   Vacas : array (Id_Vaca) of Tarea_Vaca;

   -------------------------------------------------
   --  CUERPOS DE LAS TASK SERVIDORAS
   -------------------------------------------------

   -- Sala de ordenie: como un "monitor" con capacidad 15
   task body Sala_Ordenie is
      Ocupadas : Natural := 0;
   begin
      loop
         select
            -- Dejo entrar solo si hay lugar
            when Ocupadas < Capacidad_Ordenie =>
               accept Entrar (Id : Id_Vaca) do
                  Ocupadas := Ocupadas + 1;
                  Imprimir_Mensaje ("esta entrando al area de ordenie", Id, Evento_Ordenie);
               end Entrar;

         or
            -- Dejo salir solo si hay alguna vaca adentro
            when Ocupadas > 0 =>
               accept Salir (Id : Id_Vaca) do
                  Imprimir_Mensaje ("esta saliendo del area de ordenie", Id, Evento_Ordenie);
                  Ocupadas := Ocupadas - 1;
               end Salir;

         end select;
      end loop;
   end Sala_Ordenie;

   -- Area de vacunacion:
   --   * Cantidad_Adentro <= 5
   --   * La task garantiza un unico "pasillo" para entrar/salir
   task body Area_Vacunacion is
      Cantidad_Adentro : Natural := 0;
   begin
      loop
         select
            -- Entrar: hay lugar en alguna de las 5 mangas
            when Cantidad_Adentro < Capacidad_Vacunacion =>
               accept Entrar (Id : Id_Vaca) do
                  Cantidad_Adentro := Cantidad_Adentro + 1;
                  Imprimir_Mensaje ("esta entrando al area de vacunacion", Id, Evento_Vacunacion);
               end Entrar;

         or
            -- Salir: solo si hay vacas adentro
            when Cantidad_Adentro > 0 =>
               accept Salir (Id : Id_Vaca) do
                  Imprimir_Mensaje ("esta saliendo del area de vacunacion", Id, Evento_Vacunacion);
                  Cantidad_Adentro := Cantidad_Adentro - 1;
               end Salir;

         end select;
      end loop;
   end Area_Vacunacion;

   -- Gestion de camiones:
   -- Se llenan primero el camion 1 y luego el camion 2
   task body Gestion_Camiones is
      Cant_Camion1 : Natural := 0;
      Cant_Camion2 : Natural := 0;
   begin
      loop
         select
            when Cant_Camion1 < Capacidad_Camion
              or else Cant_Camion2 < Capacidad_Camion =>
               accept Subir (Id : Id_Vaca) do
                  if Cant_Camion1 < Capacidad_Camion then
                     Cant_Camion1 := Cant_Camion1 + 1;
                     Imprimir_Mensaje ("esta entrando al Camion 1", Id, Evento_Camion1);
                  else
                     Cant_Camion2 := Cant_Camion2 + 1;
                     Imprimir_Mensaje ("esta entrando al Camion 2", Id, Evento_Camion2);
                  end if;

                  -- Si ambos camiones estan llenos, aviso fin de simulacion
                  if Cant_Camion1 = Capacidad_Camion
                    and then Cant_Camion2 = Capacidad_Camion
                  then
                     Put_Line
                       (Color_Final &
                        ">>> Ambos camiones estan llenos. Fin de la simulacion. <<<" &
                        Color_Reset);
                  end if;
               end Subir;

         or
            terminate;
         end select;
      end loop;
   end Gestion_Camiones;

begin
   -------------------------------------------------
   --  PROGRAMA PRINCIPAL: LANZA LAS 100 VACAS
   -------------------------------------------------
   for I in Id_Vaca loop
      Vacas (I).Comenzar (I);
   end loop;

   -- El main termina aqui; el programa sigue ejecutando
   -- hasta que todas las tareas (todas las vacas) finalicen.
   null;
end Tambo;
