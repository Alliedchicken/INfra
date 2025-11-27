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

   package Random_Duracion renames Ada.Numerics.Float_Random;
   package Random_Eleccion is new Ada.Numerics.Discrete_Random (Boolean);

   Capacidad_Ordenie     : constant Natural := 15;
   Capacidad_Vacunacion  : constant Natural := 5;
   Capacidad_Camion      : constant Natural := 50;

   -- Tipos de evento para colorear la salida
   type Tipo_Evento is (Evento_Ordenie, Evento_Vacunacion, Evento_Camion1, Evento_Camion2);

   -------------------------------------------------
   --  COLORES PARA LA TERMINAL (ANSI)
   -------------------------------------------------
   Color_Reset      : constant String := ESC & "[0m";
   Color_Ordenie    : constant String := ESC & "[33m";   -- amarillo
   Color_Vacunacion : constant String := ESC & "[34m";   -- azul
   Color_Camion1    : constant String := ESC & "[32m";   -- verde
   Color_Camion2    : constant String := ESC & "[35m";   -- magenta
   Color_Final      : constant String := ESC & "[1;36m"; -- cian brillante

   -------------------------------------------------
   --  IMPRESION DE MENSAJES
   -------------------------------------------------
   function Etiqueta_Vaca (Id : Id_Vaca) return String is
   begin
      -- Convierte el Id a String sin espacios a la izquierda
      return Trim (Integer'Image (Integer (Id)), Ada.Strings.Left);
   end Etiqueta_Vaca;

   procedure Imprimir_Mensaje (Texto : String; Id : Id_Vaca; Evento : Tipo_Evento) is
      Color   : constant String :=
        (case Evento is
            when Evento_Ordenie      => Color_Ordenie,
            when Evento_Vacunacion   => Color_Vacunacion,
            when Evento_Camion1      => Color_Camion1,
            when Evento_Camion2      => Color_Camion2);

      Prefijo : constant String :=
        (case Evento is
            when Evento_Ordenie      => "[ORDENIE] ",
            when Evento_Vacunacion   => "[VACUNACION] ",
            when Evento_Camion1      => "[CAMION 1] ",
            when Evento_Camion2      => "[CAMION 2] ");
   begin
      Put_Line
        (Color &
         Prefijo &
         "La vaca " & Etiqueta_Vaca (Id) & " " & Texto &
         Color_Reset);
   end Imprimir_Mensaje;

   -------------------------------------------------
   --  SEMAFOROS (PROTECTED)
   -------------------------------------------------

   -- Semaforo binario clasico
   protected type Semaforo_Binario is
      entry P;    -- wait
      procedure V; -- signal
   private
      Libre : Boolean := True;
   end Semaforo_Binario;

   protected body Semaforo_Binario is
      entry P when Libre is
      begin
         Libre := False;
      end P;

      procedure V is
      begin
         Libre := True;
      end V;
   end Semaforo_Binario;


   -- Semaforo de conteo (para las 5 mangas)
   protected type Semaforo_Conteo (Max : Positive) is
      entry P;
      procedure V;
   private
      Valor : Integer := Max;
   end Semaforo_Conteo;

   protected body Semaforo_Conteo is
      entry P when Valor > 0 is
      begin
         Valor := Valor - 1;
      end P;

      procedure V is
      begin
         Valor := Valor + 1;
      end V;
   end Semaforo_Conteo;

   -- Instancias concretas:
   Sem_Pasillo : Semaforo_Binario;                          -- solo 1 vaca en el pasillo
   Sem_Mangas  : Semaforo_Conteo (Capacidad_Vacunacion);    -- max 5 vacas vacunandose

   -------------------------------------------------
   --  TASK SERVIDORAS
   -------------------------------------------------

   -- Sala de ordenie (monitor con capacidad 15)
   task Sala_Ordenie is
      entry Entrar (Id : Id_Vaca);
      entry Salir  (Id : Id_Vaca);
   end Sala_Ordenie;

   task body Sala_Ordenie is
      Ocupadas : Natural := 0;
   begin
      loop
         select
            -- Entrar a ordenie solo si hay lugar
            when Ocupadas < Capacidad_Ordenie =>
               accept Entrar (Id : Id_Vaca) do
                  Ocupadas := Ocupadas + 1;
                  Imprimir_Mensaje
                    ("esta entrando al area de ordenie", Id, Evento_Ordenie);
               end Entrar;

         or
            -- Salir solo si hay vacas adentro
            when Ocupadas > 0 =>
               accept Salir (Id : Id_Vaca) do
                  Imprimir_Mensaje
                    ("esta saliendo del area de ordenie", Id, Evento_Ordenie);
                  Ocupadas := Ocupadas - 1;
               end Salir;

         end select;
      end loop;
   end Sala_Ordenie;


   -- Gestion de camiones: llena primero el 1 y luego el 2
   task Gestion_Camiones is
      entry Subir (Id : Id_Vaca);
   end Gestion_Camiones;

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
                     Imprimir_Mensaje
                       ("esta entrando al Camion 1", Id, Evento_Camion1);
                  else
                     Cant_Camion2 := Cant_Camion2 + 1;
                     Imprimir_Mensaje
                       ("esta entrando al Camion 2", Id, Evento_Camion2);
                  end if;

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

   -------------------------------------------------
   --  FUNCION AUXILIAR PARA TIEMPOS RANDOM
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
   --  OPERACIONES: ORDENAR Y VACUNAR
   -------------------------------------------------

   -- Ordeñe: solo usa la task Sala_Ordenie
   procedure Ordenar (Id : Id_Vaca; Gen : in out Random_Duracion.Generator) is
   begin
      Sala_Ordenie.Entrar (Id);
      delay Duracion_Aleatoria (Gen, 3);  -- hasta 3 segundos
      Sala_Ordenie.Salir (Id);
   end Ordenar;

   -- Vacunacion: usa semaforo de mangas + semaforo binario de pasillo
   procedure Vacunar (Id : Id_Vaca; Gen : in out Random_Duracion.Generator) is
   begin
      -- 1) Espera a que haya una manga libre (max 5 vacas vacunandose)
      Sem_Mangas.P;

      -- 2) Usa el pasillo para ENTRAR
      Sem_Pasillo.P;
      Imprimir_Mensaje
        ("esta usando el PASILLO para ENTRAR al area de vacunacion",
         Id, Evento_Vacunacion);
      Imprimir_Mensaje
        ("esta entrando a una de las mangas del area de vacunacion",
         Id, Evento_Vacunacion);
      Sem_Pasillo.V;

      -- 3) Tiempo de vacunacion (hasta 2 segundos)
      delay Duracion_Aleatoria (Gen, 2);

      -- 4) Usa el pasillo para SALIR
      Sem_Pasillo.P;
      Imprimir_Mensaje
        ("esta saliendo de una manga del area de vacunacion",
         Id, Evento_Vacunacion);
      Imprimir_Mensaje
        ("esta usando el PASILLO para SALIR del area de vacunacion",
         Id, Evento_Vacunacion);
      Sem_Pasillo.V;

      -- 5) Libera la manga
      Sem_Mangas.V;
   end Vacunar;

   -------------------------------------------------
   --  CADA VACA ES UNA TAREA
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
      -- Recibe su numero de vaca
      accept Comenzar (Id : Id_Vaca) do
         Mi_Id := Id;
      end Comenzar;

      -- Random inicializados con la Id de la vaca como semilla
      Random_Eleccion.Reset (Generador_Eleccion, Integer (Mi_Id));
      Random_Duracion.Reset (Generador_Duracion, Integer (Mi_Id) * 13 + 7);

      -- Decide si se vacuna primero u ordena primero
      Vacuna_Primero := Random_Eleccion.Random (Generador_Eleccion);

      if Vacuna_Primero then
         Vacunar (Mi_Id, Generador_Duracion);
         Ordenar (Mi_Id, Generador_Duracion);
      else
         Ordenar (Mi_Id, Generador_Duracion);
         Vacunar (Mi_Id, Generador_Duracion);
      end if;

      -- Luego sube a alguno de los camiones
      Gestion_Camiones.Subir (Mi_Id);
   end Tarea_Vaca;

   -------------------------------------------------
   --  ARREGLO DE 100 VACAS (TAREAS)
   -------------------------------------------------
   Vacas : array (Id_Vaca) of Tarea_Vaca;

begin
   -------------------------------------------------
   --  PROGRAMA PRINCIPAL
   -------------------------------------------------
   for I in Id_Vaca loop
      Vacas (I).Comenzar (I);
   end loop;

   -- El main termina aqui; el programa sigue ejecutando
   -- hasta que todas las tareas (todas las vacas) finalicen.
   null;
end Tambo;
