with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;

procedure Tambo is

   -------------------------------------------------
   --  TIPOS Y CONSTANTES BÁSICAS
   -------------------------------------------------
   subtype Id_Vaca is Positive range 1 .. 100;

   -- Random para tiempos de espera (en segundos)
   package Random_Duracion is new Ada.Numerics.Float_Random;
   -- Random para elegir si la vaca se vacuna primero u ordeña primero
   package Random_Eleccion is new Ada.Numerics.Discrete_Random (Boolean);

   Capacidad_Ordenie      : constant Integer:= 15;
   Capacidad_Vacunacion  : constant Integer := 5;
   Capacidad_Camion      : constant Integer := 50;

   -------------------------------------------------
   --  UTILIDADES DE IMPRESIÓN
   -------------------------------------------------
   function Etiqueta_Vaca (Id : Id_Vaca) return String is
   begin
      -- Convierte el Id a String sin espacios a la izquierda
      return Trim (Integer'Image (Integer (Id)), Ada.Strings.Left);
   end Etiqueta_Vaca;

   procedure Imprimir_Mensaje (Texto : String; Id : Id_Vaca) is
   begin
      Put_Line ("La vaca " & Etiqueta_Vaca (Id) & " " & Texto);
   end Imprimir_Mensaje;

   -------------------------------------------------
   --  MONITOR: SALA DE ORDEÑE (CAPACIDAD 15)
   -------------------------------------------------
   protected type Sala_Ordenie_Tipo is
      entry Entrar (Id : Id_Vaca);  -- entra a la sala de ordeñe
      entry Salir  (Id : Id_Vaca);  -- sale de la sala de ordeñe
   private
      Ocupadas : Natural := 0;
   end Sala_Ordenie_Tipo;

   protected body Sala_Ordenie_Tipo is

      entry Entrar (Id : Id_Vaca)
        when Ocupadas < Capacidad_Ordenie is
      begin
         Ocupadas := Ocupadas + 1;
         Imprimir_Mensaje ("está entrando al área de ordeñe", Id);
      end Entrar;

      entry Salir (Id : Id_Vaca)
        when Ocupadas > 0 is
      begin
         Imprimir_Mensaje ("está saliendo al área de ordeñe", Id);
         Ocupadas := Ocupadas - 1;
      end Salir;

   end Sala_Ordenie_Tipo;

   -------------------------------------------------
   --  MONITOR: ÁREA DE VACUNACIÓN
   --  - Hasta 5 vacas en las mangas
   --  - El objeto protegido garantiza que solo una
   --    operación (Entrar/Salir) usa el “pasillo” a la vez
   -------------------------------------------------
   protected type Area_Vacunacion_Tipo is
      entry Entrar (Id : Id_Vaca);  -- entra al área de vacunación
      entry Salir  (Id : Id_Vaca);  -- sale del área de vacunación
   private
      Cantidad_Adentro : Natural := 0;  -- cuántas hay en las 5 mangas
   end Area_Vacunacion_Tipo;

   protected body Area_Vacunacion_Tipo is

      entry Entrar (Id : Id_Vaca)
        when Cantidad_Adentro < Capacidad_Vacunacion is
      begin
         Cantidad_Adentro := Cantidad_Adentro + 1;
         Imprimir_Mensaje ("está entrando al área de vacunación", Id);
      end Entrar;

      entry Salir (Id : Id_Vaca)
        when Cantidad_Adentro > 0 is
      begin
         Imprimir_Mensaje ("está saliendo al área de vacunación", Id);
         Cantidad_Adentro := Cantidad_Adentro - 1;
      end Salir;

   end Area_Vacunacion_Tipo;

   -------------------------------------------------
   --  MONITOR: CAMIONES (2 CAMIONES DE 50 VACAS)
   -------------------------------------------------
   protected type Gestion_Camiones_Tipo is
      entry Subir (Id : Id_Vaca);
   private
      Cant_Camion1 : Natural := 0;
      Cant_Camion2 : Natural := 0;
   end Gestion_Camiones_Tipo;

   protected body Gestion_Camiones_Tipo is

      entry Subir (Id : Id_Vaca)
        -- Mientras haya espacio en alguno de los camiones
        when Cant_Camion1 < Capacidad_Camion
          or else Cant_Camion2 < Capacidad_Camion
      is
      begin
         -- Para simplificar: llenamos primero el camión 1 y luego el camión 2
         if Cant_Camion1 < Capacidad_Camion then
            Cant_Camion1 := Cant_Camion1 + 1;
            Imprimir_Mensaje ("está entrando al Camión 1", Id);
         else
            Cant_Camion2 := Cant_Camion2 + 1;
            Imprimir_Mensaje ("está entrando al Camión 2", Id);
         end if;
      end Subir;

   end Gestion_Camiones_Tipo;

   -------------------------------------------------
   --  INSTANCIAS DE LOS MONITORES
   -------------------------------------------------
   Sala_Ordenie   : Sala_Ordenie_Tipo;
   Area_Vacunacion : Area_Vacunacion_Tipo;
   Gestion_Camiones : Gestion_Camiones_Tipo;

   -------------------------------------------------
   --  FUNCIÓN AUXILIAR PARA TIEMPOS RANDOM (0 .. Max_Segundos)
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
   --  OPERACIONES DE UNA VACA: ORDEÑAR Y VACUNAR
   -------------------------------------------------
   procedure Ordeniar (Id : Id_Vaca; Gen : in out Random_Duracion.Generator) is
   begin
      Sala_Ordenie.Entrar (Id);
      delay Duracion_Aleatoria (Gen, 3);  -- hasta 3 segundos ordeñándose
      Sala_Ordenie.Salir (Id);
   end Ordeniar;

   procedure Vacunar (Id : Id_Vaca; Gen : in out Random_Duracion.Generator) is
   begin
      Area_Vacunacion.Entrar (Id);
      delay Duracion_Aleatoria (Gen, 2);  -- hasta 2 segundos vacunándose
      Area_Vacunacion.Salir (Id);
   end Vacunar;

   -------------------------------------------------
   --  CADA VACA ES UNA TAREA (TASK)
   -------------------------------------------------
   task type Tarea_Vaca is
      entry Comenzar (Id : Id_Vaca);
   end Tarea_Vaca;

   task body Tarea_Vaca is
      Mi_Id            : Id_Vaca;
      Generador_Eleccion : Random_Eleccion.Generator;
      Generador_Duracion : Random_Duracion.Generator;
      Vacuna_Primero     : Boolean;
   begin
      -- La vaca espera a que le pasen su número
      accept Comenzar (Id : Id_Vaca) do
         Mi_Id := Id;
      end Comenzar;

      -- Inicializamos generadores de random usando el Id como semilla
      Random_Eleccion.Reset   (Generador_Eleccion, Integer (Mi_Id));
      Random_Duracion.Reset   (Generador_Duracion, Integer (Mi_Id) * 13 + 7);

      -- Decide aleatoriamente si se vacuna primero u ordeña primero
      Vacuna_Primero := Random_Eleccion.Random (Generador_Eleccion);

      if Vacuna_Primero then
         Vacunar   (Mi_Id, Generador_Duracion);
         Ordeniar  (Mi_Id, Generador_Duracion);
      else
         Ordeniar  (Mi_Id, Generador_Duracion);
         Vacunar   (Mi_Id, Generador_Duracion);
      end if;

      -- Luego sube a algún camión
      Gestion_Camiones.Subir (Mi_Id);
   end Tarea_Vaca;

   -------------------------------------------------
   --  ARREGLO DE 100 VACAS (TAREAS)
   -------------------------------------------------
   Vacas : array (Id_Vaca) of Tarea_Vaca;

begin
   -------------------------------------------------
   --  PROGRAMA PRINCIPAL: LANZA LAS 100 VACAS
   -------------------------------------------------
   for I in Id_Vaca loop
      Vacas (I).Comenzar (I);
   end loop;

   -- El main termina aquí; el programa sigue ejecutando
   -- hasta que todas las tareas (todas las vacas) finalicen.
   null;
end Tambo;
