with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Characters.Latin_1;    use Ada.Characters.Latin_1;

procedure Tambo is

   -------------------------------------------------
   -- TIPOS Y CONSTANTES
   -------------------------------------------------
   subtype Id_Vaca is Positive range 1 .. 100;

   package Random_Duracion renames Ada.Numerics.Float_Random;
   package Random_Eleccion is new Ada.Numerics.Discrete_Random (Boolean);

   Capacidad_Ordenie     : constant Natural := 15;
   Capacidad_Vacunacion  : constant Natural := 5;
   Capacidad_Camion      : constant Natural := 50;

   -------------------------------------------------
   -- COLORES
   -------------------------------------------------
   Color_Reset      : constant String := ESC & "[0m";
   Color_Ordenie    : constant String := ESC & "[33m";
   Color_Vacunacion : constant String := ESC & "[34m";
   Color_Camion1    : constant String := ESC & "[32m";
   Color_Camion2    : constant String := ESC & "[35m";
   Color_Final      : constant String := ESC & "[1;36m";

   type Tipo_Evento is
     (Evento_Ordenie, Evento_Vacunacion, Evento_Camion1, Evento_Camion2);

   -------------------------------------------------
   -- IMPRESIÓN
   -------------------------------------------------
   function Etiqueta_Vaca (Id : Id_Vaca) return String is
   begin
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
      Put_Line (Color & Prefijo &
                 "La vaca " & Etiqueta_Vaca(Id) & " " & Texto &
                 Color_Reset);
   end Imprimir_Mensaje;

   -------------------------------------------------------------
   -- SEMÁFORO BINARIO IMPLEMENTADO COMO TASK
   -------------------------------------------------------------
   task type Semaforo_Binario is
      entry P;
      entry V;
   end Semaforo_Binario;

   task body Semaforo_Binario is
      Libre : Boolean := True;
   begin
      loop
         select
            when Libre =>
               accept P do
                  Libre := False;
               end P;

         or
            accept V do
               Libre := True;
            end V;

         end select;
      end loop;
   end Semaforo_Binario;

   -------------------------------------------------------------
   -- SEMÁFORO DE CONTEO (Para las 5 mangas)
   -------------------------------------------------------------
   task type Semaforo_Conteo (Max : Positive) is
      entry P;
      entry V;
   end Semaforo_Conteo;

   task body Semaforo_Conteo is
      Valor : Integer := Max;
   begin
      loop
         select
            when Valor > 0 =>
               accept P do
                  Valor := Valor - 1;
               end P;

         or
            accept V do
               Valor := Valor + 1;
            end V;

         end select;
      end loop;
   end Semaforo_Conteo;

   -- Instancias concretas
   Sem_Pasillo : Semaforo_Binario;
   Sem_Mangas  : Semaforo_Conteo (Capacidad_Vacunacion);

   -------------------------------------------------
   -- TASK: SALA DE ORDENIE
   -------------------------------------------------
   task Sala_Ordenie is
      entry Entrar (Id : Id_Vaca);
      entry Salir  (Id : Id_Vaca);
   end Sala_Ordenie;

   task body Sala_Ordenie is
      Ocupadas : Natural := 0;
   begin
      loop
         select
            when Ocupadas < Capacidad_Ordenie =>
               accept Entrar (Id : Id_Vaca) do
                  Ocupadas := Ocupadas + 1;
                  Imprimir_Mensaje("esta entrando al area de ordenie",
                                   Id, Evento_Ordenie);
               end Entrar;

         or
            when Ocupadas > 0 =>
               accept Salir (Id : Id_Vaca) do
                  Ocupadas := Ocupadas - 1;
                  Imprimir_Mensaje("esta saliendo del area de ordenie",
                                   Id, Evento_Ordenie);
               end Salir;

         end select;
      end loop;
   end Sala_Ordenie;


   -------------------------------------------------
   -- TASK: GESTIÓN DE CAMIONES
   -------------------------------------------------
   task Gestion_Camiones is
      entry Subir (Id : Id_Vaca);
   end Gestion_Camiones;

   task body Gestion_Camiones is
      Cant_Cam1 : Natural := 0;
      Cant_Cam2 : Natural := 0;
   begin
      loop
         select
            when Cant_Cam1 < Capacidad_Camion
              or else Cant_Cam2 < Capacidad_Camion =>
               accept Subir (Id : Id_Vaca) do

                  if Cant_Cam1 < Capacidad_Camion then
                     Cant_Cam1 := Cant_Cam1 + 1;
                     Imprimir_Mensaje(
                       "esta entrando al Camion 1", Id, Evento_Camion1);
                  else
                     Cant_Cam2 := Cant_Cam2 + 1;
                     Imprimir_Mensaje(
                       "esta entrando al Camion 2", Id, Evento_Camion2);
                  end if;

                  if Cant_Cam1 = Capacidad_Camion
                    and then Cant_Cam2 = Capacidad_Camion
                  then
                     Put_Line(Color_Final &
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
   -- TIEMPO RANDOM
   -------------------------------------------------
   function Duracion_Aleatoria
     (Gen : in out Random_Duracion.Generator;
      Max_Segundos : Positive)
      return Duration
   is
   begin
      return Duration(Random_Duracion.Random(Gen) * Float(Max_Segundos));
   end Duracion_Aleatoria;

   -------------------------------------------------
   -- ORDENAR & VACUNAR
   -------------------------------------------------
   procedure Ordenar (Id : Id_Vaca; Gen : in out Random_Duracion.Generator) is
   begin
      Sala_Ordenie.Entrar(Id);
      delay Duracion_Aleatoria(Gen, 3);
      Sala_Ordenie.Salir(Id);
   end Ordenar;

   procedure Vacunar (Id : Id_Vaca; Gen : in out Random_Duracion.Generator) is
   begin
      -- 1) Esperar manga disponible
      Sem_Mangas.P;

      -- 2) PASILLO: ENTRAR
      Sem_Pasillo.P;
      Imprimir_Mensaje("esta usando el PASILLO para ENTRAR al area de vacunacion",
                       Id, Evento_Vacunacion);
      Sem_Pasillo.V;

      Imprimir_Mensaje("esta entrando a una de las mangas del area de vacunacion",
                       Id, Evento_Vacunacion);

      -- 3) Tiempo de vacunación
      delay Duracion_Aleatoria(Gen, 2);

      -- 4) PASILLO: SALIR
      Sem_Pasillo.P;
      Imprimir_Mensaje("esta usando el PASILLO para SALIR del area de vacunacion",
                       Id, Evento_Vacunacion);
      Sem_Pasillo.V;

      Imprimir_Mensaje("esta saliendo de una manga del area de vacunacion",
                       Id, Evento_Vacunacion);

      -- 5) Liberar manga
      Sem_Mangas.V;
   end Vacunar;

   -------------------------------------------------
   -- TAREA DE CADA VACA
   -------------------------------------------------
   task type Tarea_Vaca is
      entry Comenzar (Id : Id_Vaca);
   end Tarea_Vaca;

   task body Tarea_Vaca is
      Mi_Id  : Id_Vaca;
      GenE   : Random_Eleccion.Generator;
      GenDur : Random_Duracion.Generator;
      Vacuna_Primero : Boolean;
   begin
      accept Comenzar (Id : Id_Vaca) do
         Mi_Id := Id;
      end Comenzar;

      Random_Eleccion.Reset(GenE, Integer(Mi_Id));
      Random_Duracion.Reset(GenDur, Integer(Mi_Id)*13 + 7);

      Vacuna_Primero := Random_Eleccion.Random(GenE);

      if Vacuna_Primero then
         Vacunar(Mi_Id, GenDur);
         Ordenar(Mi_Id, GenDur);
      else
         Ordenar(Mi_Id, GenDur);
         Vacunar(Mi_Id, GenDur);
      end if;

      Gestion_Camiones.Subir(Mi_Id);
   end Tarea_Vaca;

   Vacas : array (Id_Vaca) of Tarea_Vaca;

begin
   for I in Id_Vaca loop
      Vacas(I).Comenzar(I);
   end loop;

   null;
end Tambo;
