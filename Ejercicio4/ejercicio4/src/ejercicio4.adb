with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;

procedure Tambo is

   subtype Id_Vaca is Positive range 1 .. 100;

   package Random_Duracion is new Ada.Numerics.Float_Random;
   package Random_Eleccion is new Ada.Numerics.Discrete_Random (Boolean);

   Capacidad_Ordenie      : constant Integer:= 15;
   Capacidad_Vacunacion  : constant Integer := 5;
   Capacidad_Camion      : constant Integer := 50;

   function Etiqueta_Vaca (Id : Id_Vaca) return String is
   begin
      return Trim (Integer'Image (Integer (Id)), Ada.Strings.Left);
   end Etiqueta_Vaca;

   procedure Imprimir_Mensaje (Texto : String; Id : Id_Vaca) is
   begin
      Put_Line ("La vaca " & Etiqueta_Vaca (Id) & " " & Texto);
   end Imprimir_Mensaje;

   protected type Sala_Ordenie_Tipo is
      entry Entrar (Id : Id_Vaca);
      entry Salir  (Id : Id_Vaca);
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

   protected type Area_Vacunacion_Tipo is
      entry Entrar (Id : Id_Vaca);
      entry Salir  (Id : Id_Vaca);
   private
      Cantidad_Adentro : Natural := 0;
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

   protected type Gestion_Camiones_Tipo is
      entry Subir (Id : Id_Vaca);
   private
      Cant_Camion1 : Natural := 0;
      Cant_Camion2 : Natural := 0;
   end Gestion_Camiones_Tipo;

   protected body Gestion_Camiones_Tipo is

      entry Subir (Id : Id_Vaca)
        when Cant_Camion1 < Capacidad_Camion
          or else Cant_Camion2 < Capacidad_Camion
      is
      begin
         if Cant_Camion1 < Capacidad_Camion then
            Cant_Camion1 := Cant_Camion1 + 1;
            Imprimir_Mensaje ("está entrando al Camión 1", Id);
         else
            Cant_Camion2 := Cant_Camion2 + 1;
            Imprimir_Mensaje ("está entrando al Camión 2", Id);
         end if;
      end Subir;

   end Gestion_Camiones_Tipo;

   Sala_Ordenie   : Sala_Ordenie_Tipo;
   Area_Vacunacion : Area_Vacunacion_Tipo;
   Gestion_Camiones : Gestion_Camiones_Tipo;

   function Duracion_Aleatoria
     (Gen : in out Random_Duracion.Generator;
      Max_Segundos : Positive)
      return Duration
   is
   begin
      return Duration (Random_Duracion.Random (Gen) * Float (Max_Segundos));
   end Duracion_Aleatoria;

   procedure Ordeniar (Id : Id_Vaca; Gen : in out Random_Duracion.Generator) is
   begin
      Sala_Ordenie.Entrar (Id);
      delay Duracion_Aleatoria (Gen, 3);
      Sala_Ordenie.Salir (Id);
   end Ordeniar;

   procedure Vacunar (Id : Id_Vaca; Gen : in out Random_Duracion.Generator) is
   begin
      Area_Vacunacion.Entrar (Id);
      delay Duracion_Aleatoria (Gen, 2);
      Area_Vacunacion.Salir (Id);
   end Vacunar;

   task type Tarea_Vaca is
      entry Comenzar (Id : Id_Vaca);
   end Tarea_Vaca;

   task body Tarea_Vaca is
      Mi_Id            : Id_Vaca;
      Generador_Eleccion : Random_Eleccion.Generator;
      Generador_Duracion : Random_Duracion.Generator;
      Vacuna_Primero     : Boolean;
   begin
      accept Comenzar (Id : Id_Vaca) do
         Mi_Id := Id;
      end Comenzar;

      Random_Eleccion.Reset   (Generador_Eleccion, Integer (Mi_Id));
      Random_Duracion.Reset   (Generador_Duracion, Integer (Mi_Id) * 13 + 7);

      Vacuna_Primero := Random_Eleccion.Random (Generador_Eleccion);

      if Vacuna_Primero then
         Vacunar   (Mi_Id, Generador_Duracion);
         Ordeniar  (Mi_Id, Generador_Duracion);
      else
         Ordeniar  (Mi_Id, Generador_Duracion);
         Vacunar   (Mi_Id, Generador_Duracion);
      end if;

      Gestion_Camiones.Subir (Mi_Id);
   end Tarea_Vaca;

   Vacas : array (Id_Vaca) of Tarea_Vaca;

begin
   for I in Id_Vaca loop
      Vacas (I).Comenzar (I);
   end loop;

   null;
end Tambo;

