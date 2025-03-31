with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

procedure Main is
   -- Constants and Types
   Width  : constant Positive := 5;
   Height : constant Positive := 5;

   type Cell is (Dead, Alive) with Default_Value => Dead;
   type Rows is mod Height;
   type Cols is mod Width;
   type Board is array (Rows, Cols) of Cell;
   type Neighbors is range 0 .. 8;

   -- Helper function to reduce nesting in neighbor counting
   function Is_Alive(B : Board; R : Rows; C : Cols) return Boolean is
     (B(R, C) = Alive);

   -- Simplified Render procedure
   procedure Render_Board(B : Board) is
      use Ada.Characters.Latin_1;
   begin
      for Row in Rows loop
         Ada.Text_IO.Put_Line((for Col in Cols => (if B(Row, Col) = Alive then '#' else '.')));
      end loop;
   end Render_Board;

   -- Neighbor counting with reduced nesting
   function Count_Neighbors(B : Board; Row : Rows; Col : Cols) return Neighbors is
      Result : Neighbors := 0;
      Offsets : constant array (1 .. 8) of Integer := (-1, -1, -1, 0, -1, 1, 0, -1, 0, 1, 1, -1, 1, 0, 1, 1);
   begin
      for I in Offsets'Range loop
         if I mod 2 = 1 then  -- Start of a pair (row offset)
            declare
               R : constant Rows := Row + Rows(Offsets(I));
               C : constant Cols := Col + Cols(Offsets(I + 1));
            begin
               if Is_Alive(B, R, C) then
                  Result := Result + 1;
               end if;
            end;
         end if;
      end loop;
      return Result;
   end Count_Neighbors;

   -- Next generation with expression-based logic
   function Next(Current : Board) return Board is
      (for Row in Rows => 
         (for Col in Cols => 
            (case Current(Row, Col) is
               when Dead  => (if Count_Neighbors(Current, Row, Col) = 3 then Alive else Dead),
               when Alive => (if Count_Neighbors(Current, Row, Col) in 2 .. 3 then Alive else Dead))));

   -- Initial board setup
   Initial_Board : constant Board := (
      0 => (Dead, Alive, Dead, Dead, Dead),
      1 => (Dead, Alive, Alive, Dead, Dead),
      2 => (Alive, Alive, Alive, Dead, Dead),
      others => (others => Dead)
   );

   Current : Board := Initial_Board;

   -- ANSI escape sequences as constants
   Move_Up   : constant String := Ada.Characters.Latin_1.ESC & "[" & Ada.Strings.Fixed.Trim(Height'Image, Ada.Strings.Left) & "A";
   Move_Left : constant String := Ada.Characters.Latin_1.ESC & "[" & Ada.Strings.Fixed.Trim(Width'Image, Ada.Strings.Left) & "D";

begin
   -- Main simulation loop
   loop
      Render_Board(Current);
      Current := Next(Current);
      delay 0.25;
      Ada.Text_IO.Put(Move_Up & Move_Left);
   end loop;
end Main;