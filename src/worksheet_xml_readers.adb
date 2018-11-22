with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Worksheet_XML_Readers is

   function Get_Cell (Text : String) return Cell_Coordinate
   is
      function Convert (C : Character) return Int32 is
        (Character'Pos (C) - Character'Pos ('A') + 1);

      Cell : Cell_Coordinate := (False, 1, 1);
      First_Digit : Pos32;
      Has_Found_First_Digit : Boolean;
      Factor : Pos32 := 1;
      Column : Nat32 := 0;
      Is_Success : Boolean := True;
   begin
      for I in Text'Range loop
         if Ada.Characters.Handling.Is_Digit (Text (I)) then
            First_Digit := I;
            Has_Found_First_Digit := True;
            exit;
         end if;
      end loop;

      if Has_Found_First_Digit then
         for I in First_Digit .. Text'Last loop
            if
              (not Ada.Characters.Handling.Is_Digit (Text (I)))
            then
               Is_Success := False;
               exit;
            end if;
         end loop;

         if Is_Success then
            for I in reverse Positive range
              Text'First .. First_Digit - 1
            loop
               Column
                 := Column + Factor * Convert (Text (I));
               Factor
                 := Factor * (Character'Pos ('Z') - Character'Pos ('A') + 1);
            end loop;
            Cell := (True,
                     Pos32'Value (Text (First_Digit .. Text'Last)),
                     Column);
         end if;
      end if;

      return Cell;
   end Get_Cell;

   procedure Start_Tag
     (This           : in out Statistics_Reader;
      Tag_Name       : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Shall_Continue);
   begin
      case This.My_State is
         when Expect_Worksheet_Start_Tag => null;
            if Tag_Name = "worksheet" then
               This.My_State := Expect_SheetPr_Start_Tag;
            else
               Call_Result.Initialize (-0076459602, 1543164932);
            end if;
         when Expect_SheetPr_Start_Tag =>
            if Tag_Name = "sheetPr" then
               This.My_State := Expect_SheetPr_End_Tag;
            elsif Tag_Name = Dimension_Text then
               This.My_State := Expect_Dimension_Ref_Attribute;
            else
               Call_Result.Initialize (0045126608, 0848077892);
            end if;
         when Expect_Dimension_Start_Tag =>
            if Tag_Name = Dimension_Text then
               This.My_State := Expect_Dimension_Ref_Attribute;
            else
               Call_Result.Initialize (-0073414098, -0606251235);
            end if;
         when Expect_SheetPr_End_Tag =>
            null;  --  Ignore evrything until end sheetPr end tag
         when Expect_Dimension_Ref_Attribute |
              End_State =>
            Call_Result.Initialize (-1396168071, -1384387549);
      end case;
   end Start_Tag;

   procedure End_Tag
     (This           : in out Statistics_Reader;
      Tag_Name       : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Shall_Continue);
   begin
      case This.My_State is
         when Expect_SheetPr_End_Tag =>
            if Tag_Name = "sheetPr" then
               This.My_State := Expect_Dimension_Start_Tag;
            else
               null;
            end if;
         when Expect_Worksheet_Start_Tag |
              Expect_Dimension_Start_Tag |
              Expect_SheetPr_Start_Tag |
              Expect_Dimension_Ref_Attribute |
              End_State =>
            Call_Result.Initialize (-1056600633, -0373570022);
      end case;
   end End_Tag;

   procedure Text
     (This           : in out Statistics_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Shall_Continue);
   begin
      if Value'Length > 0 then
         case This.My_State is
         when Expect_Worksheet_Start_Tag |
              Expect_SheetPr_Start_Tag |
              Expect_SheetPr_End_Tag |
              Expect_Dimension_Start_Tag |
              Expect_Dimension_Ref_Attribute |
              End_State =>
            Call_Result.Initialize (0349926683, -1332837350);
         end case;
      end if;
   end Text;

   procedure Attribute
     (This            : in out Statistics_Reader;
      Attribute_Name  : in     String;
      Attribute_Value : in     String;
      Shall_Continue  : in out Boolean;
      Call_Result     : in out Subprogram_Call_Result)
   is
      procedure Interpret_Attribute_Value is
         Index : constant Nat32 := Ada.Strings.Fixed.Index
           (Source  => Attribute_Value,
            Pattern => ":");
      begin
         if Index > 0 then
            declare
               S1 : constant String
                 := Attribute_Value (Attribute_Value'First .. Index - 1);
               S2 : constant String
                 := Attribute_Value (Index + 1 .. Attribute_Value'Last);

               Upper_Left_Cell  : constant Cell_Coordinate := Get_Cell (S1);
               Lower_Right_Cell : constant Cell_Coordinate := Get_Cell (S2);
            begin
               if
                 Upper_Left_Cell.Is_Success
                 and Lower_Right_Cell.Is_Success
               then
                  This.My_Row_Count
                    := Lower_Right_Cell.Row - Upper_Left_Cell.Row + 1;
                  This.My_Column_Count
                    := Lower_Right_Cell.Column - Upper_Left_Cell.Column + 1;
               else
                  Call_Result.Initialize (-1693026470, 1608009843);
               end if;
            end;
         else
            Call_Result.Initialize (1602079378, -0494550097);
         end if;
         Shall_Continue := False;
      end Interpret_Attribute_Value;

   begin
      case This.My_State is
         when Expect_Worksheet_Start_Tag =>
            Call_Result.Initialize (1351396437, -0422784018);
         when Expect_SheetPr_Start_Tag       => null;
         when Expect_SheetPr_End_Tag         => null;
         when Expect_Dimension_Start_Tag     => null;
         when Expect_Dimension_Ref_Attribute =>
            if Attribute_Name = "ref" then
               Interpret_Attribute_Value;
            else
               Call_Result.Initialize (-1618035129, -0121743839);
            end if;
         when End_State                      =>
            Call_Result.Initialize (0576022333, -1670174807);
      end case;
   end Attribute;

   procedure Comment
     (This           : in out Statistics_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Value);
      pragma Unreferenced (Shall_Continue);
      pragma Unreferenced (Call_Result);
   begin
      case This.My_State is
         when Expect_Worksheet_Start_Tag |
              Expect_SheetPr_Start_Tag |
              Expect_SheetPr_End_Tag |
              Expect_Dimension_Start_Tag |
              Expect_Dimension_Ref_Attribute |
              End_State =>
            null;
      end case;
   end Comment;

   procedure CDATA
     (This           : in out Statistics_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Value, Shall_Continue, Call_Result);
   begin
      case This.My_State is
         when Expect_Worksheet_Start_Tag |
              Expect_SheetPr_Start_Tag |
              Expect_SheetPr_End_Tag |
              Expect_Dimension_Start_Tag |
              Expect_Dimension_Ref_Attribute |
              End_State =>
            null;
      end case;
   end CDATA;

end Worksheet_XML_Readers;
