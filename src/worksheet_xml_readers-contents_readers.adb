with Ada.Text_IO;
package body Worksheet_XML_Readers.Contents_Readers is

   SheetViews_Text    : constant String := "sheetViews";
   SheetFormatPr_Text : constant String := "sheetFormatPr";
   SheetData_Text     : constant String := "sheetData";
   Row_Text           : constant String := "row";
   C_Text             : constant String := "c";
   V_Text             : constant String := "v";

   procedure Start_Tag
     (This           : in out Contents_Reader;
      Tag_Name       : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Shall_Continue);
   begin
      case This.State is
         when Expect_Worksheet_Start_Tag =>
            if Tag_Name = "worksheet" then
               This.State := Expect_SheetPr_Start_Tag;
            else
               Call_Result.Initialize (-2114665078, -2095816943);
            end if;
         when Expect_SheetPr_Start_Tag =>
            if Tag_Name = "sheetPr" then
               This.State := Expect_SheetPr_End_Tag;
            elsif Tag_Name = Dimension_Text then
               This.State := Expect_Dimension_Ref_Attribute;
            else
               Call_Result.Initialize (-0461162131, 0763641059);
            end if;
         when Expect_Dimension_Start_Tag =>
            if Tag_Name = Dimension_Text then
               This.State := Expect_Dimension_Ref_Attribute;
            else
               Call_Result.Initialize (0902327768, 1144473764);
            end if;
         when Expect_SheetViews_Start_Tag =>
            if Tag_Name = SheetViews_Text then
               This.State := Expect_SheetViews_End_Tag;
            else
               Call_Result.Initialize (-1338727995, 1112034914);
            end if;
         when Expect_SheetViews_End_Tag |
              Expect_SheetFormatPr_End_Tag =>
            null;
         when Expect_SheetFormatPr_Start_Tag =>
            if Tag_Name = SheetFormatPr_Text then
               This.State := Expect_SheetFormatPr_End_Tag;
            else
               Call_Result.Initialize (-1795251690, -1574663782);
            end if;
         when Expect_SheetData_Start_Tag =>
            if Tag_Name = SheetData_Text then
               This.State := Expect_Row_Start_Tag;
            end if;
         when Expect_Row_Start_Tag =>
            if Tag_Name = Row_Text then
               This.State := Expect_Column_Start_Tag;
            else
               Call_Result.Initialize (0135296137, -1897912979);
            end if;
         when Expect_Column_Start_Tag =>
            if Tag_Name = C_Text then
               This.State := Expect_Column_R_Attribute;
            else
               Call_Result.Initialize (0302066935, -0921101321);
            end if;
         when Expect_V_Start_Tag =>
            if Tag_Name = V_Text then
               This.State := Expect_V_Text;
            else
               Call_Result.Initialize (-0257507946, -1666344813);
            end if;
         when Expect_Column_R_Attribute =>
            Call_Result.Initialize (1317210673, -1556553556);
         when Expect_Column_T_Attribute =>
            if Tag_Name = V_Text then
               This.Value := Unspecified;
               This.State := Expect_V_Text;
            else
               Call_Result.Initialize (2031878532, -1903162217);
            end if;
         when Expect_SheetPr_End_Tag =>
            null;
         when Expect_Dimension_Ref_Attribute |
              Expect_V_Text |
              Expect_V_End_Tag |
              Expect_Column_End_Tag |
              End_State =>
            Call_Result.Initialize (1765299954, 0818849365);
      end case;
   end Start_Tag;

   procedure End_Tag
     (This           : in out Contents_Reader;
      Tag_Name       : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Shall_Continue);
   begin
      case This.State is
         when Expect_SheetPr_End_Tag =>
            if Tag_Name = "sheetPr" then
               This.State := Expect_Dimension_Start_Tag;
            else
               null;
            end if;
         when Expect_SheetViews_End_Tag =>
            if Tag_Name = SheetViews_Text then
               This.State := Expect_SheetFormatPr_Start_Tag;
            end if;
         when Expect_SheetViews_Start_Tag =>
            if Tag_Name = Dimension_Text then
               null;
            else
               Call_Result.Initialize (1749988110, 0643376940);
            end if;
         when Expect_SheetFormatPr_End_Tag =>
            This.State := Expect_SheetData_Start_Tag;
         when Expect_SheetData_Start_Tag =>
            null;
         when Expect_Row_Start_Tag =>
            if Tag_Name = Row_Text then
               This.State := Expect_Column_Start_Tag;
            elsif Tag_Name = SheetData_Text then
               This.State := End_State;
               Shall_Continue := False;
            else
               Call_Result.Initialize (-1038622806, -1847854577);
            end if;
         when Expect_V_End_Tag =>
            if Tag_Name = V_Text then
               This.State := Expect_Column_End_Tag;
            else
               Call_Result.Initialize (-0377455210, 0262440448);
            end if;
         when Expect_Column_End_Tag =>
            if Tag_Name = C_Text then
               This.State := Expect_Column_Start_Tag;
            else
               Call_Result.Initialize (1351034769, -1499849680);
            end if;
         when Expect_Column_Start_Tag =>
            if Tag_Name = Row_Text then
               This.State := Expect_Row_Start_Tag;
            else
               Call_Result.Initialize (1637115202, 0332626527);
            end if;
         when Expect_Worksheet_Start_Tag |
              Expect_Dimension_Start_Tag |
              Expect_SheetPr_Start_Tag |
              Expect_Dimension_Ref_Attribute |
              Expect_SheetFormatPr_Start_Tag |
              Expect_Column_T_Attribute |
              Expect_Column_R_Attribute |
              Expect_V_Start_Tag |
              Expect_V_Text |
              End_State =>
            Call_Result.Initialize (-1871436272, 1308960720);
      end case;
   end End_Tag;

   procedure Text
     (This           : in out Contents_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result)
   is
      procedure Handle_V_Text is
      begin
         This.State := Expect_V_End_Tag;
         case This.Value is
            when Text =>
               This.Table (This.Current_Row, This.Current_Column)
                 := (Value => Text,
                     Key =>
                        Strings_Extraction.String_Id_To_String_Maps.Map_Key
                       (Int32'Value (Value) + 1));
            when Number =>
               This.Table (This.Current_Row, This.Current_Column)
                 := (Value => Unspecified,
                     Text  => new (This.Subpool) String'(Value));
            when Unspecified =>
               This.Table (This.Current_Row, This.Current_Column)
                 := (Value => Unspecified,
                     Text  => new (This.Subpool) String'(Value));
         end case;
      end Handle_V_Text;

      pragma Unreferenced (Shall_Continue);
   begin
      if Value'Length > 0 then
         case This.State is
            when Expect_SheetViews_End_Tag |
                 Expect_SheetFormatPr_End_Tag |
                 Expect_SheetData_Start_Tag |
                 Expect_Row_Start_Tag |
                 Expect_Column_Start_Tag |
                 Expect_Column_T_Attribute |
                 Expect_Column_R_Attribute |
                 Expect_V_Start_Tag |
                 Expect_V_End_Tag |
                 Expect_Column_End_Tag =>
               null;
            when Expect_V_Text =>
               Handle_V_Text;
            when Expect_Worksheet_Start_Tag |
                 Expect_SheetPr_Start_Tag |
                 Expect_SheetPr_End_Tag |
                 Expect_Dimension_Start_Tag |
                 Expect_Dimension_Ref_Attribute |
                 Expect_SheetViews_Start_Tag |
                 Expect_SheetFormatPr_Start_Tag |
                 End_State =>
               Call_Result.Initialize (1124022084, 1992910782);
         end case;
      end if;
   end Text;

   procedure Attribute
     (This            : in out Contents_Reader;
      Attribute_Name  : in     String;
      Attribute_Value : in     String;
      Shall_Continue  : in out Boolean;
      Call_Result     : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Shall_Continue);
   begin
      case This.State is
         when Expect_Worksheet_Start_Tag |
              Expect_SheetViews_Start_Tag |
              Expect_SheetFormatPr_Start_Tag =>
            Call_Result.Initialize (-1261918179, -1503098366);
         when Expect_SheetPr_Start_Tag |
              Expect_SheetPr_End_Tag |
              Expect_Dimension_Start_Tag |
              Expect_SheetViews_End_Tag |
              Expect_SheetFormatPr_End_Tag |
              Expect_Row_Start_Tag |
              Expect_Column_Start_Tag |
              Expect_V_Start_Tag |
              Expect_V_Text |
              Expect_V_End_Tag |
              Expect_Column_End_Tag |
              Expect_SheetData_Start_Tag =>
            null;
         when Expect_Column_T_Attribute =>
            if Attribute_Name = "t" then
               This.State := Expect_V_Start_Tag;
               if Attribute_Value = "n" then
                  This.Value := Number;
               elsif Attribute_Value = "s" then
                  This.Value := Text;
               else
                  Call_Result.Initialize (0087104666, 2042786229);
               end if;
            end if;
         when Expect_Column_R_Attribute =>
            if Attribute_Name = "r" then
               This.State := Expect_Column_T_Attribute;
               declare
                  C : constant Cell_Coordinate := Get_Cell (Attribute_Value);
               begin
                  if (not C.Is_Success) then
                     Call_Result.Initialize (-1644727362, -0864126531);
                  else
                     This.Current_Row := C.Row;
                     This.Current_Column := C.Column;
                  end if;
               end;
            end if;
         when Expect_Dimension_Ref_Attribute =>
            if Attribute_Name = "ref" then
               This.State := Expect_SheetViews_Start_Tag;
               --  Interpret_Attribute_Value;
               null;
            else
               Call_Result.Initialize (-1644256386, 1872626806);
            end if;
         when End_State                      =>
            Call_Result.Initialize (-1403673412, -0405787301);
      end case;
   end Attribute;

   procedure Comment
     (This           : in out Contents_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Value);
      pragma Unreferenced (Shall_Continue);
      pragma Unreferenced (Call_Result);
   begin
      case This.State is
         when Expect_Worksheet_Start_Tag |
              Expect_SheetPr_Start_Tag |
              Expect_SheetPr_End_Tag |
              Expect_Dimension_Start_Tag |
              Expect_Dimension_Ref_Attribute |
              Expect_SheetViews_Start_Tag |
              Expect_SheetViews_End_Tag |
              Expect_SheetFormatPr_Start_Tag |
              Expect_SheetFormatPr_End_Tag |
              Expect_SheetData_Start_Tag |
              Expect_Row_Start_Tag |
              Expect_Column_Start_Tag |
              Expect_Column_T_Attribute |
              Expect_Column_R_Attribute |
              Expect_V_Start_Tag |
              Expect_V_Text |
              Expect_V_End_Tag |
              Expect_Column_End_Tag |
              End_State =>
            null;
      end case;
   end Comment;

   procedure CDATA
     (This           : in out Contents_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Value, Shall_Continue, Call_Result);
   begin
      case This.State is
         when Expect_Worksheet_Start_Tag |
              Expect_SheetPr_Start_Tag |
              Expect_SheetPr_End_Tag |
              Expect_Dimension_Start_Tag |
              Expect_Dimension_Ref_Attribute |
              Expect_SheetViews_Start_Tag |
              Expect_SheetViews_End_Tag |
              Expect_SheetFormatPr_Start_Tag |
              Expect_SheetFormatPr_End_Tag |
              Expect_SheetData_Start_Tag |
              Expect_Row_Start_Tag |
              Expect_Column_Start_Tag |
              Expect_Column_T_Attribute |
              Expect_Column_R_Attribute |
              Expect_V_Start_Tag |
              Expect_V_Text |
              Expect_V_End_Tag |
              Expect_Column_End_Tag |
              End_State =>
            null;
      end case;
   end CDATA;

end Worksheet_XML_Readers.Contents_Readers;
