with Standard_Extensions; use Standard_Extensions;
with Dynamic_Pools;
with Shared_Strings_XML_Readers;

generic
   Max_Row_Count    : Pos32;
   Max_Column_Count : Pos32;
   with package Strings_Extraction is
     new Shared_Strings_XML_Readers.Strings_Extraction (<>);
package Worksheet_XML_Readers.Contents_Readers is

   subtype Row_Index    is Pos32 range 1 .. Max_Row_Count;
   subtype Column_Index is Pos32 range 1 .. Max_Column_Count;

   type Contents_Reader
     (
      Subpool : Dynamic_Pools.Subpool_Handle;
      Id_To_String_Map : not null access constant
        Strings_Extraction.String_Id_To_String_Maps.Map
     ) is
     new XML_Stoppable_SAX_Parser.SAX_Parser with private;

   function Cell_Contents
     (This   : Contents_Reader;
      Row    : Row_Index;
      Column : Column_Index) return String;

   overriding
   procedure Start_Tag
     (This           : in out Contents_Reader;
      Tag_Name       : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result);

   overriding
   procedure End_Tag
     (This           : in out Contents_Reader;
      Tag_Name       : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result);

   overriding
   procedure Text
     (This           : in out Contents_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result);

   overriding
   procedure Attribute
     (This            : in out Contents_Reader;
      Attribute_Name  : in     String;
      Attribute_Value : in     String;
      Shall_Continue  : in out Boolean;
      Call_Result     : in out Subprogram_Call_Result);

   overriding
   procedure Comment
     (This           : in out Contents_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result);

   overriding
   procedure CDATA
     (This           : in out Contents_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result);

private

   type Statistics_Parse_State2 is
     (
      Expect_Worksheet_Start_Tag,
      Expect_SheetPr_Start_Tag,
      Expect_SheetPr_End_Tag,
      Expect_Dimension_Start_Tag,
      Expect_Dimension_Ref_Attribute,
      Expect_SheetViews_Start_Tag,
      Expect_SheetViews_End_Tag,
      Expect_SheetFormatPr_Start_Tag,
      Expect_SheetFormatPr_End_Tag,
      Expect_SheetData_Start_Tag,
      Expect_Row_Start_Tag,
      --  Or expect SheetData end tag.

      Expect_Column_Start_Tag,
      --  Or expect row end tag

      Expect_Column_R_Attribute,
      Expect_Column_T_Attribute,
      Expect_V_Start_Tag,
      Expect_V_Text,
      Expect_V_End_Tag,
      Expect_Column_End_Tag,
      End_State
     ) with Default_Value => Expect_Worksheet_Start_Tag;

   type Value_Type is
     (
      Text,
      Number,
      Unspecified
     );

   subtype Current_Row_Type    is Nat32 range 0 .. Max_Row_Count;
   subtype Current_Column_Type is Nat32 range 0 .. Max_Column_Count;

   type Cell_Contents_Type (Value : Value_Type := Number) is record
      case Value is
         when Text =>
            Key : Strings_Extraction.String_Id_To_String_Maps.Map_Key;
         when Number |
              Unspecified =>
            Text : String_Ptr := Empty_String'Access;
      end case;
   end record;

   type Table_Array is
     array (Row_Index, Column_Index) of Cell_Contents_Type;

   type Contents_Reader
     (
      Subpool : Dynamic_Pools.Subpool_Handle;
      Id_To_String_Map : not null access constant
          Strings_Extraction.String_Id_To_String_Maps.Map
     ) is
     new XML_Stoppable_SAX_Parser.SAX_Parser with record
      State : Statistics_Parse_State2;
      Value : Value_Type;
      Table : Table_Array;
      Current_Row : Current_Row_Type := 0;
      Current_Column : Current_Column_Type := 0;
   end record;

   function Cell_Contents
     (This   : Contents_Reader;
      Row    : Row_Index;
      Column : Column_Index) return String is
     (case This.Table (Row, Column).Value is
         when Text =>
            This.Id_To_String_Map.Value (This.Table (Row, Column).Key),
         when Number | Unspecified =>
            This.Table (Row, Column).Text.all);

end Worksheet_XML_Readers.Contents_Readers;
