with XML_Stoppable_SAX_Parser;
with Standard_Extensions;

package Worksheet_XML_Readers is

   use Standard_Extensions;

   type Statistics_Reader is
     new XML_Stoppable_SAX_Parser.SAX_Parser with private;

   function Row_Count (Reader : Statistics_Reader) return Nat32;

   function Column_Count (Reader : Statistics_Reader) return Nat32;

   overriding
   procedure Start_Tag
     (This           : in out Statistics_Reader;
      Tag_Name       : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result);

   overriding
   procedure End_Tag
     (This           : in out Statistics_Reader;
      Tag_Name       : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result);

   overriding
   procedure Text
     (This           : in out Statistics_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result);

   overriding
   procedure Attribute
     (This            : in out Statistics_Reader;
      Attribute_Name  : in     String;
      Attribute_Value : in     String;
      Shall_Continue  : in out Boolean;
      Call_Result     : in out Subprogram_Call_Result);

   overriding
   procedure Comment
     (This           : in out Statistics_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result);

   overriding
   procedure CDATA
     (This           : in out Statistics_Reader;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result);

private

   Dimension_Text : constant String := "dimension";

   type Statistics_Parse_State is
     (
      Expect_Worksheet_Start_Tag,
      Expect_SheetPr_Start_Tag,
      Expect_SheetPr_End_Tag,
      Expect_Dimension_Start_Tag,
      Expect_Dimension_Ref_Attribute,
      End_State
     ) with Default_Value => Expect_Worksheet_Start_Tag;

   type Statistics_Reader is
     new XML_Stoppable_SAX_Parser.SAX_Parser with record
      My_Row_Count    : Nat32;
      My_Column_Count : Nat32;
      My_State        : Statistics_Parse_State;
   end record;

   function Row_Count (Reader : Statistics_Reader) return Nat32 is
     (Reader.My_Row_Count);

   function Column_Count (Reader : Statistics_Reader) return Nat32 is
     (Reader.My_Column_Count);

   type Cell_Coordinate is record
      Is_Success : Boolean;
      Row : Pos32;
      Column : Pos32;
   end record;

   function Get_Cell (Text : String) return Cell_Coordinate;

end Worksheet_XML_Readers;
