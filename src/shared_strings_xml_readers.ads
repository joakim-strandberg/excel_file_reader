with XML_SAX_Parser;
with Standard_Extensions;
with Pos32_To_String_Map;
with Dynamic_Pools;

package Shared_Strings_XML_Readers is

   use Standard_Extensions;

   type Statistics_Reader is new XML_SAX_Parser.SAX_Parser_T with private;

   function Expected_Unique_Count (Reader : Statistics_Reader) return Nat32;

   function Unique_Count (Reader : Statistics_Reader) return Nat32;

   function Max_Characters (Reader : Statistics_Reader) return Nat32;

   overriding
   procedure Start_Tag
     (This        : in out Statistics_Reader;
      Tag_Name    : in     String;
      Call_Result : in out Subprogram_Call_Result);

   overriding
   procedure End_Tag
     (This        : in out Statistics_Reader;
      Tag_Name    : in     String;
      Call_Result : in out Subprogram_Call_Result);

   overriding
   procedure Text
     (This        : in out Statistics_Reader;
      Value       : in     String;
      Call_Result : in out Subprogram_Call_Result);

   overriding
   procedure Attribute
     (This            : in out Statistics_Reader;
      Attribute_Name  : in     String;
      Attribute_Value : in     String;
      Call_Result     : in out Subprogram_Call_Result);

   overriding
   procedure Comment
     (This        : in out Statistics_Reader;
      Value       : in     String;
      Call_Result : in out Subprogram_Call_Result);

   overriding
   procedure CDATA
     (This        : in out Statistics_Reader;
      Value       : in     String;
      Call_Result : in out Subprogram_Call_Result);

   type Parse_State is
     (
      Expecting_Sst_Start_Tag,
      Expecting_Unique_Count_Attribute,
      Expecting_Si_Start_Tag,
      Expecting_T_Start_Tag,
      Expecting_Text,
      Expecting_T_End_Tag,
      Expecting_Si_End_Tag,
      End_State
     ) with Default_Value => Expecting_Sst_Start_Tag;
   --  If the generic package below is moved into a child package
   --  then this enumeration type can be moved to the private part.

   generic
      Max_Characters : Nat32;
      Max_Strings    : Nat32;
   package Strings_Extraction is

      package String_Id_To_String_Maps is new Pos32_To_String_Map
        (Max_Characters, Max_Strings);

      type String_Extractor
        (Map : not null access String_Id_To_String_Maps.Map) is
        new XML_SAX_Parser.SAX_Parser_T with private;

      overriding
      procedure Start_Tag
        (This        : in out String_Extractor;
         Tag_Name    : in     String;
         Call_Result : in out Subprogram_Call_Result);

      overriding
      procedure End_Tag
        (This        : in out String_Extractor;
         Tag_Name    : in     String;
         Call_Result : in out Subprogram_Call_Result);

      overriding
      procedure Text
        (This        : in out String_Extractor;
         Value       : in     String;
         Call_Result : in out Subprogram_Call_Result);

      overriding
      procedure Attribute
        (This            : in out String_Extractor;
         Attribute_Name  : in     String;
         Attribute_Value : in     String;
         Call_Result     : in out Subprogram_Call_Result);

      overriding
      procedure Comment
        (This        : in out String_Extractor;
         Value       : in     String;
         Call_Result : in out Subprogram_Call_Result);

      overriding
      procedure CDATA
        (This        : in out String_Extractor;
         Value       : in     String;
         Call_Result : in out Subprogram_Call_Result);

   private

      type String_Extractor
        (Map : not null access String_Id_To_String_Maps.Map) is
        new XML_SAX_Parser.SAX_Parser_T with record
         My_State : Parse_State;
      end record;

   end Strings_Extraction;

private

   type Statistics_Reader is new XML_SAX_Parser.SAX_Parser_T with record
      My_Expected_Unique_Count : Nat32 := 0;
      My_Unique_Count          : Nat32 := 0;
      My_Max_Characters        : Nat32 := 0;
      My_State                 : Parse_State;
   end record;

end Shared_Strings_XML_Readers;
