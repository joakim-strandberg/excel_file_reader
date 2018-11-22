with Standard_Extensions; use Standard_Extensions;
with Dynamic_Pools;

generic
   Max_Chars   : Pos32;
   Max_Strings : Pos32;
package Pos32_To_String_Map is

   subtype Map_Key is Pos32 range 1  .. Max_Strings;

   subtype Map_Available_Chars is Nat32 range 0 .. Max_Chars;

   subtype Map_Available_Keys is Nat32 range 0 .. Max_Strings;

   type Map is tagged limited private with
     Default_Initial_Condition =>
       (Available_Chars (Map) = Max_Chars
        and Available_Keys (Map) = Max_Strings);

   function Available_Chars (This : Map) return Map_Available_Chars with
     Global => null;

   function Available_Keys (This : Map) return Map_Available_Keys with
     Global => null;

   procedure Append
     (This  : in out Map;
      Value : String;
      Key   : out Map_Key) with
     Global => null,
       Pre'Class  =>
         (Value'Length >= 1
          and This.Available_Chars >= Value'Length
          and This.Available_Keys > 0),
     Post'Class =>
       (This.Available_Chars'Old - Value'Length = This.Available_Chars
        and This.Available_Keys + 1 = This.Available_Keys'Old
        and This.Value (Key) = Value);

   function Value
     (This  : Map;
      Index : Map_Key) return String with
     Global => null;

   function Make return Map with
     Global => null;

private

   subtype Char_Index is Nat32 range 1 .. Max_Chars;

   subtype From_Index is Pos32 range 1 .. Char_Index'Last;

   subtype To_Index is Nat32 range 0 .. Char_Index'Last;

   type Substring_T is record
      From : From_Index := 1;
      To   : To_Index   := 0;
   end record;

   type Substring_Indexes is array (Map_Key) of Substring_T;

   subtype Next is Nat32 range 0 .. Char_Index'Last;

   subtype Next_Index_T is Nat32 range 0 .. Map_Key'Last;

   type Map is tagged limited record
      My_Huge_Text  : String (Char_Index'First .. Char_Index'Last)
        := (others => ' ');
      My_Next       : Next := 0;
      My_Next_Index : Next_Index_T := 0;
      My_Substrings : Substring_Indexes;
   end record;

   function Value
     (This  : Map;
      Index : Map_Key) return String is
     (This.My_Huge_Text
        (This.My_Substrings (Index).From .. This.My_Substrings (Index).To));

   function Make return Map is
     (
      My_Huge_Text  => (others => ' '),
      My_Next       => 0,
      My_Next_Index => 0,
      My_Substrings => (others => (From => 1, To => 0))
     );

end Pos32_To_String_Map;
