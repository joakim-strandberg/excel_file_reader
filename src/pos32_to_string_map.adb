package body Pos32_To_String_Map is

   function Available_Chars (This : Map) return Map_Available_Chars is
     (Char_Index'Last - This.My_Next);

   function Available_Keys (This : Map) return Map_Available_Keys is
     (Map_Key'Last - This.My_Next_Index);

   procedure Append
     (This  : in out Map;
      Value : String;
      Key   : out Map_Key) is
   begin
      This.My_Huge_Text
        (Positive (This.My_Next + 1) .. Positive (This.My_Next + Value'Length))
          := Value;
      This.My_Next_Index := This.My_Next_Index + 1;
      This.My_Substrings (This.My_Next_Index)
        := (From => This.My_Next + 1,
            To   => This.My_Next + Value'Length);
      This.My_Next := This.My_Next + Value'Length;
      Key := This.My_Next_Index;
   end Append;

end Pos32_To_String_Map;
