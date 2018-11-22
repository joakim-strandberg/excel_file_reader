with GNAT.Source_Info;
with Ada.Text_IO;
package body Shared_Strings_XML_Readers is

   function Source_Location return String renames
     GNAT.Source_Info.Source_Location;

   function Expected_Unique_Count (Reader : Statistics_Reader) return Nat32 is
      (Reader.My_Expected_Unique_Count);

   function Unique_Count (Reader : Statistics_Reader) return Nat32 is
     (Reader.My_Unique_Count);

   function Max_Characters (Reader : Statistics_Reader) return Nat32 is
     (Reader.My_Max_Characters);

   procedure Start_Tag
     (This        : in out Statistics_Reader;
      Tag_Name    : in     String;
      Call_Result : in out Subprogram_Call_Result) is
   begin
      case This.My_State is
         when Expecting_Sst_Start_Tag =>
            if Tag_Name = "sst" then
               This.My_State := Expecting_Unique_Count_Attribute;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_Si_Start_Tag =>
            if Tag_Name = "si" then
               This.My_State := Expecting_T_Start_Tag;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_T_Start_Tag =>
            if Tag_Name = "t" then
               This.My_State := Expecting_Text;
               This.My_Unique_Count := This.My_Unique_Count + 1;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_Text |
              Expecting_T_End_Tag |
              Expecting_Si_End_Tag |
              End_State |
            Expecting_Unique_Count_Attribute =>
            Call_Result.Initialize (0, 0);
            Ada.Text_IO.Put_Line (Source_Location);
      end case;
   end Start_Tag;

   procedure End_Tag
     (This        : in out Statistics_Reader;
      Tag_Name    : in     String;
      Call_Result : in out Subprogram_Call_Result)
   is
   begin
      case This.My_State is
         when Expecting_T_End_Tag =>
            if Tag_Name = "t" then
               This.My_State := Expecting_Si_End_Tag;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_Si_End_Tag =>
            if Tag_Name = "si" then
               This.My_State := Expecting_Si_Start_Tag;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_Si_Start_Tag =>
            if Tag_Name = "sst" then
               This.My_State := End_State;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_Sst_Start_Tag |
              Expecting_T_Start_Tag |
              Expecting_Text |
              Expecting_Unique_Count_Attribute |
              End_State =>
            Call_Result.Initialize (0, 0);
            Ada.Text_IO.Put_Line (Source_Location);
      end case;
   end End_Tag;

   procedure Text
     (This        : in out Statistics_Reader;
      Value       : in     String;
      Call_Result : in out Subprogram_Call_Result)
   is
   begin
      if Value'Length > 0 then
         case This.My_State is
            when Expecting_Text =>
               This.My_State := Expecting_T_End_Tag;
               This.My_Max_Characters := This.My_Max_Characters + Value'Length;
            when Expecting_Sst_Start_Tag |
                 Expecting_Unique_Count_Attribute |
                 Expecting_Si_Start_Tag |
                 Expecting_T_Start_Tag |
                 Expecting_T_End_Tag |
                 Expecting_Si_End_Tag |
                 End_State =>
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
         end case;
      end if;
   end Text;

   procedure Attribute
     (This            : in out Statistics_Reader;
      Attribute_Name  : in     String;
      Attribute_Value : in     String;
      Call_Result     : in out Subprogram_Call_Result) is
   begin
      case This.My_State is
         when Expecting_Unique_Count_Attribute =>
            if Attribute_Name = "uniqueCount" then
               This.My_State := Expecting_Si_Start_Tag;
               This.My_Expected_Unique_Count := Nat32'Value (Attribute_Value);
            end if;
         when Expecting_Text =>
            null; --  Ignore the attributes on <t> tags.
         when Expecting_Sst_Start_Tag |
              Expecting_Si_Start_Tag |
              Expecting_T_Start_Tag |
              Expecting_T_End_Tag |
              Expecting_Si_End_Tag |
              End_State =>
            Call_Result.Initialize (0, 0);
            Ada.Text_IO.Put_Line (Source_Location);
      end case;
   end Attribute;

   procedure Comment
     (This        : in out Statistics_Reader;
      Value       : in     String;
      Call_Result : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Value);
   begin
      case This.My_State is
         when Expecting_Sst_Start_Tag |
              Expecting_Unique_Count_Attribute |
              Expecting_Si_Start_Tag |
              Expecting_T_Start_Tag |
              Expecting_Text |
              Expecting_T_End_Tag |
              End_State |
              Expecting_Si_End_Tag =>
            Call_Result.Initialize (0, 0);
            Ada.Text_IO.Put_Line (Source_Location);
      end case;
   end Comment;

   procedure CDATA
     (This        : in out Statistics_Reader;
      Value       : in     String;
      Call_Result : in out Subprogram_Call_Result)
   is
      pragma Unreferenced (Value);
   begin
      case This.My_State is
         when Expecting_Sst_Start_Tag |
              Expecting_Unique_Count_Attribute |
              Expecting_Si_Start_Tag |
              Expecting_T_Start_Tag |
              Expecting_Text |
              Expecting_T_End_Tag |
              End_State |
              Expecting_Si_End_Tag =>
            Call_Result.Initialize (0, 0);
            Ada.Text_IO.Put_Line (Source_Location);
      end case;
   end CDATA;

   package body Strings_Extraction is

      procedure Start_Tag
        (This        : in out String_Extractor;
         Tag_Name    : in     String;
         Call_Result : in out Subprogram_Call_Result) is
      begin
         case This.My_State is
         when Expecting_Sst_Start_Tag =>
            if Tag_Name = "sst" then
               This.My_State := Expecting_Unique_Count_Attribute;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_Si_Start_Tag =>
            if Tag_Name = "si" then
               This.My_State := Expecting_T_Start_Tag;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_T_Start_Tag =>
            if Tag_Name = "t" then
               This.My_State := Expecting_Text;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_Text |
              Expecting_T_End_Tag |
              Expecting_Si_End_Tag |
              End_State |
              Expecting_Unique_Count_Attribute =>
            Call_Result.Initialize (0, 0);
            Ada.Text_IO.Put_Line (Source_Location);
         end case;
      end Start_Tag;

      procedure End_Tag
        (This        : in out String_Extractor;
         Tag_Name    : in     String;
         Call_Result : in out Subprogram_Call_Result)
      is
      begin
         case This.My_State is
         when Expecting_T_End_Tag =>
            if Tag_Name = "t" then
               This.My_State := Expecting_Si_End_Tag;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_Si_End_Tag =>
            if Tag_Name = "si" then
               This.My_State := Expecting_Si_Start_Tag;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_Si_Start_Tag =>
            if Tag_Name = "sst" then
               This.My_State := End_State;
            else
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end if;
         when Expecting_Sst_Start_Tag |
              Expecting_T_Start_Tag |
              Expecting_Text |
              Expecting_Unique_Count_Attribute |
              End_State =>
            Call_Result.Initialize (0, 0);
            Ada.Text_IO.Put_Line (Source_Location);
         end case;
      end End_Tag;

      procedure Text
        (This        : in out String_Extractor;
         Value       : in     String;
         Call_Result : in out Subprogram_Call_Result)
      is
         Key : String_Id_To_String_Maps.Map_Key;
         pragma Unreferenced (Key);
      begin
         if Value'Length > 0 then
            case This.My_State is
            when Expecting_Text =>
               This.My_State := Expecting_T_End_Tag;
               This.Map.Append (Value, Key);
            when Expecting_Sst_Start_Tag |
                 Expecting_Unique_Count_Attribute |
                 Expecting_Si_Start_Tag |
                 Expecting_T_Start_Tag |
                 Expecting_T_End_Tag |
                 Expecting_Si_End_Tag |
                 End_State =>
               Call_Result.Initialize (0, 0);
               Ada.Text_IO.Put_Line (Source_Location);
            end case;
         end if;
      end Text;

      procedure Attribute
        (This            : in out String_Extractor;
         Attribute_Name  : in     String;
         Attribute_Value : in     String;
         Call_Result     : in out Subprogram_Call_Result)
      is
         pragma Unreferenced (Attribute_Value);
      begin
         case This.My_State is
         when Expecting_Unique_Count_Attribute =>
            if Attribute_Name = "uniqueCount" then
               This.My_State := Expecting_Si_Start_Tag;
            end if;
         when Expecting_Text =>
            null; --  Ignore the attributes on <t> tags.
         when Expecting_Sst_Start_Tag |
              Expecting_Si_Start_Tag |
              Expecting_T_Start_Tag |
              Expecting_T_End_Tag |
              Expecting_Si_End_Tag |
              End_State =>
            Call_Result.Initialize (0, 0);
            Ada.Text_IO.Put_Line (Source_Location);
         end case;
      end Attribute;

      procedure Comment
        (This        : in out String_Extractor;
         Value       : in     String;
         Call_Result : in out Subprogram_Call_Result)
      is
         pragma Unreferenced (Value);
      begin
         case This.My_State is
         when Expecting_Sst_Start_Tag |
              Expecting_Unique_Count_Attribute |
              Expecting_Si_Start_Tag |
              Expecting_T_Start_Tag |
              Expecting_Text |
              Expecting_T_End_Tag |
              End_State |
              Expecting_Si_End_Tag =>
            Call_Result.Initialize (0, 0);
            Ada.Text_IO.Put_Line (Source_Location);
         end case;
      end Comment;

      procedure CDATA
        (This        : in out String_Extractor;
         Value       : in     String;
         Call_Result : in out Subprogram_Call_Result)
      is
         pragma Unreferenced (Value);
      begin
         case This.My_State is
         when Expecting_Sst_Start_Tag |
              Expecting_Unique_Count_Attribute |
              Expecting_Si_Start_Tag |
              Expecting_T_Start_Tag |
              Expecting_Text |
              Expecting_T_End_Tag |
              End_State |
              Expecting_Si_End_Tag =>
            Call_Result.Initialize (0, 0);
            Ada.Text_IO.Put_Line (Source_Location);
         end case;
      end CDATA;

   end Strings_Extraction;

end Shared_Strings_XML_Readers;
