with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;
with Ada.Directories;
with Dynamic_Pools;
with Standard_Extensions;
with Ada.Streams.Stream_IO;
with XML_DOM_Parser;
with Zip;
with UnZip;
with GNAT.OS_Lib;
with Worksheet_XML_Readers.Contents_Readers;
with Shared_Strings_XML_Readers;

procedure Main is

   use Standard_Extensions;

   package Stream_IO renames Ada.Streams.Stream_IO;

   subtype Stream_Element is Ada.Streams.Stream_Element;

   use all type XML_DOM_Parser.Node_Kind_Id;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   generic
      with package Strings_Extraction is
        new Shared_Strings_XML_Readers.Strings_Extraction (<>);
   procedure Use_Map
     (Subpool : in out not null Dynamic_Pools.Subpool_Handle;
      Id_To_String_Map : Strings_Extraction.String_Id_To_String_Maps.Map);

   Countries_Directory : constant String := "countries";

   Allocation_Block_Size : constant := 30_000_000;

   Scoped_Subpool : constant Dynamic_Pools.Scoped_Subpool
     := Dynamic_Pools.Create_Subpool
       (Default_Subpool,
        Allocation_Block_Size);

   Subpool : not null Dynamic_Pools.Subpool_Handle
     := Scoped_Subpool.Handle;

   Scoped_Shared_Strings_Subpool : constant Dynamic_Pools.Scoped_Subpool
     := Dynamic_Pools.Create_Subpool (Default_Subpool,
                                      Allocation_Block_Size);

   Shared_Strings_Subpool : constant not null Dynamic_Pools.Subpool_Handle
     := Scoped_Shared_Strings_Subpool.Handle;

   procedure Unzip_Countries_Excel_File;
   procedure Read_Shared_Strings_File;

   procedure Unzip_Countries_Excel_File is

      function Make_Path
        (File_Name     : String;
         Name_encoding : Zip.Zip_name_encoding) return String;

      function Make_Path
        (File_Name     : String;
         Name_encoding : Zip.Zip_name_encoding) return String
      is
         pragma Unreferenced (Name_encoding);
      begin
         return (Countries_Directory & GNAT.OS_Lib.Directory_Separator &
                   File_Name);
      end Make_Path;

   begin
      UnZip.Extract
        (from => "countries.xlsx",
         file_system_routines =>
           (Ada.Directories.Create_Path'Access,
            null,
            Make_Path'Unrestricted_Access,
            null));
      Ada.Text_IO.Put_Line
        ("Successfully unzipped the countries.xlsx-file.");
      Read_Shared_Strings_File;
   end Unzip_Countries_Excel_File;

   Shared_Strings_Root_Node : XML_DOM_Parser.Node_Ptr;

   Max_Characters : Nat32;
   Max_Strings    : Nat32;

   procedure Read_Shared_Strings_File is

      File_Name : constant String
        := Countries_Directory & GNAT.OS_Lib.Directory_Separator &
        "xl" & GNAT.OS_Lib.Directory_Separator & "sharedStrings.xml";

      File_Size : Natural;

      File_Contents : String_Ptr;

      procedure Allocate_Space;
      procedure Read_Contents;
      procedure Parse_Contents;
      procedure Populate_Map;

      procedure Allocate_Space is
      begin
         File_Size := Natural (Ada.Directories.Size (File_Name));

         if File_Size > 4 then
            File_Contents := new (Shared_Strings_Subpool) String
              (1 .. File_Size);
            Read_Contents;
         else
            Ada.Text_IO.Put_Line ("File " & File_Name & " is too small!");
         end if;
      end Allocate_Space;

      pragma Unmodified (File_Size);
      pragma Unmodified (File_Contents);

      procedure Read_Contents is
         File : Stream_IO.File_Type;
         Element : Stream_Element;
         Stream : Stream_IO.Stream_Access;

         I : Positive := File_Contents.all'First;
      begin
         Stream_IO.Open (File, Stream_IO.In_File, File_Name);

         Stream := Stream_IO.Stream (File);

         while not Stream_IO.End_Of_File (File) loop
            Stream_Element'Read (Stream, Element);
            File_Contents (I) := Character'Val (Element);
            I := I + 1;
         end loop;

         Stream_IO.Close (File);

         Parse_Contents;
      end Read_Contents;

      procedure Parse_Contents is
         Call_Result : Subprogram_Call_Result;
         Statistics_Reader : Shared_Strings_XML_Readers.Statistics_Reader;
      begin
         if Call_Result.Has_Failed then
            Ada.Text_IO.Put_Line (Call_Result.Message);
         else
            Ada.Text_IO.Put_Line ("Successfully parsed " & File_Name & "!");
            Statistics_Reader.Parse (File_Contents.all, Call_Result);
            if
              Statistics_Reader.Expected_Unique_Count =
                Statistics_Reader.Unique_Count
            then
               Max_Characters := Statistics_Reader.Max_Characters;
               Max_Strings := Statistics_Reader.Unique_Count;
               Populate_Map;
            else
               Ada.Text_IO.Put_Line
                 (Statistics_Reader.Expected_Unique_Count'Image);
               Ada.Text_IO.Put_Line
                 (Statistics_Reader.Unique_Count'Image);
               Ada.Text_IO.Put_Line
                 (Statistics_Reader.Max_Characters'Image);
            end if;
         end if;
      end Parse_Contents;

      procedure Populate_Map is

         Default_Subpool2 : Dynamic_Pools.Dynamic_Pool (0);
         --  Allocations are done in subpools, not the default subpool

         Scoped_Subpool2 : constant Dynamic_Pools.Scoped_Subpool
           := Dynamic_Pools.Create_Subpool (Default_Subpool2,
                                            Allocation_Block_Size);

         Subpool2 : constant not null Dynamic_Pools.Subpool_Handle
           := Scoped_Subpool2.Handle;

         package Strings_Extraction is
           new Shared_Strings_XML_Readers.Strings_Extraction
             (Max_Characters, Max_Strings);

         type Map_Ptr is
           access Strings_Extraction.String_Id_To_String_Maps.Map with
             Storage_Pool => Default_Subpool2;

         Id_To_String_Map : constant not null Map_Ptr
           := new (Subpool2)
           Strings_Extraction.String_Id_To_String_Maps.Map;

         Strings_Extractor : Strings_Extraction.String_Extractor
           (Id_To_String_Map);

         Call_Result : Subprogram_Call_Result;
      begin
         Strings_Extractor.Parse (File_Contents.all, Call_Result);

         if Call_Result.Is_Success then
            declare
               procedure Do_Work is new Use_Map (Strings_Extraction);
            begin
               Do_Work (Subpool, Id_To_String_Map.all);
            end;
         else
            Ada.Text_IO.Put_Line ("Populate_Map : " & Call_Result.Message);
         end if;
      end Populate_Map;

   begin
      Allocate_Space;
   end Read_Shared_Strings_File;

   pragma Unmodified (Shared_Strings_Root_Node);

   procedure Use_Map
     (Subpool : in out not null Dynamic_Pools.Subpool_Handle;
      Id_To_String_Map : Strings_Extraction.String_Id_To_String_Maps.Map)
   is
      pragma Unmodified (Subpool);

      Sheet1_Root_Node : XML_DOM_Parser.Node_Ptr;

      procedure Read_Sheet1 is
         File_Name : constant String
           := Countries_Directory & GNAT.OS_Lib.Directory_Separator &
           "xl" & GNAT.OS_Lib.Directory_Separator &
           "worksheets" & GNAT.OS_Lib.Directory_Separator & "sheet1.xml";

         File_Size : Natural;

         File_Contents : String_Ptr;

         procedure Allocate_Space;
         procedure Read_Contents;
         procedure Parse_Contents;

         procedure Allocate_Space is
         begin
            File_Size := Natural (Ada.Directories.Size (File_Name));

            if File_Size > 4 then
               File_Contents := new (Subpool) String (1 .. File_Size);
               Read_Contents;
            else
               Ada.Text_IO.Put_Line ("File " & File_Name & " is too small!");
            end if;
         end Allocate_Space;

         pragma Unmodified (File_Size);
         pragma Unmodified (File_Contents);

         procedure Read_Contents is
            File : Stream_IO.File_Type;
            Element : Stream_Element;
            Stream : Stream_IO.Stream_Access;

            I : Positive := File_Contents.all'First;
         begin
            Stream_IO.Open (File, Stream_IO.In_File, File_Name);

            Stream := Stream_IO.Stream (File);

            while not Stream_IO.End_Of_File (File) loop
               Stream_Element'Read (Stream, Element);
               File_Contents (I) := Character'Val (Element);
               I := I + 1;
            end loop;

            Stream_IO.Close (File);

            Parse_Contents;
         end Read_Contents;

         procedure Parse_Contents is
            Statistics : Worksheet_XML_Readers.Statistics_Reader;

            Call_Result : Subprogram_Call_Result;
         begin
            Statistics.Parse (File_Contents.all, Call_Result);

            if Call_Result.Has_Failed then
               Ada.Text_IO.Put_Line (Call_Result.Message);
            else
               Ada.Text_IO.Put_Line ("Success " & File_Name & "!");
               declare
                  package Contents_Readers is
                    new Worksheet_XML_Readers.Contents_Readers
                      (Statistics.Row_Count,
                       Statistics.Column_Count,
                       Strings_Extraction);

                  Contents_Reader : Contents_Readers.Contents_Reader
                    (Subpool, Id_To_String_Map'Access);
               begin
                  Contents_Reader.Parse (File_Contents.all, Call_Result);
                  if Call_Result.Has_Failed then
                     Ada.Text_IO.Put_Line
                       ("Location(-1448777510, -1822405807) Code(" &
                          Call_Result.Message & ")");
                  else
                     Ada.Text_IO.Put_Line ("Success, party time!");

                     --
                     --  Here the contents of the first column is
                     --  printed to standard out.
                     --

                     for Row in Contents_Readers.Row_Index range
                       1 .. Contents_Readers.Row_Index'Last loop
                        Ada.Text_IO.Put_Line
                          (Contents_Reader.Cell_Contents (Row, 1));
                     end loop;

                     --
                     --  Now printing column 2 to standard out.
                     --

                     for Row in Contents_Readers.Row_Index range
                       1 .. Contents_Readers.Row_Index'Last loop
                        Ada.Text_IO.Put_Line
                          (Contents_Reader.Cell_Contents (Row, 2));
                     end loop;

                  end if;
               end;
            end if;
         end Parse_Contents;

      begin
         Allocate_Space;
      end Read_Sheet1;

      pragma Unmodified (Sheet1_Root_Node);

   begin
      Read_Sheet1;
   end Use_Map;

begin
   Unzip_Countries_Excel_File;
end Main;
