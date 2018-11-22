package body XML_DOM_Parser is

   package body Internal is

      procedure Start_Tag
        (This        : in out SAX_Parser_T;
         Tag_Name    : Standard.String;
         Call_Result : in out Subprogram_Call_Result) is
      begin
         case This.State is
            when Expecting_Object_Start =>
               if
                 Tag_Name'Length > 0 and This.Current_Nodes.Is_Empty
               then
                  declare
                     Current_Node : constant not null Node_Ptr
                       := new (This.Subpool) Node;
                  begin
                     Current_Node.Tag.Name
                       := new (This.Subpool) String'(Tag_Name);
                     This.Current_Nodes.Append (Current_Node);
                     This.Root_Node := Current_Node;
                  end;
                  This.State := Expecting_Default;
               else
                  Call_Result.Initialize (-2132671123, 1966624808);
               end if;
            when Expecting_Default =>
               if
                 Tag_Name'Length > 0
               then
                  declare
                     Current_Node : constant not null Node_Ptr
                       := new (This.Subpool) Node;
                  begin
                     Current_Node.Tag.Name
                       := new (This.Subpool) String'(Tag_Name);

                     if
                       This.Current_Nodes.Last_Element.Id = Tag_Node
                     then
                        This.Current_Nodes.Last_Element.Tag.Child_Nodes.Append
                          (Current_Node);
                        This.Current_Nodes.Append (Current_Node);
                     else
                        Call_Result.Initialize (1695756105, 1714042669);
                     end if;
                  end;
               else
                  Call_Result.Initialize (-0416079960, -1464855808);
               end if;
            when End_State =>
               Call_Result.Initialize (0561631589, 0761077416);
         end case;
      end Start_Tag;

      procedure End_Tag
        (This        : in out SAX_Parser_T;
         Tag_Name    : String;
         Call_Result : in out Subprogram_Call_Result) is
      begin
         case This.State is
            when Expecting_Default =>
               if
                 (not This.Current_Nodes.Is_Empty) and then
                 (This.Current_Nodes.Last_Element.Id = Tag_Node) and then
                 This.Current_Nodes.Last_Element.Tag.Name.all = Tag_Name
               then
                  This.Current_Nodes.Delete_Last;
                  if This.Current_Nodes.Is_Empty then
                     This.State := End_State;
                  end if;
               else
                  Call_Result.Initialize (-1355522791, 1675536860);
               end if;
            when Expecting_Object_Start |
                 End_State =>
               Call_Result.Initialize (-0728861922, -0299445966);
         end case;
      end End_Tag;

      procedure Text
        (This        : in out SAX_Parser_T;
         Value       : String;
         Call_Result : in out Subprogram_Call_Result)
      is
         Current_Node : Node_Ptr;
      begin
         case This.State is
            when Expecting_Default =>
               if Value'Length = 0 or
                 (Value'Length > 0 and then
                    (for all I in Value'Range =>
                         Value (I) = ' ' or
                         Value (I) = Character'Val (10) or
                         Value (I) = Character'Val (13)))
               then
                  null;
               elsif not This.Current_Nodes.Is_Empty then
--                  Ada.Text_IO.Put_Line ("1 " & Value);
                  Current_Node
                    := new (This.Subpool) XML_DOM_Parser.Node (Text_Node);
                  Current_Node.Text := new (This.Subpool) String'(Value);
--                  Ada.Text_IO.Put_Line ("2");

                  if This.Current_Nodes.Last_Element.Id = Tag_Node then
                     This.Current_Nodes.Last_Element.Tag.Child_Nodes.Append
                       (Current_Node);
                  else
                     Call_Result.Initialize (-0944309962, -0212130363);
                  end if;
               else
                  Call_Result.Initialize (0536156601, 0921613311);
               end if;
            when Expecting_Object_Start |
                 End_State =>
               Call_Result.Initialize (0240750889, 1723362921);
         end case;
      end Text;

      procedure Attribute
        (This            : in out SAX_Parser_T;
         Attribute_Name  : String;
         Attribute_Value : String;
         Call_Result     : in out Subprogram_Call_Result)
      is
         procedure Handle_Default;

         procedure Handle_Default is
            Attribute : Attribute_Ptr;
         begin
            if
              (not This.Current_Nodes.Is_Empty) and then
              (Attribute_Name'Length > 0 and Attribute_Value'Length > 0)
            then
               Attribute := new (This.Subpool) XML_DOM_Parser.Attribute;

               Attribute.Name := new (This.Subpool) String'(Attribute_Name);
               Attribute.Value
                 := new (This.Subpool) String'(Attribute_Value);

               if This.Current_Nodes.Last_Element.Id = Tag_Node then
                  This.Current_Nodes.Last_Element.Tag.Attributes.Append
                    (Attribute);
               else
                  Call_Result.Initialize (0612916249, -0250963769);
               end if;
            else
               Call_Result.Initialize (-0372407662, -1139199208);
            end if;
         end Handle_Default;

      begin
         case This.State is
            when Expecting_Default                  => Handle_Default;
            when Expecting_Object_Start | End_State =>
               Call_Result.Initialize (1103012185, 0319457400);
         end case;
      end Attribute;

      procedure Comment
        (This        : in out SAX_Parser_T;
         Value       : String;
         Call_Result : in out Subprogram_Call_Result)
      is
         procedure Handle_Default;

         procedure Handle_Default is
            Node : Node_Ptr;
         begin
            if
              (not This.Current_Nodes.Is_Empty) and then
              Value'Length > 0
            then
               Node := new (This.Subpool) XML_DOM_Parser.Node'
                 (Id   => Comment_Node,
                  Text => new (This.Subpool) String'(Value));

               if This.Current_Nodes.Last_Element.Id = Tag_Node then
                  This.Current_Nodes.Last_Element.Tag.Child_Nodes.Append
                    (Node);
               else
                  Call_Result.Initialize (2066772500, 1193932906);
               end if;
            else
               Call_Result.Initialize (0845969060, 0639006566);
            end if;

         end Handle_Default;

      begin
         case This.State is
            when Expecting_Default => Handle_Default;
            when Expecting_Object_Start |
                 End_State =>
               Call_Result.Initialize (-1373186804, -0874315849);
         end case;
      end Comment;

      procedure CDATA
        (This        : in out SAX_Parser_T;
         Value       : String;
         Call_Result : in out Subprogram_Call_Result)
      is
         procedure Handle_Default;

         procedure Handle_Default is
            Node : Node_Ptr;
         begin
            if
              (not This.Current_Nodes.Is_Empty) and then
              Value'Length > 0
            then
               Node := new (This.Subpool) XML_DOM_Parser.Node'
                 (Id   => CDATA_Node,
                  Text => new (This.Subpool) String'(Value));

               if
                 This.Current_Nodes.Last_Element.Id = Tag_Node
               then
                  This.Current_Nodes.Last_Element.Tag.Child_Nodes.Append
                    (Node);
               else
                  Call_Result.Initialize (-2021174626, -1403249390);
               end if;
            else
               Call_Result.Initialize (-0076965217, 0193355440);
            end if;
         end Handle_Default;

      begin
         case This.State is
            when Expecting_Default => Handle_Default;
            when Expecting_Object_Start |
                 End_State =>
               Call_Result.Initialize (0698504230, -0963685542);
         end case;
      end CDATA;

   end Internal;

   procedure Parse
     (This        : in out DOM_Parser_T;
      Subpool     : in out Dynamic_Pools.Subpool_Handle;
      XML_Message : String;
      Call_Result : in out Subprogram_Call_Result;
      Root_Node   :    out Node_Ptr)
   is
      SAX_Parser : Internal.SAX_Parser_T;
   begin
      SAX_Parser.Subpool := Subpool;
      SAX_Parser.Parse
        (XML_Message,
         Call_Result);

      Root_Node := SAX_Parser.Root_Node;
   end Parse;

end XML_DOM_Parser;
