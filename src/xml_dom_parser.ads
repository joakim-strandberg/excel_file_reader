with Standard_Extensions;
with Dynamic_Pools;
with Ada.Containers.Vectors;
with XML_SAX_Parser;

pragma Elaborate_All (XML_SAX_Parser);
pragma Elaborate_All (Standard_Extensions);

package XML_DOM_Parser is

   use Standard_Extensions;

   Empty_String : aliased String := "";

   type Attribute is tagged limited record
      Name  : String_Ptr;
      Value : String_Ptr;
   end record;

   type Attribute_Ptr is access all Attribute
     with Storage_Pool => Default_Subpool;

   package Attribute_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Attribute_Ptr);

   type Node_Kind_Id is
     (
      Tag_Node,
      Comment_Node,
      CDATA_Node,
      Text_Node
     );

   type Attributes_Ref
     (
      Element : not null access constant Attribute_Vectors.Vector
     )
   is limited null record with Implicit_Dereference => Element;

   type Node;
   type Node_Ptr is access all Node with Storage_Pool => Default_Subpool;

   package Node_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Node_Ptr);

   type Child_Nodes_Ref
     (
      E : not null access constant Node_Vectors.Vector
     )
   is limited null record with Implicit_Dereference => E;

   type XML_Tag is tagged record
      Name        : String_Ptr := Empty_String'Access;
      Child_Nodes : Node_Vectors.Vector;
      Attributes  : Attribute_Vectors.Vector;
   end record;

   type Tag_Ref
     (
      E : not null access constant XML_Tag
     )
   is limited null record
     with
       Implicit_Dereference => E;

   type Node (Id : Node_Kind_Id := Tag_Node) is record
      case Id is
         when Tag_Node =>
            Tag  : aliased XML_Tag;
         when Comment_Node |
              CDATA_Node |
              Text_Node =>
            Text : not null String_Ptr := Empty_String'Access;
      end case;
   end record;

   type DOM_Parser_T is tagged limited null record;

   procedure Parse (This        : in out DOM_Parser_T;
                    Subpool     : in out Dynamic_Pools.Subpool_Handle;
                    XML_Message : String;
                    Call_Result : in out Subprogram_Call_Result;
                    Root_Node   :    out Node_Ptr) with
     Global    => null,
       Pre'Class =>
         not Call_Result.Has_Failed and
         XML_Message'Length > 0 and
         XML_Message'Last < Integer'Last - 4;

private

   package Internal is

      type State_T is
        (
         Expecting_Object_Start,
         --  seems to only apply to the root start tag

         Expecting_Default,
         --  Attribute_Or_Text_Or_Comment_Or_CDATA_Or_Object_Start_Or_Object_En

         End_State
        );

      type SAX_Parser_T is new XML_SAX_Parser.SAX_Parser_T with record
         Root_Node     : Node_Ptr := null;
         Current_Nodes : Node_Vectors.Vector;
         --  The current node is the last Node pointed to in the container

         State         : State_T := Expecting_Object_Start;
         Subpool       : Dynamic_Pools.Subpool_Handle;
      end record;

      overriding
      procedure Start_Tag
        (This        : in out SAX_Parser_T;
         Tag_Name    : String;
         Call_Result : in out Subprogram_Call_Result)
        with
          Global => null,
          Pre    => not Call_Result.Has_Failed;

      overriding
      procedure End_Tag
        (This        : in out SAX_Parser_T;
         Tag_Name    : String;
         Call_Result : in out Subprogram_Call_Result)
        with
          Global => null,
          Pre    => not Call_Result.Has_Failed;

      overriding
      procedure Text
        (This        : in out SAX_Parser_T;
         Value       : String;
         Call_Result : in out Subprogram_Call_Result)
        with
          Global => null,
          Pre    => not Call_Result.Has_Failed;

      overriding
      procedure Attribute
        (This            : in out SAX_Parser_T;
         Attribute_Name  : String;
         Attribute_Value : String;
         Call_Result     : in out Subprogram_Call_Result)
        with
          Global => null,
          Pre    => not Call_Result.Has_Failed;

      overriding
      procedure Comment
        (This        : in out SAX_Parser_T;
         Value       : String;
         Call_Result : in out Subprogram_Call_Result)
        with
          Global => null,
          Pre    => not Call_Result.Has_Failed;

      overriding
      procedure CDATA
        (This        : in out SAX_Parser_T;
         Value       : String;
         Call_Result : in out Subprogram_Call_Result)
        with
          Global => null,
          Pre    => not Call_Result.Has_Failed;

   end Internal;

end XML_DOM_Parser;
