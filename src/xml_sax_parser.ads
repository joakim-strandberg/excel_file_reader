with Standard_Extensions;

package XML_SAX_Parser is

   use Standard_Extensions;

   type SAX_Parser_T is tagged limited null record;

   procedure Start_Tag
     (This        : in out SAX_Parser_T;
      Tag_Name    : in     String;
      Call_Result : in out Subprogram_Call_Result) is null;

   procedure End_Tag
     (This        : in out SAX_Parser_T;
      Tag_Name    : in     String;
      Call_Result : in out Subprogram_Call_Result) is null;
   --  It is the responsibility of the implementor of End_Tag to verify
   --  that the tag name corresponds to the expected tag name.

   procedure Text
     (This        : in out SAX_Parser_T;
      Value       : in     String;
      Call_Result : in out Subprogram_Call_Result) is null;

   procedure Attribute
     (This            : in out SAX_Parser_T;
      Attribute_Name  : in     String;
      Attribute_Value : in     String;
      Call_Result     : in out Subprogram_Call_Result) is null;

   procedure Comment
     (This        : in out SAX_Parser_T;
      Value       : in     String;
      Call_Result : in out Subprogram_Call_Result) is null;

   procedure CDATA
     (This        : in out SAX_Parser_T;
      Value       : in     String;
      Call_Result : in out Subprogram_Call_Result) is null;

   procedure Parse
     (This        : in out SAX_Parser_T'Class;
      Contents    : in     String;
      Call_Result : in out Subprogram_Call_Result)
     with
       Global => null,
       Pre    => ((not Call_Result.Has_Failed)
                  and Contents'Length > 0
                  and Contents'Last < Integer'Last - 4);

private

   procedure Analyze_XML
     (This        : in out SAX_Parser_T'Class;
      P_Value     : in out Int32;
      Contents    : String;
      Call_Result : in out Subprogram_Call_Result)
     with
       Global => null,
       Pre    =>
         not Call_Result.Has_Failed and
         P_Value > Contents'First and
         P_Value <= Contents'Last and
         Contents'Last < Integer'Last - 4;

end XML_SAX_Parser;
