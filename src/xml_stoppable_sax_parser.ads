with Standard_Extensions;

package XML_Stoppable_SAX_Parser is

   use Standard_Extensions;

   type SAX_Parser is tagged limited null record;

   procedure Start_Tag
     (This           : in out SAX_Parser;
      Tag_Name       : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result) is null;

   procedure End_Tag
     (This           : in out SAX_Parser;
      Tag_Name       : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result) is null;
   --  It is the responsibility of the implementor of End_Tag to verify
   --  that the tag name corresponds to the expected tag name.

   procedure Text
     (This           : in out SAX_Parser;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result) is null;

   procedure Attribute
     (This            : in out SAX_Parser;
      Attribute_Name  : in     String;
      Attribute_Value : in     String;
      Shall_Continue  : in out Boolean;
      Call_Result     : in out Subprogram_Call_Result) is null;

   procedure Comment
     (This           : in out SAX_Parser;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result) is null;

   procedure CDATA
     (This           : in out SAX_Parser;
      Value          : in     String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result) is null;

   procedure Parse
     (This        : in out SAX_Parser'Class;
      Contents    : in     String;
      Call_Result : in out Subprogram_Call_Result)
     with
       Global => null,
       Pre    => ((not Call_Result.Has_Failed)
                  and Contents'Length > 0
                  and Contents'Last < Integer'Last - 4);

private

   procedure Analyze_XML
     (This           : in out SAX_Parser'Class;
      P_Value        : in out Int32;
      Contents       : String;
      Shall_Continue : in out Boolean;
      Call_Result    : in out Subprogram_Call_Result)
     with
       Global => null,
       Pre    =>
         not Call_Result.Has_Failed and
         P_Value > Contents'First and
         P_Value <= Contents'Last and
         Contents'Last < Integer'Last - 4;

end XML_Stoppable_SAX_Parser;
