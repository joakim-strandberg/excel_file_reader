with UTF8;
with Ada.Characters.Latin_1;
--  with Ada.Text_IO;

package body XML_SAX_Parser is

   use all type UTF8.Code_Point;

   type Initial_State_Id_T is
     (Less_Sign,
      Question_Mark,
      X,
      XM,
      XML,
      XML_S,
      XML_S_V,
      XML_S_VE,
      XML_S_VER,
      XML_S_VERS,
      XML_S_VERSI,
      XML_S_VERSIO,
      XML_S_VERSION,
      XML_S_VERSION_E,
      XML_S_VERSION_E_Q,
      XML_S_VERSION_E_Q_1,
      XML_S_VERSION_E_Q_1_P,
      XML_S_VERSION_E_Q_1_P_0,
      XML_S_VERSION_E_Q_1_P_0_Q,
      XML_S_VERSION_E_Q_1_P_0_Q_S,
      XML_S_VERSION_E_Q_1_P_0_Q_S_E,
      XML_S_VERSION_E_Q_1_P_0_Q_S_EN,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q,

      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q_QM,
      --  QM = Question Mark

      Optional_S,
      Optional_ST,
      Optional_STA,
      Optional_STAN,
      Optional_STAND,
      Optional_STANDA,
      Optional_STANDAL,
      Optional_STANDALO,
      Optional_STANDALON,
      Optional_STANDALONE,
      Optional_STANDALONE_E,
      Optional_STANDALONE_E_QM,
      Optional_STANDALONE_E_QM_Y,
      Optional_STANDALONE_E_QM_YE,
      Optional_STANDALONE_E_QM_YES,
      Optional_STANDALONE_E_QM_YES_QM,

      End_State
     );

   type State_Id_Type is
     (
      Expecting_NL_Sign_Or_Space_Or_Less_Sign,
      --  NL = New Line

      Found_Less_Sign,
      --  First start tag has not yet been found

      Found_Less_Followed_By_Exclamation_Sign,
      --  First start tag has not yet been found

      Found_Less_Followed_By_Exclamation_And_Dash_Sign,
      --  First start tag has not yet been found

      Extracting_Start_Tag_Name,
      Expecting_G_Sign_Or_Attributes,
      Expecting_G_Sign_Or_Attributes_And_Found_Slash,
      Extracting_Attribute_Name,
      Expecting_Attribute_Value_Quotation_Mark,
      Extracting_Attribute_Value,
      Expecting_New_Tag_Or_Tag_Value,
      --  Or start of comment or start- tag or end-tag

      Expecting_New_Tag_Or_Tag_Value_And_Found_L,
      Expecting_Only_Trailing_Spaces,
      Extracting_End_Tag_Name,

      New_Tag_Or_Tag_Value_And_L_And_Excl_And_Dash,
      --  Expecting new tag or tag value and found less sign and
      --  exclamation mark and dash sign ('-')

      --  Enumeration values introduced to handle <!CDATA[--]]>
      New_Tag_Or_Tag_Value_And_Found_L_And_Exclamation,
      New_Tag_Or_Tag_Value_But_Expecting_C,
      New_Tag_Or_Tag_Value_But_Expecting_CD,
      New_Tag_Or_Tag_Value_But_Expecting_CDA,
      New_Tag_Or_Tag_Value_But_Expecting_CDAT,
      New_Tag_Or_Tag_Value_But_Expecting_CDATA,
      New_Tag_Or_Tag_Value_CDATA_And_Square_Bracket,
      Extracting_CDATA,
      Extracting_CDATA_Found_Square_Bracket,
      Extracting_CDATA_Found_Two_Square_Brackets,
      Init_Extracting_Comment,
      --  First start tag has not yet been found

      Init_Extracting_Comment_And_Found_Dash,
      --  First start tag has not yet been found

      Init_Extracting_Comment_And_Found_Dash_Dash,
      --  First start tag has not yet been found

      Extracting_Comment,
      Extracting_Comment_And_Found_Dash,
      Extracting_Comment_And_Found_Dash_Dash
     );

   type Expected_Quotation_Symbol_T is
     (
      Single_Quotes, -- Example: 'hello'
      Double_Quotes  -- Example: "hello"
     );

   function Is_Special_Symbol (CP : UTF8.Code_Point) return Boolean is
     (if CP = Character'Pos ('<') then
           True
      elsif CP = Character'Pos ('>') then
           True
      elsif CP = Character'Pos ('/') then
           True
      elsif CP = Character'Pos ('"') then
           True
      else
         False);

   ERROR_CODE_1 : constant Int32 := 0564906783;
   ERROR_CODE_2 : constant Int32 := -1253063082;

   procedure Analyze_XML
     (This        : in out SAX_Parser_T'Class;
      P_Value     : in out Int32;
      Contents    : String;
      Call_Result : in out Subprogram_Call_Result)
   is
      subtype P_T      is Integer range Contents'First .. Contents'Last + 4;
      subtype Prev_P_T is Integer range Contents'First + 1 .. Contents'Last;

      P : P_T := P_Value;

      Depth : Nat32 := 0;

      State_Id : State_Id_Type := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

      subtype Prev_Prev_P_T is
        Integer range Contents'First + 0 .. Contents'Last;

      subtype Contents_Index_T is
        Integer range Contents'First .. Contents'Last;

      CP : UTF8.Code_Point;

      Prev_P : Prev_P_T := P;
      Prev_Prev_P : Prev_Prev_P_T; -- := Prev_P;

      Start_Tag_Name_First_Index : Contents_Index_T := Prev_P;
      Start_Tag_Name_Last_Index  : Contents_Index_T := Prev_P;

      Tag_Value_First_Index : Contents_Index_T := Contents'First;
      Tag_Value_Last_Index  : Contents_Index_T := Contents'First;

      End_Tag_Name_First_Index : Contents_Index_T := Contents'First;
      End_Tag_Name_Last_Index  : Contents_Index_T;

      Attribute_First_Index : Contents_Index_T := Prev_P;
      Attribute_Last_Index  : Contents_Index_T := Prev_P;

      Attribute_Value_First_Index : Contents_Index_T := Prev_P;
      Attribute_Value_Last_Index  : Contents_Index_T;

      Comment_First_Index : Contents_Index_T := Prev_P;

      Expected_Quotation_Symbol : Expected_Quotation_Symbol_T
        := Double_Quotes;
   begin
      if UTF8.Is_Valid_UTF8_Code_Point (Source  => Contents,
                                        Pointer => P)
      then
         UTF8.Get (Source  => Contents,
                   Pointer => P,
                   Value   => CP);

         if CP = Character'Pos ('>') then
            while P <= Contents'Last loop
               Prev_Prev_P := Prev_P;

               Prev_P := P;

               if not UTF8.Is_Valid_UTF8_Code_Point (Source  => Contents,
                                                     Pointer => P)
               then
                  Call_Result.Initialize (0917933704, 1893541713);
                  exit;
               end if;

               UTF8.Get (Source  => Contents,
                         Pointer => P,
                         Value   => CP);

               pragma Loop_Variant (Increases => P);
               pragma Loop_Invariant (not Call_Result.Has_Failed);
               pragma Loop_Invariant (P <= Contents'Last + 4);
               pragma Loop_Invariant (Prev_Prev_P < Prev_P and Prev_P < P);
               pragma Loop_Invariant
                 (State_Id /= Extracting_Attribute_Name or
                    (State_Id = Extracting_Attribute_Name and then
                         (Attribute_First_Index < P)));

--                  Ada.Text_IO.Put ("Extracted:");
--                  Ada.Text_IO.Put (Image (CP));
--                  Ada.Text_IO.Put (", state ");
--                  Ada.Text_IO.Put_Line (State_Id_Type'Image (State_Id));
--                  Ada.Text_IO.Put (Image (CP));

               case State_Id is
                  when Expecting_NL_Sign_Or_Space_Or_Less_Sign =>
                     if
                       CP = Character'Pos (Ada.Characters.Latin_1.LF)
                       or CP = Character'Pos (Ada.Characters.Latin_1.CR)
                       or CP = Character'Pos (' ')
                     then
                        null; -- Normal
                     elsif CP = Character'Pos ('<') then
                        State_Id := Found_Less_Sign;
                     else
                        Call_Result.Initialize (-0220363574, 0662000727);
                        exit;
                     end if;
                  when Found_Less_Sign =>
                     if CP = Character'Pos ('!') then
                        State_Id
                          := Found_Less_Followed_By_Exclamation_Sign;
                     elsif CP = Character'Pos ('/') then
                        if Depth = 0 then
                           Call_Result.Initialize (-1257694268, -2112592695);
                           exit;
                        end if;

                        if P > Contents'Last then
                           Call_Result.Initialize (-0929795332, 0193766410);
                           exit;
                        end if;

                        This.Text ("",
                                   Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        State_Id := Extracting_End_Tag_Name;
                        End_Tag_Name_First_Index := P;
                     elsif not Is_Special_Symbol (CP) then
                        State_Id := Extracting_Start_Tag_Name;
                        Start_Tag_Name_First_Index := Prev_P;

                        pragma Assert (Start_Tag_Name_First_Index < P);
                     else
                        Call_Result.Initialize (0218310192, -1344536484);
                        exit;
                     end if;
                  when Found_Less_Followed_By_Exclamation_Sign =>
                     if CP = Character'Pos ('-') then
                        State_Id
                          := Found_Less_Followed_By_Exclamation_And_Dash_Sign;
                     else
                        Call_Result.Initialize (0993658621, 0982639814);
                        exit;
                     end if;
                  when Found_Less_Followed_By_Exclamation_And_Dash_Sign =>
                     if CP = Character'Pos ('-') then
                        State_Id := Init_Extracting_Comment;

                        Comment_First_Index := (if P <= Contents'Last then
                                                   P
                                                else
                                                   Contents'Last);
                     else
                        Call_Result.Initialize (0473117530, -0541753044);
                        exit;
                     end if;
                  when Extracting_Start_Tag_Name =>
                     if CP = Character'Pos (' ') then
                        Start_Tag_Name_Last_Index := Prev_Prev_P;

                        This.Start_Tag
                          (Contents (Start_Tag_Name_First_Index ..
                               Start_Tag_Name_Last_Index),
                           Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth < Int32'Last then
                           Depth := Depth + 1;
                        else
                           Call_Result.Initialize (-1181908864, -0747101082);
                           exit;
                        end if;

                        State_Id := Expecting_G_Sign_Or_Attributes;
                     elsif CP = Character'Pos ('>') then
                        Start_Tag_Name_Last_Index := Prev_Prev_P;

                        This.Start_Tag
                          (Contents (Start_Tag_Name_First_Index ..
                               Start_Tag_Name_Last_Index),
                           Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth < Int32'Last then
                           Depth := Depth + 1;
                        else
                           Call_Result.Initialize (-1064425179, -1548059736);
                           exit;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);

                        State_Id := Expecting_New_Tag_Or_Tag_Value;
                     elsif Is_Special_Symbol (CP) then
                        Call_Result.Initialize (0175636358, -0993996303);
                        exit;
                     end if;
                  when Expecting_G_Sign_Or_Attributes =>
                     if
                       CP = Character'Pos (' ') or
                       CP = Character'Pos (Ada.Characters.Latin_1.LF) or
                       CP = Character'Pos (Ada.Characters.Latin_1.CR) or
                       CP = Character'Pos (Ada.Characters.Latin_1.HT)
                     then
                        null; -- Normal
                     elsif CP = Character'Pos ('>') then
                        State_Id := Expecting_New_Tag_Or_Tag_Value;

                        if P > Contents'Last then
                           Call_Result.Initialize (1631876148, 1445349781);
                           exit;
                        end if;

                        Tag_Value_First_Index := P;
                     elsif CP = Character'Pos ('/') then
                        State_Id
                          := Expecting_G_Sign_Or_Attributes_And_Found_Slash;
                     elsif not Is_Special_Symbol (CP) then
                        Attribute_First_Index := Prev_P;
                        State_Id := Extracting_Attribute_Name;
                     else
                        Call_Result.Initialize (-0820728822, -1954112046);
                        exit;
                     end if;
                  when Expecting_G_Sign_Or_Attributes_And_Found_Slash =>
                     if CP = Character'Pos ('>') then
                        State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

                        This.Text ("",
                                   Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        This.End_Tag (Contents (Start_Tag_Name_First_Index ..
                                        Start_Tag_Name_Last_Index),
                                      Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth > 0 then
                           Depth := Depth - 1;
                        else
                           Call_Result.Initialize (-1628495447, 2036006743);
                           exit;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);
                     else
                        Call_Result.Initialize (-0464941396, 0880131948);
                        exit;
                     end if;
                  when Extracting_Attribute_Name =>
                     if CP = Character'Pos ('=') then
                        Attribute_Last_Index := Prev_Prev_P;
                        State_Id := Expecting_Attribute_Value_Quotation_Mark;
                     elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                        Call_Result.Initialize (-0209983264, -1729179731);
                        exit;
                     elsif not Is_Special_Symbol (CP) then
                        null; -- Normal
                     else
                        Call_Result.Initialize (-1717807413, -1486938619);
                        exit;
                     end if;
                  when Expecting_Attribute_Value_Quotation_Mark =>
                     if CP = Character'Pos ('"') then
                        Expected_Quotation_Symbol := Double_Quotes;

                        Attribute_Value_First_Index
                          := (if P <= Contents'Last then
                                 P
                              else
                                 Contents'Last);
                        State_Id := Extracting_Attribute_Value;
                     elsif CP = Character'Pos (''') then
                        Expected_Quotation_Symbol := Single_Quotes;
                        Attribute_Value_First_Index
                          := (if P <= Contents'Last then
                                 P
                              else
                                 Contents'Last);
                        State_Id := Extracting_Attribute_Value;
                     else
                        Call_Result.Initialize (1311446946, 0430154116);
                        exit;
                     end if;
                  when Extracting_Attribute_Value =>
                     if
                       (CP = Character'Pos ('"')
                        and Expected_Quotation_Symbol = Double_Quotes)
                       or
                         (CP = Character'Pos (''')
                          and Expected_Quotation_Symbol = Single_Quotes)
                     then
                        Attribute_Value_Last_Index := Prev_Prev_P;
                        State_Id := Expecting_G_Sign_Or_Attributes;
                        declare
                           Name : constant String
                             := Contents (Attribute_First_Index ..
                                            Attribute_Last_Index);
                           Value : constant String
                             := Contents (Attribute_Value_First_Index ..
                                            Attribute_Value_Last_Index);
                        begin
                           This.Attribute (Name,
                                           Value,
                                           Call_Result);
                        end;

                        if Call_Result.Has_Failed then
                           exit;
                        end if;
                     elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                        Call_Result.Initialize (-0846218131, 1984049987);
                        exit;
                     end if;
                  when Expecting_New_Tag_Or_Tag_Value =>
                     if CP = Character'Pos ('<') then
                        State_Id := Expecting_New_Tag_Or_Tag_Value_And_Found_L;
                        Tag_Value_Last_Index := Prev_Prev_P;

                        This.Text (Contents (Tag_Value_First_Index ..
                                     Tag_Value_Last_Index),
                                   Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;
                     end if;
                  when Expecting_New_Tag_Or_Tag_Value_And_Found_L =>
                     if CP = Character'Pos ('/') then
                        if P > Contents'Last then
                           Call_Result.Initialize (0952221716, -1424188925);
                           exit;
                        end if;

                        State_Id := Extracting_End_Tag_Name;

                        End_Tag_Name_First_Index := P;
                     elsif CP = Character'Pos ('!') then
                        State_Id
                          := New_Tag_Or_Tag_Value_And_Found_L_And_Exclamation;
                     elsif Is_Special_Symbol (CP) then
                        Call_Result.Initialize (1584399066, 0904407776);
                        exit;
                     else
                        --  Will start parsing child tag!
                        State_Id := Extracting_Start_Tag_Name;
                        Start_Tag_Name_First_Index := Prev_P;
                     end if;
                  when New_Tag_Or_Tag_Value_And_Found_L_And_Exclamation =>
                     if CP = Character'Pos ('[') then
                        State_Id := New_Tag_Or_Tag_Value_But_Expecting_C;
                     elsif CP = Character'Pos ('-') then
                        State_Id
                          := New_Tag_Or_Tag_Value_And_L_And_Excl_And_Dash;
                     else
                        State_Id := Expecting_New_Tag_Or_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_But_Expecting_C =>
                     if CP = Character'Pos ('C') then
                        State_Id := New_Tag_Or_Tag_Value_But_Expecting_CD;
                     else
                        State_Id := Expecting_New_Tag_Or_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_But_Expecting_CD =>
                     if CP = Character'Pos ('D') then
                        State_Id := New_Tag_Or_Tag_Value_But_Expecting_CDA;
                     else
                        State_Id := Expecting_New_Tag_Or_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_But_Expecting_CDA =>
                     if CP = Character'Pos ('A') then
                        State_Id := New_Tag_Or_Tag_Value_But_Expecting_CDAT;
                     else
                        State_Id := Expecting_New_Tag_Or_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_But_Expecting_CDAT =>
                     if CP = Character'Pos ('T') then
                        State_Id := New_Tag_Or_Tag_Value_But_Expecting_CDATA;
                     else
                        State_Id := Expecting_New_Tag_Or_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_But_Expecting_CDATA =>
                     if CP = Character'Pos ('A') then
                        State_Id
                          := New_Tag_Or_Tag_Value_CDATA_And_Square_Bracket;
                     else
                        State_Id := Expecting_New_Tag_Or_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_CDATA_And_Square_Bracket =>
                     if CP = Character'Pos ('[') then
                        State_Id := Extracting_CDATA;
                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);
                     else
                        State_Id := Expecting_New_Tag_Or_Tag_Value;
                     end if;
                  when Extracting_CDATA =>
                     if CP = Character'Pos (']') then
                        Tag_Value_Last_Index := Prev_Prev_P;
                        State_Id := Extracting_CDATA_Found_Square_Bracket;
                     end if;
                  when Extracting_CDATA_Found_Square_Bracket =>
                     if CP = Character'Pos (']') then
                        State_Id := Extracting_CDATA_Found_Two_Square_Brackets;
                     else
                        State_Id := Extracting_CDATA;
                     end if;
                  when Extracting_CDATA_Found_Two_Square_Brackets =>
                     if CP = Character'Pos ('>') then
                        This.CDATA (Contents (Tag_Value_First_Index ..
                                      Tag_Value_Last_Index),
                                    Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);
                        State_Id := Expecting_New_Tag_Or_Tag_Value;
                     else
                        State_Id := Extracting_CDATA;
                     end if;
                  when Extracting_End_Tag_Name =>
                     if CP = Character'Pos ('>') then

                        End_Tag_Name_Last_Index := Prev_Prev_P;

                        This.End_Tag
                          (Contents (End_Tag_Name_First_Index ..
                               End_Tag_Name_Last_Index),
                           Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth > 0 then
                           Depth := Depth - 1;
                        else
                           Call_Result.Initialize (0732511655, -1496189046);
                           exit;
                        end if;

                        if Depth = 0 then
                           State_Id := Expecting_Only_Trailing_Spaces;
                        else
                           State_Id := Expecting_New_Tag_Or_Tag_Value;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);

                     elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                        Call_Result.Initialize (-0639636331, -0602633765);
                        exit;
                     elsif Is_Special_Symbol (CP) then
                        Call_Result.Initialize (-0319834221, 0769151931);
                        exit;
                     end if;
                  when New_Tag_Or_Tag_Value_And_L_And_Excl_And_Dash =>
                     if CP = Character'Pos ('-') then
                        Comment_First_Index := (if P <= Contents'Last then
                                                   P
                                                else
                                                   Contents'Last);
                        State_Id := Extracting_Comment;
                     else
                        State_Id := Expecting_New_Tag_Or_Tag_Value;
                     end if;
                  when Init_Extracting_Comment =>
                     if CP = Character'Pos ('-') then
                        State_Id := Init_Extracting_Comment_And_Found_Dash;
                     end if;
                  when Init_Extracting_Comment_And_Found_Dash =>
                     if CP = Character'Pos ('-') then
                        State_Id
                          := Init_Extracting_Comment_And_Found_Dash_Dash;
                     else
                        State_Id := Init_Extracting_Comment;
                     end if;
                  when Init_Extracting_Comment_And_Found_Dash_Dash =>
                     if CP = Character'Pos ('>') then
                        This.Comment
                          (Value       => Contents
                             (Comment_First_Index .. (P - 4)),
                           Call_Result => Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);
                        State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;
                     else
                        State_Id := Init_Extracting_Comment;
                     end if;
                  when Extracting_Comment =>
                     if CP = Character'Pos ('-') then
                        State_Id := Extracting_Comment_And_Found_Dash;
                     end if;
                  when Extracting_Comment_And_Found_Dash =>
                     if CP = Character'Pos ('-') then
                        State_Id := Extracting_Comment_And_Found_Dash_Dash;
                     else
                        State_Id := Extracting_Comment;
                     end if;
                  when Extracting_Comment_And_Found_Dash_Dash =>
                     if CP = Character'Pos ('>') then
                        This.Comment (Value       => Contents
                                      (Comment_First_Index .. (P - 4)),
                                      Call_Result => Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Value_First_Index := (if P <= Contents'Last then
                                                     P
                                                  else
                                                     Contents'Last);
                        State_Id := Expecting_New_Tag_Or_Tag_Value;
                     else
                        State_Id := Init_Extracting_Comment;
                     end if;
                  when Expecting_Only_Trailing_Spaces =>
                     if CP = Character'Pos (' ') or CP = 10 or CP = 13 then
                        null; -- Trailing spaces are OK
                     else
                        Call_Result.Initialize (-1239181029, 1698286444);
                        exit;
                     end if;
               end case;
            end loop;

            if
              (not Call_Result.Has_Failed) and then
              State_Id /= Expecting_Only_Trailing_Spaces
            then
               Call_Result.Initialize (-2068412437, -0002457258);
            end if;
         else
            Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
         end if;
      else
         Call_Result.Initialize (-1969620808, -0689239741);
      end if;
      P_Value := P;
   end Analyze_XML;

   --  Known unsupported issues: Escaping of text (for example &amp;)
   --  The stack roof may be hit if the comments and texts in the XML are HUGE.
   --  It should not be an issue in general.
   procedure Parse
     (This        : in out SAX_Parser_T'Class;
      Contents    : String;
      Call_Result : in out Subprogram_Call_Result)
   is
      subtype P_T      is Integer range Contents'First .. Contents'Last + 4;

      Initial_State_Id : Initial_State_Id_T := Less_Sign;

      P : P_T := Contents'First;

      CP : UTF8.Code_Point;
   begin
      while P <= Contents'Last loop
         exit when Initial_State_Id = End_State;

         if not UTF8.Is_Valid_UTF8_Code_Point (Source  => Contents,
                                               Pointer => P)
         then
            Call_Result.Initialize (-0106955593, 0277648992);
            exit;
         end if;

         UTF8.Get (Source  => Contents,
                   Pointer => P,
                   Value   => CP);

         pragma Loop_Variant (Increases => P);
         pragma Loop_Invariant (not Call_Result.Has_Failed);
         pragma Loop_Invariant
           (Initial_State_Id /= Less_Sign or
              (Initial_State_Id = Less_Sign and then
                   (P > Contents'First and Contents'Last >= Contents'First)));
         pragma Loop_Invariant
           (Initial_State_Id /= Question_Mark or
              (Initial_State_Id = Question_Mark and then
                   (P > Contents'First + 1 and
                        Contents'Last >= Contents'First + 1)));
         pragma Loop_Invariant
           (Initial_State_Id /= X or
              (Initial_State_Id = X and then
                   (P > Contents'First + 2 and
                        Contents'Last >= Contents'First + 2)));
         pragma Loop_Invariant
           (Initial_State_Id /= XM or
              (Initial_State_Id = XM and then
                   (P > Contents'First + 3 and
                        Contents'Last >= Contents'First + 3)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML or
              (Initial_State_Id = XML and then
                   (P > Contents'First + 4 and
                        Contents'Last >= Contents'First + 4)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S or
              (Initial_State_Id = XML_S and then
                   (P > Contents'First + 5 and
                        Contents'Last >= Contents'First + 5)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_V or
              (Initial_State_Id = XML_S_V and then
                   (P > Contents'First + 6 and
                        Contents'Last >= Contents'First + 6)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VE or
              (Initial_State_Id = XML_S_VE and then
                   (P > Contents'First + 7 and
                        Contents'Last >= Contents'First + 7)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VER or
              (Initial_State_Id = XML_S_VER and then
                   (P > Contents'First + 8 and
                        Contents'Last >= Contents'First + 8)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERS or
              (Initial_State_Id = XML_S_VERS and then
                   (P > Contents'First + 9 and
                        Contents'Last >= Contents'First + 9)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSI or
              (Initial_State_Id = XML_S_VERSI and then
                   (P > Contents'First + 10 and
                        Contents'Last >= Contents'First + 10)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSIO or
              (Initial_State_Id = XML_S_VERSIO and then
                   (P > Contents'First + 11 and
                        Contents'Last >= Contents'First + 11)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION or
              (Initial_State_Id = XML_S_VERSION and then
                   (P > Contents'First + 12 and
                        Contents'Last >= Contents'First + 12)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E or
              (Initial_State_Id = XML_S_VERSION_E and then
                   (P > Contents'First + 13 and
                        Contents'Last >= Contents'First + 13)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q or
              (Initial_State_Id = XML_S_VERSION_E_Q and then
                   (P > Contents'First + 14 and
                        Contents'Last >= Contents'First + 14)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1 or
              (Initial_State_Id = XML_S_VERSION_E_Q_1 and then
                   (P > Contents'First + 15 and
                        Contents'Last >= Contents'First + 15)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P and then
                   (P > Contents'First + 16 and
                        Contents'Last >= Contents'First + 16)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0 or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P_0 and then
                   (P > Contents'First + 17 and
                        Contents'Last >= Contents'First + 17)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P_0_Q and then
                   (P > Contents'First + 18 and
                        Contents'Last >= Contents'First + 18)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P_0_Q_S and then
                   (P > Contents'First + 19 and
                        Contents'Last >= Contents'First + 19)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_E or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P_0_Q_S_E and then
                   (P > Contents'First + 20 and
                        Contents'Last >= Contents'First + 20)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_EN or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P_0_Q_S_EN and then
                   (P > Contents'First + 21 and
                        Contents'Last >= Contents'First + 21)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_ENC or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P_0_Q_S_ENC and then
                   (P > Contents'First + 22 and
                        Contents'Last >= Contents'First + 22)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO and then
                   (P > Contents'First + 23 and
                        Contents'Last >= Contents'First + 23)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD and then
                   (P > Contents'First + 24 and
                        Contents'Last >= Contents'First + 24)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI and then
                   (P > Contents'First + 25 and
                        Contents'Last >= Contents'First + 25)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN and then
                   (P > Contents'First + 26 and
                        Contents'Last >= Contents'First + 26)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING or
              (Initial_State_Id = XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING and then
                   (P > Contents'First + 27 and
                        Contents'Last >= Contents'First + 27)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E or
              (Initial_State_Id =
                   XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E and then
                   (P > Contents'First + 28 and
                        Contents'Last >= Contents'First + 28)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q or
              (Initial_State_Id =
                   XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q and then
                   (P > Contents'First + 29 and
                        Contents'Last >= Contents'First + 29)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U or
              (Initial_State_Id =
                   XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U and then
                   (P > Contents'First + 30 and
                        Contents'Last >= Contents'First + 30)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT or
              (Initial_State_Id =
                   XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT and then
                   (P > Contents'First + 31 and
                        Contents'Last >= Contents'First + 31)));
         pragma Loop_Invariant
           (Initial_State_Id /= XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF or
              (Initial_State_Id =
                   XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF and then
                   (P > Contents'First + 32 and
                        Contents'Last >= Contents'First + 32)));
         pragma Loop_Invariant
           (Initial_State_Id /=
              XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D or
              (Initial_State_Id =
                   XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D and then
                 (P > Contents'First + 33 and
                      Contents'Last >= Contents'First + 33)));
         pragma Loop_Invariant
           (Initial_State_Id /=
              XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8 or
                (Initial_State_Id =
                     XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8 and then
                   (P > Contents'First + 34 and
                        Contents'Last >= Contents'First + 34)));
         pragma Loop_Invariant
           (Initial_State_Id /=
              XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q or
                (Initial_State_Id =
                     XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q and then
                   (P > Contents'First + 35 and
                          Contents'Last >= Contents'First + 35)));
         pragma Loop_Invariant
           (Initial_State_Id /=
              XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q_QM or
                (Initial_State_Id =
                     XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q_QM and then
                   (P > Contents'First + 36 and
                          Contents'Last >= Contents'First + 36)));

--           Ada.Text_IO.Put ("Extracted:");
--           Ada.Text_IO.Put (Image (CP));
--           Ada.Text_IO.Put (", state ");
--           Ada.Text_IO.Put_Line (Initial_State_Id'Image);
--           Ada.Text_IO.Put (Image (CP));

         case Initial_State_Id is
         when End_State => null;
         when Less_Sign =>
            if CP = Character'Pos (' ') then
               null;
            elsif CP = Character'Pos ('<') then
               Initial_State_Id := Question_Mark;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Question_Mark =>
            if CP = Character'Pos ('?') then
               Initial_State_Id := X;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when X =>
            if CP = Character'Pos ('x') then
               Initial_State_Id := XM;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XM =>
            if CP = Character'Pos ('m') then
               Initial_State_Id := XML;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML =>
            if CP = Character'Pos ('l') then
               Initial_State_Id := XML_S;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S =>
            if CP = Character'Pos (' ') then
               Initial_State_Id := XML_S_V;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_V =>
            if CP = Character'Pos ('v') then
               Initial_State_Id := XML_S_VE;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VE =>
            if CP = Character'Pos ('e') then
               Initial_State_Id := XML_S_VER;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VER =>
            if CP = Character'Pos ('r') then
               Initial_State_Id := XML_S_VERS;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERS =>
            if CP = Character'Pos ('s') then
               Initial_State_Id := XML_S_VERSI;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSI =>
            if CP = Character'Pos ('i') then
               Initial_State_Id := XML_S_VERSIO;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSIO =>
            if CP = Character'Pos ('o') then
               Initial_State_Id := XML_S_VERSION;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION =>
            if CP = Character'Pos ('n') then
               Initial_State_Id := XML_S_VERSION_E;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E =>
            if CP = Character'Pos ('=') then
               Initial_State_Id := XML_S_VERSION_E_Q;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := XML_S_VERSION_E_Q_1;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1 =>
            if CP = Character'Pos ('1') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P =>
            if CP = Character'Pos ('.') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0 =>
            if CP = Character'Pos ('0') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S =>
            if CP = Character'Pos (' ') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_E;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_E =>
            if CP = Character'Pos ('e') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_EN;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_EN =>
            if CP = Character'Pos ('n') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENC;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC =>
            if CP = Character'Pos ('c') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO =>
            if CP = Character'Pos ('o') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD =>
            if CP = Character'Pos ('d') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI =>
            if CP = Character'Pos ('i') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN =>
            if CP = Character'Pos ('n') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING =>
            if CP = Character'Pos ('g') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E =>
            if CP = Character'Pos ('=') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U =>
            if CP = Character'Pos ('u') or CP = Character'Pos ('U') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT =>
            if CP = Character'Pos ('t') or CP = Character'Pos ('T') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF =>
            if CP = Character'Pos ('f') or CP = Character'Pos ('F') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D =>
            if CP = Character'Pos ('-') then
               Initial_State_Id := XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8 =>
            if CP = Character'Pos ('8') then
               Initial_State_Id
                 := XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q =>
            if CP = Character'Pos ('"') then
               Initial_State_Id
                 := XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q_QM;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q_QM =>
            if CP = Character'Pos (' ') then
               null;
            elsif CP = Character'Pos ('?') then
               if P <= Contents'Last then
                  Initial_State_Id := End_State;

                  pragma Assert (P > Contents'First + 36);

                  This.Analyze_XML (P, Contents, Call_Result);
               else
                  Call_Result.Initialize (0279374352, 1601495668);
                  exit;
               end if;
            elsif CP = Character'Pos ('s') then
               Initial_State_Id := Optional_S;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_S =>
            if CP = Character'Pos ('t') then
               Initial_State_Id := Optional_ST;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_ST =>
            if CP = Character'Pos ('a') then
               Initial_State_Id := Optional_STA;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STA =>
            if CP = Character'Pos ('n') then
               Initial_State_Id := Optional_STAN;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STAN =>
            if CP = Character'Pos ('d') then
               Initial_State_Id := Optional_STAND;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STAND =>
            if CP = Character'Pos ('a') then
               Initial_State_Id := Optional_STANDA;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STANDA =>
            if CP = Character'Pos ('l') then
               Initial_State_Id := Optional_STANDAL;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STANDAL =>
            if CP = Character'Pos ('o') then
               Initial_State_Id := Optional_STANDALO;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STANDALO =>
            if CP = Character'Pos ('n') then
               Initial_State_Id := Optional_STANDALON;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STANDALON =>
            if CP = Character'Pos ('e') then
               Initial_State_Id := Optional_STANDALONE;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STANDALONE =>
            if CP = Character'Pos ('=') then
               Initial_State_Id := Optional_STANDALONE_E;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STANDALONE_E =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := Optional_STANDALONE_E_QM;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STANDALONE_E_QM =>
            if CP = Character'Pos ('y') then
               Initial_State_Id := Optional_STANDALONE_E_QM_Y;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STANDALONE_E_QM_Y =>
            if CP = Character'Pos ('e') then
               Initial_State_Id := Optional_STANDALONE_E_QM_YE;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STANDALONE_E_QM_YE =>
            if CP = Character'Pos ('s') then
               Initial_State_Id := Optional_STANDALONE_E_QM_YES;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STANDALONE_E_QM_YES =>
            if CP = Character'Pos ('"') then
               Initial_State_Id := Optional_STANDALONE_E_QM_YES_QM;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         when Optional_STANDALONE_E_QM_YES_QM =>
            if CP = Character'Pos (' ') then
               null;
            elsif CP = Character'Pos ('?') then
               if P <= Contents'Last then
                  Initial_State_Id := End_State;

                  This.Analyze_XML (P, Contents, Call_Result);
               else
                  Call_Result.Initialize (0279464352, 1631499668);
                  exit;
               end if;
            else
               Call_Result.Initialize (ERROR_CODE_1, ERROR_CODE_2);
               exit;
            end if;
         end case;
      end loop;
   end Parse;

end XML_SAX_Parser;
