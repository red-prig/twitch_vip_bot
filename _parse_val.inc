
Procedure _LowerCase(FStr:PByte;FLen:SizeUInt); inline;
begin
 While (FLen<>0) do
 begin
  if Byte(FStr^-Byte($41))<Byte($1A) then
   FStr^:=FStr^ or Byte($20);
  Inc(FStr);
  Dec(FLen);
 end;
end;

{
function _LoCaseByte(Data:PByte):Byte; inline;
begin
 Result:=Data^;
 if Byte(Result-Byte($41))<Byte($1A) then
  Result:=Result or Byte($20);
end;

function _LoCaseWord(Data:PByte):Word; inline;
begin
 Result:=PWORD(Data)^;
 _LowerCase(@Result,2);
end;

function _LoCase3Byte(Data:PByte):DWord; inline;
begin
 Result:=PDWord(Data)^ and $FFFFFF;
 _LowerCase(@Result,3);
end;

function _LoCaseDWord(Data:PByte):DWord; inline;
begin
 Result:=PDWord(Data)^;
 _LowerCase(@Result,4);
end;

function _LoCase7Byte(Data:PByte):QWord; inline;
begin
 Result:=PQWord(Data)^ and $FFFFFFFFFFFFFF;
 _LowerCase(@Result,7);
end;
}

function _LoCaseQWord(Data:PByte):QWord; inline;
begin
 Result:=PQWord(Data)^;
 _LowerCase(@Result,8);
end;

function _OrCaseWord(Data:PByte):Word; inline;
begin
 Result:=PWORD(Data)^ or $2020;
end;

function _OrCase3Byte(Data:PByte):DWord; inline;
begin
 Result:=(PDWord(Data)^ and $FFFFFF) or $202020;
end;

function _OrCaseDWord(Data:PByte):DWord; inline;
begin
 Result:=PDWord(Data)^ or $20202020;
end;

function _OrCase7Byte(Data:PByte):QWord; inline;
begin
 Result:=(PQWord(Data)^ and $FFFFFFFFFFFFFF) or $20202020202020;
end;

function _OrCaseQWord(Data:PByte):QWord; inline;
begin
 Result:=PQWord(Data)^ or $2020202020202020;
end;

function _get_const_keyword(data:Pointer;len:SizeUInt):SizeInt;
begin
 Result:=-1;
 if (data=nil) or (len=0) then Exit;
 Case len of
  4:Case _OrCaseDWord(Data) of
      $6C6C756E:Result:=0; //null
      $65757274:Result:=1; //true
    end;
  5:Case _OrCaseDWord(Data) of
      $736C6166: //fals
                Case PByte(Data)[4] of
                  $65, //e
                  $45:Result:=2; //E
                end;
    end;
 end;
end;

function _get_op_id(data:Pointer;len:SizeUInt):SizeInt;
begin
 Result:=-1;
 if (data=nil) or (len=0) then Exit;
 Case len of
  1:Case PByte(Data)^ of
      $25:Result:=24; //%
      $26:Result:=19; //&
      $28:Result:=36; //(
      $29:Result:=37; //)
      $2A:Result:=22; //*
      $2B:Result:=17; //+
      $2D:Result:=18; //-
      $2F:Result:=23; ///
      $3C:Result:=16; //<
      $3D:Result:=8; //=
      $3E:Result:=15; //>
      $5E:Result:=20; //^
      $7C:Result:=21; //|
      $7E:Result:=25; //~
    end;
  2:Case PWord(Data)^ of
      $3C21:Result:=13; //!<
      $3D21:Result:=12; //!=
      $3E21:Result:=14; //!>
      $3D25:Result:=4; //%=
      $3D26:Result:=5; //&=
      $3D2A:Result:=2; //*=
      $3D2B:Result:=0; //+=
      $3D2D:Result:=1; //-=
      $3D2F:Result:=3; ///=
      $3D3C:Result:=10; //<=
      $3E3C:Result:=11; //<>
      $3D3E:Result:=9; //>=
      $3D5E:Result:=6; //^=
      $6E69, //in
      $4E69, //iN
      $6E49, //In
      $4E49:Result:=29; //IN
      $7369, //is
      $5369, //iS
      $7349, //Is
      $5349:Result:=35; //IS
      $726F, //or
      $526F, //oR
      $724F, //Or
      $524F:Result:=28; //OR
      $3D7C:Result:=7; //|=
    end;
  3:Case _OrCase3Byte(Data) of
      $6C6C61:Result:=30; //all
      $646E61:Result:=27; //and
      $796E61:Result:=31; //any
      $746F6E:Result:=26; //not
    end;
  4:Case _OrCaseDWord(Data) of
      $656B696C:Result:=34; //like
      $656D6F73:Result:=32; //some
    end;
  7:Case _OrCase7Byte(Data) of
      $6E656577746562:Result:=33; //between
    end;
 end;
end;

function _fetch_op1(data:Pointer;len:SizeUInt):SizeInt;
begin
 Result:=-1;
 if (data=nil) or (len=0) then Exit;
 Case len of
  2:Case _OrCaseWord(Data) of
      $7361:Result:=3; //as
    end;
  6:Case _OrCaseDWord(Data) of
      $73727563: //curs
                Case _OrCaseWord(Data+4) of
                  $726F:Result:=2; //or
                end;
      $6F726373: //scro
                Case _OrCaseWord(Data+4) of
                  $6C6C:Result:=1; //ll
                end;
    end;
  11:Case _OrCaseQWord(Data) of
       $7469736E65736E69: //insensit
                         Case _OrCase3Byte(Data+8) of
                           $657669:Result:=0; //ive
                         end;
     end;
 end;
end;

function _is_global_word(data:Pointer;len:SizeUInt):Boolean;
begin
 Result:=false;
 if (data=nil) or (len=0) then Exit;
 if len=6 then
  Case _OrCaseDWord(Data) of
    $626F6C67: //glob
              Case _OrCaseWord(Data+4) of
                $6C61:Result:=true; //al
              end;
  end;
end;

function _fetch_op2(data:Pointer;len:SizeUInt):SizeInt;

 function __5CaseQWord(Data:PByte):QWord; inline;
 begin
  Result:=PQWord(Data)^ or $2020200020202020;
  if Byte(Byte(Result shr 32)-Byte($41))<Byte($1A) then
   Result:=Result or $2000000000;
 end;

begin
 Result:=-1;
 if (data=nil) or (len=0) then Exit;
 Case len of
  3:Case _OrCase3Byte(Data) of
      $726F66:Result:=12; //for
    end;
  5:Case _OrCaseDWord(Data) of
      $61636F6C: //loca
                Case PByte(Data)[4] of
                  $6C, //l
                  $4C:Result:=0; //L
                end;
    end;
  6:Case _OrCaseDWord(Data) of
      $626F6C67: //glob
                Case _OrCaseWord(Data+4) of
                  $6C61:Result:=1; //al
                end;
      $7379656B: //keys
                Case _OrCaseWord(Data+4) of
                  $7465:Result:=5; //et
                end;
      $6F726373: //scro
                Case _OrCaseWord(Data+4) of
                  $6C6C:Result:=3; //ll
                end;
      $74617473: //stat
                Case _OrCaseWord(Data+4) of
                  $6369:Result:=4; //ic
                end;
    end;
  7:Case _OrCase7Byte(Data) of
      $63696D616E7964:Result:=6; //dynamic
    end;
  9:Case __5CaseQWord(Data) of
      $6C6E6F5F64616572: //read_onl
                        Case PByte(Data)[8] of
                          $79, //y
                          $59:Result:=8; //Y
                        end;
    end;
  10:Case _OrCaseQWord(Data) of
       $7473696D6974706F: //optimist
                         Case _OrCaseWord(Data+8) of
                           $6369:Result:=10; //ic
                         end;
     end;
  12:Case _LoCaseQWord(Data) of
       $726F665F74736166: //fast_for
                         Case _OrCaseDWord(Data+8) of
                           $64726177:Result:=7; //ward
                         end;
       $5F64726177726F66: //forward_
                         Case _OrCaseDWord(Data+8) of
                           $796C6E6F:Result:=2; //only
                         end;
       $6C5F6C6C6F726373: //scroll_l
                         Case _OrCaseDWord(Data+8) of
                           $736B636F:Result:=9; //ocks
                         end;
       $7261775F65707974: //type_war
                         Case _OrCaseDWord(Data+8) of
                           $676E696E:Result:=11; //ning
                         end;
     end;
 end;
end;

function _GetDataTypeId(data:Pointer;len:SizeUInt):SizeInt;
begin
 Result:=-1;
 if (data=nil) or (len=0) then Exit;
 Case len of
  3:Case _OrCase3Byte(Data) of
      $746962:Result:=6; //bit
      $746E69:Result:=5; //int
    end;
  4:Case _OrCaseDWord(Data) of
      $72616863:Result:=17; //char
      $65746164:Result:=11; //date
      $6C616572:Result:=10; //real
      $74786574:Result:=19; //text
      $656D6974:Result:=16; //time
    end;
  5:Case _OrCaseDWord(Data) of
      $616F6C66: //floa
                Case PByte(Data)[4] of
                  $74, //t
                  $54:Result:=9; //T
                end;
      $656E6F6D: //mone
                Case PByte(Data)[4] of
                  $79, //y
                  $59:Result:=8; //Y
                end;
      $6168636E: //ncha
                Case PByte(Data)[4] of
                  $72, //r
                  $52:Result:=20; //R
                end;
      $7865746E: //ntex
                Case PByte(Data)[4] of
                  $74, //t
                  $54:Result:=22; //T
                end;
    end;
  6:Case _OrCaseDWord(Data) of
      $69676962: //bigi
                Case _OrCaseWord(Data+4) of
                  $746E:Result:=2; //nt
                end;
    end;
  7:Case _OrCase7Byte(Data) of
      $6C616D69636564:Result:=1; //decimal
      $636972656D756E:Result:=0; //numeric
      $746E69796E6974:Result:=4; //tinyint
      $72616863726176:Result:=18; //varchar
    end;
  8:Case _OrCaseQWord(Data) of
      $656D697465746164:Result:=15; //datetime
      $726168637261766E:Result:=21; //nvarchar
      $746E696C6C616D73:Result:=3; //smallint
    end;
  9:Case _OrCaseQWord(Data) of
      $656D697465746164: //datetime
                        Case PByte(Data)[8] of
                          $32:Result:=13; //2
                        end;
    end;
  10:Case _OrCaseQWord(Data) of
       $6E6F6D6C6C616D73: //smallmon
                         Case _OrCaseWord(Data+8) of
                           $7965:Result:=7; //ey
                         end;
     end;
  13:Case _OrCaseQWord(Data) of
       $7461646C6C616D73: //smalldat
                         Case _OrCaseDWord(Data+8) of
                           $6D697465: //etim
                                     Case PByte(Data)[12] of
                                       $65, //e
                                       $45:Result:=14; //E
                                     end;
                         end;
     end;
  14:Case _OrCaseQWord(Data) of
       $656D697465746164: //datetime
                         Case _OrCaseDWord(Data+8) of
                           $7366666F: //offs
                                     Case _OrCaseWord(Data+12) of
                                       $7465:Result:=12; //et
                                     end;
                         end;
     end;
  16:Case _OrCaseQWord(Data) of
       $6469657571696E75: //uniqueid
                         Case _OrCaseQWord(Data+8) of
                           $7265696669746E65:Result:=23; //entifier
                         end;
     end;
 end;
end;

function _get_fetch_type(data:Pointer;len:SizeUInt):SizeInt;
begin
 Result:=-1;
 if (data=nil) or (len=0) then Exit;
 Case len of
  4:Case _OrCaseDWord(Data) of
      $7473616C:Result:=3; //last
      $7478656E:Result:=0; //next
    end;
  5:Case _OrCaseDWord(Data) of
      $73726966: //firs
                Case PByte(Data)[4] of
                  $74, //t
                  $54:Result:=2; //T
                end;
      $6F697270: //prio
                Case PByte(Data)[4] of
                  $72, //r
                  $52:Result:=1; //R
                end;
    end;
  8:Case _OrCaseQWord(Data) of
      $6574756C6F736261:Result:=4; //absolute
      $65766974616C6572:Result:=5; //relative
    end;
 end;
end;

function _is_from_word(data:Pointer;len:SizeUInt):Boolean;
begin
 Result:=false;
 if (data=nil) or (len=0) then Exit;
 Case len of
  4:Case _OrCaseDWord(Data) of
      $6D6F7266:Result:=true; //from
    end;
 end;
end;

function _is_into_word(data:Pointer;len:SizeUInt):Boolean;
begin
 Result:=false;
 if (data=nil) or (len=0) then Exit;
 Case len of
  4:Case _OrCaseDWord(Data) of
      $6F746E69:Result:=true; //into
    end;
 end;
end;

function _is_select_word(data:Pointer;len:SizeUInt):Boolean;
begin
 Result:=false;
 if (data=nil) or (len=0) then Exit;
 Case len of
  6:Case _OrCaseDWord(Data) of
      $656C6573: //sele
                Case _OrCaseWord(Data+4) of
                  $7463:Result:=true; //ct
                end;
    end;
 end;
end;

function _get_begin_op(data:Pointer;len:SizeUInt):SizeInt;
begin
 Result:=-1;
 if (data=nil) or (len=0) then Exit;
 Case len of
  3:Case _OrCase3Byte(Data) of
      $797274:Result:=2; //try
    end;
  4:Case _OrCaseDWord(Data) of
      $6E617274:Result:=0; //tran
    end;
  5:Case _OrCaseDWord(Data) of
      $63746163: //catc
                Case PByte(Data)[4] of
                  $68, //h
                  $48:Result:=3; //H
                end;
    end;
  11:Case _OrCaseQWord(Data) of
       $746361736E617274: //transact
                         Case _OrCase3Byte(Data+8) of
                           $6E6F69:Result:=1; //ion
                         end;
     end;
 end;
end;

function _get_cmd_keyword(data:Pointer;len:SizeUInt):SizeInt;
begin
 Result:=-1;
 if (data=nil) or (len=0) then Exit;
 Case len of
  2:Case _OrCaseWord(Data) of
     $6F67:Result:=6;  //go
     $6669:Result:=17; //if
    end;
  3:Case _OrCase3Byte(Data) of
     $646E65:Result:=9; //end
     $746573:Result:=0; //set
     $657375:Result:=7; //use
    end;
  4:Case _OrCaseDWord(Data) of
     $706F7264:Result:=32; //drop
     $65736C65:Result:=18; //else
     $63657865:Result:=27; //exec
     $6F746F67:Result:=15; //goto
     $6E65706F:Result:=3; //open
     $65766173:Result:=29; //save
    end;
  5:Case _OrCaseDWord(Data) of
     $65746C61: //alte
                Case PByte(Data)[4] of
                  $72, //r
                  $52:Result:=30; //R
                end;
      $69676562: //begi
                Case PByte(Data)[4] of
                  $6E, //n
                  $4E:Result:=8; //N
                end;
      $61657262: //brea
                Case PByte(Data)[4] of
                  $6B, //k
                  $4B:Result:=12; //K
                end;
      $736F6C63: //clos
                Case PByte(Data)[4] of
                  $65, //e
                  $45:Result:=4; //E
                end;
      $63746566: //fetc
                Case PByte(Data)[4] of
                  $68, //h
                  $48:Result:=5; //H
                end;
      $6E697270: //prin
                Case PByte(Data)[4] of
                  $74, //t
                  $54:Result:=20; //T
                end;
      $6F726874: //thro
                Case PByte(Data)[4] of
                  $77, //w
                  $57:Result:=21; //W
                end;
      $6C696877: //whil
                Case PByte(Data)[4] of
                  $65, //e
                  $45:Result:=14; //E
                end;
    end;
  6:Case _OrCaseDWord(Data) of
     $6D6D6F63: //comm
               Case _OrCaseWord(Data+4) of
                $7469:Result:=10; //it
               end;
     $61657263: //crea
               Case _OrCaseWord(Data+4) of
                $6574:Result:=31; //te
               end;
     $656C6564: //dele
               Case _OrCaseWord(Data+4) of
                $6574:Result:=24; //te
               end;
     $65736E69: //inse
               Case _OrCaseWord(Data+4) of
                $7472:Result:=25; //rt
               end;
     $75746572: //retu
               Case _OrCaseWord(Data+4) of
                $6E72:Result:=19; //rn
               end;
     $656C6573: //sele
               Case _OrCaseWord(Data+4) of
                $7463:Result:=23; //ct
               end;
     $61647075: //upda
               Case _OrCaseWord(Data+4) of
                $6574:Result:=26; //te
               end;
    end;
  7:Case _OrCase7Byte(Data) of
     $6572616C636564:Result:=1;  //declare
     $65747563657865:Result:=28; //execute
     $726F6674696177:Result:=16; //waitfor
    end;
  8:Case _OrCaseQWord(Data) of
     $65756E69746E6F63:Result:=13; //continue
     $6B6361626C6C6F72:Result:=11; //rollback
    end;
  9:Case _OrCaseQWord(Data) of
     $6F72726573696172: //raiserro
                        Case PByte(Data)[8] of
                          $72, //r
                          $52:Result:=22; //R
                        end;
    end;
  10:Case _OrCaseQWord(Data) of
      $61636F6C6C616564: //dealloca
                        Case _OrCaseWord(Data+8) of
                         $6574:Result:=2; //te
                        end;
     end;
 end;
end;

