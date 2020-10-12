{------------------------------------------------------------------------------}
{  Standard built-in formula support                                           }
{------------------------------------------------------------------------------}

unit fpsfunc;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, fpstypes;

procedure RegisterStdBuiltins(AManager: TComponent);


implementation

uses
  Math, lazutf8, StrUtils, DateUtils,
  xlsconst, {%H-}fpsPatches, fpsUtils,
  fpsnumformat, fpspreadsheet, fpsexprparser;


{------------------------------------------------------------------------------}
{   Builtin math functions                                                     }
{------------------------------------------------------------------------------}

procedure fpsABS(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(abs(x));
end;

procedure fpsACOS(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
  if InRange(x, -1, +1) then
    Result := FloatResult(arccos(x))
  else
    Result := ErrorResult(errOverflow);   // #NUM!
end;

procedure fpsACOSH(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
  if x >= 1 then
    Result := FloatResult(arccosh(x))
  else
    Result := ErrorResult(errOverflow);
end;

procedure fpsASIN(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
  if InRange(x, -1, +1) then
    Result := FloatResult(arcsin(x))
  else
    Result := ErrorResult(errOverflow);
end;

procedure fpsASINH(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(arcsinh(x));
end;

procedure fpsATAN(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(arctan(x));
end;

procedure fpsATANH(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
  if (x > -1) and (x < +1) then
    Result := FloatResult(arctanh(x))
  else
    Result := ErrorResult(errOverflow);   // #NUM!
end;

procedure fpsCEILING(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// CEILING( number, significance )
// returns a number rounded up to a multiple of significance
var
  num, sig: TsExprFloat;
begin
  num := ArgToFloat(Args[0]);
  sig := ArgToFloat(Args[1]);
  if IsNaN(num) or IsNaN(sig) then
    Result := ErrorResult(errWrongType)
  else
  if sig = 0 then
    Result := ErrorResult(errDivideByZero)
  else
    Result := FloatResult(ceil(num/sig)*sig);
end;

procedure fpsCOS(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(cos(x));
end;

procedure fpsCOSH(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(cosh(x));
end;

procedure fpsDEGREES(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(RadToDeg(x));
end;

procedure fpsEVEN(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// EVEN( number )
// rounds a number up to the nearest even integer.
// If the number is negative, the number is rounded away from zero.
var
  x: TsExprFloat;
  n: Integer;
begin
  if Args[0].ResultType in [rtCell, rtInteger, rtFloat, rtDateTime, rtEmpty] then begin
    x := ArgToFloat(Args[0]);
    if IsNaN(x) then
      Result := ErrorResult(errWrongType)
    else
    if x > 0 then
    begin
      n := Trunc(x) + 1;
      if odd(n) then inc(n);
    end else
    if x < 0 then
    begin
      n := Trunc(x) - 1;
      if odd(n) then dec(n);
    end else
      n := 0;
    Result := IntegerResult(n);
  end
  else
    Result := ErrorResult(errWrongType);
end;

procedure fpsEXP(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(exp(x));
end;

procedure fpsFACT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// FACT( number )
//  returns the factorial of a number.
var
  res: TsExprFloat;
  i, n: Integer;
begin
  if Args[0].ResultType in [rtCell, rtInteger, rtFloat, rtEmpty, rtDateTime] then
  begin
    res := 1.0;
    n := ArgToInt(Args[0]);
    if n < 0 then
      Result := ErrorResult(errOverflow)
    else
      try
        for i:=1 to n do
          res := res * i;
        Result := FloatResult(res);
      except on E: EFPSpreadsheet do
        Result := ErrorResult(errOverflow);
      end;
  end else
    Result := ErrorResult(errWrongType);
end;

procedure fpsFLOOR(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// FLOOR( number, significance )
// returns a number rounded down to a multiple of significance
var
  num, sig: TsExprFloat;
begin
  num := ArgToFloat(Args[0]);
  sig := ArgToFloat(Args[1]);
  if IsNaN(num) or IsNaN(sig) then
    Result := ErrorResult(errWrongType)
  else
  if sig = 0 then
    Result := ErrorResult(errDivideByZero)
  else
    Result := FloatResult(floor(num/sig)*sig);
end;

procedure fpsINT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(floor(x));
end;

procedure fpsLN(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
  if x > 0 then
    Result := FloatResult(ln(x))
  else
    Result := ErrorResult(errOverflow);   // #NUM!
end;

procedure fpsLOG(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// LOG( number [, base] )  -  base is 10 if omitted.
var
  x: TsExprFloat;
  base: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then begin
    Result := ErrorResult(errWrongType);
    exit;
  end;

  if x <= 0 then begin
    Result := ErrorResult(errOverflow);  // #NUM!
    exit;
  end;

  if (Length(Args) = 2) then
  begin
    if (Args[1].ResultType = rtMissingArg) then
    begin
      Result := ErrorResult(errOverflow);  // #NUM! as tested by Excel
      exit;
    end;
    base := ArgToFloat(Args[1]);
    if IsNaN(base) then begin
      Result := ErrorResult(errWrongType);
      exit;
    end;
    if base < 0 then begin
      Result := ErrorResult(errOverflow);  // #NUM!
      exit;
    end;
  end else
    base := 10;

  Result := FloatResult(logn(base, x));
end;

procedure fpsLOG10(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)    // #VALUE!
  else
  if x > 0 then
    Result := FloatResult(log10(x))
  else
    Result := ErrorResult(errOverflow);   // #NUM!
end;

procedure fpsMOD(var Result: TsExpressionResult; const Args: TsExprParameterArray);
//  MOD( number, divisor )
// Returns the remainder after a number is divided by a divisor.
var
  n, m: Integer;
begin
  n := ArgToInt(Args[0]);
  m := ArgToInt(Args[1]);
  if m = 0 then
    Result := ErrorResult(errDivideByZero)
  else
    Result := IntegerResult(n mod m);
end;

procedure fpsODD(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// ODD( number )
// rounds a number up to the nearest odd integer.
// If the number is negative, the number is rounded away from zero.
var
  x: TsExprFloat;
  n: Integer;
begin
  if Args[0].ResultType in [rtCell, rtInteger, rtFloat, rtDateTime, rtEmpty] then
  begin
    x := ArgToFloat(Args[0]);
    if IsNaN(x) then
      Result := ErrorResult(errWrongType)
    else
    if x >= 0 then
    begin
      n := Trunc(x) + 1;
      if not odd(n) then inc(n);
    end else
    begin
      n := Trunc(x) - 1;
      if not odd(n) then dec(n);
    end;
    Result := IntegerResult(n);
  end
  else
    Result := ErrorResult(errWrongType);
end;

procedure fpsPI(var Result: TsExpressionResult; const Args: TsExprParameterArray);
begin
  Unused(Args);
  Result := FloatResult(pi);
end;

procedure fpsPOWER(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x, y: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  y := ArgToFloat(Args[1]);
  if IsNaN(x) or IsNaN(y) then
    Result := ErrorResult(errWrongType)
  else
    try
      Result := FloatResult(Power(x, y));
    except
      Result := ErrorResult(errOverflow);
    end;
end;

procedure fpsRADIANS(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(DegToRad(x));
end;

procedure fpsRAND(var Result: TsExpressionResult; const Args: TsExprParameterArray);
begin
  Unused(Args);
  Result := FloatResult(random);
end;

// Avoids Banker's rounding
function MyRoundTo(const AValue: Double; const Digits: TRoundToRange): Double;
var
  RV: Double;
begin
  RV := IntPower(10,Digits);
  Result := fpsUtils.Round(AValue / RV) * RV;
end;

procedure fpsROUND(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
  n: Integer;
begin
  x := ArgToFloat(Args[1]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else begin
    n := Round(x);
    x := ArgToFloat(Args[0]);
    if IsNaN(x) then
      Result := ErrorResult(errWrongType)
    else
      Result := FloatResult(MyRoundTo(x, -n));
      // -n because fpc and Excel have different conventions regarding the sign
  end;
end;

function MyRoundDown(const AValue: Double; const Digits: TRoundToRange): Double;
var
  RV: Double;
begin
  RV := IntPower(10, Digits);
  Result := Trunc(AValue / RV) * RV;
end;


{ The Excel ROUNDDOWN function returns a number rounded down to a given number
  of decimal places. Unlike standard rounding, where only numbers less than 5
  are rounded down, ROUNDDOWN rounds all numbers down. }
procedure fpsROUNDDOWN(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
  n: Integer;
begin
  x := ArgToFloat(Args[1]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else begin
    n := Round(x);
    x := ArgToFloat(Args[0]);
    if IsNaN(x) then
      Result := ErrorResult(errWrongType)
    else
      Result := FloatResult(MyRoundDown(x, -n));
  end;
end;

procedure fpsSIGN(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(sign(x));
end;

procedure fpsSIN(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(sin(x));
end;

procedure fpsSINH(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(sinh(x));
end;

procedure fpsSQRT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
  if x >= 0 then
    Result := FloatResult(sqrt(x))
  else
    Result := ErrorResult(errOverflow);
end;

procedure fpsTAN(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
  if frac(x / (pi*0.5)) = 0 then
    Result := ErrorResult(errOverflow)   // #NUM!
  else
    Result := FloatResult(tan(x));
end;

procedure fpsTANH(var Result: TsExpressionResult; const Args: TsExprParameterArray);
var
  x: TsExprFloat;
begin
  x := ArgToFloat(Args[0]);
  if IsNaN(x) then
    Result := ErrorResult(errWrongType)
  else
    Result := FloatResult(tanh(x));
end;


{------------------------------------------------------------------------------}
{   Built-in date/time functions                                               }
{------------------------------------------------------------------------------}

procedure fpsDATE(var Result: TsExpressionResult;
  const Args: TsExprParameterArray);
// DATE( year, month, day )
begin
  Result := DateTimeResult(
    EncodeDate(ArgToInt(Args[0]), ArgToInt(Args[1]), ArgToInt(Args[2]))
  );
end;

procedure fpsDATEDIF(var Result: TsExpressionResult;
  const Args: TsExprParameterArray);
{ DATEDIF( start_date, end_date, interval )
    start_date <= end_date !
    interval = Y  - The number of complete years.
             = M  - The number of complete months.
             = D  - The number of days.
             = MD - The difference between the days (months and years are ignored).
             = YM - The difference between the months (days and years are ignored).
             = YD - The difference between the days (years and dates are ignored). }
var
  interval: String;
  start_date, end_date: TDate;
begin
  start_date := ArgToDateTime(Args[0]);
  end_date := ArgToDateTime(Args[1]);
  if IsNaN(start_date) or IsNaN(end_date) then begin
    Result := ErrorResult(errWrongType);
    exit;
  end;
  interval := ArgToString(Args[2]);

  if end_date > start_date then
    Result := ErrorResult(errOverflow)
  else if interval = 'Y' then
    Result := FloatResult(YearsBetween(end_date, start_date))
  else if interval = 'M' then
    Result := FloatResult(MonthsBetween(end_date, start_date))
  else if interval = 'D' then
    Result := FloatResult(DaysBetween(end_date, start_date))
  else
    Result := ErrorResult(errFormulaNotSupported);
end;

procedure fpsDATEVALUE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// Returns the serial number of a date. Input is a string.
// DATE( date_string )
var
  d: TDateTime;
begin
  if TryStrToDate(Args[0].ResString, d) then
    Result := DateTimeResult(d)
  else
    Result := ErrorResult(errWrongType);
end;

procedure fpsDAY(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// DAY( date_value )
// date_value can be a serial number or a string
var
  y,m,d: Word;
  dt: TDateTime;
begin
  Result := ErrorResult(errWrongType);
  if (Args[0].ResultType in [rtDateTime, rtFloat, rtInteger, rtString, rtCell]) then
  begin
    dt := ArgToDateTime(Args[0]);
    if not IsNaN(dt) then begin
      DecodeDate(dt, y, m, d);
      Result := IntegerResult(d);
    end;
  end;
end;

procedure fpsHOUR(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// HOUR( time_value )
// time_value can be a number or a string.
var
  h, m, s, ms: Word;
  dt: TDateTime;
begin
  Result := ErrorResult(errWrongType);
  if (Args[0].ResultType in [rtDateTime, rtFloat, rtInteger, rtString, rtCell]) then
  begin
    dt := ArgToDateTime(Args[0]);
    if not IsNaN(dt) then begin
      DecodeTime(dt, h, m, s, ms);
      Result := IntegerResult(h);
    end;
  end;
end;

procedure fpsMINUTE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// MINUTE( serial_number or string )
var
  h, m, s, ms: Word;
  dt: TDateTime;
begin
  Result := ErrorResult(errWrongType);
  if (Args[0].ResultType in [rtDateTime, rtFloat, rtInteger, rtString, rtCell]) then
  begin
    dt := ArgToDateTime(Args[0]);
    if not IsNaN(dt) then begin
      DecodeTime(dt, h, m, s, ms);
      Result := IntegerResult(m);
    end;
  end;
end;

procedure fpsMONTH(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// MONTH( date_value or string )
var
  y,m,d: Word;
  dt: TDateTime;
begin
  Result := ErrorResult(errWrongType);
  if (Args[0].ResultType in [rtDateTime, rtFloat, rtInteger, rtString, rtCell]) then
  begin
    dt := ArgToDateTime(Args[0]);
    if not IsNaN(dt) then begin
      DecodeDate(dt, y, m, d);
      Result := IntegerResult(m);
    end;
  end;
end;

procedure fpsNOW(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// Returns the current system date and time. Willrefresh the date/time value
// whenever the worksheet recalculates.
//   NOW()
begin
  Unused(Args);
  Result := DateTimeResult(Now);
end;

procedure fpsSECOND(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// SECOND( serial_number )
var
  h, m, s, ms: Word;
  dt: TDateTime;
begin
  Result := ErrorResult(errWrongType);
  if (Args[0].ResultType in [rtDateTime, rtFloat, rtInteger, rtString, rtCell]) then
  begin
    dt := ArgToDateTime(Args[0]);
    if not IsNaN(dt) then begin
      DecodeTime(dt, h, m, s, ms);
      Result := IntegerResult(s);
    end;
  end;
end;

procedure fpsTIME(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// TIME( hour, minute, second)
begin
  Result := DateTimeResult(
    EncodeTime(ArgToInt(Args[0]), ArgToInt(Args[1]), ArgToInt(Args[2]), 0)
  );
end;

procedure fpsTIMEVALUE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// Returns the serial number of a time. Input must be a string.
// DATE( date_string )
var
  t: TDateTime;
begin
  if TryStrToTime(Args[0].ResString, t) then
    Result := DateTimeResult(t)
  else
    Result := ErrorResult(errWrongType);
end;

procedure fpsTODAY(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// Returns the current system date. This function will refresh the date
// whenever the worksheet recalculates.
//   TODAY()
begin
  Unused(Args);
  Result := DateTimeResult(Date);
end;

procedure fpsWEEKDAY(var Result: TsExpressionResult; const Args: TsExprParameterArray);
{ WEEKDAY( serial_number, [return_value] )
   return_value = 1 - Returns a number from 1 (Sunday) to 7 (Saturday) (default)
                = 2 - Returns a number from 1 (Monday) to 7 (Sunday).
                = 3 - Returns a number from 0 (Monday) to 6 (Sunday). }
var
  n: Integer;
  dow: Integer;
  dt: TDateTime;
begin
  Result := ErrorResult(errWrongType);
  if Length(Args) = 2 then
    n := ArgToInt(Args[1])
  else
    n := 1;
  dt := NaN;
  if Args[0].ResultType in [rtDateTime, rtFloat, rtInteger, rtCell, rtString] then
    dt := ArgToDateTime(Args[0]);
  if IsNaN(dt) then
    exit;

  dow := DayOfWeek(dt);   // Sunday = 1 ... Saturday = 7
  case n of
    1: ;
    2: if dow > 1 then dow := dow - 1 else dow := 7;
    3: if dow > 1 then dow := dow - 2 else dow := 6;
  end;
  Result := IntegerResult(dow);
end;

procedure fpsYEAR(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// YEAR( date_value )
var
  y,m,d: Word;
  dt: TDateTime;
begin
  Result := ErrorResult(errWrongType);
  if (Args[0].ResultType in [rtDateTime, rtFloat, rtInteger, rtString, rtCell]) then
  begin
    dt := ArgToDateTime(Args[0]);
    if not IsNaN(dt) then begin
      DecodeDate(dt, y, m, d);
      Result := IntegerResult(y);
    end;
  end;
end;


{------------------------------------------------------------------------------}
{    Builtin string functions                                                  }
{------------------------------------------------------------------------------}

procedure fpsCHAR(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// CHAR( ascii_value )
// returns the character based on the ASCII value
var
  arg: Integer;
begin
  Result := ErrorResult(errWrongType);
  case Args[0].ResultType of
    rtInteger, rtFloat:
      if Args[0].ResultType in [rtInteger, rtFloat] then
      begin
        arg := ArgToInt(Args[0]);
        if (arg >= 0) and (arg < 256) then
          Result := StringResult(AnsiToUTF8(Char(arg)));
      end;
    rtError:
      Result := ErrorResult(Args[0].ResError);
    rtEmpty:
      Result.ResultType := rtEmpty;
  end;
end;

procedure fpsCODE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// CODE( text )
// returns the ASCII value of a character or the first character in a string.
var
  s: String;
  ch: Char;
begin
  s := ArgToString(Args[0]);
  if s = '' then
    Result := ErrorResult(errWrongType)
  else
  begin
    ch := UTF8ToAnsi(s)[1];
    Result := IntegerResult(ord(ch));
  end;
end;

procedure fpsCONCATENATE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// CONCATENATE( text1, text2, ... text_n )
// Joins two or more strings together
var
  s: String;
  i: Integer;
begin
  s := '';
  for i:=0 to Length(Args)-1 do
  begin
    if Args[i].ResultType = rtError then
    begin
      Result := ErrorResult(Args[i].ResError);
      exit;
    end;
    s := s + ArgToString(Args[i]);
  end;
  Result := StringResult(s);
end;

procedure fpsEXACT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// EXACT( text1, text2 )
// Compares two strings (case-sensitive) and returns TRUE if they are equal
var
  s1, s2: String;
begin
  s1 := ArgToString(Args[0]);
  s2 := ArgToString(Args[1]);
  Result := BooleanResult(s1 = s2);
end;

procedure fpsLEFT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// LEFT( text, [number_of_characters] )
// extracts a substring from a string, starting from the left-most character
var
  s: String;
  count: Integer;
begin
  s := ArgToString(Args[0]);
  if s = '' then
    Result := EmptyResult
  else
  if Length(Args) > 2 then
    Result := ErrorResult(errArgError)
  else
  begin
    if Length(Args) = 1 then
      count := 1
    else
    if Args[1].ResultType = rtMissingArg then
      count := 1
    else
      count := ArgToInt(Args[1]);
    Result := StringResult(UTF8LeftStr(s, count));
  end;
end;

procedure fpsLEN(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// LEN( text )
//  returns the length of the specified string.
begin
  Result := IntegerResult(UTF8Length(ArgToString(Args[0])));
end;

procedure fpsLOWER(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// LOWER( text )
// converts all letters in the specified string to lowercase. If there are
// characters in the string that are not letters, they are not affected.
begin
  Result := StringResult(UTF8Lowercase(ArgToString(Args[0])));
end;

procedure fpsMID(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// MID( text, start_position, number_of_characters )
// extracts a substring from a string (starting at any position).
begin
  Result := StringResult(UTF8Copy(ArgToString(Args[0]), ArgToInt(Args[1]), ArgToInt(Args[2])));
end;

procedure fpsREPLACE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// REPLACE( old_text, start, number_of_chars, new_text )
//  replaces a sequence of characters in a string with another set of characters
var
  sOld, sNew, s1, s2: String;
  start: Integer;
  count: Integer;
begin
  sOld := Args[0].ResString;
  start := ArgToInt(Args[1]);
  count := ArgToInt(Args[2]);
  sNew := Args[3].ResString;
  s1 := UTF8Copy(sOld, 1, start-1);
  s2 := UTF8Copy(sOld, start+count, UTF8Length(sOld));
  Result := StringResult(s1 + sNew + s2);
end;

procedure fpsREPT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// REPT( text, count )
// repeats text a specified number of times.
var
  s: String;
  count: Integer;
begin
  s := ArgToString(Args[0]);
  if s = '' then
    Result.ResultType := rtEmpty
  else
  if Args[1].ResultType in [rtInteger, rtFloat] then begin
    count := ArgToInt(Args[1]);
    Result := StringResult(DupeString(s, count));
  end;
end;

procedure fpsRIGHT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// RIGHT( text, [number_of_characters] )
// extracts a substring from a string, starting from the last character
var
  s: String;
  count: Integer;
begin
  s := ArgToString(Args[0]);
  if s = '' then
    Result := EmptyResult
  else
  if Length(Args) > 2 then
    Result := ErrorResult(errArgError)
  else
  begin
    if Length(Args) = 1 then
      count := 1
    else
    if Args[1].ResultType = rtMissingArg then
      count := 1
    else
      count := ArgToInt(Args[1]);
    Result := StringResult(UTF8RightStr(s, count));
  end;
end;

procedure fpsSUBSTITUTE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// SUBSTITUTE( text, old_text, new_text, [nth_appearance] )
// replaces a set of characters with another.
var
  sOld: String;
  sNew: String;
  s1, s2: String;
  n: Integer;
  s: String;
  p: Integer;
begin
  s := ArgToString(Args[0]);
  sOld := ArgToString(Args[1]);
  sNew := ArgToString(Args[2]);
  if Length(Args) = 4 then
  begin
    n := ArgToInt(Args[3]);               // THIS PART NOT YET CHECKED !!!!!!
    if n <= 0 then
    begin
      Result := ErrorResult(errWrongType);
      exit;
    end;
    p := UTF8Pos(sOld, s);
    while (n > 1) do begin
      p := UTF8Pos(sOld, s, p+1);
      dec(n);
    end;
    if p > 0 then begin
      s1 := UTF8Copy(s, 1, p-1);
      s2 := UTF8Copy(s, p+UTF8Length(sOld), UTF8Length(s));
      s := s1 + sNew + s2;
    end;
    Result := StringResult(s);
  end else
    Result := StringResult(UTF8StringReplace(s, sOld, sNew, [rfReplaceAll]));
end;

procedure fpsTEXT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// TEXT( value, format )
// Returns a value converted to text with a specified format.
var
  fmt: String;
  value: double;
begin
  value := ArgToFloat(Args[0]);
  fmt := ArgToString(Args[1]);
  if IsDateTimeFormat(fmt) then
    Result := StringResult(FormatDateTime(fmt, value))
  else
    Result := StringResult(Format(fmt, [value]));
end;

procedure fpsTRIM(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// TRIM( text )
// Returns a text value with the leading and trailing spaces removed
begin
  Result := StringResult(UTF8Trim(ArgToString(Args[0])));
end;

procedure fpsUPPER(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// UPPER( text )
// converts all letters in the specified string to uppercase. If there are
// characters in the string that are not letters, they are not affected.
begin
  Result := StringResult(UTF8Uppercase(ArgToString(Args[0])));
end;

procedure fpsVALUE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// VALUE( text )
// converts a text value that represents a number to a number.
var
  x: Double;
  n: Integer;
  s: String;
begin
  s := ArgToString(Args[0]);
  if TryStrToInt(s, n) then
    Result := IntegerResult(n)
  else
  if TryStrToFloat(s, x, ExprFormatSettings) then
    Result := FloatResult(x)
  else
  if TryStrToDateTime(s, x) then
    Result := FloatResult(x)
  else
    Result := ErrorResult(errWrongType);
end;


{------------------------------------------------------------------------------}
{    Built-in logical functions                                                }
{------------------------------------------------------------------------------}

procedure fpsAND(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// AND( condition1, [condition2], ... )
// up to 30 parameters. At least 1 parameter.
var
  i: Integer;
  b: Boolean;
begin
  b := true;
  for i:=0 to High(Args) do
    if not ArgToBoolean(Args[i]) then begin
      b := false;
      break;
    end;
  Result.ResBoolean := b;
end;

procedure fpsFALSE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// FALSE ()
begin
  Unused(Args);
  Result.ResBoolean := false;
end;

procedure fpsIF(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// IF( condition, value_if_true, [value_if_false] )
begin
  if Length(Args) > 2 then
  begin
    if ArgToBoolean(Args[0]) then
      Result := Args[1]
    else
      Result := Args[2];
  end else
  begin
    if ArgToBoolean(Args[0]) then
      Result := Args[1]
    else
      Result.ResBoolean := false;
  end;
end;

procedure fpsNOT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// NOT( condition )
begin
  Result.ResBoolean := not ArgToBoolean(Args[0]);
end;

procedure fpsOR(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// OR( condition1, [condition2], ... )
// up to 30 parameters. At least 1 parameter.
var
  i: Integer;
  b: Boolean;
begin
  b := false;
  for i:=0 to High(Args) do
    if ArgToBoolean(Args[i]) then begin
      b := true;
      break;
    end;
  Result.ResBoolean := b;
end;

procedure fpsTRUE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// TRUE()
begin
  Unused(Args);
  Result.ResBoolean := true;
end;


{------------------------------------------------------------------------------}
{    Built-in statistical functions                                            }
{------------------------------------------------------------------------------}

procedure fpsAVEDEV(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// Average value of absolute deviations of data from their mean.
// AVEDEV( value1, [value2, ... value_n] )
var
  data: TsExprFloatArray;
  m: TsExprFloat;
  i: Integer;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then begin
    Result := ErrorResult(err);
    exit;
  end;
  m := Mean(data);
  for i:=0 to High(data) do      // replace data by their average deviation from the mean
    data[i] := abs(data[i] - m);
  Result.ResFloat := Mean(data);
end;

procedure fpsAVERAGE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// AVERAGE( value1, [value2, ... value_n] )
var
  data: TsExprFloatArray;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then
    Result := ErrorResult(err)
  else
    Result.ResFloat := Mean(data);
end;

procedure fpsCOUNT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
{ counts the number of cells that contain numbers as well as the number of
  arguments that contain numbers.
    COUNT( value1, [value2, ... value_n] )  }
var
  data: TsExprFloatArray;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then
    Result := ErrorResult(err)
  else
    Result.ResInteger := Length(data);
end;

procedure fpsCOUNTA(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// Counts the number of cells that are not empty as well as the number of
// arguments that contain values
//   COUNTA( value1, [value2, ... value_n] )
var
  i, n: Integer;
  r, c: Cardinal;
  cell: PCell;
  arg: TsExpressionResult;
begin
  n := 0;
  for i:=0 to High(Args) do
  begin
    arg := Args[i];
    case arg.ResultType of
      rtInteger, rtFloat, rtDateTime, rtBoolean:
        inc(n);
      rtString:
        if arg.ResString <> '' then inc(n);
      rtError:
        if arg.ResError <> errOK then inc(n);
      rtCell:
        begin
          cell := ArgToCell(arg);
          if cell <> nil then
            case cell^.ContentType of
              cctNumber, cctDateTime, cctBool: inc(n);
              cctUTF8String: if cell^.UTF8StringValue <> '' then inc(n);
              cctError: if cell^.ErrorValue <> errOK then inc(n);
            end;
        end;
      rtCellRange:
        for r := arg.ResCellRange.Row1 to arg.ResCellRange.Row2 do
          for c := arg.ResCellRange.Col1 to arg.ResCellRange.Col2 do
          begin
            cell := (arg.Worksheet as TsWorksheet).FindCell(r, c);
            if (cell <> nil) then
              case cell^.ContentType of
                cctNumber, cctDateTime, cctBool : inc(n);
                cctUTF8String: if cell^.UTF8StringValue <> '' then inc(n);
                cctError: if cell^.ErrorValue <> errOK then inc(n);
              end;
          end;
    end;
  end;
  Result.ResInteger := n;
end;

procedure fpsCOUNTBLANK(var Result: TsExpressionResult; const Args: TsExprParameterArray);
{ Counts the number of empty cells in a range.
    COUNTBLANK( range )
  "range" is the range of cells to count empty cells. }
var
  n: Integer;
  r, c: Cardinal;
  cell: PCell;
begin
  n := 0;
  case Args[0].ResultType of
    rtEmpty:
      inc(n);
    rtCell:
      begin
        cell := ArgToCell(Args[0]);
        if cell = nil then
          inc(n)
        else
          case cell^.ContentType of
            cctNumber, cctDateTime, cctBool: ;
            cctUTF8String: if cell^.UTF8StringValue = '' then inc(n);
            cctError: if cell^.ErrorValue = errOK then inc(n);
          end;
      end;
    rtCellRange:
      for r := Args[0].ResCellRange.Row1 to Args[0].ResCellRange.Row2 do
        for c := Args[0].ResCellRange.Col1 to Args[0].ResCellRange.Col2 do begin
          cell := (Args[0].Worksheet as TsWorksheet).FindCell(r, c);
          if cell = nil then
            inc(n)
          else
            case cell^.ContentType of
              cctNumber, cctDateTime, cctBool: ;
              cctUTF8String: if cell^.UTF8StringValue = '' then inc(n);
              cctError: if cell^.ErrorValue = errOK then inc(n);
            end;
        end;
  end;
  Result.ResInteger := n;
end;

procedure DoIF(var result: TsExpressionResult; const Args: TsExprParameterArray;
  AFlag: Integer);
{ Helper function for COUNTIF (AFlag = 0) or SUMIF (AFlag = 1) or AVERAGEIF (AFlag = 2):
  Counts and adds the cells in a range if the cell values meet a given condition.
  - "range" is to the cell range to be analyzed
  - "condition" can be a cell, a value or a string starting with a symbol like ">" etc.
    (in the former two cases a value is counted if equal to the criteria value)
  - "sum_range" - option for the values to be added; if missing the values in
    "range" are used.}
type
  TCompareType = (ctEmpty, ctString, ctNumber);
var
  n: Integer;
  r, c: LongInt;
  dr, dc: LongInt;
  cell, addcell: PCell;
  s: String;
  f: Double;
  dt: TDateTime;
  book: TsWorkbook;
  sheet0: TsWorksheet;
  sheet2: TsWorksheet;
  compareNumber: Double = 0.0;
  compareStr: String = '';
  compareOp: TsCompareOperation = coEqual;
  compareType: TCompareType;
  addNumber: Double;
  fs: TFormatSettings;
  sum: Double;

  procedure DoCompareNumber(ANumber, AAddNumber: Float);
  var
    ok: Boolean;
  begin
    ok := false;
    case compareOp of
      coEqual        : if ANumber = compareNumber then ok := true;
      coLess         : if ANumber < compareNumber then ok := true;
      coGreater      : if ANumber > compareNumber then ok := true;
      coLessEqual    : if ANumber <= compareNumber then ok := true;
      coGreaterEqual : if ANumber >= compareNumber then ok := true;
      coNotEqual     : if ANumber >= compareNumber then ok := true;
    end;
    if ok then
      case AFlag of
        0 : inc(n);
        1 : sum := sum + AAddNumber;
        2 : begin inc(n); sum := sum + AAddNumber; end;
      end;
  end;

  procedure DoCompareString(AStr: String; AAddNumber: Float);
  var
    ok: Boolean;
  begin
    ok := false;
    case compareOp of
      coEqual        : if AStr = compareStr then ok := true;
      coLess         : if AStr < compareStr then ok := true;
      coGreater      : if AStr > compareStr then ok := true;
      coLessEqual    : if AStr <= compareStr then ok := true;
      coGreaterEqual : if AStr >= compareStr then ok := true;
      coNotEqual     : if AStr >= compareStr then ok := true;
    end;
    if ok then
      case AFlag of
        0: inc(n);
        1: sum := sum + AAddNumber;
        2: begin inc(n); sum := sum + AAddNumber; end;
      end;
  end;

  procedure DoCompareEmpty(IsEmpty: Boolean; AAddNumber: Float);
  var
    ok: Boolean;
  begin
    ok := false;
    case compareOp of
      coEqual        : if isEmpty then ok := true;
      coNotEqual     : if not isEmpty then ok := true;
    end;
    if ok then
      case AFlag of
        0: inc(n);
        1: sum := sum + AAddNumber;
        2: begin inc(n); sum := sum + AAddNumber; end;
      end;
  end;

begin
  // Simple cases
  if (Length(Args) < 1) then begin
    Result := IntegerResult(0);
    exit;
  end;

  // Get format settings for string-to-float or -to-datetime conversion
  if (Args[0].ResultType in [rtCell, rtCellRange]) then
    fs := (Args[0].Worksheet as TsWorksheet).FormatSettings
  else
  begin
    Result := ErrorResult(errArgError);
    exit;
  end;

  // Get compare operation and compare value
  if (Args[1].ResultType = rtCell) then
  begin
    cell := ArgToCell(Args[1]);
    if cell = nil then
      comparetype := ctEmpty
    else
      case cell^.ContentType of
        cctNumber:
          begin
            compareNumber := cell^.NumberValue;
            compareType := ctNumber;
          end;
        cctDateTime:
          begin
            compareNumber := cell^.DateTimevalue;
            compareType := ctNumber;
          end;
        cctBool:
          begin
            if cell^.BoolValue then compareNumber := 1.0 else compareNumber := 0.0;
            compareType := ctNumber;
          end;
        cctUTF8String:
          begin
            compareStr := cell^.UTF8StringValue;
            compareType := ctString;
          end;
        cctEmpty:
          begin
            compareType := ctEmpty;
          end;
        cctError:
          ; // what to do here?
      end;
  end else
  begin
    s := ArgToString(Args[1]);
    if (Length(s) > 1) and (s[1] in ['=', '<', '>']) then
      s := AnalyzeCompareStr(s, compareOp);
    if s = '' then
      compareType := ctEmpty
    else
    if TryStrToInt(s, n) then
    begin
      compareNumber := n;
      compareType := ctNumber;
    end else
    if TryStrToFloat(s, f, fs) then
    begin
      compareNumber := f;
      compareType := ctNumber;
    end else
    if TryStrToDate(s, dt, fs) or TryStrToTime(s, dt, fs) or TryStrToDateTime(s, dt, fs) then
    begin
      compareNumber := dt;
      compareType := ctNumber;
    end
    else
    begin
      compareStr := s;
      compareType := ctString;
    end;
  end;

  // Empty cells cannot be checked for <=, <, >, >=  --> error
  if (compareType = ctEmpty) and not (compareOp in [coEqual, coNotEqual]) then
  begin
    Result := ErrorResult(errArgError);
    exit;
  end;

  // Strings cannot be added --> error
  if (AFlag <> 0) and (compareType = ctString) and (Length(Args) = 2) then
  begin
    Result := ErrorResult(errArgError);
    exit;
  end;

  // The sum of empty cells is be 0.
  if (AFlag <> 0) and (compareType = ctEmpty) and (Length(Args) = 2) then
  begin
    Result := FloatResult(0.0);
    exit;
  end;

  // Offsets to "add" range
  if Length(Args) = 2 then
  begin
    // If "sum_range" argument is missing the "range" argument is used for adding
    dr := 0;
    dc := 0;
  end else
  if (Args[0].ResultType = rtCellRange) and (Args[2].ResultType = rtCellRange) then
  begin
    dr := LongInt(Args[2].ResCellRange.Row1) - LongInt(Args[0].ResCellRange.Row1);
    dc := LongInt(Args[2].ResCellRange.Col1) - LongInt(Args[0].ResCellRange.Col1);
  end else
  if (Args[0].ResultType = rtCell) and (Args[2].ResultType = rtCell) then
  begin
    dr := LongInt(Args[2].ResRow) - LongInt(Args[0].ResRow);
    dc := LongInt(Args[2].ResCol) - LongInt(Args[0].ResRow);
  end else
  begin
    Result := ErrorResult(errArgError);
    exit;
  end;

  // Iterate through range
  n := 0;
  sum := 0;
  if (Args[0].ResultType = rtCell) then
    case compareType of
      ctNumber : if Length(Args) = 2
                   then DoCompareNumber(ArgToFloat(Args[0]), ArgToFloat(Args[0]))
                   else DoCompareNumber(ArgToFloat(Args[0]), ArgToFloat(Args[2]));
      ctString : if Length(Args) = 2
                   then DoCompareNumber(ArgToFloat(Args[0]), 0)
                   else DoCompareString(ArgToString(Args[0]), ArgToFloat(Args[2]));
      ctEmpty  : if Length(Args) = 2
                   then DoCompareEmpty(ArgToString(Args[0]) = '', 0)
                   else DoCompareEmpty(ArgToString(Args[0]) = '', ArgToFloat(Args[2]));
    end
  else
  if (Args[0].ResultType = rtCellRange) then begin
    if Args[0].ResCellRange.Sheet1 <> Args[0].ResCellRange.Sheet2 then begin
      Result := ErrorResult(errArgError);
      exit;
    end;
    if (Length(Args) = 3) and (Args[2].ResCellRange.Sheet1 <> Args[2].ResCellrange.Sheet2) then
    begin
      Result := ErrorResult(errArgError);
      exit;
    end;
    book := TsWorkbook(TsWorksheet(Args[0].Worksheet).Workbook);
    sheet0 := book.GetWorksheetByIndex(Args[0].ResCellRange.Sheet1);
    sheet2 := book.GetWorksheetbyIndex(Args[2].ResCellrange.Sheet1);
    for r := Args[0].ResCellRange.Row1 to Args[0].ResCellRange.Row2 do
    begin
      for c := Args[0].ResCellRange.Col1 to Args[0].ResCellRange.Col2 do
      begin
        // Get value to be added. Not needed for counting (AFlag = 0)
        addnumber := 0;
        if AFlag > 0 then
        begin
          if Length(Args) = 2 then
            addcell := sheet0.FindCell(r + dr, c + dc)
          else
            addCell := sheet2.FindCell(r + dr, c + dc);
          if addcell <> nil then
            case addcell^.Contenttype of
              cctNumber  : addnumber := addcell^.NumberValue;
              cctDateTime: addnumber := addcell^.DateTimeValue;
              cctBool    : if addcell^.BoolValue then addnumber := 1;
            end;
        end;

        cell := sheet0.FindCell(r, c);
        case compareType of
          ctNumber:
            if cell <> nil then
            begin
              case cell^.ContentType of
                cctNumber:
                  DoCompareNumber(cell^.NumberValue, addNumber);
                cctDateTime:
                  DoCompareNumber(cell^.DateTimeValue, addNumber);
                cctBool:
                  DoCompareNumber(IfThen(cell^.Boolvalue, 1, 0), addNumber);
              end;
            end;
          ctString:
            if (cell <> nil) and (cell^.ContentType = cctUTF8String) then
              DoCompareString(cell^.Utf8StringValue, addNumber);
          ctEmpty:
            DoCompareEmpty((cell = nil) or ((cell <> nil) and (cell^.ContentType = cctEmpty)), addNumber);
        end;
      end;
    end;
  end;

  case AFlag of
    0: Result := IntegerResult(n);
    1: Result := FloatResult(sum);
    2: if n > 0 then Result := FloatResult(sum/n) else Result := FloatResult(0);
  end;
end;

procedure fpsAVERAGEIF(var result: TsExpressionresult; const Args: TsExprParameterArray);
{ Calculates the average value of the cell values if they meet a given condition.
    AVERAGEIF( range, condition, [ave_range] )
  - "range" is the cell range to be analyzed
  - "condition" can be a cell, a value or a string starting with a symbol like ">" etc.
    (in the former two cases a value is counted if equal to the criteria value)
  - "ave_range" - option for the values to be added; if missing the values in
    "range" are used.}
begin
  DoIF(Result, Args, 2);
end;

procedure fpsCOUNTIF(var result: TsExpressionResult; const Args: TsExprParameterArray);
{ Counts the number of cells in a range that meets a given condition.
    COUNTIF( range, condition )
  - "range" is the cell range to be analyzed
  - "condition" can be a cell, a value or a string starting with a symbol like ">" etc.
    (in the former two cases a value is counted if equal to the criteria value) }
begin
  DoIF(result, Args, 0);
end;

procedure fpsSUMIF(var result: TsExpressionResult; const Args: TsExprParameterArray);
{ Adds the cell values if they meet a given condition.
    SUMIF( range, condition, [sum_range] )
  - "range" is the cell range to be analyzed
  - "condition" can be a cell, a value or a string starting with a symbol like ">" etc.
    (in the former two cases a value is counted if equal to the criteria value)
  - "sum_range" - option for the values to be added; if missing the values in
    "range" are used.}
begin
  DoIF(result, Args, 1);
end;

procedure fpsMAX(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// MAX( value1, [value2, ... value_n] )
var
  data: TsExprFloatArray;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then
    Result := ErrorResult(err)
  else
    Result.ResFloat := MaxValue(data);
end;

procedure fpsMIN(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// MIN( value1, [value2, ... value_n] )
var
  data: TsExprFloatArray;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then
    Result := ErrorResult(err)
  else
    Result.ResFloat := MinValue(data);
end;

procedure fpsPRODUCT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// PRODUCT( value1, [value2, ... value_n] )
var
  data: TsExprFloatArray;
  i: Integer;
  p: TsExprFloat;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then begin
    Result := ErrorResult(err);
    exit;
  end;

  p := 1.0;
  for i := 0 to High(data) do
    p := p * data[i];
  Result.ResFloat := p;
end;

procedure fpsSTDEV(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// Returns the standard deviation of a population based on a sample of numbers
// of numbers.
//   STDEV( value1, [value2, ... value_n] )
var
  data: TsExprFloatArray;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then begin
    Result := ErrorResult(err);
    exit;
  end;

  if Length(data) > 1 then
    Result.ResFloat := StdDev(data)
  else
  begin
    Result.ResultType := rtError;
    Result.ResError := errDivideByZero;
  end;
end;

procedure fpsSTDEVP(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// Returns the standard deviation of a population based on an entire population
// STDEVP( value1, [value2, ... value_n] )
var
  data: TsExprFloatArray;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then begin
    Result := ErrorResult(err);
    exit;
  end;

  if Length(data) > 0 then
    Result.ResFloat := PopnStdDev(data)
  else
  begin
    Result.ResultType := rtError;
    Result.ResError := errDivideByZero;
  end;
end;

procedure fpsSUM(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// SUM( value1, [value2, ... value_n] )
var
  data: TsExprFloatArray;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then
    Result := ErrorResult(err)
  else
    Result.ResFloat := Sum(data);
end;

procedure fpsSUMSQ(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// Returns the sum of the squares of a series of values.
// SUMSQ( value1, [value2, ... value_n] )
var
  data: TsExprFloatArray;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then
    Result := ErrorResult(err)
  else
    Result.ResFloat := SumOfSquares(data);
end;

procedure fpsVAR(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// Returns the variance of a population based on a sample of numbers.
// VAR( value1, [value2, ... value_n] )
var
  data: TsExprFloatArray;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then
  begin
    Result := ErrorResult(err);
    exit;
  end;

  if Length(data) > 1 then
    Result.ResFloat := Variance(data)
  else
  begin
    Result.ResultType := rtError;
    Result.ResError := errDivideByZero;
  end;
end;

procedure fpsVARP(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// Returns the variance of a population based on an entire population of numbers.
// VARP( value1, [value2, ... value_n] )
var
  data: TsExprFloatArray;
  err: TsErrorValue;
begin
  ArgsToFloatArray(Args, data, err);
  if err <> errOK then
  begin
    Result := ErrorResult(err);
    exit;
  end;

  if Length(data) > 0 then
    Result.ResFloat := PopnVariance(data)
  else
  begin
    Result.ResultType := rtError;
    Result.ResError := errDivideByZero;
  end;
end;


{------------------------------------------------------------------------------}
{    Builtin info functions                                                    }
{------------------------------------------------------------------------------}

{ !!!!!!!!!!!!!!  not working !!!!!!!!!!!!!!!!!!!!!! }
{ !!!!!!!!!!!!!! needs localized strings !!!!!!!!!!! }

procedure fpsCELL(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// CELL( type, [range] )

{ from http://www.techonthenet.com/excel/formulas/cell.php:

  "type" is the type of information that we retrieve for the cell and can have
  one of the following values:
    Value         Explanation
    ------------- --------------------------------------------------------------
    "address"     Address of the cell. If the cell refers to a range, it is the
                  first cell in the range.
    "col"         Column number of the cell.
    "color"       Returns 1 if the color is a negative value; Otherwise it returns 0.
    "contents"    Contents of the upper-left cell.
    "filename"    Filename of the file that contains reference.
    "format"      Number format of the cell according to:
                     "G"    General
                     "F0"   0
                     ",0"   #,##0
                     "F2"   0.00
                     ",2"   #,##0.00
                     "C0"   $#,##0_);($#,##0)
                     "C0-"  $#,##0_);[Red]($#,##0)
                     "C2"   $#,##0.00_);($#,##0.00)
                     "C2-"  $#,##0.00_);[Red]($#,##0.00)
                     "P0"   0%
                     "P2"   0.00%
                     "S2"   0.00E+00
                     "G"    # ?/? or # ??/??
                     "D4"   m/d/yy or m/d/yy h:mm or mm/dd/yy
                     "D1"   d-mmm-yy or dd-mmm-yy
                     "D2"   d-mmm or dd-mmm
                     "D3"   mmm-yy
                     "D5"   mm/dd
                     "D6"   h:mm:ss AM/PM
                     "D7"   h:mm AM/PM
                     "D8"   h:mm:ss
                     "D9"   h:mm
    "parentheses" Returns 1 if the cell is formatted with parentheses;
                  Otherwise, it returns 0.
    "prefix"      Label prefix for the cell.
                  - Returns a single quote (') if the cell is left-aligned.
                  - Returns a double quote (") if the cell is right-aligned.
                  - Returns a caret (^) if the cell is center-aligned.
                  - Returns a back slash (\) if the cell is fill-aligned.
                  - Returns an empty text value for all others.
    "protect"     Returns 1 if the cell is locked. Returns 0 if the cell is not locked.
    "row"         Row number of the cell.
    "type"        Returns "b" if the cell is empty.
                  Returns "l" if the cell contains a text constant.
                  Returns "v" for all others.
    "width"       Column width of the cell, rounded to the nearest integer.

  !!!! NOT ALL OF THEM ARE SUPPORTED HERE !!!

  "range" is optional in Excel. It is the cell (or range) that you wish to retrieve
  information for. If the range parameter is omitted, the CELL function will
  assume that you are retrieving information for the last cell that was changed.

  "range" is NOT OPTIONAL here because we don't know the last cell changed !!!
}
var
  stype: String;
  r1, c1: Cardinal;
  cell: PCell;
  cellfmt: TsCellFormat;
begin
  if Length(Args)=1 then
  begin
    // This case is not supported by us, but it is by Excel.
    // Therefore the error is not quite correct...
    Result := ErrorResult(errIllegalRef);
    exit;
  end;

  stype := lowercase(ArgToString(Args[0]));

  case Args[1].ResultType of
    rtCell:
      begin
        cell := ArgToCell(Args[1]);
        r1 := Args[1].ResRow;
        c1 := Args[1].ResCol;
      end;
    rtCellRange:
      begin
        r1 := Args[1].ResCellRange.Row1;
        c1 := Args[1].ResCellRange.Col1;
        cell := (Args[1].Worksheet as TsWorksheet).FindCell(r1, c1);
      end;
    else
      Result := ErrorResult(errWrongType);
      exit;
  end;
  if cell <> nil then
    cellfmt := TsWorksheet(cell^.Worksheet).ReadCellFormat(cell)
  else
    InitFormatRecord(cellfmt);

  if stype = 'address' then
    Result := StringResult(GetCellString(r1, c1, []))
  else
  if stype = 'col' then
    Result := IntegerResult(c1+1)
  else
  if stype = 'color' then
  begin
    if (cell <> nil) and (cellfmt.NumberFormat = nfCurrencyRed) then
      Result := IntegerResult(1)
    else
      Result := IntegerResult(0);
  end else
  if stype = 'contents' then
  begin
    if cell = nil then
      Result := IntegerResult(0)
    else
      case cell^.ContentType of
        cctNumber     : if frac(cell^.NumberValue) = 0 then
                          Result := IntegerResult(trunc(cell^.NumberValue))
                        else
                          Result := FloatResult(cell^.NumberValue);
        cctDateTime   : Result := DateTimeResult(cell^.DateTimeValue);
        cctUTF8String : Result := StringResult(cell^.UTF8StringValue);
        cctBool       : Result := BooleanResult(cell^.BoolValue);
        cctError      : Result := ErrorResult(cell^.ErrorValue);
      end;
  end else
  if stype = 'filename' then
    Result := Stringresult(
      ExtractFilePath((Args[1].Worksheet as TsWorksheet).Workbook.FileName) + '[' +
      ExtractFileName((Args[1].Worksheet as TsWorksheet).Workbook.FileName) + ']' +
      Args[1].Worksheet.Name
    )
  else
  if stype = 'format' then begin
    Result := StringResult('G');
    if cell <> nil then
      case cellfmt.NumberFormat of
        nfGeneral:
          Result := StringResult('G');
        nfFixed:
          if cellfmt.NumberFormatStr= '0' then Result := StringResult('0') else
          if cellfmt.NumberFormatStr = '0.00' then  Result := StringResult('F0');
        nfFixedTh:
          if cellfmt.NumberFormatStr = '#,##0' then Result := StringResult(',0') else
          if cellfmt.NumberFormatStr = '#,##0.00' then Result := StringResult(',2');
        nfPercentage:
          if cellfmt.NumberFormatStr = '0%' then Result := StringResult('P0') else
          if cellfmt.NumberFormatStr = '0.00%' then Result := StringResult('P2');
        nfExp:
          if cellfmt.NumberFormatStr = '0.00E+00' then Result := StringResult('S2');
        nfShortDate, nfLongDate, nfShortDateTime:
          Result := StringResult('D4');
        nfLongTimeAM:
          Result := StringResult('D6');
        nfShortTimeAM:
          Result := StringResult('D7');
        nfLongTime:
          Result := StringResult('D8');
        nfShortTime:
          Result := StringResult('D9');
      end;
  end else
  if stype = 'prefix' then
  begin
    Result := StringResult('');
    if (cell^.ContentType = cctUTF8String) then
      case cellfmt.HorAlignment of
        haLeft  : Result := StringResult('''');
        haCenter: Result := StringResult('^');
        haRight : Result := StringResult('"');
      end;
  end else
  if stype = 'row' then
    Result := IntegerResult(r1+1)
  else
  if stype = 'type' then begin
    if (cell = nil) or (cell^.ContentType = cctEmpty) then
      Result := StringResult('b')
    else if cell^.ContentType = cctUTF8String then begin
      if (cell^.UTF8StringValue = '')
        then Result := StringResult('b')
        else Result := StringResult('l');
    end else
      Result := StringResult('v');
  end else
  if stype = 'width' then
    Result := FloatResult((Args[1].Worksheet as TsWorksheet).GetColWidth(c1, suChars))
  else
    Result := ErrorResult(errWrongType);
end;

procedure fpsERRORTYPE(var Result: TsExpressionResult; const Args: TsExprParameterArray);
//  ERROR.TYPE(value)
// returns the numeric representation of one of the errors in Excel.
// "value" can be one of the following Excel error values
//   #NULL! 	#DIV/0! 	#VALUE! 	#REF! 	#NAME? 	#NUM!   #N/A 	#GETTING_DATA
begin
  if (Args[0].ResultType = rtError) and (ord(Args[0].ResError) <= ord(errArgError))
  then
    Result := IntegerResult(ord(Args[0].ResError))
  else
    Result := EmptyResult; //ErrorResult(errArgError);
end;

procedure fpsISBLANK(var Result: TsExpressionResult; const Args: TsExprParameterArray);
//  ISBLANK( value )
// Checks for blank or null values.
// "value" is the value that you want to test.
// If "value" is blank, this function will return TRUE.
// If "value" is not blank, the function will return FALSE.
var
  cell: PCell;
begin
  Result := BooleanResult(false);
  case Args[0].ResultType of
    rtEmpty : Result := BooleanResult(true);
//    rtString: Result := BooleanResult(Args[0].ResString = '');  --> Excel returns false here!
    rtCell  : begin
                cell := ArgToCell(Args[0]);
                if (cell = nil) or (cell^.ContentType = cctEmpty) then
                  Result := BooleanResult(true);
              end;
  end;
end;

procedure fpsISERR(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// ISERR( value )
// If "value" is an error value (except #N/A), this function will return TRUE.
// Otherwise, it will return FALSE.
var
  cell: PCell;
begin
  Result := BooleanResult(false);
  if (Args[0].ResultType = rtCell) then
  begin
    cell := ArgToCell(Args[0]);
    if (cell <> nil) and (cell^.ContentType = cctError) and (cell^.ErrorValue <> errArgError)
      then Result := BooleanResult(true);
  end else
  if (Args[0].ResultType = rtError) and (Args[0].ResError <> errArgError) then
    Result := BooleanResult(true);
end;

procedure fpsISERROR(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// ISERROR( value )
// If "value" is an error value (#N/A, #VALUE!, #REF!, #DIV/0!, #NUM!, #NAME?
// or #NULL), this function will return TRUE. Otherwise, it will return FALSE.
var
  cell: PCell;
begin
  Result := BooleanResult(false);
  if (Args[0].ResultType = rtCell) then
  begin
    cell := ArgToCell(Args[0]);
    if (cell <> nil) and (cell^.ContentType = cctError) and (cell^.ErrorValue <= errArgError)
      then Result := BooleanResult(true);
  end else
  if (Args[0].ResultType = rtError) then
    Result := BooleanResult(true);
end;

procedure fpsISLOGICAL(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// ISLOGICAL( value )
var
  cell: PCell;
begin
  Result := BooleanResult(false);
  if (Args[0].ResultType = rtCell) then
  begin
    cell := ArgToCell(Args[0]);
    if (cell <> nil) and (cell^.ContentType = cctBool) then
      Result := BooleanResult(true);
  end else
  if (Args[0].ResultType = rtBoolean) then
    Result := BooleanResult(true);
end;

procedure fpsISNA(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// ISNA( value )
// If "value" is a #N/A error value , this function will return TRUE.
// Otherwise, it will return FALSE.
var
  cell: PCell;
begin
  Result := BooleanResult(false);
  if (Args[0].ResultType = rtCell) then
  begin
    cell := ArgToCell(Args[0]);
    if (cell <> nil) and (cell^.ContentType = cctError) and (cell^.ErrorValue = errArgError)
      then Result := BooleanResult(true);
  end else
  if (Args[0].ResultType = rtError) and (Args[0].ResError = errArgError) then
    Result := BooleanResult(true);
end;

procedure fpsISNONTEXT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// ISNONTEXT( value )
var
  cell: PCell;
begin
  Result := BooleanResult(false);
  if (Args[0].ResultType = rtCell) then
  begin
    cell := ArgToCell(Args[0]);
    if (cell = nil) or ((cell <> nil) and (cell^.ContentType <> cctUTF8String)) then
      Result := BooleanResult(true);
  end else
  if (Args[0].ResultType <> rtString) then
    Result := BooleanResult(true);
end;

procedure fpsISNUMBER(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// ISNUMBER( value )
// Tests "value" for a number (or date/time - checked with Excel).
var
  cell: PCell;
begin
  Result := BooleanResult(false);
  if (Args[0].ResultType = rtCell) then
  begin
    cell := ArgToCell(Args[0]);
    if (cell <> nil) and (cell^.ContentType in [cctNumber, cctDateTime]) then
      Result := BooleanResult(true);
  end else
  if (Args[0].ResultType in [rtFloat, rtInteger, rtDateTime]) then
    Result := BooleanResult(true);
end;

procedure fpsISREF(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// ISREF( value )
begin
  Result := BooleanResult(Args[0].ResultType in [rtCell, rtCellRange]);
end;

procedure fpsISTEXT(var Result: TsExpressionResult; const Args: TsExprParameterArray);
// ISTEXT( value )
var
  cell: PCell;
begin
  Result := BooleanResult(false);
  if (Args[0].ResultType = rtCell) then
  begin
    cell := ArgToCell(Args[0]);
    if (cell <> nil) and (cell^.ContentType = cctUTF8String) then
      Result := BooleanResult(true);
  end else
  if (Args[0].ResultType = rtString) then
    Result := BooleanResult(true);
end;


{------------------------------------------------------------------------------}
{    Builtin lookup/reference functions                                        }
{------------------------------------------------------------------------------}

procedure fpsCOLUMN(var Result: TsExpressionResult;
  const Args: TsExprParameterArray);
{ COLUMN( [reference] )
  Returns the column number of a cell reference (starting at 1!)
  "reference" is a reference to a cell or range of cells.
  If omitted, it is assumed that the reference is the cell address in which the
  COLUMN function has been entered in. }
begin
  Result := ErrorResult(errArgError);
  if Length(Args) = 0 then
    exit;  // We don't know here which cell contains the formula.
  case Args[0].ResultType of
    rtCell     : Result := IntegerResult(Args[0].ResCol + 1);
    rtCellRange: Result := IntegerResult(Args[0].ResCellRange.Col1 + 1);
    else         Result := ErrorResult(errWrongType);
  end;
end;

procedure fpsHYPERLINK(var Result: TsExpressionResult;
  const Args: TsExprParameterArray);
begin
  if Args[0].ResultType = rtError then
  begin
    Result := ErrorResult(errWrongType);
    exit;
  end;
  if (Length(Args) > 1) and (Args[1].ResultType = rtError) then
  begin
    Result := ErrorResult(errWrongType);
    exit;
  end;
  Result.ResString := ArgToString(Args[0]);
  if Length(Args) > 1 then Result.ResString := Result.ResString + HYPERLINK_SEPARATOR + ArgToString(Args[1]);
  Result.ResultType := rtHyperlink;
end;

procedure fpsMATCH(var Result: TsExpressionResult;
  const Args: TsExprParameterArray);
{ MATCH( value, array, [match_type]
   match_type = 1 (default): The MATCH function will find the largest value
     that is less than or equal to value. You should be sure to sort your
     array in ascending order.
   match_type = 0: The MATCH function will find the first value that is equal to
     value. The array can be sorted in any order.)
   match_type = -1: The MATCH function will find the smallest value that is
     greater than or equal to value. You should be sure to sort your array in
     descending order. }
var
  match_type: Integer;
  searchString: String;
  numSearchValue: Double = 0.0;
  r1,c1,r2,c2: Cardinal;
  r, c: Integer;
  IsCol: Boolean;
  arg: TsExpressionResult;
  sheet: TsWorksheet;
  book: TsWorkbook;

  function Matches(ACell: PCell): Boolean;
  var
    cellval: Double;
    s: String;
  begin
    Result := false;
    if ACell = nil then exit;
    if ACell^.ContentType = cctUTF8String then begin
      s := ACell^.UTF8StringValue;
      if IsWild(searchString, '*?', false) then
        Result := FindPart(searchString, s) > 0
        // NOTE: FindPart currently supports only the wildcard '?'
      else
        Result := SameStr(s, searchString);
    end else
    begin
      case ACell^.ContentType of
        cctNumber: cellval := ACell^.Numbervalue;
        cctDateTime: cellval := ACell^.DateTimeValue;
        cctBool: cellval := double(ord(ACell^.BoolValue));
        cctError: cellval := double(ord(ACell^.ErrorValue));
        cctEmpty: exit;
      end;
      case match_type of
        1 : Result := cellval <= numSearchValue;
        0 : Result := cellval = numSearchValue;
       -1 : Result := cellval >= numSearchValue;
      end;
    end;
  end;

begin
  Result := ErrorResult(errArgError);

  if Length(Args) > 2 then
    match_type := ArgToInt(Args[2])
  else
    match_type := 1;
  if not ((match_type in [0, 1]) or (match_type = -1)) then
    match_type := 1;

  arg := Args[1];
  if arg.ResultType <> rtCellRange then
    exit;

  if arg.ResCellRange.Sheet1 <> arg.ResCellRange.Sheet2 then
    exit;

  r1 := arg.ResCellRange.Row1;
  r2 := arg.ResCellRange.Row2;
  c1 := arg.ResCellRange.Col1;
  c2 := arg.ResCellRange.Col2;

  if r1=r2 then
    IsCol := false
  else
  if c1=c2 then
    IsCol := true
  else begin
    Result := ErrorResult(errArgError);
    exit;
  end;
  sheet := arg.Worksheet as TsWorksheet;
  book := sheet.Workbook as TsWorkbook;
  sheet := book.GetWorksheetByIndex(arg.ResCellRange.Sheet1);

  if Args[0].ResultType = rtString then
    searchString := ArgToString(Args[0])
  else begin
    numSearchvalue := ArgToFloat(Args[0]);
    if IsNaN(numSearchValue) then begin
      Result := ErrorResult(errWrongType);
      exit;
    end;
  end;

  if IsCol then
  begin
    for r := r2 downto r1 do
      if Matches(sheet.FindCell(r, c1)) then begin
        Result := IntegerResult(r - integer(r1) + 1);
        exit;
      end;
  end else
  begin
    for c := c2 downto c1 do
      if Matches(sheet.FindCell(r1, c)) then begin
        Result := IntegerResult(c - Integer(c1) + 1);
        exit;
      end;
  end;

  // If the procedure gets here, not match has been found --> return error #N/A
end;

procedure fpsROW(var Result: TsExpressionResult;
  const Args: TsExprParameterArray);
{ ROW( [reference] )
  Returns the row number of a cell reference (starting at 1!)
  "reference" is a reference to a cell or range of cells.
  If omitted, it is assumed that the reference is the cell address in which the
  ROW function has been entered in. }
begin
  Result := ErrorResult(errArgError);
  if Length(Args) = 0 then
    exit;  // We don't know here which cell contains the formula.
  case Args[0].ResultType of
    rtCell     : Result := IntegerResult(Args[0].ResRow + 1);
    rtCellRange: Result := IntegerResult(Args[0].ResCellRange.Row1 + 1);
    else         Result := ErrorResult(errWrongType);
  end;
end;


{------------------------------------------------------------------------------}
{   Registration                                                               }
{------------------------------------------------------------------------------}

{@@ Registers the standard built-in functions. Called automatically. }
procedure RegisterStdBuiltins(AManager : TComponent);
var
  cat: TsBuiltInExprCategory;
begin
  with AManager as TsBuiltInExpressionManager do
  begin
    // Math functions
    cat := bcMath;
    AddFunction(cat, 'ABS',       'F', 'F',    INT_EXCEL_SHEET_FUNC_ABS,        @fpsABS);
    AddFunction(cat, 'ACOS',      'F', 'F',    INT_EXCEL_SHEET_FUNC_ACOS,       @fpsACOS);
    AddFunction(cat, 'ACOSH',     'F', 'F',    INT_EXCEL_SHEET_FUNC_ACOSH,      @fpsACOSH);
    AddFunction(cat, 'ASIN',      'F', 'F',    INT_EXCEL_SHEET_FUNC_ASIN,       @fpsASIN);
    AddFunction(cat, 'ASINH',     'F', 'F',    INT_EXCEL_SHEET_FUNC_ASINH,      @fpsASINH);
    AddFunction(cat, 'ATAN',      'F', 'F',    INT_EXCEL_SHEET_FUNC_ATAN,       @fpsATAN);
    AddFunction(cat, 'ATANH',     'F', 'F',    INT_EXCEL_SHEET_FUNC_ATANH,      @fpsATANH);
    AddFunction(cat, 'CEILING',   'F', 'FF',   INT_EXCEL_SHEET_FUNC_CEILING,    @fpsCEILING);
    AddFunction(cat, 'COS',       'F', 'F',    INT_EXCEL_SHEET_FUNC_COS,        @fpsCOS);
    AddFunction(cat, 'COSH',      'F', 'F',    INT_EXCEL_SHEET_FUNC_COSH,       @fpsCOSH);
    AddFunction(cat, 'DEGREES',   'F', 'F',    INT_EXCEL_SHEET_FUNC_DEGREES,    @fpsDEGREES);
    AddFunction(cat, 'EVEN',      'I', 'F',    INT_EXCEL_SHEET_FUNC_EVEN,       @fpsEVEN);
    AddFunction(cat, 'EXP',       'F', 'F',    INT_EXCEL_SHEET_FUNC_EXP,        @fpsEXP);
    AddFunction(cat, 'FACT',      'F', 'I',    INT_EXCEL_SHEET_FUNC_FACT,       @fpsFACT);
    AddFunction(cat, 'FLOOR',     'F', 'FF',   INT_EXCEL_SHEET_FUNC_FLOOR,      @fpsFLOOR);
    AddFunction(cat, 'INT',       'I', 'F',    INT_EXCEL_SHEET_FUNC_INT,        @fpsINT);
    AddFunction(cat, 'LN',        'F', 'F',    INT_EXCEL_SHEET_FUNC_LN,         @fpsLN);
    AddFunction(cat, 'LOG',       'F', 'Ff',   INT_EXCEL_SHEET_FUNC_LOG,        @fpsLOG);
    AddFunction(cat, 'LOG10',     'F', 'F',    INT_EXCEL_SHEET_FUNC_LOG10,      @fpsLOG10);
    AddFunction(cat, 'MOD',       'I', 'II',   INT_EXCEL_SHEET_FUNC_MOD,        @fpsMOD);
    AddFunction(cat, 'ODD',       'I', 'F',    INT_EXCEL_SHEET_FUNC_ODD,        @fpsODD);
    AddFunction(cat, 'PI',        'F', '',     INT_EXCEL_SHEET_FUNC_PI,         @fpsPI);
    AddFunction(cat, 'POWER',     'F', 'FF',   INT_EXCEL_SHEET_FUNC_POWER,      @fpsPOWER);
    AddFunction(cat, 'RADIANS',   'F', 'F',    INT_EXCEL_SHEET_FUNC_RADIANS,    @fpsRADIANS);
    AddFunction(cat, 'RAND',      'F', '',     INT_EXCEL_SHEET_FUNC_RAND,       @fpsRAND);
    AddFunction(cat, 'ROUND',     'F', 'FF',   INT_EXCEL_SHEET_FUNC_ROUND,      @fpsROUND);
    AddFunction(cat, 'ROUNDDOWN', 'F', 'F',    INT_EXCEL_SHEET_FUNC_ROUNDDOWN,  @fpsROUNDDOWN);
    AddFunction(cat, 'SIGN',      'F', 'F',    INT_EXCEL_SHEET_FUNC_SIGN,       @fpsSIGN);
    AddFunction(cat, 'SIN',       'F', 'F',    INT_EXCEL_SHEET_FUNC_SIN,        @fpsSIN);
    AddFunction(cat, 'SINH',      'F', 'F',    INT_EXCEL_SHEET_FUNC_SINH,       @fpsSINH);
    AddFunction(cat, 'SQRT',      'F', 'F',    INT_EXCEL_SHEET_FUNC_SQRT,       @fpsSQRT);
    AddFunction(cat, 'TAN',       'F', 'F',    INT_EXCEL_SHEET_FUNC_TAN,        @fpsTAN);
    AddFunction(cat, 'TANH',      'F', 'F',    INT_EXCEL_SHEET_FUNC_TANH,       @fpsTANH);

    // Date/time
    cat := bcDateTime;
    AddFunction(cat, 'DATE',      'D', 'III',  INT_EXCEL_SHEET_FUNC_DATE,       @fpsDATE);
    AddFunction(cat, 'DATEDIF',   'F', 'DDS',  INT_EXCEL_SHEET_FUNC_DATEDIF,    @fpsDATEDIF);
    AddFunction(cat, 'DATEVALUE', 'D', 'S',    INT_EXCEL_SHEET_FUNC_DATEVALUE,  @fpsDATEVALUE);
    AddFunction(cat, 'DAY',       'I', '?',    INT_EXCEL_SHEET_FUNC_DAY,        @fpsDAY);
    AddFunction(cat, 'HOUR',      'I', '?',    INT_EXCEL_SHEET_FUNC_HOUR,       @fpsHOUR);
    AddFunction(cat, 'MINUTE',    'I', '?',    INT_EXCEL_SHEET_FUNC_MINUTE,     @fpsMINUTE);
    AddFunction(cat, 'MONTH',     'I', '?',    INT_EXCEL_SHEET_FUNC_MONTH,      @fpsMONTH);
    AddFunction(cat, 'NOW',       'D', '',     INT_EXCEL_SHEET_FUNC_NOW,        @fpsNOW);
    AddFunction(cat, 'SECOND',    'I', '?',    INT_EXCEL_SHEET_FUNC_SECOND,     @fpsSECOND);
    AddFunction(cat, 'TIME' ,     'D', 'III',  INT_EXCEL_SHEET_FUNC_TIME,       @fpsTIME);
    AddFunction(cat, 'TIMEVALUE', 'D', 'S',    INT_EXCEL_SHEET_FUNC_TIMEVALUE,  @fpsTIMEVALUE);
    AddFunction(cat, 'TODAY',     'D', '',     INT_EXCEL_SHEET_FUNC_TODAY,      @fpsTODAY);
    AddFunction(cat, 'WEEKDAY',   'I', '?i',   INT_EXCEL_SHEET_FUNC_WEEKDAY,    @fpsWEEKDAY);
    AddFunction(cat, 'YEAR',      'I', '?',    INT_EXCEL_SHEET_FUNC_YEAR,       @fpsYEAR);

    // Strings
    cat := bcStrings;
    AddFunction(cat, 'CHAR',      'S', 'I',    INT_EXCEL_SHEET_FUNC_CHAR,       @fpsCHAR);
    AddFunction(cat, 'CODE',      'I', 'S',    INT_EXCEL_SHEET_FUNC_CODE,       @fpsCODE);
    AddFunction(cat, 'CONCATENATE','S','S+',   INT_EXCEL_SHEET_FUNC_CONCATENATE,@fpsCONCATENATE);
    AddFunction(cat, 'EXACT',     'B', 'SS',   INT_EXCEL_SHEET_FUNC_EXACT,      @fpsEXACT);
    AddFunction(cat, 'LEFT',      'S', 'Si',   INT_EXCEL_SHEET_FUNC_LEFT,       @fpsLEFT);
    AddFunction(cat, 'LEN',       'I', 'S',    INT_EXCEL_SHEET_FUNC_LEN,        @fpsLEN);
    AddFunction(cat, 'LOWER',     'S', 'S',    INT_EXCEL_SHEET_FUNC_LOWER,      @fpsLOWER);
    AddFunction(cat, 'MID',       'S', 'SII',  INT_EXCEL_SHEET_FUNC_MID,        @fpsMID);
    AddFunction(cat, 'REPLACE',   'S', 'SIIS', INT_EXCEL_SHEET_FUNC_REPLACE,    @fpsREPLACE);
    AddFunction(cat, 'REPT',      'S', 'SI',   INT_EXCEL_SHEET_FUNC_REPT,       @fpsREPT);
    AddFunction(cat, 'RIGHT',     'S', 'Si',   INT_EXCEL_SHEET_FUNC_RIGHT,      @fpsRIGHT);
    AddFunction(cat, 'SUBSTITUTE','S', 'SSSi', INT_EXCEL_SHEET_FUNC_SUBSTITUTE, @fpsSUBSTITUTE);
    AddFunction(cat, 'TEXT',      'S', '?S',   INT_EXCEL_SHEET_FUNC_TEXT,       @fpsTEXT);
    AddFunction(cat, 'TRIM',      'S', 'S',    INT_EXCEL_SHEET_FUNC_TRIM,       @fpsTRIM);
    AddFunction(cat, 'UPPER',     'S', 'S',    INT_EXCEL_SHEET_FUNC_UPPER,      @fpsUPPER);
    AddFunction(cat, 'VALUE',     'F', 'S',    INT_EXCEL_SHEET_FUNC_VALUE,      @fpsVALUE);

    // Logical
    cat := bcLogical;
    AddFunction(cat, 'AND',       'B', 'B+',   INT_EXCEL_SHEET_FUNC_AND,        @fpsAND);
    AddFunction(cat, 'FALSE',     'B', '',     INT_EXCEL_SHEET_FUNC_FALSE,      @fpsFALSE);
    AddFunction(cat, 'IF',        'B', 'B?+',  INT_EXCEL_SHEET_FUNC_IF,         @fpsIF);
    AddFunction(cat, 'NOT',       'B', 'B',    INT_EXCEL_SHEET_FUNC_NOT,        @fpsNOT);
    AddFunction(cat, 'OR',        'B', 'B+',   INT_EXCEL_SHEET_FUNC_OR,         @fpsOR);
    AddFunction(cat, 'TRUE',      'B', '',     INT_EXCEL_SHEET_FUNC_TRUE ,      @fpsTRUE);

    // Statistical
    cat := bcStatistics;
    AddFunction(cat, 'AVEDEV',    'F', 'F+',   INT_EXCEL_SHEET_FUNC_AVEDEV,     @fpsAVEDEV);
    AddFunction(cat, 'AVERAGE',   'F', 'F+',   INT_EXCEL_SHEET_FUNC_AVERAGE,    @fpsAVERAGE);
    AddFunction(cat, 'AVERAGEIF', 'F', 'R?r',  INT_EXCEL_SHEET_FUNC_NOT_BIFF,   @fpsAVERAGEIF);
    AddFunction(cat, 'COUNT',     'I', '?+',   INT_EXCEL_SHEET_FUNC_COUNT,      @fpsCOUNT);
    AddFunction(cat, 'COUNTA',    'I', '?+',   INT_EXCEL_SHEET_FUNC_COUNTA,     @fpsCOUNTA);
    AddFunction(cat, 'COUNTBLANK','I', 'R',    INT_EXCEL_SHEET_FUNC_COUNTBLANK, @fpsCOUNTBLANK);
    AddFunction(cat, 'COUNTIF',   'I', 'R?',   INT_EXCEL_SHEET_FUNC_COUNTIF,    @fpsCOUNTIF);
    AddFunction(cat, 'MAX',       'F', 'F+',   INT_EXCEL_SHEET_FUNC_MAX,        @fpsMAX);
    AddFunction(cat, 'MIN',       'F', 'F+',   INT_EXCEL_SHEET_FUNC_MIN,        @fpsMIN);
    AddFunction(cat, 'PRODUCT',   'F', 'F+',   INT_EXCEL_SHEET_FUNC_PRODUCT,    @fpsPRODUCT);
    AddFunction(cat, 'STDEV',     'F', 'F+',   INT_EXCEL_SHEET_FUNC_STDEV,      @fpsSTDEV);
    AddFunction(cat, 'STDEVP',    'F', 'F+',   INT_EXCEL_SHEET_FUNC_STDEVP,     @fpsSTDEVP);
    AddFunction(cat, 'SUM',       'F', 'F+',   INT_EXCEL_SHEET_FUNC_SUM,        @fpsSUM);
    AddFunction(cat, 'SUMIF',     'F', 'R?r',  INT_EXCEL_SHEET_FUNC_SUMIF,      @fpsSUMIF);
    AddFunction(cat, 'SUMSQ',     'F', 'F+',   INT_EXCEL_SHEET_FUNC_SUMSQ,      @fpsSUMSQ);
    AddFunction(cat, 'VAR',       'F', 'F+',   INT_EXCEL_SHEET_FUNC_VAR,        @fpsVAR);
    AddFunction(cat, 'VARP',      'F', 'F+',   INT_EXCEL_SHEET_FUNC_VARP,       @fpsVARP);

    // Info functions
    cat := bcInfo;
    //AddFunction(cat, 'CELL',      '?', 'Sr',   INT_EXCEL_SHEET_FUNC_CELL,       @fpsCELL);
    AddFunction(cat, 'ERROR.TYPE','I', '?',    INT_EXCEL_SHEET_FUNC_ERRORTYPE,  @fpsERRORTYPE);
    AddFunction(cat, 'ISBLANK',   'B', '?',    INT_EXCEL_SHEET_FUNC_ISBLANK,    @fpsISBLANK);
    AddFunction(cat, 'ISERR',     'B', '?',    INT_EXCEL_SHEET_FUNC_ISERR,      @fpsISERR);
    AddFunction(cat, 'ISERROR',   'B', '?',    INT_EXCEL_SHEET_FUNC_ISERROR,    @fpsISERROR);
    AddFunction(cat, 'ISLOGICAL', 'B', '?',    INT_EXCEL_SHEET_FUNC_ISLOGICAL,  @fpsISLOGICAL);
    AddFunction(cat, 'ISNA',      'B', '?',    INT_EXCEL_SHEET_FUNC_ISNA,       @fpsISNA);
    AddFunction(cat, 'ISNONTEXT', 'B', '?',    INT_EXCEL_SHEET_FUNC_ISNONTEXT,  @fpsISNONTEXT);
    AddFunction(cat, 'ISNUMBER',  'B', '?',    INT_EXCEL_SHEET_FUNC_ISNUMBER,   @fpsISNUMBER);
    AddFunction(cat, 'ISREF',     'B', '?',    INT_EXCEL_SHEET_FUNC_ISREF,      @fpsISREF);
    AddFunction(cat, 'ISTEXT',    'B', '?',    INT_EXCEL_SHEET_FUNC_ISTEXT,     @fpsISTEXT);

    // Lookup / reference functions
    cat := bcLookup;
    AddFunction(cat, 'COLUMN',    'I', 'r',    INT_EXCEL_SHEET_FUNC_COLUMN,     @fpsCOLUMN);
    AddFunction(cat, 'HYPERLINK', 'S', 'Ss',   INT_EXCEL_SHEET_FUNC_HYPERLINK,  @fpsHYPERLINK);
    AddFunction(cat, 'MATCH',     'I', 'SRi',  INT_EXCEL_SHEET_FUNC_MATCH,      @fpsMATCH);
    AddFunction(cat, 'ROW',       'I', 'r',    INT_EXCEL_SHEET_FUNC_ROW,        @fpsROW);

    (*
    AddFunction(cat, 'COLUMN',    'I', 'R',    INT_EXCEL_SHEET_FUNC_COLUMN,     @fpsCOLUMN);
                  *)

  end;
end;


{ Lookup / reference functions }

(*
function fpsCOLUMNS(Args: TsArgumentStack; NumArgs: Integer): TsArgument;
{ COLUMNS( [reference] )
  returns the number of column in a cell reference. }
var
  arg: TsArgument;
begin
  Unused(NumArgs);
  arg := Args.Pop;
  case arg.ArgumentType of
    atCell     : Result := CreateNumberArg(1);
    atCellRange: Result := CreateNumberArg(arg.LastCol - arg.FirstCol + 1);
    else         Result := CreateErrorArg(errWrongType);
  end;
end;


function fpsROWS(Args: TsArgumentStack; NumArgs: Integer): TsArgument;
{ ROWS( [reference] )
  returns the number of rows in a cell reference. }
var
  arg: TsArgument;
begin
  Unused(NumArgs);
  arg := Args.Pop;
  case arg.ArgumentType of
    atCell     : Result := CreateNumberArg(1);
    atCellRange: Result := CreateNumberArg(arg.LastRow - arg.FirstRow + 1);
    else         Result := CreateErrorArg(errWrongType);
  end;
end;

*)

(*
function fpsINFO(Args: TsArgumentStack; NumArgs: Integer): TsArgument;
{  INFO( type )
   returns information about the operating environment.
   type can be one of the following values:
     + "directory"    Path of the current directory.
     + "numfile"      Number of active worksheets.
     - "origin"       The cell that is in the top, left-most cell visible in the current Excel spreadsheet.
     - "osversion"    Operating system version.
     - "recalc"       Returns the recalculation mode - either Automatic or Manual.
     - "release"      Version of Excel that you are running.
     - "system"       Name of the operating environment.
   ONLY THOSE MARKED BY "+" ARE SUPPORTED! }
var
  arg: TsArgument;
  workbook: TsWorkbook;
  s: String;
begin
  Unused(NumArgs);
  arg := Args.Pop;
  if arg.ArgumentType <> atString then
    Result := CreateErrorArg(errWrongType)
  else begin
    s := Lowercase(arg.StringValue);
    workbook := arg.Worksheet.Workbook;
    if s = 'directory' then
      Result := CreateStringArg(ExtractFilePath(workbook.FileName))
    else
    if s = 'numfile' then
      Result := CreateNumberArg(workbook.GetWorksheetCount)
    else
      Result := CreateErrorArg(errFormulaNotSupported);
  end;
end;

*)


end.
