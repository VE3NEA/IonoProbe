unit SolArray;

interface

uses SysUtils, Classes, GeomFun, Utc, Math;

const
  SEC1 = 1/86400;
  MIN1 = 1/24/60;
  HOUR1 = 1/24;

type
  TDataKind = (dkSsn, dkSfi, dkXRayBg, dkProtonBg, dkAp, dkKp3, dkKp1,
    {dkCostello,} dkIntAp, dkAp1, dkAurora, dkProtons, dkXrays, dkSsne);


  TValueKind = (vkNone, vkEstimated, vkMeasured);
  TValueKindArray = array of TValueKind;

  TAlertKind = (alPastG, alPastS, alPastR, alNextG, alNextS, alNextR);

  TGeoAlerts = class
  private
    FValues: array [TAlertKind] of integer;
    function GetValue(Index: TAlertKind): integer;
  public
    Issued: TDateTime;

    procedure Import(Lst: TStringList);
    procedure LoadFromFile(FilePath: TFileName);
    procedure SaveToFile(FilePath: TFileName);

    property Value [Index: TAlertKind]: integer read GetValue; default;
  end;


  TSolarArray = class
  protected
    FData: TSingleArray;
    FLeftCnt: integer;
    FStep: TDateTime;
    FKind: TValueKindArray;
    FUpdateTime: TDateTime;
    FStepX: integer;
    FStormThreshold: Single;
    function GetStartTime: TDateTime;
    function AddValue(AValue: Single; ATime: TDateTime; AKind: TValueKind):boolean;
    function LoadLine(Line: string; Idx: integer): boolean; virtual;
    function SaveLine(Idx: integer): string; virtual;
  public
    Title: string;
    procedure ImportText(Lst: TStrings); virtual; abstract;
    procedure LoadFromFile(FilePath: TFileName);
    procedure SaveToFile(FilePath: TFileName);
    procedure Update; virtual;
    function TimeToIdx(T: TDateTime): integer;
    function TimeToIdxFrac(T: TDateTime): Single;
    function IdxToTime(Idx: integer): TDateTime;
    function ScaledData(Idx: integer): Single;
    function ScaleValue(V: Single): Single; virtual; abstract;
    function IsStorm(Default: boolean): boolean;

    property StepX: integer read FStepX;

    property StartTime: TDateTime read GetStartTime;
    property UpdateTime: TDateTime read FUpdateTime;
    property Step: TDateTime read FStep;

    property Data: TSingleArray read FData;
    property Kind: TValueKindArray read FKind;
    property StormThreshold: Single read FStormThreshold;
  end;

  TSsnArray = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;

  TSfiArray = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;

  TXRayBgArray = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;

  TProtonBgArray = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;

  TApArray = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;

  TKp3Array = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;

  TKp1Array = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;

{
  TCostelloArray = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;
}

  TIntApArray = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;

  TAp1Array = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;

  TAuroraArray = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;

  TProtonArray = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;

  TXRayArray = class (TSolarArray)
  public
    constructor Create;
    procedure ImportText(Lst: TStrings); override;
    function ScaleValue(V: Single): Single; override;
  end;




  function DecodeYYYY_MM_DD(Str: string): TDateTime;
  function DecodeDD_mmm_YY(Str: string): TDateTime;
  function DecodeHHMM(Str: string): TDateTime;
  function DecodeYYYY_mmm_DD(Str: string): TDateTime;
  function DecodeMmmDD_YYYY(Str: string): TDateTime;
  function DecodeDD_mmm(Str: string): TDateTime;
  function DecodeDD_HHHH(Str: string): TDateTime;
  function DecodeXRAY(Str: string): Single;
  function EncodeXRAY(V: Single; Num: boolean): string;
  function MonthFromName(Str: string): WORD;
  function SkipToStr(Str: string; Lst: TStrings; var p: integer): boolean;
  function IsComment(Str: string): boolean;
  function FormatExp(V: Single): string;



implementation


//------------------------------------------------------------------------------
//                          TSolarArray
//------------------------------------------------------------------------------

{ TSolarArray }

function TSolarArray.IdxToTime(Idx: integer): TDateTime;
begin
  Result := StartTime + Idx * Step;
end;





//if StartTime = 0 and Step = 15 min then
//T = (00:00:00..00:14:59)  ->  Result = (0 .. 0.99999)
function TSolarArray.TimeToIdxFrac(T: TDateTime): Single;
begin
  Result := (T - StartTime + SEC1) / Step;
end;


procedure TSolarArray.Update;
var
  Shift: integer;
  i: integer;
begin
  //calc shift
  Shift := TimeToIdx(UtcNow) - FLeftCnt;
  if Shift = 0 then Exit;
  FUpdateTime := FUpdateTime + Step * Shift;

  //negative shift is nonsense, clear the array
  if Shift < 0 then Shift := Length(FData);

  //move data
  if Shift < Length(FData) then
    for i:=0 to Length(FData) - Shift - 1 do
      begin
      FData[i] := FData[i+Shift];
      FKind[i] := FKind[i+Shift];
      end;
  //clear space
  for i:=Max(0, Length(FData) - Shift) to High(Data) do
    FKind[i] := vkNone;
end;


function TSolarArray.TimeToIdx(T: TDateTime): integer;
begin
  Result := Floor(TimeToIdxFrac(T));
end;


function TSolarArray.GetStartTime: TDateTime;
begin
  Result := FUpdateTime - (FLeftCnt-1) * FStep;
end;


function TSolarArray.AddValue(AValue: Single; ATime: TDateTime;
  AKind: TValueKind): boolean;
var
  Idx: integer;
begin
  Result := false;
  Idx := TimeToIdx(ATime);
  if (Idx < 0) or (Idx > High(FData)) then Exit;

  //ovewrite if the same kind
  if FKind[Idx] > AKind then Exit;

  FData[Idx] := AValue;
  FKind[Idx] := AKind;
  Result := true;
end;




function TSolarArray.LoadLine(Line: string; Idx: integer): boolean;
var
  p: integer;
begin
  Result := false;
  p := Pos ('=', Line);
  if p < 2 then Exit;
  case Line[1] of
    'N': FKind[Idx] := vkNone;
    'E': FKind[Idx] := vkEstimated;
    'M': FKind[Idx] := vkMeasured;
    end;
  FData[Idx] := SafeStrToFloat(Copy(Line, p+1, MAXINT));
  Result := true;
end;


function TSolarArray.SaveLine(Idx: integer): string;
const
  Lett: array[TValueKind] of Char = ('N', 'E', 'M');
begin
  Result := Lett[FKind[Idx]] + '=' + SafeFloatToStr(FData[Idx]);
end;


procedure TSolarArray.LoadFromFile(FilePath: TFileName);
var
  i, Cnt: integer;
begin
  if not FileExists(FilePath) then Exit;
  Cnt := 0;

  try

    with TStringList.Create do
    try
      LoadFromFile(FilePath);
      FUpdateTime := IsoStrToDateTime(Strings[0]);
      Delete(0);

      for i:=0 to Min(Count-1, High(FData)) do
        if LoadLine(Strings[i], i) then Inc(Cnt);
    finally
      Free;
    end;

  except end;

  //clear missing elements
  for i:=Cnt to High(FKind) do FKind[i] := vkNone;
end;


procedure TSolarArray.SaveToFile(FilePath: TFileName);
var
  Cnt, i: integer;
begin
  Update;

  Cnt := Length(FData);
  while (Cnt > 0) and (FKind[Cnt-1] = vkNone) do Dec(Cnt);

  try
    with TStringList.Create do
    try
      Add(IsoDateTimeToStr(FUpdateTime));
      for i:=0 to Cnt-1 do Add(SaveLine(i));
      SaveToFile(FilePath);
    finally
      Free;
    end;
  except end;
end;


function TSolarArray.ScaledData(Idx: integer): Single;
begin
  Result := ScaleValue(FData[Idx]);
end;



function TSolarArray.IsStorm(Default: boolean): boolean;
var
  Idx: integer;
begin
  Result := Default;
  //index for current time
  Idx := TimeToIdx(UtcNow);
  if Idx < 0 then Exit;
  //get previous if current not available
  if (Idx > High(FData)) or (FKind[Idx] <> vkMeasured) then
    begin
    Dec(Idx);
    if (Idx < 0) or (Idx > High(FData)) or (FKind[Idx] <> vkMeasured) then Exit;
    end;
  //compare to thereshold
  Result := FData[Idx] >= FStormThreshold;
end;



function DecodeYYYY_MM_DD(Str: string): TDateTime;
var
  y, m, d: WORD;
begin
  y := StrToInt(Copy(Str, 1, 4));
  m := StrToInt(Copy(Str, 6, 2));
  d := StrToInt(Copy(Str, 9, 2));
  Result := EncodeDate(y, m, d);
end;


function MonthFromName(Str: string): WORD;
const
  Months = ':JAN:FEB:MAR:APR:MAY:JUN:JUL:AUG:SEP:OCT:NOV:DEC:';
begin
  if Length(Str) <> 3 then Result := 0 else
  Result := (Pos(':'+UpperCase(Str)+':', Months) + 3) div 4;
end;

function DecodeYYYY_mmm_DD(Str: string): TDateTime;
var
  y, m, d: WORD;
begin
  y := StrToInt(Copy(Str, 1, 4));
  m := MonthFromName(Copy(Str, 6, 3));
  d := StrToInt(Copy(Str, 10, 2));
  Result := EncodeDate(y, m, d);
end;


function DecodeMmmDD_YYYY(Str: string): TDateTime;
var
  y, m, d: WORD;
  p: integer;
begin
  p := Pos('-', Str);

  y := StrToInt(Copy(Str, p+1, 4));
  m := MonthFromName(Copy(Str, 1, 3));
  d := StrToInt(Copy(Str, 4, p-4));
  Result := EncodeDate(y, m, d);
end;



function DecodeDD_mmm_YY(Str: string): TDateTime;
var
  y, m, d: WORD;
begin
  y := StrToInt(Copy(Str, 6, 2)) + 2000;
  m := MonthFromName(Copy(Str, 3, 3));
  d := StrToInt(Copy(Str, 1, 2));
  Result := EncodeDate(y, m, d);
end;


function DecodeDD_mmm(Str: string): TDateTime;
var
  y, m, d: WORD;
  UT: TDateTime;
begin
  UT := UtcNow;
  DecodeDate(UT, y, m, d);
  d := StrToInt(Copy(Str, 1, 2));
  m := MonthFromName(Copy(Str, 4, 3));
  Result := EncodeDate(y, m, d);
  if Result > UT then Result := EncodeDate(y-1, m, d);
end;


function DecodeDD_HHHH(Str: string): TDateTime;
var
  y, m, d, NewD: WORD;
  UT: TDateTime;
begin
  UT := UtcNow;
  Newd := StrToInt(Copy(Str, 1, 2));

  try
    //try current month and year
    DecodeDate(UT, y, m, d);
    Result := EncodeDate(y, m, Newd);
    if Result > UT then Abort;
  except
    //try prev month
    DecodeDate(IncMonth(UT, -1), y, m, d);
    Result := EncodeDate(y, m, Newd);
  end;

  Result := Result + DecodeHHMM(Copy(Str, 4, 4));
end;


function DecodeHHMM(Str: string): TDateTime;
var
  h, m: WORD;
begin
  h := StrToInt(Copy(Str, 1, 2));
  m := StrToInt(Copy(Str, 3, 2));
  Result := EncodeTime(h, m, 0, 0);
end;


function DecodeHH_MM(Str: string): TDateTime;
var
  h, m: WORD;
begin
  h := StrToInt(Copy(Str, 1, 2));
  m := StrToInt(Copy(Str, 4, 2));
  Result := EncodeTime(h, m, 0, 0);
end;


function DecodeXRAY(Str: string): Single;
begin
  Result := -1; //compiler wants this
  if Length(Str) <> 4 then Abort;

  case Str[1] of
    'A': Result := 1E-8;
    'B': Result := 1E-7;
    'C': Result := 1E-6;
    'M': Result := 1E-5;
    'X': Result := 1E-4;
    else Abort;
  end;

  Result := Result * SafeStrToFloat(Trim(Copy(Str, 2, 4)));
end;


function EncodeXRAY(V: Single; Num: boolean): string;
const
  Chars: array[-8..-4] of Char = ('A', 'B', 'C', 'M', 'X');
var
  P: integer;
begin
  if V < 0 then begin Result := ''; Exit; end;

  P := Floor(Log10(V));
  if P < -8 then begin Result := '0'; Exit; end;

  Result := SafeDecimalFormat('%s%1.1f', [Chars[Min(P, -4)], V * Power(10, -p)]);
  //if Num then Result := Result + Format('  (%1.2e W/m2)', [V]);
  if Num then Result := Result + Format('  (%s W/m2)', [FormatExp(V)]);
end;


function FormatExp(V: Single): string;
begin
  Result := SafeDecimalFormat('%1.2e', [V]);
  if Length(Result) < 8 then Exit;
  Delete(Result, Length(Result)-2, 2);
end;


function SkipToStr(Str: string; Lst: TStrings; var p: integer): boolean;
begin
  Result := false;
  if Lst.Count = 0 then Exit;

  Result := true;
  while p < Lst.Count do
    if IsComment(Lst[p]) or (Pos(Str, Lst[p]) < 1)
      then Inc(p) else Exit;

  Result := false;
end;

function IsComment(Str: string): boolean;
begin
  Result := (Trim(Str) = '') or (Str[1] in [';', '#']);
end;






//------------------------------------------------------------------------------
//                         Descendant arrays
//------------------------------------------------------------------------------

{ TSsnArray }

//SESC sunspot number
//range about 0..400

constructor TSsnArray.Create;
begin
  Title := 'D1 Sunspot number';
  FStep := 1;                          //step = 1 day
  FLeftCnt := 365;                  //90 day history
  SetLength(FData, FLeftCnt + 1 + 45);  //45 day forecast
  SetLength(FKind, Length(Data));
  FStepX := 4;
  FStormThreshold := -1;
end;


procedure TSsnArray.ImportText(Lst: TStrings);
var
  i: integer;
  T: TDateTime;
  V: Single;
{
  S: string;
  p: integer;
}
begin
  if Pos('DSD.txt', Lst[0]) > 0 then
    for i:=1 to Lst.Count-1 do
      begin
      if IsComment(Lst[i]) or (Lst[i][1] <> '2') then Continue;
      try
        T := DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10));
        V := SafeStrToFloat(Copy(Lst[i], 20, 3));
        if V >= 0 then AddValue(V, T, vkMeasured);
      except end;
      end

{
  else if Pos('45DF.txt', Lst[0]) > 0 then
    begin
    p := 0; if SkipToStr('FLUX FORECAST', Lst, p) then Inc(p) else Exit;

    //Sfi forecast -> Ssn
    for i:=p to Lst.Count-1 do
      if Pos('FORECASTER:', Lst[i]) > 0 then Break else
        begin
        S := Lst[i];
        while Length(S) >= 11 do
          try
          T := DecodeDD_mmm_YY(Copy(S, 1, 7));
          V := StrToInt(Copy(S, 9, 3));
          if V >= 0 then AddValue(TIriModel.SfiToSsn(Round(V)), T, vkEstimated);
          Delete(S, 1, 12);
          except end;
        end;
    end;
}
end;


function TSsnArray.ScaleValue(V: Single): Single;
begin
  Result := V / 400;
end;



{ TSfiArray }

//Radio Flux Penticton 10.7 cm
//in units of 10 E-22/Sq Wm/Hz
//range 60..400 (?)

constructor TSfiArray.Create;
begin
  Title := 'D1 Solar Flux index';
  FStep := 1;                          //step = 1 day
  FLeftCnt := 365;                  //90 day history
  SetLength(FData, FLeftCnt + 1 + 45);  //45 day forecast
  SetLength(FKind, Length(Data));
  FStepX := 4;
  FStormThreshold := -1;
end;


procedure TSfiArray.ImportText(Lst: TStrings);
var
  i, p: integer;
  T: TDateTime;
  V: Single;
  S: string;
begin
  if Pos('DSD.txt', Lst[0]) > 0 then
    for i:=1 to Lst.Count-1 do
      try
        if IsComment(Lst[i]) or (Lst[i][1] <> '2') then Continue;
          T := DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10));
          V := SafeStrToFloat(Copy(Lst[i], 13, 3));
          if V >= 0 then AddValue(V, T, vkMeasured);
      except
      end


  else if Pos('45DF.txt', Lst[0]) > 0 then
    begin
    p := 0; if SkipToStr('FLUX FORECAST', Lst, p) then Inc(p) else Exit;

    //Sfi forecast
    for i:=p to Lst.Count-1 do
      if Pos('FORECASTER:', Lst[i]) > 0 then Break else
        begin
        S := Lst[i];
        while Length(S) >= 11 do
          begin
          try
            T := DecodeDD_mmm_YY(Copy(S, 1, 7));
            V := StrToInt(Copy(S, 9, 3));
            if V >= 0 then AddValue(V, T, vkEstimated);
          except end;
          Delete(S, 1, 12);
          S := Trim(S);
          end;
        end;
    end

{
Solar-terrestrial indices for 10 June follow.
Solar flux 152 and estimated mid-latitude A-Index 17.
}
  else if Pos('wwv.txt', Lst[0]) > 0 then
    try
      p := 0;
      if not SkipToStr('Solar-terrestrial indices for ', Lst, p) then Exit;
      T := DecodeDD_mmm(Copy(Lst[p], 31, 6));
      if not SkipToStr('Solar flux ', Lst, p) then Exit;
      i := Pos(' and ', Lst[p]);
      if i < 1 then Exit;
      V := StrToInt(Copy(Lst[p], 12, i-12));
      if V >= 0 then AddValue(V, T, vkMeasured);
    except end
end;


function TSfiArray.ScaleValue(V: Single): Single;
begin
  Result := V / 300;
end;




{ TXRayBgArray }

//X-ray Background Flux (GOES 8)
constructor TXRayBgArray.Create;
begin
  Title := 'D1 Daily X-Ray Flux';
  FStep := 1;                          //step = 1 day
  FLeftCnt := 365;                  //90 day history
  SetLength(FData, FLeftCnt + 1 + 45);  //45 day forecast
  SetLength(FKind, Length(Data));
  FStepX := 4;
  FStormThreshold := 1E-5;
end;


procedure TXRayBgArray.ImportText(Lst: TStrings);
var
  i: integer;
  V: Single;
begin
  if Pos('DSD.txt', Lst[0]) > 0 then
    for i:=1 to Lst.Count-1 do
      begin
      if IsComment(Lst[i]) or (Lst[i][1] <> '2') then Continue;
      try
        V := DecodeXRAY(Copy(Lst[i], 50, 4));
        if V > 0 then
          AddValue(V, DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10)), vkMeasured);
      except end;
      end;
end;

function TXRayBgArray.ScaleValue(V: Single): Single;
begin
//(A0..X0) = 1E-8..1E-3 W/m^2
 Result := (Log10(V) + 8) * 0.2;
end;



{ TProtonBgArray }

//Proton Fluence (GOES 8}
//Protons > 10 MeV
constructor TProtonBgArray.Create;
begin
  Title := 'D1 Daily Proton Flux';
  FStep := 1;                          //step = 1 day
  FLeftCnt := 365;                  //90 day history
  SetLength(FData, FLeftCnt + 1 + 45);  //45 day forecast
  SetLength(FKind, Length(Data));
  FStepX := 4;
  FStormThreshold := 10;
end;

procedure TProtonBgArray.ImportText(Lst: TStrings);
var
  i: integer;
  V: single;
begin
{
   Date       >1 MeV  >10 MeV  >100 MeV  >0.6 MeV   >2 MeV           % of bkgd
2002 05 26    7.4e+05  3.0e+04  2.4e+03   6.8e+09    2.1e+06            -0.10
}
  if Pos('DPD.txt', Lst[0]) > 0 then
    for i:=1 to Lst.Count-1 do
      begin
      if IsComment(Lst[i]) or (Lst[i][1] <> '2') then Continue;
      try
        V := SafeStrToFloat(Copy(Lst[i], 22, 9)) * SEC1{conversion};
        if V > 0 then
          AddValue(V, DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10)), vkMeasured);
      except end;
      end;
end;


function TProtonBgArray.ScaleValue(V: Single): Single;
begin
//in Protons/cm2-day-sr
//range 1E3..1E8 (?)
//  Result := (Log10(FData[Idx]) - 3) * 0.2;

//CONVERTING to Protons/cm2-s-sr
//range 1E-2..1E+4
  Result := (Log10(V) + 2) / 6;
end;





{ TApArray }

//Ap = [0..400] ?

constructor TApArray.Create;
begin
  Title := 'D1 Ap Index';
  FStep := 1;                          //step = 1 day
  FLeftCnt := 365;                  //90 day history
  SetLength(FData, FLeftCnt + 1 + 45);  //45 day forecast
  SetLength(FKind, Length(Data));
  FStepX := 4;
  FStormThreshold := 30;
end;

procedure TApArray.ImportText(Lst: TStrings);
var
  i, p: integer;
  T: TDateTime;
  V: Single;
  S: string;
begin
  if Pos('DGD.txt', Lst[0]) > 0 then
    for i:=1 to Lst.Count-1 do
      try
        if IsComment(Lst[i]) or (Lst[i][1] <> '2') then Continue;
        T := DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10));
        V := SafeStrToFloat(Copy(Lst[i], 13, 4));
        if V >= 0 then AddValue(V, T, vkMeasured);
      except
      end


  else if Pos('45DF.txt', Lst[0]) > 0 then
    begin
    p := 0; if SkipToStr('AP FORECAST', Lst, p) then Inc(p) else Exit;
    //forecast
    for i:=p to Lst.Count-1 do
      if Pos('FLUX FORECAST', Lst[i]) > 0 then Break else
        begin
        if Copy(Lst[i], 1, 1) = ':' then Continue;
        S := Lst[i];
        while Length(S) >= 11 do
          begin
          try
            T := DecodeDD_mmm_YY(Copy(S, 1, 7));
            V := StrToInt(Copy(S, 9, 3));
            if V >= 0 then AddValue(V, T, vkEstimated);
          except end;
          Delete(S, 1, 12);
          S := Trim(S);
          end;
        end;
    end

{
Solar-terrestrial indices for 10 June follow.
Solar flux 152 and estimated mid-latitude A-Index 17.
}
  else if Pos('wwv.txt', Lst[0]) > 0 then
    try
      p := 0;
      if not SkipToStr('Solar-terrestrial indices for ', Lst, p) then Exit;
      T := DecodeDD_mmm(Copy(Lst[p], 31, 6));
      if not SkipToStr('Solar flux ', Lst, p) then Exit;
      i := Pos(' A-INDEX ', UpperCase(Lst[p]));
      if i < 1 then Exit;
      S := Copy(Lst[p], i+9, 3);
      if S[Length(S)] = '.' then SetLength(S, Length(S)-1);
      V := StrToInt(S);
      if V >= 0 then AddValue(V, T, vkMeasured);
    except end
end;


function TApArray.ScaleValue(V: Single): Single;
begin
  Result := V / 60; //0..60 -> 0..1
end;





{ TKp3Array }

constructor TKp3Array.Create;
begin
  Title := 'H3 3-hour Kp Index';
  FStep := 1 / 8;                    //step = 3h
  FLeftCnt := 7*8 + 1;               //7 day history
  SetLength(FData, FLeftCnt + 3*8);  //3 day forecast
  SetLength(FKind, Length(Data));
  FStepX := 12;
  FStormThreshold := 5;
end;

procedure TKp3Array.ImportText(Lst: TStrings);
var
  i, p: integer;
  T, Ti: TDateTime;
  V: Single;
begin
  if Pos('pkp_15m.txt', Lst[0]) > 0 then
    for i:=1 to Lst.Count-1 do
      try
        {!}exit; //parser not verified

        
        if IsComment(Lst[i]) or (Lst[i][1] <> '2') then Continue;
        T := DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10)) + DecodeHHMM(Copy(Lst[i], 13, 4));
        T := T + 3/24{3h};   //data for 9:00..11:45 apply to 12:00
        V := SafeStrToFloat(Copy(Lst[i], 68, 5));
        if V >= 0 then AddValue(V, T, vkMeasured);
      except end


{
2002 06 02    -1 -1-1-1-1-1-1-1-1    -1 -1-1-1-1-1-1-1-1    -1  4 3 2 3 3 4 3-1
}
  else if Pos('DGD.txt', Lst[0]) > 0 then
    for i:=1 to Lst.Count-1 do
      try
        if IsComment(Lst[i]) or (Lst[i][1] <> '2') then Continue;
        T := DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10));
        for p:=0 to 7 do
          try
            V := StrToInt(Copy(Lst[i], 64+p*2, 2));
            if V >= 0 then AddValue(V, T + p*FStep, vkMeasured);
          except end;
      except
      end


{
OLD:
The mid-latitude K-index at 1200 UTC on 10 June was 4 (46 nT).
The estimated planetary K-index at 1800 UTC on 08 June was 1.
}
  else if Pos('wwv.txt', Lst[0]) > 0 then
    try
      p := 0;
      //if not SkipToStr('The mid-latitude K-index ', Lst, p) then Exit;
      //T := DecodeDD_mmm(Copy(Lst[p], 41, 6)) + DecodeHHMM(Copy(Lst[p], 29, 4));

      if not SkipToStr('The estimated planetary K-index at ', Lst, p) then Exit;
      T := DecodeDD_mmm(Copy(Lst[p], 48, 6)) + DecodeHHMM(Copy(Lst[p], 36, 4));

      i := Pos(' was ', Lst[p]);
      if i < 1 then Exit;
      V := StrToInt(Copy(Lst[p], i+5, 1));
      if V >= 0 then AddValue(V, T - FStep, vkMeasured);
    except end


{
MAGNETOMETER ANALYSIS FOR 10/1200
                 05-06  06-07  07-08  08-09  09-10  10-11  11-12
3-HOUR KP          3M     2M#    2M#    2M     0Z#    0P#    1Z
}

  else if Pos(' MAhr.txt', Lst[0]) > 0 then
    begin
    p := 0;
    //for each message in oldMAhr
    while SkipToStr('MAGNETOMETER ANALYSIS FOR', Lst, p) do
      try
        //2:00 and 3:00 must go into the same 3-h bin
        T := DecodeDD_HHHH(Copy(Lst[p], 27, 7)) - MIN1;
        if not SkipToStr('3-HOUR KP         ', Lst, p) then Continue;
        //for each value in the message
        for i:=0 to 6 do
          try
            //value
            V := StrToInt(Copy(Lst[p], 19+i*7, 2));
            if V < 0  then Continue;
            case Lst[p][21+i*7] of
              'P': V := V + 1/3;
              'M': V := V - 1/3;
              end;
          //{!}V := Trunc(V);

          //1-h and 2-h values go into the next 3-h bin
          AddValue(Max(1, V), T + HOUR1 * (i-6), vkEstimated);
          except end;
      except end;
    end


{
Mid/00-03UT               2             2             2
}
  else if Pos('daypre.txt', Lst[0]) > 0 then
    begin
    //date
    p := 0; if not SkipToStr(':Prediction_dates:', Lst, p) then Exit;
    T := DecodeYYYY_mmm_DD(Copy(Lst[p], 22, 11));
    //data
    if SkipToStr('Mid/', Lst, p) then
      for i:=0 to 7 do
        try
          if Pos('Mid/', Lst[p+i]) <> 1 then Break;
          Ti := T + HOUR1 * StrToInt(Copy(Lst[i+p], 5, 2));

          V := StrToInt(Copy(Lst[p+i], 26, 2));
          if V >= 0 then AddValue(V, Ti, vkEstimated);

          V := StrToInt(Copy(Lst[p+i], 40, 2));
          if V >= 0 then AddValue(V, Ti+1, vkEstimated);

          V := StrToInt(Copy(Lst[p+i], 54, 2));
          if V >= 0 then AddValue(V, Ti+2, vkEstimated);
        except
        end
    end;
end;


function TKp3Array.ScaleValue(V: Single): Single;
begin
  Result := V / 9;
end;




{ TKp1Array }

constructor TKp1Array.Create;
begin
  Title := 'H1 1-hour Kp Index';
  FStep := 1 / 24;                //step = 1h
  FLeftCnt := 48 + 1;             //48 hour history
  SetLength(FData, FLeftCnt);     //no forecast
  SetLength(FKind, Length(Data));
  FStepX := 4;
  FStormThreshold := -1;
end;


procedure TKp1Array.ImportText(Lst: TStrings);
var
  p, i: integer;
  T: TDateTime;
  V: Single;
begin
{
MAGNETOMETER ANALYSIS FOR 10/1200
                 05-06  06-07  07-08  08-09  09-10  10-11  11-12
1-HOUR KP          1M     2M     2M     2M     2M     3M     3P
}

  if (Pos(' MAhr.txt', Lst[0]) > 0) or (Pos(' oldMAhr.txt', Lst[0]) > 0) then
    begin
    p := 0;
    //for each message in oldMAhr
    while SkipToStr('MAGNETOMETER ANALYSIS FOR', Lst, p) do
      try
        T := DecodeDD_HHHH(Copy(Lst[p], 27, 7)) - FStep{10/1200 -> 11:00..11:59};
        if not SkipToStr('1-HOUR KP', Lst, p) then Continue;
        //for each 1-h value in the message
        for i:=0 to 6 do
          try
            V := StrToIntDef(Copy(Lst[p], 62-i*7, 1), -1);
            if V < 0  then Continue;
            case Lst[p][63-i*7] of
              'P': V := V + 1/3;
              'M': V := V - 1/3;
              end;
          AddValue(V, T - FStep * i, vkMeasured);
          except end;
      except end;
    end;
end;


function TKp1Array.ScaleValue(V: Single): Single;
begin
  Result := V / 9;
end;



(*
{ TCostelloArray }

constructor TCostelloArray.Create;
begin
  Title := 'M15 Costello Kp forecast';
  FStep := 1 / 24 / 4;              //step = 15 min
  FLeftCnt := 12*4 + 1;             //12 hours history
  SetLength(FData, FLeftCnt + 2*4); //2 hours forecast
  SetLength(FKind, Length(Data));
  FStepX := 4;
  FStormThreshold := -1;
end;


procedure TCostelloArray.ImportText(Lst: TStrings);
var
  i: integer;
  T: TDateTime;
  V: Single;
begin
{
# UT Date   Time         -Predicted Time-  Predicted  Lead-time  USAF Est.
# YR MO DA  HHMM   S     YR MO DA  HHMMSS    Index    in Minutes     Kp   
2002 06 10  1145   0   2002 06 10  124148     2.33      56.8        4.00
}
  if Pos('pkp_15m.txt', Lst[0]) > 0 then
    for i:=1 to Lst.Count-1 do
      try
        if IsComment(Lst[i]) or (Lst[i][1] <> '2') then Continue;
        T := DecodeYYYY_MM_DD(Copy(Lst[i], 24, 10)) + DecodeHHMM(Copy(Lst[i], 36, 4));
        //round to nearest 15-min 
        T := Round(T * 24*4) / (24*4);
        V := SafeStrToFloat(Copy(Lst[i], 46, 5));
        if V > 0 then AddValue(V, T, vkMeasured);
      except end
end;


function TCostelloArray.ScaledData(Idx: integer): Single;
begin
  Result := FData[Idx] / 9;
end;

*)




{ TIntApArray }

constructor TIntApArray.Create;
begin
  Title := 'H1 Integral of ap';
  FStep := 1 / 24;              //step = 1h
//  FLeftCnt := 7*24 + 1;         //7 day history
  FLeftCnt := 365*24 + 1;         //1year history
  SetLength(FData, FLeftCnt);   //0 day forecast
  SetLength(FKind, Length(Data));
  FStepX := 4;
  FStormThreshold := -1;
end;


procedure TIntApArray.ImportText(Lst: TStrings);
var
  i, p: integer;
  T, Ti: TDateTime;
  V: Single;
begin
{
:Data_list: RTST-Aug27-2002.txt
:Created: 2002 Aug 27 2345 UTC

# UTC  Hap int.ap  +70  +50  +30  +10  -10  -30  -50  -70
   0   16  236.1  0.99 1.00 1.00 1.01 1.01 1.01 1.02 1.02
}

  if Pos('list: RTST-', Lst[0]) > 0 then
    begin
    //date
    p := 0; if not SkipToStr(':Created:', Lst, p) then Exit;
    T := DecodeYYYY_mmm_DD(Copy(Lst[p], 11, 11));
    //data
    for i:=p+1 to Lst.Count-1 do
      try
        if IsComment(Lst[i]) then Continue;
        Ti := T + 1/24 * (StrToInt(Copy(Lst[i], 3, 2)));
        V := SafeStrToFloat(Copy(Lst[i], 11, 6));
        if V >= 0 then AddValue(V, Ti, vkMeasured);
      except
      end
    end

{
STORM Time Empirical Ionospheric Model
Date = Aug30-2001
UT  Hap int.ap  +70  +50  +30  +10  -10  -30  -50  -70
 0   9  210.7  1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.01
}
  else if Pos('STORM Time', Lst[0]) > 0 then
    begin
    //date
    p := 0; if not SkipToStr('Date =', Lst, p) then Exit;
    T := DecodeMmmDD_YYYY(Copy(Lst[p], 8, 10));
    //data
    for i:=p+2 to Lst.Count-1 do
      try
        Ti := T + 1/24 * (StrToInt(Copy(Lst[i], 1, 2)));
        V := SafeStrToFloat(Copy(Lst[i], 8, 6));
        if V >= 0 then AddValue(V, Ti, vkMeasured);
      except
      end
    end;
end;


function TIntApArray.ScaleValue(V: Single): Single;
begin
  //0..5000
  Result := V / 1000;
end;




{ TAuroraArray }

//Auroral Activity index [1..10]
//also Copy(S, 37, 5) = power input by auroral particles, [2.5..96+] GWatts

constructor TAuroraArray.Create;
begin
  Title := 'M15 Auroral activity';
  FStep := 1 / 24 / 4;              //step = 15 min
  FLeftCnt := 7*24*4 + 1;           //7 day history
  SetLength(FData, FLeftCnt);       //0 day forecast
  SetLength(FKind, Length(Data));
  FStepX := 1;
  FStormThreshold := -1;
end;


procedure TAuroraArray.ImportText(Lst: TStrings);
var
  i, p: integer;
  T: TDateTime;
  V: Single;
  S: string;
begin
{
2015-01-31 00:05      14      15
}
  p := MAXINT;
  for i:=0 to Lst.Count-1 do
    if Copy(Lst[i], 1, 1) <> '#' then
      begin p := i; Break; end;

  if (Lst.Count = 0) or (Length(Lst[p]) < 32) then Exit;
  S := Lst[p];
  if (S[1]+S[2]+S[5]+S[8]+S[11]+S[14]) <> '20-- :' then Exit;

  for i:=p to Lst.Count-1 do
    try
      T := DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10)) + DecodeHH_MM(Copy(Lst[i], 12, 5));
      V := 0.5 * (StrToInt(Trim(Copy(Lst[i], 23, 2))) + StrToInt(Trim(Copy(Lst[i], 31, 2))));
      V := 2.2 * Ln(V) - 0.66;
      if V >= 1 then AddValue(V, T, vkMeasured);
    except end;




(*
{
2007-01-01 00:34:40 NOAA-16 (N)  6  15.87   0.93
}
  p:= 0;
  if not SkipToStr('NOAA-', Lst, p) then Exit;

  for i:=p to Lst.Count-1 do
    try
      if Copy(Lst[i], 21, 5) <> 'NOAA-' then Continue;
      T := DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10)) + DecodeHH_MM(Copy(Lst[i], 12, 5));
      T := Round(T / FStep) * FStep;
      V := StrToInt(Copy(Lst[i], 33, 2));
      if V >= 1 then AddValue(V, T, vkMeasured);
    except end;
*)

(*
  p:= 0;
  if not SkipToStr('NOAA-', Lst, p) then Exit;

{
1-day:
NOAA-14 2002 Jun  4 0014 (N)  6      16.5   0.854
}
  if Lst[p][9] = '2' then
    for i:=0 to Lst.Count-1 do
      try
        if Copy(Lst[i], 1, 5) <> 'NOAA-' then Continue;
        T := DecodeYYYY_mmm_DD(Copy(Lst[i], 9, 11)) + DecodeHHMM(Copy(Lst[i], 21, 4));
        T := Round(T / FStep) * FStep;
        V := StrToInt(Copy(Lst[i], 30, 2));
        if V >= 1 then AddValue(V, T, vkMeasured);
      except end

{
year, possibly partial:
NOAA-16(S)  10011    11.1  5   3.320
}
  else if Lst[p][9] in ['N', 'S'] then
    begin
    //get current year
    DecodeDate(UtcNow, y, m, d);
    T := EncodeDate(y, 1, 1);
    //loop
    for i:=p to Lst.Count-1 do
      try
        if Copy(Lst[i], 1, 5) <> 'NOAA-' then Continue;
        Ti := T + (StrToInt(Copy(Lst[i], 11, 3)) - 1) + DecodeHHMM(Copy(Lst[i], 14, 4));
        Ti := Round(Ti / FStep) * FStep;
        V := StrToInt(Copy(Lst[i], 27, 2));
        if V >= 1 then AddValue(V, Ti, vkMeasured);
      except end;
    end
*)

(*
  //day
  if Pos('NOAA-', Lst[0]) = 1 then
    for i:=0 to Lst.Count-1 do
      try
        if Copy(Lst[i], 1, 5) <> 'NOAA-' then Continue;
        T := DecodeYYYY_mmm_DD(Copy(Lst[i], 9, 11)) + DecodeHHMM(Copy(Lst[i], 21, 4));
        T := Round(T / FStep) * FStep;
        V := StrToInt(Copy(Lst[i], 30, 2));
        if V >= 1 then
          AddValue(V, T, vkMeasured);
{
          case Lst[i][27] of
            'N': AddValue(V, T, vkMeasured);
            'S': AddValue(V, T, vkEstimated);
            end;
}
      except end


{
NOAA-16(S)  10011    11.1  5   3.320
}
  //history
  else if Pos(':Data_list: power_', Lst[0]) = 1 then
    begin
    //get year
    T := 0; p := 0;
    for i:=1 to Lst.Count-1 do
      if Copy(Lst[i], 1, 2) = '20' then
        try
          p := i + 1;
          T := EncodeDate(StrToInt(Copy(Lst[i], 1, 4)), 1, 1);
        except Exit; end;
    if T = 0 then Exit;

    for i:=p to Lst.Count-1 do
      try
        if Copy(Lst[i], 1, 5) <> 'NOAA-' then Continue;
        Ti := T + (StrToInt(Copy(Lst[i], 11, 3)) - 1) + DecodeHHMM(Copy(Lst[i], 14, 4));
        Ti := Round(Ti / FStep) * FStep;
        V := StrToInt(Copy(Lst[i], 27, 2));
        if V >= 1 then AddValue(V, Ti, vkMeasured);
      except end;
    end;
*)
end;


function TAuroraArray.ScaleValue(V: Single): Single;
begin
  Result := V / 10;
end;



{ TProtonArray }

//1. ACE hourly data '_ace_sis_1h.txt'
//Protons > 10 MeV
//in p/cm2-sec-ster

//2. GOES8 5 min. data 'G8part_5m.txt'
//Protons > 10 MeV
//in Protons/cm2-s-sr


constructor TProtonArray.Create;
begin
  Title := 'M15 Proton Flux';
  FStep := 1 / 24 / 4;              //step = 15 min
  FLeftCnt := 7*24*4 + 1;           //7 day history
  SetLength(FData, FLeftCnt);       //0 day forecast
  SetLength(FKind, Length(Data));
  FStepX := 1;
  FStormThreshold := 10;
end;


procedure TProtonArray.ImportText(Lst: TStrings);
var
  i: integer;
  T: TDateTime;
  V, Vsum: Single;
  N: integer;
//  Hr: string;
begin
  //ACE hourly data
  if Pos('_ace_sis_1h.txt', Lst[0]) > 0 then
    for i:=1 to Lst.Count-1 do
      try
        if IsComment(Lst[i]) or (Lst[i][1] <> '2') then Continue;
          T := DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10))
             + DecodeHHMM(Copy(Lst[i], 13, 4));
          V := SafeStrToFloat(Copy(Lst[i], 43, 9));
          if V >= 0 then AddValue(V, T, vkMeasured);
      except
      end


{
# YR MO DA  HHMM    Day     Day     P > 1     P > 5     P >10     P >30     P >50     P>100     E>0.6     E>2.0     E>4.0
2002 06 05  1145   52430  42300   1.41e+00  1.03e-01  8.24e-02  5.58e-02  3.21e-02  1.41e-02  1.83e+05  1.84e+02 -1.00e+05
}

  else if Pos('part_5m.txt', Lst[0]) > 0 then
    for i:=1 to Lst.Count-1 do
      begin
      if IsComment(Lst[i]) or (Lst[i][1] <> '2') then Continue;
      if not (StrToInt(Copy(Lst[i], 15, 2)) in [5, 20, 35, 50]) then Continue;
      try
        T := DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10))
           + DecodeHHMM(Copy(Lst[i], 13, 4)) + 1/24/60*10{10 min};

        //1-st value
        VSum := 0; N := 0;
        V := SafeStrToFloat(Copy(Lst[i], 54, 9));
        if V >= 0 then begin VSum := VSum + V; Inc(N); end;
        //2-nd value
        if i < (Lst.Count-1) then
          begin
          V := SafeStrToFloat(Copy(Lst[i], 54, 9));
          if V >= 0 then begin VSum := VSum + V; Inc(N); end;
          end;
        //3-rd value
        if i < (Lst.Count-2) then
          begin
          V := SafeStrToFloat(Copy(Lst[i], 54, 9));
          if V >= 0 then begin VSum := VSum + V; Inc(N); end;
          end;
        //store
        case N of
          1, 2: AddValue(Vsum/N, T, vkEstimated);
          3:    AddValue(Vsum/3, T, vkMeasured);
          end;
      except end;
      end;
end;


function TProtonArray.ScaleValue(V: Single): Single;
begin
  //[1E-2..1E4]
  Result := (Log10(V) + 2) / 6;
end;




{ TXRayArray }

constructor TXRayArray.Create;
begin
  Title := 'M15 X-Ray Flux';
  FStep := 1 / 24 / 4;              //step = 15 min
  FLeftCnt := 7*24*4 + 1;           //7 day history
  SetLength(FData, FLeftCnt);       //0 day forecast
  SetLength(FKind, Length(Data));
  FStepX := 1;
  FStormThreshold := 1E-5;
end;


procedure TXRayArray.ImportText(Lst: TStrings);
var
  i: integer;
  T: TDateTime;
  V, Vsum: Single;
  N: integer;
begin
  if Pos('xr_1m.txt', Lst[0]) > 0 then
    for i:=1 to Lst.Count-1 do
      begin
      if IsComment(Lst[i]) or (Lst[i][1] <> '2') then Continue;
      if not (StrToInt(Copy(Lst[i], 15, 2)) in [5, 20, 35, 50]) then Continue;
      try
        T := DecodeYYYY_MM_DD(Copy(Lst[i], 1, 10))
           + DecodeHHMM(Copy(Lst[i], 13, 4)) + 1/24/60*10{10 min};

        //1-st value
        VSum := 0; N := 0;
        V := SafeStrToFloat(Copy(Lst[i], 45, 12));
        if V >= 0 then begin VSum := VSum + V; Inc(N); end;
        //2-nd value
        if i < (Lst.Count-1) then
          begin
          V := SafeStrToFloat(Copy(Lst[i+1], 45, 12));
          if V >= 0 then begin VSum := VSum + V; Inc(N); end;
          end;
        //3-rd value
        if i < (Lst.Count-2) then
          begin
          V := SafeStrToFloat(Copy(Lst[i+2], 45, 12));
          if V >= 0 then begin VSum := VSum + V; Inc(N); end;
          end;
        //store
        case N of
          1, 2: AddValue(Vsum/N, T, vkEstimated);
          3:    AddValue(Vsum/3, T, vkMeasured);
          end;
      except end;
      end;
end;


function TXRayArray.ScaleValue(V: Single): Single;
begin
  //(A0..X0) = E-8..E-3  W/m^2
  Result := (Log10(V) + 8) * 0.2;
end;



//------------------------------------------------------------------------------
//                              Geo Alerts
//------------------------------------------------------------------------------


{ TGeoAlerts }

function TGeoAlerts.GetValue(Index: TAlertKind): integer;
begin
  Result := FValues[Index];
end;


procedure TGeoAlerts.LoadFromFile(FilePath: TFileName);
var
  i: TAlertKind;
begin
  if not FileExists(FilePath) then Exit;

  try
  
    with TStringList.Create do
      try
        LoadFromFile(FilePath);
        Issued := IsoStrToDateTime(Strings[0]);
        Delete(0);
        for i:=Low(TAlertKind) to High(TAlertKind) do
          FValues[i] := StrToIntDef(Values[IntToStr(Integer(i))], -1);
      finally
        Free;
      end;

  except end;
end;


procedure TGeoAlerts.SaveToFile(FilePath: TFileName);
var
  i: TAlertKind;
begin
  try

    with TStringList.Create do
      try
        Add(IsoDateTimeToStr(Issued));
        for i:=Low(TAlertKind) to High(TAlertKind) do
          Add(IntToStr(Integer(i)) + '=' + IntToStr(FValues[i]));
        SaveToFile(FilePath);
      finally
        Free;
      end;

  except end;
end;


procedure TGeoAlerts.Import(Lst: TStringList);
var
  a: TAlertKind;
  p: integer;
begin
{
:Issued: 2002 Jun 24 1805 UTC

Space weather for the past 24 hours has been moderate.
Geomagnetic storms reaching the G2 level occurred.
Solar radiation storms reaching the S2 level occurred.
Radio blackouts reaching the R1 level occurred.

Space weather for the next 24 hours is expected to be minor.
Geomagnetic storms reaching the G1 level are expected.
}
  if Pos('wwv.txt', Lst[0]) > 0 then
    try
      p := 0;
      if not SkipToStr(':Issued:', Lst, p) then Exit;
      Issued := DecodeYYYY_mmm_DD(Copy(Lst[p], 10, 11))
        + DecodeHHMM(Copy(Lst[p], 22, 4));
      //clear
      for a:=Low(TAlertKind) to High(TAlertKind) do FValues[a] := 0;

      //past

      if not SkipToStr('past 24 hours', Lst, p) then Exit;

      while (p < (Lst.Count-1)) and (Pos('next 24 hours', Lst[p]) < 1) do
        begin
        Inc(p);
        if Pos('Geomagnetic storms reaching the G', Lst[p]) = 1
          then FValues[alPastG] := StrToIntDef(Copy(Lst[p], 34, 1), -1)
        else if Pos('Solar radiation storms reaching the S', Lst[p]) = 1
          then FValues[alPastS] := StrToIntDef(Copy(Lst[p], 38, 1), -1)
        else if Pos('Radio blackouts reaching the R', Lst[p]) = 1
          then FValues[alPastR] := StrToIntDef(Copy(Lst[p], 31, 1), -1);
        end;

      //next

      if not SkipToStr('next 24 hours', Lst, p) then Exit;

      while p < (Lst.Count-1) do
        begin
        Inc(p);
        if Pos('Geomagnetic storms reaching the G', Lst[p]) = 1
          then FValues[alNextG] := StrToIntDef(Copy(Lst[p], 34, 1), -1)
        else if Pos('Solar radiation storms reaching the S', Lst[p]) = 1
          then FValues[alNextS] := StrToIntDef(Copy(Lst[p], 38, 1), -1)
        else if Pos('Radio blackouts reaching the R', Lst[p]) = 1
          then FValues[alNextR] := StrToIntDef(Copy(Lst[p], 31, 1), -1);
        end;
    except end;
end;





{ TAp1Array }


constructor TAp1Array.Create;
begin
  Title := 'Hourly ap';
  FStep := 1 / 24;              //step = 1h
  FLeftCnt := 365*24 + 1;         //1year history
  SetLength(FData, FLeftCnt);   //0 day forecast
  SetLength(FKind, Length(Data));
  FStepX := 4;
  FStormThreshold := -1;
end;


function TAp1Array.ScaleValue(V: Single): Single;
begin
  //0..400
  Result := V / 400;
end;


procedure TAp1Array.ImportText(Lst: TStrings);
var
  i, p: integer;
  T, Ti: TDateTime;
  V: Single;
begin
{
:Data_list: RTST-Aug27-2002.txt
:Created: 2002 Aug 27 2345 UTC

# UTC  Hap int.ap  +70  +50  +30  -30  -50  -70
  10   18  199.9  1.00 1.00 1.00 1.00 1.00 1.00
}

  if Pos('list: RTST-', Lst[0]) > 0 then
    begin
    //date
    p := 0; if not SkipToStr(':Created:', Lst, p) then Exit;
    T := DecodeYYYY_mmm_DD(Copy(Lst[p], 11, 11));
    //data
    for i:=p+1 to Lst.Count-1 do
      try
        if IsComment(Lst[i]) then Continue;
        Ti := T + 1/24 * (StrToInt(Copy(Lst[i], 3, 2)));
        V := SafeStrToFloat(Copy(Lst[i], 7, 3));
        if V >= 0 then AddValue(V, Ti, vkMeasured);
      except
      end
    end
{
//old format

STORM Time Empirical Ionospheric Model
Date = Aug30-2001
UT  Hap int.ap  +70  +50  +30  +10  -10  -30  -50  -70
 0   9  210.7  1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.01
}

  else if Pos('STORM Time', Lst[0]) > 0 then
    begin
    //date
    p := 0; if not SkipToStr('Date =', Lst, p) then Exit;
    T := DecodeMmmDD_YYYY(Copy(Lst[p], 8, 10));
    //data
    for i:=p+2 to Lst.Count-1 do
      try
        Ti := T + 1/24 * (StrToInt(Copy(Lst[i], 1, 2)));
        V := SafeStrToFloat(Copy(Lst[i], 4, 3));
        if V >= 0 then AddValue(V, Ti, vkMeasured);
      except
      end
    end;
end;



end.

