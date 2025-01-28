unit SsneArr;

interface

uses
  SysUtils, Classes, GeomFun, Utc, Math, SolArray, TgzInMem_D5, SpherHrm, MyMath,
  MagFld;


const
  HISTORY_SIZE = 49;
  SSNE_WIN = 6;

type
  PIonoStation = ^TIonoStation;
  TIonoStation = record
    Location: TGeoPoint;
    MagDip: Single;
    foF2, F0, F1: array[0..HISTORY_SIZE-1] of Single;
    end;

  TSsneArray = class (TSolarArray)
  private
    FTgz: TTgzReader;
    FStations: array of TIonoStation;
    FUrsi: TFoCalculator;
    T0: TDateTime;
    FGeoField: TGeoField;
    procedure ImportStation(Lst: TStringList);
    procedure ProcessStations;
    function SetLatLon(Stn: PIonoStation; Str: string): boolean;
    function CalcError(Hour: integer; ASsne: Single): Single;
  protected
    function LoadLine(Line: string; Idx: integer): boolean; override;
    function SaveLine(Idx: integer): string; override;
  public
    FErr: TSingleArray;
    FCnt: array of integer;
    FScale: integer;

    constructor Create;
    destructor Destroy; override;
    procedure ImportText(Lst: TStrings); override;
    procedure Update; override;
    procedure ImportTgz(Data: string);
    function ScaleValue(V: Single): Single; override;
    function GetMax: Single;
  end;



implementation


{ TSsneArray }

constructor TSsneArray.Create;
begin
  Title := 'H1 Effective Sunspot Number';
  FStep := 1 / 24;                   //step = 1h
  FLeftCnt := 24*7 + 1;              //1-week history
  SetLength(FData, FLeftCnt);        //no forecast
  SetLength(FKind, Length(Data));
  SetLength(FErr, Length(Data));
  SetLength(FCnt, Length(Data));

  FStepX := 4;
  FStormThreshold := -1;
  FScale := 400;

  FTgz := TTgzReader.Create;
  FUrsi := TFoCalculator.Create;
  FGeoField := TGeoField.Create;
end;

destructor TSsneArray.Destroy;
begin
  FGeoField.Free;
  FUrsi.Free;
  FTgz.Free;
  inherited;
end;


procedure TSsneArray.ImportText(Lst: TStrings);
begin
  //this class does not parse text, it expects a tar.gz file
end;


function TSsneArray.ScaleValue(V: Single): Single;
begin
  Result := V / FScale;
end;


procedure TSsneArray.Update;
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
      FErr[i]  := FErr[i+Shift];
      FCnt[i]  := FCnt[i+Shift];
      end;
  //clear space
  for i:=Max(0, Length(FData) - Shift) to High(Data) do
    FKind[i] := vkNone;
end;



function TSsneArray.LoadLine(Line: string; Idx: integer): boolean;
var
  p: integer;
  Se, Sc: string;
begin
  Se := ''; Sc := '';
  //chop foF2 error
  p := Pos('|', Line);
  if p > 0 then
    begin Se := Copy(Line, p+1, MAXINT); Delete(Line, p, MAXINT); end;

  //chop observation count
  p := Pos('|', Se);
  if p > 0 then
    begin Sc := Copy(Se, p+1, MAXINT); Delete(Se, p, MAXINT); end;

  FErr[Idx] := SafeStrToFloat(Se);
  FCnt[Idx] := StrToIntDef(Sc, 0);

  //the rest is SSNe
  Result := inherited LoadLine(Line, Idx);
end;


function TSsneArray.SaveLine(Idx: integer): string;
begin
  Result := inherited SaveLine(Idx)
    + '|' + SafeFloatToStr(FErr[Idx])
    + '|' + IntToStr(FCnt[Idx]);
end;



procedure TSsneArray.ImportTgz(Data: string);
var
  i: integer;
  Lst: TStringList;
begin
  try
    try
      T0 := Trunc(UtcNow) - 1;
      FStations := nil;

      //kludge; some browsers return un-gzipped file
      if Copy(Data, 1, 2) = #$1F#$8B
        then FTgz.GzipString := Data
        else FTgz.TarString := Data;


      Lst := TStringList.Create;
      try
        for i:=0 to FTgz.FileCount-1 do
          begin
          Lst.Text := FTgz.Files[i].Data;
          ImportStation(Lst);
          end;
      finally Lst.Free; end;
      ProcessStations;

    finally FTgz.Clear; end;
  except end;
end;



function TSsneArray.SetLatLon(Stn: PIonoStation; Str: string): boolean;
var
  i, lt: integer;
begin
  Result := false;

  lt := 0;
  for i:=1 to Length(Str) do if Str[i] in ['0'..'9'] then
    begin lt := i-1; Break; end;
  if lt = 0 then Exit;

  Str := Copy(Str, lt, 7);
  for i:=Length(Str) downto 1 do if Str[i] in ['0'..'9'] then
    begin SetLength(Str, i); Break; end;

//  hh.add(str);

{
  //get latlon text
  lt := Pos('(', Str);
  rt := Pos(')', Str);
  if (lt < 1) or (rt < lt) then Exit;
  Str := Trim(Copy(Str, lt+1, rt-lt-1));
  if Length(Str) < 6 then Exit;
}

  //lat value
  case Str[1] of
    'N': Stn.Location.Lat := 1;
    'S': Stn.Location.Lat := -1;
    else Exit;
    end;
  try
    Stn.Location.Lat := RinD * Stn.Location.Lat * StrToInt(Copy(Str, 2, 2));
  except Exit; end;

  //lon value
  case Str[4] of
    'E': Stn.Location.Lon := 1;
    'W': Stn.Location.Lon := -1;
    else Exit;
    end;
  try
    Stn.Location.Lon := RinD * Stn.Location.Lon * StrToInt(Copy(Str, 5, MAXINT));
  except Exit; end;

  Result := true;
end;


procedure TSsneArray.ImportStation(Lst: TStringList);
var
  Stn: PIonoStation;
  p: integer;
  T: TDateTime;
  Idx: integer;
  V: Single;
begin
  if Lst.Count = 0 then Exit;
  //add new station
  SetLength(FStations, Length(FStations) + 1);
  Stn := @FStations[High(FStations)];

  //lat/lon
  p := 1;
  while (p < Lst.Count) and (Pos('Real-Time Ionosonde Data', Lst[p]) < 1) do Inc(p);
  Inc(p);
  if p >= Lst.Count then Exit;
  if not SetLatLon(Stn, Lst[p]) then Exit;

{
# YR MO DA  HHMM    foF2 hmF2 M(D)   D  h'F yF2 fMUF  h'  fxI foF1  foE hmE foEs fbEs  ITEC
2002 08 01  0045    -1.0  -1 -1.00 3000  -1  -1 -1.0  -1 -1.0 -1.0 -1.0  -1 -1.0 -1.0  -1.0
2002 05 20  1300    11.1 275  2.95 3000 195  88  9.8 355 11.5  5.4  4.0 100  4.0 -1.0  36.5
}
  //data
  for p:=p+1 to Lst.Count-1 do
    begin
    if IsComment(Lst[p]) or (Lst[p][1] <> '2') then Continue;
    if Copy(Lst[p], 15, 2) <> '00' then Continue; //whole hours only
    try
      //observarion time
      T := DecodeYYYY_MM_DD(Copy(Lst[p], 1, 10))
         + DecodeHHMM(Copy(Lst[p], 13, 4));
      //index into the storage array
      Idx := Round((T - T0) * 24);
      if (Idx < 0) or (Idx > High(Stn.foF2)) then Continue;

      //observed foF2
      V := SafeStrToFloat(Copy(Lst[p], 20, 5));
      if V < 60 then Stn.foF2[Idx] := V;
    except end;
    end;

end;


procedure TSsneArray.ProcessStations;
var
  St, Hr, Age: integer;
  Wt, Num, Den: Single;
  Cnt: integer;
  Idx: integer;
//  NewKind: TValueKind;
begin

  //prepare data for SSNe calculations

  //magnetic coords
  for St:=0 to High(FStations) do
    begin
    FGeoField.Location := FStations[St].Location;
    FStations[St].MagDip := FGeoField.ModifMagDip;
    end;

  //foF2 for SSN=0
  FUrsi.Ssn := 0;
  for Hr:=0 to HISTORY_SIZE-1 do
    begin
    FUrsi.Utc := T0 + Hr/24;
    for St:=0 to High(FStations) do
      with FStations[St] do
        if foF2[Hr] >= 2 then F0[Hr] := FUrsi.Calculate(Location, MagDip);
    end;

  //foF2 for SSN=100
  FUrsi.Ssn := 100;
  for Hr:=0 to HISTORY_SIZE-1 do
    begin
    FUrsi.Utc := T0 + Hr/24;
    for St:=0 to High(FStations) do
      with FStations[St] do
        if foF2[Hr] >= 2 then F1[Hr] := FUrsi.Calculate(Location, MagDip);
    end;



  //calculate SSNe

  //for all times that have at least SSNE_WIN hours of data
  for Hr:=SSNE_WIN-1 to HISTORY_SIZE-1 do
    begin
    //calculate SSNe
    Cnt := 0; Num := 0; Den := 0;
    for St:=0 to High(FStations) do
      for Age:=0 to SSNE_WIN-1 do
        with FStations[St] do
          if foF2[Hr-Age] >= 2 then
            begin
            Wt := F1[Hr-Age] - F0[Hr-Age];
            Num := Num + Wt * (foF2[Hr-Age] - F0[Hr-Age]);
            Den := Den + Wt * Wt;
            Inc(Cnt);
            end;

    //store results
    if Cnt <> 0 then
      begin
      Idx := TimeToIdx(T0 + Hr/24);
      if (Idx < 0) or (Idx > High(FData)) then Continue;
      FData[Idx] := 100 * Num/Den;
      FKind[Idx] := vkMeasured;
      FErr[Idx] := CalcError(Hr, FData[Idx]);
      FCnt[Idx] := Cnt;
      end;
    end;

  FStations := nil;
end;


function TSsneArray.CalcError(Hour: integer; ASsne: Single): Single;
var
  St, Age, t: integer;
  Estim: Single;
  Cnt: integer;
begin
  Result := 0; Cnt := 0;
  for St:=0 to High(FStations) do
    for Age:=0 to SSNE_WIN-1 do
      with FStations[St] do
        begin
        t := Hour - Age;
        if foF2[t] < 2 then Continue;
        Estim := F0[t] + (F1[t] - F0[t]) * (ASsne / 100);
        Result := Result + Sqr(Estim / foF2[t] - 1);
        Inc(Cnt);
        end;

  Result := 100 * Sqrt(Result / Cnt);
end;

{
initialization
  hh:=tstringlist.create;
finalization
  hh.savetofile('d:\a.txt');
  hh.free;
}


function TSsneArray.GetMax: Single;
var
  i: integer;
begin
  Result := 0;
  for i:=0 to High(FData) do
    if FKind[i] = vkMeasured then Result := Max(Result, FData[i]);
end;



end.

