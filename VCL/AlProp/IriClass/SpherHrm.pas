unit SpherHrm;

interface

uses
  Windows, SysUtils, Classes, Math, MyMath, Interpol, IriData;

const
  FO_ROWS = 76;
  FO_COLS = 6;

  XM_ROWS = 49;
  XM_COLS = 4;

  FO_HARM_COUNTS: array[0..8] of integer = (11,11,8,4,1,0,0,0,0);
  XM_HARM_COUNTS: array[0..6] of integer = (6,7,5,2,1,0,0);


type
  TComplex = record Re, Im: Single; end;

  TCoeffEntry = record
    K0: Single;
    K: array of TComplex;
    end;

  TCoeffArray = array of TCoeffEntry;

  TCoeffArrays = array[1..12, 0..1] of TCoeffArray;

  TFoCalculator = class 
  private
    FUtc: TDateTime;
    TimeChanged, SsnOrDateChanged: boolean;
    FSsn: Single;

    //arrays are dynamic because their size is different for foF2 and M3000
    FCoeffArrays: TCoeffArrays; //all coefficients
    Coeff: TCoeffArray;         //interpolated coefficients
    SnCs: array of TComplex;
    TimeHarm: array of Single;
    HarmCounts: array of Integer;

    procedure LoadCoefficients(ResourceName: string);
    procedure SetUtc(const Value: TDateTime);
    procedure SetSsn(const Value: Single);
    procedure CalcTimeSinCos;
    procedure InterpolateCoeff;
    procedure CalcTimeHarmonics;
    function DoCalculate(P: TGeoPoint; ModDip: Single): Single;
    procedure InitCoeffArrays(ARows, ACols: integer);
  public
    constructor Create;
    function Calculate(P: TGeoPoint; ModDip: Single): Single;
    property Utc: TDateTime read FUtc write SetUtc;
    property Ssn: Single read FSsn write SetSsn;
  end;


  TXmCalculator = class (TFoCalculator)
  public
    constructor Create;
  end;





implementation


{ TFoCalculator }

//------------------------------------------------------------------------------
//                                 init
//------------------------------------------------------------------------------

constructor TFoCalculator.Create;
begin
  SetLength(SnCs, FO_COLS);
  SetLength(TimeHarm, FO_ROWS);

  SetLength(HarmCounts, Length(FO_HARM_COUNTS));
  Move(FO_HARM_COUNTS[0], HarmCounts[0], SizeOf(FO_HARM_COUNTS));


  InitCoeffArrays(FO_ROWS, FO_COLS);

  try LoadCoefficients('URSI_COEFF'); except end;

  SsnOrDateChanged := true;
  TimeChanged := true;
end;


procedure TFoCalculator.InitCoeffArrays(ARows, ACols: integer);
var
  m, s, i: integer;
begin
  for m:=1 to 12 do
    for s:=0 to 1 do
      begin
      SetLength(FCoeffArrays[m,s], ARows);
      for i:=0 to ARows-1 do
        SetLength(FCoeffArrays[m,s][i].K, ACols);
      end;

  SetLength(Coeff, ARows);
  for i:=0 to ARows-1 do
    SetLength(Coeff[i].K, ACols);
end;


procedure TFoCalculator.LoadCoefficients(ResourceName: string);
var
  m, s, i: integer;
begin
  with TResourceStream.Create(0, ResourceName, RT_RCDATA) do
    try
      for m:=1 to 12 do
        for s:=0 to 1 do
          for i:=0 to High(FCoeffArrays[m,s]) do //76
            with FCoeffArrays[m,s][i] do
              begin
              ReadBuffer(K0, SizeOf(Single));
              ReadBuffer(K[0], SizeOf(TComplex) * Length(K)); //2*6
              end;
    finally
      Free;
    end;
end;




//------------------------------------------------------------------------------
//                                 get/set
//------------------------------------------------------------------------------

procedure TFoCalculator.SetSsn(const Value: Single);
begin
  if Value <> FSsn then SsnOrDateChanged := true;
  FSsn := Value;
end;


procedure TFoCalculator.SetUtc(const Value: TDateTime);
begin
  if Trunc(Value) <> Trunc(FUtc)  then SsnOrDateChanged := true;
  if Frac(Value) <> Frac(FUtc) then TimeChanged := true;
  FUtc := Value;
end;






//------------------------------------------------------------------------------
//                               calculate
//------------------------------------------------------------------------------
function TFoCalculator.Calculate(P: TGeoPoint; ModDip: Single): Single;
begin
  if SsnOrDateChanged then InterpolateCoeff;
  if TimeChanged then CalcTimeSinCos;
  if TimeChanged or SsnOrDateChanged then CalcTimeHarmonics;

  TimeChanged := false;
  SsnOrDateChanged := false;

  Result := DoCalculate(P, ModDip);
end;



procedure TFoCalculator.CalcTimeSinCos;
var
  Hour: Single;
  i: integer;
begin
  Hour := Frac(FUtc) * TWO_PI - PI;

  SnCs[0].Re := Cos(Hour);
  SnCs[0].Im := Sin(Hour);

  for i:=1 to High(SnCs) do
    begin
    SnCs[i].Re := SnCs[0].Re * SnCs[i-1].Re - SnCs[0].Im * SnCs[i-1].Im;
    SnCs[i].Im := SnCs[0].Re * SnCs[i-1].Im + SnCs[0].Im * SnCs[i-1].Re;
    end;
end;


procedure TFoCalculator.CalcTimeHarmonics;
var
  i, j: integer;
  Sum: Single;
begin
  for i:=0 to High(TimeHarm) do
    with Coeff[i] do
      begin
      Sum := K0;
      for j:=0 to High(SnCs) do
        Sum := Sum + K[j].Re * SnCs[j].Im + K[j].Im * SnCs[j].Re;
      TimeHarm[i] := Sum;
      end;
end;


procedure TFoCalculator.InterpolateCoeff;
var
  Frame: TInterpolationFrame;
  dt: Single; //interpolation parameter in time
  dr: Single; //interpolation parameter in SSN
  z1,z2,z3,z4: Single;
  m1, m2: integer;
  i, j: integer;
begin
  FillFrame(Frame, FUtc);

  dt := Frame.dt;
  dr := FSsn / 100;

  z1 := (1-dt)*(1-dr);
  z2 := dt*(1-dr);
  z3 := (1-dt)*dr;
  z4 := dt*dr;

  m1 := Frame.Month1;
  m2 := Frame.Month2;

  for i:=0 to High(Coeff) do
    begin
    Coeff[i].K0 := FCoeffArrays[m1,0,i].K0 * z1
                 + FCoeffArrays[m2,0,i].K0 * z2
                 + FCoeffArrays[m1,1,i].K0 * z3
                 + FCoeffArrays[m2,1,i].K0 * z4;

    for j:=0 to High(Coeff[i].K) do
      begin
      Coeff[i].K[j].Re := FCoeffArrays[m1,0,i].K[j].Re * z1
                        + FCoeffArrays[m2,0,i].K[j].Re * z2
                        + FCoeffArrays[m1,1,i].K[j].Re * z3
                        + FCoeffArrays[m2,1,i].K[j].Re * z4;

      Coeff[i].K[j].Im := FCoeffArrays[m1,0,i].K[j].Im * z1
                        + FCoeffArrays[m2,0,i].K[j].Im * z2
                        + FCoeffArrays[m1,1,i].K[j].Im * z3
                        + FCoeffArrays[m2,1,i].K[j].Im * z4;
      end;
    end;
end;


function TFoCalculator.DoCalculate(P: TGeoPoint; ModDip: Single): Single;
var
  i, j, Idx: integer;
  SinDip: Single;              //Sin(ModDip)
  SinDipArr: array of Single;  //array of Sin(ModDip)^N
  CosLat: Single;              //Cos(Lat)
  CosLatN: Single;             //Cos(lat)^N
begin
  //pre-calc Sin(ModDip)^N
  SetLength(SinDipArr, Max(HarmCounts[0], HarmCounts[1]) + 1);
  SinDip := Sin(ModDip);
  SinDipArr[0] := 1;
  for i:=1 to High(SinDipArr) do
    SinDipArr[i] := SinDipArr[i-1] * SinDip;

  //ModDip
  Result := 0;
  for i:=0 to HarmCounts[0] do
    Result := Result + TimeHarm[i] * SinDipArr[i];

  //Lat, Lon
  Idx := HarmCounts[0] + 1;
  CosLat := Cos(P.Lat);
  CosLatN := CosLat;
  for i:=1 to High(HarmCounts) do
    begin
    for j:=0 to HarmCounts[i] do
      begin
      Result := Result + SinDipArr[j] * CosLatN *
        (TimeHarm[Idx] * Cos(P.Lon * i) + TimeHarm[Idx+1] * Sin(P.Lon * i));
      Inc(Idx, 2);
      end;
    CosLatN := CosLatN * CosLat;
    end;
end;






//------------------------------------------------------------------------------
//       TXmCalculator - inherits all functionality from TFoCalculator
//------------------------------------------------------------------------------

{ TXmCalculator }

constructor TXmCalculator.Create;
begin
  SetLength(SnCs, XM_COLS);
  SetLength(TimeHarm, XM_ROWS);

  SetLength(HarmCounts, Length(XM_HARM_COUNTS));
  Move(XM_HARM_COUNTS[0], HarmCounts[0], SizeOf(XM_HARM_COUNTS));

  InitCoeffArrays(XM_ROWS, XM_COLS);
  LoadCoefficients('M3000_COEFF');
end;




end.

