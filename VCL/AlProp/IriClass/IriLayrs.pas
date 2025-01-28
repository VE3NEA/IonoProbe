unit IriLayrs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Math, MyMath, LayFuns, Solar, MagFld, Schlyter, SsnHist, SpherHrm;

const
  HmE = 110;

type
  TIriLayers = class(TComponent)
  protected
    FGeoField: TGeoField;
    FSun: TSchlyterSun;
    FoCalculator: TFoCalculator;
    XmCalculator: TXmCalculator;

    //internal params
    FMagLat, FMoDip, FMagDipLat: Single;
    SunRise, SunSet: TDateTime;

    function DoCalcNmD: Single;
    function DoCalcHmD: Single;
    function DoCalcFoE: Single;
    function DoCalcFoF1: Single;
    function DoCalcHmF1: Single;
    function DoCalcFoF2: Single;
    function DoCalcHmF2: Single;
    function DoCalcYmF2: Single;
    function Rogul: Single;
  public
    //input params
    Location: TGeoPoint;
    Utc: TDateTime;
    Rz: Single;
    Ig: Single;

    //output params
    HmF2, HmF1, HmD: Single;
    FoF2, FoF1, FoE, NmD: Single;
    YmF2: Single;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //layer params
    function Calc_NmD: Single;
    function Calc_HmD: Single;
    function Calc_FoE: Single;
    function Calc_FoF1: Single;
    function Calc_HmF1: Single;
    function Calc_FoF2: Single;
    function Calc_HmF2: Single;
    function Calc_YmF2: Single;
    function Calc_MUF3000: Single;
    procedure CalcBasic_EF1;
    procedure CalcBasic_F2;
  published

  end;


function FreqToNe(Freq: Single): Single;
function NeToFreq(Ne: Single): Single;



implementation


function FreqToNe(Freq: Single): Single;
const
  W = 1.24E10;
begin
  Result := W * Sqr(Freq);
end;


function NeToFreq(Ne: Single): Single;
const
  W = 1 / 1.24E10;
begin
  Result := Sqrt(Ne * W);
end;





{ TIriLayers }

//------------------------------------------------------------------------------
//                               System
//------------------------------------------------------------------------------
constructor TIriLayers.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGeoField := TGeoField.Create;
  FSun := TSchlyterSun.Create;
  FoCalculator := TFoCalculator.Create;
  XmCalculator := TXmCalculator.Create;
end;


destructor TIriLayers.Destroy;
begin
  XmCalculator.Free;
  FoCalculator.Free;
  FSun.Free;
  FGeoField.Free;

  inherited Destroy;
end;



//------------------------------------------------------------------------------
//                          wrapper functions
//------------------------------------------------------------------------------

function TIriLayers.Calc_NmD: Single;
begin
  FSun.SetParams(Location, Utc);
  Result := DoCalcNmD;
end;


function TIriLayers.Calc_HmD: Single;
begin
  FSun.SetParams(Location, Utc);
  SunRise := FSun.ApproxSunRise;
  SunSet := FSun.ApproxSunSet;

  Result := DoCalcHmD;
end;



function TIriLayers.Calc_FoE: Single;
begin
  FSun.SetParams(Location, Utc);
  Result := DoCalcFoE;
end;


function TIriLayers.Calc_FoF1: Single;
begin
  FSun.SetParams(Location, Utc);
  FGeoField.Location := Location;
  FMagDipLat := FGeoField.MagDipLat;

  Result := DoCalcFoF1;
end;


function TIriLayers.Calc_HmF1: Single;
begin
  FSun.SetParams(Location, Utc);
  Result := DoCalcHmF1;
end;


function TIriLayers.Calc_FoF2: Single;
begin
  FGeoField.Location := Location;
  FMoDip := FGeoField.ModifMagDip;

  Result := DoCalcFoF2;
end;


function TIriLayers.Calc_HmF2: Single;
begin
  FSun.SetParams(Location, Utc);
  FGeoField.Location := Location;
  FMoDip := FGeoField.ModifMagDip;
  FMagDipLat := FGeoField.MagDipLat;

  FoE := DoCalcFoE;
  FoF2 := DoCalcFoF2;

  Result := DoCalcHmF2;
end;


function TIriLayers.Calc_MUF3000: Single;
var
  Fout: Single;
begin
  FGeoField.Location := Location;
  FMoDip := FGeoField.ModifMagDip;

  Fout := DoCalcFoF2;

  XmCalculator.Utc := Utc;
  XmCalculator.Ssn := Rz;
  Result := XmCalculator.Calculate(Location, FMoDip);

  Result := Result * Fout;
end;


function TIriLayers.Calc_YmF2: Single;
begin
  FSun.SetParams(Location, Utc);
  FGeoField.Location := Location;
  FMoDip := FGeoField.ModifMagDip;
  FMagDipLat := FGeoField.MagDipLat;

  FoE := DoCalcFoE;
  FoF2 := DoCalcFoF2;
  HmF2 := DoCalcHmF2;

  Result := DoCalcYmF2;
end;


//------------------------------------------------------------------------------
//                            group wrappers
//------------------------------------------------------------------------------
procedure TIriLayers.CalcBasic_EF1;  //FoE, FoF1, HmF1
begin
  FSun.SetParams(Location, Utc);
  FGeoField.Location := Location;
  FMagDipLat := FGeoField.MagDipLat;

  FoE := DoCalcFoE;
  FoF1 := DoCalcFoF1;
  HmF1 := DoCalcHmF1;
end;


procedure TIriLayers.CalcBasic_F2;   //FoF2, HmF2, YmF2
begin
  YmF2 := Calc_YmF2; //also calcs FoF2 and HmF2. Attention! overwrites FoE
end;





//------------------------------------------------------------------------------
//                            layer params
//------------------------------------------------------------------------------
function TIriLayers.DoCalcNmD: Single;
const
  Y_NIGHT = 4E8;
var
  Y, YY: Single;
begin
  if FSun.ZenithAngle > HALF_PI
    then
      Result := Y_NIGHT
    else
      begin
      Y := 6.05E8 + 0.088E8 * Rz;
      YY := Cos(FSun.ZenithAngle);
      Result := Y * Exp(-0.1 / (Power(YY, 2.7)));
      Result := Max(Result, Y_NIGHT);
      end;
end;


function TIriLayers.DoCalcHmD: Single;
begin
  Result := HPOL(Utc, 81, 88, Sunrise, Sunset, 1/24);
end;


//CALCULATES FOE/MHZ BY THE EDINBURGH-METHOD
function TIriLayers.DoCalcFoE: Single;
const
  Deg32 = 32 * RinD;
  Deg12 = 12 * RinD;
  Deg89_98 = 89.98 * RinD;
  Deg89_999 = 89.999 * RinD;
  Deg3 = 3 * RinD;
var
  Sfi: Single;
  AbsLat, CosLat: Single;
  NoonZen, NightZen: Single;

  A, B, C, D: Single;
  PwrB, PwrD: Single;
begin
  //input params
  Sfi := SfiFromRz(Rz);
  AbsLat := Abs(Location.Lat);

  //variation with solar activity (factor A)
  A := 1 + 0.0094 * (Sfi - 66);

  //variation with noon solar zenith angle (B) and with latitude (C)
  CosLat := Cos(AbsLat);
  if AbsLat < Deg32
    then
      begin
      PwrB := -1.93 + 1.92 * CosLat;
      C := 23 + 116 * CosLat;
      end
    else
      begin
      PwrB := 0.11 - 0.49 * CosLat;
      C := 92 + 35 * CosLat;
      end;

  NoonZen := Min(FSun.NoonZenAngle, Deg89_999);
  B := Power(Cos(NoonZen), PwrB);

  //variation with solar zenith angle (D)
  if AbsLat > Deg12 then PwrD := 1.2 else PwrD := 1.31;

  //adjusted solar zenith angle during nighttime (XHIC)
  NightZen := FSun.ZenithAngle - Deg3 * Ln(1 + Exp((FSun.ZenithAngle-Deg89_98) / Deg3));
  D := Power(Cos(NightZen), PwrD);

  //determine foE**4
  Result := A * B * C * D;

  //minimum allowable foE (sqrt[SMIN])
  Result := Max(Result, Sqr(0.121 + 0.0015 * (Sfi-60)));

  Result := Power(Result, 0.25);
end;


function TIriLayers.DoCalcFoF1: Single;
var
  DLA: Single;
  F0, F100, FS: Single;
  XMUE: Single;
begin
	Result := 0;
  if FSun.ZenithAngle >= HALF_PI then Exit;

  DLA := Abs(FMagDipLat) * DinR;

  F0 := 4.35 + DLA * (0.0058 - 1.2E-4 * DLA);
  F100 := 5.348 + DLA * (0.011 - 2.3E-4 * DLA);
  FS := F0 + (F100 - F0) * Rz / 100;

  XMUE := 0.093 + DLA * (0.0046 - 5.4E-5 * DLA) + 0.0003 * Rz;
  Result := FS * Power(Cos(FSun.ZenithAngle), XMUE);
{
  Chi0 = 49.84733 + 0.349504 * DLA
  Chi100 = 38.96113 + 0.509932 * DLA
  ChiM := Chi0 + (Chi100 - Chi0) * R / 100;
  if FSun.ZenithAngle*RinD > ChiM then Result := -Result;
}
end;


function TIriLayers.DoCalcHmF1: Single;
begin
  Result := 165 + 0.6428 * DinR * FSun.ZenithAngle;
end;


function TIriLayers.DoCalcFoF2: Single;
begin
  FoCalculator.Utc := Utc;
  FoCalculator.Ssn := Ig;
  Result := FoCalculator.Calculate(Location, FMoDip);
end;


function TIriLayers.DoCalcHmF2: Single;
var
  F1, F2, F3, DELM: Single;
begin
  F1 := (2.32E-3) * Rz + 0.222;
  F2 := 1.2 - (1.16E-2) * Exp((2.39E-2) * Rz);
  F3 := 0.096*(Rz - 25) / 150;
  DELM := 1 - Rz / 150 * Exp(-Sqr(FMagDipLat*DinR)/1600);
  DELM := F1 * DELM / (FoF2/FoE - F2) + F3;

  XmCalculator.Utc := Utc;
  XmCalculator.Ssn := Rz;
  Result := XmCalculator.Calculate(Location, FMoDip);

  Result := 1490 / (Result + DELM) - 176;
end;


function TIriLayers.DoCalcYmF2: Single;
var
  Grat: Single;
begin
  //B0_98 is better

  if FSun.NightTime
    then Grat := 0.91 - HmF2 / 4000
    else Grat := Rogul;

  Result := HmF2 * (1 - Grat);
end;


//CALCULATES RATIO H0.5/HMF2 FOR HALF-DENSITY POINT (NE(H0.5)=0.5*NMF2)
function TIriLayers.Rogul: Single;
var
  Sx, Xs: Single;
begin
  Sx := 2 - Cos(FSun.MeanAnomaly);
  Xs := (FSun.ZenithAngle*DinR - 20 * Sx) / 15;
  Result := 0.8 - 0.2 / (1 + Exp(Xs));
end;




end.

