unit IriProf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Math, MyMath, LayFuns, MagFld, Schlyter, SsnHist, SpherHrm, IriLayrs;

type
  TIriProfile = class(TIriLayers)
  protected

    //internal params
    ValleyDepth, ValleyWidth, ValleyHDeep: Single;

    //D layer
    Dela: Single;
    Hdx, D1, XKK, FP1, FP2, FP3O, FP3U: Single;

    //topside
    Delta, Beta, Eta, Zeta: Single;

    procedure PrepareLayFunctions;
    function CalcDensity(H: Single): Single;
    function DensityBelowE(H: Single): Single;
    function DensityAboveF2(H: Single): Single;
    function ProbabilityOfF1: Single;
    procedure CalcExt_D;
    procedure CalcExt_Topside;
  public
    Lay: TLayFunctions;
    //output params
    Profile: TSingleArray;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //profile params
    procedure CalcAll_D_E_F1;
    procedure CalcAllLayers;
    //profile
    procedure CalcProfile(MinH, MaxH, Step: Single);
    procedure CalcProfileFromParams(MinH, MaxH, Step: Single);

  published

  end;


procedure Register;




implementation


procedure Register;
begin
  RegisterComponents('GIS', [TIriProfile]);
end;




{ TIriProfile }

//------------------------------------------------------------------------------
//                               System
//------------------------------------------------------------------------------
constructor TIriProfile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Lay := TLayFunctions.Create;
end;


destructor TIriProfile.Destroy;
begin
  Lay.Free;
  inherited Destroy;
end;




//------------------------------------------------------------------------------
//                              wrappers
//------------------------------------------------------------------------------
procedure TIriProfile.CalcAllLayers;
begin
  CalcBasic_F2;   //no extended params for F2
  CalcExt_Topside;
  CalcAll_D_E_F1; //basic and extended params for D, E, and F1
  PrepareLayFunctions;
end;





//------------------------------------------------------------------------------
//                           profile params
//------------------------------------------------------------------------------
//all basic and extended layer params must be calculated
//before this function is called

//Input: NightTime(E), Fo*, Hm*, Valley*, Ym2
procedure TIriProfile.PrepareLayFunctions;
begin
  Lay.NightTime := FSun.NightTime;  //for E&F1
  Lay.F1Present := ProbabilityOfF1 > 0.5;

  Lay.NmF2 := FreqToNe(FoF2);
  Lay.NmF1 := FreqToNe(FoF1);
  Lay.NmE := FreqToNe(FoE);
  Lay.HmF2 := HmF2;
  Lay.HmF1 := HmF1;
  Lay.HmE := HmE;

  Lay.VBtmN := (1 - Abs(ValleyDepth) / 100) * Lay.NmE;
  Lay.VTopH := HmE + ValleyWidth;
  Lay.VBtmH  := HmE + ValleyHDeep;
  Lay.HalfH := HmF2 - YmF2;

  Lay.CalcFunctions;
end;



//Input: Zen, Rise/Set, MagLat, MoDip, Hm*, Fo*
procedure TIriProfile.CalcAll_D_E_F1;
const
  XDELS: array [TSeason] of Single = (5, 5, 5, 10);
var
  AbsMoDip: Single;
begin
  //pre-requisits
  FSun.SetParams(Location, Utc);
  SunRise := FSun.ApproxSunRise;
  SunSet := FSun.ApproxSunSet;
  //pre-requisits
  FGeoField.Location := Location;
  FMagDipLat := FGeoField.MagDipLat;
  FMagLat := FGeoField.MagLat;
  FMoDip := FGeoField.ModifMagDip;

  //Dela
  AbsMoDip := Abs(FMoDip) * DinR; //in degrees
  if AbsMoDip >= 18
    then Dela := 1 + Exp((30 - AbsMoDip) / 10)
    else Dela := 4.32;

  //D basic params
  HmD := DoCalcHmD;
  NmD := DoCalcNmD;
  //E-F1 basic params
  FoE := DoCalcFoE;
  FoF1 := DoCalcFoF1;
  HmF1 := DoCalcHmF1;

  //D ext. params
  CalcExt_D;

  //Valley
  ValleyHDeep := HPOL(Utc, 10.5/Dela, 28,           SunRise, SunSet, HOUR1);
  ValleyWidth := HPOL(Utc, 17.8/Dela, 45 + 22/Dela, SunRise, SunSet, HOUR1);
  ValleyDepth := HPOL(Utc, XDELS[FSun.Season]/Dela, 81,  SunRise, SunSet, HOUR1);

  if ValleyDepth < 1 then ValleyWidth := 0;
end;



procedure TIriProfile.CalcExt_D;
const
  XKKMAX = 5;
var
  F2, F3: Single;
  X, XDX, DXDX: Single;
  NmE: Single;
begin
  NmE := FreqToNe(FoE);

  F2 := HPOL(Utc, 4.6, 4.5, SunRise, SunSet, 1/24);
  F3 := HPOL(Utc, -11.5, -4.0, SunRise, SunSet, 1/24);

  FP1 := HPOL(Utc, 0.02 + 0.03/Dela, 0.05, SunRise, SunSet, 1/24);
  FP2 := -Sqr(FP1) / 2;
  FP3O := (-F2 * FP2 - FP1 + 1 / F2) / Sqr(F2);
  FP3U := (-F3 * FP2 - FP1 - 1 / F3) / Sqr(F3);
  HDX := HmD + F2;

  X := HDX - HMD;
  XDX := NmD * Exp(X * (FP1 + X*(FP2 + X*FP3O)));
  DXDX := XDX * (FP1 + X*(2*FP2 + X*3*FP3O));
  X := HME-HDX;
  Xkk := -DXDX * X / (XDX * Ln(XDX / NmE));

  if Xkk > XKKMAX
    then
      begin
      Xkk := XKKMAX;
      D1 := -Ln(XDX/NmE) / Power(x, Xkk);
      end
    else
      D1 := DXDX / (XDX * Xkk * Power(X, Xkk-1));
end;

procedure TIriProfile.CalcExt_Topside;
var
  COS2, FLU, Ex, Z, Z2: Single;
begin
  COS2 := Sqr(Cos(FMagLat));
  FLU := Min(188, SfiFromRz(Rz));
  FLU := (FLU-40) / 30;

  //Eta
  EX := Exp(-FMagLat * DinR / 15);
  Eta := -0.02 * 4 * EX / Sqr(EX+1);
  Eta := 0.058798 + Eta + FLU*(-0.014065 + 0.0069724*COS2)
      + (0.0024287 + 0.0042810*COS2 - 0.00015280*FoF2) * FoF2;
  //Zeta
  Zeta := 0.078922 - 0.0046702*COS2 - 0.019132*FLU + 0.0076545*FLU*COS2
    + (0.0032513 + 0.0060290*COS2 - 0.00020872*FoF2) * FoF2;
  //Beta
  Beta := -128.03 + 20.253*COS2 + FLU * (-8.0755 - 0.65896*COS2)
    + (0.44041 + 0.71458*COS2 - 0.042966*FoF2) * FoF2;
  //Delta
  Z := Exp(94.5 / Beta);
  Z2 := Z / (Beta * Sqr(Z+1));
  Delta := (Eta/(Z+1) - Zeta/2) / (Eta*Z2 + Zeta/400);
end;


//Input: Rz(Utc), MagLat, SolZen
function TIriProfile.ProbabilityOfF1: Single;
var
  A, B, C, Base, Gamma: Single;
begin
	A := 2.98 + 0.0854 * Rz;
  B := 0.0107 - 0.0022 * Rz;
	C := -0.000256 + 0.0000147 * Rz;
	Gamma := A + (B + C * FMagLat) * FMagLat;
  Base := 0.5 + 0.5 * Cos(FSun.ZenithAngle);
	Result := Power(Base, Gamma);
end;






//------------------------------------------------------------------------------
//                               profile
//------------------------------------------------------------------------------
procedure TIriProfile.CalcProfile(MinH, MaxH, Step: Single {all in km});
begin
  CalcAllLayers;
  CalcProfileFromParams(MinH, MaxH, Step);
end;


procedure TIriProfile.CalcProfileFromParams(MinH, MaxH, Step: Single);
var
  i: integer;
begin
  //clear out array
  Profile := nil;
  if MaxH < MinH then MaxH := HmF2;
  SetLength(Profile, Ceil((MaxH-MinH)/Step) + 1);

  //fill array
  for i:=0 to High(Profile) do
    Profile[i] := CalcDensity(MinH + Step*i);
end;


function TIriProfile.CalcDensity(H: Single): Single;
begin
  if H < HmE then         Result := DensityBelowE(H)
  else if H <= HmF2 then  Result := Lay.GetValue(H)
  else                    Result := DensityAboveF2(H);
end;


function TIriProfile.DensityAboveF2(H: Single): Single;
var
  dxdh, x0, xmx0, x, y, Eptr1, Eptr2: Single;
begin
  DXDH := (1000 - HmF2) / 700;
  x0 := 300 - Delta;
  xmx0 := (H - HmF2) / DXDH;
  x := xmx0 + x0;

  Eptr1 := EPTR(x, Beta, 394.5) - EPTR(x0, Beta, 394.5);
  Eptr2 := EPTR(x, 100, 300) - EPTR(x0, 100, 300);

  y := Beta * Eta * Eptr1 + Zeta * (100 * Eptr2 - xmx0);
  y := y * dxdh;
  if abs(y) > ARGMAX then y := ARGMAX * Sgn(y);
  Result := Lay.NmF2 * Exp(-y);
end;


function TIriProfile.DensityBelowE(H: Single): Single;
var
  Z, FP3: Single;
begin
  if H < 60 then Result := 0
  else if H > Hdx
    then
      begin
      Z := HmE - H;
      Result := Lay.NmE * Exp(-D1 * Power(Z,XKK));
      end
    else
      begin
      Z := H - HmD;
      if Z > 0 then FP3 := FP3O else FP3 := FP3U;
      Result := NmD * Exp(Z*(FP1+Z*(FP2+Z*FP3)));
      end;
end;



end.

