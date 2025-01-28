unit IriStdPf;

interface

uses
  SysUtils, Classes, Controls, Forms,
  Math, MyMath, MagFld, Schlyter, SsnHist, IriLayrs, LayFuns;

type
  TStdIriProfile = class (TIriLayers)
  private
    function DensityBelowF2(H: Single): Single;
    function DensityEV(H: Single): Single;
    function DensityF1(H: Single): Single;
    function DensityVF1(H: Single): Single;
    function ProbabilityOfF1: Single;
    function FindHmF1: Single;
    procedure FindHST;

    procedure Lecden(FoE, FoF1, FoF2, HmE, HmF1, HmF2, YmE, YmF1,
      YmF2: Single);
  protected
    //layer boundaries
    {?}HZ, HEF, {?}HST: Single;

    //topside
    Delta, Beta, Eta, Zeta: Single;

    //bottomside
    B0, B1: Single;  //+ HMF2, XNMF2

    //F1
    C1: Single; //+ HMF1

    //F1_V
    {?}T: Single; //+ xnme

    //V_E
    NightTime: boolean; //+ XNME, HME
    ValleyHDeep, ValleyWidth, ValleyDepth, ValleySlope{DLNDH}: Single;
    ValleyPoly: array[1..4] of Single;

    //D
    Dela: Single;
    Hdx, D1, XKK, FP1, FP2, FP3O, FP3U: Single;


    function CalcDensity(H: Single): Single;
    function DensityBelowE(H: Single): Single;
    function DensityAboveF2(H: Single): Single;
    procedure CalcExt_D;
    procedure CalcExt_TopsideF2;
    procedure CalcExt_BottomsideF2;
    function CalcValleyPoly: boolean;
public
    //output params
    Profile: TSingleArray;
    NmF2, NmE: Single;
    F1Present: boolean;
    NmF1: Single;

    LecdenX, LecdenY: TSingleArray;

    //profile params
    procedure CalcAll_D_E_F1;
    procedure CalcAllLayers;
    //profile
    procedure CalcProfile(MinH, MaxH, Step: Single);
    procedure CalcProfileFromParams(MinH, MaxH, Step: Single);
    procedure CalcProfile2;
  end;


implementation

{ TStdIriProfile }

procedure TStdIriProfile.CalcAll_D_E_F1;
const
  XDELS: array [TSeason] of Single = (5, 5, 5, 10);
  DNDS: array [TSeason] of Single = (0.016, 0.01, 0.016, 0.016);
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
  NmE := FreqToNe(FoE);

  FoF1 := DoCalcFoF1;
  NmF1 := FreqToNe(FoF1);
  HmF1 := DoCalcHmF1;

  //D ext. params
  CalcExt_D;


  //-------------------------------------


  NightTime := FSun.NightTime;
  F1Present := ProbabilityOfF1 > 0.5;

  //F1 layer shape parameter C1
  C1 := 0.09 + 0.11 / Dela;
  if SunRise = SunSet
    then C1 := 2.5 * C1
    else C1 := 2.5 * C1 * Cos(Pi * (Frac(Utc) - 0.5) / (SunSet - SunRise));
  C1 := Max(0, C1);

  //Valley
  ValleyHDeep := HPOL(Utc, 10.5/Dela, 28,           SunRise, SunSet, HOUR1);
  ValleyWidth := HPOL(Utc, 17.8/Dela, 45 + 22/Dela, SunRise, SunSet, HOUR1);
  ValleyDepth := HPOL(Utc, XDELS[FSun.Season]/Dela, 81, SunRise, SunSet, HOUR1);
  ValleySlope := HPOL(Utc ,DNDS[FSun.Season]/Dela, 0.06, SunRise, SunSet, HOUR1);
  if ValleyDepth < 1 then ValleyWidth := 0;
  if NightTime then ValleyDepth := -ValleyDepth;
  CalcValleyPoly;

  HEF := HmE + ValleyWidth;

  //F1
  HmF1 := HmF2;
  if F1Present then HmF1 := FindHmF1;

  FindHST;
end;


//Input: Rz(Utc), MagLat, SolZen
function TStdIriProfile.ProbabilityOfF1: Single;
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


function TStdIriProfile.CalcValleyPoly: Boolean;
//(ValleyHDeep, ValleyDepth, ValleyWidth, ValleySlope: Single; NightTime: boolean);
var
  Z1,Z2,Z3,Z4,B,C: Single;
begin
{
      SUBROUTINE TAL(SHABR,SDELTA,SHBR,SDTDH0,AUS6,SPT)
C CALCULATES THE COEFFICIENTS SPT FOR THE POLYNOMIAL
C Y(X)=1+SPT(1)*X**2+SPT(2)*X**3+SPT(3)*X**4+SPT(4)*X**5
C TO FIT THE VALLEY IN Y, REPRESENTED BY:
C Y(X=0)=1, THE X VALUE OF THE DEEPEST VALLEY POINT (SHABR),
C THE PRECENTAGE DEPTH (SDELTA), THE WIDTH (SHBR) AND THE
C DERIVATIVE DY/DX AT THE UPPER VALLEY BOUNDRY (SDTDH0).
C IF THERE IS AN UNWANTED ADDITIONAL EXTREMUM IN THE VALLEY
C REGION, THEN AUS6=.TRUE., ELSE AUS6=.FALSE..
C FOR -SDELTA THE COEFF. ARE CALCULATED FOR THE FUNCTION
C Y(X)=EXP(SPT(1)*X**2+...+SPT(4)*X**5).
}

  if ValleyDepth < 0
    then
      begin
      ValleyDepth := -ValleyDepth;
      Z1 := Ln(1 - ValleyDepth / 100) / Sqr(ValleyHDeep);
      end
    else
      Z1 := -ValleyDepth / (100 * Sqr(ValleyHDeep));

  Z3 := ValleySlope / (2 * ValleyWidth);
  Z4 := ValleyHDeep - ValleyWidth;

  ValleyPoly[4] := 2 * (Z1*(ValleyWidth-2*ValleyHDeep)*ValleyWidth+Z3*Z4*ValleyHDeep)/
    (ValleyHDeep*ValleyWidth* IntPower(Z4, 3));
  ValleyPoly[3] := Z1 * (2 * ValleyWidth - 3 * ValleyHDeep)/ (ValleyHDeep * Sqr(Z4))-
    (2 * ValleyHDeep + ValleyWidth) * ValleyPoly[4];
  ValleyPoly[2] := -2 * Z1 / ValleyHDeep - 2 * ValleyHDeep * ValleyPoly[3]
    - 3.0 * Sqr(ValleyHDeep) * ValleyPoly[4];
  ValleyPoly[1] := Z1 - ValleyHDeep * (ValleyPoly[2] + ValleyHDeep
     * (ValleyPoly[3] + ValleyHDeep * ValleyPoly[4]));

  Result := true;

  B := 4 * ValleyPoly[3] / (5 * ValleyPoly[4]) + ValleyHDeep;
  C := -2 * ValleyPoly[1] / (5 * ValleyPoly[4] * ValleyHDeep);
  Z2 := Sqr(B) / 4 - C;

  if Z2 >= 0 then
    begin
    Z3 := Sqrt(Z2);
    Z1 := B / 2;
    Z2 := -Z1 + Z3;
    if (Z2 > 0) and (Z2 < ValleyWidth)
      then
        Result := false
      else
        begin
        if Abs(Z3) > 1E-15 then Z2 := -Z1 - Z3 else Z2 :=  C / Z2;
        if (Z2 > 0) and (Z2 < ValleyWidth) then Result := false;
        end;
    if not Result then Beep;
    end;
end;


procedure TStdIriProfile.CalcAllLayers;
begin
  CalcBasic_F2;
  CalcExt_TopsideF2;
  CalcExt_BottomsideF2;

  CalcAll_D_E_F1; //basic and extended params for D, E, and F1
end;


procedure TStdIriProfile.CalcProfile(MinH, MaxH, Step: Single);
begin
  CalcAllLayers;
  CalcProfileFromParams(MinH, MaxH, Step);
end;

procedure TStdIriProfile.CalcProfileFromParams(MinH, MaxH, Step: Single);
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




//------------------------------------------------------------------------------
//                             layer params
//------------------------------------------------------------------------------
procedure TStdIriProfile.CalcExt_D;
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
  X := HmE-HDX;
  Xkk := -DXDX * X / (XDX * Ln(XDX / NmE));

  if Xkk > XKKMAX
    then
      begin
      Xkk := XKKMAX;
      D1 := -Ln(xdx/nme)/Power(x, Xkk);
      end
    else
      D1 := DXDX / (XDX * Xkk * Power(X, Xkk-1));
end;


procedure TStdIriProfile.CalcExt_TopsideF2;
var
  COS2, FLU, Ex, Z, Z2: Single;
begin
  //peak density
  NmF2 := FreqToNe(FoF2);

  //topside params

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


procedure TStdIriProfile.CalcExt_BottomsideF2;
const
  BOB1 = 0.755566;
begin
  B1 := 3;
  B0 := YmF2 / BOB1;
end;





//------------------------------------------------------------------------------
//                             profile
//------------------------------------------------------------------------------
function TStdIriProfile.CalcDensity(H: Single): Single;
begin
{
	if(f1reg) then
		hmf1=xhmf1
	else
		hmf1=hmf2
	endif
}

  if H >= HmF2 then Result := DensityAboveF2(H) //XE1
  else if H >= HmF1 then Result := DensityBelowF2(H)//xe2
  else if H >= HZ then Result := DensityF1(H)//XE3_1
  else if H >= HEF then Result := DensityVF1(H)//XE4_1
  else if H >= HmE then Result := DensityEV(H)//XE5
  else Result := DensityBelowE(H); //XE6
end;

function TStdIriProfile.DensityAboveF2(H: Single): Single;
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
  Result := NmF2 * Exp(-y);
end;


function TStdIriProfile.DensityBelowE(H: Single): Single;
var
  Z, FP3: Single;
begin
  if H > Hdx
    then
      begin
      Z := HmE - H;
      Result := NmE * Exp(-D1 * Power(Z,XKK));
      end
    else
      begin
      Z := H - HmD;
      if Z > 0 then FP3 := FP3O else FP3 := FP3U;
      Result := NmD * Exp(Z*(FP1+Z*(FP2+Z*FP3)))
      end;
end;


function TStdIriProfile.DensityBelowF2(H: Single): Single;
var
  Norm, Z: Single;
begin
  Norm := Max(0, (HmF2-H) / B0);
  Z := Min(ARGMAX, Power(Norm, B1));
  Result := NmF2 * Exp(-Z) / Cosh(Norm);
end;


function TStdIriProfile.DensityF1(H: Single): Single;
begin
  if F1Present then H := HmF1 * (1 - Power((HmF1-H)/HmF1, 1 + C1));
  Result := DensityBelowF2(H);
end;


function TStdIriProfile.DensityVF1(H: Single): Single;
begin
	if HST < 0
   then
	   Result := NmE + T * (H - HEF)
   else
     begin
      if HST <> HEF then
        H := HZ + 0.5*T -SIGN(1, T) * Sqrt(T*(0.25*T + HZ - H));
     Result := DensityF1(H);
     end;
end;


function TStdIriProfile.DensityEV(H: Single): Single;
var
  Tmp: Single;
begin
  Tmp := H - HmE;
  Tmp := Sqr(Tmp) * (ValleyPoly[1] + Tmp * (ValleyPoly[2] + Tmp * (ValleyPoly[3] + Tmp * ValleyPoly[4])));

  if NightTime
    then Result := NmE * Exp(Tmp)
    else Result := NmE * (1 + Tmp);
end;



function TStdIriProfile.FindHmF1: Single;
var
  OldHEF: Single;
  XE2H: Single;
begin
  OldHEF := hef;

  if NmE >= 0.9 * NmF1 then
    begin Result := 0; F1Present := false; Exit; end;

  repeat
    repeat
    XE2H := DensityBelowF2(HEF);
    if XE2H <= 0.9 * NmF1
      then
        begin
        if not BackInterpol(HEF,HmF2,XE2H,NmF2,0.001,NmF1,DensityBelowF2,Result)
          then begin Result :=0; F1Present := false; end;
        Break;
        end
      else
        begin
        hef := hef-1;
        if(hef <= hme) then begin HMF1:=0; F1Present := false; Break; end;
        end;
    until
      false;


    if HEF = OldHEF then Exit;

    ValleyWidth := HEF - HmE;
    if NightTime then ValleyDEPTH := -ValleyDEPTH;
    if CalcValleyPoly then Exit;
    ValleyWidth := 0;
    HEF := HmE;
    OldHEF := HEF;
  until
    false;
end;


procedure TStdIriProfile.FindHST;
var
  Hf1,Hf2,Xf1,Xf2, D: Single;
begin
//SEARCH FOR HST [NE3(HST)=NME]

  if F1Present
    then
      begin
      Hf1 := HmF1;
      Xf1 := NmF1;
      end
    else
      begin
      Hf1 := (HmF2 + HEF) / 2;
      Xf1 := DensityBelowF2(Hf1);
      end;

  Hf2 := HEF;
  Xf2 := DensityF1(Hf2);

  if (Xf2 > NmE) or not BackInterpol(Hf1,Hf2,Xf1,Xf2,0.001,NmE,DensityF1,HST)
    then
      begin
      HZ := (HEF + Hf1) / 2;
      T := (DensityF1(HZ) - NmE) / (HZ - HEF);
      HST := -333;
      end
    else
      begin
        HZ := (HST + Hf1) / 2;
        D := HZ - HST;
        T := Sqr(D) / (HZ - HEF - D);
      end;
end;






//------------------------------------------------------------------------------
//                                  VOA
//------------------------------------------------------------------------------
procedure TStdIriProfile.Lecden(FoE,FoF1,FoF2,HmE,HmF1,HmF2,YmE,YmF1,YmF2: Single);
var
  XLow,XUp,HLow,HUp, HZ,HTE,HB2,FCE,FC1,FC2 : Single;
  FUp,FLow,ASP,HB1,HT1,HTW, YS,S1,hiold: Single;
  DENOM,YB: Single;
  HD, XTAIL, HEX, FNX, ALP, FSQ, HDIF: Single;
  FND, FNE, FN1, FN2, FNVAL, H: Single;

  IH: integer;
  LIN: boolean;
begin
  LecdenX := nil;
  Lecdeny := nil;
  SetLength(LecdenX, 51);
  SetLength(Lecdeny, 51);

  //MUST CHECK ON F1 LAYER PARAMETERS FIRST
  XLOW := 0.8516;
  HZ := HmE - YmE;
  XUP := 0.98 * FoE / FoF2;
  HLOW := HZ + YmE * (1 + Sqrt(1 - Sqr(XLow)));
  HTE := HmE + YmE;
  HB2 := HmF2 - YmF2;
  FCE := Sqr(FoE);
  FC2 := Sqr(FoF2);

  //VALLEY FILLED FROM (FLOW,HLOW) TO (FUP,HUP)
  HUP := HB2 + YmF2 * (1 - Sqrt(1 - Sqr(XUP)));
  FUP := Sqr(XUP) * FC2;
  FLOW := Sqr(XLOW) * FCE;
  ASP := 0;
  if HUP > HLOW then ASP := (FUP - FLOW) / (HUP - HLOW);

  {!}HT1 := 0;

  if FoF1 > 0 then
    begin
    FC1 := Sqr(FoF1);
    HB1 := HmF1 - YmF1;
    HT1 := HmF1 + YmF1;

    //HEIGHT OF F2 AT F1 CRITICAL FREQUENCY
    HTW := HB2 + YmF2 * (1 - Sqrt(1 - FC1/FC2));
    if HTW > (HmF1 + 0.001) then
        begin
        LIN := false;
        //FORCE F1 ABOVE E LAYER
        YmF1 := Min(YmF1, HmF1 - HmE + 1);
        {FSECV(K) = -1.}
        end
      else
        begin
        //FORCE F1 AT CRITICAL FREQUENCY
        YS := Max(1, HTW - HB1);

        //SLOPE OF LINEAR F1
        S1 := FC1 / YS;
        hiold := HmF1;
        HmF1 := HTW;
        YmF1 := YS;

        //AVOID A SPURIOUS LAYER
        if HB2 < HB1
          then
            begin
            YmF1 := HmF1 - HB2;
            LIN := false;
            //FORCE F1 ABOVE E LAYER
            YmF1 := Min(YmF1, HmF1 - HmE + 1);
            {FSECV(K) = -1.}
            end
          else
            begin
            //SET FLAG TO INDICATE LINEAR LAYER FOR F1
            LIN := true;
            {FSECV(K) =  SQRT(FLOW)}
            DENOM := 1 - Sqr(FoE / FoF1);

            //F1 LINE NOT TO OBSCURE E LAYER
            DENOM := Max(0.17, DENOM);
            YB := (HmF1 - HmE) / DENOM;
            if YS >= YB then
              begin
              //THIS  IS CCIR(1976) IF FI(2,K) = 1.7*FI(1,K)
              //F1 PASSES THROUGH E NOSE
              YS := YB;
              YmF1 := YS;
              S1  := FC1 / YS;
              {HB1 := HmE;}
              end;
            HT1 := HTW;
            end;
      end;
    end; //if FoF1 > 0
    

  //D-REGION PROFILE - XTAIL MAY HAVE DIURNAL VARIATION.
  HD := 70;
  XTAIL := 0.85;
  HEX := HmE - XTAIL * YmE;
  FNX := 1 - Sqr(XTAIL);

  //SLOPE OF E IS SAME AS SLOPE OF V AT HEX
  ALP := 2 * (HmE - HEX) / (FNX * Sqr(YmE));
  FSQ := FNX * Exp(-ALP * (HEX - HD));
  LecdenX[1] := HD;
  LecdenX[5] := HEX;
  HDIF := (LecdenX[5] - LecdenX[1]) * 0.25;
  HDIF := Max(0, HDIF);
  LecdenX[4] := LecdenX[5] - Min(1, HDIF);
  LecdenX[2] := LecdenX[1] + HDIF;
  LecdenX[3] := (LecdenX[2] + LecdenX[4]) * 0.5;

  //E  BELOW NOSE
  LecdenX[11] := HmE;
  HDIF := (LecdenX[11] - LecdenX[5]) / 6;
  for IH:=6 to 10 do LecdenX[IH] := LecdenX[IH - 1] + HDIF;

  //E ABOVE NOSE
  LecdenX[17] := HmE + YmE;
  HDIF := (LecdenX[17] - LecdenX[11]) / 6;
  for IH:=12 to 16 do LecdenX[IH] := LecdenX[IH-1] + HDIF;
  LecdenX[11] := 0.5 * (LecdenX[10] + LecdenX[12]);
  LecdenX[50] := HmF2;

  if FoF1 > 0
    then //F1 LAYER AND F2 LAYER
      begin
      HB2 := HmF2 - YmF2;
      HB1 := HmF1 - YmF1 + 0.00001;
      if HB2 <= HB1
        then
          begin
          //F2  LAYER, NO F1 LAYER.
          LecdenX[18] := HmF2 - YmF2;
          HDIF := (LecdenX[50] - LecdenX[18]) / 32;
          for IH:=19 to 49 do LecdenX[IH] := LecdenX[IH-1] + HDIF;
          end
        else
          begin
          LecdenX[18] := HmF1 - YmF1;
          LecdenX[18] := Max(LecdenX[18], LecdenX[17] + 1);
          LecdenX[28] := HmF1;
          HDIF := (LecdenX[28] - LecdenX[18]) / 10;
          for IH:=19 to 27 do LecdenX[IH] := LecdenX[IH-1] + HDIF;
          HDIF := (LecdenX[50] - LecdenX[28]) / 22;
          for IH:=29 to 49 do LecdenX[IH] := LecdenX[IH-1] + HDIF;
          end;
      end
    else //F2 LAYER, NO F1 LAYER.
      begin
      LecdenX[18] := HmF2 - YmF2;
      HDIF := (LecdenX[50] - LecdenX[18]) / 32;
      for IH:=19 to 49 do LecdenX[IH] := LecdenX[IH-1] + HDIF;
      end;

  //FORCE F1 ABOVE E LAYER
  HB1 := Max(HmE, HmF1-YmF1);


  for IH:=1 to 50 do
    begin
    FND := 0;
    FNE := 0;
    FN1 := 0;
    FN2 := 0;
    FNVAL := 0;
    H := LecdenX[IH];

    if (H > HLOW) and (H < HUP) then FNVAL := FUP + ASP * (H - HUP);

    if H < HEX
      then
        FND := FCE * FSQ * Exp(ALP * (H-HD))
      else if H <= HTE then
        begin
        //PARABOLIC E
        FNE := FCE * (1 - Sqr((H-HmE) / YmE));
        //EXPONENTIAL D-E
        if H < HEX then
          FND := FCE * FSQ * Exp(ALP * (H - HD));
        end;

    if (FoF1 > 0) and (H >= HB1) and (H <= HT1) then
      if LIN
        //LINEAR F1
        then FN1 := S1 * (H - (HmF1-YmF1))
        //PARABOLIC F1
        else FN1 := FC1 * (1 - Sqr((H-HmF1) / YmF1));

    //PARABOLIC F2
    if H >= HB2 then FN2 := FC2 * (1.0 - Sqr((H-HmF2) / YmF2));

    Lecdeny[IH] := MaxValue([FND, FNE, FNVAL, FN1, FN2]);
    //USE THE MAXIMUM
    end; //for IH:=1 to 50
end;



procedure TStdIriProfile.CalcProfile2;
var
  YmE, YmF1, NewFoF1, NewYmF2: Single;
  i: integer;
begin
  YmE := 110 / 5.5;
  YmF1 := HmF1 / 4;

  if F1Present
    then NewFoF1 := FoF1
    else NewFoF1 := 0;

  NewYmF2 := Sqrt(2) * YmF2;

  Lecden(FoE, NewFoF1, FoF2, HmE, HmF1, HmF2, YmE, YmF1, NewYmF2);

  for i:=1 to High(LecdenY) do LecdenY[i] := FreqToNe(Sqrt(LecdenY[i]));
end;




end.

