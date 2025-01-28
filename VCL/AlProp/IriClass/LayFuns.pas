unit LayFuns;

interface

uses
  SysUtils, Math, Svd, MyMath;

const
  ARGMAX = 88; //MAXIMUM ARGUMENT ALLOWED FOR EXP-FUNCTION
  NUMLAY = 4; //number of Lay funs
  NC1 = 2;    //# of 1-st derivative constraints

type
  TLayFunctions = class
  private

    //system of linear equations A[]*x=B[]
    A: array[1..NUMLAY, 1..NUMLAY] of Single;
    B: array[1..NUMLAY] of Single;

    //fun calc result
    FErr: boolean;

    procedure LSKNM(NumCon: integer);
    procedure LNGLSN;
    procedure NewLSKNM(NumCon: integer);
  public
    FSvd: TSvdSolver;
    //Amplitude, scale and height PARAMETERS FOR LAY FUNCTIONS
    LayA, LayS, LayH: array [1..NUMLAY] of Single;
    //constraints: x = height  y = log(Ne/NmF2)  w = weights
    X, Y, W: array[1..8] of Single;

    NightTime, F1Present: boolean;
    NmF2, NmF1, NmE: Single;
    HmF2, HmF1, HmE: Single;
    VBtmN, VTopH, VBtmH: Single;
    HalfH: Single;

    constructor Create;
    destructor Destroy; override;

    procedure CalcFunctions;
    function GetValue(Height: Single): Single;
  end;



function HPOL(Utc, DayV, NightV, SunRise, SunSet, Step: Single): Single;
function EPTR(X, Scale, X0: Single): Single;


implementation

//------------------------------------------------------------------------------
//                          EPSTEIN FUNCTIONS
//------------------------------------------------------------------------------

//TRANSITION
function EPTR(X, Scale, X0: Single): Single;
var
  Temp: Single;
begin
  Temp := (X - X0) / Scale;
  if Abs(Temp) < ARGMAX then Result := Ln(1 + Exp(Temp))
  else if Temp > 0 then Result := Temp
  else Result := 0;
end;


//STEP
function EPST(X, Scale, X0: Single): Single;
var
  Temp: Single;
begin
  Temp := (X - X0) / Scale;
  if Abs(Temp) < ARGMAX then Result := 1 / (1 + Exp(-Temp))
  else if Temp > 0 then Result := 1
  else Result := 0;
end;


//RAWER  LAYER
function RLAY(X, Xm, Scale, X0: Single): Single;
begin
  Result := EPTR(X, Scale, X0) - EPTR(Xm, Scale, X0)
          - EPST(Xm, Scale, X0) * (X - Xm) / Scale;
end;


//dLay/dX
function D1LAY(X, Xm, Scale, X0: Single): Single;
begin
  Result := (EPST(X, Scale, X0) - EPST(Xm, Scale, X0)) /  Scale;
end;



function HPOL(Utc, DayV, NightV, SunRise, SunSet, Step: Single): Single;
//function HPOL(HOUR,TW,XNW,SA,SU,DSA,DSU)
{
C PROCEDURE FOR SMOOTH TIME-INTERPOLATION USING EPSTEIN
C STEP FUNCTION AT SUNRISE (SA) AND SUNSET (SU). THE
C STEP-WIDTH FOR SUNRISE IS DSA AND FOR SUNSET DSU.
C TW,NW ARE THE DAY AND NIGHT VALUE OF THE PARAMETER TO
C BE INTERPOLATED. SA AND SU ARE TIME OF SUNRIES AND
C SUNSET IN DECIMAL HOURS.
}
begin
  if Abs(SunSet) = 1
    then if SunSet > 0 then Result := DayV else Result := NightV
    else if (Abs(Utc - SunRise) < Abs(Utc - SunSet))
      then Result := NightV + (DayV-NightV) * EPST(Utc, Step, SunRise)
      else Result := DayV - (DayV-NightV) * EPST(Utc, Step, SunSet)

{
      Result := 0.5 * ((DayV+NightV) + (DayV-NightV)
        * (EPST(Utc, Step, SunRise) - EPST(Utc, Step, SunSet)));
}
{
    else Result := NightV
          + (DayV-NightV) * EPST(Utc, Step, SunRise)
          + (NightV-DayV) * EPST(Utc, Step, SunSet);
}
end;



//------------------------------------------------------------------------------
//                             TLayFunctions
//------------------------------------------------------------------------------

{ TLayFunctions }

constructor TLayFunctions.Create;
begin
  FSvd := TSvdSolver.Create;
end;


destructor TLayFunctions.Destroy;
begin
  FSvd.Free;
  inherited;
end;


function TLayFunctions.GetValue(Height: Single): Single;
var
  i: integer;
  Temp: Single;
begin
  if FErr then begin Result := 0; Exit; end;

  Result := 1;
  for i:=1 to High(LayA) do
    begin
    Temp := LayA[i] * RLAY(Height, HmF2, LayS[i], LayH[i]);
    Result := Result * Power(10, Temp);
    end;

  Result := NmF2 * Result;
end;



//AMPLITUDES FOR LAY FUNCTIONS
procedure TLayFunctions.CalcFunctions;
{SUBROUTINE INILAY(IsNight=NIGHT,IsF1=F1REG,XNMF2,XNMF1,XNME,VBtmN=VNE,
   HMF2,HMF1,HME,VTopHHV1,VBtmH=HV2,HalfH=HHALF,HXL,SCL,AMP,IQUAL)}
var
  LogF, LogEF, HalfN, LayH1Alt: Single;
  NumCon: integer;
  HFFF, XFFF: Single;
  i: integer;
begin
  //cludge
  HFFF := 0; XFFF := 0;
  for i:=1 to 8 do W[i] := 1;


  LogF := Log10(NmF2);
  LogEF := Log10(NmE) - LogF;
  HalfN := NmF2 / 2;

  X[1] := HalfH;
  X[2] := VTopH;
  X[3] := VBtmH;
  X[4] := HmE;
  X[5] := HmE - (VBtmH - HmE);

  Y[1] := -Log10(2);
  Y[2] := LogEF;
  Y[3] := Log10(VBtmN) - LogF;
  Y[4] := LogEF;
  Y[5] := Y[3];

  Y[7] := 0;


  W[2] := 1;
  W[3] := 3;//2;
  W[4] := 15;//5;


  //geometric paramters for LAY
  LayS[1] := 0.8 * 0.7 * (0.216 * (HmF2 - HalfH) + 56.8);
  LayS[2] := 10;
  LayS[3] := 9;
  LayS[4] := 6;

  LayH[3] := VBtmH;

  //DAY/night CONDITION
  if NightTime
    then //night
      begin
      NumCon := 7;
      LayH[1] := HalfH;
      LayH1Alt  := 0.4 * HmF2 + 30;
      LayH[2] := (HmF2 + VTopH) / 2;
      LayH[4] := HmE;
      X[6] := VBtmH;
      X[7] := HmE;
      Y[6] := 0;

      W[1] := 10; //1;
      W[3] := 3;
      W[5] := 0.5;
      W[6] := 50;
      W[7] := 500;

      HFFF := HalfH;
      XFFF := HalfN;
      end
    else //day
      begin
      NumCon := 8;
      LayH[1] := 0.9 * HmF2;
      LayH1Alt  := HalfH;
      LayH[2] := HmF1;
      LayH[4] := HmE - LayS[4];
      X[6] := HmF1;
      X[7] := VBtmH;
      X[8] := HmE;
      Y[8] := 0;

      W[5] := 1;
      W[7] := 50;
      W[8] := 500;


      if F1Present
        then
          begin //with F1-region
          Y[6] := Log10(NmF1) - LogF;

          W[6] := 3;
          if (NmF1-HalfN) * (HmF1-HalfH) < 0
            then W[1] := 0.5
            else W[1] := EPST(Y[1] - Y[6], 0.1, 0.15);

          if HalfH > HmF1
            then begin HFFF := HmF1;  XFFF := NmF1; end
            else begin HFFF := HalfH; XFFF := HalfN; end;
          end
        else
          begin //without F1-region
          LayH[2] := (HmF2 + HalfH) / 2;
          Y[6] := 0;

          W[6] := 0;
          W[1] := 1;

          end;
      end; //else (not night)


  //are valley-top and bottomside point compatible ?
  IF ((VTopH-HFFF)*(NmE-XFFF) < 0) or (VTopH <= (VBtmH+5)) then W[2] := 0.5;

  //DETERMINE AMPLITUDES

  //LSKNM(NumCon);
  NewLSKNM(NumCon);
  if FErr or (Abs(LayA[1]) > 10) then
    begin
    LayH[1] := LayH1Alt;
    //LSKNM(NumCon);
    NewLSKNM(NumCon);
    Beep;
    end;
end;


procedure TLayFunctions.LSKNM(NumCon: integer);
{
        SUBROUTINE LSKNM ( N, M, M0, M1, HM, SC, HX, W, X, Y, VAR, SING)
C --------------------------------------------------------------------
C   DETERMINES LAY-FUNCTIONS AMPLITUDES FOR A NUMBER OF CONSTRAINTS:
C
C       INPUT:  N       NUMBER OF AMPLITUDES ( LAY-FUNCTIONS)
C               M       NUMBER OF CONSTRAINTS
C               M0      NUMBER OF POINT CONSTRAINTS
C               M1      NUMBER OF FIRST DERIVATIVE CONSTRAINTS
C               HM      F PEAK ALTITUDE  [KM]
C               SC(N)   SCALE PARAMETERS FOR LAY-FUNCTIONS  [KM]
C               HX(N)   HEIGHT PARAMETERS FOR LAY-FUNCTIONS  [KM]
C               W(M)    WEIGHT OF CONSTRAINTS
C               X(M)    ALTITUDES FOR CONSTRAINTS  [KM]
C               Y(M)    LOG(DENSITY/NMF2) FOR CONSTRAINTS
C
C       OUTPUT: VAR(M)  AMPLITUDES
C               SING    =.TRUE.   NO SOLUTION
C ------------------------------------------------------------------------
}

//LSKNM(NUMLAY,NUMCON,NC0,NC1,HMF2,SCL,HXL,WW,XX,YY, AMP,SSIN)

var
  i, j, k: integer;
  XLI: array[1..NUMLAY, 1..10] of Single;
begin
  for i:=1 to NUMLAY do
    begin
    //function
    for k:=1 to NumCon-NC1 do XLI[i,k] := RLAY(X[k], HmF2, LayS[i], LayH[i]);
    //1-st derivative
    for k:=NumCon-NC1+1 to NumCon do XLI[i,k] := D1LAY(X[k], HmF2, LayS[i], LayH[i]);
    //2-nd derivative is not used
    //for k:=NC0+NC1+1 to NumCon do XLI[i,k] := D2LAY(X[k], HmF2, LayS[i], LayH[i]);
    end;


  FillChar(A, SizeOf(A), 0);
  FillChar(B, SizeOf(B), 0);

  for j:=1 to NUMLAY do
    for k:=1 to NumCon do
      begin
      B[j] :=                         B[j]   + W[k] * XLI[j,k] * Y[k];
      for i:=1 to NUMLAY do A[j,i] := A[j,i] + W[k] * XLI[j,k] * XLI[i,k];
      end;

  //solve the system of linear equations A[]*LayA[]=B[], results -> LayA[]
  LNGLSN;
end;



procedure TLayFunctions.NewLSKNM(NumCon: integer);
var
  i, k: integer;
  Coeffs: TSingleArray;
begin
  //resize
  FSvd.U := nil;
  SetLength(FSvd.U, NumCon, NUMLAY);
  FSvd.B := nil;
  SetLength(FSvd.B, NumCon);

  //fill U[]
  for i:=0 to NUMLAY-1 do
    begin
    for k:=0 to NumCon-NC1-1 do
      FSvd.U[k,i] := W[k+1] * RLAY(X[k+1], HmF2, LayS[i+1], LayH[i+1]);
    for k:=NumCon-NC1 to NumCon-1 do
      FSvd.U[k,i] := W[k+1] * D1LAY(X[k+1], HmF2, LayS[i+1], LayH[i+1]);
    end;

  //fill B[]
  for k:=0 to NumCon-1 do
    FSvd.B[k] := W[k+1] * Y[k+1];

  //kill if W=0
  for k:=0 to NumCon-1 do
    if W[k+1] = 0 then
      begin
      for i:=0 to NUMLAY-1 do FSvd.U[k,i] := 0;
      FSvd.B[k] := 0;
      end;
      
  //solve
  Coeffs := FSvd.Solve;
  for i:=0 to NUMLAY-1 do LayA[i+1] := Coeffs[i];
end;



//SOLVES QUADRATIC SYSTEM OF LINEAR EQUATIONS:
procedure TLayFunctions.LNGLSN;
var
  NormA: array [1..NUMLAY] of Single;
  NormB: Single;
  R, R2, RMax, C: integer;
  ZeroCnt: integer;
  Temp: Single;
begin
  for R:=1 to NUMLAY-1 do
    begin
    //PIVOTING

    //find largest element in the R-th col (in or below the R-th row)
    RMax := R; ZeroCnt := 0;
    for R2:=R to NUMLAY do
      begin
      if Abs(A[R2,R]) < 1E-8 then Inc(ZeroCnt);
      if Abs(A[R2,R]) > Abs(A[R,R]) then RMax := R2;
      end;

    //R-th col is all zeroes => degraded system
    if Abs(A[RMax,R]) < 1E-10 then begin FErr := true; Exit; end;

    //R-th col is all zeroes below the R-th row => row is done
    if ZeroCnt = NUMLAY - R then Continue;

    //swap rows
    IF RMax <> R then
      begin
      for C:=R to NUMLAY do
        begin Temp := A[RMax,C]; A[RMax,C] := A[R,C]; A[R,C] := Temp; end;
      Temp := B[RMax]; B[RMax] := B[R]; B[R] := Temp;
      end;

      
    //ELIMINATION

    //divide R-th equation by A[R,R], store result in NormA[], NormB
    Temp := 1 / A[R,R];
    NormB := B[R] * Temp;
    for C:=R+1 to NUMLAY do NormA[C] := A[R,C] * Temp;

    //subtract equation with factor
    for R2:=R+1 to NUMLAY do
      begin
      IF Abs(A[R2,R]) < 1E-8 then Continue;
      B[R2] := B[R2] - NormB * A[R2,R];
      for C:=R+1 to NUMLAY do A[R2,C] := A[R2,C] - A[R2,R] * NormA[C];
      A[R2,R] := 0;
      end

    end; //for R=1 to NUMLAY-1


  //BACK SUBSTITUTION

  for R:=NUMLAY downto 1 do
    begin
    Temp := B[R];
    for C:=R+1 to NUMLAY do
      Temp := Temp - A[R,C] * LayA[C];
    if Abs(A[R,R]) < 1E-6
      then LayA[R] := 0 else LayA[R] := Temp / A[R,R];
    end;

  FErr := false;
end;




end.

