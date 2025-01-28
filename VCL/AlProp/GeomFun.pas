unit GeomFun;

interface

uses
  Windows, SysUtils, Classes, Math;

const
  RinD = Pi / 180;
  DinR = 180 / Pi;
  HALF_PI = Pi / 2;
  TWO_PI = Pi * 2;

  SinD = 86400;

  UNITS_IN_EQUATOR = 360 * 180;
  RAD_IN_UNIT: Single = 2 * Pi / UNITS_IN_EQUATOR;
  UNITS_IN_RAD: Single = UNITS_IN_EQUATOR / (2 * Pi);
  MAX_LON = UNITS_IN_EQUATOR div 2;
  MAX_LAT = UNITS_IN_EQUATOR div 4;
  SMALL_VALUE: Single = 1E-5;


type
  TSingleArray = array of Single;
  TSingleArray2D = array of array of Single;
  TIntegerArray2D = array of array of integer;


//point & rect
function PointsEqual(P1, P2: TPoint): boolean;
function RectOverlap(const R1, R2: TRect): boolean;
procedure ValidatePoint(var P: TPoint);
function IsValidPoint(P: TPoint): boolean;
//math
function Sgn(X: integer): integer; overload;
function Sgn(X: Single): integer; overload;
function SafeArcTan(Y, X: Extended): Extended;
function SafeArcSin(X: Extended): Extended;
function SafeArcCos(X: Extended): Extended;
function SafeMod(y, x: Single): Single;
function SafeStrToFloat(S: string): Single;
function SafeFloatToStr(V: Single):string;
function SafeDecimalFormat(const Format: string;
  const Args: array of const): string;
//geographic
function LatLonToStr(P: TPoint): string;
function AdjustLongitude(const Lon: integer): integer;
function InterpolateInLatLon(Lat, Lon: Single; Arr: TSingleArray2D): Single;
function AntipodOf(P: TPoint): TPoint;
function PtInsideRect(P: TPoint; R: TRect): boolean;
//function CalcArea(Hdr: PItemHeader): Double;


implementation

{ Functions }

function PointsEqual(P1, P2: TPoint): boolean;
begin
  Result := CompareMem(@P1, @P2, Sizeof(TPoint));
end;


function RectOverlap(const R1, R2: TRect): boolean;
var
  DummyR: TRect;
begin
  Result := IntersectRect(DummyR, R1, R2);
end;

function Sgn(X: integer): integer;
begin
  if X < 0 then Result := -1
  else if X > 0 then Result := 1
  else Result := 0;
end;

function Sgn(X: Single): integer; overload;
begin
  if X < 0 then Result := -1
  else if X > 0 then Result := 1
  else Result := 0;
end;

function SafeArcTan(Y, X: Extended): Extended;
begin
  if Abs(X) < SMALL_VALUE
    then Result := Pi /2 * Sgn(Y)
    else  Result := ArcTan2(Y, X);
end;

function SafeArcSin(X: Extended): Extended;
begin
  if X > 1 then Result := HALF_PI
  else if X < -1 then Result := -HALF_PI
  else  Result := ArcSin(X);
end;

function SafeArcCos(X: Extended): Extended;
begin
  if X > 1 then Result := 0
  else if X < -1 then Result := Pi
  else  Result := ArcCos(X);
end;

function SafeMod(y, x: Single): Single;
begin
  if y >= 0
    then Result := y - x * Round(y / x)
    else Result := y + x * (Round(-y / x) + 1);
end;


function SafeStrToFloat(S: string): Single;
var
  OldDecimalSeparator: Char;
begin
  if S = '' then begin Result := 0; Exit; end;

  OldDecimalSeparator := DecimalSeparator;
  try
    DecimalSeparator := '.';
    try Result := StrToFloat(S); except Result := 0; end;
  finally
    DecimalSeparator := OldDecimalSeparator;
  end;
end;



function SafeFloatToStr(V: Single):string;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := DecimalSeparator;
  try
    DecimalSeparator := '.';
    if (Abs(V) > 1) and (Abs(V) < 100)
      then Result := Format('%.2f', [V])
      else Result := FloatToStr(V);
  finally
    DecimalSeparator := OldDecimalSeparator;
  end;
end;


function SafeDecimalFormat(const Format: string; const Args: array of const): string;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := DecimalSeparator;
  try
    DecimalSeparator := '.';
    Result := SysUtils.Format(Format, Args);
  finally
    DecimalSeparator := OldDecimalSeparator;
  end;
end;


function LatLonToStr(P: TPoint): string;
const
  LatChar: array[-1..1] of Char = ('S', 'N', 'N');
  LonChar: array[-1..1] of Char = ('W', 'E', 'E');
  Fmt = '%d°%.2d'' %s';
var
  NewP: TPoint;
begin
  NewP.y := Round(Abs(P.y) / 3);
  NewP.x := Round(Abs(P.x) / 3);
  Result := Format(Fmt, [NewP.y div 60, NewP.y mod 60, LatChar[Sgn(P.y)]]);
  Result := Result + '  ' +
    Format(Fmt, [NewP.x div 60, NewP.x mod 60, LonChar[Sgn(P.x)]]);
end;


procedure ValidatePoint(var P: TPoint);
begin
  P.x := Sgn(P.x) * (((Abs(P.x) + MAX_LON) mod UNITS_IN_EQUATOR) - MAX_LON);
  if Abs(P.y) > MAX_LAT then P.y := MAX_LAT * Sgn(P.y);
end;


function IsValidPoint(P: TPoint): boolean;
begin
  Result := (Abs(P.x) <= MAX_LON) and (Abs(P.y) <= MAX_LAT);
end;


function AdjustLongitude(const Lon: integer): integer;
begin
  Result := Lon;
  while Result > MAX_LON do Dec(Result, UNITS_IN_EQUATOR);
  while Result < -MAX_LON do Inc(Result, UNITS_IN_EQUATOR);
end;





//input: Lat in [-Pi/2..Pi/2],  Lon in [0..2*Pi] or in [-Pi..Pi], doesn't matter
//data in array: [0,0] <- (Lat = -90, Lon = -180)
function InterpolateInLatLon(Lat, Lon: Single; Arr: TSingleArray2D): Single;
var
  X0, Y0: integer;
  dX, dY: Single;
begin
  if High(Arr) < 0 then begin Result := 0; Exit; end;
  //make Lat in [0, Pi[
  Lat := Max(Lat + HALF_PI, 0);
  if Lat >= Pi then begin Result := Arr[0, High(Arr[0])]; Exit; end;
  //make Lon in [0, 2*Pi[
  Lon := Max(Lon + Pi, 0);
  if Lon >= TWO_PI then Lon := Lon - TWO_PI;
  //find cell and offset
  Lat := Lat / PI * High(Arr[0]);
  Lon := Lon / TWO_PI * High(Arr);
  X0 := Trunc(Lon);
  Y0 := Trunc(Lat);
  dX := Frac(Lon);
  dY := Frac(Lat);
  //interpolate
  Result := Arr[X0,Y0] * (1-dX)*(1-dY) + Arr[X0+1,Y0] * dX*(1-dY)
          + Arr[X0,Y0+1] * (1-dX)*dY + Arr[X0+1,Y0+1] * dX*dY;
end;


function AntipodOf(P: TPoint): TPoint;
begin
  Result.x := P.x + MAX_LON;
  Result.y := -P.y;
  ValidatePoint(Result);
  //if Result.x = -MAX_LON then Inc(Result.x);
end;


function PtInsideRect(P: TPoint; R: TRect): boolean;
begin
  Result := (P.x > R.Left) and (P.x < (R.Right-1)) and
            (P.y > R.Top) and (P.y < (R.Bottom-1));
end;
                               

{
function CalcArea(Hdr: PItemHeader): Double;
var
  i: integer;
begin
  Result := 0;
  for i:=0 to Hdr.Cnt-2 do
    with Hdr^ do
      Result := Result + Pts[i].x * Pts[i+1].y - Pts[i+1].x * Pts[i].y;
  Result := 0.5 * Abs(Result);
end;
}


end.
