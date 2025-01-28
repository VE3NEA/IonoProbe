unit MyMath;

interface

uses
  Windows, SysUtils, Math;

type
  TGeoPoint = record Lat, Lon: Single; end;
  TSingleArray = array of Single;
  TSingleArray2D = array of array of Single;
  TIntegerArray2D = array of array of integer;

  TFun = function (X: Single): Single of object;

const
  HALF_PI = Pi / 2;
  TWO_PI = Pi * 2;
  RinD = Pi / 180;
  DinR = 180 / Pi;
  SMALL_VALUE: Single = 1E-5;
  HOUR1 = 1 / 24;



function Sgn(X: integer): integer; overload;
function Sgn(X: Single): integer; overload;

function GPOINT(Lat, Lon: Single): TGeoPoint; overload;
function GPOINT(P: TPoint): TGeoPoint; overload;
function Adjust2Pi(X: Single): Single;
function AdjustPi(X: Single): Single;
function Adjust24(X: TDateTime): TDateTime;

function SafeArcSin(X: Extended): Extended;
function SafeArcCos(X: Extended): Extended;
function SafeArcTan(Y, X: Extended): Extended;

function FromDb(X: Single): Single;
function ToDb(X: Single): Single;

function InterpolateInLatLon(Lat, Lon: Single; Arr: TSingleArray2D): Single;
function BackInterpol(X1,X2,Y1,Y2,Eps,Y: Single; Fun: TFun; var X: Single): boolean;


implementation

function GPOINT(Lat, Lon: Single): TGeoPoint; overload
begin
  Result.Lat := Lat;
  Result.Lon := Lon;
end;

function GPOINT(P: TPoint): TGeoPoint; overload;
begin
  Result := GPOINT(P.y / 180 * RinD, P.x / 180 * RinD);
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


function SafeArcTan(Y, X: Extended): Extended;
begin
  if Abs(X) < SMALL_VALUE
    then Result := Pi /2 * Sgn(Y)
    else  Result := ArcTan2(Y, X);
end;


function Adjust2Pi(X: Single): Single;
begin
  Result := X;
  while Result < 0 do Result := Result + TWO_PI;
  while Result >= TWO_PI do Result := Result - TWO_PI;
end;


function AdjustPi(X: Single): Single;
begin
  Result := X;
  while Result < -Pi do Result := Result + TWO_PI;
  while Result >= Pi do Result := Result - TWO_PI;
end;

function Adjust24(X: TDateTime): TDateTime;
begin
  Result := X;
  while Result < 0 do Result := Result + 1;
  while Result >= 1 do Result := Result - 1;
end;



//------------------------------------------------------------------------------
//                            2D interpolation
//------------------------------------------------------------------------------

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


function FromDb(X: Single): Single;
begin
  Result := Power(10, 0.1*X);
end;


function ToDb(X: Single): Single;
begin
  Result := 10 * Log10(X);
end;


function BackInterpol(X1,X2,Y1,Y2,Eps,Y: Single; Fun: TFun; var X: Single): boolean;
{
SUBROUTINE REGFA1(X11,X22,FX11,FX22,EPS,FW,F,SCHALT,X)
REGULA-FALSI-PROCEDURE TO FIND X WITH F(X)-FW=0. X1,X2 ARE THE
STARTING VALUES. THE COMUTATION ENDS WHEN THE X-INTERVAL
HAS BECOME LESS THAN EPS . IF SIGN(F(X1)-FW)= SIGN(F(X2)-FW)
THEN SCHALT=.TRUE.
}
var
  L1,LINKS,K: boolean;
  NG, FX, DX: Single;
  LFD: integer;
begin
  Result := (Y1 * Y2) > 0;
  if not Result then
    begin X := 0; Result := false; Exit; end;

  Y1 := Y1 - Y;
  Y2 := Y2 - Y;

  K := false;
  NG := 2;
  LFD := 0;
  L1 := false; //compiler is happy


  X := (X1*Y2 - X2*Y1) / (Y2-Y1);

  repeat
    FX := Fun(X) - Y;
    Inc(LFD);
    if LFD > 20 then begin Eps := Eps * 10; LFD := 0; end;
    LINKS := Y1 * FX > 0;
    K := not K;
    if LINKS
      then begin X1 := X; Y1 := FX; end
      else begin X2 := X; Y2 := FX; end;
    if Abs(X2-X1) <= Eps then Exit;
    if K
      then
        begin
        L1 := LINKS;
        DX := (X2 - X1) / NG;
        if not LINKS then DX := DX * (NG-1);
        X := X1 + DX;
        end
      else
        begin
        if LINKS xor L1 then NG := 2 * NG;
        X := (X1*Y2 - X2*Y1) / (Y2-Y1);
        end;
  until
    false;
end;


end.

