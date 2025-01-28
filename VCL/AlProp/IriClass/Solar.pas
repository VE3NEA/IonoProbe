unit Solar;

interface

uses
  SysUtils, Math, MyMath;

function SubSolarPoint(Utc: TDateTime): TGeoPoint;
function ZenithAngle(P: TGeoPoint; Utc: TDateTime): Single;


implementation


function SubSolarPoint(Utc: TDateTime): TGeoPoint;
var
  Mon, Day, Year: Word;
  K: Single;
  Ma: Single;
  LonS: Single;
  RA: Single;
  SinDec: Single;
begin
  //Sun's mean anomaly
  DecodeDate(Utc, Year, Mon, Day);
  K := Utc - EncodeDate(Year, 1, 1) + 1;
  Ma := 0.017202*K - 0.0574039;

  //Sun's ecliptic true longitude
  LonS := Adjust2Pi(Ma + 0.0334405*Sin(Ma) + 3.49066E-04*Sin(2*Ma)+ 4.932894);

  //Sun's right ascension
  try RA := ArcTan(0.91764 * Tan(LonS)); except Ra := LonS; end;
  //quadrant
  RA := RA + (Floor(LonS/HALF_PI) - Floor(RA/HALF_PI)) * HALF_PI;

  //Sun's declination
  SinDec := 0.39782 * Sin(LonS);

  //subsolar point
  Result.Lat := ArcSin(SinDec);
  Result.Lon := RA - 0.0172028 * K - 1.73364 - Frac(Utc)*TWO_PI;
  Result.Lon := AdjustPi(Result.Lon);
end;


function ZenithAngle(P: TGeoPoint; Utc: TDateTime): Single;
var
  S: TGeoPoint;
begin
  S := SubSolarPoint(Utc);

  Result := SafeArcCos(
    Cos(S.Lat) * Cos(P.Lat) * Cos(P.Lon-S.Lon) + Sin(S.Lat) * Sin(P.Lat));
end;



end.

