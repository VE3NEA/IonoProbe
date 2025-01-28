unit IriSun;

//The algorithm is described at
//http://hotel04.ausys.se/pausch/comp/ppcomp.html

interface

uses
  SysUtils, Math, MyMath;

type
  TTwilightKind = (twCivil, twNautical, twAstron);
  TSeason = (seSpring, seSummer, seFall, seWinter);

type
  TSchlyterSun = class
  private
    //input
    FUtc: TDateTime;
    FObserver: TGeoPoint;
    FTwilightKind: TTwilightKind;
    //output
    FSubSolar: TGeoPoint;

    function CalcSubsolarPoint(AUtc: TDateTime): TGeoPoint;
    function CalcLocalNoonUT(AUtc: TDateTime; SunLon: Single): TDateTime;
    function CalcCosLHA(CosZ, SunLat: Single): Single;

    procedure SetUtc(const Value: TDateTime);

    function CalcRiseSet(CosZ: Single; Rise: boolean): TDateTime;
    function DoCalcRiseSet(AUtc: TDateTime; Sun: TGeoPoint; CosZ: Single;
      Rise: boolean): TDateTime;
  public
    function ZenithAngle: Single;
    function NoonZenAngle: Single;
    function NightTime: boolean;
    function Noon: TDateTime;

    function MeanAnomaly: Single;
    function Season: TSeason;

    function ApproxSunrise: TDateTime;
    function ApproxSunset: TDateTime;

    function Sunrise: TDateTime;
    function Sunset: TDateTime;
    function Dawn: TDateTime; //morning
    function Dusk: TDateTime; //evening

    property Utc: TDateTime read FUtc write SetUtc;
    property Observer: TGeoPoint read FObserver write FObserver;
    property TwilightKind: TTwilightKind read FTwilightKind write FTwilightKind;

    property SubSolar: TGeoPoint read FSubSolar;
  end;


const
  //Sun's upper limb touches the horizon; atmospheric refraction accounted for
  OFFICIAL_Z = -0.01454389765; //Cos(90° 50') = Cos(90.83°)

  COS_Z: array[TTwilightKind] of Single = (
    //one can no longer read outside without artificial illumination
    -0.10452846327, //Cos(96°)
    //navigation using a sea horizon no longer possible
    -0.20791169082, //Cos(102°)
    //the sky is completely dark
    -0.30901699437); //Cos(108°)


implementation


procedure TSchlyterSun.SetUtc(const Value: TDateTime);
begin
  FUtc := Value;
  FSubSolar := CalcSubsolarPoint(FUtc);
end;




function TSchlyterSun.CalcSubsolarPoint(AUtc: TDateTime): TGeoPoint;
var
  DayNo, Ma, e, Ecl, Ea, v, w, LonS: Single;
  xs, ys, xe, ye, ze: Single;
  RA, Decl: Single;
  GMST0: Single;
begin
  //day number (with fraction) since Dec 31, 1999 00 UTC
  DayNo := AUtc - 36525;
  //Sun's mean anomaly
  Ma := 6.214191 + 0.01720197 * DayNo;
  //eccentricity
  e := 0.016709 - 1.151E-9 * DayNo;
  //obliquity of the ecliptic
  Ecl := 0.4090929 - 6.218606E-9 * DayNo;
  //argument of perihelion
  w := 4.93824 + 8.219364E-7 * DayNo;

  //eccentric anomaly
  Ea := Ma + e * Sin(Ma) * (1 + e * Cos(Ma));
  //true anomaly
  v := SafeArcTan(Sqrt(1 - Sqr(e)) * Sin(Ea), Cos(Ea) - e);
  //the Sun's true longitude
  LonS := v + w;
  //Sun's X, Y in the plane of the ecliptic
  xs := Cos(LonS);
  ys := Sin(LonS);
  ///convert X, Y to equatorial, rectangular, geocentric coordinates
  xe := xs;
  ye := ys * Cos(Ecl);
  ze := ys * Sin(Ecl);
  //Right Ascension
  RA := SafeArcTan(ye, xe);
  //Declination
  Decl := SafeArcTan(ze, Sqrt(Sqr(xe) + Sqr(ye)));


  //sidereal time at Greenwich at 00 UTC  (radians)
  GMST0 := Ma + w + Pi;
  //subsolar
  Result.Lat := Decl;
  Result.Lon := AdjustPi(RA - GMST0 - Frac(AUtc)*TWO_PI);
end;


//Sun's Local Hour Angle at rise/set
function TSchlyterSun.CalcCosLHA(CosZ, SunLat: Single): Single;
begin
  //numerator
  Result := OFFICIAL_Z - Sin(FObserver.Lat) * Sin(SunLat);

  if Abs(FObserver.Lat) < HALF_PI
    //denominator
    then Result := Result / (Cos(FObserver.Lat) * Cos(SunLat))
    //North or South Pole: set CosLHA = +2 or -2
    else Result := 2 * Sgn(Result);
end;


function TSchlyterSun.CalcLocalNoonUT(AUtc: TDateTime; SunLon: Single): TDateTime;
begin
  Result := Trunc(AUtc) //date
    //time
    + Adjust2Pi(Frac(AUtc)*TWO_PI + SunLon - FObserver.Lon) / TWO_PI;
end;

function TSchlyterSun.CalcRiseSet(CosZ: Single; Rise: boolean): TDateTime;
const
  MIN1 = 1 / 24 / 60;
var
  OldResult: TDateTime;
  i: integer;
  Sun: TGeoPoint;
begin
  Result := DoCalcRiseSet(FUtc, SubSolar, CosZ, Rise);
  if Abs(Result) = 1 then Exit;
  OldResult := Result;

  for i:=1 to 4 do
    begin
    Sun := CalcSubsolarPoint(Result);
    Result := DoCalcRiseSet(Result, Sun, CosZ, Rise);
    if (Abs(Result) = 1) or (Abs(Result - OldResult) < MIN1) then Exit;
    OldResult := Result;
    end;
end;

function TSchlyterSun.DoCalcRiseSet(AUtc: TDateTime; Sun: TGeoPoint;
  CosZ: Single; Rise: boolean): TDateTime;
var
  NoonUT: TDateTime;
  CosLHA: Single;
begin
  NoonUT := CalcLocalNoonUT(AUtc, Sun.Lon);
  CosLHA := CalcCosLHA(CosZ, Sun.Lat);

  if Abs(CosLHA) > 1 then Result := Sgn(CosLHA)
  else if Rise then Result := NoonUT - ArcCos(CosLHA) / TWO_PI
  else Result := NoonUT + ArcCos(CosLHA) / TWO_PI;

  //use FUtc here, not AUtc
  if NightTime
    then
      begin
      if Rise and (Result < FUtc) then Result := Result + 1
      else if (not Rise) and (Result > FUtc) then Result := Result - 1;
      end
    else
      begin
      if Rise and (Result > FUtc) then Result := Result - 1
      else if (not Rise) and (Result < FUtc) then Result := Result + 1;
      end;
end;


function TSchlyterSun.MeanAnomaly: Single;
begin
  Result := Adjust2Pi(6.214191 + 0.01720197 * (FUtc - 36191));
end;

function TSchlyterSun.NightTime: boolean;
begin
  Result := ZenithAngle > 90.83*RinD;
end;

function TSchlyterSun.Noon: TDateTime;
begin
  Result := CalcLocalNoonUT(FUtc, FSubSolar.Lon);
end;

function TSchlyterSun.Season: TSeason;
var
  Sz: integer;
begin
  //is higher accuracy needed?
  Sz := Trunc((MeanAnomaly - Pi/8) / HALF_PI);
  if FObserver.Lat < 0 then Inc(Sz, 2); //hemisphere
  Result := TSeason( (Sz+4) mod 4 );   // -> 0..3
end;

function TSchlyterSun.NoonZenAngle: Single;
begin
  Result := Abs(FSubSolar.Lat - FObserver.Lat);
end;


function TSchlyterSun.Dawn: TDateTime;
begin
  Result := CalcRiseSet(COS_Z[TwilightKind], true);
end;

function TSchlyterSun.Dusk: TDateTime;
begin
  Result := CalcRiseSet(COS_Z[TwilightKind], false);
end;



function TSchlyterSun.ZenithAngle: Single;
begin
  Result := SafeArcCos(
    Sin(FObserver.Lat) * Sin(FSubSolar.Lat)
    + Cos(FObserver.Lat) * Cos(FSubSolar.Lat)
    * Cos(FObserver.Lon - FSubSolar.Lon));
end;

function TSchlyterSun.Sunrise: TDateTime;
begin
  Result := CalcRiseSet(OFFICIAL_Z, true);
end;

function TSchlyterSun.Sunset: TDateTime;
begin
  Result := CalcRiseSet(OFFICIAL_Z, false);
end;

function TSchlyterSun.ApproxSunrise: TDateTime;
begin
  Result := DoCalcRiseSet(FUtc, SubSolar, OFFICIAL_Z, true);
end;

function TSchlyterSun.ApproxSunset: TDateTime;
begin
  Result := DoCalcRiseSet(FUtc, SubSolar, OFFICIAL_Z, false);
end;

end.

