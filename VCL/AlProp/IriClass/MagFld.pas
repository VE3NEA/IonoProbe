unit MagFld;

interface

uses
  Windows, SysUtils, Classes, Math, MyMath, Solar, IriData;

type
  TGeoField = class
  private
    FLocation: TGeoPoint;
    FAltitude: Single;
    FPointChanged: boolean;
    X,
    Y,
    Z, //DOWNWARD VERTICAL COMPONENT
    F: //TOTAL FIELD (GAUSS)
    Single;

    CorMagLatTable: TSingleArray2D;
    AuMagLatTable: array[Boolean] of TSingleArray2D;

    procedure LoadTables;
    procedure SetAltitude(const Value: Single);
    procedure SetLocation(const Value: TGeoPoint);
    procedure CalcXYZ;
    function MagLonEx(P: TGeoPoint): Single;
  public
    constructor Create;

    function AuMagLat: Single;
    function MagLon: Single;
    function MagLocalTime(Utc: TDateTime): Single;

    function MagDip: Single;
    function MagDipLat: Single;
    function ModifMagDip: Single;
    function MagDecl: Single;
    function GyroFreq: Single;

    function MagLat: Single;
    function CorrMagLat: Single;

    property Location: TGeoPoint read FLocation write SetLocation;
    property Altitude: Single read FAltitude write SetAltitude;
  end;

implementation

{ TGeoField }


//------------------------------------------------------------------------------
//                                  sys
//------------------------------------------------------------------------------
constructor TGeoField.Create;
begin
  Altitude := 300;
  LoadTables;
end;


procedure TGeoField.LoadTables;
var
  i: integer;
  Cnt: integer;
  NorthHemi: boolean;
begin
  //corr. mag. lat. data

  SetLength(CorMagLatTable, 21, 91);
  Cnt := Length(CorMagLatTable[0]) * SizeOf(Single);

  try
    with TResourceStream.Create(0, 'CorMagLatTable', RT_RCDATA) do
      try
        for i:=0 to High(CorMagLatTable) do
          ReadBuffer(CorMagLatTable[i,0], Cnt);
      finally Free; end;
  except end;


  //corr. mag. lat. for Aurora

  SetLength(AuMagLatTable[false], 73, 31);
  SetLength(AuMagLatTable[true], 73, 31);
  Cnt := Length(AuMagLatTable[false, 0]) * SizeOf(Single);

  try
    with TResourceStream.Create(0, 'AuMagLatTable', RT_RCDATA) do
      try
        for NorthHemi:=false to true do
          for i:=0 to High(AuMagLatTable[false]) do
              ReadBuffer(AuMagLatTable[NorthHemi,i,0], Cnt);
      finally Free; end;
  except end;
end;


procedure TGeoField.SetAltitude(const Value: Single);
begin
  FAltitude := Value;
  FPointChanged := true;
end;

procedure TGeoField.SetLocation(const Value: TGeoPoint);
begin
  FLocation := Value;
  FPointChanged := true;
end;





//------------------------------------------------------------------------------
//                CorrMagLat and MagLocalTime for Aurora
//------------------------------------------------------------------------------
function TGeoField.MagLon: Single;
begin
  Result := MagLonEx(Location);
end;


function TGeoField.MagLonEx(P: TGeoPoint): Single;
const
  MX = -0.063283; MY = 0.048300; MZ = 0.032037;
  al1 = -0.6343055; am1 = -0.7670308; an1 = -0.0965419;
  al2 = 0.7710093; am2 = -0.6185128; an2 = -0.1516134;
  al3 = 0.0565797; am3 = -0.1706039; an3 = 0.9837139;
var
  px, py, pz: Single;
begin
  px := Cos(P.Lat) * Cos(P.Lon) - MX;
  py := Cos(P.Lat) * Sin(P.Lon) - MY;
  pz := Sin(P.Lat) - MZ;

  Result := SafeArcTan(al2*px+am2*py+an2*pz, al1*px+am1*py+an1*pz);
end;


function TGeoField.MagLocalTime(Utc: TDateTime): Single;
begin
  Result := MagLon - MagLonEx(SubSolarPoint(Utc)) + Pi;
  Result := Adjust2Pi(Result);
end;


function TGeoField.AuMagLat: Single;
begin
  with Location do
    if Abs(Lat) >= 30 * RinD
      then Result := Sgn(Lat) * InterpolateInLatLon(Abs(Lat)*3 - Pi, Lon,
                                  AuMagLatTable[Lat<0])
      else Result := 0;
end;





//------------------------------------------------------------------------------
//                                  MagDip
//------------------------------------------------------------------------------
function TGeoField.ModifMagDip: Single;
begin
 // Result := SafeArcTan(MagDip, Sqrt(Max(0,Cos(Location.Lat))));


  Result := MagDip;
  Result := SafeArcSin(Result / Sqrt(Sqr(Result) + Cos(Location.Lat)));
end;


function TGeoField.MagDipLat: Single;
begin
  Result := SafeArcTan(Tan(MagDip), 2);
end;


function TGeoField.MagDip: Single;
begin
  if FPointChanged then CalcXYZ;
  Result := SafeArcSin(Z / F);
end;


function TGeoField.MagDecl: Single;
begin
  if FPointChanged then CalcXYZ;
  Result := SafeArcSin(Y / Sqrt(Sqr(X) + Sqr(Y)));
end;


function TGeoField.GyroFreq: Single;
begin
  if FPointChanged then CalcXYZ;

  //F in Gauss, Gauss = 1E-4 Tesla
  Result := 2.80E6 * F;
end;



procedure TGeoField.CalcXYZ;
const
  G: array[1..144] of Single = (
    //DATA FEL1/
    0.0, 0.1506723,0.0101742, -0.0286519, 0.0092606,
    -0.0130846, 0.0089594, -0.0136808,-0.0001508, -0.0093977,
    0.0130650, 0.0020520, -0.0121956, -0.0023451, -0.0208555,
    0.0068416,-0.0142659, -0.0093322, -0.0021364, -0.0078910,
    0.0045586,  0.0128904, -0.0002951, -0.0237245,0.0289493,
    0.0074605, -0.0105741, -0.0005116, -0.0105732, -0.0058542,
    0.0033268, 0.0078164,0.0211234, 0.0099309, 0.0362792,
    -0.0201070,-0.0046350,-0.0058722,0.0011147,-0.0013949,
    -0.0108838,  0.0322263, -0.0147390,  0.0031247, 0.0111986,
    -0.0109394,0.0058112,  0.2739046, -0.0155682, -0.0253272,
    0.0163782, 0.0205730,  0.0022081, 0.0112749,-0.0098427,
    0.0072705, 0.0195189, -0.0081132, -0.0071889, -0.0579970,
    -0.0856642, 0.1884260,-0.7391512, 0.1210288, -0.0241888,
    -0.0052464, -0.0096312, -0.0044834, 0.0201764,  0.0258343,
    0.0083033,  0.0077187,
    //DATA FEL2/
    0.0586055,0.0102236,-0.0396107,
    -0.0167860, -0.2019911, -0.5810815,0.0379916,  3.7508268,
    1.8133030, -0.0564250, -0.0557352, 0.1335347, -0.0142641,
    -0.1024618,0.0970994, -0.0751830,-0.1274948, 0.0402073,
    0.0386290, 0.1883088,  0.1838960, -0.7848989,0.7591817,
    -0.9302389,-0.8560960, 0.6633250, -4.6363869, -13.2599277,
    0.1002136,  0.0855714,-0.0991981, -0.0765378,-0.0455264,
    0.1169326, -0.2604067, 0.1800076, -0.2223685, -0.6347679,
    0.5334222, -0.3459502,-0.1573697,  0.8589464, 1.7815990,
    -6.3347645, -3.1513653, -9.9927750,13.3327637, -35.4897308,
    37.3466339, -0.5257398,  0.0571474, -0.5421217,  0.2404770,
    -0.1747774,-0.3433644, 0.4829708,0.3935944, 0.4885033,
    0.8488121, -0.7640999, -1.8884945, 3.2930784,-7.3497229,
    0.1672821,-0.2306652, 10.5782146, 12.6031065, 8.6579742,
    215.5209961, -27.1419220,22.3405762,1108.6394043);
var
  CT, ST, CP, SP: Single;
  XXX, YYY, ZZZ: Single;
  NMAX, IMAX, IHMAX, LAST, i, K, IH, IL, M: integer;
  D, RHO, RQ: Single;
  XI1, XI2, XI3: Single;
  H: array[1..144] of Single;
  X1, Y1, Z1, F1: Single;
  S, XT: Single;
  Brh0: Single;
begin
      CT := Sin(FLocation.Lat);
      ST := Cos(FLocation.Lat);
      NMAX := 11;
      D := Sqrt(40680925.0-272336.0 * Sqr(CT));
      CP := Cos(FLocation.Lon);
      SP := Sin(FLocation.Lon);
      ZZZ := (FAltitude + 40408589 / D) * CT * (1/6371.2);
      RHO := (FAltitude + 40680925 / D) * ST * (1/6371.2);
      XXX := RHO * CP;
      YYY := RHO * SP;
      RQ := 1 / (Sqr(XXX) + Sqr(YYY) + Sqr(ZZZ));
      XI1 :=XXX * RQ;
      XI2 :=YYY * RQ;
      XI3 :=ZZZ * RQ;
      IHMAX := Sqr(NMAX) + 1;
      LAST := IHMAX + NMAX + NMAX;
      IMAX := NMAX + NMAX - 1;
      for i:=IHMAX to LAST do H[i] := G[i];

      K := 1;
      while K <= 3 do
        begin
        I := IMAX;
        IH := IHMAX;

        repeat
          IL := IH-I;
          F1 := 2 / (I-K+2);
          X1 := XI1*F1;
          Y1 := XI2*F1;
          Z1 := XI3*(F1+F1);
          Dec(I, 2);

          if i > 1 then
            begin
            M := 3;
            while M <= I do
              begin
              H[IL+M+1] := G[IL+M+1]+Z1*H[IH+M+1]+X1*(H[IH+M+3]-H[IH+M-1])
               - Y1*(H[IH+M+2]+H[IH+M-2]);
              H[IL+M] := G[IL+M]+Z1*H[IH+M]+X1*(H[IH+M+2]-H[IH+M-2])
               + Y1*(H[IH+M+3]+H[IH+M-1]);
              Inc(M, 2)
              end;
            end;

          if I >= 1 then
            begin
            H[IL+2] := G[IL+2]+Z1*H[IH+2]+X1*H[IH+4]-Y1*(H[IH+3]+H[IH]);
            H[IL+1] := G[IL+1]+Z1*H[IH+1]+Y1*H[IH+4]+X1*(H[IH+3]-H[IH]);
            end;

          H[IL] := G[IL]+Z1*H[IH]+2*(X1*H[IH+1]+Y1*H[IH+2]);
          IH := IL;
        until I < K;

        Inc(K, 2);
        end;

      S := 0.5*H[1]+2*(H[2]*XI3+H[3]*XI1+H[4]*XI2);
      XT := (RQ+RQ)*Sqrt(RQ);
      X := XT*(H[3]-S*XXX);
      Y := XT*(H[4]-S*YYY);
      Z := XT*(H[2]-S*ZZZ);
      F := Sqrt(Sqr(X)+Sqr(Y)+Sqr(Z));
      Brh0 := Y*SP+X*CP;
      Y := Y*CP-X*SP;
      X := Z*ST-Brh0*CT;
      Z := -Z*CT-Brh0*ST;


  FPointChanged := false;
end;







//------------------------------------------------------------------------------
//                                  MagLat
//------------------------------------------------------------------------------
function TGeoField.MagLat: Single;
var
  CBG,YLG: Single;
begin
  CBG := 11.4 * RinD;
  YLG := FLocation.Lon + 69.8 * RinD;
  
  Result := Sin(FLocation.Lat)*Cos(CBG) + Cos(FLocation.Lat)*Cos(YLG)*Sin(CBG);
  Result := SafeArcSin(Result);
end;


function TGeoField.CorrMagLat: Single;
begin
  Result := InterpolateInLatLon(FLocation.Lat, FLocation.Lon, CorMagLatTable);
end;




end.

