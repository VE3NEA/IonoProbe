unit PathGeom;

interface

uses
  SysUtils, Classes, Math, MyMath;

type
  TPathGeometry = class
  private
    FDist: Single;
    FAzim: Single;
    FTx: TGeoPoint;
    FRx: TGeoPoint;
    FLongPath: boolean;

    SnTx, CsTx, SnRx, CsRx, SnL, CsL: Double;

    procedure SetTx(const Value: TGeoPoint);
    procedure SetRx(const Value: TGeoPoint);
    procedure SetAzim(const Value: Single);
    procedure SetDist(const Value: Single);
    procedure SetLongPath(const Value: boolean);

    procedure CalcSinCos;
    procedure CalcAzDist;
    procedure CalcRxPos;
    function GetBackAzim: Single;
  public
    procedure SetTxRx(ATx, ARx: TGeoPoint; LongPath: boolean);
    procedure SetAzDist(Azim, Dist: Single);

    property LongPath: boolean read FLongPath write SetLongPath;
    property Tx: TGeoPoint read FTx write SetTx;
    property Rx: TGeoPoint read FRx write SetRx;
    property Azim: Single read FAzim write SetAzim;
    property Dist: Single read FDist write SetDist;
    property BackAzim: Single read GetBackAzim;
  end;


implementation



{ TPathGeometry }

//------------------------------------------------------------------------------
//                                get/set
//------------------------------------------------------------------------------
procedure TPathGeometry.SetTxRx(ATx, ARx: TGeoPoint; LongPath: boolean);
begin
  FTx := ATx;
  FRx := ARx;
  FLongPath := LongPath;
  CalcSinCos;
  CalcAzDist;
end;

procedure TPathGeometry.SetTx(const Value: TGeoPoint);
begin
  FTx := Value;
  CalcSinCos;
  CalcAzDist;
end;

procedure TPathGeometry.SetRx(const Value: TGeoPoint);
begin
  FRx := Value;
  CalcSinCos;
  CalcAzDist;
end;

procedure TPathGeometry.SetAzDist(Azim, Dist: Single);
begin
  FAzim := Azim;
  FDist := Dist;
  CalcRxPos;
  CalcSinCos;
end;

procedure TPathGeometry.SetAzim(const Value: Single);
begin
  FAzim := Value;
  CalcRxPos;
  CalcSinCos;
end;

procedure TPathGeometry.SetDist(const Value: Single);
begin
  FDist := Value;
  CalcRxPos;
  CalcSinCos;
end;


procedure TPathGeometry.SetLongPath(const Value: boolean);
begin
  if Value = FLongPath then Exit;
  FLongPath := Value;
  CalcAzDist;
end;





//------------------------------------------------------------------------------
//                               calculate
//------------------------------------------------------------------------------
procedure TPathGeometry.CalcSinCos;
begin
  SnTx := Sin(FTx.Lat);
  CsTx := Cos(FTx.Lat);
  SnRx := Sin(FRx.Lat);
  CsRx := Cos(FRx.Lat);
  SnL  := Sin(FRx.Lon - FTx.Lon);
  CsL  := Cos(FRx.Lon - FTx.Lon);
end;


procedure TPathGeometry.CalcAzDist;
begin

  FDist := SafeArcCos(CsTx * CsRx * CsL + SnTx * SnRx);

  //Rx near Tx
  if FDist < SMALL_VALUE then FAzim := 0
  //Tx near North Pole
  else if Abs(FTx.Lat - HALF_PI) < SMALL_VALUE
    then FAzim := Pi + FTx.Lon - FRx.Lon
  //Tx near South Pole
  else if Abs(FTx.Lat + HALF_PI) < SMALL_VALUE
    then FAzim := FRx.Lon - FTx.Lon
  //Other
  else FAzim := SafeArcTan(SnL * CsRx, CsTx * SnRx - SnTx * CsRx * CsL);


  //long path
  if FLongPath then
    begin
    FAzim := FAzim + Pi;
    FDist := TWO_PI - FDist;
    end;

  FAzim := Adjust2Pi(FAzim);
end;


function TPathGeometry.GetBackAzim: Single;
begin
  //Rx near Tx
  if FDist < SMALL_VALUE then Result := Pi
  //Rx near North Pole
  else if Abs(FRx.Lat - HALF_PI) < SMALL_VALUE
    then Result := Pi + FRx.Lon - FTx.Lon
  //Rx near South Pole
  else if Abs(FRx.Lat + HALF_PI) < SMALL_VALUE
    then Result := FTx.Lon - FRx.Lon
  //Other
  else Result := SafeArcTan(-SnL * CsTx, CsRx * SnTx - SnRx * CsTx * CsL);

  //long path
  if FLongPath then Result := Result + Pi;

  Result := Adjust2Pi(Result);
end;


procedure TPathGeometry.CalcRxPos;
var
  Cs, Sn: Single;
begin
  Sn := Sin(FTx.Lat);
  Cs := Cos(FTx.Lat);

  //Az, Dist -> Lat, Lon
  FRx.Lat := SafeArcSin(Sin(FDist) * Cs * Cos(FAzim) + Cos(FDist) * Sn);

  //Tx near North Pole
  if Abs(FTx.Lat - HALF_PI) < SMALL_VALUE then FRx.Lon := Pi - FAzim
  //Tx near South Pole
  else if Abs(FTx.Lat + HALF_PI) < SMALL_VALUE then FRx.Lon := FAzim
  //Other
  else FRx.Lon := SafeArcTan(Sin(FAzim) * Sin(FDist) * Cs,
                             Cos(FDist) - Sn * Sin(FRx.Lat));

  FRx.Lon := AdjustPi(FRx.Lon + FTx.Lon);
end;



end.

