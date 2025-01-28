unit SsnHist;

interface

uses
  Windows, SysUtils, Classes, Interpol, Math;

type
  TSsnArchive = class
  private
    MonthB: integer;
    RzArr, IgArr: array of Single;
    FUtc: TDateTime;
    FRz: Single;
    FIg: Single;

    procedure LoadData;
    procedure SetUtc(const Value: TDateTime);
  public
    constructor Create;

    property Utc: TDateTime read FUtc write SetUtc;
    property Ig: Single read FIg;
    property Rz: Single read FRz;
  end;


function IgFromRz(ARz: Single): Single;
function SfiFromRz(ARz: Single): Single;





implementation

function IgFromRz(ARz: Single): Single;
begin
  Result := Min(-12.349154 + (1.4683266 - 2.67690893e-03 * ARz) * ARz, 174);
end;

function SfiFromRz(ARz: Single): Single;
begin
  Result := 63.75 + ARz * (0.728 + ARz * 0.00089);
end;



{ TSsnArchive }

constructor TSsnArchive.Create;
begin
  LoadData;
end;


procedure TSsnArchive.LoadData;
var
  Cnt: integer;
  MonthE: integer;
begin
  with TResourceStream.Create(0, 'RZ_IG', RT_RCDATA) do
    try
      //read date range
      ReadBuffer(MonthB, SizeOf(Integer));
      ReadBuffer(MonthE, SizeOf(Integer));
      Cnt := MonthE - MonthB + 1;
      //read Ig data
      SetLength(IgArr, Cnt);
      ReadBuffer(IgArr[0], Cnt * SizeOf(Single));
      //read Rz data
      SetLength(RzArr, Cnt);
      ReadBuffer(RzArr[0], Cnt * SizeOf(Single));
    finally Free; end;
end;


procedure TSsnArchive.SetUtc(const Value: TDateTime);
var
  UtcDay: TDateTime;
  Frame: TInterpolationFrame;
  Idx1, Idx2: integer;
begin
  UtcDay := Trunc(Value);
  if UtcDay = FUtc then Exit;

  //calculate interpolation parameters
  FillFrame(Frame, UtcDay);
  Idx1 := (Frame.Year1)*12 + Frame.Month1 - MonthB;
  Idx2 := (Frame.Year2)*12 + Frame.Month2 - MonthB;

  //validate
  if (Idx1 < 0) or (Idx1 > High(RzArr)) or (Idx2 < 0) or (Idx2 > High(RzArr))
    then raise Exception.Create('No SSN data for ' + DateToStr(UtcDay));

  //interpolate
  FRz := RzArr[Idx1] * (1 - Frame.dt) + RzArr[Idx2] * Frame.dt;
  FIg := IgArr[Idx1] * (1 - Frame.dt) + IgArr[Idx2] * Frame.dt;
  FUtc := UtcDay;
end;



end.

