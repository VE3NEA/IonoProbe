unit Interpol;

interface

uses
  SysUtils;

type
  TInterpolationFrame = record
    Year1, Month1: WORD;
    Year2, Month2: WORD;
    dt: Single;
    end;


procedure FillFrame(var AFrame: TInterpolationFrame; AUtc: TDateTime);



implementation

function Sgn(i: integer): integer;
begin
  if i > 0 then Result := 1
  else if i < 0 then Result := -1
  else Result := 0;
end;

procedure FillFrame(var AFrame: TInterpolationFrame; AUtc: TDateTime);
var
  Day: WORD;             //day component of the date
  Dat1, Dat2: TDateTime; //mid of the month
  MidDay: integer;       //mid day of the month
begin
  with AFrame do
    begin
    DecodeDate(AUtc, Year1, Month1, Day);
    if Month1 = 2 then MidDay := 14 else MidDay := 15;
    Dat1 := EncodeDate(Year1, Month1, MidDay);

    Dat2 := IncMonth(AUtc, Sgn(Day-MidDay));

    DecodeDate(Dat2, Year2, Month2, Day);
    if Month2 = 2 then MidDay := 14 else MidDay := 15;
    Dat2 := EncodeDate(Year2, Month2, MidDay);

    if Dat2 = Dat1
      then dt := 0
      else dt := (AUtc - Dat1) / (Dat2 - Dat1);
    end;
end;



end.

