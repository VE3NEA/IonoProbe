unit Utc;

interface

uses
  Windows, SysUtils;

function UtcNow: TDateTime;
function UtcToLocalTime(Utc: TDateTime): TDateTime;
function LocalTimeToUtc(Local: TDateTime): TDateTime;
function IsoDateTimeToStr(T: TDateTime): string;
function IsoStrToDateTime(S: string): TDateTime;
function GetYearStart(T: TDateTime): TDateTime;
function HttpDateTimeToStr(T: TDateTime): string;
function TimeIntervalToStr(Interval: TDateTime): string;
function TimeMod(Num, Denom: TDateTime): TDateTime;


implementation


function UtcNow: TDateTime;
var
  St: TSystemTime;
begin
  GetSystemTime(St);
  Result := SystemTimeToDateTime(St);
end;



function WeekdayToDate(St: TSystemTime; Year: Word): TDateTime;
begin
  //absolute date
  if St.wYear <> 0 then
      begin
      St.wYear := Year;
      Result := SystemTimeToDateTime(St);
      end

  //last week day of the month
  else if St.wDay = 5 then
    begin
    //1-st day of next month
    Result := IncMonth(EncodeDate(Year, St.wMonth, 1), 1);
    repeat Result := Result - 1 until DayOfWeek(Result) = (St.wDayOfWeek + 1); 
    //add time
    Result := Result + EncodeTime(St.wHour, St.wMinute, 0, 0);
    end

  //n-th week day
  else
    begin
    //1-st day of month
    Result := EncodeDate(Year, St.wMonth, 1);
    //1-st week day in this month month
    while DayOfWeek(Result) <> (St.wDayOfWeek + 1) do Result := Result + 1;
    //n-th weekday
    Result := Result + (St.wDay - 1) * 7;
    //add time
    Result := Result + EncodeTime(St.wHour, St.wMinute, 0, 0);
    end;
end;


function UtcToLocalTime(Utc: TDateTime): TDateTime;
var
  Year, m, d: Word;
  TzInfo: TTimeZoneInformation;
  DstStart, DstEnd: TDateTime;
  Dst: boolean;
begin
  //get year from the input date
  DecodeDate(Utc, Year, m, d);
  //get TZ info
  GetTimeZoneInformation(TzInfo);
  //Is DST used in this TZ?
  Dst := (TzInfo.StandardDate.wMonth <> 0) and (TzInfo.DaylightDate.wMonth <> 0);

  if Dst then
    begin
    //calculate DST range
    DstStart := WeekdayToDate(TzInfo.DaylightDate, Year);
    DstEnd := WeekdayToDate(TzInfo.StandardDate, Year);
    //check if date in range
    if DstStart < DstEnd //Northern Hemisphere
      then Dst := (Utc >= DstStart) and (Utc < DstEnd)
      else Dst := (Utc >= DstStart) or (Utc < DstEnd);
    end;

  //convert
  if Dst
    then Result := Utc - (TzInfo.Bias + TzInfo.DaylightBias) / 1440
    else Result := Utc - (TzInfo.Bias + TzInfo.StandardBias) / 1440;
end;



function LocalTimeToUtc(Local: TDateTime): TDateTime;
var
  Year, m, d: Word;
  TzInfo: TTimeZoneInformation;
  DstStart, DstEnd: TDateTime;
  Dst: boolean;
begin
  //get year from the input date
  DecodeDate(Local, Year, m, d);
  //get TZ info
  GetTimeZoneInformation(TzInfo);
  //Is DST used in this TZ?
  Dst := (TzInfo.StandardDate.wMonth <> 0) and (TzInfo.DaylightDate.wMonth <> 0);

  if Dst then
    begin
    //calculate DST range
    DstStart := WeekdayToDate(TzInfo.DaylightDate, Year);
    DstEnd := WeekdayToDate(TzInfo.StandardDate, Year);
    //DST range to local time
    DstStart := DstStart - (TzInfo.Bias + TzInfo.StandardBias) / 1440;
    DstEnd := DstEnd - (TzInfo.Bias + TzInfo.DaylightBias) / 1440;

    //check if date in range
    if DstStart < DstEnd //Northern Hemisphere
      then Dst := (Local >= DstStart) and (Local < DstEnd)
      else Dst := (Local >= DstStart) or (Local < DstEnd);
    end;

  //convert
  if Dst
    then Result := Local + (TzInfo.Bias + TzInfo.DaylightBias) / 1440
    else Result := Local + (TzInfo.Bias + TzInfo.StandardBias) / 1440;
end;


function IsoDateTimeToStr(T: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', T);
end;


function IsoStrToDateTime(S: string): TDateTime;
var
  OldDateSeparator: Char;
  OldShortDateFormat: string;
  OldTimeSeparator: Char;
  OldShortTimeFormat: string;
begin
  if S = '' then begin Result := 0; Exit; end;

  if UpperCase(Copy(S, Length(S)-2, 3)) = 'UTC'
    then S := Trim(Copy(S, 1, Length(S)-3));

  OldDateSeparator := DateSeparator;
  OldShortDateFormat := ShortDateFormat;
  OldTimeSeparator := TimeSeparator;
  OldShortTimeFormat := ShortTimeFormat;
  try
    DateSeparator := '-';
    ShortDateFormat := 'yyyy-mm-dd';
    TimeSeparator := ':';
    ShortTimeFormat := 'hh:nn:ss';

    try Result := StrToDateTime(S); except Result := 0; end;
  finally
    DateSeparator := OldDateSeparator;
    ShortDateFormat := OldShortDateFormat;
    TimeSeparator := OldTimeSeparator;
    ShortTimeFormat := OldShortTimeFormat;
  end;
end;



function GetYearStart(T: TDateTime): TDateTime;
var
  y,m,d: WORD;
begin
  DecodeDate(T, y, m, d);
  Result := EncodeDate(y, 1, 1);
end;


//Sat, 09 Oct 1994 19:43:31 GMT
function HttpDateTimeToStr(T: TDateTime): string;
const
  MonthName: array[1..12] of string =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  DayName: array[1..7] of string =
    ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
var
  y,m,d, h,n,s,f: WORD;
begin
  DecodeDate(T, y, m, d);
  DecodeTime(T, h, n, s, f);
  Result := Format('%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT',
    [DayName[DayOfWeek(T)], d, MonthName[m], y, h, n, s]);
end;


function TimeIntervalToStr(Interval: TDateTime): string;
var
  Hours: integer;
begin
  try
    if Interval < 0 then Abort;
    Hours := Trunc(Interval*24);
    Result := FormatDateTime('nn:ss', (Interval-Hours/24));
    if Hours > 0 then  Result := IntToStr(Hours) + ':' + Result;
  except
    Result := '';
  end;
end;


function TimeMod(Num, Denom: TDateTime): TDateTime;
begin
  if Denom = 0
    then Result := 0
    else Result := Num - Denom * Trunc(Num / Denom);
end;


end.
