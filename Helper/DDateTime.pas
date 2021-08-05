unit DDateTime;
interface
uses
  System.DateUtils,SysUtils, System.Classes;

type
  CDateTime = Class

  private

  public
      constructor Create();
      function StrToDate(DateTxt: string): TDate;
      function TimeStampToDate(DateTxt: string): TDate;
      function StrToDateTime(DateTxt: String): TDateTime;
      function CalculateAge(DateTxt: String) : Integer;
      function UTCtoDatetime(DateTxt: string) : TDateTime;
      function CalculateTime(DateTxt:TDateTime;TypeCalculate:String;Value:Integer):TDateTime;
      function TradeTimeToDate(DateTxt: string) : String;
      function ThaiDate(DateTxt: string) : string;
      function ContentfulStrToDatetime(DateTxt:String):TDateTime;
  End;
implementation

function CDateTime.CalculateTime(DateTxt: TDateTime; TypeCalculate: String;
  Value: Integer): TDateTime;
begin
if TypeCalculate = 'plushour' then
begin
  DateTxt:=IncHour(DateTxt, Value);
end;
if TypeCalculate = 'minushour' then
begin
  DateTxt:=IncHour(DateTxt, -Value);
end;
Result:=DateTxt;
end;

function CDateTime.ContentfulStrToDatetime(DateTxt: String): TDateTime;
var Year, Month, Day, Hour, Minute, Second, Millisecond: Word;
begin
  Year        := StrToInt(Copy(DateTxt, 1, 4));
  Month       := StrToInt(Copy(DateTxt, 6, 2));
  Day         := StrToInt(Copy(DateTxt, 9, 2));
  Hour        := StrToInt(Copy(DateTxt, 12, 2));
  Minute      := StrToInt(Copy(DateTxt, 15, 2));
  Second      := Round(0000);
  Millisecond := Round(0000);
  Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Millisecond);
end;

constructor CDateTime.Create();
 begin
 end;

function CDateTime.CalculateAge(DateTxt: string): Integer;
var
  NowYear, NowMonth, NowDay: Word;
  Year, Month, Day: Word;
  YearsOld: Word;
begin
  if DateTxt <> '' then
  begin
    DecodeDate(Now, NowYear, NowMonth, NowDay);
    DecodeDate(StrToDate(DateTxt), Year, Month, Day);
    if NowMonth>Month then
      YearsOld:=NowYear-Year;
    if NowMonth<Month then
      YearsOld:=NowYear-Year-1;
    if NowMonth=Month then
    begin
      if NowDay>=Day then
        YearsOld:=NowYear-Year;
      if NowDay<=Day then
        YearsOld:=NowYear-Year-1;
    end;
    Result:=YearsOld;
  end;
end;

function CDateTime.StrToDate(DateTxt: String): TDate;
var Year, Month, Day : Word;
begin
  if DateTxt <> '' then
    begin
      Year        := StrToInt(Copy(DateTxt, 7, 4));
      Month       := StrToInt(Copy(DateTxt, 4, 2));
      Day         := StrToInt(Copy(DateTxt, 1, 2));
      Result := EncodeDate(Year, Month, Day);
    end
  else
    begin
      Result := Date;
    end;
end;

function CDateTime.TimeStampToDate(DateTxt: string): TDate;
var Year, Month, Day, Hour, Minute, Second, Millisecond: Word;
begin
  if DateTxt <> '' then
    begin
      Year        := StrToInt(Copy(DateTxt, 1, 4));
      Month       := StrToInt(Copy(DateTxt, 5, 2));
      Day         := StrToInt(Copy(DateTxt, 7, 2));
      Result := EncodeDate(Year, Month, Day);
    end
  else
    begin
      Result := Date;
    end;
end;

function CDateTime.ThaiDate(DateTxt: string) : string;
var Day,Month,Year : Integer;
monthList:TStringList;
begin
  if DateTxt <> '' then
    begin
      Year        := StrToInt(Copy(DateTxt, 7, 4));
      Month       := StrToInt(Copy(DateTxt, 4, 2));
      Day         := StrToInt(Copy(DateTxt, 1, 2));
      monthList:=TStringList.Create;
      monthList.Delimiter:=',';
      monthList.DelimitedText :='มกราคม,กุมภาพันธ์,มีนาคม,เมษายน,พฤษภาคม,มิถุนายน,กรกฎาคม,สิงหาคม,กันยายน,ตุลาคม,พฤศจิกายน,ธันวาคม';
      Result := Concat(IntToStr(Day),' ',monthList[Month-1],' ',IntToStr(Year+543));
    end
  else
    begin
      Result := '';
    end;
end;

function CDateTime.UTCtoDatetime(DateTxt: string): TDateTime;
var Year, Month, Day, Hour, Minute, Second, Millisecond: Word;
NewDate:TDate;
begin
  if DateTxt <> '' then
    begin
      Year        := StrToInt(Copy(DateTxt, 1, 4));
      Month       := StrToInt(Copy(DateTxt, 6, 2));
      Day         := StrToInt(Copy(DateTxt, 9, 2));
      Hour        := StrToInt(Copy(DateTxt, 12, 2));
      Minute      := StrToInt(Copy(DateTxt, 15, 2));
      Second      := Round(00);
      Millisecond := Round(0000);
      Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Millisecond);
    end
  else
    begin
      Result := now();
    end;
end;

function CDateTime.TradeTimeToDate(DateTxt: string): String;
var Year, Month, Day, Hour, Minute, Second, Millisecond: Word;
NewDate,UtcDate:TDate;
begin
  if DateTxt <> '' then
    begin
      Year        := StrToInt(Copy(DateTxt, 1, 4));
      Month       := StrToInt(Copy(DateTxt, 6, 2));
      Day         := StrToInt(Copy(DateTxt, 9, 2));
      Hour        := StrToInt(Copy(DateTxt, 12, 2));
      Minute      := StrToInt(Copy(DateTxt, 15, 2));
      Second      := Round(00);
      Millisecond := Round(0000);
      NewDate := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Millisecond);
      Result := FormatDateTime('dd/MM/yyyy hh:mm',UtcDate);
    end
  else
    begin
      Result := '';
    end;
end;

function CDateTime.StrToDateTime(DateTxt: String): TDateTime;
var Year, Month, Day, Hour, Minute, Second, Millisecond: Word;
begin
  Year        := StrToInt(Copy(DateTxt, 1, 4));
  Month       := StrToInt(Copy(DateTxt, 5, 2));
  Day         := StrToInt(Copy(DateTxt, 7, 2));
  Hour        := StrToInt(Copy(DateTxt, 9, 2));
  Minute      := StrToInt(Copy(DateTxt, 11, 2));
  Second      := StrToInt(Copy(DateTxt, 13, 2));
  Millisecond := Round(0000);

  Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Millisecond);
end;

end.
