unit TgzInMem_D5;

interface

uses
  Windows, SysUtils, Classes, Math, ZlibEx;

type
  TFileEntry = record
    Name: string;
    Size: integer;
    Data: PChar;
    end;

  TTgzReader = class
  private
    FGzipString: string;
    FTarString: string;
    FFiles: array of TFileEntry;
    function GetFiles(Idx: integer): TFileEntry;
    procedure SetGzipString(const Value: string);
    procedure SetTarString(const Value: string);
    function GetFileCount: integer;

    function OctToSize(S: array of Char): integer;
    function EvenBlocks(Size: integer): integer;

    function UnGZip(S: string): string;
    procedure ScanTar;
  public
    procedure Clear;

    property GzipString: string read FGzipString write SetGzipString;
    property TarString: string read FTarString write SetTarString;
    property FileCount: integer read GetFileCount;
    property Files[Idx: integer]: TFileEntry read GetFiles; default;
  end;

  TZAlloc = function (opaque: Pointer; items, size: Integer): Pointer;
  TZFree  = procedure (opaque, block: Pointer);
  TZStreamRec = packed record
    next_in  : PChar;     // next input byte
    avail_in : Longint;   // number of bytes available at next_in
    total_in : Longint;   // total nb of input bytes read so far

    next_out : PChar;     // next output byte should be put here
    avail_out: Longint;   // remaining free space at next_out
    total_out: Longint;   // total nb of bytes output so far

    msg      : PChar;     // last error message, NULL if no error
    state    : Pointer;   // not visible by applications

    zalloc   : TZAlloc;   // used to allocate the internal state
    zfree    : TZFree;    // used to free the internal state
    opaque   : Pointer;   // private data object passed to zalloc and zfree

    data_type: Integer;   // best guess about the data type: ascii or binary
    adler    : Longint;   // adler32 value of the uncompressed data
    reserved : Longint;   // reserved for future use
  end;

implementation

{ TTgzReader }


//------------------------------------------------------------------------------
//                              interface
//------------------------------------------------------------------------------
procedure TTgzReader.Clear;
begin
  FFiles := nil;
  FGzipString := '';
  FTarString := '';
end;

function TTgzReader.GetFileCount: integer;
begin
  Result := Length(FFiles);
end;

function TTgzReader.GetFiles(Idx: integer): TFileEntry;
begin
  Result := FFiles[Idx];
end;

procedure TTgzReader.SetGzipString(const Value: string);
begin
  Clear;
  FGzipString := Value;
  FTarString := UnGZip(FGZipString);
  ScanTar;
end;

procedure TTgzReader.SetTarString(const Value: string);
begin
  Clear;
  FTarString := Value;
  ScanTar;
end;



//------------------------------------------------------------------------------
//                                  GZIP
//------------------------------------------------------------------------------
const
  //signature
  GZIP_MAGIC     = $8B1F;
  OLD_GZIP_MAGIC = $9E1F;
  //compression methods
  mcSTORED      = 0;
  mcDEFLATED    = 8;
  //gzip flags
  ASCII_FLAG   = $01;  //file probably ascii text
  FHCRC        = $02;  //CRC16 present
  EXTRA_FIELD  = $04;  //extra field present
  ORIG_NAME    = $08;  //original file name present
  COMMENT      = $10;  //file comment present
  ENCRYPTED    = $20;  //file is encrypted
  RESERVED     = $C0;  //reserved


type
  PGZipHeader = ^TGZipHeader;
  TGZipHeader = packed record
    Magic: WORD;
    Method: byte;
    Flags: byte;
    MTime: DWORD;
    Xflags: byte;
    OS: byte;
  end;



function TTgzReader.UnGZip(S: string): string;
const
  BUF_SIZE = 10 * 1024;
var
  BytePos: integer;
  Buf: string;
  strm: TZStreamRec;
  rc: integer;
  Fi, Fo: TStringStream;
  Fz: TZDecompressionStream;
begin
  Result := '';

  with PGZipHeader(@FGZipString[1])^ do
    begin
    //verify

    if Length(S) < SizeOf(TGZipHeader) then Exit;
    if Magic <> GZIP_MAGIC then Exit;
    if (Flags and (ENCRYPTED or RESERVED)) <> 0 then Exit;
    if Method <> mcDEFLATED then Exit;

    //skip trash

    BytePos := SizeOf(TGZipHeader) + 1;

    if (Flags and EXTRA_FIELD) <> 0 then
      if BytePos >= Length(S)
        then Exit else Inc(BytePos, SizeOf(Word) + PWORD(@S[BytePos])^);

    if (Flags and ORIG_NAME) <> 0 then
      if BytePos > Length(S)
        then Exit else Inc(BytePos, StrLen(@S[BytePos]) + 1{null terminator});

    if (Flags and COMMENT) <> 0 then
      if BytePos > Length(S)
        then Exit else Inc(BytePos, StrLen(@S[BytePos]) + 1);

    if (Flags and FHCRC) <> 0 then
      if BytePos > Length(S)
        then Exit else Inc(BytePos, SizeOf(WORD));

    if BytePos > Length(S) then Exit;


    //deflate

    Fi := TStringStream.Create(Copy(S, BytePos, MAXINT));
    Fz := TZDecompressionStream.Create(Fi, -15);
    Fo := TStringStream.Create('');
    try
      Fo.CopyFrom(Fz, 0);
      Result := Fo.DataString;
    finally
      Fz.Free;
      Fo.Free;
      Fi.Free;
    end;

    end;
end;




//------------------------------------------------------------------------------
//                                  TAR
//------------------------------------------------------------------------------
const
  TAR_BLOCK_SIZE = 512;


type
  PTarHeader = ^TTarHeader;
  TTarHeader = packed record
    Name     : array [0..99] of Char;
    Mode     : array [0..7]  of Char;
    UID      : array [0..7]  of Char;
    GID      : array [0..7]  of Char;
    Size     : array [0..11] of Char;
    MTime    : array [0..11] of Char;
    ChkSum   : array [0..7]  of Char;
    LinkFlag : Char;
    LinkName : array [0..99] of Char;
    Magic    : array [0..7]  of Char;
    UName    : array [0..31] of Char;
    GName    : array [0..31] of Char;
    DevMajor : array [0..7]  of Char;
    DevMinor : array [0..7]  of Char;
    end;




function TTgzReader.OctToSize(S: array of Char): integer;
var
  i: integer;
begin
  Result := 0;
  for i:=0 to 11 do
    if S[i] in ['0'..'7']
      then Result := (Result shl 3) or (Ord(S[i]) - Ord('0')) else Exit;
end;



function TTgzReader.EvenBlocks(Size: integer): integer;
begin
  Result := Ceil(Size / TAR_BLOCK_SIZE) * TAR_BLOCK_SIZE;
end;


procedure TTgzReader.ScanTar;
var
  BytePos: integer;
  FileCnt: integer;
  Hdr: PTarHeader;
begin
  SetLength(FFiles, 1000);
  FileCnt := 0;

  BytePos := 1;
  while BytePos <= (Length(FTarString) - TAR_BLOCK_SIZE + 1) do
    begin
    Hdr := PTarHeader(@FTarString[BytePos]);
    if Hdr^.Name[0] = #0 then Break;
    if not (Hdr.LinkFlag in [#0,'0','3','4','7']) then Exit;

    with FFiles[FileCnt] do
      begin
      Name := string(Hdr.Name);
      Size := OctToSize(Hdr.Size);
      Inc(BytePos, TAR_BLOCK_SIZE);
      Data := @FTarString[BytePos];
      Inc(BytePos, EvenBlocks(Size));
      end;
    Inc(FileCnt);
    end;

  SetLength(FFiles, FileCnt);
end;



end.







