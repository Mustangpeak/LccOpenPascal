unit lcc_threaded_circulararray;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  {$IFDEF LCC_FPC}
  syncobjs,
  {$ELSE}
  System.SyncObjs,
  {$ENDIF}
  SysUtils,
  lcc_defines;

type

  TLccCircularArray = array[0..2048] of Byte;

  { TThreadedCircularArray }

  TThreadedCircularArray = class
  private
    FCircularArray: TLccCircularArray;
    FCount: Word;
    FHead: Word;
    FLock: TCriticalSection;
    FTail: Word;
  protected
    property Lock: TCriticalSection read FLock write FLock;
    property CircularArray: TLccCircularArray read FCircularArray write FCircularArray;
    property Head: Word read FHead write FHead;
    property Tail: Word read FTail write FTail;
  public
    property Count: Word read FCount;
    constructor Create;
    destructor Destroy; override;
    procedure LockArray;
    procedure UnLockArray;
    procedure AddChunk(AChunk: TLccDynamicByteArray);
    procedure PullArray(var AChunk: TLccDynamicByteArray);
  end;

  { TCircularArray }

  TCircularArray = class
  private
    FCircularArray: TLccCircularArray;
    FCount: Word;
    FHead: Word;
    FTail: Word;
  protected
    property CircularArray: TLccCircularArray read FCircularArray write FCircularArray;
    property Head: Word read FHead write FHead;
    property Tail: Word read FTail write FTail;
  public
    property Count: Word read FCount;
    procedure AddChunk(AChunk: TLccDynamicByteArray);
    procedure PullArray(var AChunk: TLccDynamicByteArray);
  end;

implementation

{ TCircularArray }

procedure TCircularArray.AddChunk(AChunk: TLccDynamicByteArray);
var
  i: Integer;
begin
   // Do we have room?
   if Length(FCircularArray) - Count > Length(AChunk) then
   begin
     for i := 0 to Length(AChunk) - 1 do
     begin
       FCircularArray[Tail] := AChunk[i];
       Inc(FTail);
       Inc(FCount);
       if Tail >= Length(FCircularArray) then
         Tail := 0;
     end
   end;
end;

procedure TCircularArray.PullArray(var AChunk: TLccDynamicByteArray);
var
  i: Integer;
  LocalCount: Word;
begin
   if Count > 0 then
   begin
     LocalCount := Count;
     SetLength(AChunk, Count);
     for i := 0 to LocalCount - 1 do
     begin
       AChunk[i] := CircularArray[Head];
       Inc(FHead);
       Dec(FCount);
       if Head >= Length(FCircularArray) then
         Head := 0;
     end
   end;
end;

{ TThreadedCircularArray }

procedure TThreadedCircularArray.AddChunk(AChunk: TLccDynamicByteArray);
var
  i: Integer;
begin
  LockArray;
  try
     // Do we have room?
     if Length(FCircularArray) - Count > Length(AChunk) then
     begin
       for i := 0 to Length(AChunk) - 1 do
       begin
         FCircularArray[Tail] := AChunk[i];
         Inc(FTail);
         Inc(FCount);
         if Tail >= Length(FCircularArray) then
           Tail := 0;
       end
     end;
    finally
    UnLockArray;
  end;
end;

constructor TThreadedCircularArray.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TThreadedCircularArray.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TThreadedCircularArray.LockArray;
begin
  FLock.Enter;
end;

procedure TThreadedCircularArray.PullArray(var AChunk: TLccDynamicByteArray);
var
  i: Integer;
  LocalCount: Word;
begin
  LockArray;
  try
     if Count > 0 then
     begin
       LocalCount := Count;
       SetLength(AChunk, Count);
       for i := 0 to LocalCount - 1 do
       begin
         AChunk[i] := CircularArray[Head];
         Inc(FHead);
         Dec(FCount);
         if Head >= Length(FCircularArray) then
           Head := 0;
       end
     end;
    finally
    UnLockArray;
  end;
end;

procedure TThreadedCircularArray.UnLockArray;
begin
  FLock.Leave;
end;

end.

