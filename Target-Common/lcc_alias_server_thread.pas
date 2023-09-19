unit lcc_alias_server_thread;


{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses

  Classes,
  SysUtils,
  {$IFNDEF LCC_FPC}
    System.Types,
    FMX.Types,
    System.Generics.Collections,
  {$ENDIF}
  {$IFDEF WEB_APP}
    generics.collections,
  {$ENDIF}
  lcc_defines,
  lcc_utilities,
  lcc_node_messages;

type

{ TReceiveMessageAliasServerThread }

TReceiveMessageAliasServerThread = class(TThread)
  private
    FInProcessMessageList: TThreadList;
    FOutgoingProcessedMessageList: TThreadList;
    FIncomingMessageList: TThreadList;
 //   FOwner: TLccNodeManager;
    FReceivedMessage: TLccMessage;
    FWorkerMessage: TLccMessage;
  protected
    property ReceivedMessage: TLccMessage read FReceivedMessage write FReceivedMessage;
    property IncomingMessageList: TThreadList read FIncomingMessageList write FIncomingMessageList;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property InProcessMessageList: TThreadList read FInProcessMessageList write FInProcessMessageList;

    procedure Execute; override;
    procedure ClearOutgoingProcessedList;
    procedure ClearIncomingList;
    procedure ClearInProcessList;
//    procedure ReceiveMessageSyncronize;   // Syncronize Method called in context of main thread, this thread is stalled during that time
//    procedure UpdateGlobalMappings(AMessage: TLccMessage); // Called in context of thread
 //   procedure NodeIdentificationToCallbackProc(ANodeIdentification: TLccNodeIdentificationObject); // Called in context of thread

  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize); reintroduce;
    destructor Destroy; override;

    property OutgoingProcessedMessageList: TThreadList read FOutgoingProcessedMessageList;

    procedure AddIncomingMessage(AMessage: TLccMessage; GridConnect: Boolean);

  end;

var
  AliasServerThread: TReceiveMessageAliasServerThread;

implementation


{ TReceiveMessageAliasServerThread }

procedure TReceiveMessageAliasServerThread.Execute;
begin

  while not Terminated do
  begin
    // Any of this will ONLY run if in during the AddIncomingMessage call it was defined as a GridConnect so
    // the assumption here is ONLY gridconnect with Alias's are here.

  end;
end;

procedure TReceiveMessageAliasServerThread.ClearOutgoingProcessedList;
var
  List: TList;
  i: Integer;
begin
  List := OutgoingProcessedMessageList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    OutgoingProcessedMessageList.UnlockList;
  end;
end;

procedure TReceiveMessageAliasServerThread.ClearIncomingList;
var
  List: TList;
  i: Integer;
begin
  List := IncomingMessageList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    IncomingMessageList.UnlockList;
  end;
end;

procedure TReceiveMessageAliasServerThread.ClearInProcessList;
var
  List: TList;
  i: Integer;
begin
  List := InProcessMessageList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    InProcessMessageList.UnlockList;
  end;
end;

constructor TReceiveMessageAliasServerThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FOutgoingProcessedMessageList := TThreadList.Create;
  FIncomingMessageList := TThreadList.Create;
  FInProcessMessageList := TThreadList.Create;
end;

destructor TReceiveMessageAliasServerThread.Destroy;
begin
  ClearIncomingList;
  ClearOutgoingProcessedList;
  ClearInProcessList;
  FreeAndNil(FOutgoingProcessedMessageList);
  FreeAndNil(FIncomingMessageList);
  FreeAndNil(FInProcessMessageList);
  inherited Destroy;
end;

procedure TReceiveMessageAliasServerThread.AddIncomingMessage(AMessage: TLccMessage; GridConnect: Boolean);
begin
  // No such thing as an Alias if not dealing with GridConnect
  if GridConnect then
    IncomingMessageList.Add(AMessage.Clone)
  else
    OutgoingProcessedMessageList.Add(AMessage.Clone);
end;

initialization
  AliasServerThread := TReceiveMessageAliasServerThread.Create(False);
  AliasServerThread.FreeOnTerminate := True;

finalization
  AliasServerThread.Terminate;


end.

