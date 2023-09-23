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
  lcc_alias_server,
  lcc_node_messages;

type


{ TReceiveMessageAliasServerThread }

TReceiveMessageAliasServerThread = class(TThread)
  private
    FOutgoingProcessedMessageList: TThreadList;
    FIncomingMessageList: TThreadList;
    FReceivedMessage: TLccMessage;
    FWaitingForMappingMessageList: TThreadList;
    FWorkerMessage: TLccMessage;
  protected
    property ReceivedMessage: TLccMessage read FReceivedMessage write FReceivedMessage;
    property IncomingMessageList: TThreadList read FIncomingMessageList write FIncomingMessageList;
    property WaitingForMappingMessageList: TThreadList read FWaitingForMappingMessageList write FWaitingForMappingMessageList;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    procedure Execute; override;
    procedure ClearOutgoingProcessedList;
    procedure ClearIncomingList;
    procedure ClearWaitingForMappingList;
    procedure ProcessAliasAndNodeIDMessages(AMessage: TLccMessage);
    procedure RequestMappingCallback(ANodeID: TNodeID; AnAliasID: Word);


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
var
  List: TList;
  i: Integer;
  LocalMessage: TLccMessage;
begin

  while not Terminated do
  begin
    // Any of this will ONLY run if in during the AddIncomingMessage call it was defined as a GridConnect so
    // the assumption here is ONLY gridconnect with Alias's are here.

    List := IncomingMessageList.LockList;
    try
      for i := 0 to List.Count -1 do
      begin
        LocalMessage := TLccMessage( List[i]);
        ProcessAliasAndNodeIDMessages(LocalMessage);
        if LocalMessage.ValidateAliasMappings({$IFNDEF LCC_DELPHI}@{$ENDIF}RequestMappingCallback) then
          OutgoingProcessedMessageList.Add(LocalMessage)
        else
          WaitingForMappingMessageList.Add(LocalMessage)

      end;
    finally
      IncomingMessageList.Clear;
      IncomingMessageList.UnlockList;
    end;

    Sleep(50);
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

procedure TReceiveMessageAliasServerThread.ClearWaitingForMappingList;
var
  List: TList;
  i: Integer;
begin
  List := WaitingForMappingMessageList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    WaitingForMappingMessageList.UnlockList;
  end;
end;

procedure TReceiveMessageAliasServerThread.ProcessAliasAndNodeIDMessages(AMessage: TLccMessage);
var
  DummyNodeID: TNodeID;
begin
  case AMessage.CAN_MTI of
    MTI_CAN_AMR :
      begin
        AliasServer.MarkForRemovalByAlias(AMessage.SourceAlias);
      end;
    MTI_CAN_AMD :
      begin
        DummyNodeID := NULL_NODE_ID;
        AliasServer.AddMapping(AMessage.ExtractDataBytesAsNodeID(0, DummyNodeID), AMessage.SourceAlias);
      end;
    MTI_CAN_AME :
      begin
        if AMessage.DataCount = 0 then      // A global AME will repoplulate the entire database so this will flush invalid mappings as well since they won't respond anymore
          AliasServer.Clear;
      end;
  end;

  case AMessage.MTI of
    MTI_VERIFIED_NODE_ID_NUMBER,
    MTI_INITIALIZATION_COMPLETE :
      begin
        DummyNodeID := NULL_NODE_ID;
        AliasServer.AddMapping(AMessage.ExtractDataBytesAsNodeID(0, DummyNodeID), AMessage.SourceAlias);
      end;
    MTI_VERIFY_NODE_ID_NUMBER :
      begin
        if AMessage.DataCount = 0 then     // A global Verify Node ID will repoplulate the entire database so this will flush invalid mappings as well since they won't respond anymore
          AliasServer.Clear;
      end;
  end;
end;

procedure TReceiveMessageAliasServerThread.RequestMappingCallback(ANodeID: TNodeID; AnAliasID: Word);
begin
//  NEED TO SEND MESSAGES FOR MAPPINGS HERE.......
end;

constructor TReceiveMessageAliasServerThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FOutgoingProcessedMessageList := TThreadList.Create;
  FIncomingMessageList := TThreadList.Create;
  FWaitingForMappingMessageList := TThreadList.Create;
end;

destructor TReceiveMessageAliasServerThread.Destroy;
begin
  ClearWaitingForMappingList;
  ClearOutgoingProcessedList;
  ClearIncomingList;
  FreeAndNil(FOutgoingProcessedMessageList);
  FreeAndNil(FIncomingMessageList);
  FreeAndNil(FWaitingForMappingMessageList);
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

