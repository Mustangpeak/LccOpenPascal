unit lcc_alias_server_thread;


{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{.$DEFINE LOG_MAPPING}

interface

uses

  Classes,
  SysUtils,
  {$IFNDEF LCC_FPC}
    System.Types,
    FMX.Types,
    System.Generics.Collections,
  {$ELSE}
  {$IFDEF LOG_MAPPING}lazlogger,{$ENDIF}
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
    FMappingRequestMessageList: TThreadList;
    FOutgoingProcessedMessageList: TThreadList;
    FIncomingMessageList: TThreadList;
    FReceivedMessage: TLccMessage;
    FWaitingForMappingMessageList: TThreadList;
    FWorkerMessage: TLccMessage;
  protected
    property ReceivedMessage: TLccMessage read FReceivedMessage write FReceivedMessage;
    // Messages coming in from the connections
    property IncomingMessageList: TThreadList read FIncomingMessageList write FIncomingMessageList;
    // Message waiting for the Mapping of any NodeID/AliasID that the message requires to be found in the AliasServer
    property WaitingForMappingMessageList: TThreadList read FWaitingForMappingMessageList write FWaitingForMappingMessageList;
    // internal worker message
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    procedure Execute; override;
    procedure ClearThreadedObjectList(AThreadedList: TThreadList);
    procedure ProcessAliasAndNodeIDMessages(AMessage: TLccMessage);
    procedure RequestMappingMessageSentCallback(ANodeID: TNodeID; AnAliasID: Word);
    function MappingRequestExists(ANodeID: TNodeID; AnAliasID: Word): Boolean;

  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize); reintroduce;
    destructor Destroy; override;

    // Messages that have valid mappings and can be picked up by the Node Manager
    property OutgoingProcessedMessageList: TThreadList read FOutgoingProcessedMessageList;
    // Mapping Requests, TLccMessages filled in to request the mappings the Alias Server needs.. These need to picked up and sent by the Node Manager
    property MappingRequestMessageList: TThreadList read FMappingRequestMessageList write FMappingRequestMessageList;

    // Adds a message that is incoming from a connection.  This thread will validate that the Alias Server contains
    // any mappings necessary for the message and place them in the OutgoingProcessedMessageList when Node Manager can handle them
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
        if LocalMessage.ValidateAndRequestIfNecessaryAliasMappings({$IFNDEF LCC_DELPHI}@{$ENDIF}RequestMappingMessageSentCallback) then
          OutgoingProcessedMessageList.Add(LocalMessage)
        else
          WaitingForMappingMessageList.Add(LocalMessage)

      end;
    finally
      IncomingMessageList.Clear;
      IncomingMessageList.UnlockList;
    end;

    // See if any mapping messages have come in that clears an incoming message that was
    // placed in the waiting list because it was missing a mapping
    List := WaitingForMappingMessageList.LockList;
    try
      for i := 0 to List.Count -1 do
      begin
        LocalMessage := TLccMessage( List[i]);
        if LocalMessage.ValidateAndRequestIfNecessaryAliasMappings({$IFNDEF LCC_DELPHI}@{$ENDIF}RequestMappingMessageSentCallback) then
          OutgoingProcessedMessageList.Add(LocalMessage)
        else
          WaitingForMappingMessageList.Add(LocalMessage);
      end;
    finally
      WaitingForMappingMessageList.UnlockList;
    end;

    Sleep(50);
  end;
end;

procedure TReceiveMessageAliasServerThread.ClearThreadedObjectList(
  AThreadedList: TThreadList);
var
  List: TList;
  i: Integer;
begin
  List := AThreadedList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    AThreadedList.UnlockList;
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

procedure TReceiveMessageAliasServerThread.RequestMappingMessageSentCallback(
  ANodeID: TNodeID; AnAliasID: Word);
var
  LccMessage: TLccMessage;
begin
  if not MappingRequestExists(ANodeID, AnAliasID) then
  begin
    if AnAliasID <> 0 then
    begin
      LccMessage := TLccMessage.Create;
      LccMessage.LoadVerifyNodeIDAddressed(NULL_NODE_ID, 0, ANodeID, AnAliasID, NULL_NODE_ID);
      MappingRequestMessageList.Add(LccMessage);
      {$IFDEF LOG_MAPPING}DebugLn('Mapping Request Sent via LoadVerifyNodeIDAddressed: 0x' + IntToHex(AnAliasID, 4) + '; ' + NodeIDToString(ANodeID, True));{$ENDIF}
    end else
    if not NullNodeID(ANodeID) then
    begin
      LccMessage := TLccMessage.Create;
      LccMessage.LoadVerifyNodeID(NULL_NODE_ID, 0, ANodeID);
      MappingRequestMessageList.Add(LccMessage);
      {$IFDEF LOG_MAPPING}DebugLn('Mapping Request Sent via LoadVerifyNodeID: 0x' + IntToHex(AnAliasID, 4) + '; ' + NodeIDToString(ANodeID, True));{$ENDIF}
    end else
    begin
      {$IFDEF LOG_MAPPING}DebugLn('Mapping Request requsted but failed: 0x' + IntToHex(AnAliasID, 4) + '; ' + NodeIDToString(ANodeID, True));{$ENDIF}
    end;
  end;
end;

function TReceiveMessageAliasServerThread.MappingRequestExists(ANodeID: TNodeID; AnAliasID: Word): Boolean;
var
  List: TList;
  i: Integer;
  LccMessage: TLccMessage;
begin
  Result := False;
  List := MappingRequestMessageList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      LccMessage := TLccMessage( List[i]);
      if (LccMessage.DestAlias = AnAliasID) or EqualNodeID(ANodeID, LccMessage.DestID, False) then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    MappingRequestMessageList.UnlockList;
  end;
end;

constructor TReceiveMessageAliasServerThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FOutgoingProcessedMessageList := TThreadList.Create;
  FIncomingMessageList := TThreadList.Create;
  FWaitingForMappingMessageList := TThreadList.Create;
  FMappingRequestMessageList := TThreadList.Create;
end;

destructor TReceiveMessageAliasServerThread.Destroy;
begin
  ClearThreadedObjectList(WaitingForMappingMessageList);
  ClearThreadedObjectList(OutgoingProcessedMessageList);
  ClearThreadedObjectList(IncomingMessageList);
  ClearThreadedObjectList(MappingRequestMessageList);
  FreeAndNil(FOutgoingProcessedMessageList);
  FreeAndNil(FIncomingMessageList);
  FreeAndNil(FWaitingForMappingMessageList);
  FreeAndNil(FMappingRequestMessageList);
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

