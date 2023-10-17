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


const
  SLEEP_ALIAS_SERVER_THREAD_MS = 20;
  VERIFYNODE_RETRY_TIME_MS = 500;     // must be greater than SLEEP_ALIAS_SERVER_THREAD_MS
  VERIFYNODE_ABANDON_TIME_MS = 5000;  // must be greater than VERIFYNODE_RETRY_TIME_MS


type

TLccAliasServerDispatchProcessedMessageFunc = procedure(ALccMessage: TLccMessage) of object;

{ TReceiveMessageAliasServerThread }

TReceiveMessageAliasServerThread = class(TThread)
  private
    FMappingRequestMessageList: TThreadList;                                         // Storage for messages that still require Alias mapping of nodes referenced in the message (source, destination, payload NodeIDs, etc)
    FOutgoingProcessedMessageList: TThreadList;                                      // Holds the messages that are fully processed LCC messages ready to be sent via DispatchProcessedMessageCallback
    FIncomingMessageList: TThreadList;                                               // Incoming CAN messages that need to be processed to ensures all mappings are validated to the nodes it references (source, destination, payload NodeIDs, etc)                                    // Before the fully processed
    FReceiveMessageCallback: TOnMessageEvent;
    FSendMessageCallback: TOnSendMessageEvent;
    FWaitingForMappingMessageList: TThreadList;
    FWorkerMessage: TLccMessage;
  protected
    // Messages coming in from the connections
    property IncomingMessageList: TThreadList read FIncomingMessageList write FIncomingMessageList;
    // Messages waiting for the Mapping of any NodeID/AliasID that the message required to be retrieved
    property WaitingForMappingMessageList: TThreadList read FWaitingForMappingMessageList write FWaitingForMappingMessageList;
    // Messages that have valid mappings and can be dispatched to the system
    property OutgoingProcessedMessageList: TThreadList read FOutgoingProcessedMessageList;
    // Mapping Requests, TLccMessages filled in to request the mappings the Alias Server needs.. These need to picked up and sent by the Node Manager
    property MappingRequestMessageList: TThreadList read FMappingRequestMessageList write FMappingRequestMessageList;

    // General use message
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    procedure Execute; override;

    // Checks for messages that will create a mapping (CAN or Lcc) and flushes the mappings if a message is sent that will cause all nodes to respond with AMD or VerifyNodeID to refresh it
    procedure ProcessAliasAndNodeIDMappingMessages(AMessage: TLccMessage);
    // When a message is asked to validate all node references it carries (Destination, payload NodeID, etc) it may need to send a VerifyNodeID or AME and it does so through this callback
    procedure RequestMappingMessageSentCallback(ANodeID: TNodeID; AnAliasID: Word);
    // Locates the TLccMessage in the Mapping Request List that matches EITHER OR of these IDs
    function FindMessageWithMappingRequest(ANodeID: TNodeID; AnAliasID: Word): TLccMessage;
    // Mapping Request messages are time coded so they can be resent if the VerifiedNode or AMD does not come and then be deleted after some time
    procedure IncrementMappingRequestMessageTagAnCheckForAbandonment;

    // Methods called through Syncronize() to utilize the callbacks within the main threads context
    procedure DispatchMessageThroughSyncronize;
    procedure SendMessageThroughSyncronize;

  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize); reintroduce;
    destructor Destroy; override;

    // This property is set to a function that will send a Message to the all node, currently calls into TLccNodeManager.SendLccMessageNodeManager so it can pickup a node as the source to send the message
    property SendMessageCallback: TOnSendMessageEvent read FSendMessageCallback write FSendMessageCallback;
    // This is where to deliver processed message tht have been verified for AliasMappings, currently calls into ConnectionFactory.ReceiveMessageConnectinFactory
    property ReceiveMessageCallback: TOnMessageEvent read FReceiveMessageCallback write FReceiveMessageCallback;

    // When new messages come in from Connections (TCP, WebSockets, CAN, anything) they must call this method to process them to make sure the AliasMapping Table valid for all referenced Nodes in the message
    procedure AddIncomingLccMessage(AMessage: TLccMessage; GridConnect: Boolean);
  end;

var
  AliasServerThread: TReceiveMessageAliasServerThread;

implementation

uses
  lcc_connection_common;


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
        // Process any mapping definition messages.  If they are mapping messages then we know the mapping exisits and can skip any more checking.
        ProcessAliasAndNodeIDMappingMessages(LocalMessage);

        // If it is a CAN message send it on, we don't try to map CAN messages since during Alias
        // reconciliation they must go through because the Alias is not valid yet and a CAN message only delivers
        // NodeIDs it never requires them to be used

        if not LocalMessage.IsCAN then
        begin
          if LocalMessage.ValidateAndRequestIfNecessaryAliasMappings({$IFNDEF LCC_DELPHI}@{$ENDIF}RequestMappingMessageSentCallback) then
            OutgoingProcessedMessageList.Add(LocalMessage)
          else
            WaitingForMappingMessageList.Add(LocalMessage)
        end else
          OutgoingProcessedMessageList.Add(LocalMessage)

      end;
    finally
      List.Clear;
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
        begin
          List[i] := nil;
          OutgoingProcessedMessageList.Add(LocalMessage);
        end;
      end;
    finally
      ListClearNilObjects(List);
      WaitingForMappingMessageList.UnlockList;
    end;

    IncrementMappingRequestMessageTagAnCheckForAbandonment;
    Synchronize({$IFNDEF LCC_DELPHI}@{$ENDIF}DispatchMessageThroughSyncronize);

    Sleep(SLEEP_ALIAS_SERVER_THREAD_MS);
  end;
end;

procedure TReceiveMessageAliasServerThread.ProcessAliasAndNodeIDMappingMessages(
  AMessage: TLccMessage);
var
  DummyNodeID: TNodeID;
begin
  case AMessage.CAN_MTI of
    MTI_CAN_AMR :
      begin
        AliasServer.RemoveMapping(AMessage.SourceAlias, False);
      end;
    MTI_CAN_AMD :
      begin
        DummyNodeID := NULL_NODE_ID;
        AliasServer.AddMapping(AMessage.ExtractDataBytesAsNodeID(0, DummyNodeID), AMessage.SourceAlias, False);
      end;
    MTI_CAN_AME :
      begin
        if AMessage.DataCount = 0 then      // A global AME will repoplulate the entire database so this will flush invalid mappings as well since they won't respond anymore
          AliasServer.Clear(False);                // BUT be aware that the node sending this message will not be restored as it won't send out an AME message
      end;
  end;

  case AMessage.MTI of
    MTI_VERIFIED_NODE_ID_NUMBER,
    MTI_INITIALIZATION_COMPLETE :
      begin
        DummyNodeID := NULL_NODE_ID;
        AliasServer.AddMapping(AMessage.ExtractDataBytesAsNodeID(0, DummyNodeID), AMessage.SourceAlias, False);
      end;
    MTI_VERIFY_NODE_ID_NUMBER :
      begin
        if AMessage.DataCount = 0 then     // A global Verify Node ID will repoplulate the entire database so this will flush invalid mappings as well since they won't respond anymore
          AliasServer.Clear(False);               // BUT be aware that the node sending this message will not be restored as it won't send out an MTI_VERIFIED_NODE_ID_NUMBER message
      end;
  end;
end;

procedure TReceiveMessageAliasServerThread.RequestMappingMessageSentCallback(ANodeID: TNodeID; AnAliasID: Word);
var
  LccMessage: TLccMessage;
  MappingRequestMessage: TLccMessage;
begin
  Assert(Assigned(SendMessageCallback), 'TReceiveMessageAliasServerThread,SendMessageCallback not assigned');

  // This mechinism does not work for trying to request mapping on internal virtual nodes as it
  // may be trying to send a message to itself to VerifyNode and we need to use the first Node as the source
  // The nodes Add and Removing mapping directly as they are created to simplify this

  MappingRequestMessage := FindMessageWithMappingRequest(ANodeID, AnAliasID);

  if AnAliasID <> 0 then   // If we have the Alias use the CAN messages to get the full Node ID
  begin
    if not Assigned(MappingRequestMessage) then
    begin   // Did not find it so create it and send a Verify request
      LccMessage := TLccMessage.Create;
      LccMessage.LoadVerifyNodeIDAddressed(NULL_NODE_ID, 0, ANodeID, AnAliasID, NULL_NODE_ID);
      MappingRequestMessageList.Add(LccMessage);
      LccMessage.iTag := 1;
      LccMessage.CopyToTarget(WorkerMessage);
      Synchronize(@SendMessageThroughSyncronize);
    end else
    if MappingRequestMessage.iTag mod (VERIFYNODE_RETRY_TIME_MS div SLEEP_ALIAS_SERVER_THREAD_MS) = 0 then
    begin  // Every VERIFYNODE_RETRY_TIME_MS seconds try again if it still has not cleared
      MappingRequestMessage.CopyToTarget(WorkerMessage);
      Synchronize(@SendMessageThroughSyncronize);
    end
  end else
  if not NullNodeID(ANodeID) then  // If we have the NodeID use it to send us the Alias
  begin
    if not Assigned(MappingRequestMessage) then
    begin   // Did not find it so create it and send a Verify request
      LccMessage := TLccMessage.Create;
      LccMessage.LoadVerifyNodeID(NULL_NODE_ID, 0, ANodeID);
      MappingRequestMessageList.Add(LccMessage);
      LccMessage.iTag := 1;
      LccMessage.CopyToTarget(WorkerMessage);
      Synchronize(@SendMessageThroughSyncronize);
    end else // Every VERIFYNODE_RETRY_TIME_MS seconds try again if it still has not cleared
    if MappingRequestMessage.iTag mod VERIFYNODE_RETRY_TIME_MS div SLEEP_ALIAS_SERVER_THREAD_MS = 0 then
    begin
      MappingRequestMessage.CopyToTarget(WorkerMessage);
      Synchronize(@SendMessageThroughSyncronize);
    end
  end
end;

function TReceiveMessageAliasServerThread.FindMessageWithMappingRequest(ANodeID: TNodeID; AnAliasID: Word): TLccMessage;
var
  List: TList;
  i: Integer;
  LccMessage: TLccMessage;
begin
  Result := nil;
  List := MappingRequestMessageList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      LccMessage := TLccMessage( List[i]);
      if (LccMessage.DestAlias = AnAliasID) or EqualNodeID(ANodeID, LccMessage.DestID, False) then
      begin
        Result := LccMessage;
        Break;
      end;
    end;
  finally
    MappingRequestMessageList.UnlockList;
  end;
end;

procedure TReceiveMessageAliasServerThread.IncrementMappingRequestMessageTagAnCheckForAbandonment;
var
  List: TList;
  i: Integer;
  LccMessage: TLccMessage;
begin
  List := MappingRequestMessageList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      LccMessage := TLccMessage( List[i]);
      LccMessage.iTag := LccMessage.iTag + 1;
      if LccMessage.iTag > VERIFYNODE_ABANDON_TIME_MS then
      begin
        List[i] := nil;
        LccMessage.Free;
      end;
    end;
  finally
    ListClearNilObjects(List);
    MappingRequestMessageList.UnlockList;
  end;
end;

procedure TReceiveMessageAliasServerThread.DispatchMessageThroughSyncronize;
var
  List: TList;
  i: Integer;
begin
  Assert( Assigned(ReceiveMessageCallback), 'TReceiveMessageAliasServerThread,ReceiveMessageSource is not assigned');

  List := OutgoingProcessedMessageList.LockList;
  try
    try
      for i := 0 to List.Count - 1 do
      begin
        ReceiveMessageCallback(TLccMessage( List[i]));
        TObject( List[i]).Free;
      end;
    finally
      List.Clear
    end;
  finally
    OutgoingProcessedMessageList.UnlockList;
  end;
end;

procedure TReceiveMessageAliasServerThread.SendMessageThroughSyncronize;
begin
  if Assigned(SendMessageCallback) then
    SendMessageCallback(WorkerMessage, True);
  {$IFDEF LOG_MAPPING}DebugLn('Mapping Request Sent via LoadVerifyNodeIDAddressed: 0x' + IntToHex(WorkerMessage.SourceAlias, 4) + '; ' + NodeIDToString(WorkerMessage.SourceID, True));{$ENDIF}
end;

constructor TReceiveMessageAliasServerThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FOutgoingProcessedMessageList := TThreadList.Create;
  FIncomingMessageList := TThreadList.Create;
  FWaitingForMappingMessageList := TThreadList.Create;
  FMappingRequestMessageList := TThreadList.Create;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TReceiveMessageAliasServerThread.Destroy;
begin
  ThreadListClearObjects(WaitingForMappingMessageList);
  ThreadListClearObjects(OutgoingProcessedMessageList);
  ThreadListClearObjects(IncomingMessageList);
  ThreadListClearObjects(MappingRequestMessageList);
  FreeAndNil(FOutgoingProcessedMessageList);
  FreeAndNil(FIncomingMessageList);
  FreeAndNil(FWaitingForMappingMessageList);
  FreeAndNil(FMappingRequestMessageList);
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

procedure TReceiveMessageAliasServerThread.AddIncomingLccMessage(AMessage: TLccMessage; GridConnect: Boolean);
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
  if Assigned(ConnectionFactory) then
    AliasServerThread.ReceiveMessageCallback := @ConnectionFactory.ReceiveMessageConnectinFactory;

finalization
  AliasServerThread.ReceiveMessageCallback := nil;
  AliasServerThread.Terminate;
  AliasServerThread := nil;


end.

