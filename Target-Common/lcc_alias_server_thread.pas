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
  VERIFYNODE_RETRY_TIME_MS = 2500;     // must be greater than SLEEP_ALIAS_SERVER_THREAD_MS
  VERIFYNODE_ABANDON_TIME_MS = 5000;  // must be greater than VERIFYNODE_RETRY_TIME_MS


type

TLccAliasServerDispatchProcessedMessageFunc = procedure(ALccMessage: TLccMessage) of object;

{ TReceiveMessageAliasServerThread }

TReceiveMessageAliasServerThread = class(TThread)
  private
    FMappingRequestsSentMessageList: TThreadList;                                         // Storage for messages that still require Alias mapping of nodes referenced in the message (source, destination, payload NodeIDs, etc)
    FOutgoingProcessedMessageList: TThreadList;                                      // Holds the messages that are fully processed LCC messages ready to be sent via DispatchProcessedMessageCallback
    FIncomingMessageList: TThreadList;                                               // Incoming CAN messages that need to be processed to ensures all mappings are validated to the nodes it references (source, destination, payload NodeIDs, etc)                                    // Before the fully processed
    FReceiveMessageCallback: TOnMessageEvent;
    FSendMessageCallback: TOnSendMessageEvent;
    FWaitingForMappingValidationMessageList: TThreadList;
    FWorkerMessage: TLccMessage;
  protected
    // Messages coming in from the connections
    property IncomingMessageList: TThreadList read FIncomingMessageList write FIncomingMessageList;
    // Messages waiting for the Mapping of any NodeID/AliasID that the message required to be retrieved
    property WaitingForMappingValidationMessageList: TThreadList read FWaitingForMappingValidationMessageList write FWaitingForMappingValidationMessageList;
    // Messages that have valid mappings and can be dispatched to the system
    property OutgoingProcessedMessageList: TThreadList read FOutgoingProcessedMessageList;
    // Mapping Requests, TLccMessages filled in to request the mappings the Alias Server needs.. Sent by the Node Manager to pick up a NodeID and sent to the network
    property MappingRequestsSentMessageList: TThreadList read FMappingRequestsSentMessageList write FMappingRequestsSentMessageList;

    // General use message
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    // Overridden thread Execution method
    procedure Execute; override;

    // Checks for messages that will create a mapping (CAN or Lcc) and flushes the mappings if a message is sent that will cause all nodes to respond with AMD or VerifyNodeID to refresh it
    procedure ProcessAliasAndNodeIDMappingMessages(AMessage: TLccMessage);
    // When a message is asked to validate all node references it carries (Destination, payload NodeID, etc) it may need to send a VerifyNodeID or AME and it does so through this callback
    procedure RequestMappingMessageSentCallback(ANodeID: TNodeID; AnAliasID: Word);
    // Locates the TLccMessage in the Mapping Request List that matches EITHER OR of these IDs
    function FindMessageWithMappingRequest(ANodeID: TNodeID; AnAliasID: Word): TLccMessage;

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
  i, j: Integer;
  LocalMessage: TLccMessage;
  ProblemArray: TLccAliasIDArray;
begin

  while not Terminated do
  begin
    // Any of this will ONLY run if in during the AddIncomingMessage call it was defined as a GridConnect so
    // the assumption here is ONLY gridconnect with Alias's are here.

    // *************************************************************************
    // First we run through the IncomingMessageList testing each to see if it contains Alias (and/or NodeIDs in payloads)
    // that don't have a Alias/NodeID mapping in our AliasMapping database.  If they are all there then just move it
    // to the OutgoingProcessedMessageList, if not then move them into the WaitingForMappingValidationMessageList to continute
    // to check on if the Mappings have been validated.  Note the call to LocalMessage.ValidateAndRequestIfNecessaryAliasMappings
    // will use the callback to send a message trying to request the information it needs.  The messages sent is kept in MappingRequestsSentMessageList
    // so we can keep network traffic to a minimum by not duplicating requests over and over.  We can see that I request was
    // made some time ago and if enough time has lapsed we can send the same message again
    List := IncomingMessageList.LockList;
    try
      for i := 0 to List.Count -1 do
      begin
        LocalMessage := TLccMessage( List[i]);

        LocalMessage.AbandonTickCount := 0;
        LocalMessage.RetryAttempts := 0;

        // Process any mapping definition messages.  If they are mapping messages then we know the mapping exisits and can skip any more checking.
        ProcessAliasAndNodeIDMappingMessages(LocalMessage);

        // If it is a CAN message send it on, we don't try to map CAN messages since during Alias
        // reconciliation they must go through because the Alias is not valid yet and a CAN message only delivers
        // NodeIDs it never requires them to be used

        ProblemArray := [];
        if LocalMessage.ValidateAndRequestIfNecessaryAliasMappings(ProblemArray, {$IFNDEF LCC_DELPHI}@{$ENDIF}RequestMappingMessageSentCallback) then
          OutgoingProcessedMessageList.Add(LocalMessage)
        else
          WaitingForMappingValidationMessageList.Add(LocalMessage)
      end;
    finally
      List.Clear;
      IncomingMessageList.UnlockList;
    end;
    // *************************************************************************


    // *************************************************************************
    // Now look at the Messages we put in the WaitingForMappingValidationMessageList. Check them again
    // and see if the Mappings are valid.  Again the call back will see if the Verify Node ID message
    // was already sent and if so is time up to try it again. If it is validated then put it in the
    // OutgoingProcessedMessageList and remove it from the Waiting for Validation list.  If not
    // see if the overall timeout has expired and we just give up on this Incoming message since we
    // can't find it on the network anyway it won't care if we don't respond.  The Sent Messages
    // have their own timer system so they will check for a retry a few times until they hit the
    // Abandon time and they will be tossed as well.
    List := WaitingForMappingValidationMessageList.LockList;
    try
      for i := 0 to List.Count -1 do
      begin
        LocalMessage := TLccMessage( List[i]);
        if LocalMessage.ValidateAndRequestIfNecessaryAliasMappings(ProblemArray, {$IFNDEF LCC_DELPHI}@{$ENDIF}RequestMappingMessageSentCallback) then
        begin
          List[i] := nil;
          OutgoingProcessedMessageList.Add(LocalMessage);
        end else
        begin
          LocalMessage.AbandonTickCount := LocalMessage.AbandonTickCount + 1;
          LocalMessage.RetryAttempts := LocalMessage.RetryAttempts + 1;
          if LocalMessage.AbandonTickCount > (VERIFYNODE_ABANDON_TIME_MS div SLEEP_ALIAS_SERVER_THREAD_MS) then
          begin
            for j := 0 to Length(ProblemArray) - 1 do
              AliasServer.RemoveMapping(ProblemArray[j], False);
            LocalMessage.Free;
            List[i] := nil;
          end;
        end;
      end;
    finally
      ListClearNilObjects(List);
      WaitingForMappingValidationMessageList.UnlockList;
    end;
    // *************************************************************************

    // *************************************************************************
    // Now run through  the Messages we put in the MappingRequestsSentMessageList.
    // Increment the timer tick and if they have hit the abandon time then toss them
    List := MappingRequestsSentMessageList.LockList;
    try
      for i := 0 to List.Count - 1 do
      begin
        LocalMessage := TLccMessage( List[i]);
        LocalMessage.AbandonTickCount := LocalMessage.AbandonTickCount + 1;
        LocalMessage.RetryAttempts := LocalMessage.RetryAttempts + 1;
        if LocalMessage.AbandonTickCount > (VERIFYNODE_ABANDON_TIME_MS div SLEEP_ALIAS_SERVER_THREAD_MS) then
        begin
          List[i] := nil;
          LocalMessage.Free;
        end;
      end;
    finally
      ListClearNilObjects(List);
      MappingRequestsSentMessageList.UnlockList;
    end;
    // *************************************************************************


    // *************************************************************************
    // Dispatch any messages that are ready to be passed on to anyone who cares,
    // at this point the AliasMapping database contains mappings for any node referenced
    // in this message.  Do it in the context of the main thread to make things safer
    Synchronize({$IFDEF FPC}@{$ENDIF}DispatchMessageThroughSyncronize);
    // *************************************************************************

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
          AliasServer.Clear(False);         // BUT be aware that the node sending this message will not be restored as it won't send out an AME message
      end;
  end;

  case AMessage.MTI of
    MTI_VERIFIED_NODE_ID_NUMBER,
    MTI_INITIALIZATION_COMPLETE :
      begin
        DummyNodeID := NULL_NODE_ID;
        AliasServer.AddMapping(AMessage.ExtractDataBytesAsNodeID(0, DummyNodeID), AMessage.SourceAlias, False);
      end;
    MTI_VERIFY_NODE_ID_NUMBER:
      begin
        if AMessage.DataCount = 0 then     // A global Verify Node ID will repoplulate the entire database so this will flush invalid mappings as well since they won't respond anymore
          AliasServer.Clear(False);        // BUT be aware that the node sending this message will not be restored as it won't send out an MTI_VERIFIED_NODE_ID_NUMBER message
      end;
  end;
end;

procedure TReceiveMessageAliasServerThread.RequestMappingMessageSentCallback(ANodeID: TNodeID; AnAliasID: Word);
var
  LocalLccMessage: TLccMessage;
  CopyAndSend: Boolean;
begin
  Assert(Assigned(SendMessageCallback), 'TReceiveMessageAliasServerThread,SendMessageCallback not assigned');

  // This mechinism does not work for trying to request mapping on internal virtual nodes as it
  // may be trying to send a message to itself to VerifyNode and we need to use the first Node as the source
  // The nodes Add and Removing mapping directly as they are created to simplify this

  CopyAndSend := False;

  LocalLccMessage := FindMessageWithMappingRequest(ANodeID, AnAliasID);

  if AnAliasID <> 0 then   // If we have the Alias use the CAN messages to get the full Node ID
  begin
    if not Assigned(LocalLccMessage) then
    begin   // Did not find it so create it and send a Verify request
      LocalLccMessage := TLccMessage.Create;
      LocalLccMessage.LoadVerifyNodeIDAddressed(NULL_NODE_ID, 0, ANodeID, AnAliasID, NULL_NODE_ID);
      MappingRequestsSentMessageList.Add(LocalLccMessage);
      LocalLccMessage.RetryAttempts := 1;
      CopyAndSend := True;
    end else
    if LocalLccMessage.AbandonTickCount mod (VERIFYNODE_RETRY_TIME_MS div SLEEP_ALIAS_SERVER_THREAD_MS) = 0 then
    begin  // Every VERIFYNODE_RETRY_TIME_MS seconds try again if it still has not cleared
      LocalLccMessage.RetryAttempts := LocalLccMessage.RetryAttempts + 1;
      if LocalLccMessage.RetryAttempts < MAX_MESSAGE_RETRY_ATTEMPTS then
        CopyAndSend := True;      // else it will timeout and be released
    end
  end else
  if not NullNodeID(ANodeID) then  // If we have the NodeID use it to send us the Alias
  begin
    if not Assigned(LocalLccMessage) then
    begin   // Did not find it so create it and send a Verify request
      LocalLccMessage := TLccMessage.Create;
      LocalLccMessage.LoadVerifyNodeID(NULL_NODE_ID, 0, ANodeID);
      MappingRequestsSentMessageList.Add(LocalLccMessage);
      LocalLccMessage.RetryAttempts := 1;
      CopyAndSend := True;;
    end else // Every VERIFYNODE_RETRY_TIME_MS seconds try again if it still has not cleared
    if LocalLccMessage.iTag mod VERIFYNODE_RETRY_TIME_MS div SLEEP_ALIAS_SERVER_THREAD_MS = 0 then
    begin
      LocalLccMessage.RetryAttempts := LocalLccMessage.RetryAttempts + 1;
      if LocalLccMessage.RetryAttempts < MAX_MESSAGE_RETRY_ATTEMPTS then
        CopyAndSend := True;      // else it will timeout and be released
    end
  end;

  if CopyAndSend then
  begin
    LocalLccMessage.CopyToTarget(WorkerMessage);
    Synchronize({$IFDEF FPC}@{$ENDIF}SendMessageThroughSyncronize);
  end;
end;

function TReceiveMessageAliasServerThread.FindMessageWithMappingRequest(ANodeID: TNodeID; AnAliasID: Word): TLccMessage;
var
  List: TList;
  i: Integer;
  LccMessage: TLccMessage;
begin
  Result := nil;
  List := MappingRequestsSentMessageList.LockList;
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
    MappingRequestsSentMessageList.UnlockList;
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
  {$IFNDEF POSIX}
  inherited Create(CreateSuspended, StackSize);
  {$ELSE}
  inherited Create(CreateSuspended);
  {$ENDIF}
  FOutgoingProcessedMessageList := TThreadList.Create;
  FIncomingMessageList := TThreadList.Create;
  FWaitingForMappingValidationMessageList := TThreadList.Create;
  FMappingRequestsSentMessageList := TThreadList.Create;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TReceiveMessageAliasServerThread.Destroy;
begin
  ThreadListClearObjects(WaitingForMappingValidationMessageList);
  ThreadListClearObjects(OutgoingProcessedMessageList);
  ThreadListClearObjects(IncomingMessageList);
  ThreadListClearObjects(MappingRequestsSentMessageList);
  FreeAndNil(FOutgoingProcessedMessageList);
  FreeAndNil(FIncomingMessageList);
  FreeAndNil(FWaitingForMappingValidationMessageList);
  FreeAndNil(FMappingRequestsSentMessageList);
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
    AliasServerThread.ReceiveMessageCallback := {$IFDEF FPC}@{$ENDIF}ConnectionFactory.ReceiveMessageConnectinFactory;

finalization
  AliasServerThread.ReceiveMessageCallback := nil;
  AliasServerThread.Terminate;
  AliasServerThread := nil;


end.

