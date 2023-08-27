unit lcc_node_manager;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ..\lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
    contnrs,
    {$IFNDEF FPC_CONSOLE_APP}
    ExtCtrls,
    {$ENDIF}
  {$ELSE}
    System.Types,
    FMX.Types,
    System.Generics.Collections,
  {$ENDIF}
  lcc_node,
  lcc_node_controller,
  lcc_defines,
  lcc_base_classes,
  lcc_utilities,
  lcc_node_messages,
  lcc_train_server,
  lcc_alias_server;

type

  // This is how the Node Manager has access to the Connection Links with needing
  // to have access the objects associated with the Links.  The Links have a
  // pointer to TLccNodeManager so this allows us to not have to have a single unit
  // to have visibility back and forth.
  IHardwareConnectionManagerLink = interface
    ['{619C8E64-69C3-94A6-B6FE-B16B6CB57A45}']
    procedure SendMessage(AMessage: TLccMessage);
    function IsSelf(Test: TObject): Boolean;
    function IsLccLink: Boolean;
    function GetConnected: Boolean;
//    if the node manager has no more active threads in its links then it should free all nodes and alias maps since it is no longer connected to any LCC networks and everythinbg is stale.
  end;

  { INodeManagerCallbacks }

  INodeManagerCallbacks = interface
    ['{C6920bCA-08BC-4D45-B27C-174640FA3106}']
    procedure DoAliasIDChanged(LccNode: TLccNode);
    procedure DoAliasMappingChange(LccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
    procedure DoAliasRelease(LccNode: TLccNode);
    procedure DoCDIRead(LccNode: TLccNode);
    procedure DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode);
    procedure DoConfigMemOptionsReply(LccNode: TLccNode);
    procedure DoConfigMemReadReply(LccNode: TLccNode);
    procedure DoConfigMemWriteReply(LccNode: TLccNode);
    procedure DoConsumerIdentified(LccNode: TLccNode; LccMessage: TLccMessage);
    procedure DoCreateLccNode(LccNode: TLccNode);
    procedure DoDatagramReply(LccNode: TLccNode);
    procedure DoDestroyLccNode(LccNode: TLccNode);
    procedure DoInitializationComplete(LccNode: TLccNode);
    procedure DoLogInNode(LccNode: TLccNode);
    procedure DoNodeIDChanged(LccNode: TLccNode);
    procedure DoOptionalInteractionRejected(LccNode: TLccNode; LccMessage: TLccMessage);
    procedure DoProducerIdentified(LccNode: TLccNode; LccMessage: TLccMessage);
    procedure DoProtocolIdentifyReply(LccNode: TLccNode; LccMessage: TLccMessage);
    procedure DoRemoteButtonReply(LccNode: TLccNode; LccMessage: TLccMessage);
    procedure DoSimpleNodeIdentReply(LccNode: TLccNode; LccMessage: TLccMessage);
    procedure DoVerifiedNodeID(LccNode: TLccNode; LccMessage: TLccMessage);
  end;

    // These are just straight callbacks on the traction messages
  { INodeManagerTractionCallbacks }

  INodeManagerTractionCallbacks = interface
    ['{3FEB8AAB-060F-CD7E-1747-22D6E6F6FBC7}']
    procedure DoTractionControllerAssign(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionControllerRelease(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionControllerQuery(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionControllerChangedNotify(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionEmergencyStop(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionListenerAttach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionListenerAttached(LccNode: TLccNode; ALccMessage: TLccMessage);
    procedure DoTractionListenerDetach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionListenerDetached(LccNode: TLccNode; ALccMessage: TLccMessage);
    procedure DoTractionListenerQuery(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionManageReserve(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionManageRelease(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionQuerySpeed(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionQueryFunction(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionSetFunction(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionSetSpeed(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionTrainSNIP(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
  end;

type

  TOnLccNodeMessageCallBack = procedure(Sender: TObject; ALccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean) of object;
  TOnLccNodeMessage = procedure(Sender: TObject; ALccNode: TLccNode) of object;
  TOnLccNodeMessageReply = procedure(Sender: TObject; ALccNode: TLccNode; LccMessage: TLccMessage) of object;
  TOnAliasMappingChange = procedure(Sender: TObject; ALccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean) of object;

  TLccNodeManager = class;  // forward

  ENodeIdentificationObjectIsNull = class(Exception);


  TDelayedMessage = class
  public
 //   property AMessage: TLccMessage read FAMessage write FAMessage;
 //   property
  end;

  { TReceiveMessageServerThread }

  TReceiveMessageServerThread = class(TThread)
  private
    FReceivedMessages: TThreadList;
    FOwner: TLccNodeManager;
    FReceivedMessage: TLccMessage;
    FWorkerMessage: TLccMessage;
  protected
    property ReceivedMessage: TLccMessage read FReceivedMessage write FReceivedMessage;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    procedure Execute; override;
    procedure ReceiveMessageSyncronize;   // Syncronize Method called in context of main thread, this thread is stalled during that time
    procedure UpdateGlobalMappings(AMessage: TLccMessage); // Called in context of thread
    procedure NodeIdentificationToCallbackProc(ANodeIdentification: TLccNodeIdentificationObject); // Called in context of thread

  public
    property ReceivedMessages: TThreadList read FReceivedMessages write FReceivedMessages;
    property Owner: TLccNodeManager read FOwner write FOwner;

    procedure ReceiveMessageServerAddMessage(AMessage: TLccMessage);
  end;

  { TLccNodeManager }

  TLccNodeManager = class(TComponent, INodeManagerCallbacks, INodeManagerTractionCallbacks)
  private
    FGridConnect: Boolean;
    FHardwarewareConnectionList: TInterfaceList;
    FNodes: TList;
    FOnAliasMappingChange: TOnAliasMappingChange;
    FOnAliasRelease: TOnLccNodeMessage;
    FOnLccNodeMessageCallBack: TOnLccNodeMessageCallBack;
    FOnNodeAliasIDChanged: TOnLccNodeMessage;
    FOnNodeCDI: TOnLccNodeMessage;
    FOnNodeConfigMemAddressSpaceInfoReply: TOnLccNodeMessage;
    FOnNodeConfigMemOptionsReply: TOnLccNodeMessage;
    FOnNodeConfigMemReadReply: TOnLccNodeMessage;
    FOnNodeConfigMemWriteReply: TOnLccNodeMessage;
    FOnNodeConsumerIdentified: TOnLccNodeMessageReply;
    FOnNodeCreate: TOnLccNodeMessage;
    FOnNodeDatagramReply: TOnLccNodeMessage;
    FOnNodeDestroy: TOnLccNodeMessage;
    FOnNodeIDChanged: TOnLccNodeMessage;
    FOnNodeInitializationComplete: TOnLccNodeMessage;
    FOnNodeLogin: TOnLccNodeMessage;
    FOnNodeOptionalInteractionRejected: TOnLccNodeMessageReply;
    FOnNodeProducerIdentified: TOnLccNodeMessageReply;
    FOnNodeProtocolIdentifyReply: TOnLccNodeMessageReply;
    FOnNodeRemoteButtonReply: TOnLccNodeMessageReply;
    FOnNodeSimpleNodeIdentReply: TOnLccNodeMessageReply;
    FOnNodeTractionControllerChangedNotify: TOnLccNodeMessageCallBack;
    FOnNodeTractionControllerAssign: TOnLccNodeMessageCallBack;
    FOnNodeTractionControllerRelease: TOnLccNodeMessageCallBack;
    FOnNodeTractionControllerQuery: TOnLccNodeMessageCallBack;
    FOnNodeTractionEmergencyStop: TOnLccNodeMessageCallBack;
    FOnNodeTractionListenerAttach: TOnLccNodeMessageCallBack;
    FOnNodeTractionListenerAttached: TOnLccNodeMessageReply;
    FOnNodeTractionListenerDetach: TOnLccNodeMessageCallBack;
    FOnNodeTractionListenerDetached: TOnLccNodeMessageReply;
    FOnNodeTractionListenerQuery: TOnLccNodeMessageCallBack;
    FOnNodeTractionManageRelease: TOnLccNodeMessageCallBack;
    FOnNodeTractionManageReserve: TOnLccNodeMessageCallBack;
    FOnNodeTractionQueryFunction: TOnLccNodeMessageCallBack;
    FOnNodeTractionQuerySpeed: TOnLccNodeMessageCallBack;
    FOnNodeTractionSetSpeed: TOnLccNodeMessageCallBack;
    FOnNodeTractionTrainSNIP: TOnLccNodeMessageCallBack;
    FOnNodeVerifiedNodeID: TOnLccNodeMessageReply;
    FReceiveMessageServerThread: TReceiveMessageServerThread;
    FWorkerMessage: TLccMessage;
    F_100msTimer: TLccTimer;


    function GetNode(Index: Integer): TLccNode;
  protected

    // INodeManagerCallbacks
    procedure DoAliasIDChanged(LccNode: TLccNode);     // Check
    procedure DoAliasMappingChange(LccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);  // Check
    procedure DoAliasRelease(LccNode: TLccNode);
    procedure DoCDIRead(LccNode: TLccNode);    // TODO  Necessary?
    procedure DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode);   // TODO  Necessary?
    procedure DoConfigMemOptionsReply(LccNode: TLccNode);   // TODO  Necessary?
    procedure DoConfigMemReadReply(LccNode: TLccNode);  // TODO  Necessary?
    procedure DoConfigMemWriteReply(LccNode: TLccNode);  // TODO  Necessary?
    procedure DoConsumerIdentified(LccNode: TLccNode; LccMessage: TLccMessage); // Check
    procedure DoCreateLccNode(LccNode: TLccNode);   // Check
    procedure DoDatagramReply(LccNode: TLccNode);   // TODO Necessary?
    procedure DoDestroyLccNode(LccNode: TLccNode);  // Check
    procedure DoInitializationComplete(LccNode: TLccNode);  // Check
    procedure DoLogInNode(LccNode: TLccNode);   // Check
    procedure DoNodeIDChanged(LccNode: TLccNode);  // Check
    procedure DoOptionalInteractionRejected(LccNode: TLccNode; LccMessage: TLccMessage);  // Check
    procedure DoProducerIdentified(LccNode: TLccNode; LccMessage: TLccMessage);   // Check
    procedure DoProtocolIdentifyReply(LccNode: TLccNode; LccMessage: TLccMessage); // TODO  Necessary?
    procedure DoRemoteButtonReply(LccNode: TLccNode; LccMessage: TLccMessage);  // TODO  Necessary?
    procedure DoSimpleNodeIdentReply(LccNode: TLccNode; LccMessage: TLccMessage); // Check
    procedure DoVerifiedNodeID(LccNode: TLccNode; LccMessage: TLccMessage);


    // INodeManagerTractionCallbacks
    procedure DoTractionControllerAssign(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionControllerChangedNotify(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionControllerRelease(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionControllerQuery(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionEmergencyStop(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionListenerAttach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionListenerAttached(LccNode: TLccNode; ALccMessage: TLccMessage);
    procedure DoTractionListenerDetach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionListenerDetached(LccNode: TLccNode; ALccMessage: TLccMessage);
    procedure DoTractionListenerQuery(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionManageReserve(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionManageRelease(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionQuerySpeed(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionQueryFunction(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionSetFunction(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionSetSpeed(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionTrainSNIP(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);


   public
    // Connection Manager

    property GridConnect: Boolean read FGridConnect;

    property Nodes: TList read FNodes write FNodes;

    property Node[Index: Integer]: TLccNode read GetNode; default;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    // May have many places a message needs to go such at TCP, WebSocket, UART, etc
    // Automatically updated through the TLccHardwareConnectionManager when it is created/destroyed
    property HardwarewareConnectionList: TInterfaceList read FHardwarewareConnectionList write FHardwarewareConnectionList;
    property ReceiveMessageServerThread: TReceiveMessageServerThread read FReceiveMessageServerThread write FReceiveMessageServerThread;
    property _100msTimer: TLccTimer read F_100msTimer write F_100msTimer;

    constructor Create(AnOwner: TComponent; GridConnectLink: Boolean); reintroduce; virtual;
    destructor Destroy; override;

    // Interal Node manipulation
    procedure Clear;
    function AddNode(CdiXML: string; AutoLogin: Boolean): TLccNode; virtual;
    function AddNodeByClass(CdiXML: string; NodeClass: TLccNodeClass; AutoLogin: Boolean; NodeID: TNodeID): TLccNode; virtual;
    function FindNode(ANodeID: TNodeID): TLccNode; overload;
    function FindNode(AnAlias: Word): TLccNode; overload;
    function FindInitializedNode: TLccNode;

    procedure ReleaseAliasAll;

    procedure SendMessage(Sender: TObject; LccMessage: TLccMessage); // Outgoing messages are passed through this method, its address is given to Nodes and other objects that need to send messages
    procedure On_100msTimer(Sender: TObject);  virtual;

  published

    // Node Management
    property OnNodeCreate: TOnLccNodeMessage read FOnNodeCreate write FOnNodeCreate;
    property OnNodeDestroy: TOnLccNodeMessage read FOnNodeDestroy write FOnNodeDestroy;
    property OnNodeLogin: TOnLccNodeMessage read FOnNodeLogin write FOnNodeLogin;
    property OnNodeIDChanged: TOnLccNodeMessage read FOnNodeIDChanged write FOnNodeIDChanged;
    property OnNodeInitializationComplete: TOnLccNodeMessage read FOnNodeInitializationComplete write FOnNodeInitializationComplete;
    property OnNodeVerifiedNodeID: TOnLccNodeMessageReply read FOnNodeVerifiedNodeID write FOnNodeVerifiedNodeID;
    property OnNodeProtocolIdentifyReply: TOnLccNodeMessageReply read FOnNodeProtocolIdentifyReply write FOnNodeProtocolIdentifyReply;

    // CAN Node Management
    property OnNodeAliasIDChanged: TOnLccNodeMessage read FOnNodeAliasIDChanged write FOnNodeAliasIDChanged;
    property OnAliasRelease: TOnLccNodeMessage read FOnAliasRelease write FOnAliasRelease;

    // Configuration Memory Information
    property OnNodeConfigMemAddressSpaceInfoReply: TOnLccNodeMessage read FOnNodeConfigMemAddressSpaceInfoReply write FOnNodeConfigMemAddressSpaceInfoReply;
    property OnNodeConfigMemOptionsReply: TOnLccNodeMessage read FOnNodeConfigMemOptionsReply write FOnNodeConfigMemOptionsReply;

    // Configuration Memory Access
    property OnNodeCDI: TOnLccNodeMessage read FOnNodeCDI write FOnNodeCDI;
    property OnNodeConfigMemReadReply: TOnLccNodeMessage read FOnNodeConfigMemReadReply write FOnNodeConfigMemReadReply;
    property OnNodeConfigMemWriteReply: TOnLccNodeMessage read FOnNodeConfigMemWriteReply write FOnNodeConfigMemWriteReply;

    // Events
    property OnNodeConsumerIdentified: TOnLccNodeMessageReply read FOnNodeConsumerIdentified write FOnNodeConsumerIdentified;
    property OnNodeProducerIdentified: TOnLccNodeMessageReply read FOnNodeProducerIdentified write FOnNodeProducerIdentified;

    // SNIP
    property OnNodeSimpleNodeIdentReply: TOnLccNodeMessageReply read FOnNodeSimpleNodeIdentReply write FOnNodeSimpleNodeIdentReply;

    // Datagrams
    property OnNodeDatagramReply: TOnLccNodeMessage read FOnNodeDatagramReply write FOnNodeDatagramReply;

    // Traction - Raw message overrides
    property OnNodeTractionQuerySpeed: TOnLccNodeMessageCallBack read FOnNodeTractionQuerySpeed write FOnNodeTractionQuerySpeed;
    property OnNodeTractionQueryFunction: TOnLccNodeMessageCallBack read FOnNodeTractionQueryFunction write FOnNodeTractionQueryFunction;
    property OnNodeTractionControllerAssign: TOnLccNodeMessageCallBack read FOnNodeTractionControllerAssign write FOnNodeTractionControllerAssign;
    property OnNodeTractionControllerChangedNotify: TOnLccNodeMessageCallBack read FOnNodeTractionControllerChangedNotify write FOnNodeTractionControllerChangedNotify;
    property OnNodeTractionControllerRelease: TOnLccNodeMessageCallBack read FOnNodeTractionControllerRelease write FOnNodeTractionControllerRelease;
    property OnNodeTractionControllerQuery: TOnLccNodeMessageCallBack read FOnNodeTractionControllerQuery write FOnNodeTractionControllerQuery;
    property OnNodeTractionEmergencyStop: TOnLccNodeMessageCallBack read FOnNodeTractionEmergencyStop write FOnNodeTractionEmergencyStop;
    property OnNodeTractionManageReserve: TOnLccNodeMessageCallBack read FOnNodeTractionManageReserve write FOnNodeTractionManageReserve;
    property OnNodeTractionManageRelease: TOnLccNodeMessageCallBack read FOnNodeTractionManageRelease write FOnNodeTractionManageRelease;
    property OnNodeTractionListenerAttach: TOnLccNodeMessageCallBack read FOnNodeTractionListenerAttach write FOnNodeTractionListenerAttach;
    property OnNodeTractionListenerAttached: TOnLccNodeMessageReply read FOnNodeTractionListenerAttached write FOnNodeTractionListenerAttached;
    property OnNodeTractionListenerDetach: TOnLccNodeMessageCallBack read FOnNodeTractionListenerDetach write FOnNodeTractionListenerDetach;
    property OnNodeTractionListenerDetached: TOnLccNodeMessageReply read FOnNodeTractionListenerDetached write FOnNodeTractionListenerDetached;
    property OnNodeTractionListenerQuery: TOnLccNodeMessageCallBack read FOnNodeTractionListenerQuery write FOnNodeTractionListenerQuery;
    property OnNodeTractionSetFunction: TOnLccNodeMessageCallBack read FOnLccNodeMessageCallBack write FOnLccNodeMessageCallBack;
    property OnNodeTractionSetSpeed: TOnLccNodeMessageCallBack read FOnNodeTractionSetSpeed write FOnNodeTractionSetSpeed;
    property OnNodeTractionTrainSNIP: TOnLccNodeMessageCallBack read FOnNodeTractionTrainSNIP write FOnNodeTractionTrainSNIP;

    // Traction DCC Functions

    // Other stuff that may not be useful
    property OnNodeOptionalInteractionRejected: TOnLccNodeMessageReply read FOnNodeOptionalInteractionRejected write FOnNodeOptionalInteractionRejected;
    property OnNodeRemoteButtonReply: TOnLccNodeMessageReply read FOnNodeRemoteButtonReply write FOnNodeRemoteButtonReply;

    // Other interesting stuff
    property OnAliasMappingChange: TOnAliasMappingChange read FOnAliasMappingChange write FOnAliasMappingChange;
  end;


implementation

{ TReceiveMessageServerThread }

procedure TReceiveMessageServerThread.Execute;
var
  LocalMessageList, LocalValidatedMessageList, LocalUnValidatedMessageList: TList;
  i: Integer;
  LocalMessage: TLccMessage;
begin
  ReceivedMessages := TThreadList.Create;
  LocalValidatedMessageList := TList.Create;
  LocalUnValidatedMessageList := TList.Create;
  WorkerMessage := TLccMessage.Create;
  try
    while not Terminated do
    begin
      // First run through the waiting messages.  If there is a Mapping move them in to the
      // LocalValidatedMessageList and set that slot to nil.  If there is no Mapping then the message is moved to the
      // UnValidated list to be handled outside the locked list
      // Need to run from 0 up to maintain order
      LocalMessageList := ReceivedMessages.LockList;
      try
        for i := 0 to LocalMessageList.Count - 1 do
        begin
          LocalMessage := TLccMessage( LocalMessageList[i]);
          if Owner.GridConnect then
          begin
            if EqualNodeID(LocalMessage.SourceID, NULL_NODE_ID, True) and (LocalMessage.CAN.SourceAlias = 0) then    // Malformed Message with no SourceID
              LocalMessage.Free
            else begin
              // Pick out the Verify Message and AMR/AMD to update the AliasMapping Database
              UpdateGlobalMappings(LocalMessage);

              // Pull the message apart and find all the Nodes it requires then test them againt the AliasMapping Database.
              // If they are not there then push the Alias or NodeID (from a message payload) into the Alias Mapping Request list to Send that node a message
              // to Verify it
              if LocalMessage.ExtractNodeIdentificationToCallback({$IFNDEF DELPHI}@{$ENDIF}NodeIdentificationToCallbackProc, True, True) then
                LocalValidatedMessageList.Add(LocalMessageList[i])
              else begin
                LocalMessage.AbandonCount := 0;
                LocalUnValidatedMessageList.Add(LocalMessage);
              end;
            end;
          end else
          begin  // Raw TCP we move them all
            LocalValidatedMessageList.Add(LocalMessage);
          end;
        end;
      finally
        LocalMessageList.Clear; // All received messages have been moved to other places
        ReceivedMessages.UnlockList;
      end;

      // Deliever the messages we moved from the MainList to the LocalValidatedMessageList
      // ***********************************************************************
      try
        for i := 0 to LocalValidatedMessageList.Count - 1 do
        begin
          if not Terminated then
          try
            try
              ReceivedMessage := TLccMessage(LocalValidatedMessageList[i]);
              Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessageSyncronize);
            finally
              FreeAndNil(FReceivedMessage);
            end;
          except
            FreeAndNil(FReceivedMessage);
          end;
        end;
      finally
        LocalValidatedMessageList.Clear
      end;
      // ***********************************************************************

      // ***********************************************************************

      // See if any UnValidated Messages have been validated and the Mappings now exist
      // ***********************************************************************
      if not Terminated then
      begin
        if Owner.GridConnect then
        begin // Must keep order intact so have to run them this way
          for i := 0 to LocalUnValidatedMessageList.Count - 1 do
          begin
            LocalMessage := TLccMessage( LocalUnValidatedMessageList[i]);
            if LocalMessage.ExtractNodeIdentificationToCallback({$IFNDEF DELPHI}@{$ENDIF}NodeIdentificationToCallbackProc, True, True) then
            begin
              LocalValidatedMessageList.Add(LocalMessage);
              LocalUnValidatedMessageList[i] := nil;
            end else
            begin    // Give it 5 seconds or throw it out
              if (LocalMessage.AbandonCount * TIMEOUT_RECIEVE_THREAD) > 5000 then
              begin
                LocalMessage.Free;
                LocalUnValidatedMessageList[i] := nil;
              end;
            end;
            LocalMessage.AbandonCount := LocalMessage.AbandonCount + 1;
         end;

          // Now go through and delete the slots that are empty
          for i := LocalUnValidatedMessageList.Count - 1 downto 0 do
          begin
            if LocalUnValidatedMessageList[i] = nil then
              LocalUnValidatedMessageList.Delete(i);
          end;
        end;
      end;
      // ***********************************************************************


      Sleep(TIMEOUT_RECIEVE_THREAD);
    end;

  finally
    LocalMessageList := ReceivedMessages.LockList;
    try
      for i := 0 to LocalMessageList.Count - 1 do
        TObject(LocalMessageList[i]).Free
    finally
      LocalMessageList.Clear;
      ReceivedMessages.UnlockList;
      FreeAndNil(FReceivedMessages);
    end;

    try
      for i := 0 to LocalValidatedMessageList.Count - 1 do
        TObject(LocalValidatedMessageList[i]).Free
    finally
      LocalValidatedMessageList.Clear;
      FreeAndNil(LocalValidatedMessageList);
      LocalValidatedMessageList.Free;
    end;

    WorkerMessage.Free;
  end;
end;

procedure TReceiveMessageServerThread.ReceiveMessageSyncronize;
// This is called through Syncronize... The main Message Queue is not locked so more messages can be added
// but this will dead lock here and messages to the nodes in the application will starve plus you can't
// Send a message then wait here for it.. it will get placed in the main Message Queue but this won't be called
// until it returns.
var
  i: Integer;
  LocalSourceNode: TLccNode;
begin
  if Owner.GridConnect then
    LocalSourceNode := Owner.FindNode(ReceivedMessage.CAN.SourceAlias)
  else
    LocalSourceNode := Owner.FindNode(ReceivedMessage.SourceID);

  // SourceNode may be nil if the message is not from one of our nodes but that is Ok
  // and this test still is correct... the message needs to be sent to all our nodes
  for i := 0 to Owner.Nodes.Count - 1 do
  begin
    // Don't send the message back to the node that created it
    if LocalSourceNode <> TLccNode(Owner.Nodes[i]) then
      TLccNode(Owner.Nodes[i]).ProcessMessage(ReceivedMessage)
  end;
end;

procedure TReceiveMessageServerThread.UpdateGlobalMappings(AMessage: TLccMessage);
var
  LocalNodeID: TNodeID;
begin
  case AMessage.CAN.MTI of
    MTI_CAN_AMR :
      begin
        AliasServer.MarkForRemovalByAlias(AMessage.CAN.SourceAlias);
      end;
    MTI_CAN_AMD :
      begin
        LocalNodeID := NULL_NODE_ID;
        AliasServer.AddMapping(AMessage.ExtractDataBytesAsNodeID(0, LocalNodeID), AMessage.CAN.SourceAlias);
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
        LocalNodeID := NULL_NODE_ID;
        AliasServer.AddMapping(AMessage.ExtractDataBytesAsNodeID(0, LocalNodeID), AMessage.CAN.SourceAlias);
      end;
    MTI_VERIFY_NODE_ID_NUMBER :
      begin
        if AMessage.DataCount = 0 then     // A global Verify Node ID will repoplulate the entire database so this will flush invalid mappings as well since they won't respond anymore
          AliasServer.Clear;
      end;
  end;
end;

procedure TReceiveMessageServerThread.NodeIdentificationToCallbackProc(ANodeIdentification: TLccNodeIdentificationObject);
begin
  AliasServer.AddMappingRequest(ANodeIdentification.NodeID, ANodeIdentification.Alias);
end;

procedure TReceiveMessageServerThread.ReceiveMessageServerAddMessage(AMessage: TLccMessage);
var
  LocalList: TList;
begin
  LocalList := ReceivedMessages.LockList;
  try
    LocalList.Add(AMessage.Clone);
  finally
    ReceivedMessages.UnlockList;
  end;
end;

{ TLccNodeManager }

procedure TLccNodeManager.DoAliasIDChanged(LccNode: TLccNode);
begin
  if Assigned(OnNodeAliasIDChanged) then
    OnNodeAliasIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoAliasRelease(LccNode: TLccNode);
begin
   if Assigned(FOnAliasRelease) then
     FOnAliasRelease(Self, LccNode);
end;

procedure TLccNodeManager.DoCDIRead(LccNode: TLccNode);
begin
  if Assigned(OnNodeCDI) then
    OnNodeCDI(Self, LccNode)
end;

procedure TLccNodeManager.DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode);
begin
 if Assigned(OnNodeConfigMemAddressSpaceInfoReply) then
   OnNodeConfigMemAddressSpaceInfoReply(Self, LccNode);
end;

procedure TLccNodeManager.DoConfigMemOptionsReply(LccNode: TLccNode);
begin
  if Assigned(OnNodeConfigMemOptionsReply) then
    OnNodeConfigMemOptionsReply(Self, LccNode);
end;

procedure TLccNodeManager.DoConfigMemReadReply(LccNode: TLccNode);
begin
  if Assigned(OnNodeConfigMemReadReply) then
    OnNodeConfigMemReadReply(Self, LccNode);
end;

procedure TLccNodeManager.DoConfigMemWriteReply(LccNode: TLccNode);
begin
  if Assigned(OnNodeConfigMemWriteReply) then
    OnNodeConfigMemWriteReply(Self, LccNode);
end;

procedure TLccNodeManager.DoCreateLccNode(LccNode: TLccNode);
begin
  if Assigned(OnNodeCreate) then
    OnNodeCreate(Self, LccNode)
end;

procedure TLccNodeManager.DoConsumerIdentified(LccNode: TLccNode; LccMessage: TLccMessage);
begin
  if Assigned(OnNodeConsumerIdentified) then
    OnNodeConsumerIdentified(Self, LccNode, LccMessage);
end;

procedure TLccNodeManager.DoDatagramReply(LccNode: TLccNode);
begin
  if Assigned(OnNodeDatagramReply) then
    OnNodeDatagramReply(Self, LccNode);
end;

procedure TLccNodeManager.DoDestroyLccNode(LccNode: TLccNode);
begin
  if Assigned(OnNodeDestroy) then
    OnNodeDestroy(Self, LccNode);
end;

procedure TLccNodeManager.DoInitializationComplete(LccNode: TLccNode);
begin
  if Assigned(OnNodeInitializationComplete) then
    OnNodeInitializationComplete(Self, LccNode);
end;

procedure TLccNodeManager.DoLogInNode(LccNode: TLccNode);
begin
  if Assigned(OnNodeLogin) then
    OnNodeLogin(Self, LccNode);
end;

procedure TLccNodeManager.DoNodeIDChanged(LccNode: TLccNode);
begin
  if Assigned(OnNodeIDChanged) then
    OnNodeIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoOptionalInteractionRejected(LccNode: TLccNode; LccMessage: TLccMessage);
begin
  if Assigned(OnNodeOptionalInteractionRejected) then
    OnNodeOptionalInteractionRejected(Self, LccNode, LccMessage);
end;

procedure TLccNodeManager.DoProducerIdentified(LccNode: TLccNode; LccMessage: TLccMessage);
begin
  if Assigned(OnNodeProducerIdentified) then
    OnNodeProducerIdentified(Self, LccNode, LccMessage);
end;

procedure TLccNodeManager.DoProtocolIdentifyReply(LccNode: TLccNode; LccMessage: TLccMessage);
begin
  if Assigned(OnNodeProtocolIdentifyReply) then
    OnNodeProtocolIdentifyReply(Self, LccNode, LccMessage);
end;

procedure TLccNodeManager.DoRemoteButtonReply(LccNode: TLccNode; LccMessage: TLccMessage);
begin
  if Assigned(OnNodeRemoteButtonReply) then
    OnNodeRemoteButtonReply(Self, LccNode, LccMessage);
end;

procedure TLccNodeManager.DoSimpleNodeIdentReply(LccNode: TLccNode; LccMessage: TLccMessage);
begin
  if Assigned(OnNodeSimpleNodeIdentReply) then
    OnNodeSimpleNodeIdentReply(Self, LccNode, LccMessage);
end;

procedure TLccNodeManager.DoTractionQuerySpeed(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionQuerySpeed) then
    OnNodeTractionQuerySpeed(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionQueryFunction(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionQueryFunction) then
    OnNodeTractionQueryFunction(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionSetFunction(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionSetFunction) then
    OnNodeTractionSetFunction(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionSetSpeed(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionSetSpeed) then
    OnNodeTractionSetSpeed(Self, LccNode, ALccMessage, DoDefault);

end;

procedure TLccNodeManager.DoTractionTrainSNIP(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionTrainSNIP) then
    OnNodeTractionTrainSNIP(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionControllerAssign(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionControllerAssign) then
    OnNodeTractionControllerAssign(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionControllerRelease(LccNode: TLccNode;ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionControllerRelease) then
    OnNodeTractionControllerRelease(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionControllerQuery(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionControllerQuery) then
    OnNodeTractionControllerQuery(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionEmergencyStop(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionEmergencyStop) then
    OnNodeTractionEmergencyStop(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionControllerChangedNotify(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionControllerChangedNotify) then
    OnNodeTractionControllerChangedNotify(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionListenerAttach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionListenerAttach) then
    OnNodeTractionListenerAttach(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionListenerAttached(LccNode: TLccNode; ALccMessage: TLccMessage);
begin
  if Assigned(OnNodeTractionListenerAttached) then
    OnNodeTractionListenerDetached(Self, LccNode, ALccMessage);
end;

procedure TLccNodeManager.DoTractionListenerDetach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionListenerDetach) then
    OnNodeTractionListenerDetach(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionListenerDetached(LccNode: TLccNode; ALccMessage: TLccMessage);
begin
  if Assigned(OnNodeTractionListenerDetached) then
    OnNodeTractionListenerDetached(Self, LccNode, ALccMessage);
end;

procedure TLccNodeManager.DoTractionListenerQuery(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionListenerQuery) then
     OnNodeTractionListenerQuery(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionManageReserve(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionManageReserve) then
    OnNodeTractionManageReserve(Self, LccNode, LccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionManageRelease(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionManageRelease) then
    OnNodeTractionManageRelease(Self, LccNode, LccMessage, DoDefault);
end;

procedure TLccNodeManager.DoVerifiedNodeID(LccNode: TLccNode;LccMessage: TLccMessage);
begin
  if Assigned(OnNodeVerifiedNodeID) then
    OnNodeVerifiedNodeID(Self, LccNode, LccMessage);
end;

procedure TLccNodeManager.DoAliasMappingChange(LccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
begin
  if Assigned(OnAliasMappingChange) then
    OnAliasMappingChange(Self, LccNode, AnAliasMapping, IsMapped);
end;

constructor TLccNodeManager.Create(AnOwner: TComponent; GridConnectLink: Boolean);
begin
  inherited Create(AnOwner);
  FNodes := TList.Create;

  FGridConnect := GridConnectLink;
  FHardwarewareConnectionList := TInterfaceList.Create;
  FWorkerMessage := TLccMessage.Create;
  FReceiveMessageServerThread := TReceiveMessageServerThread.Create(True);
  FReceiveMessageServerThread.Owner := Self;
  FReceiveMessageServerThread.FreeOnTerminate := True;
  FReceiveMessageServerThread.Suspended := False;

  _100msTimer := TLccTimer.Create(nil);
  _100msTimer.OnTimer := {$IFNDEF DELPHI}@{$ENDIF}On_100msTimer;
  _100msTimer.Interval := 100;
  _100msTimer.Enabled := True;
end;

function TLccNodeManager.AddNode(CdiXML: string; AutoLogin: Boolean): TLccNode;
begin
  Result := TLccNode.Create(Self, CdiXML, GridConnect);
  Result.SendMessageFunc := {$IFDEF FPC}@{$ENDIF}SendMessage;
  Nodes.Add(Result);
  DoCreateLccNode(Result);
  if AutoLogin then
    Result.Login(NULL_NODE_ID);
end;

function TLccNodeManager.AddNodeByClass(CdiXML: string; NodeClass: TLccNodeClass; AutoLogin: Boolean; NodeID: TNodeID): TLccNode;
begin
  Result := nil;
  if Assigned(NodeClass) then
  begin
    Result := NodeClass.Create(Self, CdiXML, GridConnect);
    Result.SendMessageFunc := {$IFDEF FPC}@{$ENDIF}SendMessage;
    Nodes.Add(Result);
    DoCreateLccNode(Result);
    if AutoLogin then
      Result.Login(NodeID);
  end;
end;

function TLccNodeManager.FindNode(ANodeID: TNodeID): TLccNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Nodes.Count - 1 do
  begin
    if EqualNodeID(ANodeID, TLccNode(Nodes.Items[i]).NodeID, False) then
    begin
      Result := TLccNode(Node[i]);
      Break
    end;
  end;
end;

function TLccNodeManager.FindNode(AnAlias: Word): TLccNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Nodes.Count - 1 do
  begin
    if AnAlias = TLccNode(Nodes.Items[i]).AliasID then
    begin
      Result := TLccNode(Node[i]);
      Break
    end;
  end;
end;

function TLccNodeManager.FindInitializedNode: TLccNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Nodes.Count - 1 do
  begin
    if TLccNode( Nodes[i]).Initialized then
    begin
      Result := TLccNode( Nodes[i]);
      Break;
    end;
  end;
end;

destructor TLccNodeManager.Destroy;
begin
  Clear;
  _100msTimer.Enabled := False;
  FNodes.Free;
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FHardwarewareConnectionList);
  FreeAndNil(F_100msTimer);
  ReceiveMessageServerThread.Terminate;
  inherited Destroy;
end;

procedure TLccNodeManager.Clear;
var
  i: Integer;
begin
  try
    // for large number of nodes this is faster as we don't delay for each release as it
    // would if we just called Free on all the nodes and let them each delay 100ms
    // One delay after all are released is much faster
    ReleaseAliasAll;
    for i := 0 to Nodes.Count - 1 do
      TLccNode(Nodes.Items[i]).Free;
  finally
    Nodes.Clear;
  end;
end;

function TLccNodeManager.GetNode(Index: Integer): TLccNode;
begin
  Result := nil;
  if Index < Nodes.Count then
    Result := TLccNode(Nodes.Items[Index]);
end;

procedure TLccNodeManager.ReleaseAliasAll;
var
  i: Integer;
begin
  for i := 0 to Nodes.Count - 1 do
    TLccNode(Nodes.Items[i]).ReleaseAlias(0);
  Sleep(500);  // Wait for the AMR messages to be sent
end;

procedure TLccNodeManager.SendMessage(Sender: TObject; LccMessage: TLccMessage);
var
  iHardwareConnection: Integer;
begin
  // Send the message to the interfaces (Ethernet, WebSocket, UART, ect)
  for iHardwareConnection := 0 to HardwarewareConnectionList.Count - 1 do
    if (HardwarewareConnectionList[iHardwareConnection] as IHardwareConnectionManagerLink).IsLccLink then
      (HardwarewareConnectionList[iHardwareConnection] as IHardwareConnectionManagerLink).SendMessage(LccMessage);

  // Send the messages to all the other virtual nodes where this would be the receiving end of the sendmessage
  // Never filter these messages.
  ReceiveMessageServerThread.ReceiveMessageServerAddMessage(LccMessage);  // I think this works....
end;

procedure TLccNodeManager.On_100msTimer(Sender: TObject);
var
  i: Integer;
  LccNode: TLccNode;
begin
  for i := 0 to Nodes.Count - 1 do
  begin
    LccNode := TLccNode( Nodes[i]);
    LccNode.On_100msTimer(Sender);
  end;
end;

end.

