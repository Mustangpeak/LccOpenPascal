unit lcc_node_manager;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface


uses
  Classes,
  SysUtils,
  {$IFDEF LCC_FPC}
    contnrs,
  {$ELSE}
    System.Types,
    FMX.Types,
    System.Generics.Collections,
  {$ENDIF}
  lcc_node,
  lcc_node_controller,
  lcc_defines,
  lcc_base_classes,
  lcc_common_classes,
  lcc_utilities,
  lcc_node_messages,
  lcc_train_server,
  lcc_alias_server,
  lcc_alias_server_thread;

type

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

  { TLccNodeManager }

  TLccNodeManager = class(TComponent, INodeManagerCallbacks, INodeManagerTractionCallbacks)
  private
    FEmulateCanNetworkLogin: Boolean;
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


    procedure DispatchMessageCallback(ALccMessage: TLccMessage);  // callback for AliasServerThread to connect to the incoming messages

   public
    property Nodes: TList read FNodes write FNodes;

    property Node[Index: Integer]: TLccNode read GetNode; default;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    // May have many places a message needs to go such at TCP, WebSocket, UART, etc
    // Automatically updated through the TLccHardwareConnectionManager when it is created/destroyed
    property _100msTimer: TLccTimer read F_100msTimer write F_100msTimer;
    property EmulateCanNetworkLogin: Boolean read FEmulateCanNetworkLogin write FEmulateCanNetworkLogin;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;

    // Interal Node manipulation
    procedure Clear;
    function AddNode(CdiXML: string; AutoLogin: Boolean): TLccNode; virtual;
    function AddNodeByClass(CdiXML: string; NodeClass: TLccNodeClass; AutoLogin: Boolean; NodeID: TNodeID): TLccNode; virtual;
    function FindNode(ANodeID: TNodeID): TLccNode; overload;
    function FindNode(AnAlias: Word): TLccNode; overload;
    function FindInitializedNode: TLccNode;

    procedure ReleaseAliasAll;

    procedure On_100msTimer(Sender: TObject);  virtual;
    procedure SendMessage(ALccMessage: TLccMessage; NeedsSourceNode: Boolean = False);

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

procedure TLccNodeManager.DispatchMessageCallback(ALccMessage: TLccMessage);
var
  i: Integer;
begin
  // This is called from the AliasServerThread from incoming connection and the Connection Factory which relays Sent Messages back through here to
  // dispatch to other virtual nodes so we need to make sure we don't send the message back to the node
  // that initially sent it.
  for i := 0 to Nodes.Count - 1 do
  begin
    if not EqualNodeID(Node[i].NodeID, ALccMessage.SourceID, False) then
      Node[i].ProcessMessage(ALccMessage);
  end;
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

constructor TLccNodeManager.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  FNodes := TList.Create;
  FWorkerMessage := TLccMessage.Create;

  _100msTimer := TLccTimer.Create(nil);
  _100msTimer.OnTimer := {$IFNDEF LCC_DELPHI}@{$ENDIF}On_100msTimer;
  _100msTimer.Interval := 100;
  _100msTimer.Enabled := True;

  AliasServerThread.DispatchProcessedMessageCallback := {$IFNDEF LCC_DELPHI}@{$ENDIF}DispatchMessageCallback;
  AliasServerThread.SendMessageCallback := {$IFNDEF LCC_DELPHI}@{$ENDIF}SendMessage;
end;

function TLccNodeManager.AddNode(CdiXML: string; AutoLogin: Boolean): TLccNode;
begin
  Result := TLccNode.Create(Self, CdiXML);
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
    Result := NodeClass.Create(Self, CdiXML);
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
  AliasServerThread.DispatchProcessedMessageCallback := nil;
  AliasServerThread.SendMessageCallback := nil;
  Clear;
  _100msTimer.Enabled := False;
  FNodes.Free;
  FreeAndNil(FWorkerMessage);
  FreeAndNil(F_100msTimer);
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

procedure TLccNodeManager.SendMessage(ALccMessage: TLccMessage; NeedsSourceNode: Boolean);
begin
  if NeedsSourceNode then
  begin;
    if Nodes.Count > 0 then
    begin
      ALccMessage.SourceAlias := Node[0].AliasID;
      ALccMessage.SourceID := Node[0].NodeID;
    end else
      Exit;
  end;
  ConnectionFactory.SendLccMessage(ALccMessage);
end;

end.

