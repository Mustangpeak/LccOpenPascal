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
  lcc_utilities,
  lcc_node_messages,
  lcc_train_server,
  lcc_alias_server;


const
  MAX_HARDWARE_CONNECTIONS = 10;  // Lazy here to make it dynamic to use with SMS should never ever need more than 10

type

  // This is how the Node Manager has access to the Connection Links with needing
  // to have access the objects associated with the Links.  The Links have a
  // pointer to TLccNodeManager so this allows us to not have to have a single unit
  // to have visibility back and forth.
  IHardwareConnectionManagerLink = interface
    ['{619C8E64-69C3-94A6-B6FE-B16B6CB57A45}']
    procedure SendMessage(AMessage: TLccMessage);
    function IsLccLink: Boolean;
    function GetConnected: Boolean;
//    if the node manager has no more active threads in its links then it should free all nodes and alias maps since it is no longer connected to any LCC networks and everythinbg is stale.
  end;

  { INodeManagerCallbacks }

  INodeManagerCallbacks = interface
    ['{C6920bCA-08BC-4D45-B27C-174640FA3106}']
    procedure DoAliasIDChanged(LccNode: TLccNode);
    procedure DoAliasMappingChange(LccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
    procedure DoAliasReset(LccNode: TLccNode);
    procedure DoCDIRead(LccNode: TLccNode);
    procedure DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode; AddressSpace: Byte);
    procedure DoConfigMemOptionsReply(LccNode: TLccNode);
    procedure DoConfigMemReadReply(LccNode: TLccNode);
    procedure DoConfigMemWriteReply(LccNode: TLccNode);
    procedure DoConsumerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
    procedure DoCreateLccNode(LccNode: TLccNode);
    procedure DoDatagramReply(LccNode: TLccNode);
    procedure DoDestroyLccNode(LccNode: TLccNode);
    procedure DoInitializationComplete(LccNode: TLccNode);
    procedure DoLogInNode(LccNode: TLccNode);
    procedure DoNodeIDChanged(LccNode: TLccNode);
    procedure DoOptionalInteractionRejected(LccNode: TLccNode; LccMessage: TLccMessage);
    procedure DoProducerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
    procedure DoProtocolIdentifyReply(LccNode: TLccNode; LccMessage: TLccMessage);
    procedure DoRemoteButtonReply(LccNode: TLccNode; LccMessage: TLccMessage);
    procedure DoSimpleNodeIdentReply(LccNode: TLccNode; LccMessage: TLccMessage);
    procedure DoVerifiedNodeID(LccNode: TLccNode; LccMessage: TLccMessage; VerifiedNode: TNodeID);
  end;

  { INodeManagerTractionCallbacks }

  INodeManagerTractionCallbacks = interface
    ['{3FEB8AAB-060F-CD7E-1747-22D6E6F6FBC7}']
    procedure DoTractionControllerAssign(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionControllerQuery(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionControllerChangedNotify(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionControllerChangingNotify(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionFDI(LccNode: TLccNode);
    procedure DoTractionFunctionConfiguration(LccNode: TLccNode);
    procedure DoTractionListenerAttach(LccNode: TLccNode; ListenerNode: TNodeID; Flags: Byte);
    procedure DoTractionListenerDetach(LccNode: TLccNode; ListenerNode: TNodeID; Flags: Byte);
    procedure DoTractionListenerQuery(LccNode: TLccNode; Index: Integer);
    procedure DoTractionManage(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure DoTractionProducerIsTrainIdentified(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionSimpleTrainNodeIdentReply(LccNode: TLccNode);
    procedure DoTractionUpdateSNIP(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionUpdateTrainSNIP(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionUpdateListenerCount(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionQuerySpeed(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionQueryFunction(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionRegisteringChange(LccNode: TLccNode; TractionObject: TLccTractionObject; IsRegistered: Boolean);
  end;

type

  TOnLccNodeMessage = procedure(Sender: TObject; ALccNode: TLccNode) of object;
  TOnLccNodeMessageReply = procedure(Sender: TObject; ALccNode: TLccNode; LccMessage: TLccMessage) of object;
  TOnLccNodeVerfiedNodeIDMessage = procedure(Sender: TObject; ALccNode: TLccNode; LccMessage: TLccMessage; VerifiedNode: TNodeID) of object;
  TOnLccNodeEventIdentified = procedure(Sender: TObject; Lccnode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState) of object;
  TOnLccNodeMessageResultCode = procedure(Sender: TObject; LccNode: TLccNode; LccMessage: TLccMessage; ResultCode: Byte) of object;
  TOnLccNodeConfigMemAddressSpace = procedure(Sender: TObject; LccNode: TLccNode; AddressSpace: Byte) of object;
  TOnLccNodeMessageWithReply = procedure(Sender: TObject; ALccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean) of object;  // TODO:
  TOnLccNodeMessageWithTractionObject = procedure(Sender: TObject; ALccNode: TLccNode; TractionObject: TLccTractionObject) of object;
  TOnLccNodeListenerQuery = procedure(Sender: TObject; ALccNode: TLccNode; Index: Integer) of object;
  TOnLccTractionUpdateSNIP = procedure(Sender: TObject; ALccNode: TLccNode; TractionObject: TLccTractionObject) of object;
  TOnLccTractionUpdateTrainSNIP = procedure(Sender: TObject; ALccNode: TLccNode; TractionObject: TLccTractionObject) of object;
  TOnLccTractionUpdateListenerCount = procedure(Sender: TObject; ALccNode: TLccNode; TractionObject: TLccTractionObject) of object;
  TOnAliasMappingChange = procedure(Sender: TObject; ALccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean) of object;
  TOnTrainRegisteringChange = procedure(Sender: TObject; ALccNode: TLccNode; TractionObject: TLccTractionObject; IsRegistered: Boolean) of object;
  TOnTrainInformationChange = procedure(Sender: TObject; ALccNode: TLccNode; TractionObject: TLccTractionObject) of object;
  TOnTractionProducerIsTrainIdentified = procedure(Sender: TObject; TractionObject: TLccTractionObject) of object;
  TOnTractionListener = procedure (Sender: TObject; LccNode: TLccNode; ListenerNode: TNodeID; Flags: Byte) of object;

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
    FDestAlias: Word;
    FDestNodeID: TNodeID;
    FMessages: TThreadList;
    FOwner: TLccNodeManager;
    FReceivedMessage: TLccMessage;
    FSourceAlias: Word;
    FSourceNodeID: TNodeID;
    FWorkerMessage: TLccMessage;
  protected
    property ReceivedMessage: TLccMessage read FReceivedMessage write FReceivedMessage;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property SourceAlias: Word read FSourceAlias write FSourceAlias;
    property DestAlias: Word read FDestAlias write FDestAlias;
    property SourceNodeID: TNodeID read FSourceNodeID write FSourceNodeID;
    property DestNodeID: TNodeID read FDestNodeID write FDestNodeID;

    procedure Execute; override;
    procedure ReceiveMessage;   // Syncronize Method
    procedure SendMessage;      // Syncronize Method
    function RequestMapping(AMessage: TLccMessage): Boolean;
    procedure UpdateGlobalMappings(AMessage: TLccMessage);
    function ValidateMapping(AMessage: TLccMessage): Boolean;

  public
    property Messages: TThreadList read FMessages write FMessages;
    property Owner: TLccNodeManager read FOwner write FOwner;

    procedure AddMessage(AMessage: TLccMessage);
  end;

  { TLccNodeManager }

  TLccNodeManager = class(TComponent, INodeManagerCallbacks, INodeManagerTractionCallbacks)
  private
    FGridConnect: Boolean;
    FHardwarewareConnectionList: TInterfaceList;
    FNodes: TList;
    FOnAliasMappingChange: TOnAliasMappingChange;
    FOnAliasReset: TOnLccNodeMessage;
    FOnNodeAliasIDChanged: TOnLccNodeMessage;
    FOnNodeCDI: TOnLccNodeMessage;
    FOnNodeConfigMemAddressSpaceInfoReply: TOnLccNodeConfigMemAddressSpace;
    FOnNodeConfigMemOptionsReply: TOnLccNodeMessage;
    FOnNodeConfigMemReadReply: TOnLccNodeMessage;
    FOnNodeConfigMemWriteReply: TOnLccNodeMessage;
    FOnNodeConsumerIdentified: TOnLccNodeEventIdentified;
    FOnNodeCreate: TOnLccNodeMessage;
    FOnNodeDatagramReply: TOnLccNodeMessage;
    FOnNodeDestroy: TOnLccNodeMessage;
    FOnNodeFDI: TOnLccNodeMessage;
    FOnNodeFunctionConfiguration: TOnLccNodeMessage;
    FOnNodeIDChanged: TOnLccNodeMessage;
    FOnNodeInitializationComplete: TOnLccNodeMessage;
    FOnNodeLogin: TOnLccNodeMessage;
    FOnNodeOptionalInteractionRejected: TOnLccNodeMessageReply;
    FOnNodeProducerIdentified: TOnLccNodeEventIdentified;
    FOnNodeProtocolIdentifyReply: TOnLccNodeMessageReply;
    FOnNodeRemoteButtonReply: TOnLccNodeMessageReply;
    FOnNodeSimpleNodeIdentReply: TOnLccNodeMessageReply;
    FOnNodeSimpleTrainNodeIdentReply: TOnLccNodeMessage;
    FOnNodeTractionControllerChangeNotify: TOnLccNodeMessageWithTractionObject;
    FOnNodeTractionControllerChangingNotify: TOnLccNodeMessageWithTractionObject;
    FOnNodeTractionControllerConfig: TOnLccNodeMessageWithTractionObject;
    FOnNodeTractionControllerQuery: TOnLccNodeMessageWithTractionObject;
    FOnNodeTractionListenerAttach: TOnTractionListener;
    FOnNodeTractionListenerDetach: TOnTractionListener;
    FOnNodeTractionListenerQuery: TOnLccNodeListenerQuery;
    FOnNodeTractionManage: TOnLccNodeMessageWithReply;
    FOnNodeTractionQueryFunction: TOnLccNodeMessageWithTractionObject;
    FOnNodeTractionQuerySpeed: TOnLccNodeMessageWithTractionObject;
    FOnNodeVerifiedNodeID: TOnLccNodeVerfiedNodeIDMessage;
    FOnTractionProducerIsTrainIdentified: TOnTractionProducerIsTrainIdentified;
    FOnTractionUpdateListenerCount: TOnLccTractionUpdateListenerCount;
    FOnTractionUpdateSNIP: TOnLccTractionUpdateSNIP;
    FOnTractionUpdateTrainSNIP: TOnLccTractionUpdateTrainSNIP;
    FOnTrainInformationChange: TOnTrainInformationChange;
    FOnTrainRegisteringChange: TOnTrainRegisteringChange;
    FReceiveMessageServerThread: TReceiveMessageServerThread;
    FWorkerMessage: TLccMessage;
    function GetNode(Index: Integer): TLccNode;
  protected

    // INodeManagerCallbacks
    procedure DoAliasIDChanged(LccNode: TLccNode);     // Check
    procedure DoAliasMappingChange(LccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);  // Check
    procedure DoAliasReset(LccNode: TLccNode);
    procedure DoCDIRead(LccNode: TLccNode);    // TODO  Necessary?
    procedure DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode; AddressSpace: Byte);   // TODO  Necessary?
    procedure DoConfigMemOptionsReply(LccNode: TLccNode);   // TODO  Necessary?
    procedure DoConfigMemReadReply(LccNode: TLccNode);  // TODO  Necessary?
    procedure DoConfigMemWriteReply(LccNode: TLccNode);  // TODO  Necessary?
    procedure DoConsumerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState); // Check
    procedure DoCreateLccNode(LccNode: TLccNode);   // Check
    procedure DoDatagramReply(LccNode: TLccNode);   // TODO Necessary?
    procedure DoDestroyLccNode(LccNode: TLccNode);  // Check
    procedure DoInitializationComplete(LccNode: TLccNode);  // Check
    procedure DoLogInNode(LccNode: TLccNode);   // Check
    procedure DoNodeIDChanged(LccNode: TLccNode);  // Check
    procedure DoOptionalInteractionRejected(LccNode: TLccNode; LccMessage: TLccMessage);  // Check
    procedure DoProducerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);   // Check
    procedure DoProtocolIdentifyReply(LccNode: TLccNode; LccMessage: TLccMessage); // TODO  Necessary?
    procedure DoRemoteButtonReply(LccNode: TLccNode; LccMessage: TLccMessage);  // TODO  Necessary?
    procedure DoSimpleNodeIdentReply(LccNode: TLccNode; LccMessage: TLccMessage); // Check
    procedure DoVerifiedNodeID(LccNode: TLccNode; LccMessage: TLccMessage; VerifiedNode: TNodeID);


    // INodeManagerTractionCallbacks
    procedure DoTractionControllerAssign(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionControllerQuery(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionControllerChangedNotify(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionControllerChangingNotify(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionFDI(LccNode: TLccNode);
    procedure DoTractionFunctionConfiguration(LccNode: TLccNode);
    procedure DoTractionListenerAttach(LccNode: TLccNode; ListenerNode: TNodeID; Flags: Byte);
    procedure DoTractionListenerDetach(LccNode: TLccNode; ListenerNode: TNodeID; Flags: Byte);
    procedure DoTractionListenerQuery(LccNode: TLccNode; Index: Integer);
    procedure DoTractionManage(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure DoTractionProducerIsTrainIdentified(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionSimpleTrainNodeIdentReply(LccNode: TLccNode);
    procedure DoTractionUpdateSNIP(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionUpdateTrainSNIP(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionUpdateListenerCount(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionQuerySpeed(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionQueryFunction(LccNode: TLccNode; TractionObject: TLccTractionObject);
    procedure DoTractionRegisteringChange(LccNode: TLccNode; TractionObject: TLccTractionObject; IsRegistered: Boolean);

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


  published

    // Node Management
    property OnNodeCreate: TOnLccNodeMessage read FOnNodeCreate write FOnNodeCreate;
    property OnNodeDestroy: TOnLccNodeMessage read FOnNodeDestroy write FOnNodeDestroy;
    property OnNodeLogin: TOnLccNodeMessage read FOnNodeLogin write FOnNodeLogin;
    property OnNodeIDChanged: TOnLccNodeMessage read FOnNodeIDChanged write FOnNodeIDChanged;
    property OnNodeInitializationComplete: TOnLccNodeMessage read FOnNodeInitializationComplete write FOnNodeInitializationComplete;
    property OnNodeVerifiedNodeID: TOnLccNodeVerfiedNodeIDMessage read FOnNodeVerifiedNodeID write FOnNodeVerifiedNodeID;
    property OnNodeProtocolIdentifyReply: TOnLccNodeMessageReply read FOnNodeProtocolIdentifyReply write FOnNodeProtocolIdentifyReply;

    // CAN Node Management
    property OnNodeAliasIDChanged: TOnLccNodeMessage read FOnNodeAliasIDChanged write FOnNodeAliasIDChanged;
    property OnAliasReset: TOnLccNodeMessage read FOnAliasReset write FOnAliasReset;

    // Configuration Memory Information
    property OnNodeConfigMemAddressSpaceInfoReply: TOnLccNodeConfigMemAddressSpace read FOnNodeConfigMemAddressSpaceInfoReply write FOnNodeConfigMemAddressSpaceInfoReply;
    property OnNodeConfigMemOptionsReply: TOnLccNodeMessage read FOnNodeConfigMemOptionsReply write FOnNodeConfigMemOptionsReply;

    // Configuration Memory Access
    property OnNodeCDI: TOnLccNodeMessage read FOnNodeCDI write FOnNodeCDI;
    property OnNodeConfigMemReadReply: TOnLccNodeMessage read FOnNodeConfigMemReadReply write FOnNodeConfigMemReadReply;
    property OnNodeConfigMemWriteReply: TOnLccNodeMessage read FOnNodeConfigMemWriteReply write FOnNodeConfigMemWriteReply;

    // Events
    property OnNodeConsumerIdentified: TOnLccNodeEventIdentified read FOnNodeConsumerIdentified write FOnNodeConsumerIdentified;
    property OnNodeProducerIdentified: TOnLccNodeEventIdentified read FOnNodeProducerIdentified write FOnNodeProducerIdentified;

    // SNIP
    property OnNodeSimpleNodeIdentReply: TOnLccNodeMessageReply read FOnNodeSimpleNodeIdentReply write FOnNodeSimpleNodeIdentReply;

    // Datagrams
    property OnNodeDatagramReply: TOnLccNodeMessage read FOnNodeDatagramReply write FOnNodeDatagramReply;

    // Traction
    property OnNodeSimpleTrainNodeIdentReply: TOnLccNodeMessage read FOnNodeSimpleTrainNodeIdentReply write FOnNodeSimpleTrainNodeIdentReply;
    property OnNodeTractionQuerySpeed: TOnLccNodeMessageWithTractionObject read FOnNodeTractionQuerySpeed write FOnNodeTractionQuerySpeed;
    property OnNodeTractionQueryFunction: TOnLccNodeMessageWithTractionObject read FOnNodeTractionQueryFunction write FOnNodeTractionQueryFunction;
    property OnNodeTractionControllerConfig: TOnLccNodeMessageWithTractionObject read FOnNodeTractionControllerConfig write FOnNodeTractionControllerConfig;
    property OnNodeTractionControllerQuery: TOnLccNodeMessageWithTractionObject read FOnNodeTractionControllerQuery write FOnNodeTractionControllerQuery;
    property OnNodeTractionControllerChangeNotify: TOnLccNodeMessageWithTractionObject read FOnNodeTractionControllerChangeNotify write FOnNodeTractionControllerChangeNotify;
    property OnNodeTractionControllerChangingNotify: TOnLccNodeMessageWithTractionObject read FOnNodeTractionControllerChangingNotify write FOnNodeTractionControllerChangingNotify;

    property OnNodeTractionManage: TOnLccNodeMessageWithReply read FOnNodeTractionManage write FOnNodeTractionManage;
    property OnNodeTractionListenerAttach: TOnTractionListener read FOnNodeTractionListenerAttach write FOnNodeTractionListenerAttach;
    property OnNodeTractionListenerDetach: TOnTractionListener read FOnNodeTractionListenerDetach write FOnNodeTractionListenerDetach;
    property OnNodeTractionListenerQuery: TOnLccNodeListenerQuery read FOnNodeTractionListenerQuery write FOnNodeTractionListenerQuery;
    property OnTractionUpdateSNIP: TOnLccTractionUpdateSNIP read FOnTractionUpdateSNIP write FOnTractionUpdateSNIP;
    property OnTractionUpdateTrainSNIP: TOnLccTractionUpdateTrainSNIP read FOnTractionUpdateTrainSNIP write FOnTractionUpdateTrainSNIP;
    property OnTractionUpdateListenerCount: TOnLccTractionUpdateListenerCount read FOnTractionUpdateListenerCount write FOnTractionUpdateListenerCount;
    property OnTractionProducerIsTrainIdentified: TOnTractionProducerIsTrainIdentified read FOnTractionProducerIsTrainIdentified write FOnTractionProducerIsTrainIdentified;

    // Traction DCC Functions
    property OnNodeFDI: TOnLccNodeMessage read FOnNodeFDI write FOnNodeFDI;
    property OnNodeFunctionConfiguration: TOnLccNodeMessage read FOnNodeFunctionConfiguration write FOnNodeFunctionConfiguration;

    // Other stuff that may not be useful
    property OnNodeOptionalInteractionRejected: TOnLccNodeMessageReply read FOnNodeOptionalInteractionRejected write FOnNodeOptionalInteractionRejected;
    property OnNodeRemoteButtonReply: TOnLccNodeMessageReply read FOnNodeRemoteButtonReply write FOnNodeRemoteButtonReply;

    // Other interesting stuff
    property OnAliasMappingChange: TOnAliasMappingChange read FOnAliasMappingChange write FOnAliasMappingChange;
    property OnTrainRegisteringChange: TOnTrainRegisteringChange read FOnTrainRegisteringChange write FOnTrainRegisteringChange;
    property OnTrainInformationChange: TOnTrainInformationChange read FOnTrainInformationChange write FOnTrainInformationChange;
  end;


implementation

{ TReceiveMessageServerThread }

procedure TReceiveMessageServerThread.Execute;
var
  LocalList, SideList: TList;
  i: Integer;
begin
  Messages := TThreadList.Create;
  SideList := TList.Create;
  WorkerMessage := TLccMessage.Create;
  try
    while not Terminated do
    begin
      LocalList := Messages.LockList;
      try

        // Need to run from 0 up to maintain order
        for i := 0 to LocalList.Count - 1 do
        begin
          if Owner.GridConnect then
          begin
            UpdateGlobalMappings(TLccMessage( LocalList[i]));
            if ValidateMapping(TLccMessage( LocalList[i])) then
            begin // all good, we all the required mappings for this message
              SideList.Add(LocalList[i]);
              LocalList[i] := nil;
            end else  // not so good... need some mappings to deal with this message
            begin
              if not RequestMapping(TLccMessage( LocalList[i])) then
              begin // Tried but message contains nodes we can't reach
                TLccMessage( LocalList[i]).Free;
                LocalList[i] := nil;
              end;
            end;
          end else
          begin
            SideList.Add(LocalList[i]);
          end;

        end;

        if Owner.GridConnect then
        begin
          // Delete the messages that were removed
          for i := LocalList.Count - 1 downto 0 do
          begin
            if LocalList[i] = nil then
              LocalList.Delete(i);
          end;
        end else
          LocalList.Clear;

      finally
        Messages.UnlockList;
      end;

      try
        for i := 0 to SideList.Count - 1 do
        begin
          if not Terminated then
          try
            ReceivedMessage := TLccMessage(SideList[i]);
            Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessage);
          except
          end;
        end;
      finally
        try
          for i := 0 to SideList.Count - 1 do
            TObject(SideList[i]).Free;
        finally
          SideList.Clear
        end;
      end;
      Sleep(10);
    end;
  finally
    LocalList := Messages.LockList;
    try
      for i := 0 to LocalList.Count - 1 do
        TObject(LocalList[i]).Free
    finally
      LocalList.Clear;
      Messages.UnlockList;
      Messages.Free
    end;

    try
      for i := 0 to SideList.Count - 1 do
        TObject(SideList[i]).Free
    finally
      SideList.Free;
      WorkerMessage.Free;
    end;
  end;
end;

procedure TReceiveMessageServerThread.ReceiveMessage;
// This is called through Syncronize... The main Message Queue is not locked so more messages can be added
// but this will dead lock here and messages to the nodes in the application will starve plus you can't
// Send a message then wait here for it.. it will get placed in the main Message Queue but this won't be called
// until it returns.
var
  i: Integer;
  LocalNode: TLccNode;
begin
  LocalNode := nil;
  if Owner.GridConnect then
    LocalNode := Owner.FindNode(ReceivedMessage.CAN.SourceAlias)
  else
    LocalNode := Owner.FindNode(ReceivedMessage.SourceID);

  for i := 0 to Owner.Nodes.Count - 1 do
  begin
    // Don't send the message back to the node that created it
    if LocalNode <> TLccNode(Owner.Nodes[i]) then
      TLccNode(Owner.Nodes[i]).ProcessMessage(ReceivedMessage)
  end;
end;

procedure TReceiveMessageServerThread.SendMessage;
var
  LocalNode: TLccNode;
begin
  LocalNode := Owner.FindInitializedNode;
  if Assigned(WorkerMessage) then
  begin
    if DestAlias > 0 then
    begin
      WorkerMessage.LoadVerifyNodeIDAddressed(LocalNode.NodeID, LocalNode.AliasID, DestNodeID, DestAlias, NULL_NODE_ID);
      Owner.SendMessage(LocalNode, WorkerMessage);
    end else
    if not NullNodeID(DestNodeID) then
    begin
      WorkerMessage.LoadVerifyNodeID(LocalNode.NodeID, LocalNode.AliasID, DestNodeID);
      Owner.SendMessage(LocalNode, WorkerMessage);
    end;
  end;
end;

function TReceiveMessageServerThread.RequestMapping(AMessage: TLccMessage): Boolean;
var
  i: Integer;
begin
  Result := not AMessage.NodeIdentifications.RetryCountMaxedOut(5);

  if Result then
  begin
    for i := 0 to AMessage.NodeIdentifications.Count - 1 do
    begin
      if AMessage.NodeIdentifications[i].Valid then
      begin
        DestAlias := AMessage.NodeIdentifications[i].Alias;
        DestNodeID := AMessage.NodeIdentifications[i].NodeID;
        Synchronize({$IFDEF FPC}@{$ENDIF}SendMessage);
        AMessage.NodeIdentifications[i].RetryCount := AMessage.NodeIdentifications[i].RetryCount + 1;
      end;
    end;
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
        AliasServer.AddMapping(AMessage.CAN.SourceAlias, AMessage.ExtractDataBytesAsNodeID(0, LocalNodeID));
      end;
  end;

  case AMessage.MTI of
    MTI_VERIFIED_NODE_ID_NUMBER,
    MTI_INITIALIZATION_COMPLETE :
      begin
        LocalNodeID := NULL_NODE_ID;
        AliasServer.AddMapping(AMessage.CAN.SourceAlias, AMessage.ExtractDataBytesAsNodeID(0, LocalNodeID));
      end;
  end;
end;

function TReceiveMessageServerThread.ValidateMapping(AMessage: TLccMessage): Boolean;
var
  LocalNodeIdentificationObjectList: TLccNodeIdentificationObjectList;
  LocalAliasMapping: TLccAliasMapping;
  i: Integer;
begin
  Result := True;

  // CAN messages includes messages for allocating the alias so we can't expect a valid
  // alias under all conditions for CAN messages... just ignore them and let them do their job.
  if not AMessage.IsCAN then
  begin
    LocalNodeIdentificationObjectList := AMessage.ExtractNodeIdentifications(False);
    for i := 0 to LocalNodeIdentificationObjectList.Count - 1 do
    begin
      // An index may not be valid... i.e. if a node ID is carried in an event there won't be a destination
      if LocalNodeIdentificationObjectList[i].Valid then
      begin
        if LocalNodeIdentificationObjectList[i].Alias <> 0 then
          LocalAliasMapping := AliasServer.FindMapping(LocalNodeIdentificationObjectList[i].Alias)
        else
        if not NullNodeID(LocalNodeIdentificationObjectList[i].NodeID) then
          LocalAliasMapping := AliasServer.FindMapping(LocalNodeIdentificationObjectList[i].NodeID)
        else
          raise ENodeIdentificationObjectIsNull.Create('Received Message Alias/Node Extractions failed');

        if not Assigned(LocalAliasMapping) then
        begin // Gotta do something..... but what.......  or do I do this during the transfer to the local list
              //  in the thread and not transfer it if there are ID's to be mapped?
          {$IFDEF WriteLnDebug}
          AliasServer.WriteMapping('Cound not find Mapping at Message Position: ' +
            IntToStr(i) +
            ' - Alias' +
            IntToHex(LocalNodeIdentificationObjectList[i].Alias, 4) + ' ID: ' +
            NodeIDToString(LocalNodeIdentificationObjectList[i].NodeID, True),
            nil);
          {$ENDIF}

          Result := False;

        end;
      end;
    end;
  end;
end;

procedure TReceiveMessageServerThread.AddMessage(AMessage: TLccMessage);
var
  LocalList: TList;
begin
  LocalList := Messages.LockList;
  try
    LocalList.Add(AMessage.Clone);
  finally
    Messages.UnlockList;
  end;
end;

{ TLccNodeManager }

procedure TLccNodeManager.DoAliasIDChanged(LccNode: TLccNode);
begin
  if Assigned(OnNodeAliasIDChanged) then
    OnNodeAliasIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoAliasReset(LccNode: TLccNode);
begin
   if Assigned(FOnAliasReset) then
     FOnAliasReset(Self, LccNode);
end;

procedure TLccNodeManager.DoCDIRead(LccNode: TLccNode);
begin
  if Assigned(OnNodeCDI) then
    OnNodeCDI(Self, LccNode)
end;

procedure TLccNodeManager.DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode; AddressSpace: Byte);
begin
 if Assigned(OnNodeConfigMemAddressSpaceInfoReply) then
   OnNodeConfigMemAddressSpaceInfoReply(Self, LccNode, AddressSpace);
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

procedure TLccNodeManager.DoConsumerIdentified(LccNode: TLccNode;
  LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
begin
  if Assigned(OnNodeConsumerIdentified) then
    OnNodeConsumerIdentified(Self, LccNode, LccMessage, Event, State);
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

procedure TLccNodeManager.DoTractionFDI(LccNode: TLccNode);
begin
  if Assigned(OnNodeFDI) then
    OnNodeFDI(Self, LccNode)
end;

procedure TLccNodeManager.DoTractionFunctionConfiguration(LccNode: TLccNode);
begin
  if Assigned(OnNodeFunctionConfiguration) then
    OnNodeFunctionConfiguration(Self, LccNode)
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

procedure TLccNodeManager.DoProducerIdentified(LccNode: TLccNode;
  LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
begin
  if Assigned(OnNodeProducerIdentified) then
    OnNodeProducerIdentified(Self, LccNode, LccMessage, Event, State);
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

procedure TLccNodeManager.DoTractionSimpleTrainNodeIdentReply(LccNode: TLccNode);
begin
  if Assigned(OnNodeSimpleTrainNodeIdentReply) then
    OnNodeSimpleTrainNodeIdentReply(Self, LccNode);
end;

procedure TLccNodeManager.DoTractionQuerySpeed(LccNode: TLccNode; TractionObject: TLccTractionObject);
begin
  if Assigned(OnNodeTractionQuerySpeed) then
    OnNodeTractionQuerySpeed(Self, LccNode, TractionObject);
end;

procedure TLccNodeManager.DoTractionQueryFunction(LccNode: TLccNode; TractionObject: TLccTractionObject);
begin
  if Assigned(OnNodeTractionQueryFunction) then
    OnNodeTractionQueryFunction(Self, LccNode, TractionObject);
end;

procedure TLccNodeManager.DoTractionControllerAssign(LccNode: TLccNode; TractionObject: TLccTractionObject);
begin
  if Assigned(OnNodeTractionControllerConfig) then
    OnNodeTractionControllerConfig(Self, LccNode, TractionObject);
end;

procedure TLccNodeManager.DoTractionControllerQuery(LccNode: TLccNode; TractionObject: TLccTractionObject);
begin
  if Assigned(OnNodeTractionControllerQuery) then
    OnNodeTractionControllerQuery(Self, LccNode, TractionObject);
end;

procedure TLccNodeManager.DoTractionControllerChangedNotify(LccNode: TLccNode; TractionObject: TLccTractionObject);
begin
  if Assigned(OnNodeTractionControllerChangeNotify) then
    OnNodeTractionControllerChangeNotify(Self, LccNode, TractionObject);
end;

procedure TLccNodeManager.DoTractionControllerChangingNotify(LccNode: TLccNode; TractionObject: TLccTractionObject);
begin
  if Assigned(OnNodeTractionControllerChangingNotify) then
    OnNodeTractionControllerChangingNotify(Self, LccNode, TractionObject);
end;

procedure TLccNodeManager.DoTractionListenerAttach(LccNode: TLccNode;
  ListenerNode: TNodeID; Flags: Byte);
begin
  if Assigned(OnNodeTractionListenerAttach) then
    OnNodeTractionListenerAttach(Self, LccNode, ListenerNode, Flags);
end;

procedure TLccNodeManager.DoTractionListenerDetach(LccNode: TLccNode; ListenerNode: TNodeID; Flags: Byte);
begin
  if Assigned(OnNodeTractionListenerDetach) then
    OnNodeTractionListenerDetach(Self, LccNode, ListenerNode, Flags);
end;

procedure TLccNodeManager.DoTractionListenerQuery(LccNode: TLccNode;
  Index: Integer);
begin
  if Assigned(OnNodeTractionListenerQuery) then
     OnNodeTractionListenerQuery(Self, LccNode, Index);
end;

procedure TLccNodeManager.DoTractionManage(LccNode: TLccNode;
  LccMessage: TLccMessage; IsReply: Boolean);
begin
  if Assigned(OnNodeTractionManage) then
    OnNodeTractionManage(Self, LccNode, LccMessage, IsReply);
end;

procedure TLccNodeManager.DoTractionUpdateSNIP(LccNode: TLccNode; TractionObject: TLccTractionObject);
begin
  if Assigned(OnTractionUpdateSNIP) then
    OnTractionUpdateSNIP(Self, LccNode, TractionObject);
end;

procedure TLccNodeManager.DoTractionUpdateTrainSNIP(LccNode: TLccNode; TractionObject: TLccTractionObject);
begin
  if Assigned(OnTractionUpdateTrainSNIP) then
    OnTractionUpdateTrainSNIP(Self, LccNode, TractionObject);
end;

procedure TLccNodeManager.DoTractionUpdateListenerCount(LccNode: TLccNode; TractionObject: TLccTractionObject);
begin
  if Assigned(OnTractionUpdateListenerCount) then
    OnTractionUpdateListenerCount(Self, LccNode, TractionObject);
end;

procedure TLccNodeManager.DoTractionProducerIsTrainIdentified(LccNode: TLccNode; TractionObject: TLccTractionObject);
begin
  if Assigned(OnTractionProducerIsTrainIdentified) then
    OnTractionProducerIsTrainIdentified(Self, TractionObject);
end;

procedure TLccNodeManager.DoVerifiedNodeID(LccNode: TLccNode;
  LccMessage: TLccMessage; VerifiedNode: TNodeID);
begin
  if Assigned(OnNodeVerifiedNodeID) then
    OnNodeVerifiedNodeID(Self, LccNode, LccMessage, VerifiedNode);
end;

procedure TLccNodeManager.DoAliasMappingChange(LccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
begin
  if Assigned(OnAliasMappingChange) then
    OnAliasMappingChange(Self, LccNode, AnAliasMapping, IsMapped);
end;

procedure TLccNodeManager.DoTractionRegisteringChange(LccNode: TLccNode; TractionObject: TLccTractionObject; IsRegistered: Boolean);
begin
  if Assigned(OnTrainRegisteringChange) then
    OnTrainRegisteringChange(Self, LccNode, TractionObject, IsRegistered);
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
  FNodes.Free;
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FHardwarewareConnectionList);
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
  // Never filter these messages.  For the internal callback system to work correctly the internal nodes must
  // snoop on eachothers messages to keep things like the internal Train data base updated... that does mean there
  // is some future risk that if a train is found outside of this database the networks must not filter messages
  // based on where the destinstions segement exists.  The trains messages must be sent to all segments
  ReceiveMessageServerThread.AddMessage(LccMessage);  // I think this works....
end;

end.

