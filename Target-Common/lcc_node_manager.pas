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
    procedure DoAliasIDChanged(LccNode: TLccNode);               //*
    procedure DoCANAliasReset(LccNode: TLccNode);             //*
    procedure DoCDIRead(LccNode: TLccNode);
    procedure DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode; AddressSpace: Byte);
    procedure DoConfigMemOptionsReply(LccNode: TLccNode);
    procedure DoConfigMemReadReply(LccNode: TLccNode);
    procedure DoConfigMemWriteReply(LccNode: TLccNode);
    procedure DoCreateLccNode(LccNode: TLccNode);     //*
    procedure DoConsumerIdentify(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoConsumerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
    procedure DoDatagramReply(LccNode: TLccNode);
    procedure DoDestroyLccNode(LccNode: TLccNode);   //*
    procedure DoLogInNode(LccNode: TLccNode);
    procedure DoFDI(LccNode: TLccNode);
    procedure DoFunctionConfiguration(LccNode: TLccNode);
    procedure DoInitializationComplete(LccNode: TLccNode);   //*
    procedure DoNodeIDChanged(LccNode: TLccNode);                  //*
    procedure DoOptionalInteractionRejected(LccNode: TLccNode);
    procedure DoProducerIdentify(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoProducerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
    procedure DoProtocolIdentifyReply(LccNode: TLccNode);
    procedure DoRemoteButtonReply(LccNode: TLccNode);
    procedure DoSimpleNodeIdentReply(LccNode: TLccNode);
    procedure DoSimpleTrainNodeIdentReply(LccNode: TLccNode);

    procedure DoTractionQuerySpeed(LccNode: TLccNode; TrainObject: TLccTrainObject);
    procedure DoTractionQueryFunction(LccNode: TLccNode; TrainObject: TLccTrainObject);
    procedure DoTractionControllerAssign(LccNode: TLccNode; TrainObject: TLccTrainObject);
    procedure DoTractionControllerQuery(LccNode: TLccNode; TrainObject: TLccTrainObject);
    procedure DoTractionControllerChangedNotify(LccNode: TLccNode; TrainObject: TLccTrainObject);
    procedure DoTractionListenerAttach(LccNode: TLccNode; Listener: TNodeID; Flags: Byte);
    procedure DoTractionListenerDetach(LccNode: TLccNode; Listener: TNodeID; Flags: Byte);
    procedure DoTractionListenerQuery(LccNode: TLccNode; Index: Integer);
    procedure DoTractionUpdateSNIP(LccNode: TLccNode; TrainObject: TLccTrainObject);
    procedure DoTractionUpdateTrainSNIP(LccNode: TLccNode; TrainObject: TLccTrainObject);
    procedure DoTractionUpdateListenerCount(LccNode: TLccNode; TrainObject: TLccTrainObject);
    procedure DoTractionManage(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure DoVerifiedNodeID(LccNode: TLccNode);
    procedure DoAliasMappingChange(LccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
    procedure DoTrainRegisteringChange(LccNode: TLccNode; TrainObject: TLccTrainObject; IsRegistered: Boolean);
  end;

type

  TOnLccNodeMessage = procedure(Sender: TObject; LccSourceNode: TLccNode) of object;
  TOnLccNodeMessageWithDest = procedure(Sender: TObject; LccNode: TLccNode) of object;
  TOnLccNodeEventIdentify = procedure(Sender: TObject; LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean) of object;
  TOnLccNodeEventIdentified = procedure(Sender: TObject; Lccnode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState) of object;
  TOnLccNodeMessageResultCode = procedure(Sender: TObject; LccNode: TLccNode; LccMessage: TLccMessage; ResultCode: Byte) of object;
  TOnLccNodeConfigMem = procedure(Sender: TObject; LccNode: TLccNode) of object;
  TOnLccNodeConfigMemAddressSpace = procedure(Sender: TObject; LccNode: TLccNode; AddressSpace: Byte) of object;
  TOnLccNodeMessageWithReply = procedure(Sender: TObject; LccSourceNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean) of object;  // TODO:
  TOnLccNodeMessageWithTrainObject = procedure(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject) of object;
  TOnLccNodeListenerAttach = procedure(Sender: TObject; LccSourceNode: TLccNode; ListenerID: TNodeID; Flags: Byte) of object;
  TOnLccNodeListenerDetach = procedure(Sender: TObject; LccSourceNode: TLccNode; ListenerID: TNodeID; Flags: Byte) of object;
  TOnLccNodeListenerQuery = procedure(Sender: TObject; LccSourceNode: TLccNode; Index: Integer) of object;
  TOnLccTractionUpdateSNIP = procedure(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject) of object;
  TOnLccTractionUpdateTrainSNIP = procedure(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject) of object;
  TOnLccTractionUpdateListenerCount = procedure(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject) of object;
  TOnAliasMappingChange = procedure(Sender: TObject; LccSourceNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean) of object;
  TOnTrainRegisteringChange = procedure(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject; IsRegistered: Boolean) of object;
  TOnTrainInformationChange = procedure(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject) of object;

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

  TLccNodeManager = class(TComponent, INodeManagerCallbacks)
  private
    FGridConnect: Boolean;
    FHardwarewareConnectionList: TInterfaceList;
    FOnAliasMappingChange: TOnAliasMappingChange;
    FOnLccNodeAliasIDChanged: TOnLccNodeMessage;
    FOnLccNodeConfigMemAddressSpaceInfoReply: TOnLccNodeConfigMemAddressSpace;
    FOnLccNodeConfigMemOptionsReply: TOnLccNodeConfigMem;
    FOnLccNodeConsumerIdentify: TOnLccNodeEventIdentify;
    FOnLccNodeIDChanged: TOnLccNodeMessage;
    FOnLccCANAliasReset: TOnLccNodeMessage;
    FOnLccNodeCDI: TOnLccNodeMessageWithDest;
    FOnLccNodeConfigMemReadReply: TOnLccNodeConfigMem;
    FOnLccNodeConfigMemWriteReply: TOnLccNodeConfigMem;
    FOnLccNodeConsumerIdentified: TOnLccNodeEventIdentified;
    FOnLccNodeCreate: TOnLccNodeMessage;
    FOnLccNodeDatagramReply: TOnLccNodeMessageWithDest;
    FOnLccNodeDestroy: TOnLccNodeMessage;
    FOnLccNodeFDI: TOnLccNodeMessageWithDest;
    FOnLccNodeFunctionConfiguration: TOnLccNodeMessageWithDest;
    FOnLccNodeInitializationComplete: TOnLccNodeMessage;
    FOnLccNodeLogin: TOnLccNodeMessage;
    FOnLccNodeOptionalInteractionRejected: TOnLccNodeMessageWithDest;
    FOnLccNodeProducerIdentified: TOnLccNodeEventIdentified;
    FOnLccNodeProducerIdentify: TOnLccNodeEventIdentify;
    FOnLccNodeProtocolIdentifyReply: TOnLccNodeMessageWithDest;
    FOnLccNodeRemoteButtonReply: TOnLccNodeMessageWithDest;
    FOnLccNodeSimpleNodeIdentReply: TOnLccNodeMessageWithDest;
    FOnLccNodeSimpleTrainNodeIdentReply: TOnLccNodeMessageWithDest;
    FOnLccNodeTractionControllerChangeNotify: TOnLccNodeMessageWithTrainObject;
    FOnLccNodeTractionControllerConfig: TOnLccNodeMessageWithTrainObject;
    FOnLccNodeTractionControllerQuery: TOnLccNodeMessageWithTrainObject;
    FOnLccNodeTractionListenerAttach: TOnLccNodeListenerAttach;
    FOnLccNodeTractionListenerDetach: TOnLccNodeListenerDetach;
    FOnLccNodeTractionListenerQuery: TOnLccNodeListenerQuery;
    FOnLccNodeTractionManage: TOnLccNodeMessageWithReply;
    FOnLccNodeTractionQueryFunction: TOnLccNodeMessageWithTrainObject;
    FOnLccNodeTractionQuerySpeed: TOnLccNodeMessageWithTrainObject;
    FOnLccNodeVerifiedNodeID: TOnLccNodeMessage;
    FOnLccTractionUpdateListenerCount: TOnLccTractionUpdateListenerCount;
    FOnLccTractionUpdateSNIP: TOnLccTractionUpdateSNIP;
    FOnLccTractionUpdateTrainSNIP: TOnLccTractionUpdateTrainSNIP;
    FOnTrainInformationChange: TOnTrainInformationChange;
    FOnTrainRegisteringChange: TOnTrainRegisteringChange;
    FNodes: TList;
    FReceiveMessageServerThread: TReceiveMessageServerThread;
    FWorkerMessage: TLccMessage;
    function GetNode(Index: Integer): TLccNode;
  protected
    procedure DoAliasIDChanged(LccNode: TLccNode); virtual;               //*
    procedure DoCANAliasReset(LccNode: TLccNode); virtual;             //*
    procedure DoCDIRead(LccNode: TLccNode); virtual;
    procedure DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode; AddressSpace: Byte); virtual;
    procedure DoConfigMemOptionsReply(LccNode: TLccNode); virtual;
    procedure DoConfigMemReadReply(LccNode: TLccNode); virtual;
    procedure DoConfigMemWriteReply(LccNode: TLccNode); virtual;
    procedure DoCreateLccNode(LccNode: TLccNode); virtual;     //*
    procedure DoLogInNode(LccNode: TLccNode); virtual;         //*
    procedure DoConsumerIdentify(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoConsumerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState); virtual;
    procedure DoDatagramReply(LccNode: TLccNode); virtual;
    procedure DoDestroyLccNode(LccNode: TLccNode); virtual;   //*
    procedure DoFDI(LccNode: TLccNode); virtual;
    procedure DoFunctionConfiguration(LccNode: TLccNode); virtual;
    procedure DoInitializationComplete(LccNode: TLccNode); virtual;   //*
    procedure DoNodeIDChanged(LccNode: TLccNode); virtual;                  //*
    procedure DoOptionalInteractionRejected(LccNode: TLccNode); virtual;
    procedure DoProducerIdentify(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoProducerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState); virtual;
    procedure DoProtocolIdentifyReply(LccNode: TLccNode); virtual;
    procedure DoRemoteButtonReply(LccNode: TLccNode); virtual;
    procedure DoSimpleNodeIdentReply(LccNode: TLccNode); virtual;
    procedure DoSimpleTrainNodeIdentReply(LccNode: TLccNode); virtual;

    procedure DoTractionQuerySpeed(LccNode: TLccNode; TrainObject: TLccTrainObject); virtual;
    procedure DoTractionQueryFunction(LccNode: TLccNode; TrainObject: TLccTrainObject); virtual;
    procedure DoTractionControllerAssign(LccNode: TLccNode; TrainObject: TLccTrainObject); virtual;
    procedure DoTractionControllerQuery(LccNode: TLccNode; TrainObject: TLccTrainObject); virtual;
    procedure DoTractionControllerChangedNotify(LccNode: TLccNode; TrainObject: TLccTrainObject); virtual;

    procedure DoTractionListenerAttach(LccNode: TLccNode; Listener: TNodeID; Flags: Byte); virtual;
    procedure DoTractionListenerDetach(LccNode: TLccNode; Listener: TNodeID; Flags: Byte); virtual;
    procedure DoTractionListenerQuery(LccNode: TLccNode; Index: Integer); virtual;

    procedure DoTractionManage(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean); virtual;
    procedure DoTractionUpdateSNIP(LccNode: TLccNode; TrainObject: TLccTrainObject); virtual;
    procedure DoTractionUpdateTrainSNIP(LccNode: TLccNode; TrainObject: TLccTrainObject); virtual;
    procedure DoTractionUpdateListenerCount(LccNode: TLccNode; TrainObject: TLccTrainObject); virtual;

    procedure DoVerifiedNodeID(LccNode: TLccNode); virtual;
    procedure DoAliasMappingChange(LccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean); virtual;
    procedure DoTrainRegisteringChange(LccNode: TLccNode; TrainObject: TLccTrainObject; IsRegistered: Boolean); virtual;
    procedure DoTrainInformationChange(LccNode: TLccNode; TrainObject: TLccTrainObject); virtual;

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
    property OnLccNodeCreate: TOnLccNodeMessage read FOnLccNodeCreate write FOnLccNodeCreate;
    property OnLccNodeDestroy: TOnLccNodeMessage read FOnLccNodeDestroy write FOnLccNodeDestroy;
    property OnLccNodeLogin: TOnLccNodeMessage read FOnLccNodeLogin write FOnLccNodeLogin;
    property OnLccNodeIDChanged: TOnLccNodeMessage read FOnLccNodeIDChanged write FOnLccNodeIDChanged;
    property OnLccNodeInitializationComplete: TOnLccNodeMessage read FOnLccNodeInitializationComplete write FOnLccNodeInitializationComplete;
    property OnLccNodeVerifiedNodeID: TOnLccNodeMessage read FOnLccNodeVerifiedNodeID write FOnLccNodeVerifiedNodeID;
    property OnLccNodeProtocolIdentifyReply: TOnLccNodeMessageWithDest read FOnLccNodeProtocolIdentifyReply write FOnLccNodeProtocolIdentifyReply;

    // CAN Node Management
    property OnLccNodeAliasIDChanged: TOnLccNodeMessage read FOnLccNodeAliasIDChanged write FOnLccNodeAliasIDChanged;
    property OnLccCANAliasReset: TOnLccNodeMessage read FOnLccCANAliasReset write FOnLccCANAliasReset;

    // Configuration Memory Information
    property OnLccNodeConfigMemAddressSpaceInfoReply: TOnLccNodeConfigMemAddressSpace read FOnLccNodeConfigMemAddressSpaceInfoReply write FOnLccNodeConfigMemAddressSpaceInfoReply;
    property OnLccNodeConfigMemOptionsReply: TOnLccNodeConfigMem read FOnLccNodeConfigMemOptionsReply write FOnLccNodeConfigMemOptionsReply;

    // Configuration Memory Access
    property OnLccNodeCDI: TOnLccNodeMessageWithDest read FOnLccNodeCDI write FOnLccNodeCDI;
    property OnLccNodeConfigMemReadReply: TOnLccNodeConfigMem read FOnLccNodeConfigMemReadReply write FOnLccNodeConfigMemReadReply;
    property OnLccNodeConfigMemWriteReply: TOnLccNodeConfigMem read FOnLccNodeConfigMemWriteReply write FOnLccNodeConfigMemWriteReply;

    // Events
    property OnLccNodeConsumerIdentify: TOnLccNodeEventIdentify read FOnLccNodeConsumerIdentify write FOnLccNodeConsumerIdentify;
    property OnLccNodeConsumerIdentified: TOnLccNodeEventIdentified read FOnLccNodeConsumerIdentified write FOnLccNodeConsumerIdentified;
    property OnLccNodeProducerIdentify: TOnLccNodeEventIdentify read FOnLccNodeProducerIdentify write FOnLccNodeProducerIdentify;
    property OnLccNodeProducerIdentified: TOnLccNodeEventIdentified read FOnLccNodeProducerIdentified write FOnLccNodeProducerIdentified;

    // SNIP
    property OnLccNodeSimpleNodeIdentReply: TOnLccNodeMessageWithDest read FOnLccNodeSimpleNodeIdentReply write FOnLccNodeSimpleNodeIdentReply;
    property OnLccNodeSimpleTrainNodeIdentReply: TOnLccNodeMessageWithDest read FOnLccNodeSimpleTrainNodeIdentReply write FOnLccNodeSimpleTrainNodeIdentReply;

    // Datagrams
    property OnLccNodeDatagramReply: TOnLccNodeMessageWithDest read FOnLccNodeDatagramReply write FOnLccNodeDatagramReply;

    // Traction
    property OnLccNodeTractionQuerySpeed: TOnLccNodeMessageWithTrainObject read FOnLccNodeTractionQuerySpeed write FOnLccNodeTractionQuerySpeed;
    property OnLccNodeTractionQueryFunction: TOnLccNodeMessageWithTrainObject read FOnLccNodeTractionQueryFunction write FOnLccNodeTractionQueryFunction;
    property OnLccNodeTractionControllerConfig: TOnLccNodeMessageWithTrainObject read FOnLccNodeTractionControllerConfig write FOnLccNodeTractionControllerConfig;
    property OnLccNodeTractionControllerQuery: TOnLccNodeMessageWithTrainObject read FOnLccNodeTractionControllerQuery write FOnLccNodeTractionControllerQuery;
    property OnLccNodeTractionControllerChangeNotify: TOnLccNodeMessageWithTrainObject read FOnLccNodeTractionControllerChangeNotify write FOnLccNodeTractionControllerChangeNotify;

    property OnLccNodeTractionManage: TOnLccNodeMessageWithReply read FOnLccNodeTractionManage write FOnLccNodeTractionManage;
    property OnLccNodeTractionListenerAttach: TOnLccNodeListenerAttach read FOnLccNodeTractionListenerAttach write FOnLccNodeTractionListenerAttach;
    property OnLccNodeTractionListenerDetach: TOnLccNodeListenerDetach read FOnLccNodeTractionListenerDetach write FOnLccNodeTractionListenerDetach;
    property OnLccNodeTractionListenerQuery: TOnLccNodeListenerQuery read FOnLccNodeTractionListenerQuery write FOnLccNodeTractionListenerQuery;
    property OnLccTractionUpdateSNIP: TOnLccTractionUpdateSNIP read FOnLccTractionUpdateSNIP write FOnLccTractionUpdateSNIP;
    property OnLccTractionUpdateTrainSNIP: TOnLccTractionUpdateTrainSNIP read FOnLccTractionUpdateTrainSNIP write FOnLccTractionUpdateTrainSNIP;
    property OnLccTractionUpdateListenerCount: TOnLccTractionUpdateListenerCount read FOnLccTractionUpdateListenerCount write FOnLccTractionUpdateListenerCount;

    // Traction DCC Functions
    property OnLccNodeFDI: TOnLccNodeMessageWithDest read FOnLccNodeFDI write FOnLccNodeFDI;
    property OnLccNodeFunctionConfiguration: TOnLccNodeMessageWithDest read FOnLccNodeFunctionConfiguration write FOnLccNodeFunctionConfiguration;

    // Other stuff that may not be useful
    property OnLccNodeOptionalInteractionRejected: TOnLccNodeMessageWithDest read FOnLccNodeOptionalInteractionRejected write FOnLccNodeOptionalInteractionRejected;
    property OnLccNodeRemoteButtonReply: TOnLccNodeMessageWithDest read FOnLccNodeRemoteButtonReply write FOnLccNodeRemoteButtonReply;

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
  if Assigned(OnLccNodeAliasIDChanged) then
    OnLccNodeAliasIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoCANAliasReset(LccNode: TLccNode);
begin
   if Assigned(FOnLccCANAliasReset) then
     FOnLccCANAliasReset(Self, LccNode);
end;

procedure TLccNodeManager.DoCDIRead(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeCDI) then
    OnLccNodeCDI(Self, LccNode)
end;

procedure TLccNodeManager.DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode;
  AddressSpace: Byte);
begin
 if Assigned(OnLccNodeConfigMemAddressSpaceInfoReply) then
   OnLccNodeConfigMemAddressSpaceInfoReply(Self, LccNode, AddressSpace);
end;

procedure TLccNodeManager.DoConfigMemOptionsReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeConfigMemOptionsReply) then
    OnLccNodeConfigMemOptionsReply(Self, LccNode);
end;

procedure TLccNodeManager.DoConfigMemReadReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeConfigMemReadReply) then
    OnLccNodeConfigMemReadReply(Self, LccNode);
end;

procedure TLccNodeManager.DoConfigMemWriteReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeConfigMemWriteReply) then
    OnLccNodeConfigMemWriteReply(Self, LccNode);
end;

procedure TLccNodeManager.DoCreateLccNode(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeCreate) then
    OnLccNodeCreate(Self, LccNode)
end;

procedure TLccNodeManager.DoConsumerIdentified(LccNode: TLccNode;
  LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
begin
  if Assigned(OnLccNodeConsumerIdentified) then
    OnLccNodeConsumerIdentified(Self, LccNode, LccMessage, Event, State);
end;

procedure TLccNodeManager.DoConsumerIdentify(LccNode: TLccNode;
  LccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnLccNodeConsumerIdentify) then
    OnLccNodeConsumerIdentify(Self, LccNode, LccMessage, DoDefault);
end;

procedure TLccNodeManager.DoDatagramReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeDatagramReply) then
    OnLccNodeDatagramReply(Self, LccNode);
end;

procedure TLccNodeManager.DoDestroyLccNode(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeDestroy) then
    OnLccNodeDestroy(Self, LccNode);
end;

procedure TLccNodeManager.DoFDI(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeFDI) then
    OnLccNodeFDI(Self, LccNode)
end;

procedure TLccNodeManager.DoFunctionConfiguration(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeFunctionConfiguration) then
    OnLccNodeFunctionConfiguration(Self, LccNode)
end;

procedure TLccNodeManager.DoInitializationComplete(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeInitializationComplete) then
    OnLccNodeInitializationComplete(Self, LccNode);
end;

procedure TLccNodeManager.DoLogInNode(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeLogin) then
    OnLccNodeLogin(Self, LccNode);
end;

procedure TLccNodeManager.DoNodeIDChanged(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeIDChanged) then
    OnLccNodeIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoOptionalInteractionRejected(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeOptionalInteractionRejected) then
    OnLccNodeOptionalInteractionRejected(Self, LccNode);
end;

procedure TLccNodeManager.DoProducerIdentified(LccNode: TLccNode;
  LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
begin
  if Assigned(OnLccNodeProducerIdentified) then
    OnLccNodeProducerIdentified(Self, LccNode, LccMessage, Event, State);
end;

procedure TLccNodeManager.DoProducerIdentify(LccNode: TLccNode;
  LccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnLccNodeProducerIdentify) then
    OnLccNodeProducerIdentify(Self, LccNode, LccMessage, DoDefault);
end;

procedure TLccNodeManager.DoProtocolIdentifyReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeProtocolIdentifyReply) then
    OnLccNodeProtocolIdentifyReply(Self, LccNode);
end;

procedure TLccNodeManager.DoRemoteButtonReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeRemoteButtonReply) then
    OnLccNodeRemoteButtonReply(Self, LccNode);
end;

procedure TLccNodeManager.DoSimpleNodeIdentReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeSimpleNodeIdentReply) then
    OnLccNodeSimpleNodeIdentReply(Self, LccNode);
end;

procedure TLccNodeManager.DoSimpleTrainNodeIdentReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeSimpleTrainNodeIdentReply) then
    OnLccNodeSimpleTrainNodeIdentReply(Self, LccNode);
end;

procedure TLccNodeManager.DoTractionQuerySpeed(LccNode: TLccNode; TrainObject: TLccTrainObject);
begin
  if Assigned(OnLccNodeTractionQuerySpeed) then
    OnLccNodeTractionQuerySpeed(Self, LccNode, TrainObject);
end;

procedure TLccNodeManager.DoTractionQueryFunction(LccNode: TLccNode; TrainObject: TLccTrainObject);
begin
  if Assigned(OnLccNodeTractionQueryFunction) then
    OnLccNodeTractionQueryFunction(Self, LccNode, TrainObject);
end;

procedure TLccNodeManager.DoTractionControllerAssign(LccNode: TLccNode; TrainObject: TLccTrainObject);
begin
  if Assigned(OnLccNodeTractionControllerConfig) then
    OnLccNodeTractionControllerConfig(Self, LccNode, TrainObject);
end;

procedure TLccNodeManager.DoTractionControllerQuery(LccNode: TLccNode; TrainObject: TLccTrainObject);
begin
  if Assigned(OnLccNodeTractionControllerQuery) then
    OnLccNodeTractionControllerQuery(Self, LccNode, TrainObject);
end;

procedure TLccNodeManager.DoTractionControllerChangedNotify(LccNode: TLccNode; TrainObject: TLccTrainObject);
begin
  if Assigned(OnLccNodeTractionControllerChangeNotify) then
    OnLccNodeTractionControllerChangeNotify(Self, LccNode, TrainObject);
end;

procedure TLccNodeManager.DoTractionListenerAttach(LccNode: TLccNode;
  Listener: TNodeID; Flags: Byte);
begin
  if Assigned(OnLccNodeTractionListenerAttach) then
    OnLccNodeTractionListenerAttach(Self, LccNode, Listener, Flags);
end;

procedure TLccNodeManager.DoTractionListenerDetach(LccNode: TLccNode;
  Listener: TNodeID; Flags: Byte);
begin
  if Assigned(OnLccNodeTractionListenerDetach) then
    OnLccNodeTractionListenerDetach(Self, LccNode, Listener, Flags);
end;

procedure TLccNodeManager.DoTractionListenerQuery(LccNode: TLccNode;
  Index: Integer);
begin
  if Assigned(OnLccNodeTractionListenerQuery) then
     OnLccNodeTractionListenerQuery(Self, LccNode, Index);
end;

procedure TLccNodeManager.DoTractionManage(LccNode: TLccNode;
  LccMessage: TLccMessage; IsReply: Boolean);
begin
  if Assigned(OnLccNodeTractionManage) then
    OnLccNodeTractionManage(Self, LccNode, LccMessage, IsReply);
end;

procedure TLccNodeManager.DoTractionUpdateSNIP(LccNode: TLccNode; TrainObject: TLccTrainObject);
begin
  if Assigned(OnLccTractionUpdateSNIP) then
    OnLccTractionUpdateSNIP(Self, LccNode, TrainObject);
end;

procedure TLccNodeManager.DoTractionUpdateTrainSNIP(LccNode: TLccNode; TrainObject: TLccTrainObject);
begin
  if Assigned(OnLccTractionUpdateTrainSNIP) then
    OnLccTractionUpdateTrainSNIP(Self, LccNode, TrainObject);
end;

procedure TLccNodeManager.DoTractionUpdateListenerCount(LccNode: TLccNode; TrainObject: TLccTrainObject);
begin
  if Assigned(OnLccTractionUpdateListenerCount) then
    OnLccTractionUpdateListenerCount(Self, LccNode, TrainObject);
end;

procedure TLccNodeManager.DoVerifiedNodeID(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeVerifiedNodeID) then
    OnLccNodeVerifiedNodeID(Self, LccNode);
end;

procedure TLccNodeManager.DoAliasMappingChange(LccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
begin
  if Assigned(OnAliasMappingChange) then
    OnAliasMappingChange(Self, LccNode, AnAliasMapping, IsMapped);
end;

procedure TLccNodeManager.DoTrainRegisteringChange(LccNode: TLccNode; TrainObject: TLccTrainObject; IsRegistered: Boolean);
begin
  if Assigned(OnTrainRegisteringChange) then
    OnTrainRegisteringChange(Self, LccNode, TrainObject, IsRegistered);
end;

procedure TLccNodeManager.DoTrainInformationChange(LccNode: TLccNode; TrainObject: TLccTrainObject);
begin
  if Assigned(OnTrainInformationChange) then
    OnTrainInformationChange(Self, LccNode, TrainObject);
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
  ReceiveMessageServerThread.AddMessage(LccMessage);  // I think this works....
end;

end.

