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
    procedure DoTractionControllerChanged(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionControllerChanging(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionEmergencyStop(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionListenerAttach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionListenerDetach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
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
    FDestAlias: Word;
    FDestNodeID: TNodeID;
    FMessages: TThreadList;
    FOwner: TLccNodeManager;
    FReceivedMessage: TLccMessage;
    FWorkerMessage: TLccMessage;
  protected
    property ReceivedMessage: TLccMessage read FReceivedMessage write FReceivedMessage;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property DestAlias: Word read FDestAlias write FDestAlias;
    property DestNodeID: TNodeID read FDestNodeID write FDestNodeID;

    procedure Execute; override;
    procedure ReceiveMessageSyncronize;   // Syncronize Method
    procedure SendMessageSyncronize;      // Syncronize Method
    procedure UpdateGlobalMappings(AMessage: TLccMessage);
    function ValidateMappingsInMessage(AMessage: TLccMessage; ANodeIdentificationForSendMessageList: TList): Boolean;
    function IsDuplicateNodeIdentificationObject(ANodeIdentificationObjectList: TList; ATestNodeIDObject: TLccNodeIdentificationObject): Boolean;

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
    FOnNodeTractionControllerChanged: TOnLccNodeMessageCallBack;
    FOnNodeTractionControllerChanging: TOnLccNodeMessageCallBack;
    FOnNodeTractionControllerAssign: TOnLccNodeMessageCallBack;
    FOnNodeTractionControllerRelease: TOnLccNodeMessageCallBack;
    FOnNodeTractionControllerQuery: TOnLccNodeMessageCallBack;
    FOnNodeTractionEmergencyStop: TOnLccNodeMessageCallBack;
    FOnNodeTractionListenerAttach: TOnLccNodeMessageCallBack;
    FOnNodeTractionListenerDetach: TOnLccNodeMessageCallBack;
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


    function GetNode(Index: Integer): TLccNode;
  protected

    // INodeManagerCallbacks
    procedure DoAliasIDChanged(LccNode: TLccNode);     // Check
    procedure DoAliasMappingChange(LccNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);  // Check
    procedure DoAliasReset(LccNode: TLccNode);
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
    procedure DoTractionControllerChanged(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionControllerChanging(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionControllerRelease(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionControllerQuery(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionEmergencyStop(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionListenerAttach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoTractionListenerDetach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
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
    property OnNodeVerifiedNodeID: TOnLccNodeMessageReply read FOnNodeVerifiedNodeID write FOnNodeVerifiedNodeID;
    property OnNodeProtocolIdentifyReply: TOnLccNodeMessageReply read FOnNodeProtocolIdentifyReply write FOnNodeProtocolIdentifyReply;

    // CAN Node Management
    property OnNodeAliasIDChanged: TOnLccNodeMessage read FOnNodeAliasIDChanged write FOnNodeAliasIDChanged;
    property OnAliasReset: TOnLccNodeMessage read FOnAliasReset write FOnAliasReset;

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
    property OnNodeTractionControllerChanged: TOnLccNodeMessageCallBack read FOnNodeTractionControllerChanged write FOnNodeTractionControllerChanged;
    property OnNodeTractionControllerChanging: TOnLccNodeMessageCallBack read FOnNodeTractionControllerChanging write FOnNodeTractionControllerChanging;
    property OnNodeTractionControllerRelease: TOnLccNodeMessageCallBack read FOnNodeTractionControllerRelease write FOnNodeTractionControllerRelease;
    property OnNodeTractionControllerQuery: TOnLccNodeMessageCallBack read FOnNodeTractionControllerQuery write FOnNodeTractionControllerQuery;
    property OnNodeTractionEmergencyStop: TOnLccNodeMessageCallBack read FOnNodeTractionEmergencyStop write FOnNodeTractionEmergencyStop;
    property OnNodeTractionManageReserve: TOnLccNodeMessageCallBack read FOnNodeTractionManageReserve write FOnNodeTractionManageReserve;
    property OnNodeTractionManageRelease: TOnLccNodeMessageCallBack read FOnNodeTractionManageRelease write FOnNodeTractionManageRelease;
    property OnNodeTractionListenerAttach: TOnLccNodeMessageCallBack read FOnNodeTractionListenerAttach write FOnNodeTractionListenerAttach;
    property OnNodeTractionListenerDetach: TOnLccNodeMessageCallBack read FOnNodeTractionListenerDetach write FOnNodeTractionListenerDetach;
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
  LocalMessageList, LocalValidatedMessageList, LocalUnValidatedMessageList, LocalNodeIdentificationForSendMessageList: TList;
  LocalNodeIDObject: TLccNodeIdentificationObject;
  i: Integer;
  {$IFDEF WriteLnDebug}iPrint: Integer;{$ENDIF WriteLnDebug}
  LocalMessage: TLccMessage;
begin
  Messages := TThreadList.Create;
  LocalValidatedMessageList := TList.Create;
  LocalUnValidatedMessageList := TList.Create;
  LocalNodeIdentificationForSendMessageList := TList.Create;
  WorkerMessage := TLccMessage.Create;
  {$IFDEF WriteLnDebug}iPrint := 0;{$ENDIF WriteLnDebug}
  try
    while not Terminated do
    begin
      {$IFDEF WriteLnDebug}
      if iPrint > 100 then
      begin
        WriteLn('LocalValidatedMessageList count: ' + IntToStr(LocalValidatedMessageList.Count));
        WriteLn('LocalUnValidatedMessageList count: ' + IntToStr(LocalUnValidatedMessageList.Count));
        WriteLn('NodeIdentificationForSendMessageList count: ' + IntToStr(LocalNodeIdentificationForSendMessageList.Count));
      end;
      {$ENDIF WriteLnDebug}

      // First run through the waiting messages.  If there is a Mapping move them in to the
      // LocalValidatedMessageList and set that slot to nil.  If there is no Mapping then the message is moved to the
      // UnValidated list to be handled outside the locked list
      // Need to run from 0 up to maintain order
      LocalMessageList := Messages.LockList;
      try
        {$IFDEF WriteLnDebug}
        if iPrint > 100 then
        begin
          WriteLn('Main MesageList count: ' + IntToStr(LocalMessageList.Count));
          iPrint := 0;
        end;
        Inc(iPrint);
        {$ENDIF WriteLnDebug}
        for i := 0 to LocalMessageList.Count - 1 do
        begin
          if Owner.GridConnect then
          begin
            // Pick out the Verify Message and AMR/AMD to update the AliasMapping Database
            UpdateGlobalMappings(TLccMessage( LocalMessageList[i]));

            // Pull the message apart and find all the Nodes it requires then test them againt the AliasMapping Database.
            // If they are not there then push the Alias or NodeID (from a message payload) into the SendMessage Queue and
            // return false and put them in the LocalUnValidatedMessageList to hold else they get put in the ValidatedList for
            // sending to the nodes
            if ValidateMappingsInMessage(TLccMessage( LocalMessageList[i]), LocalNodeIdentificationForSendMessageList) then
            begin
              {$IFDEF WriteLnDebug}WriteLn('Message added to the Validated List');{$ENDIF WriteLnDebug}
              LocalValidatedMessageList.Add(LocalMessageList[i])
            end else
            begin
              {$IFDEF WriteLnDebug}WriteLn('Message added to the UnValidated List');{$ENDIF WriteLnDebug}
              LocalUnValidatedMessageList.Add(LocalMessageList[i]);
            end;
          end else
          begin  // Raw TCP we move them all
            LocalValidatedMessageList.Add(LocalMessageList[i]);
          end;
        end;
      finally
        LocalMessageList.Clear; // All messages have been moved to other places
        Messages.UnlockList;
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
              {$IFDEF WriteLnDebug}WriteLn('Message freed from Validated List');{$ENDIF WriteLnDebug}
              FreeAndNil(FReceivedMessage);
            end;
          except
            {$IFDEF WriteLnDebug}WriteLn('Message freed from Validated List');{$ENDIF WriteLnDebug}
            FreeAndNil(FReceivedMessage);
          end;
        end;
      finally
        LocalValidatedMessageList.Clear
      end;
      // ***********************************************************************

      // Deliever the requests for VerifyNode we moved from the MainList to the LocalNodeIdentificationForSendMessageList
      // ***********************************************************************
      for i := 0 to LocalNodeIdentificationForSendMessageList.Count - 1 do
      begin
        if not Terminated then
        try
          LocalNodeIDObject := TLccNodeIdentificationObject( LocalNodeIdentificationForSendMessageList[i]);

          // Lets hold this in the list to allow the node to respond.  This will also stop lots of duplicate
          // call to the same node while waiting for a reply
          if LocalNodeIDObject.AbandonCount = 0 then
          begin
            DestAlias := LocalNodeIDObject.Alias;
            DestNodeID := LocalNodeIDObject.NodeID;
            {$IFDEF WriteLnDebug}WriteLn('VerifyNode sent from NodeIdentification List');{$ENDIF WriteLnDebug}
            Synchronize({$IFDEF FPC}@{$ENDIF}SendMessageSyncronize);
          end;

          // If abandon then done.. this must be longer than the lifetime of the object that created them
          if LocalNodeIDObject.AbandonCount > TIMEOUT_NODE_IDENTIFICTION_OBJECT_COUNT then
          begin
            {$IFDEF WriteLnDebug}WriteLn('NodeIdentification List item Freed: 0x' + IntToHex(LocalNodeIDObject.Alias, 4));{$ENDIF WriteLnDebug}
            TObject( LocalNodeIdentificationForSendMessageList[i]).Free;
            LocalNodeIdentificationForSendMessageList[i] := nil;
          end;

           LocalNodeIDObject.AbandonCount  := LocalNodeIDObject.AbandonCount  + 1;

        except
        end;
      end;

      // Now go through and delete the slot that are empty
      for i := LocalNodeIdentificationForSendMessageList.Count - 1 downto 0 do
      begin
        if LocalNodeIdentificationForSendMessageList[i] = nil then
          LocalNodeIdentificationForSendMessageList.Delete(i);
      end;


      // ***********************************************************************

      // See if any UnValidated Messages have been validated and the Mappings now exist
      // TODO:  THESE NEED TO BE COUNTED AND DECIDED WHAT TO DO IF THEY DON'T GET A REPLY FROM THE
      //        VERIFY ID CALL.  TRY SENDING VERIFY ID A FEW MORE TIMES THEN THROW IT AWAY?
      // ***********************************************************************
      if not Terminated then
      begin
        if Owner.GridConnect then
        begin // Must keep order intact so have to run them this way
          for i := 0 to LocalUnValidatedMessageList.Count - 1 do
          begin // Sending the NIL for the list will cause just the Message required Nodes to be tested against the Mapping Database
            if ValidateMappingsInMessage(TLccMessage( LocalUnValidatedMessageList[i]), LocalNodeIdentificationForSendMessageList) then
            begin // We now have a mapping so move it into the Validated list.
              {$IFDEF WriteLnDebug}WriteLn('UnValidated Message Mapping found and moved to Validated Message List');{$ENDIF WriteLnDebug}
              LocalValidatedMessageList.Add(LocalUnValidatedMessageList[i]);
              LocalUnValidatedMessageList[i] := nil;
            end else
            begin
              LocalMessage := TLccMessage( LocalUnValidatedMessageList[i]);
              if LocalMessage.AbandonCount > TIMEOUT_UNVALIDATED_MESSAGE_COUNT then
              begin
                {$IFDEF WriteLnDebug}WriteLn('UnValidated Message Mapping not found so Abandoned and Freed');{$ENDIF WriteLnDebug}
                LocalMessage.Free;
                LocalUnValidatedMessageList[i] := nil;
              end else
                LocalMessage.AbandonCount := LocalMessage.AbandonCount + 1;
            end;
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
    LocalMessageList := Messages.LockList;
    try
      for i := 0 to LocalMessageList.Count - 1 do
        TObject(LocalMessageList[i]).Free
    finally
      LocalMessageList.Clear;
      Messages.UnlockList;
      FreeAndNil(FMessages);
    end;

    try
      for i := 0 to LocalValidatedMessageList.Count - 1 do
        TObject(LocalValidatedMessageList[i]).Free
    finally
      LocalValidatedMessageList.Clear;
      FreeAndNil(LocalValidatedMessageList);
      LocalValidatedMessageList.Free;
    end;

    LocalNodeIdentificationForSendMessageList.Clear;
    FreeAndNil(LocalNodeIdentificationForSendMessageList);

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
  LocalSourceNode := nil;
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

procedure TReceiveMessageServerThread.SendMessageSyncronize;
var
  LocalInitiailizedNode: TLccNode;
begin
  // Find a Node to use as the Source of the message... it does not really matter
  // who that is.
  LocalInitiailizedNode := Owner.FindInitializedNode;

  if Assigned(LocalInitiailizedNode) then
  begin
    // Either field could be valid, Alias or NodeID
    if DestAlias > 0 then
    begin // NOTICE: This only works with GridConnect since the DestNodeID is unknown and only the Alias is valid here
      WorkerMessage.LoadVerifyNodeIDAddressed(LocalInitiailizedNode.NodeID, LocalInitiailizedNode.AliasID, DestNodeID, DestAlias, NULL_NODE_ID);
      Owner.SendMessage(LocalInitiailizedNode, WorkerMessage);
    end else
    if not NullNodeID(DestNodeID) then
    begin // Here we know the full NodeID but not the Alias so we use this form
      WorkerMessage.LoadVerifyNodeID(LocalInitiailizedNode.NodeID, LocalInitiailizedNode.AliasID, DestNodeID);
      Owner.SendMessage(LocalInitiailizedNode, WorkerMessage);
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

function TReceiveMessageServerThread.ValidateMappingsInMessage(AMessage: TLccMessage; ANodeIdentificationForSendMessageList: TList): Boolean;
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
    // Have the messages extract all the nodes that it will require to be processed
    LocalNodeIdentificationObjectList := AMessage.ExtractNodeIdentifications(True);

    // Run through all these Nodes
    for i := 0 to LocalNodeIdentificationObjectList.Count - 1 do
    begin
      // An index may not be valid... i.e. if a node ID is carried in an event there won't be a destination
      if LocalNodeIdentificationObjectList[i].Valid then
      begin
        // See if the mapping is in the Alias Server. May have the Alias or a full NodeID from a payload
        if LocalNodeIdentificationObjectList[i].Alias <> 0 then
          LocalAliasMapping := AliasServer.FindMapping(LocalNodeIdentificationObjectList[i].Alias)
        else
        if not NullNodeID(LocalNodeIdentificationObjectList[i].NodeID) then
          LocalAliasMapping := AliasServer.FindMapping(LocalNodeIdentificationObjectList[i].NodeID)
        else
          raise ENodeIdentificationObjectIsNull.Create('TReceiveMessageServerThread.ValidateMapping: Received Message Alias/Node Extractions failed');


        if Assigned(LocalAliasMapping) then
        begin  // If we have a mapping then backload the message with the values
          case i of
            0 :
              begin
                AMessage.SourceID := LocalAliasMapping.NodeID;
                AMessage.CAN.SourceAlias := LocalAliasMapping.NodeAlias;
              end;
            1 :
              begin
                AMessage.DestID := LocalAliasMapping.NodeID;
                AMessage.CAN.DestAlias := LocalAliasMapping.NodeAlias;
              end;
          end;
        end else
        begin // Mapping did not exist
          {$IFDEF WriteLnDebug}
      {    AliasServer.WriteMapping('Cound not find Mapping at Message Position: ' +
            IntToStr(i) +
            ' - Alias' +
            IntToHex(LocalNodeIdentificationObjectList[i].Alias, 4) + ' ID: ' +
            NodeIDToString(LocalNodeIdentificationObjectList[i].NodeID, True),
            nil);  }
          {$ENDIF}

          // Load it up in the SendMessage Queue to send a Verify node
          if not IsDuplicateNodeIdentificationObject(ANodeIdentificationForSendMessageList, LocalNodeIdentificationObjectList[i]) then
          begin
            {$IFDEF WriteLnDebug}WriteLn('New NodeIDObject added to NodeIdentificationObjectList');{$ENDIF}
            ANodeIdentificationForSendMessageList.Add(LocalNodeIdentificationObjectList[i].Clone);
          end;

          Result := False;
        end;
      end;
    end;
  end;
end;

function TReceiveMessageServerThread.IsDuplicateNodeIdentificationObject(ANodeIdentificationObjectList: TList; ATestNodeIDObject: TLccNodeIdentificationObject): Boolean;
var
  i: Integer;
  LocalNodeIDObject: TLccNodeIdentificationObject;
begin
  Result := False;
  for i := 0 to ANodeIdentificationObjectList.Count - 1 do
  begin
    LocalNodeIDObject := TLccNodeIdentificationObject(ANodeIdentificationObjectList[i]);
    if (LocalNodeIDObject.NodeID[0] = ATestNodeIDObject.NodeID[0]) and (LocalNodeIDObject.NodeID[1] = ATestNodeIDObject.NodeID[1]) and (LocalNodeIDObject.Alias = ATestNodeIDObject.Alias) then
    begin
      Result := True;
      Break
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

procedure TLccNodeManager.DoTractionControllerChanged(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionControllerChanged) then
    OnNodeTractionControllerChanged(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionControllerChanging(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionControllerChanging) then
    OnNodeTractionControllerChanging(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionListenerAttach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionListenerAttach) then
    OnNodeTractionListenerAttach(Self, LccNode, ALccMessage, DoDefault);
end;

procedure TLccNodeManager.DoTractionListenerDetach(LccNode: TLccNode; ALccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnNodeTractionListenerDetach) then
    OnNodeTractionListenerDetach(Self, LccNode, ALccMessage, DoDefault);
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

