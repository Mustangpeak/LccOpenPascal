unit lcc_connection_common;


{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
  syncobjs,
  {$ELSE}
  FMX.Forms,
  System.SyncObjs,
  {$ENDIF}

  {$IFDEF FPC}
  {$ELSE}
    System.Generics.Collections,
    System.Types,
  {$ENDIF}
  lcc_utilities,
  lcc_node_messages,
  lcc_app_common_settings,
  lcc_defines,
  lcc_node_messages_can_assembler_disassembler,
  lcc_alias_server_thread;

type
  TLccConnectionThreadManager = class;
  TLccConnectionInfo = class;
  TLccConnectionThread = class;
  TLccConnectionFactory = class;

  TOnLccConnectionInfoEvent = procedure(Sender: TObject; Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; ConnectionInfo: TLccConnectionInfo) of object;
  TOnLccConnectionReceiveMessageEvent = procedure(Sender: TObject; LccMessage: TLccMessage) of object;
  TOnLccConnectionReceiveGridConnectStringEvent = procedure(Sender: TObject; GridConnectStr: string) of object;
  TOnLccConnectionSendMessageEvent = procedure(Sender: TObject; LccMessage: TLccMessage) of object;


  { TLccConnectionInfo }

  TLccConnectionInfo = class
  private
    FConnectionState: TLccConnectionState;       // OUT
    FErrorMessage: String;                       // OUT
    FLccMessage: TLccMessage;                    // OUT
    FMessageStr: String;                         // OUT
    FSuppressErrorMessages: Boolean;             // IN
  public
    MessageArray: TLccDynamicByteArray;                                         // Contains the TCP Protocol message bytes if not using GridConnect

    constructor Create;
    destructor Destroy; override;

    function Clone: TLccConnectionInfo; virtual;

    property ConnectionState: TLccConnectionState read FConnectionState write FConnectionState;  // Current State of the connection
    property ErrorMessage: String read FErrorMessage write FErrorMessage;
    property LccMessage: TLccMessage read FLccMessage write FLccMessage;
    property MessageStr: String read FMessageStr write FMessageStr;             // Contains the string for the resulting message from the thread
    property SuppressErrorMessages: Boolean read FSuppressErrorMessages write FSuppressErrorMessages;
  end;

  { TReceiveMessageCallbackObject }

  TReceiveMessageCallbackObject = class
  public
    Callback: TLccAliasServerDispatchProcessedMessageFunc;
    IsNodeManagerReceive: Boolean;
  end;

  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FOwnerConnectionManager: TLccConnectionThreadManager;
    FReceiveStreamConnectionThread: TMemoryStream;
    FRunning: Boolean;
    FSendMessageLccMessageBuffer: Classes.TThreadList;
    FSendMessageStream: TMemoryStream;
    FSendStreamConnectionThread: TMemoryStream;
    function GetIsTerminated: Boolean;

  protected
    // Contains the data coming in from the connection (GridConnect/TCP raw data/etc)
    property ReceiveStreamConnectionThread: TMemoryStream read FReceiveStreamConnectionThread write FReceiveStreamConnectionThread;
    // Contains the data going out from the connection (GridConnect/TCP raw data/etc), converted from the messages in SendMessageLccMessgeBuffer
    property SendStreamConnectionThread: TMemoryStream read FSendStreamConnectionThread write FSendStreamConnectionThread;
    // Messages that have been given to be sent out
    property SendMessageLccMessageBuffer: Classes.TThreadList read FSendMessageLccMessageBuffer write FSendMessageLccMessageBuffer;
    // Flag to say the thread Execute method is still running
    property Running: Boolean read FRunning write FRunning;

    // Calls the actual event handler back through the Connection Factory
    procedure ConnectionStateChangePossiblyThroughSyncronize; virtual;
    //
    procedure ErrorMessagePossiblyThroughSyncronize; virtual;
    // Loads the stream with data based on the LccMessages sent in the ALccMessageList could be GridConnect strings or TCP depending on OwnerConnectionManager.EmulateCanBus
    function LoadStreamFromMessageBuffer(AStream: TStream; ALccMessageList: Classes.TThreadList; ClearBuffer: Boolean = True): Boolean; virtual;
    // Called to place a message into the SendMessageLccMessgeBuffer, typically called by the OwnerConnectionManager
    procedure SendMessageConnectionThread(ALccMessage: TLccMessage); virtual;
    // Allows the events for state changes of the connection during connect and disconnect
    procedure HandleSendConnectionChangeNotify(NewConnectionState: TLccConnectionState; ThroughSyncronize: Boolean); virtual;
    // Allows the events for error messages
    procedure HandleErrorAndDisconnect(SuppressMessage: Boolean; ThroughSyncronize: Boolean = True); virtual;

  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager); reintroduce; virtual;
    destructor Destroy; override;

    // Connection Manager that owns this thread
    property OwnerConnectionManager: TLccConnectionThreadManager read FOwnerConnectionManager;
    // Give public access to test if the thread has been set to Terminate
    property IsTerminated: Boolean read GetIsTerminated;
  end;

  { TLccConnectionThreadManager }
  // Manager for a Thread.  The data and code in this object runs in the context of the main thread
  // so accessing anything from here is thread safe
  TLccConnectionThreadManager = class(TComponent)
  private
    FConnectionThreadList: Classes.TThreadList;
    FCustomConnection: Boolean;
    FConnectionInfo: TLccConnectionInfo;
    FCriticalSection: TCriticalSection;
    FEmulateCanBus: Boolean;
    FOwnerConnectionFactory: TLccConnectionFactory;
    FReceiveGridConnectStringSyncronize: string;
    FReceiveMessageWorkerMessageSyncronize: TLccMessage;
    FSendMessageWorkerMessageSyncronize: TLccMessage;

    procedure SetDefaultConnectionInfo(AValue: TLccConnectionInfo);
  protected

    property ConnectionThreadList: Classes.TThreadList read FConnectionThreadList write FConnectionThreadList;

    // Decendents only know what connected means to them
    // Property Getters to override
    function GetConnected: Boolean; virtual;
    function GetConnecting: Boolean; virtual;

    procedure SendMessageConnectionThreadManager(ALccMessage: TLccMessage); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Connection info, a copy is made and passed to the thread for its use when it is created
    property ConnectionInfo: TLccConnectionInfo read FConnectionInfo write SetDefaultConnectionInfo;

    // True if the Manager is capabable of receiveing/sending messages on the wire... getter must be overridden
    property Connected: Boolean read GetConnected;
    property Connecting: Boolean read GetConnecting;
    property OwnerConnectionFactory: TLccConnectionFactory read FOwnerConnectionFactory write FOwnerConnectionFactory;
    property EmulateCanBus: Boolean read FEmulateCanBus write FEmulateCanBus;
    property CustomConnection: Boolean read FCustomConnection write FCustomConnection;

    // Holds the information that was copied in from the thread so the main thread can access it through a Syncronize call from the thread
    property ReceiveMessageWorkerMessageSyncronize: TLccMessage read FReceiveMessageWorkerMessageSyncronize write FReceiveMessageWorkerMessageSyncronize;
    property ReceiveGridConnectStringSyncronize: string read FReceiveGridConnectStringSyncronize write FReceiveGridConnectStringSyncronize;
    property SendMessageWorkerMessageSyncronize: TLccMessage read FSendMessageWorkerMessageSyncronize write FSendMessageWorkerMessageSyncronize;

    procedure CriticalSectionEnter;
    procedure CriticalSectionLeave;
    procedure CriticalSectionTryEnter;

    // When a thread owned by the manager receives a message it will call these centraized methods
    // ----------------------
    procedure ReceiveGridConnectStrThoughSyncronize;
    procedure SendMessageThroughSyncronize;

    // Puts a GridConnect string in the buffer to be sent without needing to deal with a TLccMessage as not all links are LCC, using custom GridConnect for the UART to the Command Station
    procedure SendMessageRawGridConnect(GridConnectStr: String); virtual;
    // Decendant must override this.  The Node Manager calls this when its nodes needs to send a message to the "wire".
    procedure ErrorMessage(Thread: TLccConnectionThread; AConnectionInfo: TLccConnectionInfo); virtual;
    // ----------------------

    function OpenConnection: TLccConnectionThread; virtual;
    procedure CloseConnection; virtual;


  end;
  TLccConnectionThreadManagerClass = class of TLccConnectionThreadManager;

  { TLccConnectionFactory }

  TLccConnectionFactory = class(TComponent)
  private
    FOnConnectionStateChange: TOnLccConnectionInfoEvent;
    FOnError: TOnLccConnectionInfoEvent;
    FOnLccGridConnectStrReceive: TOnLccConnectionReceiveGridConnectStringEvent;
    FOnLccMessageReceive: TOnLccConnectionReceiveMessageEvent;
    FOnLccMessageSend: TOnLccConnectionSendMessageEvent;
    FReceiveMessageCallbackList: TList;
    FServerList: Classes.TThreadList;
  protected
    property ServerList: Classes.TThreadList read FServerList write FServerList;
    // List of ReceiveMessageCallbacks
    property ReceiveMessageCallbackList: TList read FReceiveMessageCallbackList write FReceiveMessageCallbackList;

    // Event Doer for the messages coming in from child Connections
    procedure DoReceiveLccMessage(LccMessage: TLccMessage); virtual;
    // Event Doer for the GridConnectStrings coming in from child Connections
    procedure DoReceiveGridConnectStr(GridConnectStr: string); virtual;
    /// Event Doer for messages being sent to child Connections
    procedure DoSendLccMessage(ALccMessage: TLccMessage); virtual;
    // Event Doer for the Connection State Change of a child Connection
    procedure DoStateChange(Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; ConnectionInfo: TLccConnectionInfo); virtual;
    // Event Doer for the Connection Error of a child Connection
    procedure DoError(Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; ConnectionInfo: TLccConnectionInfo); virtual;


    // Closes and frees the Servers
    procedure ClearServerList;
    procedure SendLccMessageToThreadManagers(ALccMessage: TLccMessage);
    function IsDuplicateCallback(ACallback: TLccAliasServerDispatchProcessedMessageFunc): Boolean;
    procedure ReceiveMessageCallbacksDispatch(ALccMessage: TLccMessage; SendOnlyToNodeManager: Boolean = False);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Creates a connection that receives and sendmessages from the NodeManager to other nodes on the connection network
    function CreateLccMessageConnection(AConnectionManagerClass: TLccConnectionThreadManagerClass; ConnectionInfo: TLccConnectionInfo; IsEmulateCanBus: Boolean): TLccConnectionThreadManager;
    // Creates a connection that sends custom data across the connection (such as the GridConnect NMRA DCC packets), These will not get the OnLccReceiveMessage/OnLccSendMessage events as they don't define LccMessages as their data format
    function CreateConnection(AConnectionManagerClass: TLccConnectionThreadManagerClass; ConnectionInfo: TLccConnectionInfo): TLccConnectionThreadManager;
    procedure DestroyConnection(AConnection: TLccConnectionThreadManager);

    procedure SendLccMessage(ALccMessage: TLccMessage);
    // Register a callback to get notified when a Message has been validated and ready to be dispatched
    procedure RegisterReceiveMessageCallback(ACallback: TLccAliasServerDispatchProcessedMessageFunc; IsNodeManager: Boolean = False);
    procedure UnregisterReceiveMessageCallback(ACallback: TLccAliasServerDispatchProcessedMessageFunc);

    procedure ReceiveMessageConnectinFactory(ALccMessage: TLccMessage);  // Resistered with the Alias Server Thread as the sink for the messages it dispatches

    property OnLccMessageReceive: TOnLccConnectionReceiveMessageEvent read FOnLccMessageReceive write FOnLccMessageReceive;
    property OnLccGridConnectStrReceive: TOnLccConnectionReceiveGridConnectStringEvent read FOnLccGridConnectStrReceive write FOnLccGridConnectStrReceive;
    property OnLccMessageSend: TOnLccConnectionSendMessageEvent read FOnLccMessageSend write FOnLccMessageSend;
    property OnStateChange: TOnLccConnectionInfoEvent read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnError: TOnLccConnectionInfoEvent read FOnError write FOnError;
  end;

var
  ConnectionFactory: TLccConnectionFactory;


implementation

{ TLccConnectionFactory }

procedure TLccConnectionFactory.DoReceiveLccMessage(LccMessage: TLccMessage);
begin
  if Assigned(OnLccMessageReceive) then
    OnLccMessageReceive(Self, LccMessage);
end;

procedure TLccConnectionFactory.DoReceiveGridConnectStr(GridConnectStr: string);
begin
  if Assigned(OnLccGridConnectStrReceive) then
    OnLccGridConnectStrReceive(Self, GridConnectStr);
end;

procedure TLccConnectionFactory.DoSendLccMessage(ALccMessage: TLccMessage);
begin
  if Assigned(OnLccMessageSend) then
    OnLccMessageSend(Self, ALccMessage);
end;

procedure TLccConnectionFactory.DoStateChange(
  Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread;
  ConnectionInfo: TLccConnectionInfo);
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self, Manager, Thread, ConnectionInfo);
end;

procedure TLccConnectionFactory.DoError(Manager: TLccConnectionThreadManager;
  Thread: TLccConnectionThread; ConnectionInfo: TLccConnectionInfo);
begin
  if Assigned(OnError) then
    OnError(Self, Manager, Thread, ConnectionInfo);
end;

procedure TLccConnectionFactory.ClearServerList;
var
  i: Integer;
  List: TList;
  Manager: TLccConnectionThreadManager;
begin
  List := ServerList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      Manager := TLccConnectionThreadManager( List[i]);
      if Manager.Connected then
        Manager.CloseConnection;
      Manager.Free;
    end;
  finally
    List.Clear;
    ServerList.UnlockList;
  end;
end;

procedure TLccConnectionFactory.SendLccMessageToThreadManagers(ALccMessage: TLccMessage);
var
  i: Integer;
  List: TList;
  Manager: TLccConnectionThreadManager;
begin
  List := ServerList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      Manager := TLccConnectionThreadManager( List[i]);
      if not Manager.CustomConnection then
        Manager.SendMessageConnectionThreadManager(ALccMessage);
    end;
  finally
    ServerList.UnlockList;
  end;
end;

function TLccConnectionFactory.IsDuplicateCallback(ACallback: TLccAliasServerDispatchProcessedMessageFunc): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to ReceiveMessageCallbackList.Count - 1 do
  begin
    if TReceiveMessageCallbackObject( ReceiveMessageCallbackList[i]).Callback = ACallback then
    begin
      Result := True;
      Break
    end;
  end;
end;

procedure TLccConnectionFactory.ReceiveMessageCallbacksDispatch(ALccMessage: TLccMessage; SendOnlyToNodeManager: Boolean);
var
  i: Integer;
begin
  for i := 0 to ReceiveMessageCallbackList.Count - 1 do
  if SendOnlyToNodeManager then
  begin
    if SendOnlyToNodeManager then
    begin
      if TReceiveMessageCallbackObject( ReceiveMessageCallbackList[i]).IsNodeManagerReceive then
      begin
        TReceiveMessageCallbackObject( ReceiveMessageCallbackList[i]).Callback(ALccMessage);
        Break;
      end;
    end
  end else
    TReceiveMessageCallbackObject( ReceiveMessageCallbackList[i]).Callback(ALccMessage);
end;

constructor TLccConnectionFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ServerList := Classes.TThreadList.Create;
  FReceiveMessageCallbackList := TList.Create;
end;

destructor TLccConnectionFactory.Destroy;
begin
  UnregisterReceiveMessageCallback(nil);
  ClearServerList;
  FreeAndNil(FServerList);
  FreeAndNil(FReceiveMessageCallbackList);
  inherited Destroy;
end;

function TLccConnectionFactory.CreateLccMessageConnection(
  AConnectionManagerClass: TLccConnectionThreadManagerClass;
  ConnectionInfo: TLccConnectionInfo; IsEmulateCanBus: Boolean
  ): TLccConnectionThreadManager;
begin
  Result := AConnectionManagerClass.Create(Self);
  Result.OwnerConnectionFactory := Self;
  Result.ConnectionInfo := ConnectionInfo;
  Result.EmulateCanBus := IsEmulateCanBus;
  ServerList.Add(Result);
end;

function TLccConnectionFactory.CreateConnection(
  AConnectionManagerClass: TLccConnectionThreadManagerClass;
  ConnectionInfo: TLccConnectionInfo): TLccConnectionThreadManager;
begin
  Result := CreateLccMessageConnection(AConnectionManagerClass, ConnectionInfo, False);
  Result.CustomConnection := True;
end;

procedure TLccConnectionFactory.DestroyConnection(
  AConnection: TLccConnectionThreadManager);
var
  i: Integer;
  Servers: TList;
begin
  Servers := ServerList.LockList;
  try
    for i := 0 to Servers.Count - 1 do
    begin
      if TLccConnectionThreadManager( Servers[i]) = AConnection then
      begin
        AConnection.CloseConnection;
        Servers.Delete(i);
        AConnection.Free;
        Break
      end;
    end;
  finally
    ServerList.UnlockList;
  end;
end;

procedure TLccConnectionFactory.SendLccMessage(ALccMessage: TLccMessage);
begin
  DoSendLccMessage(ALccMessage);   // Event handler to allow application to see the messages sent
  SendLccMessageToThreadManagers(ALccMessage);

  // This will send the message back to other virtual nodes.
  ReceiveMessageCallbacksDispatch(ALccMessage, True);
end;

procedure TLccConnectionFactory.RegisterReceiveMessageCallback(
  ACallback: TLccAliasServerDispatchProcessedMessageFunc; IsNodeManager: Boolean);
var
  LocalCallbackObj: TReceiveMessageCallbackObject;
begin
  if not IsDuplicateCallback(ACallback) then
  begin
    LocalCallbackObj := TReceiveMessageCallbackObject.Create;
    LocalCallbackObj.IsNodeManagerReceive := IsNodeManager;
    LocalCallbackObj.Callback := ACallback;
    ReceiveMessageCallbackList.Add( LocalCallbackObj);
  end;
end;

procedure TLccConnectionFactory.UnregisterReceiveMessageCallback(ACallback: TLccAliasServerDispatchProcessedMessageFunc);
var
  i: Integer;
begin
  for i := 0 to  ReceiveMessageCallbackList.Count - 1 do
  begin
    if (TReceiveMessageCallbackObject( ReceiveMessageCallbackList[i]).Callback = ACallback) or (ACallback = nil) then
    begin
      TObject( ReceiveMessageCallbackList[i]).Free;
      ReceiveMessageCallbackList.Delete(i);
      Break;
    end;
  end;
end;

procedure TLccConnectionFactory.ReceiveMessageConnectinFactory(
  ALccMessage: TLccMessage);
begin
  DoReceiveLccMessage(ALccMessage);
  ReceiveMessageCallbacksDispatch(ALccMessage);
end;

{ TLccConnectionInfo }

constructor TLccConnectionInfo.Create;
begin
  inherited;
  FLccMessage := TLccMessage.Create;
  FConnectionState := lcsDisconnected;
end;

function TLccConnectionInfo.Clone: TLccConnectionInfo;
begin
  Result := Self.ClassType.Create as TLccConnectionInfo;
  Result.ConnectionState := ConnectionState;
  Result.ErrorMessage := ErrorMessage;
  Result.MessageStr := MessageStr;
  Result.MessageArray := MessageArray;
  Result.FLccMessage := TLccMessage.Create;
  Result.SuppressErrorMessages := SuppressErrorMessages;
end;

destructor TLccConnectionInfo.Destroy;
begin
  FreeAndNil(FLccMessage);
  inherited Destroy;
end;


{ TLccConnectionThreadManager }

procedure TLccConnectionThreadManager.SetDefaultConnectionInfo(AValue: TLccConnectionInfo);
begin
  if Assigned(FConnectionInfo) then
    FreeAndNil(FConnectionInfo);
  FConnectionInfo := AValue;
end;

function TLccConnectionThreadManager.GetConnected: Boolean;
begin
  CriticalSectionEnter;
  try
    Result := ConnectionInfo.ConnectionState = lcsConnected;
  finally
    CriticalSectionLeave;
  end;
end;

function TLccConnectionThreadManager.GetConnecting: Boolean;
begin
  CriticalSectionEnter;
  try
    Result := ConnectionInfo.ConnectionState = lcsConnecting;
  finally
    CriticalSectionLeave;
  end;
end;

procedure TLccConnectionThreadManager.SendMessageConnectionThreadManager(
  ALccMessage: TLccMessage);
var
  i: Integer;
  List: TList;
begin
  List := ConnectionThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TLccConnectionThread( List[i]).SendMessageConnectionThread(ALccMessage);
  finally
    ConnectionThreadList.UnlockList;
  end;
end;

procedure TLccConnectionThreadManager.ReceiveGridConnectStrThoughSyncronize;
begin
  OwnerConnectionFactory.DoReceiveGridConnectStr(ReceiveGridConnectStringSyncronize);
end;

procedure TLccConnectionThreadManager.SendMessageThroughSyncronize;
begin
  OwnerConnectionFactory.SendLccMessage(SendMessageWorkerMessageSyncronize);
end;

procedure TLccConnectionThreadManager.SendMessageRawGridConnect(GridConnectStr: String);
begin

end;

procedure TLccConnectionThreadManager.ErrorMessage(
  Thread: TLccConnectionThread; AConnectionInfo: TLccConnectionInfo);
begin

end;

function TLccConnectionThreadManager.OpenConnection: TLccConnectionThread;
begin
  Result := nil;
end;

procedure TLccConnectionThreadManager.CloseConnection;
begin

end;

constructor TLccConnectionThreadManager.Create(AOwner: TComponent);
begin
  FConnectionThreadList := Classes.TThreadList.Create;
  FReceiveMessageWorkerMessageSyncronize := TLccMessage.Create;
  FSendMessageWorkerMessageSyncronize := TLccMessage.Create;

  inherited Create(AOwner);
  FCriticalSection := TCriticalSection.Create;
end;

destructor TLccConnectionThreadManager.Destroy;
begin
  ThreadListClearObjects(ConnectionThreadList);
  FreeAndNil(FConnectionThreadList);
  FreeAndNil(FCriticalSection);
  FreeAndNil(FReceiveMessageWorkerMessageSyncronize);
  FreeAndNil(FSendMessageWorkerMessageSyncronize);
  FreeAndNil(FConnectionInfo);
  inherited Destroy;
end;

procedure TLccConnectionThreadManager.CriticalSectionEnter;
begin
  FCriticalSection.Enter
end;

procedure TLccConnectionThreadManager.CriticalSectionLeave;
begin
  FCriticalSection.Leave
end;

procedure TLccConnectionThreadManager.CriticalSectionTryEnter;
begin
  FCriticalSection.TryEnter
end;

{ TLccConnectionThread }

constructor TLccConnectionThread.Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager);
begin
  inherited Create(CreateSuspended);
  Priority := tpHighest;
  FOwnerConnectionManager := AnOwner;
  FSendMessageLccMessageBuffer := Classes.TThreadList.Create;
  FSendMessageStream := TMemoryStream.Create;
  FReceiveStreamConnectionThread := TMemoryStream.Create;
  FSendStreamConnectionThread := TMemoryStream.Create;
end;

destructor TLccConnectionThread.Destroy;
begin
  ThreadListClearObjects(SendMessageLccMessageBuffer);
  FreeAndNil(FSendMessageLccMessageBuffer);
  FreeAndNil(FSendMessageStream);
  FreeAndNil(FReceiveStreamConnectionThread);
  FreeAndNil(FSendStreamConnectionThread);
  inherited Destroy;
end;

function TLccConnectionThread.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TLccConnectionThread.HandleErrorAndDisconnect(SuppressMessage: Boolean; ThroughSyncronize: Boolean);
begin
  if ThroughSyncronize then
  begin
    if not SuppressMessage then
      Synchronize({$IFDEF FPC}@{$ENDIF}ErrorMessagePossiblyThroughSyncronize);
    HandleSendConnectionChangeNotify(lcsDisconnected, ThroughSyncronize);
    Terminate;
  end else
  begin
    if not SuppressMessage then
      ErrorMessagePossiblyThroughSyncronize;
    HandleSendConnectionChangeNotify(lcsDisconnected, ThroughSyncronize);
    Terminate;
  end;
end;

procedure TLccConnectionThread.HandleSendConnectionChangeNotify(NewConnectionState: TLccConnectionState; ThroughSyncronize: Boolean);
begin
  OwnerConnectionManager.CriticalSectionEnter;
  try
    OwnerConnectionManager.ConnectionInfo.ConnectionState := NewConnectionState;
  finally
    OwnerConnectionManager.CriticalSectionLeave;
  end;

  if ThroughSyncronize then
    Synchronize({$IFDEF FPC}@{$ENDIF}ConnectionStateChangePossiblyThroughSyncronize)
  else
    ConnectionStateChangePossiblyThroughSyncronize;
end;

procedure TLccConnectionThread.ErrorMessagePossiblyThroughSyncronize;
begin
  OwnerConnectionManager.CriticalSectionEnter;
  try
    (OwnerConnectionManager.Owner as TLccConnectionFactory).DoError(OwnerConnectionManager, Self, OwnerConnectionManager.ConnectionInfo);
  finally
    OwnerConnectionManager.CriticalSectionLeave;
  end;
end;

procedure TLccConnectionThread.ConnectionStateChangePossiblyThroughSyncronize;
begin
  OwnerConnectionManager.CriticalSectionEnter;
  try
    (OwnerConnectionManager.Owner as TLccConnectionFactory).DoStateChange(OwnerConnectionManager, Self, OwnerConnectionManager.ConnectionInfo);
  finally
    OwnerConnectionManager.CriticalSectionLeave;
  end;
end;

function TLccConnectionThread.LoadStreamFromMessageBuffer(AStream: TStream; ALccMessageList: Classes.TThreadList; ClearBuffer: Boolean): Boolean;
var
  MessageList: TList;
  iMessage: Integer;
  DynamicByteArray: TLccDynamicByteArray;
  iString: Integer;
  GridConnectStr: string;
begin
  AStream.Position := 0;
  AStream.Size := 0;

  MessageList := ALccMessageList.LockList;
  try
    if OwnerConnectionManager.EmulateCanBus then
    begin
      for iMessage := 0 to MessageList.Count - 1 do
      begin
        GridConnectStr := TLccMessage( MessageList[iMessage]).ConvertToGridConnectStr(#10, False) + #10;
        for iString := 1 to Length(GridConnectStr) do
          AStream.WriteByte( Ord(GridConnectStr[iString]));
      end;
    end else
    begin
      DynamicByteArray := nil;
      for iMessage := 0 to MessageList.Count - 1 do
      begin
        if TLccMessage( MessageList[iMessage]).ConvertToLccTcp(DynamicByteArray) then
          AStream.Write(DynamicByteArray, Length(DynamicByteArray));
      end;
    end;
  finally
    if ClearBuffer then
      ListClearObjects(MessageList);

    ALccMessageList.UnlockList;

    Result := AStream.Size > 0;   // Did it write anything?
  end;
end;

procedure TLccConnectionThread.SendMessageConnectionThread(
  ALccMessage: TLccMessage);
begin
  // Default way to deal with it... decendant may override how this is handled
  SendMessageLccMessageBuffer.Add(ALccMessage.Clone);
end;

initialization
  ConnectionFactory := TLccConnectionFactory.Create(nil);
  if Assigned(AliasServerThread) then
    AliasServerThread.ReceiveMessageCallback := @ConnectionFactory.ReceiveMessageConnectinFactory;

finalization
  if Assigned(AliasServerThread) then
    AliasServerThread.ReceiveMessageCallback := nil;
  ConnectionFactory.Free;
  ConnectionFactory := nil;

end.

