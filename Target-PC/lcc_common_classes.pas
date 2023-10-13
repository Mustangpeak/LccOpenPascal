unit lcc_common_classes;


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
  TOnLccConnectionReceiveMessageEvent = procedure(Sender: TObject; Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; LccMessage: TLccMessage) of object;
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

  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FErrorOnExit: Boolean;
    FOwnerConnectionManager: TLccConnectionThreadManager;
    FReceiveStream: TMemoryStream;
    FRunning: Boolean;
    FSendMessageBuffer: Classes.TThreadList;
    FSendMessageStream: TMemoryStream;
    function GetIsTerminated: Boolean;

  protected
    procedure ConnectionStateChangePossiblyThroughSyncronize; virtual;

    function LoadStreamFromMessageBuffer(AStream: TStream; AMessageBuffer: Classes.TThreadList; ClearBuffer: Boolean = True): Boolean; virtual;
    procedure SendMessage(ALccMessage: TLccMessage); virtual;

    property ReceiveStream: TMemoryStream read FReceiveStream write FReceiveStream;
    property SendMessageBuffer: Classes.TThreadList read FSendMessageBuffer write FSendMessageBuffer;
    property SendMessageStream: TMemoryStream read FSendMessageStream write FSendMessageStream;
    property Running: Boolean read FRunning write FRunning;

  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager); reintroduce; virtual;
    destructor Destroy; override;

    procedure ClearSendMessageBuffer;

    procedure ErrorMessage; virtual;
    procedure HandleSendConnectionChangeNotify(NewConnectionState: TLccConnectionState; ThroughSyncronize: Boolean); virtual;
    procedure HandleErrorAndDisconnect(SuppressMessage: Boolean; ThroughSyncronize: Boolean = True); virtual;

    property ErrorOnExit: Boolean read FErrorOnExit write FErrorOnExit;
    property OwnerConnectionManager: TLccConnectionThreadManager read FOwnerConnectionManager;
    property IsTerminated: Boolean read GetIsTerminated;
  end;

  { TLccConnectionThreadManager }
  // Manager for a Thread.  The data and code in this object runs in the context of the main thread
  // so accessing anything from here is thread safe
  TLccConnectionThreadManager = class(TComponent)
  private
    FConnectionThreadList: Classes.TThreadList;
    FCustomConnection: Boolean;
    FDefaultConnectionInfo: TLccConnectionInfo;
    FCriticalSection: TCriticalSection;
    FEmulateCanBus: Boolean;
    FOwnerConnectionFactory: TLccConnectionFactory;
    FReceiveMessageObject: TObject;
    FReceiveMessageThread: TLccConnectionThread;
    FReceiveMessageWorkerMessage: TLccMessage;

    procedure SetDefaultConnectionInfo(AValue: TLccConnectionInfo);
  protected

    property ConnectionThreadList: Classes.TThreadList read FConnectionThreadList write FConnectionThreadList;

    // Decendents only know what connected means to them
    // Property Getters to override
    function GetConnected: Boolean; virtual;
    function GetConnecting: Boolean; virtual;

    procedure ClearConnectionThreadList;
    procedure SendMessage(ALccMessage: TLccMessage); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Default Connection info, a copy is made and passed to the thread for its use when it is created
    property DefaultConnectionInfo: TLccConnectionInfo read FDefaultConnectionInfo write SetDefaultConnectionInfo;

    // True if the Manager is capabable of receiveing/sending messages on the wire... getter must be overridden
    property Connected: Boolean read GetConnected;
    property Connecting: Boolean read GetConnecting;
    property OwnerConnectionFactory: TLccConnectionFactory read FOwnerConnectionFactory write FOwnerConnectionFactory;
    property EmulateCanBus: Boolean read FEmulateCanBus write FEmulateCanBus;
    property CustomConnection: Boolean read FCustomConnection write FCustomConnection;
    property ReceiveMessageWorkerMessage: TLccMessage read FReceiveMessageWorkerMessage write FReceiveMessageWorkerMessage;
    property ReceiveMessageObject: TObject read FReceiveMessageObject write FReceiveMessageObject;
    property ReceiveMessageThread: TLccConnectionThread read FReceiveMessageThread write FReceiveMessageThread;

    procedure CriticalSectionEnter;
    procedure CriticalSectionLeave;
    procedure CriticalSectionTryEnter;
    // When a thread owned by the manager receives a message it will call these centraized methods
    // ----------------------

    procedure ReceiveMessageThroughSyncronize;
    // Puts a GridConnect string in the buffer to be sent without needing to deal with a TLccMessage as not all links are LCC, using custom GridConnect for the UART to the Command Station
    procedure SendMessageRawGridConnect(GridConnectStr: String); virtual;
    // Decendant must override this.  The Node Manager calls this when its nodes needs to send a message to the "wire".
    procedure ErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccConnectionInfo); virtual;
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
    FOnLccMessageReceive: TOnLccConnectionReceiveMessageEvent;
    FOnLccMessageSend: TOnLccConnectionSendMessageEvent;
    FServerList: Classes.TThreadList;
  protected
    property ServerList: Classes.TThreadList read FServerList write FServerList;

    // Event Doer for the messages coming in from child Connections
    procedure DoReceiveLccMessage(Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; LccMessage: TLccMessage); virtual;
    /// Event Doer for messages being sent to child Connections
    procedure DoSendLccMessage(ALccMessage: TLccMessage); virtual;
    // Event Doer for the Connection State Change of a child Connection
    procedure DoStateChange(Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; ConnectionInfo: TLccConnectionInfo); virtual;
    // Event Doer for the Connection Error of a child Connection
    procedure DoError(Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread; ConnectionInfo: TLccConnectionInfo); virtual;


    // Closes and frees the Servers
    procedure ClearServerList;
    procedure SendLccMessageToOtherManagers(ALccMessage: TLccMessage);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Creates a connection that receives and sendmessages from the NodeManager to other nodes on the connection network
    function CreateLccMessageConnection(AConnectionManagerClass: TLccConnectionThreadManagerClass; ConnectionInfo: TLccConnectionInfo; IsEmulateCanBus: Boolean): TLccConnectionThreadManager;
    // Creates a connection that sends custom data across the connection (such as the GridConnect NMRA DCC packets), These will not get the OnLccReceiveMessage/OnLccSendMessage events as they don't define LccMessages as their data format
    function CreateConnection(AConnectionManagerClass: TLccConnectionThreadManagerClass; ConnectionInfo: TLccConnectionInfo): TLccConnectionThreadManager;
    procedure SendLccMessage(ALccMessage: TLccMessage);

    property OnLccMessageReceive: TOnLccConnectionReceiveMessageEvent read FOnLccMessageReceive write FOnLccMessageReceive;
    property OnLccMessageSend: TOnLccConnectionSendMessageEvent read FOnLccMessageSend write FOnLccMessageSend;
    property OnStateChange: TOnLccConnectionInfoEvent read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnError: TOnLccConnectionInfoEvent read FOnError write FOnError;
  end;

var
  ConnectionFactory: TLccConnectionFactory;


implementation

{ TLccConnectionFactory }

procedure TLccConnectionFactory.DoReceiveLccMessage(
  Manager: TLccConnectionThreadManager; Thread: TLccConnectionThread;
  LccMessage: TLccMessage);
begin
  if Assigned(OnLccMessageReceive) then
    OnLccMessageReceive(Self, Manager, Thread, LccMessage);
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

procedure TLccConnectionFactory.SendLccMessageToOtherManagers(ALccMessage: TLccMessage);
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
        Manager.SendMessage(ALccMessage);
    end;
  finally
    ServerList.UnlockList;
  end;
end;

constructor TLccConnectionFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ServerList := Classes.TThreadList.Create;
end;

destructor TLccConnectionFactory.Destroy;
begin
  ClearServerList;
  FreeAndNil(FServerList);
  inherited Destroy;
end;

function TLccConnectionFactory.CreateLccMessageConnection(
  AConnectionManagerClass: TLccConnectionThreadManagerClass;
  ConnectionInfo: TLccConnectionInfo; IsEmulateCanBus: Boolean
  ): TLccConnectionThreadManager;
begin
  Result := AConnectionManagerClass.Create(Self);
  Result.OwnerConnectionFactory := Self;
  Result.DefaultConnectionInfo := ConnectionInfo;
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

procedure TLccConnectionFactory.SendLccMessage(ALccMessage: TLccMessage);
begin
  DoSendLccMessage(ALccMessage);
  SendLccMessageToOtherManagers(ALccMessage);

  // This will send the message back to other virtual nodes.
  AliasServerThread.DispatchProcessedMessageCallback(ALccMessage);
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
  if Assigned(FDefaultConnectionInfo) then
    FreeAndNil(FDefaultConnectionInfo);
  FDefaultConnectionInfo := AValue;
end;

function TLccConnectionThreadManager.GetConnected: Boolean;
begin
  CriticalSectionEnter;
  try
    Result := DefaultConnectionInfo.ConnectionState = lcsConnected;
  finally
    CriticalSectionLeave;
  end;
end;

function TLccConnectionThreadManager.GetConnecting: Boolean;
begin
  CriticalSectionEnter;
  try
    Result := DefaultConnectionInfo.ConnectionState = lcsConnecting;
  finally
    CriticalSectionLeave;
  end;
end;

procedure TLccConnectionThreadManager.ClearConnectionThreadList;
var
  i: Integer;
  List: TList;
begin
  List := ConnectionThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    ConnectionThreadList.Clear;
    ConnectionThreadList.UnlockList;
  end;
end;

procedure TLccConnectionThreadManager.SendMessage(ALccMessage: TLccMessage);
var
  i: Integer;
  List: TList;
begin
  List := ConnectionThreadList.LockList;
  try
    for i := 0 to List.Count - 1 do
      TLccConnectionThread( List[i]).SendMessage(ALccMessage);
  finally
    ConnectionThreadList.UnlockList;
  end;
end;

procedure TLccConnectionThreadManager.ReceiveMessageThroughSyncronize;
begin
  if Assigned(OwnerConnectionFactory.OnLccMessageReceive) then
    OwnerConnectionFactory.OnLccMessageReceive(ReceiveMessageObject, Self, ReceiveMessageThread, ReceiveMessageWorkerMessage);
end;

procedure TLccConnectionThreadManager.SendMessageRawGridConnect(GridConnectStr: String);
begin

end;

procedure TLccConnectionThreadManager.ErrorMessage(
  Thread: TLccConnectionThread; ConnectionInfo: TLccConnectionInfo);
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
  FReceiveMessageWorkerMessage := TLccMessage.Create;

  inherited Create(AOwner);
  FCriticalSection := TCriticalSection.Create;
end;

destructor TLccConnectionThreadManager.Destroy;
begin
  ClearConnectionThreadList;
  FreeAndNil(FConnectionThreadList);
  FreeAndNil(FCriticalSection);
  FreeAndNil(FReceiveMessageWorkerMessage);
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
  FOwnerConnectionManager := AnOwner;
  FSendMessageBuffer := Classes.TThreadList.Create;
  FSendMessageStream := TMemoryStream.Create;
  FReceiveStream := TMemoryStream.Create;
end;

destructor TLccConnectionThread.Destroy;
begin
  ClearSendMessageBuffer;
  FreeAndNil(FSendMessageBuffer);
  FreeAndNil(FSendMessageStream);
  FreeAndNil(FReceiveStream);
  inherited Destroy;
end;

procedure TLccConnectionThread.ClearSendMessageBuffer;
var
  i: Integer;
  List: TList;
begin
  List := SendMessageBuffer.LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
    SendMessageBuffer.UnlockList;
  end;
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
      Synchronize({$IFDEF FPC}@{$ENDIF}ErrorMessage);
    HandleSendConnectionChangeNotify(lcsDisconnected, ThroughSyncronize);
    Terminate;
  end else
  begin
    if not SuppressMessage then
      ErrorMessage;
    HandleSendConnectionChangeNotify(lcsDisconnected, ThroughSyncronize);
    Terminate;
  end;
end;

procedure TLccConnectionThread.HandleSendConnectionChangeNotify(NewConnectionState: TLccConnectionState; ThroughSyncronize: Boolean);
begin
  OwnerConnectionManager.CriticalSectionEnter;
  try
    OwnerConnectionManager.DefaultConnectionInfo.ConnectionState := NewConnectionState;
  finally
    OwnerConnectionManager.CriticalSectionLeave;
  end;

  if ThroughSyncronize then
    Synchronize({$IFDEF FPC}@{$ENDIF}ConnectionStateChangePossiblyThroughSyncronize)
  else
    ConnectionStateChangePossiblyThroughSyncronize;
end;

procedure TLccConnectionThread.ErrorMessage;
// Called in context of main thread from connection thread
var
  LocalConnectionInfo: TLccConnectionInfo;
begin
{  LocalConnectionInfo := ConnectionInfo.Clone;
  try
    OwnerConnectionManager.ErrorMessage(Self, LocalConnectionInfo);
  finally
    LocalConnectionInfo.Free
  end; }
end;

procedure TLccConnectionThread.ConnectionStateChangePossiblyThroughSyncronize;
begin
  OwnerConnectionManager.CriticalSectionEnter;
  try
    (OwnerConnectionManager.Owner as TLccConnectionFactory).DoStateChange(OwnerConnectionManager, Self, OwnerConnectionManager.DefaultConnectionInfo);
  finally
    OwnerConnectionManager.CriticalSectionLeave;
  end;
end;

function TLccConnectionThread.LoadStreamFromMessageBuffer(AStream: TStream; AMessageBuffer: Classes.TThreadList; ClearBuffer: Boolean): Boolean;
var
  MessageList: TList;
  i: Integer;
  DynamicByteArray: TLccDynamicByteArray;
  IntStreamPos: Int64;
begin
  IntStreamPos := AStream.Position;

  MessageList := AMessageBuffer.LockList;
  try
    if OwnerConnectionManager.EmulateCanBus then
    begin
      for i := 0 to MessageList.Count - 1 do
        AStream.WriteAnsiString(TLccMessage( MessageList[i]).ConvertToGridConnectStr(#10, False) + #10);
    end else
    begin
      DynamicByteArray := nil;
      for i := 0 to MessageList.Count - 1 do
      begin
        if TLccMessage( MessageList[i]).ConvertToLccTcp(DynamicByteArray) then
          AStream.Write(DynamicByteArray, Length(DynamicByteArray));
      end;
    end;
  finally
    if ClearBuffer then
    begin
      try
        for i := 0 to MessageList.Count - 1 do
          TObject( MessageList[i]).Free;
      finally
        MessageList.Clear;
      end;
    end;
    AMessageBuffer.UnlockList;
    Result := AStream.Position > IntStreamPos;   // Did it write anything?
  end;
end;

procedure TLccConnectionThread.SendMessage(ALccMessage: TLccMessage);
begin
  // Default way to deal with it... decendant may override how this is handled
  SendMessageBuffer.Add(ALccMessage.Clone);
end;

initialization
  ConnectionFactory := TLccConnectionFactory.Create(nil);

finalization
  ConnectionFactory.Free;

end.

