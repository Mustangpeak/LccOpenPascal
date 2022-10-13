unit lcc_common_classes;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ../lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP}Forms, {$ENDIF}
  {$ELSE}
  FMX.Forms,
  {$ENDIF}

  {$IFDEF FPC}
  {$ELSE}
    System.Generics.Collections,
    System.Types,
  {$ENDIF}
  lcc_node_messages,
  lcc_node_manager,
  lcc_app_common_settings,
  lcc_threaded_circulararray,
  lcc_ethernet_tcp,
  lcc_threaded_stringlist,
  lcc_defines,
  lcc_node_messages_can_assembler_disassembler;

type
  TLccHardwareConnectionManager = class;
  TLccHardwareConnectionInfo = class;
  TLccConnectionThread = class;

  TOnHardwareConnectionStateChangeEvent = procedure(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo) of object;
  TOnHardwareConnectionErrorEvent = procedure(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo) of object;

  { TLccHardwareConnectionInfo }

  TLccHardwareConnectionInfo = class
  private
    FConnectionState: TLccConnectionState;
    FErrorCode: Integer;
    FGridConnect: Boolean;
    FHub: Boolean;
    FLccMessage: TLccMessage;
    FMessageStr: String;
    FSleepCofunt: Integer;
    FSleepCount: Integer;
    FSuppressErrorMessages: Boolean;
    FThread: TLccConnectionThread;
    FUseSyncronize: Boolean;
  public
    MessageArray: TLccDynamicByteArray;                                         // Contains the TCP Protocol message bytes of not using GridConnect

    constructor Create;
    destructor Destroy; override;

    function Clone: TLccHardwareConnectionInfo; virtual;

    property ConnectionState: TLccConnectionState read FConnectionState write FConnectionState;  // Current State of the connection
    property GridConnect: Boolean read FGridConnect write FGridConnect;
    property Thread: TLccConnectionThread read FThread write FThread;
    property Hub: Boolean read FHub write FHub;
    property LccMessage: TLccMessage read FLccMessage write FLccMessage;
    property ErrorCode: Integer read FErrorCode write FErrorCode;
    property MessageStr: String read FMessageStr write FMessageStr;             // Contains the string for the resulting message from the thread
    property SleepCount: Integer read FSleepCount write FSleepCofunt;
    property SuppressErrorMessages: Boolean read FSuppressErrorMessages write FSuppressErrorMessages;
  end;

  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FConnectionInfo: TLccHardwareConnectionInfo;
    FOutgoingCircularArray: TThreadedCircularArray;
    FOutgoingGridConnect: TThreadStringList;
    FOwner: TLccHardwareConnectionManager;
    FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
    FWorkerMessage: TLccMessage;
    function GetIsTerminated: Boolean;
  protected
    FRunning: Boolean;

    // Link back to the ConnectionManager that owns the thread
    property Owner: TLccHardwareConnectionManager read FOwner write FOwner;
    property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;

    procedure HandleErrorAndDisconnect(SuppressMessage: Boolean); virtual;
    procedure HandleSendConnectionNotification(NewConnectionState: TLccConnectionState); virtual;

    procedure ErrorMessage; virtual;
    procedure RequestErrorMessageSent; virtual;
    procedure ConnectionStateChange; virtual;

  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo); reintroduce; virtual;
    destructor Destroy; override;

    property ConnectionInfo: TLccHardwareConnectionInfo read FConnectionInfo;
    property OutgoingGridConnect: TThreadStringList read FOutgoingGridConnect write FOutgoingGridConnect;
    property OutgoingCircularArray: TThreadedCircularArray read FOutgoingCircularArray write FOutgoingCircularArray;
    property Running: Boolean read FRunning write FRunning;
    property IsTerminated: Boolean read GetIsTerminated;
    // Holds the next message received in a thread is Syncronize is called so the main thread can
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  end;

  { TLccHardwareConnectionManager }

  TLccHardwareConnectionManager = class(TComponent, IHardwareConnectionManagerLink)
  private
    FHub: Boolean;
    FNodeManager: TLccNodeManager;
    FOnConnectionStateChange: TOnHardwareConnectionStateChangeEvent;
    FOnErrorMessage: TOnHardwareConnectionErrorEvent;
    FOnReceiveMessage: TOnMessageEvent;
    FOnSendMessage: TOnMessageEvent;
    FWorkerMessage: TLccMessage;
  protected
    // useful in decendants, GetConnected could just return this with the object setting FConnected correctly
    FConnected: Boolean;

    // Decendents only know what connected means to them
    function GetConnected: Boolean; virtual; abstract;

    property Hub: Boolean read FHub write FHub;         // <<<<<<<<< Should the Hub be in the NodeManager or is this just to make this connection not part of the hub system... to block it

    // Event call methods
    procedure DoReceiveMessage(LccMessage: TLccMessage); virtual;
    procedure DoSendMessage(ALccMessage: TLccMessage); virtual;
    procedure DoConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    procedure DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;

    // Decendants must override this to tell the Node Manager if this Connection is used to move Lcc packets or not (HTTP server, ComPort with custom protocol server are examples on "no")
    function IsLccLink: Boolean; virtual; abstract;

  public
    // True if the Manager is capabable of receiveing/sending messages on the wire... getter must be overridden
    property Connected: Boolean read GetConnected;
    // The Connection Mangaer is assigned to this Connection Manager and it uses it to pass messages
    property NodeManager: TLccNodeManager read FNodeManager;

    constructor Create(AOwner: TComponent; ANodeManager: TLccNodeManager); reintroduce; virtual;
    destructor Destroy; override;

    // When a thread owned by the manager receives a message it will call these centraized methods
    // ----------------------
    // Decendant must override this.  The Connection Threads call when a message come in on the "wire".
    // This is what is called from the threads Syncronize method
    procedure ReceiveMessage; virtual; abstract;
    // Decendant must override this.  The Node Manager calls this when its nodes needs to send a message to the "wire".
    procedure SendMessage(ALccMessage: TLccMessage); virtual;
    // Puts a GridConnect string in the buffer to be sent without needing to deal with a TLccMessage as not all links are LCC, using custom GridConnect for the UART to the Command Station
    procedure SendMessageRawGridConnect(GridConnectStr: String); virtual;
    // When a thread owned by the manager receives a message it will call this centraized method
    procedure ConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    // Decendant must override this.  The Node Manager calls this when its nodes needs to send a message to the "wire".
    procedure ErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    // ----------------------

    function OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread; virtual;
    function OpenConnectionWithLccSettings: TLccConnectionThread; virtual;
    procedure CloseConnection; virtual;

  published
    property OnConnectionStateChange: TOnHardwareConnectionStateChangeEvent read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnHardwareConnectionErrorEvent read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnMessageEvent read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  end;


implementation

{ TLccHardwareConnectionInfo }

constructor TLccHardwareConnectionInfo.Create;
begin
  inherited;
  FLccMessage := TLccMessage.Create;
  FConnectionState := lcsDisconnected;
  FUseSyncronize := True;
end;

function TLccHardwareConnectionInfo.Clone: TLccHardwareConnectionInfo;
begin
  Result := Self.ClassType.Create as TLccHardwareConnectionInfo;
  Result.ConnectionState := ConnectionState;
  Result.GridConnect := GridConnect;;
  Result.Thread := Thread;
  Result.Hub := Hub;
  Result.ErrorCode := ErrorCode;
  Result.MessageStr := MessageStr;
  Result.MessageArray := MessageArray;
  Result.SleepCount := SleepCount;
  Result.FLccMessage := TLccMessage.Create;
  Result.SuppressErrorMessages := SuppressErrorMessages;
end;

destructor TLccHardwareConnectionInfo.Destroy;
begin
  FreeAndNil(FLccMessage);
  inherited Destroy;
end;


{ TLccHardwareConnectionManager }

procedure TLccHardwareConnectionManager.DoReceiveMessage(LccMessage: TLccMessage);
begin
  if Assigned(OnReceiveMessage) then
    OnReceiveMessage(Self, LccMessage);
end;

procedure TLccHardwareConnectionManager.DoSendMessage(ALccMessage: TLccMessage);
begin
  if Assigned(OnSendMessage) then
    OnSendMessage(Self, ALccMessage);
end;

procedure TLccHardwareConnectionManager.DoConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Thread, ConnectionInfo);
end;

procedure TLccHardwareConnectionManager.DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  if Assigned(OnErrorMessage) then
    OnErrorMessage(Thread, ConnectionInfo);
end;

procedure TLccHardwareConnectionManager.SendMessage(ALccMessage: TLccMessage);
begin
  DoSendMessage(ALccMessage);
end;

procedure TLccHardwareConnectionManager.ConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  DoConnectionStateChange(Thread, ConnectionInfo);
end;

procedure TLccHardwareConnectionManager.ErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  DoErrorMessage(Thread, ConnectionInfo);
end;

procedure TLccHardwareConnectionManager.SendMessageRawGridConnect(GridConnectStr: String);
begin

end;

function TLccHardwareConnectionManager.OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  Result := nil;
  FHub := ConnectionInfo.Hub;
end;

function TLccHardwareConnectionManager.OpenConnectionWithLccSettings: TLccConnectionThread;
begin
  Result := nil;   // Override
end;

procedure TLccHardwareConnectionManager.CloseConnection;
begin

end;

constructor TLccHardwareConnectionManager.Create(AOwner: TComponent; ANodeManager: TLccNodeManager);
begin
  Assert(Assigned(ANodeManager), 'TLccHardwareConnectionManager must have an assigned TLccNodeManager');

  inherited Create(AOwner);
  FNodeManager := ANodeManager;
  FWorkerMessage := TLccMessage.Create;
  NodeManager.HardwarewareConnectionList.Add(Self as IHardwareConnectionManagerLink);
end;

destructor TLccHardwareConnectionManager.Destroy;
begin
  NodeManager.HardwarewareConnectionList.Remove(Self as IHardwareConnectionManagerLink);
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

{ TLccConnectionThread }

constructor TLccConnectionThread.Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FConnectionInfo := AConnectionInfo.Clone;
  FConnectionInfo.Thread := Self;
  FConnectionInfo.ConnectionState := lcsDisconnected;
  FConnectionInfo.ErrorCode := 0;
  FConnectionInfo.FMessageStr := '';
  FWorkerMessage := TLccMessage.Create;
  FOutgoingCircularArray := TThreadedCircularArray.Create;
  FOutgoingGridConnect := TThreadStringList.Create;
  OutgoingGridConnect.Delimiter := #10;
  FTcpDecodeStateMachine := TOPStackcoreTcpDecodeStateMachine.Create;
end;

destructor TLccConnectionThread.Destroy;
begin
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FOutgoingCircularArray);
  FreeAndNil(FOutgoingGridConnect);
  FreeAndNil(FTcpDecodeStateMachine);
  inherited Destroy;
end;

function TLccConnectionThread.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TLccConnectionThread.HandleErrorAndDisconnect(SuppressMessage: Boolean);
begin
  if not SuppressMessage and (ConnectionInfo.ErrorCode <> 0) then
    Synchronize({$IFDEF FPC}@{$ENDIF}ErrorMessage);
  HandleSendConnectionNotification(lcsDisconnected);
  Terminate;
end;

procedure TLccConnectionThread.HandleSendConnectionNotification(NewConnectionState: TLccConnectionState);
begin
  ConnectionInfo.ConnectionState := NewConnectionState;
  Synchronize({$IFDEF FPC}@{$ENDIF}ConnectionStateChange);
end;

procedure TLccConnectionThread.ErrorMessage;
// Called in context of main thread from connection thread
var
  LocalConnectionInfo: TLccHardwareConnectionInfo;
begin
  LocalConnectionInfo := ConnectionInfo.Clone;
  try
    Owner.ErrorMessage(Self, LocalConnectionInfo);
  finally
    LocalConnectionInfo.Free
  end;
end;

procedure TLccConnectionThread.RequestErrorMessageSent;
begin

end;

procedure TLccConnectionThread.ConnectionStateChange;
// Called in context of main thread from connection thread
var
  LocalConnectionInfo: TLccHardwareConnectionInfo;
begin
  LocalConnectionInfo := ConnectionInfo.Clone;
  try
    Owner.ConnectionStateChange(Self, LocalConnectionInfo);
  finally
    LocalConnectionInfo.Free
  end;
end;

end.

