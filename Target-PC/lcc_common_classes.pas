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
  syncobjs,
    {$IFNDEF FPC_CONSOLE_APP}Forms, {$ENDIF}
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
  lcc_node_manager,
  lcc_app_common_settings,
  lcc_threaded_circulararray,
  lcc_ethernet_tcp,
  lcc_threaded_stringlist,
  lcc_defines,
  lcc_gridconnect,
  lcc_node_messages_can_assembler_disassembler;

type
  TLccHardwareConnectionManager = class;
  TLccHardwareConnectionInfo = class;
  TLccConnectionThread = class;

  TOnHardwareConnectionInfoEvent = procedure(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo) of object;

  { TLccHardwareConnectionInfo }

  TLccHardwareConnectionInfo = class
  private
    FConnectionState: TLccConnectionState;
    FErrorMessage: String;
    FGridConnect: Boolean;
    FHub: Boolean;
    FLccMessage: TLccMessage;
    FMessageStr: String;
    FSleepCofunt: Integer;
    FSleepCount: Integer;
    FSuppressErrorMessages: Boolean;
  public
    MessageArray: TLccDynamicByteArray;                                         // Contains the TCP Protocol message bytes of not using GridConnect

    constructor Create;
    destructor Destroy; override;

    function Clone: TLccHardwareConnectionInfo; virtual;

    property ConnectionState: TLccConnectionState read FConnectionState write FConnectionState;  // Current State of the connection
    property ErrorMessage: String read FErrorMessage write FErrorMessage;
    property GridConnect: Boolean read FGridConnect write FGridConnect;
    property Hub: Boolean read FHub write FHub;
    property LccMessage: TLccMessage read FLccMessage write FLccMessage;
    property MessageStr: String read FMessageStr write FMessageStr;             // Contains the string for the resulting message from the thread
    property SleepCount: Integer read FSleepCount write FSleepCofunt;
    property SuppressErrorMessages: Boolean read FSuppressErrorMessages write FSuppressErrorMessages;
  end;

  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FConnecting: Boolean;
    FConnectionInfo: TLccHardwareConnectionInfo;
    FErrorOnExit: Boolean;
    FGridConnectHelper: TGridConnectHelper;
    FGridConnectMessageAssembler: TLccGridConnectMessageAssembler;
    FOutgoingCircularArray: TThreadedCircularArray;
    FOutgoingGridConnect: TThreadStringList;
    FOwner: TLccHardwareConnectionManager;
    FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
    FWorkerMessage: TLccMessage;
    function GetIsTerminated: Boolean;
  protected
    FRunning: Boolean;

    procedure SetConnecting(AValue: Boolean); virtual;

    procedure ConnectionStateChange; virtual;

  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo); reintroduce; virtual;
    destructor Destroy; override;

    procedure ErrorMessage; virtual;
    procedure HandleSendConnectionNotification(NewConnectionState: TLccConnectionState; ContextOfThread: Boolean = True); virtual;
    procedure HandleErrorAndDisconnect(SuppressMessage: Boolean; ContextOfThread: Boolean = True); virtual;

    property ConnectionInfo: TLccHardwareConnectionInfo read FConnectionInfo;
    property Connecting: Boolean read FConnecting write SetConnecting;
    property ErrorOnExit: Boolean read FErrorOnExit write FErrorOnExit;
    property GridConnectHelper: TGridConnectHelper read FGridConnectHelper;
    property OutgoingGridConnect: TThreadStringList read FOutgoingGridConnect write FOutgoingGridConnect;
    property OutgoingCircularArray: TThreadedCircularArray read FOutgoingCircularArray write FOutgoingCircularArray;
    property Owner: TLccHardwareConnectionManager read FOwner;
    property Running: Boolean read FRunning write FRunning;
    property IsTerminated: Boolean read GetIsTerminated;
    property GridConnectMessageAssembler: TLccGridConnectMessageAssembler read FGridConnectMessageAssembler write FGridConnectMessageAssembler;
    property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;
    // Holds the next message received in a thread is Syncronize is called so the main thread can
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  end;

  { TLccHardwareConnectionManager }

  TLccHardwareConnectionManager = class(TComponent, IHardwareConnectionManagerLink)
  private
    FHub: Boolean;
    FNodeManager: TLccNodeManager;
    FOnConnectionStateChange: TOnHardwareConnectionInfoEvent;
    FOnErrorMessage: TOnHardwareConnectionInfoEvent;
    FOnLccMessageReceive: TOnMessageEvent;
    FOnLccMessageSend: TOnMessageEvent;
    FWorkerMessage: TLccMessage;
  protected
    FCriticalSection: TCriticalSection;

    // Decendents only know what connected means to them
    function GetConnected: Boolean; virtual; abstract;
    function GetConnecting: Boolean; virtual; abstract;

    // Event call methods
    procedure DoReceiveMessage(LccMessage: TLccMessage); virtual;
    procedure DoSendMessage(ALccMessage: TLccMessage); virtual;
    procedure DoConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    procedure DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;

    // Decendants must override this to tell the Node Manager if this Connection is used to move Lcc packets or not (HTTP server, ComPort with custom protocol server are examples on "no")
    function IsLccLink: Boolean; virtual; abstract;
    function IsSelf(Test: TObject): Boolean;

  public
    // True if the Manager is capabable of receiveing/sending messages on the wire... getter must be overridden
    property Connected: Boolean read GetConnected;
    property Connecting: Boolean read GetConnecting;
    //
    property Hub: Boolean read FHub write FHub;
    // The Connection Mangaer is assigned to this Connection Manager and it uses it to pass messages
    property NodeManager: TLccNodeManager read FNodeManager;

    constructor Create(AOwner: TComponent; ANodeManager: TLccNodeManager); reintroduce; virtual;
    destructor Destroy; override;

    procedure CriticalSectionEnter;
    procedure CriticalSectionLeave;
    procedure CriticalSectionTryEnter;
    // When a thread owned by the manager receives a message it will call these centraized methods
    // ----------------------
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
    property OnConnectionStateChange: TOnHardwareConnectionInfoEvent read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnHardwareConnectionInfoEvent read FOnErrorMessage write FOnErrorMessage;
    property OnLccMessageReceive: TOnMessageEvent read FOnLccMessageReceive write FOnLccMessageReceive;
    property OnLccMessageSend: TOnMessageEvent read FOnLccMessageSend write FOnLccMessageSend;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  end;


implementation

{ TLccHardwareConnectionInfo }

constructor TLccHardwareConnectionInfo.Create;
begin
  inherited;
  FLccMessage := TLccMessage.Create;
  FConnectionState := lcsDisconnected;
end;

function TLccHardwareConnectionInfo.Clone: TLccHardwareConnectionInfo;
begin
  Result := Self.ClassType.Create as TLccHardwareConnectionInfo;
  Result.ConnectionState := ConnectionState;
  Result.ErrorMessage := ErrorMessage;
  Result.GridConnect := GridConnect;;
  Result.Hub := Hub;
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
var
  i: Integer;
  ConnectionLink: IHardwareConnectionManagerLink;
begin
  if Assigned(OnLccMessageReceive) then
    OnLccMessageReceive(Self, LccMessage);

  NodeManager.HardwarewareConnectionList.Lock;
  try
    for i := 0 to NodeManager.HardwarewareConnectionList.Count - 1 do
    begin
      ConnectionLink := (NodeManager.HardwarewareConnectionList[i] as IHardwareConnectionManagerLink);
      if ConnectionLink.IsLccLink and ConnectionLink.GetConnected and not ConnectionLink.IsSelf(self) then
        ConnectionLink.SendMessage(LccMessage);
    end;
  finally
    NodeManager.HardwarewareConnectionList.UnLock
  end;
end;

procedure TLccHardwareConnectionManager.DoSendMessage(ALccMessage: TLccMessage);
begin
  if Assigned(OnLccMessageSend) then
    OnLccMessageSend(Self, ALccMessage);
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

function TLccHardwareConnectionManager.IsSelf(Test: TObject): Boolean;
begin
  Result := Test = self;
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
  FCriticalSection := TCriticalSection.Create;
end;

destructor TLccHardwareConnectionManager.Destroy;
begin
  NodeManager.HardwarewareConnectionList.Remove(Self as IHardwareConnectionManagerLink);
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FCriticalSection);
  inherited Destroy;
end;

procedure TLccHardwareConnectionManager.CriticalSectionEnter;
begin
  FCriticalSection.Enter
end;

procedure TLccHardwareConnectionManager.CriticalSectionLeave;
begin
  FCriticalSection.Leave
end;

procedure TLccHardwareConnectionManager.CriticalSectionTryEnter;
begin
  FCriticalSection.TryEnter
end;

{ TLccConnectionThread }

constructor TLccConnectionThread.Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FConnectionInfo := AConnectionInfo.Clone;
  FConnectionInfo.ConnectionState := lcsDisconnected;
  FConnectionInfo.FMessageStr := '';
  FWorkerMessage := TLccMessage.Create;
  FOutgoingCircularArray := TThreadedCircularArray.Create;
  FOutgoingGridConnect := TThreadStringList.Create;
  OutgoingGridConnect.Delimiter := #10;
  FTcpDecodeStateMachine := TOPStackcoreTcpDecodeStateMachine.Create;
  FGridConnectHelper := TGridConnectHelper.Create;
  FGridConnectMessageAssembler := TLccGridConnectMessageAssembler.Create;
end;

destructor TLccConnectionThread.Destroy;
begin
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FOutgoingCircularArray);
  FreeAndNil(FOutgoingGridConnect);
  FreeAndNil(FTcpDecodeStateMachine);
  FreeAndNil(FGridConnectHelper);
  FreeAndNil(FGridConnectMessageAssembler);
  inherited Destroy;
end;

function TLccConnectionThread.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TLccConnectionThread.SetConnecting(AValue: Boolean);
begin
  if FConnecting=AValue then Exit;
  FConnecting:=AValue;
end;

procedure TLccConnectionThread.HandleErrorAndDisconnect(SuppressMessage: Boolean; ContextOfThread: Boolean);
begin
  if ContextOfThread then
  begin
    if not SuppressMessage then
      Synchronize({$IFDEF FPC}@{$ENDIF}ErrorMessage);
    HandleSendConnectionNotification(lcsDisconnected);
    Terminate;
  end else
  begin
    if not SuppressMessage then
      ErrorMessage;
    HandleSendConnectionNotification(lcsDisconnected);
    Terminate;
  end;
end;

procedure TLccConnectionThread.HandleSendConnectionNotification(NewConnectionState: TLccConnectionState; ContextOfThread: Boolean);
begin
  if ContextOfThread then
  begin
    ConnectionInfo.ConnectionState := NewConnectionState;
    Synchronize({$IFDEF FPC}@{$ENDIF}ConnectionStateChange);
  end else
  begin
    ConnectionInfo.ConnectionState := NewConnectionState;
    ConnectionStateChange;
  end;
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

