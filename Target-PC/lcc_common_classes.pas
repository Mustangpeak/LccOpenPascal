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
  lcc_node_manager,
  lcc_app_common_settings,
  lcc_threaded_circulararray,
  lcc_ethernet_tcp,
  lcc_threaded_stringlist,
  lcc_defines,
  lcc_gridconnect,
  lcc_node_messages_can_assembler_disassembler;

type
  TLccConnectionThreadManager = class;
  TLccHardwareConnectionInfo = class;
  TLccConnectionThread = class;

  TOnHardwareConnectionInfoEvent = procedure(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo) of object;

  { TLccHardwareConnectionInfo }

  TLccHardwareConnectionInfo = class
  private
    FConnectionState: TLccConnectionState;       // OUT
    FErrorMessage: String;                       // OUT
    FGridConnect: Boolean;                       // IN
    FHub: Boolean;                               // IN
    FLccMessage: TLccMessage;                    // OUT
    FMessageStr: String;                         // OUT
    FSuppressErrorMessages: Boolean;             // IN
  public
    MessageArray: TLccDynamicByteArray;                                         // Contains the TCP Protocol message bytes if not using GridConnect

    constructor Create;
    destructor Destroy; override;

    function Clone: TLccHardwareConnectionInfo; virtual;

    property ConnectionState: TLccConnectionState read FConnectionState write FConnectionState;  // Current State of the connection
    property ErrorMessage: String read FErrorMessage write FErrorMessage;
    property GridConnect: Boolean read FGridConnect write FGridConnect;
    property Hub: Boolean read FHub write FHub;
    property LccMessage: TLccMessage read FLccMessage write FLccMessage;
    property MessageStr: String read FMessageStr write FMessageStr;             // Contains the string for the resulting message from the thread
    property SuppressErrorMessages: Boolean read FSuppressErrorMessages write FSuppressErrorMessages;
  end;

  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FConnecting: Boolean;
    FConnectionInfo: TLccHardwareConnectionInfo;
    FErrorOnExit: Boolean;
    FOwner: TLccConnectionThreadManager;
    FReceiveStream: TMemoryStream;
    FSendMessageBuffer: Classes.TThreadList;
    FSendMessageStream: TMemoryStream;
    function GetIsTerminated: Boolean;
  protected
    FRunning: Boolean;

    procedure SetConnecting(AValue: Boolean); virtual;   // virtual property setter

    procedure ConnectionStateChange; virtual;



    function LoadStreamFromMessageBuffer(AStream: TStream; AMessageBuffer: Classes.TThreadList; ClearBuffer: Boolean = True): Boolean; virtual;
    procedure SendMessage(ALccMessage: TLccMessage); virtual;

    property ReceiveStream: TMemoryStream read FReceiveStream write FReceiveStream;
    property SendMessageBuffer: Classes.TThreadList read FSendMessageBuffer write FSendMessageBuffer;
    property SendMessageStream: TMemoryStream read FSendMessageStream write FSendMessageStream;

  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager; AConnectionInfo: TLccHardwareConnectionInfo); reintroduce; virtual;
    destructor Destroy; override;

    procedure ClearSendMessageBuffer;



    procedure ErrorMessage; virtual;
    procedure HandleSendConnectionNotification(NewConnectionState: TLccConnectionState; ContextOfThread: Boolean = True); virtual;
    procedure HandleErrorAndDisconnect(SuppressMessage: Boolean; ContextOfThread: Boolean = True); virtual;

    property ConnectionInfo: TLccHardwareConnectionInfo read FConnectionInfo;
    property Connecting: Boolean read FConnecting write SetConnecting;
    property ErrorOnExit: Boolean read FErrorOnExit write FErrorOnExit;
    property Owner: TLccConnectionThreadManager read FOwner;
    property Running: Boolean read FRunning write FRunning;
    property IsTerminated: Boolean read GetIsTerminated;
  end;

  { TLccConnectionThreadManager }
  // Manager for a Thread.  The data and code in this object runs in the context of the main thread
  // so accessing anything from here is thread safe
  TLccConnectionThreadManager = class(TComponent)
  private
    FConnectionThreadList: Classes.TThreadList;
    FDefaultConnectionInfo: TLccHardwareConnectionInfo;
    FCriticalSection: TCriticalSection;



    FHub: Boolean;
    FNodeManager: TLccNodeManager;
    FOnConnectionStateChange: TOnHardwareConnectionInfoEvent;
    FOnErrorMessage: TOnHardwareConnectionInfoEvent;
    FWorkerMessage: TLccMessage;


    procedure SetDefaultConnectionInfo(AValue: TLccHardwareConnectionInfo);


  protected


    property ConnectionThreadList: Classes.TThreadList read FConnectionThreadList write FConnectionThreadList;

    procedure ClearConnectionThreadList;




    // Decendents only know what connected means to them
    function GetConnected: Boolean; virtual; abstract;    // IHardwareConnectionManagerLink
    function GetConnecting: Boolean; virtual; abstract;

    // Event call methods
    procedure DoConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    procedure DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;

    // Decendant must override this and pass the messages to the connections
    procedure SendMessage(ALccMessage: TLccMessage); virtual;

  public
    constructor Create(AOwner: TComponent; ANodeManager: TLccNodeManager); reintroduce; virtual;
    destructor Destroy; override;

    // Default Connection info, a copy is made and passed to the thread for its use when it is created
    property DefaultConnectionInfo: TLccHardwareConnectionInfo read FDefaultConnectionInfo write SetDefaultConnectionInfo;




    // True if the Manager is capabable of receiveing/sending messages on the wire... getter must be overridden
    property Connected: Boolean read GetConnected;
    property Connecting: Boolean read GetConnecting;
    //
    property Hub: Boolean read FHub write FHub;
    // The Connection Mangaer is assigned to this Connection Manager and it uses it to pass messages
    property NodeManager: TLccNodeManager read FNodeManager;


    procedure CriticalSectionEnter;
    procedure CriticalSectionLeave;
    procedure CriticalSectionTryEnter;
    // When a thread owned by the manager receives a message it will call these centraized methods
    // ----------------------

    // Puts a GridConnect string in the buffer to be sent without needing to deal with a TLccMessage as not all links are LCC, using custom GridConnect for the UART to the Command Station
    procedure SendMessageRawGridConnect(GridConnectStr: String); virtual;
    // When a thread owned by the manager receives a message it will call this centraized method
    procedure ConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    // Decendant must override this.  The Node Manager calls this when its nodes needs to send a message to the "wire".
    procedure ErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    // ----------------------

    function OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread; virtual;
    procedure CloseConnection; virtual;

  published
    property OnConnectionStateChange: TOnHardwareConnectionInfoEvent read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnHardwareConnectionInfoEvent read FOnErrorMessage write FOnErrorMessage;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  end;
  TLccConnectionThreadManagerClass = class of TLccConnectionThreadManager;

  { TLccServerManager }

  TLccServerManager = class(TComponent)
  private
    FOnLccMessageReceive: TOnMessageEvent;
    FOnLccMessageSend: TOnMessageEvent;
    FServerList: Classes.TThreadList;
  protected
    property ServerList: Classes.TThreadList read FServerList write FServerList;

    // Event Handler for messages coming in
    procedure DoReceiveMessage(LccMessage: TLccMessage); virtual;
    // Event Handler for messages going out
    procedure DoSendMessage(ALccMessage: TLccMessage); virtual;
    // Closes and frees the Servers
    procedure ClearServerList;
    // Incoming messages from the Incoming Message Thread
    procedure ReceiveMessage(ALccMessage: TLccMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateServer(AServerManagerClass: TLccConnectionThreadManagerClass; ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThreadManager;
    procedure SendMessage(ALccMessage: TLccMessage);

    property OnLccMessageReceive: TOnMessageEvent read FOnLccMessageReceive write FOnLccMessageReceive;
    property OnLccMessageSend: TOnMessageEvent read FOnLccMessageSend write FOnLccMessageSend;
  end;


implementation

{ TLccServerManager }

procedure TLccServerManager.DoReceiveMessage(LccMessage: TLccMessage);
begin
  if Assigned(OnLccMessageReceive) then
    OnLccMessageReceive(Self, LccMessage);
end;

procedure TLccServerManager.DoSendMessage(ALccMessage: TLccMessage);
begin
  if Assigned(OnLccMessageSend) then
    OnLccMessageSend(Self, ALccMessage);
end;

procedure TLccServerManager.ClearServerList;
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

procedure TLccServerManager.ReceiveMessage(ALccMessage: TLccMessage);
begin
  DoReceiveMessage(ALccMessage);

  // Need to Dispatch to the Nodes.....
end;

constructor TLccServerManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ServerList := Classes.TThreadList.Create;
end;

destructor TLccServerManager.Destroy;
begin
  ClearServerList;
  FreeAndNil(FServerList);
  inherited Destroy;
end;

function TLccServerManager.CreateServer(AServerManagerClass: TLccConnectionThreadManagerClass; ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThreadManager;
begin
  Result := AServerManagerClass.Create(Self, nil);
  Result.DefaultConnectionInfo := ConnectionInfo;
  ServerList.Add(Result);
end;

procedure TLccServerManager.SendMessage(ALccMessage: TLccMessage);
var
  i: Integer;
  List: TList;
  Manager: TLccConnectionThreadManager;
begin
  DoSendMessage(ALccMessage);

  List := ServerList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      Manager := TLccConnectionThreadManager( List[i]);
      Manager.SendMessage(ALccMessage);
    end;
  finally
    ServerList.UnlockList;
  end;
end;

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
  Result.FLccMessage := TLccMessage.Create;
  Result.SuppressErrorMessages := SuppressErrorMessages;
end;

destructor TLccHardwareConnectionInfo.Destroy;
begin
  FreeAndNil(FLccMessage);
  inherited Destroy;
end;


{ TLccConnectionThreadManager }

procedure TLccConnectionThreadManager.SetDefaultConnectionInfo(AValue: TLccHardwareConnectionInfo);
begin
  if Assigned(FDefaultConnectionInfo) then
    FreeAndNil(FDefaultConnectionInfo);
  FDefaultConnectionInfo := AValue;
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

procedure TLccConnectionThreadManager.DoConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Thread, ConnectionInfo);
end;

procedure TLccConnectionThreadManager.DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  if Assigned(OnErrorMessage) then
    OnErrorMessage(Thread, ConnectionInfo);
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

procedure TLccConnectionThreadManager.ConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  DoConnectionStateChange(Thread, ConnectionInfo);
end;

procedure TLccConnectionThreadManager.ErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  DoErrorMessage(Thread, ConnectionInfo);
end;

procedure TLccConnectionThreadManager.SendMessageRawGridConnect(GridConnectStr: String);
begin

end;

function TLccConnectionThreadManager.OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  Result := nil;

  FHub := ConnectionInfo.Hub;
end;

procedure TLccConnectionThreadManager.CloseConnection;
begin

end;

constructor TLccConnectionThreadManager.Create(AOwner: TComponent; ANodeManager: TLccNodeManager);
begin
  Assert(Assigned(ANodeManager), 'TLccHardwareConnectionManager must have an assigned TLccNodeManager');

  FConnectionThreadList := Classes.TThreadList.Create;

  inherited Create(AOwner);
  FNodeManager := ANodeManager;
  FWorkerMessage := TLccMessage.Create;
//  NodeManager.HardwarewareConnectionList.Add(Self as IHardwareConnectionManagerLink);
  FCriticalSection := TCriticalSection.Create;
  FHub := True;

end;

destructor TLccConnectionThreadManager.Destroy;
begin
 // NodeManager.HardwarewareConnectionList.Remove(Self as IHardwareConnectionManagerLink);

  ClearConnectionThreadList;
  FreeAndNil(FConnectionThreadList);
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FCriticalSection);
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

constructor TLccConnectionThread.Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager; AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FConnectionInfo := AConnectionInfo.Clone;
  FConnectionInfo.ConnectionState := lcsDisconnected;
  FConnectionInfo.FMessageStr := '';
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
    if ConnectionInfo.GridConnect then
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

end.

