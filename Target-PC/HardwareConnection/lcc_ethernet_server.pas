unit lcc_ethernet_server;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ../../lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP}
      Forms,
    {$ENDIF}
  {$ELSE}
    FMX.Forms,
    Types,
    System.Generics.Collections,
  {$ENDIF}

  IdBaseComponent,
  IdCustomTCPServer,
  IdTCPConnection,
  IdTCPServer,
  IdContext,
  IdComponent,
  IdGlobal,
  lcc_threaded_circulararray,
  lcc_utilities,
  lcc_defines,
  lcc_node_messages_can_assembler_disassembler,
  lcc_common_classes,
  lcc_node_messages,
  lcc_node_manager,
  lcc_ethernet_common,
  lcc_gridconnect;

type

  TLccEthernetListener = class;

  { TLccServerContexts }

  TLccServerContexts = class
  private
    FCircularArray: TCircularArray;
    FContext: TIdContext;
    FGridConnectHelper: TGridConnectHelper;
    FGridConnectMessageAssembler: TLccGridConnectMessageAssembler;
    FStringList: TStringList;
  public
    property Context: TIdContext read FContext write FContext;
    property StringList: TStringList read FStringList write FStringList;
    property CircularArray: TCircularArray read FCircularArray write FCircularArray;
    property GridConnectHelper: TGridConnectHelper read FGridConnectHelper;
    property GridConnectMessageAssembler: TLccGridConnectMessageAssembler read FGridConnectMessageAssembler write FGridConnectMessageAssembler;

    constructor Create(AContext: TIdContext);
    destructor Destroy; override;
  end;

  { TLccGridConnectContextsList }

  TLccGridConnectContextsList = class(TThreadList)
  private
    FOwner: TLccEthernetListener;
    function GetCount: Integer;
    function GetGridConnectContext(Index: Integer): TLccServerContexts;
  public
    property Count: Integer read GetCount;
    property GridConnectContext[Index: Integer]: TLccServerContexts read GetGridConnectContext;
    property Owner: TLccEthernetListener read FOwner;

    destructor Destroy; override;
    function Add(AContext: TIdContext): TLccServerContexts;
    function AddStringByContext(AContext: TIdContext; AString: string): Boolean;
    procedure Clear;
    function Remove(AContext: TIdContext): Boolean;
  end;

  { TLccMessageList }

  TLccMessageList = class(TThreadList)
  private
    FOwner: TLccEthernetListener;
    function GetInteger: Integer;
    function GetLccMessage(Index: Integer): TLccMessage;
  public
    property Count: Integer read GetInteger;
    property LccMessage[Index: Integer]: TLccMessage read GetLccMessage;
    property Owner: TLccEthernetListener read FOwner;

    destructor Destroy; override;
    procedure Push(AMessage: TLccMessage);
    procedure Clear;
    function Pop: TLccMessage;
  end;

  { TLccEthernetListener }

  TLccEthernetListener = class(TLccBaseEthernetThread)
  private
    FGridConnectContextList: TLccGridConnectContextsList;
    FIdTCPServer: TIdTCPServer;
    FMessageList: TLccMessageList;
    FNextMessage: TLccMessage;
    function GetIsTerminated: Boolean;
  protected
    property Running: Boolean read FRunning write FRunning;
    property IsTerminated: Boolean read GetIsTerminated;
    property IdTCPServer: TIdTCPServer read FIdTCPServer write FIdTCPServer;
    property GridConnectContextList: TLccGridConnectContextsList read FGridConnectContextList write FGridConnectContextList;
    property MessageList: TLccMessageList read FMessageList write FMessageList;
    property NextMessage: TLccMessage read FNextMessage;


    procedure IdTCPServerConnect(AContext: TIdContext);
    procedure IdTCPServerDisconnect(AContext: TIdContext);
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure IdTCPServerStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);

    procedure Execute; override;
    procedure SendMessage(AMessage: TLccMessage); override;
    procedure ReceiveMessage; override;
  public
  end;

  { TLccEthernetServer }

  TLccEthernetServer = class(TLccEthernetHardwareConnectionManager)
  private
    FListenerThread: TLccEthernetListener;
    { Private declarations }
  protected
    { Protected declarations }
    function CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener; virtual;
    function IsLccLink: Boolean; override;
    function GetConnected: Boolean; override;
  public
    { Public declarations }

    function OpenConnection(AConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread; override;
    procedure CloseConnection;  override;

    property ListenerThread: TLccEthernetListener read FListenerThread write FListenerThread;
  end;


implementation

{ TLccMessageList }

function TLccMessageList.GetLccMessage(Index: Integer): TLccMessage;
var
  List: TList;
begin
  List := LockList;
  try
    Result := TLccMessage( List[Index]);
  finally
    UnlockList;
  end;
end;

function TLccMessageList.GetInteger: Integer;
var
  List: TList;
begin
  List := LockList;
  try
    Result := List.Count
  finally
    UnlockList;
  end;
end;

destructor TLccMessageList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLccMessageList.Push(AMessage: TLccMessage);
var
  List: TList;
begin
  List := LockList;
  try
    List.Add(AMessage);
  finally
    UnlockList;
  end;
end;

procedure TLccMessageList.Clear;
var
  List: TList;
  i: Integer;
begin
  List := LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject(List[i]).Free;
  finally
    List.Clear;
    UnlockList;
  end;
end;

function TLccMessageList.Pop: TLccMessage;
var
  List: TList;
begin
  Result := nil;
  List := LockList;
  try
    if List.Count > 0 then
    begin
      Result := TLccMessage( List[0]);
      List.Delete(0);
    end;
  finally
    UnlockList;
  end;
end;

{ TLccGridConnectContextsList }

function TLccGridConnectContextsList.GetGridConnectContext(Index: Integer): TLccServerContexts;
var
  List: TList;
begin
  List := LockList;
  try
    Result := TLccServerContexts(List[Index]);
  finally
    UnlockList;
  end;
end;

function TLccGridConnectContextsList.GetCount: Integer;
var
  List: TList;
begin
  List := LockList;
  try
    Result := List.Count
  finally
    UnlockList;
  end;
end;

destructor TLccGridConnectContextsList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TLccGridConnectContextsList.Add(AContext: TIdContext): TLccServerContexts;
var
  List: TList;
  i: Integer;
  Found: Boolean;
begin
  Found := False;
  Result := nil;
  List := LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      if TLccServerContexts(List[i]).Context = AContext then
      begin
        Found := True;
        Result := TLccServerContexts(List[i]);
        Break;
      end;
    end;
    if not Found then
    begin
      Result := TLccServerContexts.Create(AContext);
      List.Add(Result);
    end;
  finally
    UnlockList;
  end;
end;

procedure TLccGridConnectContextsList.Clear;
var
  List: TList;
  i: Integer;
begin
  List := LockList;
  try
    for i := 0 to List.Count - 1 do
      TObject(List[i]).Free;
  finally
    List.Clear;
    UnlockList;
  end;
end;

function TLccGridConnectContextsList.Remove(AContext: TIdContext): Boolean;
var
  List: TList;
  i: Integer;
begin
  Result := False;
  List := LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      if TLccServerContexts(List[i]).Context = AContext then
      begin
        List.Delete(i);
        Result := True;
        Break;
      end;
    end;
  finally
    UnlockList;
  end;
end;

function TLccGridConnectContextsList.AddStringByContext(AContext: TIdContext; AString: string): Boolean;
var
  List: TList;
  i, j: Integer;
  ServerContext: TLccServerContexts;
  GridConnectStrPtr: PGridConnectString;
  MessageStr: String;
  Message: TLccMessage;
begin
  Result := False;
  List := LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      ServerContext := TLccServerContexts(List[i]);
      if ServerContext.Context = AContext then
      begin
        for j := 1 to Length(AString) do
        begin
          GridConnectStrPtr := nil;
          if ServerContext.GridConnectHelper.GridConnect_DecodeMachine(Ord(AString[i]), GridConnectStrPtr) then
          begin
            MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
            Message := TLccMessage.Create;
            Message.LoadByGridConnectStr(MessageStr);

            case ServerContext.GridConnectMessageAssembler.IncomingMessageGridConnect(Message) of
              imgcr_True :
                begin
                  Result := True;
                  Owner.MessageList.Add(Message);
                end;
              imgcr_ErrorToSend :
                begin
                  Message.Free;
         //         ConnectionInfo.LccMessage.CopyToTarget(WorkerMessage);
         //         Synchronize({$IFDEF FPC}@{$ENDIF}RequestErrorMessageSent);
                end;
              imgcr_False,
              imgcr_UnknownError :
                begin
                  Message.Free;
                end;
            end;
          end;
        end;

        Result := True;
        Break;
      end;
    end;
  finally
    List.Clear;
    UnlockList;
  end;
end;

{ TLccServerContexts }

constructor TLccServerContexts.Create(AContext: TIdContext);
begin
  FContext := AContext;
  FStringList := TStringList.Create;
  FCircularArray := TCircularArray.Create;
  FGridConnectHelper := TGridConnectHelper.Create;
  FGridConnectMessageAssembler := TLccGridConnectMessageAssembler.Create;
end;

destructor TLccServerContexts.Destroy;
begin
  inherited;
  FreeAndNil(FStringList);
  FreeAndNil(FCircularArray);
  FreeAndNil(FGridConnectHelper);
  FreeAndNil(FGridConnectMessageAssembler);
end;

{ TLccEthernetListener }

procedure TLccEthernetListener.Execute;
var
  i: Integer;
begin
  FRunning := True;
  FGridConnectContextList := TLccGridConnectContextsList.Create;
  FMessageList := TLccMessageList.Create;
  IdTCPServer := TIdTCPServer.Create(nil);
  try
    try
      IdTCPServer.Active          := False;
      IdTCPServer.MaxConnections  := 255;
        // ... assign a new context class (if you need)
     // IdTCPServer.ContextClass    := TYourContext;

      // add some callback functions
      IdTCPServer.OnConnect := {$IFDEF FPC}@{$ENDIF}IdTCPServerConnect;
      IdTCPServer.OnDisconnect := {$IFDEF FPC}@{$ENDIF}IdTCPServerDisconnect;
      IdTCPServer.OnExecute := {$IFDEF FPC}@{$ENDIF}IdTCPServerExecute;
      IdTCPServer.OnStatus := {$IFDEF FPC}@{$ENDIF}IdTCPServerStatus;
      IdTCPServer.TerminateWaitTime := 2;


      HandleSendConnectionNotification(lcsConnecting);

      if (ConnectionInfo as TLccEthernetConnectionInfo).AutoResolveIP then
      begin
        {$IFDEF LCC_WINDOWS}
        (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := ResolveWindowsIp
        {$ELSE}
        (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := ResolveUnixIp;
        {$ENDIF}
      end;

      IdTCPServer.Bindings.Clear;
      IdTCPServer.Bindings.Add.Port := 12021;
      IdTCPServer.Bindings.Add.IP := (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP;

      IdTCPServer.Active := True;

      if IdTCPServer.Active then
      begin
        HandleSendConnectionNotification(lcsConnected);
        while not Terminated do
        begin
          FNextMessage := MessageList.Pop;
          if Assigned(NextMessage) then
            Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessage);
          IndySleep(200);
        end
      end;
    finally
      HandleSendConnectionNotification(lcsDisconnecting);
      try
        IdTCPServer.Active := False;
        for i := 0 to 9 do
          IndySleep(IdTCPServer.TerminateWaitTime div 10);   // Wait for the client threads to end and stop sending data to Execute
      finally
        HandleSendConnectionNotification(lcsDisconnected);
        IdTCPServer.Free;
        GridConnectContextList.Free;
        MessageList.Free;
      end;
    end;
  except  // idTCPServer uses exceptions to throw faults, trap them so the users does not see them

  //  ConnectionInfo.ErrorCode := Socket.LastError;
  //  ConnectionInfo.MessageStr := Socket.LastErrorDesc;
    Synchronize({$IFDEF FPC}@{$ENDIF}ErrorMessage);
  end;
end;

procedure TLccEthernetListener.SendMessage(AMessage: TLccMessage);
var
  i: Integer;
begin
  for i := 0 to 10 do
  begin
    sleep(10);
  end;
  AMessage := AMessage;
  // must override abstract methods
end;

procedure TLccEthernetListener.ReceiveMessage;
begin
  Owner.NodeManager.ReceiveMessage(Owner as IHardwareConnectionManagerLink, NextMessage);
  FreeAndNil(FNextMessage);
end;

function TLccEthernetListener.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TLccEthernetListener.IdTCPServerConnect(AContext: TIdContext);
begin
  GridConnectContextList.Add(AContext);
  (ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := AContext.Binding.PeerIP;
  (ConnectionInfo as TLccEthernetConnectionInfo).ClientPort := AContext.Binding.PeerPort;
  HandleSendConnectionNotification(lcsClientConnected);
end;

procedure TLccEthernetListener.IdTCPServerDisconnect(AContext: TIdContext);
begin
  GridConnectContextList.Remove(AContext);
  (ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := AContext.Binding.PeerIP;
  (ConnectionInfo as TLccEthernetConnectionInfo).ClientPort := AContext.Binding.PeerPort;
  HandleSendConnectionNotification(lcsClientDisconnected);
end;

procedure TLccEthernetListener.IdTCPServerExecute(AContext: TIdContext);
var
  Str: string;
  Char: AnsiChar;
begin
  // Well well this just got more complicated... Need to have a separate buffer for each Context...
  Str := '';
  while not AContext.Connection.IOHandler.InputBufferIsEmpty and AContext.Connection.IOHandler.Connected do
  begin
    Char := AnsiChar(AContext.Connection.IOHandler.ReadByte);
    Str := Str + string(Char);
  end;

  if Str <> '' then
    GridConnectContextList.AddStringByContext(AContext, Str);

     // https://stackoverflow.com/questions/64593756/delphi-rio-indy-tcpserver-high-cpu-usage
    // There is another way to do this but with this simple program this is fine
  IndySleep(500);
end;

procedure TLccEthernetListener.IdTCPServerStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin   {
  case AStatus of
    hsConnecting    : HandleSendConnectionNotification(lcsConnecting);
    hsConnected     : HandleSendConnectionNotification(lcsConnected);
    hsDisconnecting : HandleSendConnectionNotification(lcsDisconnecting);
    hsDisconnected  : HandleSendConnectionNotification(lcsDisconnected);
  end;     }
end;

{ TLccEthernetServer }

procedure TLccEthernetServer.CloseConnection;
var
  TimeCount: Integer;
begin
  inherited CloseConnection;

  if Assigned(ListenerThread) then
  begin
    TimeCount := 0;
    ListenerThread.Terminate;
    while ListenerThread.Running do
    begin
      {$IFNDEF FPC_CONSOLE_APP}Application.ProcessMessages;{$ELSE}CheckSynchronize();{$ENDIF}  // Pump the timers
      Inc(TimeCount);
      Sleep(100);
      if TimeCount = 10 then Break // Something went really wrong
    end;
    FreeAndNil(FListenerThread);
  end;
end;

function TLccEthernetServer.IsLccLink: Boolean;
begin
  Result := True;
end;

function TLccEthernetServer.GetConnected: Boolean;
begin
  Result := False;
  if Assigned(ListenerThread) and Assigned(ListenerThread.IdTCPServer) then
    Result := ListenerThread.IdTCPServer.Active;
end;

function TLccEthernetServer.OpenConnection(AConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  inherited OpenConnection(AConnectionInfo);
  Result := CreateListenerObject(AConnectionInfo.Clone as TLccEthernetConnectionInfo);
  ListenerThread := Result as TLccEthernetListener;
  ListenerThread.Suspended := False;
end;

function TLccEthernetServer.CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener;
begin
  Result := TLccEthernetListener.Create(True, Self, AConnectionInfo);
end;

end.

