unit lcc_ethernet_client;
{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$I ..\..\lcc_compilers.inc}
interface
uses
  Classes, SysUtils,
  {$IFDEF LCC_FPC}
    {$IFNDEF FPC_CONSOLE_APP}
      LResources,
      Forms,
      Controls,
      Graphics,
      Dialogs,
    {$ENDIF}
  {$ELSE}
    FMX.Forms,
    Types,
    System.Generics.Collections,
  {$ENDIF}
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdThreadComponent,
  IdGlobal,
  IdException,
  lcc_node_messages_can_assembler_disassembler,
  lcc_gridconnect,
  lcc_ethernet_tcp,
  lcc_defines,
  lcc_node,
  lcc_node_messages,
  lcc_utilities,
  lcc_app_common_settings,
  lcc_common_classes,
  lcc_ethernet_common,
  lcc_alias_server;

type
  TLccEthernetClientThreadManager = class;   // Forward

  { TLccEthernetClientThread }
  TLccEthernetClientThread =  class(TLccConnectionThread)
  private
    FGridConnectHelper: TGridConnectDecodeStateMachine;
    FidTCPClient: TIdTCPClient;
    FidThreadComponent: TIdThreadComponent;
    FOwner: TLccEthernetClientThreadManager;
    protected
      property GridConnectHelper: TGridConnectDecodeStateMachine read FGridConnectHelper write FGridConnectHelper;
      property Owner: TLccEthernetClientThreadManager read FOwner write FOwner;
      procedure Execute; override;
      procedure OnClientConnected(Sender: TObject);
      procedure OnClientDisconnected(Sender: TObject);
      procedure OnThreadComponentRun(Sender: TIdThreadComponent);
      procedure SetConnecting(AValue: Boolean); override;
    public
      property idTCPClient: TIdTCPClient read FidTCPClient write FidTCPClient;
      property idThreadComponent: TIdThreadComponent read FidThreadComponent write FidThreadComponent;
      constructor Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager; AConnectionInfo: TLccHardwareConnectionInfo); override;
      destructor Destroy; override;
  end;

  { TLccEthernetClientThreadManager }
  TLccEthernetClientThreadManager = class(TLccConnectionThreadManager)
  private
    FClientThread: TLccEthernetClientThread;
    FClosingConnection: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
    property ClosingConnection: Boolean read FClosingConnection write FClosingConnection;

 //   function IsLccLink: Boolean; override;
    function GetConnected: Boolean; override;
    function GetConnecting: Boolean; override;
  public
    { Public declarations }
    property ClientThread: TLccEthernetClientThread read FClientThread write FClientThread;

    function OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread; override;
    procedure CloseConnection; override;
    procedure SendMessage(ALccMessage: TLccMessage); override;
  published
    { Published declarations }
  end;

implementation

{ TLccEthernetClientThreadManager }
{function TLccEthernetClientThreadManager.IsLccLink: Boolean;
begin
  Result := True;
end;   }

function TLccEthernetClientThreadManager.GetConnected: Boolean;
begin
  Result := False;
  CriticalSectionEnter;
  try
    if Assigned(ClientThread) then
      if Assigned(ClientThread.IdTCPClient) then
        Result := ClientThread.idTCPClient.Connected;
  finally
    CriticalSectionLeave;
  end;
end;

function TLccEthernetClientThreadManager.GetConnecting: Boolean;
begin
  Result := False;
  CriticalSectionEnter;
  try
    if Assigned(ClientThread) then
      Result := ClientThread.Connecting;
  finally
    CriticalSectionLeave;
  end;
end;

procedure TLccEthernetClientThreadManager.SendMessage(ALccMessage: TLccMessage);
begin

end;

function TLccEthernetClientThreadManager.OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  CloseConnection;
  inherited OpenConnection(ConnectionInfo as TLccEthernetConnectionInfo);
  Result := TLccEthernetClientThread.Create(True, Self, ConnectionInfo);
  ClientThread := Result as TLccEthernetClientThread;
  ClientThread.Owner := Self;
  ClientThread.Suspended := False;
end;

procedure TLccEthernetClientThreadManager.CloseConnection;
var
  TimeCount: Integer;
begin
  NodeManager.Clear;
  inherited CloseConnection;
  if Assigned(ClientThread) then
  begin
    try
      TimeCount := 0;
      ClientThread.HandleSendConnectionNotification(lcsDisconnecting, False);
      ClientThread.Terminate;
      while ClientThread.Running do
      begin
        Inc(TimeCount);
        Sleep(50);
        if TimeCount = 100 then
        begin
          Break // Something went really wrong
        end;
      end;
      if ClientThread.ErrorOnExit then
        ClientThread.ErrorMessage;
      ClientThread.HandleSendConnectionNotification(lcsDisconnected, False);
    finally
      FreeAndNil(FClientThread);
    end;
  end;
end;

{ TLccEthernetClientThread }
constructor TLccEthernetClientThread.Create(CreateSuspended: Boolean;
  AnOwner: TLccConnectionThreadManager;
  AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited;
//  WriteLn('TLccEthernetClientThread Create');
end;

destructor TLccEthernetClientThread.Destroy;
begin
//  WriteLn('TLccEthernetClientThread Destroy');
  inherited;
end;

procedure TLccEthernetClientThread.Execute;
begin
  FRunning := True;
  Connecting := True;
  HandleSendConnectionNotification(lcsConnecting);
  GridConnectHelper := TGridConnectDecodeStateMachine.Create;
  idTCPClient := TIdTCPClient.Create();
  idThreadComponent := TIdThreadComponent.Create();
  try
    idTCPClient.IPVersion := Id_IPv4;
    idTCPClient.ConnectTimeout := 0;
    idTCPClient.OnConnected     := {$IFDEF LCC_FPC}@{$ENDIF}OnClientConnected;
    idTCPClient.OnDisconnected  := {$IFDEF LCC_FPC}@{$ENDIF}OnClientDisconnected;
    idThreadComponent.OnRun := {$IFDEF LCC_FPC}@{$ENDIF}OnThreadComponentRun;
    if (ConnectionInfo as TLccEthernetConnectionInfo).AutoResolveIP then
    begin
      {$IFDEF LCC_WINDOWS}
      (ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := ResolveWindowsIp;
      {$ELSE}
      (ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := ResolveUnixIp;
      {$ENDIF}
    end;
    idTCPClient.Connect((ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP, (ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort);
    try
      Connecting := False;

      while not IsTerminated and idTCPClient.Connected do
      begin
        SendMessageStream.Clear;
        if LoadStreamFromMessageBuffer(SendMessageStream, SendMessageBuffer) and idTCPClient.IOHandler.Connected then
          idTCPClient.IOHandler.Write(SendMessageStream);

        Sleep(THREAD_SLEEP_TIME);
      end;
    finally
      Connecting := False;
      idThreadComponent.Active := False;
      idTCPClient.Disconnect;
      idTCPClient.Free;
      idThreadComponent.Free;
      GridConnectHelper.Free;
      FRunning := False;
    end;
  except   // Indy uses exceptions to trigger problems
    on E: EIdException do
    begin
      Connecting := False;
      ConnectionInfo.ErrorMessage := E.Message;
      ErrorOnExit := True;
      FRunning := False;
    end;
  end;
end;

procedure TLccEthernetClientThread.OnClientConnected(Sender: TObject);
begin
  if not Terminated then
    HandleSendConnectionNotification(lcsConnected);
  idThreadComponent.Active := True;
end;
procedure TLccEthernetClientThread.OnClientDisconnected(Sender: TObject);
begin
  if idThreadComponent.Active then
    idThreadComponent.Active := False;
end;

procedure TLccEthernetClientThread.OnThreadComponentRun(Sender: TIdThreadComponent);
begin

  if not IdTCPClient.IOHandler.InputBufferIsEmpty and IdTCPClient.IOHandler.Connected then
  begin
    ReceiveStream.Size := 0;
    idTCPClient.IOHandler.ReadStream(ReceiveStream);
  end;

    // https://stackoverflow.com/questions/64593756/delphi-rio-indy-tcpserver-high-cpu-usage
    // There is another way to do this but with this simple program this is fine
  IndySleep(THREAD_SLEEP_TIME);
end;

procedure TLccEthernetClientThread.SetConnecting(AValue: Boolean);
begin
  Owner.CriticalSectionEnter;
  try
    inherited SetConnecting(AValue);
  finally
    Owner.CriticalSectionLeave;
  end;
end;

end.
