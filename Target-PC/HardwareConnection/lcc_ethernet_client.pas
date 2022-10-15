unit lcc_ethernet_client;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$I ..\..\lcc_compilers.inc}
interface
uses
  Classes, SysUtils,
  {$IFDEF FPC}
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
  lcc_defines,
  lcc_node_manager,
  lcc_node_messages,
  lcc_utilities,
  lcc_app_common_settings,
  lcc_common_classes,
  lcc_ethernet_common;
type
  TLccEthernetClient = class;   // Forward

  { TLccEthernetClientThread }
  TLccEthernetClientThread =  class(TLccBaseEthernetThread)
  private
    FGridConnectHelper: TGridConnectHelper;
    FidTCPClient: TIdTCPClient;
    FidThreadComponent: TIdThreadComponent;
    protected
      property GridConnectHelper: TGridConnectHelper read FGridConnectHelper write FGridConnectHelper;
      procedure Execute; override;
      procedure OnClientConnected(Sender: TObject);
      procedure OnClientDisconnected(Sender: TObject);
      procedure OnThreadComponentRun(Sender: TIdThreadComponent);
    public
      property idTCPClient: TIdTCPClient read FidTCPClient write FidTCPClient;
      property idThreadComponent: TIdThreadComponent read FidThreadComponent write FidThreadComponent;
      constructor Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo); override;
      destructor Destroy; override;
  end;

  { TLccEthernetClient }
  TLccEthernetClient = class(TLccEthernetHardwareConnectionManager)
  private
    FClientThread: TLccEthernetClientThread;
    { Private declarations }
  protected
    { Protected declarations }
    function IsLccLink: Boolean; override;
    function GetConnected: Boolean; override;
  public
    { Public declarations }
    property ClientThread: TLccEthernetClientThread read FClientThread write FClientThread;

    function OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread; override;
    procedure CloseConnection; override;
    procedure SendMessage(ALccMessage: TLccMessage); override;
    procedure ReceiveMessage; override;
  published
    { Published declarations }
  end;

implementation

{ TLccEthernetClient }
function TLccEthernetClient.IsLccLink: Boolean;
begin
  Result := True;
end;

function TLccEthernetClient.GetConnected: Boolean;
begin
  Result := False;
  if Assigned(ClientThread) and Assigned(ClientThread.IdTCPClient) then
    Result := ClientThread.idTCPClient.Connected;
end;

procedure TLccEthernetClient.SendMessage(ALccMessage: TLccMessage);
begin
  inherited SendMessage(ALccMessage);
  if Assigned(ClientThread) then
    ClientThread.OutgoingGridConnect.Add(ALccMessage.ConvertToGridConnectStr(#10, False));
end;

procedure TLccEthernetClient.ReceiveMessage;
begin
  if Assigned(ClientThread) then
  begin
    NodeManager.ReceiveMessage(ClientThread.TryReceiveWorkerMessage);
    DoReceiveMessage(ClientThread.TryReceiveWorkerMessage);
  end;
end;

function TLccEthernetClient.OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  inherited OpenConnection(ConnectionInfo as TLccEthernetConnectionInfo);
  Result := TLccEthernetClientThread.Create(True, Self, ConnectionInfo);
  FClientThread := Result as TLccEthernetClientThread;
  ClientThread.Suspended := False;
end;
procedure TLccEthernetClient.CloseConnection;
var
  TimeCount: Integer;
begin
  inherited CloseConnection;
  TimeCount := 0;
  ClientThread.Terminate;
  while ClientThread.Running do
  begin
    {$IFNDEF FPC_CONSOLE_APP}Application.ProcessMessages;{$ELSE}CheckSynchronize();{$ENDIF}  // Pump the timers
    Inc(TimeCount);
    Sleep(100);
    if TimeCount = 10 then Break // Something went really wrong
  end;
  FreeAndNil(FClientThread);
end;
{ TLccEthernetClientThread }
constructor TLccEthernetClientThread.Create(CreateSuspended: Boolean;
  AnOwner: TLccHardwareConnectionManager;
  AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited;

end;

destructor TLccEthernetClientThread.Destroy;
begin

  inherited;
end;

procedure TLccEthernetClientThread.Execute;
begin
  FRunning := True;
  HandleSendConnectionNotification(lcsConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  idTCPClient := TIdTCPClient.Create();
  idThreadComponent := TIdThreadComponent.Create();
  try
    idTCPClient.IPVersion := Id_IPv4;
    idTCPClient.ConnectTimeout := 0;
    idTCPClient.OnConnected     := {$IFDEF FPC}@{$ENDIF}OnClientConnected;
    idTCPClient.OnDisconnected  := {$IFDEF FPC}@{$ENDIF}OnClientDisconnected;
    idThreadComponent.OnRun := {$IFDEF FPC}@{$ENDIF}OnThreadComponentRun;
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
      try
        while not IsTerminated and idTCPClient.Connected do
        begin
          if ConnectionInfo.Gridconnect then   // Handle the Client using GridConnect
            TryTransmitGridConnect(idTCPClient.IOHandler)
          else
            TryTransmitTCPProtocol(idTCPClient.IOHandler);
          Sleep(1);
        end;
      finally
        HandleSendConnectionNotification(lcsDisconnecting);
        if ConnectionInfo.Gridconnect then
          TryTransmitGridConnect(idTCPClient.IOHandler) // Flush last message it
        else
          TryTransmitTCPProtocol(idTCPClient.IOHandler);;
      end;
    finally
      while not idTCPClient.IOHandler.InputBufferIsEmpty do
      begin
        IndySleep(500); // empty the buffer in the call back
      end;
      IndySleep(2000);
      idThreadComponent.Active := False;
      idTCPClient.Disconnect;
      HandleSendConnectionNotification(lcsDisconnected);
      idTCPClient.Free;
      idThreadComponent.Free;
      GridConnectHelper.Free;
      FRunning := False;
    end;
  except   // Indy uses exceptions to trigger problems
    on E: EIdException do
    begin
      ConnectionInfo.ErrorMessage := E.Message;
      Synchronize({$IFDEF FPC}@{$ENDIF}ErrorMessage);
    end;
  end;
end;

procedure TLccEthernetClientThread.OnClientConnected(Sender: TObject);
begin
  HandleSendConnectionNotification(lcsConnected);
  idThreadComponent.Active := True;
end;
procedure TLccEthernetClientThread.OnClientDisconnected(Sender: TObject);
begin
  HandleSendConnectionNotification(lcsDisconnected);
  if idThreadComponent.Active then
    idThreadComponent.Active := False;
end;

procedure TLccEthernetClientThread.OnThreadComponentRun(Sender: TIdThreadComponent);
var
  AString: String;
  AChar: AnsiChar;
begin
  AString := '';
  while not IdTCPClient.IOHandler.InputBufferIsEmpty and IdTCPClient.IOHandler.Connected do
  begin
    AChar := AnsiChar(idTCPClient.IOHandler.ReadByte);
    AString := AString + string(AChar);
  end;

  if AString <> '' then
  begin
    if ConnectionInfo.Gridconnect then
      TryReceiveGridConnect(AString, GridConnectHelper)
    else
      TryReceiveTCPProtocol(AString)
  end;
    // https://stackoverflow.com/questions/64593756/delphi-rio-indy-tcpserver-high-cpu-usage
    // There is another way to do this but with this simple program this is fine
  IndySleep(200);
end;

end.
