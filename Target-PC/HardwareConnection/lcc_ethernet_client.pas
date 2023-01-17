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
  TLccEthernetClient = class;   // Forward

  { TLccEthernetClientThread }
  TLccEthernetClientThread =  class(TLccBaseEthernetThread)
  private
    FGridConnectHelper: TGridConnectHelper;
    FidTCPClient: TIdTCPClient;
    FidThreadComponent: TIdThreadComponent;
    FOwner: TLccEthernetClient;
    protected
      property GridConnectHelper: TGridConnectHelper read FGridConnectHelper write FGridConnectHelper;
      property Owner: TLccEthernetClient read FOwner write FOwner;
      procedure Execute; override;
      procedure ReceiveMessage;  // For Syncronize
      procedure OnClientConnected(Sender: TObject);
      procedure OnClientDisconnected(Sender: TObject);
      procedure OnThreadComponentRun(Sender: TIdThreadComponent);
      procedure SetConnecting(AValue: Boolean); override;
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
    FClosingConnection: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
    property ClosingConnection: Boolean read FClosingConnection write FClosingConnection;

    function IsLccLink: Boolean; override;
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

{ TLccEthernetClient }
function TLccEthernetClient.IsLccLink: Boolean;
begin
  Result := True;
end;

function TLccEthernetClient.GetConnected: Boolean;
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

function TLccEthernetClient.GetConnecting: Boolean;
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

procedure TLccEthernetClient.SendMessage(ALccMessage: TLccMessage);
begin
  inherited SendMessage(ALccMessage);
  CriticalSectionEnter;
  try
    if Assigned(ClientThread) then
      ClientThread.AddToOutgoingBuffer(ALccMessage);
  finally
    CriticalSectionLeave;
  end;
end;

function TLccEthernetClient.OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  CloseConnection;
  inherited OpenConnection(ConnectionInfo as TLccEthernetConnectionInfo);
  Result := TLccEthernetClientThread.Create(True, Self, ConnectionInfo);
  ClientThread := Result as TLccEthernetClientThread;
  ClientThread.Owner := Self;
  ClientThread.Suspended := False;
end;

procedure TLccEthernetClient.CloseConnection;
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
  AnOwner: TLccHardwareConnectionManager;
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
var
  i: Integer;
  TxStr: string;
  TxList: TStringList;
  DynamicByteArray: TLccDynamicByteArray;
begin
  FRunning := True;
  Connecting := True;
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
      Connecting := False;

      while not IsTerminated and idTCPClient.Connected do
      begin

        if (ConnectionInfo as TLccEthernetConnectionInfo).GridConnect then
        begin
          // Get all the strings from the outgoing buffer into a single concatinated string
          TxStr := '';
          TxList := OutgoingGridConnect.LockList;
          try
            for i := 0 to TxList.Count - 1 do
              TxStr := TxStr + TxList[i] + #10;
            TxList.Clear;
          finally
            OutgoingGridConnect.UnlockList;
          end;

          // Outside of the threaded string list (so not to block the main thread sending more messages)
          // dispatch this string to all the connections
          if TxStr <> '' then
          begin
            if idTCPClient.IOHandler.Connected then
              idTCPClient.IOHandler.WriteLn(TxStr);
          end;
        end else
        begin
          // Get a block of raw TCP bytes
          DynamicByteArray := nil;
          OutgoingCircularArray.LockArray;
          try
            if OutgoingCircularArray.Count > 0 then
              OutgoingCircularArray.PullArray(DynamicByteArray);
          finally
            OutgoingCircularArray.UnLockArray;
          end;

          // Outside of the threaded Byte Array (so not to block the main thread sending more messages)
          // dispatch this data to all the connections
          if Length(DynamicByteArray) > 0 then
          begin
            if idTCPClient.IOHandler.Connected then
              idTCPClient.IOHandler.Write(TIdBytes( DynamicByteArray), Length(DynamicByteArray));
          end;
        end;

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

procedure TLccEthernetClientThread.ReceiveMessage;
begin
  (Owner as TLccEthernetClient).DoReceiveMessage(WorkerMessage);
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
var
  AString: String;
  AChar: AnsiChar;
  iString: Integer;
  GridConnectStrPtr: PGridConnectString;
  MessageStr: String;
  AByte: Byte;
  TcpMessage: TLccDynamicByteArray;
begin
  if (ConnectionInfo as TLccEthernetConnectionInfo).GridConnect then
  begin
    AString := '';
    while not IdTCPClient.IOHandler.InputBufferIsEmpty and IdTCPClient.IOHandler.Connected do
    begin
      AChar := AnsiChar(idTCPClient.IOHandler.ReadByte);
      AString := AString + string(AChar);
    end;

    if AString <> '' then
    begin
      for iString := 1 to Length(AString) do
      begin
        // Take the incoming characters and try to make a valid gridconnect message
        GridConnectStrPtr := nil;
        if GridConnectHelper.GridConnect_DecodeMachine(Ord(AString[iString]), GridConnectStrPtr) then
        begin
          // Have a valid gridconnect message
          MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
          WorkerMessage.LoadByGridConnectStr(MessageStr);

          // Message may only be part of a larger string of messages to make up a full LCC message.
          // This call will concatinate these partial Lcc message and return with a fully qualified
          // Lcc message.
          case GridConnectMessageAssembler.IncomingMessageGridConnect(WorkerMessage) of
            imgcr_True :
              begin
                Owner.NodeManager.ReceiveMessageServerThread.ReceiveMessageServerAddMessage(WorkerMessage);
                try
                  if not Terminated then
                    Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessage);  // WorkerMessage contains the message
                except
                end;
              end;
            imgcr_ErrorToSend :
              begin
       //         ConnectionInfo.LccMessage.CopyToTarget(WorkerMessage);
       //       if not Terminated then
       //         Synchronize({$IFDEF FPC}@{$ENDIF}RequestErrorMessageSent);
              end;
            imgcr_False,
            imgcr_UnknownError :
              begin

              end;
          end;
        end;
      end;
    end;
  end else
  begin
    while not IdTCPClient.IOHandler.InputBufferIsEmpty and IdTCPClient.IOHandler.Connected do
    begin
      TcpMessage := nil;
      AByte := idTCPClient.IOHandler.ReadByte;
      if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(AByte, TcpMessage) then
      begin
        if WorkerMessage.LoadByLccTcp(TcpMessage) then
        begin
          Owner.NodeManager.ReceiveMessageServerThread.ReceiveMessageServerAddMessage(WorkerMessage);
          try
            if not Terminated then
              Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessage);  // WorkerMessage contains the message
          except
          end;
        end
      end;
    end;
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
