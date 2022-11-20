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
    protected
      property GridConnectHelper: TGridConnectHelper read FGridConnectHelper write FGridConnectHelper;
      procedure Execute; override;
      procedure ReceiveMessage;  // For Syncronize
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
    FClosingConnection: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
    property ClosingConnection: Boolean read FClosingConnection write FClosingConnection;

    function IsLccLink: Boolean; override;
    function GetConnected: Boolean; override;
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
  if Assigned(ClientThread) and Assigned(ClientThread.IdTCPClient) then
    Result := ClientThread.idTCPClient.Connected;
end;

procedure TLccEthernetClient.SendMessage(ALccMessage: TLccMessage);
begin
  inherited SendMessage(ALccMessage);
  if Assigned(ClientThread) then
    ClientThread.AddToOutgoingBuffer(ALccMessage);
end;

function TLccEthernetClient.OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  ClosingConnection := False;
  inherited OpenConnection(ConnectionInfo as TLccEthernetConnectionInfo);
  Result := TLccEthernetClientThread.Create(True, Self, ConnectionInfo);
  FClientThread := Result as TLccEthernetClientThread;
  ClientThread.Suspended := False;
end;
procedure TLccEthernetClient.CloseConnection;
var
  TimeCount: Integer;
begin
  if not ClosingConnection then  // Stop reentrant from that nasty ProcessMessage
  begin
    inherited CloseConnection;
    TimeCount := 0;
    ClientThread.Terminate;
    while ClientThread.Running do
    begin
      // Needed to make Syncronize function if it is called during the thread shut down for notifications
      {$IFNDEF FPC_CONSOLE_APP}Application.ProcessMessages;{$ELSE}CheckSynchronize();{$ENDIF}  // Pump the timers
      Inc(TimeCount);
      Sleep(100);
      if TimeCount = 10 then Break // Something went really wrong
    end;
    FreeAndNil(FClientThread);
  end;
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
var
  i: Integer;
  TxStr: string;
  TxList: TStringList;
  DynamicByteArray: TLccDynamicByteArray;
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
                idTCPClient.IOHandler.Write(DynamicByteArray, Length(DynamicByteArray));
            end;
          end;

          Sleep(50);
        end;
      finally
        HandleSendConnectionNotification(lcsDisconnecting);
      end
    finally
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

procedure TLccEthernetClientThread.ReceiveMessage;
begin
  (Owner as TLccEthernetClient).DoReceiveMessage(WorkerMessage);
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
  iString: Integer;
  GridConnectStrPtr: PGridConnectString;
  MessageStr: String;
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
              Owner.NodeManager.ReceiveMessageServerThread.AddMessage(WorkerMessage);
              try
                Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessage);  // WorkerMessage contains the message
              except
              end;
            end;
          imgcr_ErrorToSend :
            begin
     //         ConnectionInfo.LccMessage.CopyToTarget(WorkerMessage);
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
    // https://stackoverflow.com/questions/64593756/delphi-rio-indy-tcpserver-high-cpu-usage
    // There is another way to do this but with this simple program this is fine
  IndySleep(50);
end;

end.
