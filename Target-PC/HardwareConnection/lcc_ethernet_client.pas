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
  lcc_node,
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
        end
      finally
        while not idTCPClient.IOHandler.InputBufferIsEmpty do
        begin
          IndySleep(50); // empty the buffer in the call back
        end;

   //   IndySleep(2000);
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
  NodeList, MessageStackList, MessageStackMappingList: TList;
  ThreadedNode: TLccNode;
  iString, iNode: Integer;
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
              NodeList := Owner.NodeManager.Nodes.LockList;
              try
                for iNode := 0 to NodeList.Count - 1 do
                begin
                  ThreadedNode := TLccNode(NodeList.Items[iNode]);
                  MessageStackList := ThreadedNode.MessageStack.LockList;
                  MessageStackMappingList := ThreadedNode.MessageStackMapping.LockList;
                  try
                    case WorkerMessage.CAN.MTI of
                      MTI_CAN_AMR : MessageStackMappingList.Add( WorkerMessage.Clone);
                      MTI_CAN_AMD : MessageStackMappingList.Add( WorkerMessage.Clone)
                    else
                      case WorkerMessage.MTI of
                        MTI_VERIFIED_NODE_ID_NUMBER,
                        MTI_INITIALIZATION_COMPLETE : MessageStackMappingList.Add( WorkerMessage.Clone)
                      else
                        MessageStackList.Add( WorkerMessage.Clone)
                      end;
                    end;
                  finally
                    ThreadedNode.MessageStack.UnlockList;
                    ThreadedNode.MessageStackMapping.UnlockList;
                  end;
                end;

              finally
                Owner.NodeManager.Nodes.UnlockList;
              end;

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
