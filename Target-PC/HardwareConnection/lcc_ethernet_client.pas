unit lcc_ethernet_client;

{$I ../../lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

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
  lcc_connection_common,
  lcc_ethernet_common,
  lcc_alias_server_thread,
  lcc_alias_server;

type
  TLccEthernetClientThreadManager = class;   // Forward

  { TLccEthernetClientThread }
  TLccEthernetClientThread =  class(TLccConnectionThread)
  private
    FGridConnectDecodeStateMachine: TGridConnectDecodeStateMachine;
    FGridConnectMessageAssembler: TLccGridConnectMessageAssembler;
    FidTCPClient: TIdTCPClient;
    FidThreadComponent: TIdThreadComponent;
    FTcpDecodeStateMachine: TTcpDecodeStateMachine;
    protected
      property GridConnectDecodeStateMachine: TGridConnectDecodeStateMachine read FGridConnectDecodeStateMachine write FGridConnectDecodeStateMachine;
      property GridConnectMessageAssembler: TLccGridConnectMessageAssembler read FGridConnectMessageAssembler write FGridConnectMessageAssembler;
      property TcpDecodeStateMachine: TTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;

      procedure Execute; override;
      procedure OnClientConnected(Sender: TObject);
      procedure OnClientDisconnected(Sender: TObject);
      procedure OnThreadComponentRun(Sender: TIdThreadComponent);
    public
      property idTCPClient: TIdTCPClient read FidTCPClient write FidTCPClient;
      property idThreadComponent: TIdThreadComponent read FidThreadComponent write FidThreadComponent;

      constructor Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager); override;
      destructor Destroy; override;
  end;

  { TLccEthernetClientThreadManager }
  TLccEthernetClientThreadManager = class(TLccConnectionThreadManager)
  private
    FClientThread: TLccEthernetClientThread;
    { Private declarations }
  protected
    { Protected declarations }

  public
    { Public declarations }
    property ClientThread: TLccEthernetClientThread read FClientThread write FClientThread;

    function OpenConnection: TLccConnectionThread; override;
    procedure CloseConnection; override;
  published
    { Published declarations }
  end;

  TLccEthernetClientThreadManagerClass = class of TLccEthernetClientThreadManager;

implementation

{ TLccEthernetClientThreadManager }

function TLccEthernetClientThreadManager.OpenConnection: TLccConnectionThread;
begin
  CloseConnection;
  inherited OpenConnection;
  Result := TLccEthernetClientThread.Create(True, Self);
  if Assigned(Result) then
  begin
    ClientThread := Result as TLccEthernetClientThread;
    ConnectionThreadList.Add(ClientThread);  // Add to the Internal List
    ClientThread.Suspended := False;
  end;
end;

procedure TLccEthernetClientThreadManager.CloseConnection;
var
  TimeCount: Integer;
begin
  inherited CloseConnection;
  if Assigned(ClientThread) then
  begin
    try
      ConnectionThreadList.Remove(ClientThread);
      TimeCount := 0;
      ClientThread.HandleSendConnectionChangeNotify(lcsDisconnecting, False);
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
      ClientThread.HandleSendConnectionChangeNotify(lcsDisconnected, False);
    finally
      FreeAndNil(FClientThread);
    end;
  end;
end;

{ TLccEthernetClientThread }
constructor TLccEthernetClientThread.Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager);
begin
  inherited Create(CreateSuspended, AnOwner);
  GridConnectDecodeStateMachine := TGridConnectDecodeStateMachine.Create;
  FGridConnectMessageAssembler := TLccGridConnectMessageAssembler.Create;
  FTcpDecodeStateMachine := TTcpDecodeStateMachine.Create;
  idTCPClient := TIdTCPClient.Create();
  idThreadComponent := TIdThreadComponent.Create();
end;

destructor TLccEthernetClientThread.Destroy;
begin
  FreeAndNil(FidTCPClient);
  FreeAndNil(FidThreadComponent);
  FreeAndNil(FGridConnectDecodeStateMachine);
  FreeAndNil(FGridConnectMessageAssembler);
  FreeAndNil(FTcpDecodeStateMachine);
  inherited;
end;

procedure TLccEthernetClientThread.Execute;
var
  LocalListenerPort: Word;
  LocalListenerIP: String;
begin
  Running := True;
  try
    HandleSendConnectionChangeNotify(lcsConnecting, True);
    try
      idTCPClient.IPVersion := Id_IPv4;
      idTCPClient.ConnectTimeout := 4000;  // Default 0 mean forever
      idTCPClient.OnConnected     := {$IFDEF LCC_FPC}@{$ENDIF}OnClientConnected;
      idTCPClient.OnDisconnected  := {$IFDEF LCC_FPC}@{$ENDIF}OnClientDisconnected;
      idThreadComponent.OnRun := {$IFDEF LCC_FPC}@{$ENDIF}OnThreadComponentRun;

      OwnerConnectionManager.CriticalSectionEnter;
      try
        if (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).AutoResolveIP then
        begin
          {$IFDEF LCC_WINDOWS}
          (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := ResolveWindowsIp;
          {$ELSE}
          (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := ResolveUnixIp;
          {$ENDIF}
        end;
        LocalListenerIP := (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP;
        LocalListenerPort := (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort;
      finally
        OwnerConnectionManager.CriticalSectionLeave;
      end;

      idTCPClient.Connect(LocalListenerIP, LocalListenerPort);
      try
        while not IsTerminated and idTCPClient.Connected do
        begin
          if LoadStreamFromMessageBuffer(SendStreamConnectionThread, SendMessageLccMessageBuffer) then
          begin
            SendStreamConnectionThread.Position := 0;
            idTCPClient.IOHandler.Write(SendStreamConnectionThread, SendStreamConnectionThread.Size);
          end;
        end;

        IndySleep(THREAD_SLEEP_TIME);
      finally
        idThreadComponent.Active := False;
        idTCPClient.Disconnect;
      end;
    except   // Indy uses exceptions to trigger problems
      on E: EIdException do
      begin
        OwnerConnectionManager.ConnectionInfo.ErrorMessage := E.Message;
        // Order important here to help with applications and auto connect timers and such... make
        // sure it is signaled disconnected before the Error comes so it can trigger a retry
        HandleSendConnectionChangeNotify(lcsDisconnected, True);
        HandleErrorAndDisconnect(OwnerConnectionManager.ConnectionInfo.SuppressErrorMessages, True);
      end;
    end;
  finally
    Running := False;
  end;
end;

procedure TLccEthernetClientThread.OnClientConnected(Sender: TObject);
begin
  if not Terminated then
    HandleSendConnectionChangeNotify(lcsConnected, True);
  idThreadComponent.Active := True;
end;
procedure TLccEthernetClientThread.OnClientDisconnected(Sender: TObject);
begin
  if idThreadComponent.Active then
    idThreadComponent.Active := False;
end;

procedure TLccEthernetClientThread.OnThreadComponentRun(Sender: TIdThreadComponent);
var
  iData: Integer;
  LocalDataArray: TLccDynamicByteArray;
  GridConnectStrPtr: PGridConnectString;
  MessageStr: String;
begin

  if not IdTCPClient.IOHandler.InputBufferIsEmpty then
  begin
    ReceiveStreamConnectionThread.Position := 0;
    ReceiveStreamConnectionThread.Size := 0;    // Would this set Postion too?
    idTCPClient.IOHandler.ReadStream(ReceiveStreamConnectionThread, idTCPClient.IOHandler.InputBuffer.Size);



    ReceiveStreamConnectionThread.Position := 0;

    if OwnerConnectionManager.EmulateCanBus then
    begin

      for iData := 0 to ReceiveStreamConnectionThread.Size - 1 do
      begin
        // Take the incoming characters and try to make a valid gridconnect message
        GridConnectStrPtr := nil;

        if GridConnectDecodeStateMachine.GridConnect_DecodeMachine(StreamReadByte(ReceiveStreamConnectionThread), GridConnectStrPtr) then
        begin     // Have a valid gridconnect message
          MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
          WorkerMessage.LoadByGridConnectStr(MessageStr);
          WorkerMessage.ConnectionThread := Self;

          // Not a fan of having it here to block the main connection thread but need to hook into the raw individual messages.
          // after the next call to GridConnectMessageAssembler split up CAN messages will be recombined into a single LCC message
          if Assigned(OwnerConnectionManager.OwnerConnectionFactory.OnLccMessageReceive) then
          begin
            OwnerConnectionManager.ReceiveGridConnectStringSyncronize := MessageStr;
            Synchronize({$IFDEF LCC_FPC}@{$ENDIF}OwnerConnectionManager.ReceiveGridConnectStrThoughSyncronize);
          end;

          // Message may only be part of a larger string of messages to make up a full LCC message.
          // This call will concatinate these partial Lcc message and return with a fully qualified
          // Lcc message.
          case GridConnectMessageAssembler.IncomingMessageGridConnect(WorkerMessage) of
            imgcr_True         : AliasServerThread.AddIncomingLccMessage(WorkerMessage, True);
            imgcr_ErrorToSend  : begin
                                   WorkerMessage.CheckNodeIDsBeforeDelivery := True;  // We just swapped Souce/Dest NodeID, make sure we don't feed the message back into our nodes as a received message with ourthese Alias/NodeID or we will trigger a duplicate Node/Alias
                                   WorkerMessage.CopyToTarget(OwnerConnectionManager.SendMessageWorkerMessageSyncronize);
                                   Synchronize({$IFDEF LCC_FPC}@{$ENDIF}OwnerConnectionManager.SendMessageThroughSyncronize);
                                 end;
            imgcr_False,
            imgcr_UnknownError : begin end;
          end;
        end;

      end;
    end else
    begin
      LocalDataArray := [];
      for iData := 0 to ReceiveStreamConnectionThread.Size - 1 do
      begin
        if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(StreamReadByte(ReceiveStreamConnectionThread), LocalDataArray) then
        begin
          if WorkerMessage.LoadByLccTcp(LocalDataArray) then
            AliasServerThread.AddIncomingLccMessage(WorkerMessage, False);
        end;
      end;
    end;
  end;

    // https://stackoverflow.com/questions/64593756/delphi-rio-indy-tcpserver-high-cpu-usage
    // There is another way to do this but with this simple program this is fine
  IndySleep(THREAD_SLEEP_TIME);
end;

end.
