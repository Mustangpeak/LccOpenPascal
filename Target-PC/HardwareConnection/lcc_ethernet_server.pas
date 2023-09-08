unit lcc_ethernet_server;

{$I ../../lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF LCC_FPC}
    Generics.Collections,
    strutils,
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
  IdIOHandler,
  IdContext,
  IdComponent,
  IdGlobal,
  IdCoderMIME,
  IdHashSHA,
  IdSSL,
  IdSSLOpenSSL,
  IdException,
  IdSocketHandle,
  lcc_threaded_circulararray,
  lcc_utilities,
  lcc_defines,
  lcc_node_messages_can_assembler_disassembler,
  lcc_common_classes,
  lcc_node_messages,
  lcc_node_manager,
  lcc_ethernet_common,
  lcc_gridconnect,
  lcc_ethernet_tcp,
  lcc_node,
  lcc_alias_server;

type

  TLccEthernetListener = class;

  { TLccConnectionContext }

  TLccConnectionContext = class
  private
    FCircularArray: TCircularArray;
    FContext: TIdContext;
    FGridConnectHelper: TGridConnectHelper;
    FGridConnectMessageAssembler: TLccGridConnectMessageAssembler;
    FStringList: TStringList;
    FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
  public
    property Context: TIdContext read FContext write FContext;
    property StringList: TStringList read FStringList write FStringList;
    property CircularArray: TCircularArray read FCircularArray write FCircularArray;
    property GridConnectHelper: TGridConnectHelper read FGridConnectHelper;
    property GridConnectMessageAssembler: TLccGridConnectMessageAssembler read FGridConnectMessageAssembler write FGridConnectMessageAssembler;
    property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;

    constructor Create(AContext: TIdContext);
    destructor Destroy; override;
  end;

  TLccConnectionContextClass = class of TLccConnectionContext;

  { TLccContextsList }

  TLccContextsList = class(Classes.TThreadList)
  private
    FDefaultContextClass: TLccConnectionContextClass;
    FOwner: TLccEthernetListener;
    FWorkerMessage: TLccMessage;
    function GetCount: Integer;
    function GetGridConnectContext(Index: Integer): TLccConnectionContext;
  protected
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

  public
    property Count: Integer read GetCount;
    property GridConnectContext[Index: Integer]: TLccConnectionContext read GetGridConnectContext;
    property Owner: TLccEthernetListener read FOwner;
    property DefaultContextClass: TLccConnectionContextClass read FDefaultContextClass write FDefaultContextClass;

    constructor Create(AnOwner: TLccEthernetListener);
    destructor Destroy; override;
    function AddContext(AContext: TIdContext): TLccConnectionContext;
    procedure AddGridConnectStringByContext(AContext: TIdContext; AString: string; NodeManager: TLccNodeManager);
    procedure AddTcpDataByContext(AContext: TIdContext; ADataArray: TIdBytes; NodeManager: TLccNodeManager);
    procedure Clear;
    function FindContext(AContext: TIdContext): TLccConnectionContext;
    function RemoveContext(AContext: TIdContext): Boolean;
  end;

  TLccEthernetServer = class; // forward

  { TLccEthernetListener }

  TLccEthernetListener = class(TLccBaseEthernetThread)
  private
    FConnectionContextList: TLccContextsList;
    FIdTCPServer: TIdTCPServer;
    FOwner: TLccEthernetServer;
  protected
    property Running: Boolean read FRunning;
    property IdTCPServer: TIdTCPServer read FIdTCPServer write FIdTCPServer;
    // List that contains the contexts (threads) the server is serviceing.
    // Allows us to keep the data coming in/going out associated with each context
    property ConnectionContextList: TLccContextsList read FConnectionContextList write FConnectionContextList;
    property Owner: TLccEthernetServer read FOwner write FOwner;

    function ReceiveContextDataAsString(AContext: TIdContext): string; virtual;
    function ReceiveContextDataAsBytes(AContext: TIdContext): TIdBytes; virtual;
    procedure SendContextDataAsString(AContext: TIdContext; AString: string); virtual;
    procedure SendContextDataAsBytes(AContext: TIdContext; ABytes: TIdBytes); virtual;

    procedure IdTCPServerConnect(AContext: TIdContext); virtual;
    procedure IdTCPServerDisconnect(AContext: TIdContext); virtual;
    procedure IdTCPServerExecute(AContext: TIdContext); virtual;
    procedure IdTCPServerStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string); virtual;

    function ParseHeader(const msg: string): TDictionary<string, string>;
    procedure ReceiveMessage;  // For Syncronize
    procedure SetConnecting(AValue: Boolean); override;

    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo); override;
    destructor Destroy; override;
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
    function GetConnecting: Boolean; override;

    procedure DoReceiveMessage(LccMessage: TLccMessage); override;
  public
    { Public declarations }
    property ListenerThread: TLccEthernetListener read FListenerThread write FListenerThread;

    function OpenConnection(AConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread; override;
    procedure CloseConnection;  override;
    procedure SendMessage(ALccMessage: TLccMessage); override;
  end;


  { TLccConnectionWebsocketContext }

  TLccConnectionWebsocketContext = class(TLccConnectionContext)
  private
    FUpgraded: Boolean;
  public
    property Upgraded: Boolean read FUpgraded write FUpgraded;
  end;

  TLccConnectionWebsocketContextClass = class of TLccConnectionWebsocketContext;


  { TLccWebSocketListener }

  TLccWebSocketListener = class(TLccEthernetListener)
  private
    FHashSHA1: TIdHashSHA1;
    FIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL;
  protected
    property IdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL read FIdServerIOHandlerSSLOpenSSL write FIdServerIOHandlerSSLOpenSSL;
    property HashSHA1: TIdHashSHA1 read FHashSHA1 write FHashSHA1;

    function ReceiveContextDataAsString(AContext: TIdContext): string; override;
    function ReceiveContextDataAsBytes(AContext: TIdContext): TIdBytes; override;
    procedure SendContextDataAsString(AContext: TIdContext; AString: string); override;
    procedure SendContextDataAsBytes(AContext: TIdContext; ABytes: TIdBytes); override;

    procedure IdTCPServerExecute(AContext: TIdContext); override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo); override;
    destructor Destroy; override;
  end;


  { TLccWebSocketServer }

  TLccWebSocketServer = class(TLccEthernetServer)
  protected
    function CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener; override;
  public
    procedure InitSSL(AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL);

  end;


implementation

{ TLccWebSocketListener }

constructor TLccWebSocketListener.Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited Create(CreateSuspended, AnOwner, AConnectionInfo);

  HashSHA1 := TIdHashSHA1.Create;
  IdServerIOHandlerSSLOpenSSL := nil;
end;

destructor TLccWebSocketListener.Destroy;
begin
  HashSHA1.Free;
  inherited Destroy;
end;

function TLccWebSocketListener.ReceiveContextDataAsString(AContext: TIdContext): string;
begin
  Result := string( IndyTextEncoding_UTF8.GetString(ReceiveContextDataAsBytes(AContext)));
end;

function TLccWebSocketListener.ReceiveContextDataAsBytes(AContext: TIdContext): TIdBytes;
var
  l: byte;
  b: array [0..7] of byte;
  i, DecodedSize: int64;
  Mask: array [0..3] of byte;
  io: TIdIOHandler;
begin
  // https://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side

  // Strip off the header from the data
  try
    io := AContext.Connection.IOHandler;
    if io.ReadByte = $81 then
    begin
      l := io.ReadByte;
      case l of
        $FE:
          begin
            b[1] := io.ReadByte; b[0] := io.ReadByte;
            b[2] := 0; b[3] := 0; b[4] := 0; b[5] := 0; b[6] := 0; b[7] := 0;
            DecodedSize := Int64(b);
          end;
        $FF:
          begin
            b[7] := io.ReadByte; b[6] := io.ReadByte; b[5] := io.ReadByte; b[4] := io.ReadByte;
            b[3] := io.ReadByte; b[2] := io.ReadByte; b[1] := io.ReadByte; b[0] := io.ReadByte;
            DecodedSize := Int64(b);
          end;
        else
          DecodedSize := l - 128;
      end;
      Mask[0] := io.ReadByte; Mask[1] := io.ReadByte; Mask[2] := io.ReadByte; Mask[3] := io.ReadByte;

      if DecodedSize < 1 then
      begin
        Result := [];
        exit;
      end;

      SetLength(Result, DecodedSize);
      io.ReadBytes(TIdBytes(Result), DecodedSize, False);
      for i := 0 to DecodedSize - 1 do
        Result[i] := Result[i] xor Mask[i mod 4];
    end;
  except
  end;
end;

procedure TLccWebSocketListener.SendContextDataAsString(AContext: TIdContext; AString: string);
begin
  SendContextDataAsBytes(AContext, IndyTextEncoding_UTF8.GetBytes( UnicodeString( AString)));
end;

procedure TLccWebSocketListener.SendContextDataAsBytes(AContext: TIdContext; ABytes: TIdBytes);
var
  Msg: TIdBytes;
begin
  // https://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side

  // Add the header to the data

  // No mask from Server to Client
  if Length(ABytes) <= 125 then
  begin
    SetLength(Msg, 2);
    Msg[0] := $81;
    Msg[1] := Length(ABytes)
  end else if (Length(ABytes) >= 126) and (Length(ABytes) <= 65535) then
  begin
    SetLength(Msg, 4);
    Msg[0] := $81;
    Msg[1] := 126;
    Msg[2] := (Length(ABytes) shr 8) and 255;
    Msg[3] := Length(ABytes) and 255;
  end else
  begin
    SetLength(Msg, 10);
    Msg[0] := $81;
    Msg[1] := 127;
    Msg[2] := (int64(Length(ABytes)) shr 56) and 255;
    Msg[3] := (int64(Length(ABytes)) shr 48) and 255;
    Msg[4] := (int64(Length(ABytes)) shr 40) and 255;
    Msg[5] := (int64(Length(ABytes)) shr 32) and 255;
    Msg[6] := (Length(ABytes) shr 24) and 255;
    Msg[7] := (Length(ABytes) shr 16) and 255;
    Msg[8] := (Length(ABytes) shr 8) and 255;
    Msg[9] :=  Length(ABytes) and 255
  end;

  Msg := Msg + ABytes;     // Works because we are in Delphi mode in this unit

  try
    inherited SendContextDataAsBytes(AContext, Msg);
  except
  end;
end;

procedure TLccWebSocketListener.IdTCPServerExecute(AContext: TIdContext);
var
  Bytes: TIdBytes;
  msg, SecWebSocketKey, Hash: string;
  ParsedHeaders: TDictionary<string, string>;
  ConnectionContext: TLccConnectionWebsocketContext;
begin

  Bytes := [];

  ConnectionContext := ConnectionContextList.AddContext(AContext) as TLccConnectionWebsocketContext;
  if not ConnectionContext.Upgraded then
  begin
    if not AContext.Connection.IOHandler.InputBufferIsEmpty then
    begin
      // Read string and parse HTTP headers
      try
        AContext.Connection.IOHandler.InputBuffer.ExtractToBytes(Bytes);
        msg := string( IndyTextEncoding_UTF8.GetString(Bytes));
      except
      end;

      ParsedHeaders := ParseHeader(msg);

      if ParsedHeaders.ContainsKey('Upgrade') and (ParsedHeaders['Upgrade'] = 'websocket') and
        ParsedHeaders.ContainsKey('Sec-WebSocket-Key') then
      begin
        // Handle handshake request
        // https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers

        SecWebSocketKey := ParsedHeaders['Sec-WebSocket-Key'];

        // Send handshake response
        Hash := TIdEncoderMIME.EncodeBytes(
          HashSHA1.HashString(SecWebSocketKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));

        try
          AContext.Connection.IOHandler.Write('HTTP/1.1 101 Switching Protocols'#13#10
            + 'Upgrade: websocket'#13#10
            + 'Connection: Upgrade'#13#10
            + 'Sec-WebSocket-Accept: ' + Hash
            + #13#10#13#10, IndyTextEncoding_UTF8);
        except
        end;

        // Mark IOHandler as handshaked
        ConnectionContext.Upgraded := True;;
      end;

      ParsedHeaders.Free;
    end;
  end else
    inherited IdTCPServerExecute(AContext);
end;

{ TLccWebSocketServer }

function TLccWebSocketServer.CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener;
begin
  Result := TLccWebSocketListener.Create(True, Self, AConnectionInfo);
  Result.ConnectionContextList.DefaultContextClass := TLccConnectionWebsocketContext;
end;

procedure TLccWebSocketServer.InitSSL(AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL);
var
  CurrentActive: boolean;
begin
  CurrentActive := (ListenerThread as TLccWebSocketListener).IdTCPServer.Active;
  if CurrentActive then
    (ListenerThread as TLccWebSocketListener).IdTCPServer.Active := false;

  (ListenerThread as TLccWebSocketListener).IdServerIOHandlerSSLOpenSSL := AIdServerIOHandlerSSLOpenSSL;
  (ListenerThread as TLccWebSocketListener).IdTCPServer.IOHandler := AIdServerIOHandlerSSLOpenSSL;

  if CurrentActive then
    (ListenerThread as TLccWebSocketListener).IdTCPServer.Active := true;
end;

{ TLccWebSocketListener }


{ TLccContextsList }

function TLccContextsList.GetGridConnectContext(Index: Integer): TLccConnectionContext;
var
  List: TList;
begin
  List := LockList;
  try
    Result := TLccConnectionContext(List[Index]);
  finally
    UnlockList;
  end;
end;

constructor TLccContextsList.Create(AnOwner: TLccEthernetListener);
begin
  inherited Create;
  FDefaultContextClass := TLccConnectionContext;
  FOwner := AnOwner;
  FWorkerMessage := TLccMessage.Create;
end;

function TLccContextsList.GetCount: Integer;
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

destructor TLccContextsList.Destroy;
begin
  Clear;
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

function TLccContextsList.AddContext(AContext: TIdContext): TLccConnectionContext;
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
      if TLccConnectionContext(List[i]).Context = AContext then
      begin
        Found := True;
        Result := TLccConnectionContext(List[i]);
        Break;
      end;
    end;
    if not Found then
    begin
      Result := DefaultContextClass.Create(AContext);
      List.Add(Result);
    end;
  finally
    UnlockList;
  end;
end;

procedure TLccContextsList.Clear;
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

function TLccContextsList.FindContext(AContext: TIdContext): TLccConnectionContext;
var
  List: TList;
  i: Integer;
begin
  Result := nil;
  List := LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      if TLccConnectionContext(List[i]).Context = AContext then
      begin
        Result := TLccConnectionContext(List[i]);
        Break;
      end;
    end;
  finally
    UnlockList;
  end;

end;

function TLccContextsList.RemoveContext(AContext: TIdContext): Boolean;
var
  List: TList;
  i: Integer;
begin
  Result := False;
  List := LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      if TLccConnectionContext(List[i]).Context = AContext then
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

procedure TLccContextsList.AddGridConnectStringByContext(AContext: TIdContext; AString: string; NodeManager: TLccNodeManager);
var
  ContextList:  TList;
  iContext, iString: Integer;
  ServerContext: TLccConnectionContext;
  GridConnectStrPtr: PGridConnectString;
  MessageStr: String;
begin
  ContextList := LockList;
  try
    // Contexts are adding/removed in IdTCPServerConnect/IdTCPServerDisconnectConnect
    for iContext := 0 to ContextList.Count - 1 do
    begin
      ServerContext := TLccConnectionContext(ContextList[iContext]);
      if ServerContext.Context = AContext then
      begin
        for iString := 1 to Length(AString) do
        begin
          // Take the incoming characters and try to make a valid gridconnect message
          GridConnectStrPtr := nil;
          if ServerContext.GridConnectHelper.GridConnect_DecodeMachine(Ord(AString[iString]), GridConnectStrPtr) then
          begin
            // Have a valid gridconnect message
            MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
            WorkerMessage.LoadByGridConnectStr(MessageStr);

            // Message may only be part of a larger string of messages to make up a full LCC message.
            // This call will concatinate these partial Lcc message and return with a fully qualified
            // Lcc message.
            case ServerContext.GridConnectMessageAssembler.IncomingMessageGridConnect(WorkerMessage) of
              imgcr_True :
                begin
                  NodeManager.ReceiveMessageServerThread.ReceiveMessageServerAddMessage(WorkerMessage);
                  try
                    if not Owner.Terminated then
                      Owner.Synchronize(Owner.ReceiveMessage);  // WorkerMessage contains the message
                  except
                  end;
                end;
              imgcr_ErrorToSend :
                begin
         //         ConnectionInfo.LccMessage.CopyToTarget(WorkerMessage);
         //         if not Terminated then
         //           Synchronize({$IFDEF LCC_FPC}@{$ENDIF}RequestErrorMessageSent);
                end;
              imgcr_False,
              imgcr_UnknownError :
                begin

                end;
            end;
          end;
        end;

        Break;
      end;
    end;
  finally
    UnlockList;
  end;
end;

procedure TLccContextsList.AddTcpDataByContext(AContext: TIdContext; ADataArray: TIdBytes; NodeManager: TLccNodeManager);
var
  ContextList:  TList;
  iContext, iDataArray: Integer;
  ServerContext: TLccConnectionContext;
  LocalDataArray: TLccDynamicByteArray;
begin
  LocalDataArray := nil;
  ContextList := LockList;
  try
    // Contexts are adding/removed in IdTCPServerConnect/IdTCPServerDisconnectConnect
    for iContext := 0 to ContextList.Count - 1 do
    begin
      ServerContext := TLccConnectionContext(ContextList[iContext]);
      if ServerContext.Context = AContext then
      begin
        for iDataArray := 0 to Length(ADataArray) - 1 do
        begin
          if ServerContext.TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(ADataArray[iDataArray], LocalDataArray) then
          begin
            if WorkerMessage.LoadByLccTcp(LocalDataArray) then
            begin
              NodeManager.ReceiveMessageServerThread.ReceiveMessageServerAddMessage(WorkerMessage);
              try
                if not Owner.Terminated then
                  Owner.Synchronize(Owner.ReceiveMessage);
              except
              end;
            end
          end;
        end;
        Break;
      end;
    end;
  finally
    UnlockList;
  end;
end;

{ TLccConnectionContext }

constructor TLccConnectionContext.Create(AContext: TIdContext);
begin
  FContext := AContext;
  FStringList := TStringList.Create;
  FCircularArray := TCircularArray.Create;
  FGridConnectHelper := TGridConnectHelper.Create;
  FGridConnectMessageAssembler := TLccGridConnectMessageAssembler.Create;
  FTcpDecodeStateMachine := TOPStackcoreTcpDecodeStateMachine.Create;
end;

destructor TLccConnectionContext.Destroy;
begin
  inherited;
  FreeAndNil(FStringList);
  FreeAndNil(FCircularArray);
  FreeAndNil(FGridConnectHelper);
  FreeAndNil(FGridConnectMessageAssembler);
  FreeAndNil(FTcpDecodeStateMachine);
end;

{ TLccEthernetListener }

procedure TLccEthernetListener.Execute;
// Receiving messages are handled in the IdTCPServerExecute handler and dispatched directly to each LccNode
var
  i: Integer;
  ContextList: TList;
  TxStr: string;
  TxList: TStringList;
  DynamicByteArray: TLccDynamicByteArray;
  IdSocketHandle: TIdSocketHandle;
begin
  FRunning := True;
  Connecting := True;
  try
    try
      try
        IdTCPServer.Active          := False;
        IdTCPServer.MaxConnections  := 255;
          // ... assign a new context class (if you need)
       // IdTCPServer.ContextClass    := TYourContext;

        // add some callback functions
        IdTCPServer.OnConnect := IdTCPServerConnect;
        IdTCPServer.OnDisconnect := IdTCPServerDisconnect;
        IdTCPServer.OnExecute := IdTCPServerExecute;
        IdTCPServer.OnStatus := IdTCPServerStatus;
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
        IdSocketHandle := IdTCPServer.Bindings.Add;
        IdSocketHandle.Port := (ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort;
        IdSocketHandle.IP := (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP;

        IdTCPServer.Active := True;

        if IdTCPServer.Active then
        begin
          Connecting := False;

          HandleSendConnectionNotification(lcsConnected);
          while not Terminated do
          begin
            // Sending out what need to be sent to the connections
            ContextList := IdTCPServer.Contexts.LockList;
            try
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
                  for i := 0 to IdTCPServer.Contexts.Count - 1 do
                  begin
                    if TIdContext( ContextList[i]).Connection.Connected then
                      TIdContext( ContextList[i]).Connection.IOHandler.WriteLn(TxStr);
                  end;
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
                  for i := 0 to IdTCPServer.Contexts.Count - 1 do
                  begin
                    if TIdContext( ContextList[i]).Connection.Connected then
                      TIdContext( ContextList[i]).Connection.IOHandler.Write(TIdBytes( DynamicByteArray), Length(DynamicByteArray));
                  end;
                end;
              end;
            finally
              IdTCPServer.Contexts.UnlockList;
            end;

            IndySleep(THREAD_SLEEP_TIME);
          end
        end;
      finally
        Connecting := False;
        IdTCPServer.Active := False;
      end;
    except  // idTCPServer uses exceptions to throw faults, trap them so the users does not see them
      on E: EIdException do
      begin
        Connecting := False;
        ConnectionInfo.ErrorMessage := E.Message;
        ErrorOnExit := True;
      end;
    end;
  finally
    FRunning := False;
  end;
end;

constructor TLccEthernetListener.Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited Create(CreateSuspended, AnOwner, AConnectionInfo);
  IdTCPServer := TIdTCPServer.Create(nil);
  FConnectionContextList := TLccContextsList.Create(Self);
  FreeOnTerminate := False;
end;

destructor TLccEthernetListener.Destroy;
begin

  IdTCPServer.Free;
  ConnectionContextList.Free;
  inherited Destroy;
end;

function TLccEthernetListener.ReceiveContextDataAsString(AContext: TIdContext): string;
var
  AChar: AnsiChar;
begin
  Result := '';
  while not AContext.Connection.IOHandler.InputBufferIsEmpty and AContext.Connection.IOHandler.Connected do
  begin
    AChar := AnsiChar(AContext.Connection.IOHandler.ReadByte);
    Result := Result + string(AChar);
  end;
end;

function TLccEthernetListener.ReceiveContextDataAsBytes(AContext: TIdContext): TIdBytes;
begin
  Result := nil;
  while not AContext.Connection.IOHandler.InputBufferIsEmpty and AContext.Connection.IOHandler.Connected do
  begin
    // Slow but simple
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result)-1] := AContext.Connection.IOHandler.ReadByte;
  end;
end;

procedure TLccEthernetListener.SendContextDataAsString(AContext: TIdContext; AString: string);
var
  iByte: Integer;
begin
  for iByte := 1 to Length(AString) do
    AContext.Connection.IOHandler.Write(Ord( AString[iByte]));
end;

procedure TLccEthernetListener.SendContextDataAsBytes(AContext: TIdContext; ABytes: TIdBytes);
var
  iByte: Integer;
begin
  for iByte := 0 to Length(ABytes) - 1 do
    AContext.Connection.IOHandler.Write(ABytes[iByte]);
end;

procedure TLccEthernetListener.IdTCPServerConnect(AContext: TIdContext);
begin
  ConnectionContextList.AddContext(AContext);

  (ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := AContext.Binding.PeerIP;
  (ConnectionInfo as TLccEthernetConnectionInfo).ClientPort := AContext.Binding.PeerPort;
  if IdTCPServer.Bindings.Count > 0 then
  begin
    (ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort := IdTCPServer.Bindings[0].Port;
    (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := IdTCPServer.Bindings[0].IP;
  end;

  if Terminated then Exit;
  HandleSendConnectionNotification(lcsClientConnected);
end;

procedure TLccEthernetListener.IdTCPServerDisconnect(AContext: TIdContext);
begin
  ConnectionContextList.RemoveContext(AContext);

  (ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := AContext.Binding.PeerIP;
  (ConnectionInfo as TLccEthernetConnectionInfo).ClientPort := AContext.Binding.PeerPort;
  if IdTCPServer.Bindings.Count > 0 then
  begin
    (ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort := IdTCPServer.Bindings[0].Port;
    (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := IdTCPServer.Bindings[0].IP;
  end;

  if Terminated then Exit;
  HandleSendConnectionNotification(lcsClientDisconnected);
end;

procedure TLccEthernetListener.IdTCPServerExecute(AContext: TIdContext);
var
  AString: string;
  List: TList;
  iContext: Integer;
  OtherContext: TIdContext;
//  AByte: Byte;
  TcpMessage: TIdBytes;
begin

  // Messages sent to the Listener from all the Connections (Contexts)

  if (ConnectionInfo as TLccEthernetConnectionInfo).GridConnect then
  begin
  // Well well this just got more complicated... Need to have a separate buffer for each Context...

    AString := ReceiveContextDataAsString(AContext);

    // Put the string in the correct context (each context is a connection to the server so keep the messages sorted per context)... do I need to do this?  Unsure.
    // the main Execute method will pick this up and pass them along... another reason is to allow this thread to be a hub and relay the messages from one context
    // to another...
    if AString <> '' then
    begin
      ConnectionContextList.AddGridConnectStringByContext(AContext, AString, Owner.NodeManager);

   //   Owner.NodeManager.HardwarewareConnectionList.Lock;
   //   try
   //     for i := 0 to List.Count - 1 do
    //    begin
    //      (Owner.NodeManager.HardwarewareConnectionList.Items[i] as IHardwareConnectionManagerLink).SendMessage(nil);
    //    end;
    //  finally
    //     Owner.NodeManager.HardwarewareConnectionList.Unlock;
   //   end;

      if Owner.Hub then
      begin
        List := IdTCPServer.Contexts.LockList;
        try
          for iContext := 0 to List.Count - 1 do
          begin
            OtherContext := TIdContext(List[iContext]);
            if OtherContext <> AContext then
              if OtherContext.Connection.Connected then
                SendContextDataAsString(OtherContext, AString);
          end;
        finally
          IdTCPServer.Contexts.UnlockList;
        end;
      end;
    end;
  end else
  begin
    TcpMessage := ReceiveContextDataAsBytes(AContext);

    if Length(TcpMessage) > 0 then
    begin
      ConnectionContextList.AddTcpDataByContext(AContext, TcpMessage, Owner.NodeManager);

      if Owner.Hub then
      begin
        List := IdTCPServer.Contexts.LockList;
        try
          for iContext := 0 to List.Count - 1 do
          begin
            OtherContext := TIdContext(List[iContext]);
            if OtherContext <> AContext then
              if OtherContext.Connection.Connected then
                SendContextDataAsBytes(OtherContext, TcpMessage);
          end;
        finally
          IdTCPServer.Contexts.UnlockList;
        end;
      end;
    end;
  end;

     // https://stackoverflow.com/questions/64593756/delphi-rio-indy-tcpserver-high-cpu-usage
    // There is another way to do this but with this simple program this is fine
  IndySleep(THREAD_SLEEP_TIME);
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

function TLccEthernetListener.ParseHeader(const msg: string): TDictionary<string, string>;
{$IFDEF LCC_FPC}
var
  lines, SplittedLine: TStringArray;
  line: string;
begin
  Result := TDictionary<string, string>.Create;
  lines := SplitString(msg, #13#10);
  for line in lines do
  begin
    SplittedLine := SplitString(line, ': ');
    if Length(SplittedLine) > 1 then
      Result.AddOrSetValue(Trim(SplittedLine[0]), Trim(SplittedLine[1]));
  end;
end;
{$ELSE}
var
  lines: TArray<string>;
  line: string;
  SplittedLine: TArray<string>;
begin
  result := TDictionary<string, string>.Create;
  lines := msg.Split([#13#10]);
  for line in lines do
  begin
    SplittedLine := line.Split([': ']);
    if Length(SplittedLine) > 1 then
      result.AddOrSetValue(Trim(SplittedLine[0]), Trim(SplittedLine[1]));
  end;
end;

{$ENDIF}

procedure TLccEthernetListener.ReceiveMessage;
begin
  (Owner as TLccEthernetServer).DoReceiveMessage(ConnectionContextList.WorkerMessage);
end;

procedure TLccEthernetListener.SetConnecting(AValue: Boolean);
begin
  Owner.CriticalSectionEnter;
  try
    inherited SetConnecting(AValue);
  finally
    Owner.CriticalSectionLeave;
  end;
end;

{ TLccEthernetServer }

procedure TLccEthernetServer.CloseConnection;
var
  TimeCount: Integer;
begin
  NodeManager.Clear;
  inherited CloseConnection;
  if Assigned(ListenerThread) then
  begin
    try
      TimeCount := 0;
      ListenerThread.HandleSendConnectionNotification(lcsDisconnecting, False);
      ListenerThread.Terminate;
      while ListenerThread.Running do
      begin
        Inc(TimeCount);
        Sleep(50);
        if TimeCount = 100 then
        begin
          Break // Something went really wrong
        end;
      end;
      if ListenerThread.ErrorOnExit then
        ListenerThread.ErrorMessage;
      ListenerThread.HandleSendConnectionNotification(lcsDisconnected, False);
    finally
      FreeAndNil(FListenerThread);
    end
  end;
end;

function TLccEthernetServer.IsLccLink: Boolean;
begin
  Result := True;
end;

function TLccEthernetServer.GetConnected: Boolean;
begin
  Result := False;
  CriticalSectionEnter;
  try
    if Assigned(ListenerThread) and Assigned(ListenerThread.IdTCPServer) then
      Result := ListenerThread.IdTCPServer.Active;
  finally
    CriticalSectionLeave;
  end;
end;

function TLccEthernetServer.GetConnecting: Boolean;
begin
  Result := False;
  CriticalSectionEnter;
  try
    if Assigned(ListenerThread) then
      Result := ListenerThread.Connecting;
  finally
    CriticalSectionLeave;
  end;
end;

procedure TLccEthernetServer.DoReceiveMessage(LccMessage: TLccMessage);
begin
  inherited DoReceiveMessage(LccMessage);
end;

procedure TLccEthernetServer.SendMessage(ALccMessage: TLccMessage);
begin
  CriticalSectionEnter;
  try
    if Assigned(ListenerThread) then
      ListenerThread.AddToOutgoingBuffer(ALccMessage);
  finally
  end;
    CriticalSectionLeave;
  inherited SendMessage(ALccMessage);
end;

function TLccEthernetServer.OpenConnection(AConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  CloseConnection;
  inherited OpenConnection(AConnectionInfo);
  Result := CreateListenerObject(AConnectionInfo.Clone as TLccEthernetConnectionInfo);
  ListenerThread := Result as TLccEthernetListener;
  ListenerThread.Owner := Self;
  ListenerThread.Suspended := False;
end;

function TLccEthernetServer.CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener;
begin
  Result := TLccEthernetListener.Create(True, Self, AConnectionInfo);
end;

end.

