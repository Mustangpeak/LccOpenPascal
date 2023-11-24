unit lcc_ethernet_server;

{$I ../../lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode delphi}{$H+}     // Needed to deal with generics for the WebSocket code and Dictionary
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF LCC_FPC}
    LazLogger,
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
  lcc_defines,
  lcc_node_messages_can_assembler_disassembler,
  lcc_connection_common,
  lcc_node_messages,
  lcc_ethernet_common,
  lcc_gridconnect,
  lcc_ethernet_tcp,
  lcc_node,
  lcc_utilities,
  lcc_alias_server_thread;

type

  TLccEthernetServerThread = class;
  TLccConnectionContextList = class;
  TLccEthernetServerThreadManager = class; // forward

  // One of these days need to figure out how to create a custom Indy context class. Now
  // Indy taking the context threads, serializes them to the one method in the Listener
  // and I then have to unserialize them and create structures keeping track of each context
  // separatly.  Lot of wasted effort

  { TLccConnectionContext }

  TLccConnectionContext = class
  private
    FContext: TIdContext;
    FGridConnectDecodeStateMachine: TGridConnectDecodeStateMachine;
    FGridConnectMessageAssembler: TLccGridConnectMessageAssembler;
    FOwnerConnectionContextList: TLccConnectionContextList;
    FTcpDecodeStateMachine: TTcpDecodeStateMachine;
    FWorkerMessage: TLccMessage;
    FWorkerStream: TMemoryStream;
  protected
    // Helpers
    property GridConnectDecodeStateMachine: TGridConnectDecodeStateMachine read FGridConnectDecodeStateMachine;
    property GridConnectMessageAssembler: TLccGridConnectMessageAssembler read FGridConnectMessageAssembler write FGridConnectMessageAssembler;
    property TcpDecodeStateMachine: TTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property WorkerStream: TMemoryStream read FWorkerStream write FWorkerStream;

    procedure ExtractWorkerStreamAsGridConnect;
    procedure ExtractWorkerStreamAsTCP;
  public
    property Context: TIdContext read FContext write FContext;
    property OwnerConnectionContextList: TLccConnectionContextList read FOwnerConnectionContextList;

    constructor Create(AnOwner: TLccConnectionContextList; AContext: TIdContext); virtual;
    destructor Destroy; override;

    procedure IncomingRawData(var AStream: TMemoryStream); virtual;
  end;

  TLccConnectionContextClass = class of TLccConnectionContext;

  { TLccConnectionContextList }

  TLccConnectionContextList = class(Classes.TThreadList)
  private
    FDefaultContextClass: TLccConnectionContextClass;    // What ConnectionContext type to create for the list
    FOwnerConnectionThread: TLccEthernetServerThread;      // What thread owns us

  public
    property OwnerConnectionThread: TLccEthernetServerThread read FOwnerConnectionThread;
    property DefaultContextClass: TLccConnectionContextClass read FDefaultContextClass write FDefaultContextClass;

    constructor Create(AnOwner: TLccEthernetServerThread);
    destructor Destroy; override;
    function ContextAdd(AContext: TIdContext): TLccConnectionContext;
    function ContextRemove(AContext: TIdContext): Boolean;
    procedure IncomingRawDataForContext(AContext: TIdContext; var AStream: TMemoryStream);
  end;


  { TLccEthernetServerThread }

  TLccEthernetServerThread = class(TLccConnectionThread)
  private
    FConnectionContextList: TLccConnectionContextList;
    FIdTCPServer: TIdTCPServer;
  protected
    property IdTCPServer: TIdTCPServer read FIdTCPServer write FIdTCPServer;
    property ConnectionContextList: TLccConnectionContextList read FConnectionContextList write FConnectionContextList;

    procedure IdTCPServerConnect(AContext: TIdContext); virtual;
    procedure IdTCPServerDisconnect(AContext: TIdContext); virtual;
    procedure IdTCPServerExecute(AContext: TIdContext); virtual;
    procedure Execute; override;
    procedure RelayToOtherConnections(ASourceContext: TIdContext; AStream: TStream);
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager); override;
    destructor Destroy; override;
  end;

  { TLccEthernetServerThreadManager }

  TLccEthernetServerThreadManager = class(TLccConnectionThreadManager)
  private
    FServerListener: TLccEthernetServerThread;
    { Private declarations }
  protected
    { Protected declarations }
    function CreateListenerObject: TLccEthernetServerThread; virtual;
  public
    { Public declarations }
    property ServerListener: TLccEthernetServerThread read FServerListener write FServerListener;

    function OpenConnection: TLccConnectionThread; override;
    procedure CloseConnection;  override;
  end;


  { TLccWebsocketConnectionContext }

  TLccWebsocketConnectionContext = class(TLccConnectionContext)
  private
    FHashSHA1: TIdHashSHA1;
    FIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL;
    FReceiveStreamWithWSHeader: TMemoryStream;
    FUpgraded: Boolean;
  protected
    property IdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL read FIdServerIOHandlerSSLOpenSSL write FIdServerIOHandlerSSLOpenSSL;
    property HashSHA1: TIdHashSHA1 read FHashSHA1 write FHashSHA1;
    property ReceiveStreamWithWSHeader: TMemoryStream read FReceiveStreamWithWSHeader write FReceiveStreamWithWSHeader;

    function ParseHeader(const msg: string): TDictionary<string, string>;
  public
    property Upgraded: Boolean read FUpgraded write FUpgraded;

    constructor Create(AnOwner: TLccConnectionContextList; AContext: TIdContext); override;
    destructor Destroy; override;

    procedure IncomingRawData(var AStream: TMemoryStream); override;
  end;

  TLccWebsocketConnectionContextClass = class of TLccWebsocketConnectionContext;


  { TLccWebSocketServerThread }

  TLccWebSocketServerThread = class(TLccEthernetServerThread)
  private
    FSendMessageWithoutWSHeader: TMemoryStream;
  protected
    property SendMessageWithoutWSHeader: TMemoryStream read FSendMessageWithoutWSHeader write FSendMessageWithoutWSHeader;

    procedure IdTCPServerExecute(AContext: TIdContext); override;

    function LoadStreamFromMessageBuffer(AStream: TStream; ALccMessageList: Classes.TThreadList; ClearBuffer: Boolean = True): Boolean; override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager); override;
    destructor Destroy; override;
  end;


  { TLccWebSocketServerThreadManager }

  TLccWebSocketServerThreadManager = class(TLccEthernetServerThreadManager)
  protected
    function CreateListenerObject: TLccEthernetServerThread; override;
  public
    procedure InitSSL(AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL);
  end;
  TLccWebSocketServerThreadManagerClass = class of TLccWebSocketServerThreadManager;


implementation

{ TLccWebsocketConnectionContext }

constructor TLccWebsocketConnectionContext.Create(AnOwner: TLccConnectionContextList; AContext: TIdContext);
begin
  inherited Create(AnOwner, AContext);
  HashSHA1 := TIdHashSHA1.Create;
  IdServerIOHandlerSSLOpenSSL := nil;
  ReceiveStreamWithWSHeader := TMemoryStream.Create;
end;

destructor TLccWebsocketConnectionContext.Destroy;
begin
  FreeAndNil(FHashSHA1);
  FreeAndNil(FReceiveStreamWithWSHeader);
  inherited Destroy;
end;

{$IFDEF LCC_FPC}
function TLccWebsocketConnectionContext.ParseHeader(const msg: string): TDictionary<string, string>;
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
{$ENDIF}

{$IFNDEF LCC_FPC}
function TLccWebsocketConnectionContext.ParseHeader(const msg: string): TDictionary<string, string>;
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

procedure TLccWebsocketConnectionContext.IncomingRawData(var AStream: TMemoryStream);
var
  // HTTP Upgrade
  Bytes: TIdBytes;
  msg, SecWebSocketKey, Hash: string;
  ParsedHeaders: TDictionary<string, string>;

  // Normal Payload
  i: Int64;
  l, StartByte: byte;
  b: array [0..7] of byte;
  PayloadSize: int64; // Size of the incoming Data - the Header size which is variable depending on the length of the payload
  Mask: array [0..3] of byte;
begin

  // https://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side

  AStream := nil;

  Bytes := [];

  if not Upgraded then
  begin
    Context.Connection.IOHandler.CheckForDataOnSource(1);
    if not Context.Connection.IOHandler.InputBufferIsEmpty then
    begin
      // Read string and parse HTTP headers
      try
        Context.Connection.IOHandler.InputBuffer.ExtractToBytes(Bytes);
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
          Context.Connection.IOHandler.Write('HTTP/1.1 101 Switching Protocols'#13#10
            + 'Upgrade: websocket'#13#10
            + 'Connection: Upgrade'#13#10
            + 'Sec-WebSocket-Accept: ' + Hash
            + #13#10#13#10, IndyTextEncoding_UTF8);
        except
        end;

        // Mark IOHandler as handshaked
        Upgraded := True;;
      end;

      ParsedHeaders.Free;
    end;
  end else
  begin
    Context.Connection.IOHandler.CheckForDataOnSource(1);
    if not Context.Connection.IOHandler.InputBufferIsEmpty then
    begin
      AStream := WorkerStream;

      ReceiveStreamWithWSHeader.Position := 0;
      ReceiveStreamWithWSHeader.Size := 0;
      Context.Connection.IOHandler.ReadStream(ReceiveStreamWithWSHeader, Context.Connection.IOHandler.InputBuffer.Size);

      // Need to strip the WebSocket Header info off here so we can send just the GridConnect Data to the other Contexts, can't use inherited

      // https://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side

      ReceiveStreamWithWSHeader.Position := 0;

      while ReceiveStreamWithWSHeader.Position < ReceiveStreamWithWSHeader.Size do
      begin
        // Incase of white space or junk at the beginning of the frame
        StartByte := StreamReadByte(ReceiveStreamWithWSHeader);     // Read the WebSocket header from the data
        while StartByte <> $81 do
        begin
          StartByte := StreamReadByte(ReceiveStreamWithWSHeader);
          if ReceiveStreamWithWSHeader.Position >= ReceiveStreamWithWSHeader.Size then
            Exit;
        end;

        l := StreamReadByte(ReceiveStreamWithWSHeader);
        case l of
          $FE:
            begin
              b[1] := StreamReadByte(ReceiveStreamWithWSHeader);
              b[0] := StreamReadByte(ReceiveStreamWithWSHeader);
              b[2] := 0;
              b[3] := 0;
              b[4] := 0;
              b[5] := 0;
              b[6] := 0;
              b[7] := 0;
              PayloadSize := Int64(b);
            end;
          $FF:
            begin
              b[7] := StreamReadByte(ReceiveStreamWithWSHeader);
              b[6] := StreamReadByte(ReceiveStreamWithWSHeader);
              b[5] := StreamReadByte(ReceiveStreamWithWSHeader);
              b[4] := StreamReadByte(ReceiveStreamWithWSHeader);
              b[3] := StreamReadByte(ReceiveStreamWithWSHeader);
              b[2] := StreamReadByte(ReceiveStreamWithWSHeader);
              b[1] := StreamReadByte(ReceiveStreamWithWSHeader);
              b[0] := StreamReadByte(ReceiveStreamWithWSHeader);
              PayloadSize := Int64(b);
            end;
          else
            PayloadSize := l and $7F;   // Strip the top bit, always one
        end;
        Mask[0] := StreamReadByte(ReceiveStreamWithWSHeader);
        Mask[1] := StreamReadByte(ReceiveStreamWithWSHeader);
        Mask[2] := StreamReadByte(ReceiveStreamWithWSHeader);
        Mask[3] := StreamReadByte(ReceiveStreamWithWSHeader);

        if PayloadSize > 0 then
        begin
          WorkerStream.Position := 0;
          WorkerStream.Size := 0;

          // Client To Server has the data Masked
          for i := 0 to PayloadSize - 1 do
            StreamWriteByte(WorkerStream, StreamReadByte(ReceiveStreamWithWSHeader) xor Mask[i mod 4]);

          if OwnerConnectionContextList.OwnerConnectionThread.OwnerConnectionManager.EmulateCanBus then
            ExtractWorkerStreamAsGridConnect
          else
            ExtractWorkerStreamAsTCP;
        end;
      end;
    end;
  end;
end;

{ TLccWebSocketServerThread }

constructor TLccWebSocketServerThread.Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager);
begin
  inherited Create(CreateSuspended, AnOwner);
  SendMessageWithoutWSHeader := TMemoryStream.Create;
end;

destructor TLccWebSocketServerThread.Destroy;
begin
  FreeAndNil(FSendMessageWithoutWSHeader);
  inherited Destroy;
end;

procedure TLccWebSocketServerThread.IdTCPServerExecute(AContext: TIdContext);
begin
  // Add a new context if needed
 // ConnectionContext := ConnectionContextList.ContextAdd(AContext) as TLccWebsocketConnectionContext;

  inherited IdTCPServerExecute(AContext);
end;

function TLccWebSocketServerThread.LoadStreamFromMessageBuffer(AStream: TStream; ALccMessageList: Classes.TThreadList;
  ClearBuffer: Boolean = True): Boolean;
var
  i, PayloadLen: Int64;
begin
  // https://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side

  Result := inherited LoadStreamFromMessageBuffer(SendMessageWithoutWSHeader, ALccMessageList, ClearBuffer);
  PayloadLen := SendMessageWithoutWSHeader.Size;

  if Result and (PayloadLen > 0) then
  begin
    AStream.Position:= 0;
    AStream.Size := 0;
    // Write in the WebSocket Header to the real SendBuffer
    if PayloadLen <= 125 then
    begin
      StreamWriteByte(AStream, $81);
      StreamWriteByte(AStream, PayloadLen);
    end else if (PayloadLen >= 126) and (PayloadLen <= 65535) then
    begin
      StreamWriteByte(AStream, $81);
      StreamWriteByte(AStream, 126);
      StreamWriteByte(AStream, (PayloadLen shr 8) and 255);
      StreamWriteByte(AStream, PayloadLen and 255);
    end else
    begin
      StreamWriteByte(AStream, $81);
      StreamWriteByte(AStream, 127);
      StreamWriteByte(AStream, (int64(PayloadLen) shr 56) and 255);
      StreamWriteByte(AStream, (int64(PayloadLen) shr 48) and 255);
      StreamWriteByte(AStream, (int64(PayloadLen) shr 40) and 255);
      StreamWriteByte(AStream, (int64(PayloadLen) shr 32) and 255);
      StreamWriteByte(AStream, (PayloadLen shr 24) and 255);
      StreamWriteByte(AStream, (PayloadLen shr 16) and 255);
      StreamWriteByte(AStream, (PayloadLen shr 8) and 255);
      StreamWriteByte(AStream, PayloadLen and 255);
    end;

    // No mask from Server to Client
    // Now append the real payload to the header
    SendMessageWithoutWSHeader.Position := 0;
    for i := 0 to SendMessageWithoutWSHeader.Size - 1 do
      StreamWriteByte(AStream, StreamReadByte(SendMessageWithoutWSHeader));
  end;
end;

{ TLccWebSocketServerThreadManager }

function TLccWebSocketServerThreadManager.CreateListenerObject: TLccEthernetServerThread;
begin
  Result := TLccWebSocketServerThread.Create(True, Self);
  Result.ConnectionContextList.DefaultContextClass := TLccWebsocketConnectionContext;
end;

procedure TLccWebSocketServerThreadManager.InitSSL(AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL);
var
  CurrentActive: boolean;
  iContext: Integer;
  List: TList;

begin
  CurrentActive := (ServerListener as TLccWebSocketServerThread).IdTCPServer.Active;
  if CurrentActive then
    (ServerListener as TLccWebSocketServerThread).IdTCPServer.Active := false;

  List := ServerListener.ConnectionContextList.LockList;
  try
    for iContext := 0 to List.Count - 1 do
      TLccWebsocketConnectionContext(List[iContext]).IdServerIOHandlerSSLOpenSSL := AIdServerIOHandlerSSLOpenSSL;
    (ServerListener as TLccWebSocketServerThread).IdTCPServer.IOHandler := AIdServerIOHandlerSSLOpenSSL;
  finally
    ServerListener.ConnectionContextList.UnlockList;
  end;

  if CurrentActive then
    (ServerListener as TLccWebSocketServerThread).IdTCPServer.Active := true;
end;

{ TLccWebSocketServerThread }


{ TLccConnectionContextList }

constructor TLccConnectionContextList.Create(AnOwner: TLccEthernetServerThread);
begin
  inherited Create;
  FDefaultContextClass := TLccConnectionContext;
  FOwnerConnectionThread := AnOwner;
end;

destructor TLccConnectionContextList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TLccConnectionContextList.ContextAdd(AContext: TIdContext): TLccConnectionContext;
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
      Result := DefaultContextClass.Create(Self, AContext);
      List.Add(Result);
    end;
  finally
    UnlockList;
  end;
end;

function TLccConnectionContextList.ContextRemove(AContext: TIdContext): Boolean;
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
        TLccConnectionContext(List[i]).Free;
        List.Delete(i);
        Result := True;
        Break;
      end;
    end;
  finally
    UnlockList;
  end;
end;

procedure TLccConnectionContextList.IncomingRawDataForContext(
  AContext: TIdContext; var AStream: TMemoryStream);
var
  ContextList:  TList;
  iContext: Integer;
  ConnectionContext: TLccConnectionContext;
begin
  ContextList := LockList;
  try
    // Contexts are added/removed in IdTCPServerConnect/IdTCPServerDisconnectConnect events
    for iContext := 0 to ContextList.Count - 1 do
    begin
      ConnectionContext := TLccConnectionContext(ContextList[iContext]);
      if ConnectionContext.Context = AContext then
      begin
        ConnectionContext.IncomingRawData(AStream);
        Break;
      end;
    end;
  finally
    UnlockList;
  end;
end;

{ TLccConnectionContext }

procedure TLccConnectionContext.ExtractWorkerStreamAsGridConnect;
var
  iData: Integer;
  GridConnectStrPtr: PGridConnectString;
  MessageStr: String;
begin
  WorkerStream.Position := 0;

  for iData := 0 to WorkerStream.Size - 1 do
  begin
    // Take the incoming characters and try to make a valid gridconnect message
    GridConnectStrPtr := nil;

    if GridConnectDecodeStateMachine.GridConnect_DecodeMachine(StreamReadByte(WorkerStream), GridConnectStrPtr) then
    begin     // Have a valid gridconnect message
      MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
      WorkerMessage.LoadByGridConnectStr(MessageStr);
      WorkerMessage.ConnectionThread := OwnerConnectionContextList.OwnerConnectionThread;

      // Not a fan of having it here to block the main connection thread but need to hook into the raw individual messages.
      // after the next call to GridConnectMessageAssembler split up CAN messages will be recombined into a single LCC message
      if Assigned(OwnerConnectionContextList.OwnerConnectionThread.OwnerConnectionManager.OwnerConnectionFactory.OnLccGridConnectStrReceive) then
      begin
        OwnerConnectionContextList.OwnerConnectionThread.OwnerConnectionManager.ReceiveGridConnectStringSyncronize := MessageStr;
        OwnerConnectionContextList.OwnerConnectionThread.Synchronize(OwnerConnectionContextList.OwnerConnectionThread.OwnerConnectionManager.ReceiveGridConnectStrThoughSyncronize);
      end;

      // Message may only be part of a larger string of messages to make up a full LCC message.
      // This call will concatinate these partial Lcc message and return with a fully qualified
      // Lcc message.
      case GridConnectMessageAssembler.IncomingMessageGridConnect(WorkerMessage) of
        imgcr_True         : AliasServerThread.AddIncomingLccMessage(WorkerMessage, True);
        imgcr_ErrorToSend  : begin
                               WorkerMessage.CheckNodeIDsBeforeDelivery := True;  // We just swapped Souce/Dest NodeID, make sure we don't feed the message back into our nodes as a received message with ourthese Alias/NodeID or we will trigger a duplicate Node/Alias
                               WorkerMessage.CopyToTarget( OwnerConnectionContextList.OwnerConnectionThread.OwnerConnectionManager.SendMessageWorkerMessageSyncronize);
                               OwnerConnectionContextList.OwnerConnectionThread.Synchronize(OwnerConnectionContextList.OwnerConnectionThread.OwnerConnectionManager.SendMessageThroughSyncronize);
                             end;
        imgcr_False,
        imgcr_UnknownError : begin end;
      end;
    end;
  end;
end;

procedure TLccConnectionContext.ExtractWorkerStreamAsTCP;
var
  LocalDataArray: TLccDynamicByteArray;
  iData: Integer;
begin
  WorkerStream.Position := 0;

  LocalDataArray := [];
  for iData := 0 to WorkerStream.Size - 1 do
  begin
    if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(StreamReadByte(WorkerStream), LocalDataArray) then
    begin
      if WorkerMessage.LoadByLccTcp(LocalDataArray) then
      begin
        WorkerMessage.ConnectionThread := OwnerConnectionContextList.OwnerConnectionThread;
        AliasServerThread.AddIncomingLccMessage(WorkerMessage, False);
      end;
    end;
  end;
end;

constructor TLccConnectionContext.Create(AnOwner: TLccConnectionContextList; AContext: TIdContext);
begin
  FOwnerConnectionContextList := AnOwner;
  FContext := AContext;
  FGridConnectDecodeStateMachine := TGridConnectDecodeStateMachine.Create;
  FGridConnectMessageAssembler := TLccGridConnectMessageAssembler.Create;
  FTcpDecodeStateMachine := TTcpDecodeStateMachine.Create;
  FWorkerMessage := TLccMessage.Create;
  FWorkerStream := TMemoryStream.Create;
end;

destructor TLccConnectionContext.Destroy;
begin
  inherited;
  FreeAndNil(FGridConnectDecodeStateMachine);
  FreeAndNil(FGridConnectMessageAssembler);
  FreeAndNil(FTcpDecodeStateMachine);
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FWorkerStream);
end;


//The read and write streams seem to be having random issues.....


procedure TLccConnectionContext.IncomingRawData(var AStream: TMemoryStream);
begin
  AStream := nil;
  Context.Connection.IOHandler.CheckForDataOnSource(1);
  if not Context.Connection.IOHandler.InputBufferIsEmpty then
  begin
    AStream := WorkerStream;
    WorkerStream.Position := 0;
    WorkerStream.Size := 0;
    Context.Connection.IOHandler.ReadStream(WorkerStream, Context.Connection.IOHandler.InputBuffer.Size);

    if OwnerConnectionContextList.OwnerConnectionThread.OwnerConnectionManager.EmulateCanBus then
      ExtractWorkerStreamAsGridConnect
    else
      ExtractWorkerStreamAsTCP;
  end;
end;

{ TLccEthernetServerThread }

procedure TLccEthernetServerThread.Execute;
// Receiving messages are handled in the IdTCPServerExecute handler and dispatched directly to each LccNode
var
  i: Integer;
  ContextList: TList;
  IdSocketHandle: TIdSocketHandle;
begin
  Running := True;
  try
    try
      try
        HandleSendConnectionChangeNotify(lcsConnecting, True);

        IdTCPServer.Active          := False;
        IdTCPServer.MaxConnections  := 255;
          // ... assign a new context class (if you need)
       // IdTCPServer.ContextClass    := TYourContext;

        // add some callback functions
        IdTCPServer.OnConnect := IdTCPServerConnect;
        IdTCPServer.OnDisconnect := IdTCPServerDisconnect;
        IdTCPServer.OnExecute := IdTCPServerExecute;
        IdTCPServer.TerminateWaitTime := 2;

        OwnerConnectionManager.CriticalSectionEnter;
        try
          if (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).AutoResolveIP then
          begin
            {$IFDEF LCC_WINDOWS}
            (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := ResolveWindowsIp
            {$ELSE}
            (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := ResolveUnixIp;
            {$ENDIF}
          end;

          IdTCPServer.Bindings.Clear;
          IdSocketHandle := IdTCPServer.Bindings.Add;
          IdSocketHandle.Port := (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort;
          IdSocketHandle.IP := (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP;
        finally
          OwnerConnectionManager.CriticalSectionLeave;
        end;

        IdTCPServer.Active := True;

        if IdTCPServer.Active then
        begin

          HandleSendConnectionChangeNotify(lcsConnected, True);
          while not Terminated do
          begin
            // Sending out what need to be sent to the connections
            ContextList := IdTCPServer.Contexts.LockList;
            try
              // Outside of the Message Buffer (so not to block the main thread sending more messages)
              // dispatch this string to all the connections
              if LoadStreamFromMessageBuffer(SendStreamConnectionThread, SendMessageLccMessageBuffer) then
              begin
                // Decided to make the sendmessage side more centralized vs having buffers
                // in each connection context and have the message convertered N times in
                // each context.  Incoming this makes sense as it has to happen, here
                // it does not.
                for i := 0 to IdTCPServer.Contexts.Count - 1 do
                begin
                  SendStreamConnectionThread.Position := 0;
                  if TIdContext( ContextList[i]).Connection.Connected then
                    TIdContext( ContextList[i]).Connection.IOHandler.Write(SendStreamConnectionThread, SendStreamConnectionThread.Size);
                end;
              end;
            finally
              IdTCPServer.Contexts.UnlockList;
            end;

            IndySleep(THREAD_SLEEP_TIME);
          end
        end;
      finally
        IdTCPServer.Active := False;
      end;
    except  // idTCPServer uses exceptions to throw faults, trap them so the users does not see them
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

procedure TLccEthernetServerThread.RelayToOtherConnections(ASourceContext: TIdContext; AStream: TStream);
var
  List: TList;
  iContext: Integer;
  NextContext: TIdContext;
begin
  List := IdTCPServer.Contexts.LockList;
  try
    for iContext := 0 to List.Count - 1 do
    begin
      NextContext := TIdContext(List[iContext]);
      if NextContext <> ASourceContext then
        if NextContext.Connection.Connected then
        begin
          AStream.Position := 0;
          NextContext.Connection.IOHandler.Write(AStream, AStream.Size)
        end;
    end;
  finally
    IdTCPServer.Contexts.UnlockList;
  end;
end;

constructor TLccEthernetServerThread.Create(CreateSuspended: Boolean;
  AnOwner: TLccConnectionThreadManager);
begin
  inherited Create(CreateSuspended, AnOwner);
  IdTCPServer := TIdTCPServer.Create(nil);
  FConnectionContextList := TLccConnectionContextList.Create(Self);
  FreeOnTerminate := False;
end;

destructor TLccEthernetServerThread.Destroy;
begin
  FreeAndNil(FIdTCPServer);
  FreeAndNil(FConnectionContextList);
  inherited Destroy;
end;

procedure TLccEthernetServerThread.IdTCPServerConnect(AContext: TIdContext);
begin
  // Make sure we have a Context to send data to
  ConnectionContextList.ContextAdd(AContext);

  OwnerConnectionManager.CriticalSectionEnter;
  try
    // Update the ConnectionInfo structure with the current connection
    (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := AContext.Binding.PeerIP;
    (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ClientPort := AContext.Binding.PeerPort;

    if IdTCPServer.Bindings.Count > 0 then
    begin
      (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort := IdTCPServer.Bindings[0].Port;
      (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := IdTCPServer.Bindings[0].IP;
    end;

  finally
    OwnerConnectionManager.CriticalSectionLeave;
  end;

  // Calling Syncronize is bad when the thread is being taken down
  if not Terminated then
    HandleSendConnectionChangeNotify(lcsClientConnected, True);
end;

procedure TLccEthernetServerThread.IdTCPServerDisconnect(AContext: TIdContext);
begin
  // Remove this Context from our list, it is going away
  ConnectionContextList.ContextRemove(AContext);

  OwnerConnectionManager.CriticalSectionEnter;
  try
    (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := AContext.Binding.PeerIP;
    (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ClientPort := AContext.Binding.PeerPort;
    if IdTCPServer.Bindings.Count > 0 then
    begin
      (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort := IdTCPServer.Bindings[0].Port;
      (OwnerConnectionManager.ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := IdTCPServer.Bindings[0].IP;
    end;
  finally
    OwnerConnectionManager.CriticalSectionLeave;
  end;

  // Calling Syncronize is bad when the thread is being taken down
  if not Terminated then
    HandleSendConnectionChangeNotify(lcsClientDisconnected, True);
end;

procedure TLccEthernetServerThread.IdTCPServerExecute(AContext: TIdContext);
var
  WorkerStream: TMemoryStream;
begin
  // Messages are NOT serialized here from all the Connections (Contexts)

  WorkerStream := nil;
  ConnectionContextList.IncomingRawDataForContext(AContext, WorkerStream);

  if Assigned(WorkerStream) then
    RelayToOtherConnections(AContext, WorkerStream);

     // https://stackoverflow.com/questions/64593756/delphi-rio-indy-tcpserver-high-cpu-usage
    // There is another way to do this but with this simple program this is fine
  IndySleep(THREAD_SLEEP_TIME);
end;

{ TLccEthernetServerThreadManager }

procedure TLccEthernetServerThreadManager.CloseConnection;
var
  TimeCount: Integer;
begin
  inherited CloseConnection;
  if Assigned(ServerListener) then
  begin
    try
      ConnectionThreadList.Remove(ServerListener); // Remove from the connection list
      TimeCount := 0;
      ServerListener.HandleSendConnectionChangeNotify(lcsDisconnecting, False);
      ServerListener.Terminate;
      while ServerListener.Running do
      begin
        Inc(TimeCount);
        Sleep(50);
        if TimeCount = 100 then
        begin
          Break // Something went really wrong
        end;
      end;
      ServerListener.HandleSendConnectionChangeNotify(lcsDisconnected, False);
    finally
      FreeAndNil(FServerListener);
    end
  end;
end;

function TLccEthernetServerThreadManager.OpenConnection: TLccConnectionThread;
begin
  CloseConnection;
  inherited OpenConnection;
  Result := CreateListenerObject;
  if Assigned(Result) then
  begin
    ServerListener := Result as TLccEthernetServerThread;
    ConnectionThreadList.Add(ServerListener);  // Add to the Internal List
    ServerListener.Suspended := False;
  end;
end;

function TLccEthernetServerThreadManager.CreateListenerObject: TLccEthernetServerThread;
begin
  Result := TLccEthernetServerThread.Create(True, Self);
end;

end.

