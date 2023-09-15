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
    FOwner: TLccConnectionContextList;
    FTcpDecodeStateMachine: TTcpDecodeStateMachine;
    FWorkerMessage: TLccMessage;
  protected
    // Helpers
    property GridConnectDecodeStateMachine: TGridConnectDecodeStateMachine read FGridConnectDecodeStateMachine;
    property GridConnectMessageAssembler: TLccGridConnectMessageAssembler read FGridConnectMessageAssembler write FGridConnectMessageAssembler;
    property TcpDecodeStateMachine: TTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  public
    property Context: TIdContext read FContext write FContext;
    property Owner: TLccConnectionContextList read FOwner;

    constructor Create(AnOwner: TLccConnectionContextList; AContext: TIdContext); virtual;
    destructor Destroy; override;

    procedure IncomingRawData(DataStream: TStream); virtual;
  end;

  TLccConnectionContextClass = class of TLccConnectionContext;

  { TLccConnectionContextList }

  TLccConnectionContextList = class(Classes.TThreadList)
  private
    FDefaultContextClass: TLccConnectionContextClass;    // What ConnectionContext type to create for the list
    FOwnerListenerThread: TLccEthernetServerThread;      // What thread owns us

  public
    property OwnerListenerThread: TLccEthernetServerThread read FOwnerListenerThread;
    property DefaultContextClass: TLccConnectionContextClass read FDefaultContextClass write FDefaultContextClass;

    constructor Create(AnOwner: TLccEthernetServerThread);
    destructor Destroy; override;
    function ContextAdd(AContext: TIdContext): TLccConnectionContext;
    function ContextRemove(AContext: TIdContext): Boolean;
    procedure IncomingRawDataForContext(AContext: TIdContext; ADataStream: TStream);
  end;


  { TLccEthernetServerThread }

  TLccEthernetServerThread = class(TLccConnectionThread)
  private
    FConnectionContextList: TLccConnectionContextList;
    FIdTCPServer: TIdTCPServer;
  protected
    property Running: Boolean read FRunning;
    property IdTCPServer: TIdTCPServer read FIdTCPServer write FIdTCPServer;
    property ConnectionContextList: TLccConnectionContextList read FConnectionContextList write FConnectionContextList;


    procedure IdTCPServerConnect(AContext: TIdContext); virtual;
    procedure IdTCPServerDisconnect(AContext: TIdContext); virtual;
    procedure IdTCPServerExecute(AContext: TIdContext); virtual;

    procedure ReceiveMessageFromContextViaSyncronize;  // For Syncronize
    procedure SetConnecting(AValue: Boolean); override;

    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager; AConnectionInfo: TLccHardwareConnectionInfo); override;
    destructor Destroy; override;
  end;

  { TLccEthernetServerThreadManager }

  TLccEthernetServerThreadManager = class(TLccConnectionThreadManager)
  private
    FServerListener: TLccEthernetServerThread;
    { Private declarations }
  protected
    { Protected declarations }
    function CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetServerThread; virtual;
    function GetConnected: Boolean; override;
    function GetConnecting: Boolean; override;


  public
    { Public declarations }
    property ServerListener: TLccEthernetServerThread read FServerListener write FServerListener;

    function OpenConnection(AConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread; override;
    procedure CloseConnection;  override;
  end;


  { TLccWebsocketConnectionContext }

  TLccWebsocketConnectionContext = class(TLccConnectionContext)
  private
    FWebSocketHeaderStrippedReceiveStream: TMemoryStream;
    FUpgraded: Boolean;
  public
    property Upgraded: Boolean read FUpgraded write FUpgraded;
    property WebSocketHeaderStrippedReceiveStream: TMemoryStream read FWebSocketHeaderStrippedReceiveStream write FWebSocketHeaderStrippedReceiveStream;

    constructor Create(AnOwner: TLccConnectionContextList; AContext: TIdContext); override;
    destructor Destroy; override;

    procedure IncomingRawData(DataStream: TStream); override;
  end;

  TLccWebsocketConnectionContextClass = class of TLccWebsocketConnectionContext;


  { TLccWebSocketServerThread }

  TLccWebSocketServerThread = class(TLccEthernetServerThread)
  private
    FHashSHA1: TIdHashSHA1;
    FIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL;
    FSendMessageTemp: TMemoryStream;
  protected
    property IdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL read FIdServerIOHandlerSSLOpenSSL write FIdServerIOHandlerSSLOpenSSL;
    property HashSHA1: TIdHashSHA1 read FHashSHA1 write FHashSHA1;
    property SendMessageTemp: TMemoryStream read FSendMessageTemp write FSendMessageTemp;

  {  function ReceiveContextDataAsString(AContext: TIdContext): string; override;
    function ReceiveContextDataAsBytes(AContext: TIdContext): TIdBytes; override;
    procedure SendContextDataAsString(AContext: TIdContext; AString: string); override;
    procedure SendContextDataAsBytes(AContext: TIdContext; ABytes: TIdBytes); override;
  }
    procedure IdTCPServerExecute(AContext: TIdContext); override;

    function ParseHeader(const msg: string): TDictionary<string, string>;
    function LoadStreamFromMessageBuffer(AStream: TStream; AMessageBuffer: Classes.TThreadList; ClearBuffer: Boolean = True): Boolean; override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager; AConnectionInfo: TLccHardwareConnectionInfo); override;
    destructor Destroy; override;
  end;


  { TLccWebSocketServerThreadManager }

  TLccWebSocketServerThreadManager = class(TLccEthernetServerThreadManager)
  protected
    function CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetServerThread; override;
  public
    procedure InitSSL(AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL);

  end;


implementation

{ TLccWebsocketConnectionContext }

constructor TLccWebsocketConnectionContext.Create(AnOwner: TLccConnectionContextList; AContext: TIdContext);
begin
  inherited Create(AnOwner, AContext);
  WebSocketHeaderStrippedReceiveStream := TMemoryStream.Create;
end;

destructor TLccWebsocketConnectionContext.Destroy;
begin
  FreeAndNil(FWebSocketHeaderStrippedReceiveStream);
  inherited Destroy;
end;

procedure TLccWebsocketConnectionContext.IncomingRawData(DataStream: TStream);
var
  l: byte;
  b: array [0..7] of byte;
  i, DecodedSize: int64;
  Mask: array [0..3] of byte;
begin
  // https://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side

  DataStream.Position := 0;
  // Strip off the header from the data
  try
    if DataStream.ReadByte = $81 then
    begin
      l := DataStream.ReadByte;
      case l of
        $FE:
          begin
            b[1] := DataStream.ReadByte; b[0] := DataStream.ReadByte;
            b[2] := 0; b[3] := 0; b[4] := 0; b[5] := 0; b[6] := 0; b[7] := 0;
            DecodedSize := Int64(b);
          end;
        $FF:
          begin
            b[7] := DataStream.ReadByte; b[6] := DataStream.ReadByte; b[5] := DataStream.ReadByte; b[4] := DataStream.ReadByte;
            b[3] := DataStream.ReadByte; b[2] := DataStream.ReadByte; b[1] := DataStream.ReadByte; b[0] := DataStream.ReadByte;
            DecodedSize := Int64(b);
          end;
        else
          DecodedSize := l - 128;
      end;
      Mask[0] := DataStream.ReadByte; Mask[1] := DataStream.ReadByte; Mask[2] := DataStream.ReadByte; Mask[3] := DataStream.ReadByte;

      if DecodedSize > 0 then
      begin
        // Client To Server has the data Masked
        WebSocketHeaderStrippedReceiveStream.Size := DecodedSize;
        WebSocketHeaderStrippedReceiveStream.Position := 0;
        WebSocketHeaderStrippedReceiveStream.WriteByte( DataStream.ReadByte xor Mask[WebSocketHeaderStrippedReceiveStream.Position mod 4]);
        inherited IncomingRawData(WebSocketHeaderStrippedReceiveStream);
      end;
    end;
  except
  end;
end;

{ TLccWebSocketServerThread }

constructor TLccWebSocketServerThread.Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager; AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited Create(CreateSuspended, AnOwner, AConnectionInfo);

  HashSHA1 := TIdHashSHA1.Create;
  IdServerIOHandlerSSLOpenSSL := nil;
  SendMessageTemp := TMemoryStream.Create;
end;

destructor TLccWebSocketServerThread.Destroy;
begin
  FreeAndNil(FSendMessageTemp);
  FreeAndNil(FHashSHA1);
  inherited Destroy;
end;

{
function TLccWebSocketServerThread.ReceiveContextDataAsString(AContext: TIdContext): string;
begin
  Result := string( IndyTextEncoding_UTF8.GetString(ReceiveContextDataAsBytes(AContext)));
end;  }

{

function TLccWebSocketServerThread.ReceiveContextDataAsBytes(AContext: TIdContext): TIdBytes;
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
      }

      {
procedure TLccWebSocketServerThread.SendContextDataAsString(AContext: TIdContext; AString: string);
begin
  SendContextDataAsBytes(AContext, IndyTextEncoding_UTF8.GetBytes( UnicodeString( AString)));
end;

  }

  {
procedure TLccWebSocketServerThread.SendContextDataAsBytes(AContext: TIdContext; ABytes: TIdBytes);
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

}

procedure TLccWebSocketServerThread.IdTCPServerExecute(AContext: TIdContext);
var
  Bytes: TIdBytes;
  msg, SecWebSocketKey, Hash: string;
  ParsedHeaders: TDictionary<string, string>;
  ConnectionContext: TLccWebsocketConnectionContext;
begin

  Bytes := [];

  ConnectionContext := ConnectionContextList.ContextAdd(AContext) as TLccWebsocketConnectionContext;
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

function TLccWebSocketServerThread.LoadStreamFromMessageBuffer(AStream: TStream; AMessageBuffer: Classes.TThreadList; ClearBuffer: Boolean): Boolean;
var
  StreamDataLen, i: Int64;
begin
  // https://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side

  // Add the header to the data
  // No mask from Server to Client

  SendMessageTemp.Position := 0;
  Result := inherited LoadStreamFromMessageBuffer(SendMessageTemp, AMessageBuffer, ClearBuffer);

  if Result then
  begin

    StreamDataLen := SendMessageTemp.Size;

    AStream.Position:= 0;
    if StreamDataLen <= 125 then
    begin
      AStream.WriteByte($81);
      AStream.WriteByte(StreamDataLen);
    end else if (StreamDataLen >= 126) and (StreamDataLen <= 65535) then
    begin
      AStream.WriteByte($81);
      AStream.WriteByte(126);
      AStream.WriteByte((StreamDataLen shr 8) and 255);
      AStream.WriteByte(StreamDataLen and 255);
    end else
    begin
      AStream.WriteByte($81);
      AStream.WriteByte(127);
      AStream.WriteByte((int64(StreamDataLen) shr 56) and 255);
      AStream.WriteByte((int64(StreamDataLen) shr 48) and 255);
      AStream.WriteByte((int64(StreamDataLen) shr 40) and 255);
      AStream.WriteByte((int64(StreamDataLen) shr 32) and 255);
      AStream.WriteByte((StreamDataLen shr 24) and 255);
      AStream.WriteByte((StreamDataLen shr 16) and 255);
      AStream.WriteByte((StreamDataLen shr 8) and 255);
      AStream.WriteByte(StreamDataLen and 255);
    end;

    AStream.Size := AStream.Size + SendMessageTemp.Size;

    for i := 0 to SendMessageTemp.Size - 1 do
      AStream.WriteByte( SendMessageTemp.ReadByte);
  end;

end;

{$IFDEF LCC_FPC}
function TLccWebSocketServerThread.ParseHeader(const msg: string): TDictionary<string, string>;
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
function TLccWebSocketServerThread.ParseHeader(const msg: string): TDictionary<string, string>;
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

{ TLccWebSocketServerThreadManager }

function TLccWebSocketServerThreadManager.CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetServerThread;
begin
  Result := TLccWebSocketServerThread.Create(True, Self, AConnectionInfo);
  Result.ConnectionContextList.DefaultContextClass := TLccWebsocketConnectionContext;
end;

procedure TLccWebSocketServerThreadManager.InitSSL(AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL);
var
  CurrentActive: boolean;
begin
  CurrentActive := (ServerListener as TLccWebSocketServerThread).IdTCPServer.Active;
  if CurrentActive then
    (ServerListener as TLccWebSocketServerThread).IdTCPServer.Active := false;

  (ServerListener as TLccWebSocketServerThread).IdServerIOHandlerSSLOpenSSL := AIdServerIOHandlerSSLOpenSSL;
  (ServerListener as TLccWebSocketServerThread).IdTCPServer.IOHandler := AIdServerIOHandlerSSLOpenSSL;

  if CurrentActive then
    (ServerListener as TLccWebSocketServerThread).IdTCPServer.Active := true;
end;

{ TLccWebSocketServerThread }


{ TLccConnectionContextList }

constructor TLccConnectionContextList.Create(AnOwner: TLccEthernetServerThread);
begin
  inherited Create;
  FDefaultContextClass := TLccConnectionContext;
  FOwnerListenerThread := AnOwner;
end;

destructor TLccConnectionContextList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TLccConnectionContextList.ContextAdd(AContext: TIdContext
  ): TLccConnectionContext;
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
  AContext: TIdContext; ADataStream: TStream);
var
  ContextList:  TList;
  iContext: Integer;
  ConnectionContext: TLccConnectionContext;
begin
  if AContext.Connection.IOHandler.Connected then
  begin
    ContextList := LockList;
    try
      // Contexts are added/removed in IdTCPServerConnect/IdTCPServerDisconnectConnect events
      for iContext := 0 to ContextList.Count - 1 do
      begin
        ConnectionContext := TLccConnectionContext(ContextList[iContext]);
        if ConnectionContext.Context = AContext then
        begin
          ConnectionContext.IncomingRawData(ADataStream);
          Break;
        end;
      end;
    finally
      UnlockList;
    end;
  end;
end;

{ TLccConnectionContext }

constructor TLccConnectionContext.Create(AnOwner: TLccConnectionContextList; AContext: TIdContext);
begin
  FOwner := AnOwner;
  FContext := AContext;
  FGridConnectDecodeStateMachine := TGridConnectDecodeStateMachine.Create;
  FGridConnectMessageAssembler := TLccGridConnectMessageAssembler.Create;
  FTcpDecodeStateMachine := TTcpDecodeStateMachine.Create;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TLccConnectionContext.Destroy;
begin
  inherited;
  FreeAndNil(FGridConnectDecodeStateMachine);
  FreeAndNil(FGridConnectMessageAssembler);
  FreeAndNil(FTcpDecodeStateMachine);
  FreeAndNil(FWorkerMessage);
end;

procedure TLccConnectionContext.IncomingRawData(DataStream: TStream);
var
  iData: Integer;
  LocalDataArray: TLccDynamicByteArray;
  GridConnectStrPtr: PGridConnectString;
  MessageStr: String;
begin
  DataStream.Position := 0;

  if Owner.OwnerListenerThread.ConnectionInfo.GridConnect then
  begin
    for iData := 0 to DataStream.Size - 1 do
    begin
      // Take the incoming characters and try to make a valid gridconnect message
      GridConnectStrPtr := nil;
      if GridConnectDecodeStateMachine.GridConnect_DecodeMachine(DataStream.ReadByte, GridConnectStrPtr) then
      begin     // Have a valid gridconnect message
        MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
        WorkerMessage.LoadByGridConnectStr(MessageStr);

        // Message may only be part of a larger string of messages to make up a full LCC message.
        // This call will concatinate these partial Lcc message and return with a fully qualified
        // Lcc message.
        case GridConnectMessageAssembler.IncomingMessageGridConnect(WorkerMessage) of
          imgcr_True :
            begin
          //    NodeManager.ReceiveMessageServerThread.ReceiveMessageServerAddMessage(WorkerMessage);
          //    try
           //     if not OwnerListenerThread.Terminated then
           //       OwnerListenerThread.Synchronize(OwnerListenerThread.ReceiveMessageFromContextViaSyncronize);  // WorkerMessage contains the message
           //   except
          //    end;
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
  end else
  begin
    LocalDataArray := [];
    for iData := 0 to DataStream.Size - 1 do
    begin
      if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(DataStream.ReadByte, LocalDataArray) then
      begin
        if WorkerMessage.LoadByLccTcp(LocalDataArray) then
        begin
    //      NodeManager.ReceiveMessageServerThread.ReceiveMessageServerAddMessage(WorkerMessage);
    //      try
    //        if not OwnerListenerThread.Terminated then
    //          OwnerListenerThread.Synchronize(OwnerListenerThread.ReceiveMessageFromContextViaSyncronize);
    //      except
    //      end;
        end
      end;
    end;
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
              // Outside of the Message Buffer (so not to block the main thread sending more messages)
              // dispatch this string to all the connections
              SendMessageStream.Clear;
              if LoadStreamFromMessageBuffer(SendMessageStream, SendMessageBuffer) then
              begin
                // Decided to make the sendmessage side more centralized vs having buffers
                // in each connection context and have the message convertered N times in
                // each context.  Incoming this makes sense as it has to happen, here
                // it does not.
                for i := 0 to IdTCPServer.Contexts.Count - 1 do
                begin
                  if TIdContext( ContextList[i]).Connection.Connected then
                    TIdContext( ContextList[i]).Connection.IOHandler.Write(SendMessageStream);
                  SendMessageStream.Position := 0;  // Reset for the next Connection Context
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

constructor TLccEthernetServerThread.Create(CreateSuspended: Boolean; AnOwner: TLccConnectionThreadManager; AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited Create(CreateSuspended, AnOwner, AConnectionInfo);
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
  ConnectionContextList.ContextAdd(AContext);

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

procedure TLccEthernetServerThread.IdTCPServerDisconnect(AContext: TIdContext);
begin
  ConnectionContextList.ContextRemove(AContext);

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

procedure TLccEthernetServerThread.IdTCPServerExecute(AContext: TIdContext);
var
  List: TList;
  iContext: Integer;
  OtherContext: TIdContext;
begin

  // Messages serialized here from all the Connections (Contexts)

  ReceiveStream.Size := 0;
  AContext.Connection.IOHandler.ReadStream(ReceiveStream);
  ConnectionContextList.IncomingRawDataForContext(AContext, ReceiveStream);

  if Owner.Hub then
  begin
    List := IdTCPServer.Contexts.LockList;
    try
      for iContext := 0 to List.Count - 1 do
      begin
        OtherContext := TIdContext(List[iContext]);
        if OtherContext <> AContext then
          if OtherContext.Connection.Connected then
  //          SendContextDataAsString(OtherContext, AString);         // TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      end;
    finally
      IdTCPServer.Contexts.UnlockList;
    end;
  end;

     // https://stackoverflow.com/questions/64593756/delphi-rio-indy-tcpserver-high-cpu-usage
    // There is another way to do this but with this simple program this is fine
  IndySleep(THREAD_SLEEP_TIME);
end;

procedure TLccEthernetServerThread.ReceiveMessageFromContextViaSyncronize;
begin
 // (Owner as TLccEthernetServerThreadManager).DoReceiveMessage(ConnectionContextList.WorkerMessage);
end;

procedure TLccEthernetServerThread.SetConnecting(AValue: Boolean);
begin
  Owner.CriticalSectionEnter;
  try
    inherited SetConnecting(AValue);
  finally
    Owner.CriticalSectionLeave;
  end;
end;

{ TLccEthernetServerThreadManager }

procedure TLccEthernetServerThreadManager.CloseConnection;
var
  TimeCount: Integer;
begin
  NodeManager.Clear;
  inherited CloseConnection;
  if Assigned(ServerListener) then
  begin
    try
      TimeCount := 0;
      ServerListener.HandleSendConnectionNotification(lcsDisconnecting, False);
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
      if ServerListener.ErrorOnExit then
        ServerListener.ErrorMessage;
      ServerListener.HandleSendConnectionNotification(lcsDisconnected, False);
    finally
      FreeAndNil(FServerListener);
    end
  end;
end;

function TLccEthernetServerThreadManager.GetConnected: Boolean;
begin
  Result := False;
  CriticalSectionEnter;
  try
    if Assigned(ServerListener) and Assigned(ServerListener.IdTCPServer) then
      Result := ServerListener.IdTCPServer.Active;
  finally
    CriticalSectionLeave;
  end;
end;

function TLccEthernetServerThreadManager.GetConnecting: Boolean;
begin
  Result := False;
  CriticalSectionEnter;
  try
    if Assigned(ServerListener) then
      Result := ServerListener.Connecting;
  finally
    CriticalSectionLeave;
  end;
end;

function TLccEthernetServerThreadManager.OpenConnection(AConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  CloseConnection;
  inherited OpenConnection(AConnectionInfo);
  Result := CreateListenerObject(AConnectionInfo.Clone as TLccEthernetConnectionInfo);
  ServerListener := Result as TLccEthernetServerThread;
  ServerListener.Suspended := False;
end;

function TLccEthernetServerThreadManager.CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetServerThread;
begin
  Result := TLccEthernetServerThread.Create(True, Self, AConnectionInfo);
end;

end.

