unit lcc_ethernet_websocket;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ../../lcc_compilers.inc}

uses
  Classes,
  SysUtils,

  {$IFDEF FPC}
    Base64,
    sha1,
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
  {$IFDEF LOGGING}
    frame_lcc_logging,
    lcc_detailed_logging,
  {$ENDIF}

  blcksock,
  synsock,
  Synautil,
  lcc_threaded_stringlist,
  lcc_gridconnect,
  lcc_defines,
  lcc_node_messages,
  lcc_app_common_settings,
  lcc_node_messages_can_assembler_disassembler,
  lcc_common_classes,
  lcc_ethernet_common,
  lcc_ethernet_server;



const
  LCC_WEBSOCKET_OPCODE_CONTINUE = $00;
  LCC_WEBSOCKET_OPCODE_TEXT     = $01;
  LCC_WEBSOCKET_OPCODE_BINARY   = $02;
  LCC_WEBSOCKET_OPCODE_CLOSE    = $08;
  LCC_WEBSOCKET_OPCODE_PING     = $09;
  LCC_WEBSOCKET_OPCODE_PONG     = $0A;

  LCC_WEBSOCKET_CLOSE_HEADER_SERVER_LEN = 2;
  LCC_WEBSOCKET_CLOSE_HEADER_SERVER: array[0..LCC_WEBSOCKET_CLOSE_HEADER_SERVER_LEN-1] of Byte = ($88, $80);
  LCC_WEBSOCKET_CLOSE_HEADER_CLIENT_LEN = 6;
  LCC_WEBSOCKET_CLOSE_HEADER_CLIENT: array[0..LCC_WEBSOCKET_CLOSE_HEADER_CLIENT_LEN-1] of Byte = ($88, $00, $56, $78, $AB, $55);

implementation
(*

https://github.com/staspiter/delphi-websocket/blob/master/WebSocketServer.pas

unit WebSocketServer;

interface

uses
  System.SysUtils, System.Generics.Collections,

  IdCustomTCPServer, IdTCPConnection, IdContext, IdIOHandler, IdGlobal, IdCoderMIME, IdHashSHA,
  IdSSL, IdSSLOpenSSL;

type

  TWebSocketServer = class(TIdCustomTCPServer)
  private
    IdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL;
    HashSHA1: TIdHashSHA1;

  protected
    procedure DoConnect(AContext: TIdContext); override;
    function DoExecute(AContext: TIdContext): Boolean; override;

  public
    procedure InitSSL(AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL);

    property OnExecute;

    constructor Create;
    destructor Destroy; override;
  end;

  TWebSocketIOHandlerHelper = class(TIdIOHandler)
  public
    function ReadBytes: TArray<byte>;
    function ReadString: string;

    procedure WriteBytes(RawData: TArray<byte>);
    procedure WriteString(const str: string);
  end;

implementation

function HeadersParse(const msg: string): TDictionary<string, string>;
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

{ TWebSocketServer }

constructor TWebSocketServer.Create;
begin
  inherited Create;

  HashSHA1 := TIdHashSHA1.Create;
  IdServerIOHandlerSSLOpenSSL := nil;
end;

destructor TWebSocketServer.Destroy;
begin
  HashSHA1.DisposeOf;

  inherited;
end;

procedure TWebSocketServer.InitSSL(AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL);
var
  CurrentActive: boolean;
begin
  CurrentActive := Active;
  if CurrentActive then
    Active := false;

  IdServerIOHandlerSSLOpenSSL := AIdServerIOHandlerSSLOpenSSL;
  IOHandler := AIdServerIOHandlerSSLOpenSSL;

  if CurrentActive then
    Active := true;
end;

procedure TWebSocketServer.DoConnect(AContext: TIdContext);
begin
  if AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase then
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough := false;

  // Mark connection as "not handshaked"
  AContext.Connection.IOHandler.Tag := -1;

  inherited;
end;

function TWebSocketServer.DoExecute(AContext: TIdContext): Boolean;
var
  c: TIdIOHandler;
  Bytes: TArray<byte>;
  msg, SecWebSocketKey, Hash: string;
  ParsedHeaders: TDictionary<string, string>;
begin
  c := AContext.Connection.IOHandler;

  // Handshake

  if c.Tag = -1 then
  begin
    c.CheckForDataOnSource(10);

    if not c.InputBufferIsEmpty then
    begin
      // Read string and parse HTTP headers
      try
        c.InputBuffer.ExtractToBytes(TIdBytes(Bytes));
        msg := IndyTextEncoding_UTF8.GetString(TIdBytes(Bytes));
      except
      end;

      ParsedHeaders := HeadersParse(msg);

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
          c.Write('HTTP/1.1 101 Switching Protocols'#13#10
            + 'Upgrade: websocket'#13#10
            + 'Connection: Upgrade'#13#10
            + 'Sec-WebSocket-Accept: ' + Hash
            + #13#10#13#10, IndyTextEncoding_UTF8);
        except
        end;

        // Mark IOHandler as handshaked
        c.Tag := 1;
      end;

      ParsedHeaders.DisposeOf;
    end;
  end;

  Result := inherited;
end;

{ TWebSocketIOHandlerHelper }

function TWebSocketIOHandlerHelper.ReadBytes: TArray<byte>;
var
  l: byte;
  b: array [0..7] of byte;
  i, DecodedSize: int64;
  Mask: array [0..3] of byte;
begin
  // https://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side

  try
    if ReadByte = $81 then
    begin
      l := ReadByte;
      case l of
        $FE:
          begin
            b[1] := ReadByte; b[0] := ReadByte;
            b[2] := 0; b[3] := 0; b[4] := 0; b[5] := 0; b[6] := 0; b[7] := 0;
            DecodedSize := Int64(b);
          end;
        $FF:
          begin
            b[7] := ReadByte; b[6] := ReadByte; b[5] := ReadByte; b[4] := ReadByte;
            b[3] := ReadByte; b[2] := ReadByte; b[1] := ReadByte; b[0] := ReadByte;
            DecodedSize := Int64(b);
          end;
        else
          DecodedSize := l - 128;
      end;
      Mask[0] := ReadByte; Mask[1] := ReadByte; Mask[2] := ReadByte; Mask[3] := ReadByte;

      if DecodedSize < 1 then
      begin
        result := [];
        exit;
      end;

      SetLength(result, DecodedSize);
      inherited ReadBytes(TIdBytes(result), DecodedSize, False);
      for i := 0 to DecodedSize - 1 do
        result[i] := result[i] xor Mask[i mod 4];
    end;
  except
  end;
end;

procedure TWebSocketIOHandlerHelper.WriteBytes(RawData: TArray<byte>);
var
  Msg: TArray<byte>;
begin
  // https://stackoverflow.com/questions/8125507/how-can-i-send-and-receive-websocket-messages-on-the-server-side

  Msg := [$81];

  if Length(RawData) <= 125 then
    Msg := Msg + [Length(RawData)]
  else if (Length(RawData) >= 126) and (Length(RawData) <= 65535) then
    Msg := Msg + [126, (Length(RawData) shr 8) and 255, Length(RawData) and 255]
  else
    Msg := Msg + [127, (int64(Length(RawData)) shr 56) and 255, (int64(Length(RawData)) shr 48) and 255,
      (int64(Length(RawData)) shr 40) and 255, (int64(Length(RawData)) shr 32) and 255,
      (Length(RawData) shr 24) and 255, (Length(RawData) shr 16) and 255, (Length(RawData) shr 8) and 255, Length(RawData) and 255];

  Msg := Msg + RawData;

  try
    Write(TIdBytes(Msg), Length(Msg));
  except
  end;
end;

function TWebSocketIOHandlerHelper.ReadString: string;
begin
  result := IndyTextEncoding_UTF8.GetString(TIdBytes(ReadBytes));
end;

procedure TWebSocketIOHandlerHelper.WriteString(const str: string);
begin
  WriteBytes(TArray<byte>(IndyTextEncoding_UTF8.GetBytes(str)));
end;

end.

*)


// OLD VERSION TO REPLACE

(*
type

  { TLccWebSocketServerThread }

  TLccWebSocketServerThread =  class(TLccEthernetServerThread)
  private
   FWebSocketInitialized: Boolean;
  protected
    procedure BuildAndSendInitializeSuccessReply(ASocket: TTCPBlockSocket; SecWebSocketKey: string);
    procedure BuildAndSendInitializeFailureReply(ASocket: TTCPBlockSocket);
    procedure Execute; override;
    procedure ExtractInitializationRequest(ASocket: TTCPBlockSocket; var Method, URL, Protocol: string);
    function ExtractInitializationHeaderKeys(ASocket: TTCPBlockSocket): TStringList;
   function ReceiveWebSocketStateMachine(ASocket: TTCPBlockSocket): TLccDynamicByteArray;
    procedure SendWebSocketStateMachine(ASocket: TTCPBlockSocket; OutStr: string);

    property WebSocketInitialized: Boolean read FWebSocketInitialized write FWebSocketInitialized;
  end;


  { TLccWebSocketListener }

  TLccWebSocketListener = class(TLccEthernetListener)
  protected
    function CreateThreadObject: TLccEthernetServerThread; override;
  end;

  { TLccWebsocketServer }

  TLccWebsocketServer = class(TLccEthernetServer)
  protected
    function CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener; override;
    function IsLccLink: Boolean; override;
  end;

implementation

{ TLccWebSocketListener }

function TLccWebSocketListener.CreateThreadObject: TLccEthernetServerThread;
begin
  Result := TLccWebSocketServerThread.Create(True, Owner, ConnectionInfo)
end;

{ TLccWebSocketServerThread }

procedure TLccWebSocketServerThread.BuildAndSendInitializeSuccessReply(ASocket: TTCPBlockSocket; SecWebSocketKey: string);
var
  Hash: TSHA1Digest;
  StringStream: TStringStream;
  Base64Stream: TBase64EncodingStream;
  tempS: string;
begin
  StringStream := TStringStream.Create('');
  Base64Stream := TBase64EncodingStream.Create(StringStream);

  ASocket.SendString('HTTP/1.1 101 Switching Protocols' + CRLF);
  ASocket.SendString('Upgrade: websocket' + CRLF);
  ASocket.SendString('Connection: Upgrade' + CRLF);
  tempS := SecWebSocketKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  Hash := SHA1String(tempS);
  tempS := SHA1Print(Hash);   // Test only

  Base64Stream.Write(Hash, SizeOf(TSHA1Digest));
  Base64Stream.Flush;  // not multiple of 3 so need to pad
  tempS := StringStream.DataString;

  ASocket.SendString('Sec-WebSocket-Accept: ' + tempS + CRLF);
  ASocket.SendString('Sec-WebSocket-Protocol: openlcb.websocket' + CRLF);
  ASocket.SendString('' + CRLF);

  FreeAndNil(StringStream);
  FreeAndNil(Base64Stream);

end;

procedure TLccWebSocketServerThread.BuildAndSendInitializeFailureReply(ASocket: TTCPBlockSocket);
begin
  // TODO
  ASocket := ASocket;
end;

procedure TLccWebSocketServerThread.Execute;
var
  TxStr: String;
  GridConnectStrPtr: PGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList, RxList: TStringList;
  DynamicByteArray: TLccDynamicByteArray;
  LocalSleepCount, i: Integer;
  HeaderStrings: TStringList;
  RequestMethod, RequestURL, RequestProtocol: string;
begin
  FRunning := True;

  HandleSendConnectionNotification(lcsConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  if (ConnectionInfo as TLccEthernetConnectionInfo).LingerTime > 0 then
    Socket.SetLinger(True, (ConnectionInfo as TLccEthernetConnectionInfo).LingerTime);
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := (ConnectionInfo as TLccEthernetConnectionInfo).HeartbeatRate;
  Socket.SetTimeout(0);
  Socket.Socket := ListenerSocketHandle;    // Read back the handle
  if Socket.LastError <> 0 then
  begin
    HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
    Socket.CloseSocket;
    Socket.Free;
    Socket := nil;
    GridConnectHelper.Free;
    FRunning := False
  end else
  begin
    (ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := Socket.GetRemoteSinIP;
    (ConnectionInfo as TLccEthernetConnectionInfo).ClientPort := Socket.GetRemoteSinPort;
    (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := Socket.GetLocalSinIP;
    (ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort := Socket.GetLocalSinPort;
    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      GridConnectHelper.Free;
      FRunning := False
    end else
    begin
      HandleSendConnectionNotification(lcsConnected);
      try
        try
          LocalSleepCount := 0;
          while not IsTerminated and (ConnectionInfo.ConnectionState = lcsConnected) do
          begin

            if not WebSocketInitialized then
            begin
              RequestMethod := '';
              RequestURL := '';
              RequestProtocol := '';
              ExtractInitializationRequest(Socket, RequestMethod, RequestURL, RequestProtocol);
              HeaderStrings := ExtractInitializationHeaderKeys(Socket);

              if (RequestURL = '/') and (HeaderStrings.Values['Upgrade:'] = 'websocket') and (HeaderStrings.Values['Sec-WebSocket-Protocol:'] = 'openlcb.websocket') then
                BuildAndSendInitializeSuccessReply(Socket, HeaderStrings.Values['Sec-WebSocket-Key:'])
              else begin
                BuildAndSendInitializeFailureReply(Socket);
                HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
              end;

              FreeAndNil(HeaderStrings);
              WebSocketInitialized := True;
            end;

            if not Terminated then
            begin
              // Handle the Socket using GridConnect
              if ConnectionInfo.Gridconnect then
              begin
                if LocalSleepCount >= ConnectionInfo.SleepCount then
                begin
                  TxStr := '';
                  TxList := OutgoingGridConnect.LockList;
                  try
                    if TxList.Count > 0 then
                    begin
                      TxStr := TxList[0];
                      TxList.Delete(0);
                    end;
                  finally
                    OutgoingGridConnect.UnlockList;
                  end;

                  if (TxStr <> '') and WebSocketInitialized then
                    SendWebSocketStateMachine(Socket, TxStr);
                  LocalSleepCount := 0;
                end;
                Inc(LocalSleepCount);

                if not Terminated then
                begin
                  DynamicByteArray := ReceiveWebSocketStateMachine(Socket);
                  if Length(DynamicByteArray) > 0 then
                  begin
                    GridConnectStrPtr := nil;
                    for i := 0 to Length(DynamicByteArray) - 1 do
                    begin
                      if GridConnectHelper.GridConnect_DecodeMachine(DynamicByteArray[i], GridConnectStrPtr) and WebSocketInitialized then
                      begin
                        ConnectionInfo.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                        ConnectionInfo.LccMessage.LoadByGridConnectStr(ConnectionInfo.MessageStr);

                        case GridConnectMessageAssembler.IncomingMessageGridConnect(ConnectionInfo.LccMessage) of
                            imgcr_True :
                              begin
                                if ConnectionInfo.UseSyncronize  then
                                  Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessage)
                                else begin
                                  RxList := Owner.IncomingGridConnect.LockList;
                                  try
                                    RxList.Add(ConnectionInfo.LccMessage.ConvertToGridConnectStr('', False));
                                  finally
                                    Owner.IncomingGridConnect.UnlockList;
                                  end;
                                end;
                              end;
                            imgcr_False,
                            imgcr_ErrorToSend,
                            imgcr_UnknownError : begin end;
                         end;
                      end;
                    end;
                  end
                end;
              end else
              begin    // Handle the Socket with LCC TCP Protocol
              {
                if LocalSleepCount >= SleepCount then
                begin
                  DynamicByteArray := nil;
                  OutgoingCircularArray.LockArray;
                  try
                    if OutgoingCircularArray.Count > 0 then
                      OutgoingCircularArray.PullArray(DynamicByteArray);
                  finally
                    OutgoingCircularArray.UnLockArray;
                  end;

                  if Length(DynamicByteArray) > 0 then
                  begin
                    Socket.SendBuffer(@DynamicByteArray[0], Length(DynamicByteArray));
                    if Socket.LastError <> 0 then
                      HandleErrorAndDisconnect;
                    DynamicByteArray := nil;
                  end;
                  LocalSleepCount := 0;
                end;
                Inc(LocalSleepCount);

                RcvByte := Socket.RecvByte(1);
                case Socket.LastError of
                  0 :
                    begin
                      DynamicByteArray := nil;
                      if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(RcvByte, FEthernetRec.MessageArray) then
                        Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage);
                    end;
                  WSAETIMEDOUT :
                    begin

                    end;
                  WSAECONNRESET   :
                    begin
                      if not (ConnectionInfo as TLccEthernetConnectionInfo).SuppressConnectionResetError then
                        HandleErrorAndDisconnect
                    end
                else
                  HandleErrorAndDisconnect
                end;    }
              end
            end;
          end;
        finally
          HandleSendConnectionNotification(lcsDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
          GridConnectHelper.Free;
        end;
      finally
        HandleSendConnectionNotification(lcsDisconnected);
        (Owner as TLccEthernetHardwareConnectionManager).ConnectionThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;

procedure TLccWebSocketServerThread.ExtractInitializationRequest(ASocket: TTCPBlockSocket; var Method, URL, Protocol: string);
var
  s: String;
  StrList: TSTringList;
begin
  s := ASocket.RecvString(120000);

  s := StringReplace(s, ' ', #10, [rfReplaceAll]);
  StrList := TStringList.Create;
  StrList.Delimiter := #10;
  StrList.DelimitedText := s;
  if StrList.Count = 3 then
  begin
    Method := StrList[0];
    URL := StrList[1];
    Protocol := StrList[2];
  end else
  begin
    Method := '';
    URL := '';
    Protocol := ''
  end;
end;

function TLccWebSocketServerThread.ExtractInitializationHeaderKeys(ASocket: TTCPBlockSocket): TStringList;
var
  s: string;
begin
  Result := TStringList.Create;
  repeat
    s := ASocket.RecvString(120000);
    s := StringReplace(s, ' ', '=', []);
    Result.Add(s);
  until s = '';
end;

function TLccWebSocketServerThread.ReceiveWebSocketStateMachine(ASocket: TTCPBlockSocket): TLccDynamicByteArray;
var
  AByte: Byte;
  iHeader: Integer;
  PayloadSize, iPayload: QWord;
  HasMask, IsClose, IsPing, IsPong: Boolean;
  Mask: array[0..3] of Byte;
begin
  Result := nil;
  AByte := ASocket.RecvByte(0);
  case ASocket.LastError of
    0 :
      begin
        // We assume we are in sync and this is the start of the Message
  //      IsBinary := AByte and $0F = WEBSOCKET_OPCODE_BINARY;
  //      IsText := AByte and $0F = WEBSOCKET_OPCODE_TEXT;
        IsClose := AByte and $0F = LCC_WEBSOCKET_OPCODE_CLOSE;
  //      IsContinuation := AByte and $0F = WEBSOCKET_OPCODE_CONTINUE;
        IsPing := AByte and $0F = LCC_WEBSOCKET_OPCODE_PING;
        IsPong := AByte and $0F = LCC_WEBSOCKET_OPCODE_PONG;

        if (AByte and $80 = $80) then  // Last Frame of a concatinated message
        begin
          AByte := ASocket.RecvByte(0);
          PayloadSize := AByte and $7F;
          HasMask := AByte and $80 = $80;
          if PayloadSize = 126 then
          begin
            PayloadSize := ASocket.RecvByte(0) shl 8;
            PayloadSize := PayloadSize or ASocket.RecvByte(0);
          end else
          if PayloadSize = 127 then
          begin
            PayloadSize := ASocket.RecvByte(0) shl 56;
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 48);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 40);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 32);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 24);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 16);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 8);
            PayloadSize := PayloadSize or ASocket.RecvByte(0);
          end;
          if HasMask then
          begin
            Mask[0] := ASocket.RecvByte(0);
            Mask[1] := ASocket.RecvByte(0);
            Mask[2] := ASocket.RecvByte(0);
            Mask[3] := ASocket.RecvByte(0);
          end;
          iPayload := 0;
          SetLength(Result, PayloadSize);
          while iPayload < PayloadSize do
          begin
            AByte := ASocket.RecvByte(0);
            Result[iPayload] := AByte XOR Mask[iPayload mod 4];
            Inc(iPayload);
          end;
        end;
        if IsClose then
        begin
          ASocket.ResetLastError;
          for iHeader := 0 to LCC_WEBSOCKET_CLOSE_HEADER_SERVER_LEN - 1 do
            ASocket.SendByte(LCC_WEBSOCKET_CLOSE_HEADER_SERVER[iHeader]);
          HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages)
        end else
        if IsPing then
        begin

        end else
        if IsPong then
        begin

        end
      end;
    WSAETIMEDOUT :
      begin

      end;
    WSAECONNRESET   :
      begin
        HandleErrorAndDisconnect((ConnectionInfo.SuppressErrorMessages) or (ConnectionInfo as TLccEthernetConnectionInfo).SuppressConnectionResetError)
      end
  end;
end;

procedure TLccWebSocketServerThread.SendWebSocketStateMachine(ASocket: TTCPBlockSocket; OutStr: string);
var
  i: Integer;
begin
  // No mask from Server to Client
  ASocket.SendByte($80 or LCC_WEBSOCKET_OPCODE_TEXT);
  ASocket.SendByte(Length(OutStr));
  {$IFDEF LCC_MOBILE}
   for i := 0 to Length(OutStr) - 1 do
    ASocket.SendByte(Ord(OutStr[i]));
  {$ELSE}
  for i := 1 to Length(OutStr) do
    ASocket.SendByte(Ord(OutStr[i]));
  {$ENDIF}

 if ASocket.LastError <> 0 then
   HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
end;

{ TLccWebsocketServer }

function TLccWebsocketServer.CreateListenerObject(
  AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener;
begin
  Result := TLccWebSocketListener.Create(True, Self, AConnectionInfo)
end;

function TLccWebsocketServer.IsLccLink: Boolean;
begin
  Result := True;
end;
       *)

end.

