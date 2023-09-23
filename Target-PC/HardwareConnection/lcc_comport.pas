unit lcc_comport;

{$I ../../lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
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

  LazSynaSer,
  lcc_threaded_circulararray,
  lcc_threaded_stringlist,
  lcc_gridconnect,
  lcc_defines,
  lcc_node_manager,
  lcc_node_messages,
  lcc_app_common_settings,
  lcc_common_classes,
  lcc_ethernet_tcp,
  lcc_node_messages_can_assembler_disassembler;

type
  TLccComPortThread = class;             // Forward
  TLccComPort = class;
  TLccComPortConnectionInfo = class;

  { TComPortConnectionInfo }

  { TLccComPortConnectionInfo }

  TLccComPortConnectionInfo = class(TLccConnectionInfo)
  private
    FBaud: Integer;
    FBits: Integer;
    FComPort: String;
    FHardwareHandshake: Boolean;
    FParity: Char;
    FSoftwareHandshake: Boolean;
    FStopBits: Integer;
  public
    property ComPort: String read FComPort write FComPort;                                  // Comport
    property Baud: Integer read FBaud write FBaud;                                          // Define connection speed. Baud rate can be from 50 to 4000000 bits per second. (it depends on your hardware!))
    property Bits: Integer read FBits write FBits;                                          // Number of bits in communication.
    property Parity: Char read FParity write FParity;                                       // Define communication parity (N - None, O - Odd, E - Even, M - Mark or S - Space)
    property StopBits: Integer read FStopBits write FStopBits;                              // Use constants SB1, SB1andHalf, SB2
    property SoftwareHandshake: Boolean read FSoftwareHandshake write FSoftwareHandshake;   // Enable XON/XOFF handshake.
    property HardwareHandShake: Boolean read FHardwareHandshake write FHardwareHandshake;   // Enable CTS/RTS handshake

    function Clone: TLccConnectionInfo; override;
  end;


  { TLccComPortThread }

  TLccComPortThread =  class(TLccConnectionThread)
    private
      FConnected: Boolean;
      FRawData: Boolean;
      FSerial: TBlockSerial;                                                      // Serial object
    protected
      procedure Execute; override;
      procedure ReceiveMessage;  // For Syncronize

      property Serial: TBlockSerial read FSerial write FSerial;
    public
      property Connected: Boolean read FConnected;
      property RawData: Boolean read FRawData write FRawData;
  end;

  { TLccComPort }

  TLccComPort = class(TLccConnectionThreadManager)
  private
    FComPortThread: TLccComPortThread;
    FCurrentConnectionState: TLccConnectionState;
    FHub: Boolean;
    FLccSettings: TLccSettings;
    FNodeManager: TLccNodeManager;
  //  FOnReceiveMessage: TOnLccConnectionInfoEvent;
    FRawData: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
 //   function IsLccLink: Boolean; override;
    function GetConnected: Boolean; override;
    function GetConnecting: Boolean; override;
  public
    { Public declarations }
    property ComPortThread: TLccComPortThread read FComPortThread write FComPortThread;
    property RawData: Boolean read FRawData write FRawData;
    property CurrentConnectionState: TLccConnectionState read FCurrentConnectionState write FCurrentConnectionState;  // Current State of the connection

    constructor Create(AOwner: TComponent); override;

    function FormatComPortString(ComPort: string): string;
    function OpenConnection: TLccConnectionThread; override;

    procedure SendMessageRawGridConnect(GridConnectStr: String); override;
  published
    { Published declarations }
    property Hub: Boolean read FHub write FHub;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;

    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
  end;
  TLccComPortClass = class of TLccComPort;


implementation

{ TLccComPortConnectionInfo }

function TLccComPortConnectionInfo.Clone: TLccConnectionInfo;
begin
  Result := inherited Clone;
  (Result as TLccComPortConnectionInfo).ComPort := ComPort;
  (Result as TLccComPortConnectionInfo).Baud := Baud;
  (Result as TLccComPortConnectionInfo).Bits := Bits;
  (Result as TLccComPortConnectionInfo).Parity := Parity;
  (Result as TLccComPortConnectionInfo).StopBits := StopBits;
  (Result as TLccComPortConnectionInfo).SoftwareHandshake := SoftwareHandshake;
  (Result as TLccComPortConnectionInfo).HardwareHandShake := HardwareHandShake;
  (Result as TLccComPortConnectionInfo).ConnectionState := ConnectionState;
end;

{ TLccComPort }

{function TLccComPort.IsLccLink: Boolean;
begin
  Result := False;    // This link does not speak LCC Gridconnect/TCP... it is a custom DCC Gridconnect
end;  }

function TLccComPort.GetConnected: Boolean;
begin
  Result := False;
  if Assigned(FComPortThread) then
    Result := ComPortThread.Connected;
end;

function TLccComPort.GetConnecting: Boolean;
begin
  Result := False;
end;

procedure TLccComPort.SendMessageRawGridConnect(GridConnectStr: String);
begin
 // if Assigned(FComPortThread) then
 //   ComPortThread.OutgoingGridConnectList.Add(GridConnectStr + #10);
end;

constructor TLccComPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHub := False;
end;

function TLccComPort.FormatComPortString(ComPort: string): string;
begin
  {$IFDEF MSWINDOWS}
    Result := ComPort;
  {$ELSE}
    {$IFDEF DARWIN}
    Result := PATH_OSX_DEV + ComPort;
    {$ELSE}
    Result := PATH_LINUX_DEV + ComPort;
    {$ENDIF}
  {$ENDIF}
end;

function TLccComPort.OpenConnection: TLccConnectionThread;
begin
  Result := nil;
  (*

  // All done by the new LazSynaSer component
  {$IFDEF MSWINDOWS}

  {$ELSE}
    {$IFDEF DARWIN}
    (ConnectionInfo as TLccComPortConnectionInfo).ComPort := PATH_OSX_DEV + (ConnectionInfo as TLccComPortConnectionInfo).ComPort;
    {$ELSE}
    (ConnectionInfo as TLccComPortConnectionInfo).ComPort := PATH_LINUX_DEV + (ConnectionInfo as TLccComPortConnectionInfo).ComPort;
    {$ENDIF}
  {$ENDIF}      *)
{  ComPortThread := TLccComPortThread.Create(True, Self, ConnectionInfo);
  ComPortThread.RawData := RawData;
  ComPortThread.Suspended := False;
  Result := ComPortThread;    }
end;

{ TLccComPortThread }

procedure TLccComPortThread.Execute;
var
  TxStr, RcvStr: String;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
  TxList: TStringList;
begin
 { Running := True;
  try

    HandleSendConnectionChangeNotify(lcsConnecting, True);
    Serial := TBlockSerial.Create;                                                // Create the Serial object in the context of the thread
    Serial.LinuxLock:=False;
    Serial.RaiseExcept:=False;
    {$IFDEF UNIX}
    Serial.NonBlock := True;
    {$ENDIF}
    Serial.Connect((ConnectionInfo as TLccComPortConnectionInfo).ComPort);
    if Serial.LastError <> 0 then
    begin
      ConnectionInfo.MessageStr := Serial.LastErrorDesc;
      HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
      Running := False;
    end
    else begin
      with (ConnectionInfo as TLccComPortConnectionInfo) do
        Serial.Config(Baud, Bits, Parity, StopBits, SoftwareHandshake, HardwareHandShake);
      if Serial.LastError <> 0 then
      begin
        ConnectionInfo.MessageStr := Serial.LastErrorDesc;
        HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
        Serial.CloseSocket;
        Serial.Free;
        Serial := nil;
        Running := False;
      end
      else begin
        HandleSendConnectionChangeNotify(lcsConnected, True);
        try
          try
            FConnected := True;
            while not IsTerminated and ((ConnectionInfo as TLccComPortConnectionInfo).ConnectionState = lcsConnected) do
            begin
              if ConnectionInfo.Gridconnect then              // Handle the ComPort using GridConnect
              begin
                 // Get all the strings from the outgoing buffer into a single concatinated string
                TxStr := '';
           //     TxList := OutgoingGridConnectList.LockList;
                try
                  for i := 0 to TxList.Count - 1 do
                    TxStr := TxStr + TxList[i] + #10;
                  TxList.Clear;
                finally
        //          OutgoingGridConnectList.UnlockList;
                end;

                // Outside of the threaded string list (so not to block the main thread sending more messages)
                // dispatch this string to all the connections
                if TxStr <> '' then
                begin
                  Serial.SendString(TxStr);
                  if Serial.LastError <> 0 then
                    HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
                end;


                RcvStr := Serial.Recvstring(1);
                case Serial.LastError of
                  0, ErrTimeout : begin end;
                else
                  HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages)
                end;

                for i := 1 to Length(RcvStr) do
                begin
                  GridConnectStrPtr := nil;

            //      if GridConnectHelper.GridConnect_DecodeMachine(Ord( RcvStr[i]), GridConnectStrPtr) then
                  begin
                    ConnectionInfo.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                    if not RawData then
                      ConnectionInfo.LccMessage.LoadByGridConnectStr(ConnectionInfo.MessageStr);
                    Synchronize({$IFDEF LCC_FPC}@{$ENDIF}ReceiveMessage);
                  end;
                end;
              end;
            end;
          finally
            HandleSendConnectionChangeNotify(lcsDisconnecting, True);

            FConnected := False;
            if Serial.InstanceActive then
              Serial.CloseSocket;
            Serial.Free;
          end;
        finally
          HandleSendConnectionChangeNotify(lcsDisconnected, True);
        end;
      end;
    end;

  finally
    Running := False;
  end;     }
end;

procedure TLccComPortThread.ReceiveMessage;
begin
  // (OwnerConnectionManager as TLccComPort).DoReceiveMessage(ConnectionInfo);
end;


initialization
  RegisterClass(TLccComPort);

finalization

end.
