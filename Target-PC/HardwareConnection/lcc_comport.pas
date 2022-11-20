unit lcc_comport;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ../../lcc_compilers.inc}

uses
  Classes,
  SysUtils,
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

  TLccComPortConnectionInfo = class(TLccHardwareConnectionInfo)
  private
    FBaud: Integer;
    FBits: Integer;
    FComPort: String;
    FHardwareHandshake: Boolean;
    FParity: Char;
    FSoftwareHandshake: Boolean;
    FStopBits: Integer;
  public
    property ComPort: String read FComPort write FComPort;                     // Comport
    property Baud: Integer read FBaud write FBaud;                      // Define connection speed. Baud rate can be from 50 to 4000000 bits per second. (it depends on your hardware!))
    property Bits: Integer read FBits write FBits;                      // Number of bits in communication.
    property Parity: Char read FParity write FParity;                        // Define communication parity (N - None, O - Odd, E - Even, M - Mark or S - Space)
    property StopBits: Integer read FStopBits write FStopBits;                   // Use constants SB1, SB1andHalf, SB2
    property SoftwareHandshake: Boolean read FSoftwareHandshake write FSoftwareHandshake;          // Enable XON/XOFF handshake.
    property HardwareHandShake: Boolean read FHardwareHandshake write FHardwareHandshake;         // Enable CTS/RTS handshake

    function Clone: TLccHardwareConnectionInfo; override;
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

  TLccComPort = class(TLccHardwareConnectionManager)
  private
    FComPortThread: TLccComPortThread;
    FCurrentConnectionState: TLccConnectionState;
    FHub: Boolean;
    FLccSettings: TLccSettings;
    FNodeManager: TLccNodeManager;
    FOnReceiveMessage: TOnHardwareConnectionInfoEvent;
    FRawData: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
    function IsLccLink: Boolean; override;
    function GetConnected: Boolean; override;
    procedure DoReceiveMessage(Info: TLccHardwareConnectionInfo); reintroduce; virtual;
  public
    { Public declarations }
    property ComPortThread: TLccComPortThread read FComPortThread write FComPortThread;
    property RawData: Boolean read FRawData write FRawData;
    property CurrentConnectionState: TLccConnectionState read FCurrentConnectionState write FCurrentConnectionState;  // Current State of the connection

    constructor Create(AOwner: TComponent; ANodeManager: TLccNodeManager); override;

    function FormatComPortString(ComPort: string): string;
    function OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread; override;
    function OpenConnectionWithLccSettings: TLccConnectionThread; override;
  published
    { Published declarations }
    property Hub: Boolean read FHub write FHub;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property OnReceiveMessage: TOnHardwareConnectionInfoEvent read FOnReceiveMessage write FOnReceiveMessage;

    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
  end;


implementation

{ TLccComPortConnectionInfo }

function TLccComPortConnectionInfo.Clone: TLccHardwareConnectionInfo;
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

function TLccComPort.IsLccLink: Boolean;
begin
  Result := False;    // This link does not speak LCC Gridconnect/TCP... it is a custom DCC Gridconnect
end;

function TLccComPort.GetConnected: Boolean;
begin
  Result := False;
  if Assigned(FComPortThread) then
    Result := ComPortThread.Connected;
end;

procedure TLccComPort.DoReceiveMessage(Info: TLccHardwareConnectionInfo);
begin
  if Assigned(OnReceiveMessage) then
    OnReceiveMessage(Self, Info);
end;

constructor TLccComPort.Create(AOwner: TComponent; ANodeManager: TLccNodeManager);
begin
  inherited Create(AOwner, ANodeManager);
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

function TLccComPort.OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
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
  ComPortThread := TLccComPortThread.Create(True, Self, ConnectionInfo);
  ComPortThread.RawData := RawData;
  ComPortThread.Suspended := False;
  Result := ComPortThread;
end;

function TLccComPort.OpenConnectionWithLccSettings: TLccConnectionThread;
var
  AComPortConnectionInfo: TLccComPortConnectionInfo;
begin
  if Assigned(LccSettings) then
  begin
    AComPortConnectionInfo := TLccComPortConnectionInfo.Create;
    try
      AComPortConnectionInfo.Baud := LccSettings.ComPort.BaudRate;
      AComPortConnectionInfo.ComPort := FormatComPortString(LccSettings.ComPort.Port);

      case LccSettings.ComPort.StopBits of
        cpsb_1_StopBit   : AComPortConnectionInfo.StopBits := SB1;
        cpsb_1_5_StopBit : AComPortConnectionInfo.StopBits := SB1andHalf;
        cpsb_2_StopBit   : AComPortConnectionInfo.StopBits := SB2;
      end;

      case LccSettings.ComPort.DataBits of
        cpdb_8_Bits : AComPortConnectionInfo.Bits :=  8;
        cpdb_9_Bits : AComPortConnectionInfo.Bits :=  9;
      end;

      case LccSettings.ComPort.FlowControl of
        cpf_None      :
          begin
            AComPortConnectionInfo.HardwareHandShake := False;
            AComPortConnectionInfo.SoftwareHandshake := False;
          end;
        cpf_CTS_RTS,                // Hardware with CTS/RTS
        cpf_DTR_DSR :              // Hardware with DTR/DSR
          begin
            AComPortConnectionInfo.HardwareHandShake := True;
            AComPortConnectionInfo.SoftwareHandshake := False;
          end;
        cpf_XON_XOFF :            // Software;
          begin
            AComPortConnectionInfo.HardwareHandShake := False;
            AComPortConnectionInfo.SoftwareHandshake := True;
          end;
      end;

      case LccSettings.ComPort.Parity of
        cpp_None    : AComPortConnectionInfo.Parity := 'N';
        cpp_Even    : AComPortConnectionInfo.Parity := 'E';
        cpp_Odd     : AComPortConnectionInfo.Parity := 'O';
        cpp_Mark    : AComPortConnectionInfo.Parity := 'M';
        cpp_Space   : AComPortConnectionInfo.Parity := 'S';
      end;

      Result := OpenConnection(AComPortConnectionInfo);
    finally
      AComPortConnectionInfo.Free;
    end;
  end;
end;

{ TLccComPortThread }

procedure TLccComPortThread.Execute;
var
  TxStr, RcvStr: String;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
  TxList: TStringList;
begin
  FRunning := True;

  HandleSendConnectionNotification(lcsConnecting);
  Serial := TBlockSerial.Create;                                                // Create the Serial object in the context of the thread
  Serial.LinuxLock:=False;
  Serial.RaiseExcept:=False;
  Serial.NonBlock := True;
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
      HandleSendConnectionNotification(lcsConnected);
      try
        try
          FConnected := True;
          while not IsTerminated and ((ConnectionInfo as TLccComPortConnectionInfo).ConnectionState = lcsConnected) do
          begin
            if ConnectionInfo.Gridconnect then              // Handle the ComPort using GridConnect
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

                if GridConnectHelper.GridConnect_DecodeMachine(Ord( RcvStr[i]), GridConnectStrPtr) then
                begin
                  ConnectionInfo.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                  if not RawData then
                    ConnectionInfo.LccMessage.LoadByGridConnectStr(ConnectionInfo.MessageStr);
                  Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessage);
                end;
              end;
            end;
          end;
        finally
          HandleSendConnectionNotification(lcsDisconnecting);

          FConnected := False;
          if Serial.InstanceActive then
            Serial.CloseSocket;
          Serial.Free;
        end;
      finally
        HandleSendConnectionNotification(lcsDisconnected);
        FRunning := False;
      end;
    end;
  end;
end;

procedure TLccComPortThread.ReceiveMessage;
begin
   (Owner as TLccComPort).DoReceiveMessage(ConnectionInfo);
end;


initialization
  RegisterClass(TLccComPort);

finalization

end.
