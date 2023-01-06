unit lcc_ethernet_common;

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
      Forms,
    {$ENDIF}
  {$ELSE}
    FMX.Forms,
    System.Generics.Collections,
  {$ENDIF}
  IdIOHandler,
  lcc_node_messages,
  lcc_node_manager,
  lcc_node,
  lcc_utilities,
  lcc_app_common_settings,
  lcc_threaded_circulararray,
  lcc_ethernet_tcp,
  lcc_threaded_stringlist,
  lcc_defines,
  lcc_gridconnect,
  lcc_common_classes,
  lcc_node_messages_can_assembler_disassembler;

const
  THREAD_SLEEP_TIME = 2;

type

  TLccBaseEthernetThread = class;
  TLccEthernetConnectionInfo = class;

  TOnEthernetEvent = procedure(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo) of object;

  { TLccEthernetConnectionInfo }

  TLccEthernetConnectionInfo = class(TLccHardwareConnectionInfo)
  private
    FAutoResolve: Boolean;
    FClientIP: string;
    FClientPort: word;
    FHeartbeat: Integer;
    FLingerTime: Integer;
    FListenerIP: string;
    FListenerPort: word;
    FSuppressConnectionResetError: Boolean;
  public
    property AutoResolveIP: Boolean read FAutoResolve write FAutoResolve;                     // Tries to autoresolve the local unique netword IP of the machine
    property ClientIP: string read FClientIP write FClientIP;
    property ClientPort: word read FClientPort write FClientPort;
    property HeartbeatRate: Integer read FHeartbeat write FHeartbeat;
    property ListenerIP: string read FListenerIP write FListenerIP;
    property ListenerPort: word read FListenerPort write FListenerPort;
    property LingerTime: Integer read FLingerTime write FLingerTime;
    property SuppressConnectionResetError: Boolean read FSuppressConnectionResetError write FSuppressConnectionResetError;

    constructor Create;
    function Clone: TLccHardwareConnectionInfo; override;
  end;

  { TLccBaseEthernetThread }

  TLccBaseEthernetThread = class(TLccConnectionThread)
  protected
    procedure OnConnectionStateChange; virtual;
    procedure OnErrorMessageReceive; virtual;

    procedure AddToOutgoingBuffer(AMessage: TLccMessage);

  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo); override;
    destructor Destroy; override;
  end;

  TLccEthernetHardwareConnectionListener = class(TThread)
  public
    // Must override and create an object of the decentant type
    function CreateThreadObject: TLccEthernetHardwareConnectionListener; virtual; abstract;
  end;

  { TLccEthernetHardwareConnectionManager }

  TLccEthernetHardwareConnectionManager = class(TLccHardwareConnectionManager)
  private
    FLccSettings: TLccSettings;
  protected
    procedure DoConnectionState(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    procedure DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); override;
  public
    function OpenConnectionWithLccSettings: TLccConnectionThread; override;
  published
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
  end;

implementation

{ TLccEthernetConnectionInfo }

constructor TLccEthernetConnectionInfo.Create;
begin
  inherited;
  FSuppressConnectionResetError := True;
end;

function TLccEthernetConnectionInfo.Clone: TLccHardwareConnectionInfo;
begin
  Result := inherited Clone;
  (Result as TLccEthernetConnectionInfo).AutoResolveIP := (Self as TLccEthernetConnectionInfo).AutoResolveIP;
  (Result as TLccEthernetConnectionInfo).ClientIP  := (Self as TLccEthernetConnectionInfo).ClientIP;
  (Result as TLccEthernetConnectionInfo).ClientPort := (Self as TLccEthernetConnectionInfo).ClientPort;
  (Result as TLccEthernetConnectionInfo).ListenerIP := (Self as TLccEthernetConnectionInfo).ListenerIP;
  (Result as TLccEthernetConnectionInfo).ListenerPort := (Self as TLccEthernetConnectionInfo).ListenerPort;
  (Result as TLccEthernetConnectionInfo).HeartbeatRate := (Self as TLccEthernetConnectionInfo).HeartbeatRate;
  (Result as TLccEthernetConnectionInfo).LingerTime := (Self as TLccEthernetConnectionInfo).LingerTime;
  (Result as TLccEthernetConnectionInfo).SuppressConnectionResetError := (Self as TLccEthernetConnectionInfo).SuppressConnectionResetError;
end;


{ TLccEthernetHardwareConnectionManager }

procedure TLccEthernetHardwareConnectionManager.DoConnectionState(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Thread, ConnectionInfo)
end;

procedure TLccEthernetHardwareConnectionManager.DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  if Assigned(OnErrorMessage) then
    OnErrorMessage(Thread, ConnectionInfo)
end;

function TLccEthernetHardwareConnectionManager.OpenConnectionWithLccSettings: TLccConnectionThread;
var
  ConnectionInfo: TLccEthernetConnectionInfo;
begin
  Result := nil;
  if Assigned(LccSettings) then
  begin
    ConnectionInfo := TLccEthernetConnectionInfo.Create;
    try
      ConnectionInfo.ListenerPort := LccSettings.Ethernet.RemoteListenerPort;
      ConnectionInfo.ListenerIP := LccSettings.Ethernet.RemoteListenerIP;
      ConnectionInfo.ClientIP := LccSettings.Ethernet.LocalClientIP;
      ConnectionInfo.ClientPort := LccSettings.Ethernet.LocalClientPort;
      ConnectionInfo.AutoResolveIP := LccSettings.Ethernet.AutoResolveClientIP;
      Result := OpenConnection(ConnectionInfo);
    finally
      ConnectionInfo.Free;
    end;
  end;
end;

{ TLccBaseEthernetThread }

procedure TLccBaseEthernetThread.OnConnectionStateChange;
begin
  inherited;
  (Owner as TLccEthernetHardwareConnectionManager).DoConnectionState(Self, ConnectionInfo);
end;

procedure TLccBaseEthernetThread.OnErrorMessageReceive;
begin
  inherited;
  (Owner as TLccEthernetHardwareConnectionManager).DoErrorMessage(Self, ConnectionInfo);
end;

procedure TLccBaseEthernetThread.AddToOutgoingBuffer(AMessage: TLccMessage);
var
  LocalChunk: TLccDynamicByteArray;
begin
  if (ConnectionInfo as TLccEthernetConnectionInfo).GridConnect then
    OutgoingGridConnect.Add(AMessage.ConvertToGridConnectStr(#10, False))
  else begin
    LocalChunk := nil;
    if AMessage.ConvertToLccTcp(LocalChunk) then
      OutgoingCircularArray.AddChunk(LocalChunk);
  end;
end;


constructor TLccBaseEthernetThread.Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited Create(CreateSuspended, AnOwner, AConnectionInfo);
end;

destructor TLccBaseEthernetThread.Destroy;
begin
  inherited Destroy;
end;

end.

