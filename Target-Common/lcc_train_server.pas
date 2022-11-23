unit lcc_train_server;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ..\lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
    contnrs,
    {$IFNDEF FPC_CONSOLE_APP}
      ExtCtrls,
    {$ENDIF}
  {$ELSE}
    System.Types,
    FMX.Types,
    System.Generics.Collections,
  {$ENDIF}
  lcc_defines,
  lcc_node_messages,
  lcc_utilities,
  lcc_math_float16;

type

  TLccTrainControllerResult = (tcr_Ok, tcr_ControllerRefused, tcr_TrainRefused);

  TLccTractionServer = class; // forward

  { TLccTractionControllerObject }

  TLccTractionControllerObject = class
  private
    FNodeID: TNodeID;
    FFlags: Byte;
  public
    property Node: TNodeID read FNodeID write FNodeID;
    property Flags: Byte read FFlags write FFlags;
  end;

  { TLccTractionObject }

  TLccTractionObject = class
  private
    FController: TLccTractionControllerObject;
    FNodeAlias: Word;
    FNodeID: TNodeID;
    FSNIP: TLccSNIPObject;
    FSpeedActual: THalfFloat;
    FSpeedCommanded: THalfFloat;
    FSpeedSet: THalfFloat;
    FFunctions: TLccFunctions;
    FSpeedStatusEmergencyStop: Boolean;
    FTrainSNIP: TLccTrainSNIPObject;
    function GetFunctions(Index: Integer): Word;
    procedure SetFunctions(Index: Integer; AValue: Word);
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;
    property SpeedSet: THalfFloat read FSpeedSet;
    property SpeedCommanded: THalfFloat read FSpeedCommanded;
    property SpeedActual: THalfFloat read FSpeedActual;
    property SpeedStatusEmergencyStop: Boolean read FSpeedStatusEmergencyStop;
    property SNIP: TLccSNIPObject read FSNIP;
    property TrainSNIP: TLccTrainSNIPObject read FTrainSNIP;
    property Controller: TLccTractionControllerObject read FController write FController;

    constructor Create;
    destructor Destroy; override;
  end;

  TOnLccServerRegisterChange = procedure(TractionServer: TLccTractionServer; LccNode: TObject; TractionObject: TLccTractionObject; IsRegistered: Boolean) of object;
  TOnLccServerChange = procedure(TractionServer: TLccTractionServer; LccNode: TObject; TractionObject: TLccTractionObject) of object;

  { TLccTractionServer }

  TLccTractionServer = class
  private
    FEnabled: Boolean;
    FOnEmergencyStopChange: TOnLccServerChange;
    FOnFunctionChange: TOnLccServerChange;
    FOnListenerAttach: TOnLccServerChange;
    FOnListenerDetach: TOnLccServerChange;
    FOnListenerManageRelease: TOnLccServerChange;
    FOnListenerManageReserve: TOnLccServerChange;
    FOnRegisterChange: TOnLccServerRegisterChange;
    FOnSNIPChange: TOnLccServerChange;
    FOnSpeedChange: TOnLccServerChange;
    FOnTrainSNIPChange: TOnLccServerChange;
    {$IFDEF DELPHI}
    FTractionList: TObjectList<TLccTractionObject>;
    {$ELSE}
    FTractionList: TObjectList;
    FWorkerMessage: TLccMessage;
    {$ENDIF}
  protected
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    procedure DoRegisterChange(LccNode: TObject; TractionObject: TLccTractionObject; IsRegistered: Boolean);
    procedure DoSNIPChange(LccNode: TObject; TractionObject: TLccTractionObject);
    procedure DoTrainSNIPChange(LccNode: TObject; TractionObject: TLccTractionObject);
    procedure DoSpeedChange(LccNode: TObject; TractionObject: TLccTractionObject);
    procedure DoFunctionChange(LccNode: TObject; TractionObject: TLccTractionObject);
    procedure DoEmergencyStopChange(LccNode: TObject; TractionObject: TLccTractionObject);
    procedure DoListenerAttach(LccNode: TObject; TractionObject: TLccTractionObject);
    procedure DoListenerDetach(LccNode: TObject; TractionObject: TLccTractionObject);
    procedure DoListenerManageReserve(LccNode: TObject; TractionObject: TLccTractionObject);
    procedure DoListenerManageRelease(LccNode: TObject; TractionObject: TLccTractionObject);

    procedure HandleProducerIdentifiedAll(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleSimpleNodeInfoReply(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionControllerAssign(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionControllerRelease(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionControllerQuery(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionControllerChangingNotify(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionControllerChangedNotify(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionEStop(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionListenerAttach(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionListenerDetach(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionListenerQuery(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionManageReserve(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionManageRelease(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionSetSpeed(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionSetFunction(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionSimpleTrainInfoReply(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionQuerySpeed(ALccNode: TObject; var SourceMessage: TLccMessage);
    procedure HandleTractionQueryFunction(ALccNode: TObject; var SourceMessage: TLccMessage);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(NewNodeID: TNodeID; NewAlias: Word): TLccTractionObject;
    function Remove(TestNodeID: TNodeID): TLccTractionObject;
    function Find(TestNodeID: TNodeID): TLccTractionObject;

    procedure Clear;

    {$IFDEF DELPHI}
    property TractionList: TObjectList<TLccTractionObject> read FTractionList write FTractionList;
    {$ELSE}
    property TractionList: TObjectList read FTractionList write FTractionList;
    {$ENDIF}

    property Enabled: Boolean read FEnabled write FEnabled;

    property OnRegisterChange: TOnLccServerRegisterChange read FOnRegisterChange write FOnRegisterChange;
    property OnSNIPChange: TOnLccServerChange read FOnSNIPChange write FOnSNIPChange;
    property OnTrainSNIPChange: TOnLccServerChange read FOnTrainSNIPChange write FOnTrainSNIPChange;
    property OnSpeedChange: TOnLccServerChange read FOnSpeedChange write FOnSpeedChange;
    property OnFunctionChange: TOnLccServerChange read FOnFunctionChange write FOnFunctionChange;
    property OnEmergencyStopChange: TOnLccServerChange read FOnEmergencyStopChange write FOnEmergencyStopChange;
    property OnListenerAttach: TOnLccServerChange read FOnListenerAttach write FOnListenerAttach;
    property OnListenerDetach: TOnLccServerChange read FOnListenerDetach write FOnListenerDetach;
    property OnListenerManageReserve: TOnLccServerChange read FOnListenerManageReserve write FOnListenerManageReserve;
    property OnListenerManageRelease: TOnLccServerChange read FOnListenerManageRelease write FOnListenerManageRelease;


    procedure ProcessMessageLCC(ALccNode: TObject; SourceMessage: TLccMessage); virtual;
  end;

implementation

uses
  lcc_alias_server,
  lcc_node;

{ TLccTractionObjectTLccTractionObject }

function TLccTractionObject.GetFunctions(Index: Integer): Word;
begin
  if (Index > -1) and (Index < MAX_FUNCTIONS) then
    Result := FFunctions[Index]
  else
    Result := 0
end;

procedure TLccTractionObject.SetFunctions(Index: Integer; AValue: Word);
begin
   if (Index > -1) and (Index < MAX_FUNCTIONS) then
     FFunctions[Index] := AValue
end;

constructor TLccTractionObject.Create;
begin
  FSNIP := TLccSNIPObject.Create;
  FTrainSNIP := TLccTrainSNIPObject.Create;
  FController := TLccTractionControllerObject.Create;
end;

destructor TLccTractionObject.Destroy;
begin
  FreeAndNil(FSNIP);
  FreeAndNil(FTrainSNIP);
  FreeAndNil(FController);
  inherited Destroy;
end;

{ TLccTractionServer }

procedure TLccTractionServer.HandleTractionSimpleTrainInfoReply(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
  TrainVersion: Byte;
  TrainRoadName,
  TrainClass,
  TrainRoadNumber,
  TrainName,
  TrainManufacturer,
  TrainOwner: string;
begin
  TractionObject := Find(SourceMessage.SourceID);
  if Assigned(TractionObject) then
  begin
    TrainVersion := 0;
    TrainRoadName := '';
    TrainClass := '';
    TrainRoadNumber := '';
    TrainName := '';
    TrainManufacturer := '';
    TrainOwner := '';
    SourceMessage.ExtractSimpleTrainNodeIdentInfo(TrainVersion, TrainRoadName, TrainClass, TrainRoadNumber, TrainName, TrainManufacturer, TrainOwner);
    TractionObject.TrainSNIP.Manufacturer := TrainManufacturer;
    TractionObject.TrainSNIP.Owner := TrainOwner;
    TractionObject.TrainSNIP.Roadname := TrainRoadName;
    TractionObject.TrainSNIP.RoadNumber := TrainRoadNumber;
    TractionObject.TrainSNIP.TrainClass := TrainClass;
    TractionObject.TrainSNIP.TrainName := TrainName;
    TractionObject.TrainSNIP.Version := TrainVersion;
    TractionObject.TrainSNIP.Valid := True;
    DoTrainSNIPChange(ALccNode, TractionObject);
  end;
end;

procedure TLccTractionServer.DoRegisterChange(LccNode: TObject; TractionObject: TLccTractionObject; IsRegistered: Boolean);
begin
  if Assigned(OnRegisterChange) then
    OnRegisterChange(Self, LccNode, TractionObject, IsRegistered);
end;

procedure TLccTractionServer.DoSNIPChange(LccNode: TObject; TractionObject: TLccTractionObject);
begin
  if Assigned(OnSNIPChange) then
    OnSNIPChange(Self, LccNode, TractionObject);
end;

procedure TLccTractionServer.DoTrainSNIPChange(LccNode: TObject; TractionObject: TLccTractionObject);
begin
  if Assigned(OnTrainSNIPChange) then
    OnTrainSNIPChange(Self, LccNode, TractionObject);
end;

procedure TLccTractionServer.DoSpeedChange(LccNode: TObject; TractionObject: TLccTractionObject);
begin
  if Assigned(OnSpeedChange) then
    OnSpeedChange(Self, LccNode, TractionObject);
end;

procedure TLccTractionServer.DoFunctionChange(LccNode: TObject; TractionObject: TLccTractionObject);
begin
  if Assigned(OnFunctionChange) then
    OnFunctionChange(Self, LccNode, TractionObject);
end;

procedure TLccTractionServer.DoEmergencyStopChange(LccNode: TObject; TractionObject: TLccTractionObject);
begin
  if Assigned(OnEmergencyStopChange) then
    OnEmergencyStopChange(Self, LccNode, TractionObject);
end;

procedure TLccTractionServer.DoListenerAttach(LccNode: TObject; TractionObject: TLccTractionObject);
begin
  if Assigned(OnListenerAttach) then
    OnListenerAttach(Self, LccNode, TractionObject);
end;

procedure TLccTractionServer.DoListenerDetach(LccNode: TObject; TractionObject: TLccTractionObject);
begin
  if Assigned(OnListenerDetach) then
    OnListenerDetach(Self, LccNode, TractionObject);
end;

procedure TLccTractionServer.DoListenerManageReserve(LccNode: TObject; TractionObject: TLccTractionObject);
begin
  if Assigned(OnListenerManageReserve) then
    OnListenerManageReserve(Self, LccNode, TractionObject);
end;

procedure TLccTractionServer.DoListenerManageRelease(LccNode: TObject; TractionObject: TLccTractionObject);
begin
  if Assigned(OnListenerManageRelease) then
    OnListenerManageRelease(Self, LccNode, TractionObject);
end;

procedure TLccTractionServer.HandleProducerIdentifiedAll(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  LccNode: TLccNode;
  LocalAliasMapping: TLccAliasMapping;
  LocalTractionNodeID: TNodeID;
  LocalTractionObject: TLccTractionObject;
begin
  if SourceMessage.IsEqualEventID(EVENT_IS_TRAIN) then
  begin
    if Assigned(ALccNode) then
    begin
      LccNode := ALccNode as TLccNode;
      if LccNode.GridConnect then
      begin
        LocalAliasMapping := AliasServer.FindMapping(SourceMessage.CAN.SourceAlias);
        Assert(Assigned(LocalAliasMapping), 'Could not Assign Node AliasMapping in TLccTrainDatabaseNode.ProcessMessageLCC');
        LocalTractionNodeID := LocalAliasMapping.NodeID;
      end else
        LocalTractionNodeID := SourceMessage.SourceID;

      LocalTractionObject := Find(LocalTractionNodeID);
      if not Assigned(LocalTractionObject) then
      begin
        LocalTractionObject := Add(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
        DoRegisterChange(ALccNode, LocalTractionObject, True);
      end;

      // Get some information about this train
      WorkerMessage.LoadSimpleNodeIdentInfoRequest(LccNode.NodeID, LccNode.AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
      LccNode.SendMessageFunc(Self, WorkerMessage);
      WorkerMessage.LoadSimpleTrainNodeIdentInfoRequest(LccNode.NodeID, LccNode.AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
      LccNode.SendMessageFunc(Self, WorkerMessage);
      WorkerMessage.LoadTractionListenerQueryCount(LccNode.NodeID, LccNode.AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
      LccNode.SendMessageFunc(Self, WorkerMessage);
    end;
  end
end;

procedure TLccTractionServer.HandleSimpleNodeInfoReply(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
  Version,
  UserVersion: byte;
  Manufacturer,
  Model,
  HardwareVersion,
  SoftwareVersion,
  UserName,
  UserDescription: string;
begin
  TractionObject := Find(SourceMessage.SourceID);
  if Assigned(TractionObject) then
  begin
    Version := 0;
    UserVersion := 0;
    Manufacturer := '';
    Model := '';
    HardwareVersion := '';
    SoftwareVersion := '';
    UserName := '';
    UserDescription := '';
    SourceMessage.ExtractSimpleNodeIdentInfo(Version, Manufacturer, Model, HardwareVersion, SoftwareVersion, UserVersion, UserName, UserDescription);
    TractionObject.SNIP.Version := Version;
    TractionObject.SNIP.Manufacturer := Manufacturer;
    TractionObject.SNIP.Model := Model;
    TractionObject.SNIP.HardwareVersion := HardwareVersion;
    TractionObject.SNIP.SoftwareVersion := SoftwareVersion;
    TractionObject.SNIP.UserVersion := UserVersion;
    TractionObject.SNIP.UserName := UserName;
    TractionObject.SNIP.UserDescription := UserDescription;
    TractionObject.SNIP.Valid := True;
    DoSNIPChange(ALccNode, TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionControllerAssign(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  // TODO: This is complicated if the train asks for permission...
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin
    TractionObject.Controller.Flags := SourceMessage.TractionExtractControllerAssignResult;
    TractionObject.Controller.Node := SourceMessage.TractionExtractControllerNodeID;
  end;
end;

procedure TLccTractionServer.HandleTractionControllerRelease(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  // TODO: This is complicated if the train asks for permission...
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin

  end;
end;

procedure TLccTractionServer.HandleTractionControllerQuery(ALccNode: TObject; var SourceMessage: TLccMessage);
begin
   // Do Nothing: Just a query won't change the database
end;

procedure TLccTractionServer.HandleTractionControllerChangingNotify(ALccNode: TObject; var SourceMessage: TLccMessage);
begin
  // TODO: This is complicated if the train asks for permission...
end;

procedure TLccTractionServer.HandleTractionControllerChangedNotify(ALccNode: TObject; var SourceMessage: TLccMessage);
begin
  // TODO: This is complicated if the train asks for permission...
end;

procedure TLccTractionServer.HandleTractionEStop(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin
    TractionObject.FSpeedSet := 0;
    TractionObject.FSpeedCommanded := 0;
    TractionObject.FSpeedActual :=  0;
    TractionObject.FSpeedStatusEmergencyStop := True;
    DoEmergencyStopChange(ALccNode, TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionListenerAttach(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin
    DoListenerAttach(ALccNode, TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionListenerDetach(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin
    DoListenerDetach(ALccNode, TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionListenerQuery(ALccNode: TObject; var SourceMessage: TLccMessage);
begin
  // Do Nothing: Just a query won't change the database
end;

procedure TLccTractionServer.HandleTractionManageReserve(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin
    DoListenerManageReserve(ALccNode, TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionManageRelease(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin
    DoListenerManageRelease(ALccNode, TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionSetSpeed(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin
    TractionObject.FSpeedSet := SourceMessage.TractionExtractSetSpeed;
    TractionObject.FSpeedCommanded := SourceMessage.TractionExtractCommandedSpeed;
    TractionObject.FSpeedActual :=  SourceMessage.TractionExtractActualSpeed;
    TractionObject.FSpeedStatusEmergencyStop := SourceMessage.TractionExtractSpeedStatus and TRACTION_SPEED_STATUS_E_STOP = TRACTION_SPEED_STATUS_E_STOP;
    DoSpeedChange(ALccNode, TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionSetFunction(ALccNode: TObject; var SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin
    TractionObject.Functions[SourceMessage.TractionExtractFunctionAddress] := SourceMessage.TractionExtractFunctionValue;
    DoFunctionChange(ALccNode, TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionQuerySpeed(ALccNode: TObject; var SourceMessage: TLccMessage);
begin
  // Do Nothing: Just a query won't change the database
end;

procedure TLccTractionServer.HandleTractionQueryFunction(ALccNode: TObject; var SourceMessage: TLccMessage);
begin
  // Do Nothing: Just a query won't change the database
end;

constructor TLccTractionServer.Create;
begin
  {$IFDEF DELPHI}
  FTractionList := TObjectList<TLccTractionObject>.Create(False);
  {$ELSE}
  FTractionList := TObjectList.Create(False);
  {$ENDIF}
  FWorkerMessage := TLccMessage.Create;
end;

destructor TLccTractionServer.Destroy;
begin
  FreeAndNil(FTractionList);
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

function TLccTractionServer.Add(NewNodeID: TNodeID; NewAlias: Word): TLccTractionObject;
begin
  Result := TLccTractionObject.Create;
  TractionList.Add(Result);
  Result.NodeAlias := NewAlias;
  Result.NodeID := NewNodeID;
end;

function TLccTractionServer.Remove(TestNodeID: TNodeID): TLccTractionObject;
var
  TractionObject: TLccTractionObject;
begin
  Result := nil;
  TractionObject := Find(TestNodeID);
  if Assigned(TractionObject) then
  begin
    TractionList.Remove(TractionObject);
    Result := TractionObject;
  end;
end;

function TLccTractionServer.Find(TestNodeID: TNodeID): TLccTractionObject;
var
  i: Integer;
  TractionObject: TLccTractionObject;
begin
  Result := nil;
  for i := 0 to TractionList.Count - 1 do
  begin
    TractionObject := TractionList.Items[i] as TLccTractionObject;
    if EqualNodeID(TestNodeID, TractionObject.NodeID, False) then
    begin
      Result := TractionObject;
      Break
    end;
  end;
end;

procedure TLccTractionServer.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to TractionList.Count - 1 do
      TractionList[i].Free;
  finally
    TractionList.Clear;
  end;

end;

procedure TLccTractionServer.ProcessMessageLCC(ALccNode: TObject; SourceMessage: TLccMessage);
begin
  if Enabled then
  begin
    case SourceMessage.MTI of
      MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY : HandleTractionSimpleTrainInfoReply(ALccNode, SourceMessage);
      MTI_SIMPLE_NODE_INFO_REPLY           : HandleSimpleNodeInfoReply(ALccNode, SourceMessage);
      MTI_PRODUCER_IDENTIFIED_CLEAR,
      MTI_PRODUCER_IDENTIFIED_SET,
      MTI_PRODUCER_IDENTIFIED_UNKNOWN      : HandleProducerIdentifiedAll(ALccNode, SourceMessage);
      MTI_TRACTION_REQUEST :
        begin
          case SourceMessage.DataArray[0] of
            TRACTION_SET_SPEED_DIR       : HandleTractionSetSpeed(ALccNode, SourceMessage);
            TRACTION_SET_FUNCTION        : HandleTractionSetFunction(ALccNode, SourceMessage);
            TRACTION_SET_E_STOP          : HandleTractionEStop(ALccNode, SourceMessage);
            TRACTION_QUERY_SPEED         : HandleTractionQuerySpeed(ALccNode, SourceMessage);
            TRACTION_QUERY_FUNCTION      : HandleTractionQueryFunction(ALccNode, SourceMessage);
            TRACTION_CONTROLLER_CONFIG :
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_CONTROLLER_CONFIG_ASSIGN  : HandleTractionControllerAssign(ALccNode, SourceMessage);
                  TRACTION_CONTROLLER_CONFIG_RELEASE : HandleTractionControllerRelease(ALccNode, SourceMessage);
                  TRACTION_CONTROLLER_CONFIG_QUERY   : HandleTractionControllerQuery(ALccNode, SourceMessage);
                  TRACTION_CONTROLLER_CONFIG_CHANGING_NOTIFY : HandleTractionControllerChangingNotify(ALccNode, SourceMessage);
                end
              end;
            TRACTION_LISTENER :
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_LISTENER_ATTACH : HandleTractionListenerAttach(ALccNode, SourceMessage);
                  TRACTION_LISTENER_DETACH : HandleTractionListenerDetach(ALccNode, SourceMessage);
                  TRACTION_LISTENER_QUERY  : HandleTractionListenerQuery(ALccNode, SourceMessage);
                end;
              end;
            TRACTION_MANAGE :
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_MANAGE_RESERVE : HandleTractionManageReserve(ALccNode, SourceMessage);
                  TRACTION_MANAGE_RELEASE : HandleTractionManageRelease(ALccNode, SourceMessage);
                end
              end;
          end;
        end;
    end;
  end;
end;

end.

