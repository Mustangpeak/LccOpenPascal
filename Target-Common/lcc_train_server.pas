unit lcc_train_server;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF LCC_FPC}
    contnrs,
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

    procedure Clear;
  end;


  { TLccTractionObject }

  TLccTractionObject = class
  private
    FController: TLccTractionControllerObject;
    FNodeAlias: Word;
    FNodeID: TNodeID;
    FServer: TLccTractionServer;
    FSNIP: TLccSNIPObject;
    FSpeedActual: THalfFloat;
    FSpeedCommanded: THalfFloat;
    FSpeedSet: THalfFloat;
    FFunctions: TLccFunctions;
    FSpeedStatusEmergencyStop: Boolean;
    FTrainSNIP: TLccTrainSNIPObject;
    FNodeCDI: TLccNodeCDI;
    function GetFunctions(Index: Integer): Word;
    procedure SetFunctions(Index: Integer; AValue: Word);
  public
    property NodeCDI: TLccNodeCDI read FNodeCDI write FNodeCDI;
    property NodeID: TNodeID read FNodeID write FNodeID;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;
    property SpeedSet: THalfFloat read FSpeedSet;
    property SpeedCommanded: THalfFloat read FSpeedCommanded;
    property SpeedActual: THalfFloat read FSpeedActual;
    property SpeedStatusEmergencyStop: Boolean read FSpeedStatusEmergencyStop;
    property SNIP: TLccSNIPObject read FSNIP;
    property TrainSNIP: TLccTrainSNIPObject read FTrainSNIP;
    property Controller: TLccTractionControllerObject read FController write FController;    // Is it even possible to keep this valid across network segments?
    property Server: TLccTractionServer read FServer;
    constructor Create(AServer: TLccTractionServer);
    destructor Destroy; override;
    function DisplayName: string;
  end;

  TOnLccServerRegisterChange = procedure(TractionObject: TLccTractionObject; IsRegistered: Boolean) of object;
  TOnLccServerChange = procedure(TractionObject: TLccTractionObject) of object;
  TOnLccServerChangeWithMessage = procedure(TractionObject: TLccTractionObject; SourceMessage: TLccMessage) of object;


  { TLccTractionServer }

  TLccTractionServer = class
  private
    FAutoGatherInformation: Boolean;
    FEnabled: Boolean;
    FOnEmergencyStopChange: TOnLccServerChange;
    FOnFunctionChange: TOnLccServerChange;
    FOnListenerAttach: TOnLccServerChangeWithMessage;
    FOnListenerDetach: TOnLccServerChangeWithMessage;
    FOnListenerManageRelease: TOnLccServerChangeWithMessage;
    FOnListenerManageReserve: TOnLccServerChangeWithMessage;
    FOnListenerQuery: TOnLccServerChangeWithMessage;
    FOnRegisterChange: TOnLccServerRegisterChange;
    FOnSNIPChange: TOnLccServerChange;
    FOnSpeedChange: TOnLccServerChange;
    FOnTrainSNIPChange: TOnLccServerChange;
    {$IFDEF LCC_DELPHI}
    FList: TObjectList<TLccTractionObject>;
    {$ELSE}
    FList: TObjectList;
    {$ENDIF}
    FWorkerMessage: TLccMessage;
    function GetItem(Index: Integer): TLccTractionObject;
  protected
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    procedure DoRegisterChange(TractionObject: TLccTractionObject; IsRegistered: Boolean);
    procedure DoSNIPChange(TractionObject: TLccTractionObject);
    procedure DoTrainSNIPChange(TractionObject: TLccTractionObject);
    procedure DoSpeedChange(TractionObject: TLccTractionObject);
    procedure DoFunctionChange(TractionObject: TLccTractionObject);
    procedure DoEmergencyStopChange(TractionObject: TLccTractionObject);
    procedure DoListenerAttach(TractionObject: TLccTractionObject; SourceMessage: TLccMessage);
    procedure DoListenerDetach(TractionObject: TLccTractionObject; SourceMessage: TLccMessage);
    procedure DoListenerQuery(TractionObject: TLccTractionObject; SourceMessage: TLccMessage);
    procedure DoListenerManageReserve(TractionObject: TLccTractionObject; SourceMessage: TLccMessage);
    procedure DoListenerManageRelease(TractionObject: TLccTractionObject; SourceMessage: TLccMessage);

    procedure HandleProducerIdentifiedAll(ALccNode: TObject; SourceMessage: TLccMessage);
    procedure HandleSimpleNodeInfoReply( SourceMessage: TLccMessage);
    procedure HandleTractionControllerAssign(SourceMessage: TLccMessage);
    procedure HandleTractionControllerRelease(SourceMessage: TLccMessage);
    procedure HandleTractionControllerQuery(SourceMessage: TLccMessage);
    procedure HandleTractionControllerChangedNotify(SourceMessage: TLccMessage);
    procedure HandleTractionEStop(SourceMessage: TLccMessage);
    procedure HandleTractionListenerAttach(SourceMessage: TLccMessage);
    procedure HandleTractionListenerDetach(SourceMessage: TLccMessage);
    procedure HandleTractionListenerQuery(SourceMessage: TLccMessage);
    procedure HandleTractionManageReserve(SourceMessage: TLccMessage);
    procedure HandleTractionManageRelease(SourceMessage: TLccMessage);
    procedure HandleTractionSetSpeed(SourceMessage: TLccMessage);
    procedure HandleTractionSetFunction(SourceMessage: TLccMessage);
    procedure HandleTractionSimpleTrainInfoReply(SourceMessage: TLccMessage);
    procedure HandleTractionQuerySpeed(SourceMessage: TLccMessage);
    procedure HandleTractionQueryFunction(SourceMessage: TLccMessage);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(NewNodeID: TNodeID; NewAlias: Word): TLccTractionObject;
    function Remove(TestNodeID: TNodeID): TLccTractionObject;
    function Find(TestNodeID: TNodeID): TLccTractionObject;
    procedure Clear;
    {$IFDEF LCC_DELPHI}
    property List: TObjectList<TLccTractionObject> read FList write FList;
    {$ELSE}
    property List: TObjectList read FList write FList;
    {$ENDIF}
    property AutoGatherInformation: Boolean read FAutoGatherInformation write FAutoGatherInformation;   // When a train is detected send out SNIP/TRAINSNIP, etc messages
    property Enabled: Boolean read FEnabled write FEnabled;
    property Item[Index: Integer]: TLccTractionObject read GetItem; default;
    property OnRegisterChange: TOnLccServerRegisterChange read FOnRegisterChange write FOnRegisterChange;
    property OnSNIPChange: TOnLccServerChange read FOnSNIPChange write FOnSNIPChange;
    property OnTrainSNIPChange: TOnLccServerChange read FOnTrainSNIPChange write FOnTrainSNIPChange;
    property OnSpeedChange: TOnLccServerChange read FOnSpeedChange write FOnSpeedChange;
    property OnFunctionChange: TOnLccServerChange read FOnFunctionChange write FOnFunctionChange;
    property OnEmergencyStopChange: TOnLccServerChange read FOnEmergencyStopChange write FOnEmergencyStopChange;
    property OnListenerAttach: TOnLccServerChangeWithMessage read FOnListenerAttach write FOnListenerAttach;  // TractionObject cound be nil as you don't have to be a train to be a listener
    property OnListenerDetach: TOnLccServerChangeWithMessage read FOnListenerDetach write FOnListenerDetach;  // TractionObject cound be nil as you don't have to be a train to be a listener
    property OnListenerQuery: TOnLccServerChangeWithMessage read FOnListenerQuery write FOnListenerQuery;     // TractionObject cound be nil as you don't have to be a train to be a listener
    property OnListenerManageReserve: TOnLccServerChangeWithMessage read FOnListenerManageReserve write FOnListenerManageReserve;  // TractionObject cound be nil as you don't have to be a train to be a Manage
    property OnListenerManageRelease: TOnLccServerChangeWithMessage read FOnListenerManageRelease write FOnListenerManageRelease;  // TractionObject cound be nil as you don't have to be a train to be a Manage

    procedure ProcessMessageLCC(ALccNode: TObject; SourceMessage: TLccMessage); virtual;
  end;


implementation

uses
  lcc_alias_server,
  lcc_node;

{ TLccTractionControllerObject }

procedure TLccTractionControllerObject.Clear;
begin
  FNodeID := NULL_NODE_ID;
  FFlags := 0;
end;


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

constructor TLccTractionObject.Create(AServer: TLccTractionServer);
begin
  FServer := AServer;
  FSNIP := TLccSNIPObject.Create;
  FTrainSNIP := TLccTrainSNIPObject.Create;
  FController := TLccTractionControllerObject.Create;
  FNodeCDI := TLccNodeCDI.Create;
end;

destructor TLccTractionObject.Destroy;
begin
  FreeAndNil(FSNIP);
  FreeAndNil(FTrainSNIP);
  FreeAndNil(FController);
  FreeAndNil(FNodeCDI);
  inherited Destroy;
end;

function TLccTractionObject.DisplayName: string;
begin
  Result := TrainSNIP.TrainName;
  if Result = '' then
    Result := SNIP.UserName;
  if Result = '' then
    Result := NodeIDToString(NodeID, True);
end;

{ TLccTractionServer }

procedure TLccTractionServer.HandleTractionSimpleTrainInfoReply(SourceMessage: TLccMessage);
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
    DoTrainSNIPChange(TractionObject);
  end;
end;

function TLccTractionServer.GetItem(Index: Integer): TLccTractionObject;
begin
  Result := List[Index] as TLccTractionObject;
end;

procedure TLccTractionServer.DoRegisterChange(TractionObject: TLccTractionObject; IsRegistered: Boolean);
begin
  if Assigned(OnRegisterChange) then
    OnRegisterChange(TractionObject, IsRegistered);
end;

procedure TLccTractionServer.DoSNIPChange(TractionObject: TLccTractionObject);
begin
  if Assigned(OnSNIPChange) then
    OnSNIPChange(TractionObject);
end;

procedure TLccTractionServer.DoTrainSNIPChange(TractionObject: TLccTractionObject);
begin
  if Assigned(OnTrainSNIPChange) then
    OnTrainSNIPChange(TractionObject);
end;

procedure TLccTractionServer.DoSpeedChange(TractionObject: TLccTractionObject);
begin
  if Assigned(OnSpeedChange) then
    OnSpeedChange(TractionObject);
end;

procedure TLccTractionServer.DoFunctionChange(TractionObject: TLccTractionObject);
begin
  if Assigned(OnFunctionChange) then
    OnFunctionChange(TractionObject);
end;

procedure TLccTractionServer.DoEmergencyStopChange(TractionObject: TLccTractionObject);
begin
  if Assigned(OnEmergencyStopChange) then
    OnEmergencyStopChange(TractionObject);
end;

procedure TLccTractionServer.DoListenerAttach(TractionObject: TLccTractionObject; SourceMessage: TLccMessage);
begin
  if Assigned(OnListenerAttach) then
    OnListenerAttach(TractionObject, SourceMessage);
end;

procedure TLccTractionServer.DoListenerDetach(TractionObject: TLccTractionObject; SourceMessage: TLccMessage);
begin
  if Assigned(OnListenerDetach) then
    OnListenerDetach(TractionObject, SourceMessage);
end;

procedure TLccTractionServer.DoListenerQuery(TractionObject: TLccTractionObject; SourceMessage: TLccMessage);
begin
  if Assigned(OnListenerQuery) then
    OnListenerManageReserve(TractionObject, SourceMessage);
end;

procedure TLccTractionServer.DoListenerManageReserve(TractionObject: TLccTractionObject; SourceMessage: TLccMessage);
begin
  if Assigned(OnListenerManageReserve) then
    OnListenerManageReserve(TractionObject, SourceMessage);
end;

procedure TLccTractionServer.DoListenerManageRelease(TractionObject: TLccTractionObject; SourceMessage: TLccMessage);
begin
  if Assigned(OnListenerManageRelease) then
    OnListenerManageRelease(TractionObject, SourceMessage);
end;

procedure TLccTractionServer.HandleProducerIdentifiedAll(ALccNode: TObject; SourceMessage: TLccMessage);
var
  LocalAliasMapping: TLccAliasMapping;
  LocalTractionNodeID: TNodeID;
  LocalTractionObject: TLccTractionObject;
begin
 { if SourceMessage.TractionIsSearchEvent then
  begin
  end else
  if SourceMessage.TractionIsTrainEvent then
  begin
    if GridConnect then
    begin
      LocalAliasMapping := AliasServer.FindMapping(SourceMessage.SourceAlias);
      LocalTractionNodeID := LocalAliasMapping.NodeID;
    end else
      LocalTractionNodeID := SourceMessage.SourceID;
    LocalTractionObject := Find(LocalTractionNodeID);
    if not Assigned(LocalTractionObject) then
    begin
      LocalTractionObject := Add(SourceMessage.SourceID, SourceMessage.SourceAlias);
      DoRegisterChange(LocalTractionObject, True);
    end;
    if AutoGatherInformation then
    begin
    // Get some information about this train
      WorkerMessage.LoadSimpleNodeIdentInfoRequest(TLccNode( ALccNode).NodeID, TLccNode( ALccNode).AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias);
      TLccNode( ALccNode).SendMessage(WorkerMessage);
      WorkerMessage.LoadSimpleTrainNodeIdentInfoRequest(TLccNode( ALccNode).NodeID, TLccNode( ALccNode).AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias);
      TLccNode( ALccNode).SendMessage(WorkerMessage);
      WorkerMessage.LoadTractionListenerQueryCount(TLccNode( ALccNode).NodeID, TLccNode( ALccNode).AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias);
      TLccNode( ALccNode).SendMessage(WorkerMessage);
    end;
  end   }
end;

procedure TLccTractionServer.HandleSimpleNodeInfoReply(SourceMessage: TLccMessage);
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
    DoSNIPChange(TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionControllerAssign(SourceMessage: TLccMessage);
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

procedure TLccTractionServer.HandleTractionControllerRelease(SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  // TODO: This is complicated if the train asks for permission...
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin
  end;
end;

procedure TLccTractionServer.HandleTractionControllerQuery(SourceMessage: TLccMessage);
begin
   // Do Nothing: Just a query won't change the database
end;


procedure TLccTractionServer.HandleTractionControllerChangedNotify(SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  // TODO: This is complicated if the train asks for permission...
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin
    TractionObject.Controller.Clear;  // Taken by another Controller
  end;
end;

procedure TLccTractionServer.HandleTractionEStop(SourceMessage: TLccMessage);
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
    DoEmergencyStopChange(TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionListenerAttach(SourceMessage: TLccMessage);
begin
  // User can just read the Listener property in the LccNode (only works with internal trains nodes)
  // A non Train can be a Listener so pass the raw message too
  DoListenerAttach(Find(SourceMessage.DestID), SourceMessage);
end;

procedure TLccTractionServer.HandleTractionListenerDetach(SourceMessage: TLccMessage);
begin
  // User can just read the Listener property in the LccNode (only works with internal trains nodes)
  // A non Train can be a Listener so pass the raw message too
  DoListenerDetach(Find(SourceMessage.DestID), SourceMessage);
end;

procedure TLccTractionServer.HandleTractionListenerQuery(SourceMessage: TLccMessage);
begin
  // User can just read the Listener property in the LccNode (only works with internal trains nodes)
  // A non Train can be a Listener so pass the raw message too
  DoListenerQuery(Find(SourceMessage.DestID), SourceMessage);
end;

procedure TLccTractionServer.HandleTractionManageReserve(SourceMessage: TLccMessage);
begin
  // User can just read the Listener property in the LccNode (only works with internal trains nodes)
  // A non Train can Manage too so pass the raw message too
  DoListenerManageReserve(Find(SourceMessage.DestID), SourceMessage);
end;

procedure TLccTractionServer.HandleTractionManageRelease(SourceMessage: TLccMessage);
begin
  // User can just read the Listener property in the LccNode (only works with internal trains nodes)
  // A non Train can Manage too so pass the raw message too
  DoListenerManageRelease(Find(SourceMessage.DestID), SourceMessage)
end;

procedure TLccTractionServer.HandleTractionSetSpeed(SourceMessage: TLccMessage);
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
    DoSpeedChange(TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionSetFunction(SourceMessage: TLccMessage);
var
  TractionObject: TLccTractionObject;
begin
  TractionObject := Find(SourceMessage.DestID);
  if Assigned(TractionObject) then
  begin
    TractionObject.Functions[SourceMessage.TractionExtractFunctionAddress] := SourceMessage.TractionExtractFunctionValue;
    DoFunctionChange(TractionObject);
  end;
end;

procedure TLccTractionServer.HandleTractionQuerySpeed(SourceMessage: TLccMessage);
begin
  // Do Nothing: Just a query won't change the database
end;

procedure TLccTractionServer.HandleTractionQueryFunction(SourceMessage: TLccMessage);
begin
  // Do Nothing: Just a query won't change the database
end;

constructor TLccTractionServer.Create;
begin
  {$IFDEF LCC_DELPHI}
  FList := TObjectList<TLccTractionObject>.Create(False);
  {$ELSE}
  FList := TObjectList.Create(False);
  {$ENDIF}
  FWorkerMessage := TLccMessage.Create;
 // FGridConnect := IsGridConnect;
end;

destructor TLccTractionServer.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

function TLccTractionServer.Add(NewNodeID: TNodeID; NewAlias: Word): TLccTractionObject;
begin
  Result := TLccTractionObject.Create(Self);
  List.Add(Result);
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
    List.Remove(TractionObject);
    Result := TractionObject;
  end;
end;

function TLccTractionServer.Find(TestNodeID: TNodeID): TLccTractionObject;
var
  i: Integer;
  TractionObject: TLccTractionObject;
begin
  Result := nil;
  for i := 0 to List.Count - 1 do
  begin
    TractionObject := List.Items[i] as TLccTractionObject;
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
    for i := 0 to List.Count - 1 do
      List[i].Free;
  finally
    List.Clear;
  end;
end;

procedure TLccTractionServer.ProcessMessageLCC(ALccNode: TObject; SourceMessage: TLccMessage);
begin
  if Enabled then
  begin
    case SourceMessage.MTI of
      MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY : HandleTractionSimpleTrainInfoReply(SourceMessage);
      MTI_SIMPLE_NODE_INFO_REPLY           : HandleSimpleNodeInfoReply(SourceMessage);
      MTI_PRODUCER_IDENTIFIED_CLEAR,
      MTI_PRODUCER_IDENTIFIED_SET,
      MTI_PRODUCER_IDENTIFIED_UNKNOWN      : HandleProducerIdentifiedAll(ALccNode, SourceMessage);
      MTI_TRACTION_REQUEST :
        begin
          case SourceMessage.DataArray[0] of
            TRACTION_SET_SPEED_DIR       : HandleTractionSetSpeed(SourceMessage);
            TRACTION_SET_FUNCTION        : HandleTractionSetFunction(SourceMessage);
            TRACTION_SET_E_STOP          : HandleTractionEStop(SourceMessage);
            TRACTION_QUERY_SPEED         : HandleTractionQuerySpeed(SourceMessage);
            TRACTION_QUERY_FUNCTION      : HandleTractionQueryFunction(SourceMessage);
            TRACTION_CONTROLLER_CONFIG :
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_CONTROLLER_CONFIG_ASSIGN  : HandleTractionControllerAssign(SourceMessage);
                  TRACTION_CONTROLLER_CONFIG_RELEASE : HandleTractionControllerRelease(SourceMessage);
                  TRACTION_CONTROLLER_CONFIG_QUERY   : HandleTractionControllerQuery(SourceMessage);
                  TRACTION_CONTROLLER_CONFIG_CHANGED_NOTIFY : HandleTractionControllerChangedNotify(SourceMessage);
                end
              end;
            TRACTION_LISTENER_CONFIG :
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_LISTENER_CONFIG_ATTACH : HandleTractionListenerAttach(SourceMessage);
                  TRACTION_LISTENER_CONFIG_DETACH : HandleTractionListenerDetach(SourceMessage);
                  TRACTION_LISTENER_CONFIG_QUERY  : HandleTractionListenerQuery(SourceMessage);
                end;
              end;
            TRACTION_MANAGE :
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_MANAGE_RESERVE : HandleTractionManageReserve(SourceMessage);
                  TRACTION_MANAGE_RELEASE : HandleTractionManageRelease(SourceMessage);
                end
              end;
          end;
        end;
      MTI_DATAGRAM :
        begin
          case SourceMessage.DataArray[0] of
            DATAGRAM_PROTOCOL_CONFIGURATION :
            begin
            end;
          end;
        end;
      end;
    end;
  end;

end.
