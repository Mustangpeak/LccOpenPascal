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

  { TLccTractionControllerObject }

  TLccTractionControllerObject = class
  private
    FRequestingNodeID: TNodeID;
    FRequestResult: TLccTrainControllerResult;
  public
    property RequestingNodeID: TNodeID read FRequestingNodeID write FRequestingNodeID;
    property RequestResult: TLccTrainControllerResult read FRequestResult write FRequestResult;
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

  { TLccTractionServer }

  TLccTractionServer = class
  private
    {$IFDEF DELPHI}
    FTractionList: TObjectList<TLccTractionObject>;
    {$ELSE}
    FTractionList: TObjectList;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function Add(NewNodeID: TNodeID; NewAlias: Word): TLccTractionObject;
    function Remove(TestNodeID: TNodeID): TLccTractionObject;
    function Find(TestNodeID: TNodeID): TLccTractionObject;
    function UpdateSNIP(AMessage: TLccMessage): TLccTractionObject;
    function UpdateTrainSNIP(AMessage: TLccMessage): TLccTractionObject;
    function UpdateListenerCount(AMessage: TLccMessage): TLccTractionObject;
    function UpdateSpeed(AMessage: TLccMessage): TLccTractionObject;
    function UpdateFunction(AMessage: TLccMessage): TLccTractionObject;
    function UpdateControllerAssign(AMessage: TLccMessage): TLccTractionObject;
    function UpdateControllerQuery(AMessage: TLccMessage): TLccTractionObject;

    procedure Clear;

    {$IFDEF DELPHI}
    property TractionList: TObjectList<TLccTractionObject> read FTractionList write FTractionList;
    {$ELSE}
    property TractionList: TObjectList read FTractionList write FTractionList;
    {$ENDIF}
  end;

implementation

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

constructor TLccTractionServer.Create;
begin
  {$IFDEF DELPHI}
  FTractionList := TObjectList<TLccTractionObject>.Create(False);
  {$ELSE}
  FTractionList := TObjectList.Create(False);
  {$ENDIF}
end;

destructor TLccTractionServer.Destroy;
begin
  FreeAndNil(FTractionList);
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

function TLccTractionServer.UpdateSNIP(AMessage: TLccMessage): TLccTractionObject;
var
  Version,
  UserVersion: byte;
  Manufacturer,
  Model,
  HardwareVersion,
  SoftwareVersion,
  UserName,
  UserDescription: string;
begin
  Result := Find(AMessage.SourceID);
  if Assigned(Result) then
  begin
    Version := 0;
    UserVersion := 0;
    Manufacturer := '';
    Model := '';
    HardwareVersion := '';
    SoftwareVersion := '';
    UserName := '';
    UserDescription := '';
    AMessage.ExtractSimpleNodeIdentInfo(Version, Manufacturer, Model, HardwareVersion, SoftwareVersion, UserVersion, UserName, UserDescription);
    Result.SNIP.Version := Version;
    Result.SNIP.Manufacturer := Manufacturer;
    Result.SNIP.Model := Model;
    Result.SNIP.HardwareVersion := HardwareVersion;
    Result.SNIP.SoftwareVersion := SoftwareVersion;
    Result.SNIP.UserVersion := UserVersion;
    Result.SNIP.UserName := UserName;
    Result.SNIP.UserDescription := UserDescription;
    Result.SNIP.Valid := True;
  end;
end;

function TLccTractionServer.UpdateTrainSNIP(AMessage: TLccMessage): TLccTractionObject;
var
  TrainVersion: Byte;
  TrainRoadName,
  TrainClass,
  TrainRoadNumber,
  TrainName,
  TrainManufacturer,
  TrainOwner: string;
begin
  Result := Find(AMessage.SourceID);
  if Assigned(Result) then
  begin
    TrainVersion := 0;
    TrainRoadName := '';
    TrainClass := '';
    TrainRoadNumber := '';
    TrainName := '';
    TrainManufacturer := '';
    TrainOwner := '';
    AMessage.ExtractSimpleTrainNodeIdentInfo(TrainVersion, TrainRoadName, TrainClass, TrainRoadNumber, TrainName, TrainManufacturer, TrainOwner);
    Result.TrainSNIP.Manufacturer := TrainManufacturer;
    Result.TrainSNIP.Owner := TrainOwner;
    Result.TrainSNIP.Roadname := TrainRoadName;
    Result.TrainSNIP.RoadNumber := TrainRoadNumber;
    Result.TrainSNIP.TrainClass := TrainClass;
    Result.TrainSNIP.TrainName := TrainName;
    Result.TrainSNIP.Version := TrainVersion;
    Result.TrainSNIP.Valid := True;
  end;
end;

function TLccTractionServer.UpdateListenerCount(AMessage: TLccMessage): TLccTractionObject;
begin
  Result := Find(AMessage.SourceID);
  if Assigned(Result) then
  begin

  end;
end;

function TLccTractionServer.UpdateSpeed(AMessage: TLccMessage): TLccTractionObject;
begin
  Result := Find(AMessage.SourceID);
  if Assigned(Result) then
  begin
    Result.FSpeedSet := AMessage.TractionExtractSetSpeed;
    Result.FSpeedCommanded := AMessage.TractionExtractCommandedSpeed;
    Result.FSpeedActual :=  AMessage.TractionExtractActualSpeed;
    Result.FSpeedStatusEmergencyStop := AMessage.TractionExtractSpeedStatus and TRACTION_SPEED_STATUS_E_STOP = TRACTION_SPEED_STATUS_E_STOP;
  end;
end;

function TLccTractionServer.UpdateFunction(AMessage: TLccMessage): TLccTractionObject;
begin
  Result := Find(AMessage.SourceID);
  if Assigned(Result) then
    Result.Functions[AMessage.TractionExtractFunctionAddress] := AMessage.TractionExtractFunctionValue;
end;

function TLccTractionServer.UpdateControllerAssign(AMessage: TLccMessage): TLccTractionObject;
begin
  Result := Find(AMessage.SourceID);
  if Assigned(Result) then
  begin
    case AMessage.TractionExtractControllerAssignResult of
      TRACTION_CONTROLLER_CONFIG_REPLY_OK :
        begin
          Result.Controller.RequestResult := tcr_Ok;
        end;
      TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :
        begin
          Result.Controller.RequestResult := tcr_ControllerRefused;
        end;
      TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
        begin
          Result.Controller.RequestResult := tcr_TrainRefused;
        end;
    end;
  end;
end;

function TLccTractionServer.UpdateControllerQuery(AMessage: TLccMessage): TLccTractionObject;
begin
  Result := Find(AMessage.SourceID);
  if Assigned(Result) then
    AMessage.ExtractDataBytesAsNodeID(3, Result.Controller.FRequestingNodeID);
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

end.

