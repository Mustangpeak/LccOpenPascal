unit lcc_node_controller;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface


{.$DEFINE DISABLE_STATE_TIMEOUTS_FOR_DEBUG}

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
  lcc_utilities,
  lcc_node,
  lcc_defines,
  lcc_alias_server,

  lcc_node_messages;

const
  CDI_XML_CONTROLLER: string = (
  '<?xml version="1.0" encoding="utf-8"?>'+
  '<?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>'+
  '<cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">'+
         '<identification>'+
       	  '<manufacturer>Mustangpeak</manufacturer>'+
       	  '<model>TCN1000</model>'+
       	  '<hardwareVersion>1.0.0.0</hardwareVersion>'+
       	  '<softwareVersion>1.0.0.0</softwareVersion>'+
         '</identification>'+
         '<segment origin="1" space="253">'+
       	  '<name>User</name>'+
       	  '<description>User defined information</description>'+
       	  '<group>'+
       		  '<name>User Data</name>'+
       		  '<description>Add your own unique node info here</description>'+
       		  '<string size="63">'+
       			  '<name>User Name</name>'+
       		  '</string>'+
       		  '<string size="64">'+
       			  '<name>User Description</name>'+
       		  '</string>'+
       	  '</group>'+
         '</segment>'+
  '</cdi>');


type

  TLccTrainController = class;
  TLccTaskSearchTrain = class;

  { TAssignedTrainState }

  TAssignedTrainState = class
  private
    FOwner: TLccTrainController;
    FSearchCriteriaPending: DWORD;
    FTrain: TLccAliasMappingRec;
    FSearchCriteria: DWORD;
    FWorkerMessage: TLccMessage;
  public
    property Owner: TLccTrainController read FOwner write FOwner;
    property Train: TLccAliasMappingRec read FTrain write FTrain;
    property SearchCriteriaPending: DWORD read FSearchCriteriaPending write FSearchCriteriaPending;
  //  property SearchCriteria: DWORD read FSearchCriteria write FSearchCriteria;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    constructor Create;
    destructor Destroy; override;

    procedure AcceptSearchCriteriaPending;
    procedure ClearTrain;
    function IsAssigned: Boolean;
    function IsEqual(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
    procedure SetSpeed(NewSpeed: Single);
    procedure SetFunction(AnAddress: DWord; AValue: Word);
  end;


  TOnTLccTaskSearchTrainCallback = procedure(ATask: TLccTaskSearchTrain) of object;

  { TLccTaskSearchTrain }

  TLccTaskSearchTrain = class(TLccTaskBase)
  private
    FDccAddress: String;
    FIsLongAddress: Boolean;
    FSearchCriteria: DWord;
    FSpeedSteps: TLccDccSpeedStep;
    FTrainNodeID: TNodeID;
  public
    property SearchCriteria: DWord read FSearchCriteria write FSearchCriteria;
    property DccAddress: String read FDccAddress write FDccAddress;
    property IsLongAddress: Boolean read FIsLongAddress write FIsLongAddress;
    property SpeedSteps: TLccDccSpeedStep read FSpeedSteps write FSpeedSteps;

    property TrainNodeID: TNodeID read FTrainNodeID;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskControllerAttach }

  TLccTaskControllerAttach = class(TLccTaskBase)
  private
    FTrainNodeID: TNodeID;
  public
    property TrainNodeID: TNodeID read FTrainNodeID write FTrainNodeID;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskControllerRelease }

  TLccTaskControllerRelease = class(TLccTaskBase)
  private
    FControllerNode: TNodeID;
    FTrainNodeID: TNodeID;
  public
    property TrainNodeID: TNodeID read FTrainNodeID write FTrainNodeID;
    property ControllerNode: TNodeID read FControllerNode write FControllerNode;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;  // abstract must override
  end;

  { TLccTaskControllerQuery }

  TLccTaskControllerQuery = class(TLccTaskBase)
  private
    FActiveController: TNodeID;
    FFlags: Byte;
    FTrainNodeID: TNodeID;
  public
    property TrainNodeID: TNodeID read FTrainNodeID write FTrainNodeID;

    property Flags: Byte read FFlags;
    property ActiveController: TNodeID read FActiveController;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;


  // ******************************************************************************

  { TLccTrainController }

  TLccTrainController = class(TLccNode)
  private
    FAssignedTrain: TAssignedTrainState;
    FTaskControllerAttach: TLccTaskControllerAttach;
    FTaskControllerQuery: TLccTaskControllerQuery;
    FTaskControllerRelease: TLccTaskControllerRelease;
    FTaskSearchTrain: TLccTaskSearchTrain;

  protected
    property TaskSearchTrain: TLccTaskSearchTrain read FTaskSearchTrain write FTaskSearchTrain;
    property TaskControllerAttach: TLccTaskControllerAttach read FTaskControllerAttach write FTaskControllerAttach;
    property TaskControllerRelease: TLccTaskControllerRelease read FTaskControllerRelease write FTaskControllerRelease;
    property TaskControllerQuery: TLccTaskControllerQuery read FTaskControllerQuery write FTaskControllerQuery;

    function GetCdiFile: string; override;
    procedure BeforeLogin; override;

    procedure HandleProducerIdentifiedAll(var SourceMessage: TLccMessage);
    procedure HandleProducerIdentifiedClear(var SourceMessage: TLccMessage); override;
    procedure HandleProducerIdentifiedSet(var SourceMessage: TLccMessage); override;
    procedure HandleProducerIdentifiedUnknown(var SourceMessage: TLccMessage); override;
    procedure HandleTractionControllerAssignReply(var SourceMessage: TLccMessage); override;
 //   procedure HandleTractionControllerChangedNotify(var SourceMessage: TLccMessage); override;

    function MessageFromControlledTrain(ALccMessage: TLccMessage): Boolean;
  public

    property AssignedTrain: TAssignedTrainState read FAssignedTrain write FAssignedTrain;



    constructor Create(AOwner: TComponent; CdiXML: string); override;
    destructor Destroy; override;

    procedure AfterLogin; override;

    function SearchByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnTaskCallback): Boolean;
    function SearchByDccAddress(DccAddress: String; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnTaskCallback): Boolean;

    function AssignToTrain(ATrainID: TNodeID; ACallback: TOnTaskCallback): Boolean;
    function ReleaseFromTrain(ATrainID: TNodeID): Boolean;    // Messages expects no reply so no callback
    function QueryActiveController(ACallback: TOnTaskCallback): Boolean;

    procedure ListenerDetachFromAssignedTrain;

    procedure FindAllTrains;
    procedure ReleaseTrain;
    function IsTrainAssigned: Boolean;
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

{ TLccTaskControllerQuery }

procedure TLccTaskControllerQuery.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(TrainNodeID);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionControllerQuery(OwnerNode.NodeID, OwnerNode.AliasID, TrainNodeID, Alias);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);  // There is no reply from this
    Complete;
    if Assigned(Callback) then
      Callback(Self);
    Reset;
  end else
  begin
    Error(1, 'Unable to resolve an Alias');
  end;
end;

procedure TLccTaskControllerQuery.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_CONTROLLER_CONFIG_REPLY then
      if SourceMessage.DataArray[1] = TRACTION_CONTROLLER_CONFIG_QUERY then
      begin
        FFlags := SourceMessage.DataArray[2];
        SourceMessage.ExtractDataBytesAsNodeID(3, FActiveController);
        Complete;
        if Assigned(Callback) then
          Callback(Self);
        Reset;
      end;
end;

{ TLccTaskControllerRelease }

procedure TLccTaskControllerRelease.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(TrainNodeID);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionControllerRelease(OwnerNode.NodeID, OwnerNode.AliasID, TrainNodeID, Alias, ControllerNode);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);  // There is no reply from this
    Complete;
    if Assigned(Callback) then
      Callback(Self);
    Reset;
  end else
  begin
    Error(1, 'Unable to resolve an Alias');
  end;
end;

procedure TLccTaskControllerRelease.Process(SourceMessage: TLccMessage);
begin
  // abstract override
end;

{ TLccTaskControllerAttach }

procedure TLccTaskControllerAttach.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(TrainNodeID);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionControllerAssign(OwnerNode.NodeID, OwnerNode.AliasID, TrainNodeID, Alias, OwnerNode.NodeID);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  end;
end;

procedure TLccTaskControllerAttach.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_CONTROLLER_CONFIG_REPLY then
      if SourceMessage.DataArray[1] = TRACTION_CONTROLLER_CONFIG_ASSIGN then
      begin
        case SourceMessage.DataArray[2] of
          S_OK : begin
                   Complete;
                   if Assigned(Callback) then
                     Callback(Self);
                   Reset;
                 end;
         TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
                begin
                   Error(TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN, 'Train Refused to connect to the throttle');
                   if Assigned(Callback) then
                     Callback(Self);
                   Reset;
                end;
      end
  end;
end;

{ TLccTaskSearchTrain }

procedure TLccTaskSearchTrain.Start(ATimeout: Integer);
var
  TrackProtocolFlags: Word;
begin

  inherited Start(ATimeout);

  FTrainNodeID := NULL_NODE_ID;

  TrackProtocolFlags := TRACTION_SEARCH_TARGET_ADDRESS_MATCH or TRACTION_SEARCH_ALLOCATE_FORCE or TRACTION_SEARCH_TYPE_EXACT_MATCH or
                        TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY;

  if IsLongAddress then
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG
  else
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_DEFAULT;

  case SpeedSteps of
     ldssDefault : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ANY_SPEED_STEP;
     ldss14      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP;
     ldss28      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP;
     ldss128     : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP;
  end;

  SearchCriteria := 0;
  WorkerMessage.TractionSearchEncodeSearchString(DccAddress, TrackProtocolFlags, FSearchCriteria);
  WorkerMessage.LoadTractionSearch(OwnerNode.NodeID, OwnerNode.AliasID, SearchCriteria);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TLccTaskSearchTrain.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.IsCAN then
    Exit;
  if IsRunning then
  begin
    case SourceMessage.MTI of
       MTI_PRODUCER_IDENTIFIED_UNKNOWN,
       MTI_PRODUCER_IDENTIFIED_SET,
       MTI_PRODUCER_IDENTIFIED_CLEAR:
         begin
            if SourceMessage.TractionIsSearchEvent then
            begin
              if SourceMessage.TractionSearchExtractSearchData = SearchCriteria then
              begin
                FTrainNodeID := SourceMessage.SourceID;
                Complete;
                if Assigned(Callback) then
                  Callback(Self);
                Reset;
              end
            end;
         end;
    end
  end;
end;

{ TAssignedTrainState }

constructor TAssignedTrainState.Create;
begin
  FWorkerMessage := TLccMessage.Create;
end;

destructor TAssignedTrainState.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;


procedure TAssignedTrainState.AcceptSearchCriteriaPending;
begin
  FSearchCriteria := SearchCriteriaPending;
  FSearchCriteriaPending := 0;
end;

procedure TAssignedTrainState.ClearTrain;
begin
  FTrain.NodeID := NULL_NODE_ID;
  FTrain.Alias := 0;
  FSearchCriteria := 0;
  FSearchCriteriaPending := 0;
end;

function TAssignedTrainState.IsAssigned: Boolean;
begin
  Result := not NullNodeID(FTrain.NodeID) or (FTrain.Alias <> 0)
end;

function TAssignedTrainState.IsEqual(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
begin
  Result := EqualNodeID(ATestNodeID, FTrain.NodeID, False) or (ATestNodeAlias = FTrain.Alias);
end;

procedure TAssignedTrainState.SetSpeed(NewSpeed: Single);
begin
  if IsAssigned then
  begin
    WorkerMessage.LoadTractionSetSpeed(Owner.NodeID, Owner.AliasID, Train.NodeID, Train.Alias, NewSpeed);
    Owner.SendMessage(WorkerMessage, Owner);
  end;
end;

procedure TAssignedTrainState.SetFunction(AnAddress: DWord; AValue: Word);
begin
if IsAssigned then
  begin
    WorkerMessage.LoadTractionSetFunction(Owner.NodeID, Owner.AliasID, Train.NodeID, Train.Alias, AnAddress, AValue);
    Owner.SendMessage(WorkerMessage, Owner);
  end;
end;

{ TLccTrainController }

procedure TLccTrainController.BeforeLogin;
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.MemConfig := True;
  ProtocolSupportedProtocols.Datagram := True;
  ProtocolSupportedProtocols.EventExchange := True;
  ProtocolSupportedProtocols.SimpleNodeInfo := True;
  ProtocolSupportedProtocols.TractionControl := True;

  ProtocolEventsProduced.Add(EVENT_EMERGENCY_STOP, evs_InValid);

  ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, StreamCdi.Size);
  ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);

  ProtocolMemoryOptions.WriteUnderMask := True;
  ProtocolMemoryOptions.UnAlignedReads := True;
  ProtocolMemoryOptions.UnAlignedWrites := True;
  ProtocolMemoryOptions.SupportACDIMfgRead := True;
  ProtocolMemoryOptions.SupportACDIUserRead := True;
  ProtocolMemoryOptions.SupportACDIUserWrite := True;
  ProtocolMemoryOptions.WriteLenOneByte := True;
  ProtocolMemoryOptions.WriteLenTwoBytes := True;
  ProtocolMemoryOptions.WriteLenFourBytes := True;
  ProtocolMemoryOptions.WriteLenSixyFourBytes := True;
  ProtocolMemoryOptions.WriteArbitraryBytes := True;
  ProtocolMemoryOptions.WriteStream := False;
  ProtocolMemoryOptions.HighSpace := MSI_CDI;
  ProtocolMemoryOptions.LowSpace := MSI_TRACTION_FUNCTION_CONFIG;
end;


procedure TLccTrainController.HandleProducerIdentifiedAll(var SourceMessage: TLccMessage);
var
  AliasMapping: TLccAliasMapping;
begin
{  if SourceMessage.TractionIsSearchEvent and (SourceMessage.TractionSearchExtractSearchData = AssignedTrain.SearchCriteriaPending) then
  begin
    // Move from Pending the this is the Search Critera used.
    AssignedTrain.AcceptSearchCriteriaPending;
    if GridConnect then
    begin
      AliasMapping := AliasServer.FindMapping(SourceMessage.SourceAlias);
      Assert(Assigned(AliasMapping), 'TLccTrainController.' +'ProcessMessageLCC: Alias Mapping was nil');
      WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, AliasMapping.NodeID, AliasMapping.NodeAlias, NodeID);
    end else
      WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, NodeID);
    SendMessage(WorkerMessage);    // The reply will fill in the assigned train if successful
  end;  }
end;

procedure TLccTrainController.HandleProducerIdentifiedClear(var SourceMessage: TLccMessage);
begin
  HandleProducerIdentifiedAll(SourceMessage)
end;

procedure TLccTrainController.HandleProducerIdentifiedSet(var SourceMessage: TLccMessage);
begin
  HandleProducerIdentifiedAll(SourceMessage)
end;

procedure TLccTrainController.HandleProducerIdentifiedUnknown(var SourceMessage: TLccMessage);
begin
  HandleProducerIdentifiedAll(SourceMessage)
end;

procedure TLccTrainController.HandleTractionControllerAssignReply(var SourceMessage: TLccMessage);
begin
  case SourceMessage.TractionExtractControllerAssignResult of
    TRACTION_CONTROLLER_CONFIG_REPLY_OK :
      begin
 //       AssignedTrain.AssignTrain(SourceMessage.SourceID, SourceMessage.SourceAlias);
      end;
{    TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :      // Depreciated bad idea
      begin
         AssignedTrain.ClearTrain;
      end;        }
    TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
      begin
         AssignedTrain.ClearTrain;
      end
    else
      AssignedTrain.ClearTrain;
  end;
end;

{
procedure TLccTrainController.HandleTractionControllerChangedNotify(var SourceMessage: TLccMessage);
begin
  inherited HandleTractionControllerChangedNotify(SourceMessage);
  // Controller is loosing the train
end;
}

function TLccTrainController.MessageFromControlledTrain(ALccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;


procedure TLccTrainController.FindAllTrains;
begin
  // Send global Producer Identify.  When done the TrainServer property will have the trains and the information
   WorkerMessage.LoadTractionIsTrainProducer(NodeID, AliasID, NULL_NODE_ID, 0);
   SendMessage(WorkerMessage, Self);
end;

procedure TLccTrainController.ReleaseTrain;
begin
//
end;

function TLccTrainController.IsTrainAssigned: Boolean;
begin
  Result := AssignedTrain.IsAssigned;
end;

constructor TLccTrainController.Create(AOwner: TComponent; CdiXML: string);
begin
  inherited Create(AOwner, CdiXML);

  FAssignedTrain := TAssignedTrainState.Create;
  AssignedTrain.Owner := Self;

  FTaskSearchTrain := TLccTaskSearchTrain.Create(Self);
  FTaskControllerAttach := TLccTaskControllerAttach.Create(Self);
  FTaskControllerRelease := TLccTaskControllerRelease.Create(Self);
  FTaskControllerQuery := TLccTaskControllerQuery.Create(Self);

  RegisterTask(TaskSearchTrain);
  RegisterTask(TaskControllerAttach);
  RegisterTask(TaskControllerRelease);
  RegisterTask(TaskControllerQuery);
end;

destructor TLccTrainController.Destroy;
begin
  UnRegisterTask(TaskSearchTrain);
  UnRegisterTask(TaskControllerAttach);
  UnRegisterTask(TaskControllerRelease);

  FreeAndNil(FTaskSearchTrain);
  FreeAndNil(FAssignedTrain);
  FreeAndNil(FTaskControllerAttach);
  FreeAndNil(FTaskControllerAttach);
  inherited Destroy;
end;

procedure TLccTrainController.AfterLogin;
begin
  inherited AfterLogin;
 // FindAllTrains;
end;


function TLccTrainController.SearchByDccAddress(DccAddress: Word;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep;
  ACallback: TOnTaskCallback): Boolean;
begin
  Result := SearchByDccAddress(IntToStr( DccAddress), IsLongAddress, SpeedSteps, ACallback);
end;

function TLccTrainController.SearchByDccAddress(DccAddress: String;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep;
  ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskSearchTrain.IsIdle then
  begin
    TaskSearchTrain.DccAddress := DccAddress;
    TaskSearchTrain.IsLongAddress := IsLongAddress;
    TaskSearchTrain.SpeedSteps := SpeedSteps;
    TaskSearchTrain.Callback := ACallback;
    TaskSearchTrain.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.AssignToTrain(ATrainID: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskControllerAttach.IsIdle then
  begin
    TaskControllerAttach.Callback := ACallback;
    TaskControllerAttach.TrainNodeID := ATrainID;
    TaskControllerAttach.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ReleaseFromTrain(ATrainID: TNodeID): Boolean;
begin
  Result := False;
  if TaskControllerAttach.IsIdle then
  begin
    TaskControllerRelease.TrainNodeID := ATrainID;
    TaskControllerRelease.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.QueryActiveController(ACallback: TOnTaskCallback): Boolean;
begin
   Result := False;
  if TaskControllerQuery.IsIdle then
  begin
    TaskControllerQuery.Callback := ACallback;
    TaskControllerQuery.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;


procedure TLccTrainController.ListenerDetachFromAssignedTrain;
begin
  if AssignedTrain.IsAssigned then
  begin
    WorkerMessage.LoadTractionListenerDetach(NodeID, AliasID, AssignedTrain.Train.NodeID, AssignedTrain.Train.Alias, 0, NodeID);
    SendMessage(WorkerMessage, Self);
  end;
end;

function TLccTrainController.GetCdiFile: string;
begin
  Result := CDI_XML_CONTROLLER;
end;

end.

