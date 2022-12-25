unit lcc_node_controller;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ..\lcc_compilers.inc}

{.$DEFINE DISABLE_STATE_TIMEOUTS_FOR_DEBUG}

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
  lcc_utilities,
  lcc_node,
  lcc_defines,
  lcc_alias_server,
  lcc_train_server,
  lcc_node_traindatabase,
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

  TOnLccTrainControllerAssignChange = procedure(Sender: TObject; TractionServer: TLccTractionServer; TractionObject: TLccTractionObject; IsAssigned: Boolean) of object;

  { TAssignedTrainState }

  TAssignedTrainState = class
  private
    FOwner: TLccTrainController;
    FSearchCriteriaPending: DWORD;
    FTrain: TNodeIdentification;
    FSearchCriteria: DWORD;
    FWorkerMessage: TLccMessage;
  public
    property Owner: TLccTrainController read FOwner write FOwner;
    property Train: TNodeIdentification read FTrain write FTrain;
    property SearchCriteriaPending: DWORD read FSearchCriteriaPending write FSearchCriteriaPending;
  //  property SearchCriteria: DWORD read FSearchCriteria write FSearchCriteria;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    constructor Create;
    destructor Destroy; override;

    procedure AssignTrain(ANodeID: TNodeId; AnAlias: Word);
    procedure AcceptSearchCriteriaPending;
    procedure ClearTrain;
    function IsAssigned: Boolean;
    function IsEqual(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
    procedure SetSpeed(NewSpeed: Single);
    procedure SetFunction(AnAddress: DWord; AValue: Word);
  end;

  // ******************************************************************************

  { TLccTrainController }

  TLccTrainController = class(TLccTractionServerNode)
  private
    FAssignedTrain: TAssignedTrainState;
    FOnControllerAssignChange: TOnLccTrainControllerAssignChange;

  protected
    function GetCdiFile: string; override;
    procedure BeforeLogin; override;

    procedure DoControllerTakeOverRequest(var AllowTakeOver: Boolean);
    procedure DoControllerAssignChange(ATractionServer: TLccTractionServer; ATractionObject: TLccTractionObject; IsAssigned: Boolean);

    procedure HandleProducerIdentifiedAll(var SourceMessage: TLccMessage);
    procedure HandleProducerIdentifiedClear(var SourceMessage: TLccMessage); override;
    procedure HandleProducerIdentifiedSet(var SourceMessage: TLccMessage); override;
    procedure HandleProducerIdentifiedUnknown(var SourceMessage: TLccMessage); override;
    procedure HandleTractionControllerAssignReply(var SourceMessage: TLccMessage); override;
    procedure HandleTractionControllerChanging(var SourceMessage: TLccMessage); override;

    function MessageFromControlledTrain(ALccMessage: TLccMessage): Boolean;
  public

    property AssignedTrain: TAssignedTrainState read FAssignedTrain write FAssignedTrain;

    property OnControllerAssignChange: TOnLccTrainControllerAssignChange read FOnControllerAssignChange write FOnControllerAssignChange;

    constructor Create(ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean); override;
    destructor Destroy; override;

    procedure AfterLogin; override;
    procedure AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);

    procedure ListenerAttach(TrainObject: TLccTractionObject);
    procedure ListenerDetach(TrainObject: TLccTractionObject);
    procedure ListenerDetachFromAssignedTrain;

    procedure FindAllTrains;
    procedure ReleaseTrain;
    function IsTrainAssigned: Boolean;
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

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

procedure TAssignedTrainState.AssignTrain(ANodeID: TNodeId; AnAlias: Word);
begin
  FTrain.NodeID := ANodeID;
  FTrain.Alias := AnAlias;
  Owner.DoControllerAssignChange(Owner.TractionServer, Owner.TractionServer.Find(Train.NodeID), True);
end;

procedure TAssignedTrainState.AcceptSearchCriteriaPending;
begin
  FSearchCriteria := SearchCriteriaPending;
  FSearchCriteriaPending := 0;
end;

procedure TAssignedTrainState.ClearTrain;
begin
  if IsAssigned then
    Owner.DoControllerAssignChange(Owner.TractionServer, Owner.TractionServer.Find(Train.NodeID), False);
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
    Owner.SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TAssignedTrainState.SetFunction(AnAddress: DWord; AValue: Word);
begin
if IsAssigned then
  begin
    WorkerMessage.LoadTractionSetFunction(Owner.NodeID, Owner.AliasID, Train.NodeID, Train.Alias, AnAddress, AValue);
    Owner.SendMessageFunc(Self, WorkerMessage);
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

  ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, $FFFFFFFF);
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

procedure TLccTrainController.DoControllerTakeOverRequest(var AllowTakeOver: Boolean);
begin
  AllowTakeOver := True;
end;

procedure TLccTrainController.DoControllerAssignChange(ATractionServer: TLccTractionServer; ATractionObject: TLccTractionObject; IsAssigned: Boolean);
begin
  if Assigned(OnControllerAssignChange) then
    OnControllerAssignChange(Self, ATractionServer, ATractionObject, IsAssigned);
end;

procedure TLccTrainController.HandleProducerIdentifiedAll(var SourceMessage: TLccMessage);
var
  AliasMapping: TLccAliasMapping;
begin
  if SourceMessage.TractionIsSearchEvent and (SourceMessage.TractionSearchExtractSearchData = AssignedTrain.SearchCriteriaPending) then
  begin
    // Move from Pending the this is the Search Critera used.
    AssignedTrain.AcceptSearchCriteriaPending;
    if GridConnect then
    begin
      AliasMapping := AliasServer.FindMapping(SourceMessage.CAN.SourceAlias);
      Assert(Assigned(AliasMapping), 'TLccTrainController.' +'ProcessMessageLCC: Alias Mapping was nil');
      WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, AliasMapping.NodeID, AliasMapping.NodeAlias, NodeID);
    end else
      WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, NodeID);
    SendMessageFunc(Self, WorkerMessage);    // The reply will fill in the assigned train if successful
  end;
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
        AssignedTrain.AssignTrain(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
      end;
    TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :
      begin
         AssignedTrain.ClearTrain;
      end;
    TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
      begin
         AssignedTrain.ClearTrain;
      end
    else
      AssignedTrain.ClearTrain;
  end;
end;

// This is called from the train to ask this controller to give up control
procedure TLccTrainController.HandleTractionControllerChanging(var SourceMessage: TLccMessage);
var
  AllowTakeover: Boolean;
begin
  // Only care if coming from our Assigned Train
  if MessageFromControlledTrain(SourceMessage) then
  begin
    AllowTakeover := True;
    DoControllerTakeOverRequest(AllowTakeover);
    if AllowTakeover then
    begin
      ReleaseTrain;
      WorkerMessage.LoadTractionControllerChangingReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, True);
      SendMessageFunc(Self, WorkerMessage);
   //       DoTrainReleased;
    end else
    begin
      WorkerMessage.LoadTractionControllerChangingReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, False );
      SendMessageFunc(Self, WorkerMessage);
    end;
  end;
end;


function TLccTrainController.MessageFromControlledTrain(ALccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;


procedure TLccTrainController.FindAllTrains;
begin
  // Send global Producer Identify.  When done the TrainServer property will have the trains and the information
   WorkerMessage.LoadTractionIsTrainProducer(NodeID, AliasID, NULL_NODE_ID, 0);
   SendMessageFunc(Self, WorkerMessage);
end;

procedure TLccTrainController.ReleaseTrain;
begin
  if AssignedTrain.IsAssigned then
  begin
    WorkerMessage.LoadTractionControllerRelease(NodeID, AliasID, AssignedTrain.Train.NodeID, AssignedTrain.Train.Alias, NodeID, AliasID);
    SendMessageFunc(Self, WorkerMessage);
    AssignedTrain.ClearTrain;
  end;
end;

function TLccTrainController.IsTrainAssigned: Boolean;
begin
  Result := AssignedTrain.IsAssigned;
end;

constructor TLccTrainController.Create(ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean);
begin
  inherited Create(ANodeManager, CdiXML, GridConnectLink);
  TractionServer.Enabled := True;
  FAssignedTrain := TAssignedTrainState.Create;
  AssignedTrain.Owner := Self;
end;

destructor TLccTrainController.Destroy;
begin
  FreeAndNil(FAssignedTrain);
  inherited Destroy;
end;

procedure TLccTrainController.AfterLogin;
begin
  inherited AfterLogin;
 // FindAllTrains;
end;

procedure TLccTrainController.AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
begin
  AssignTrainByDccTrain(IntToStr(DccAddress), IsLongAddress, SpeedSteps);
end;

procedure TLccTrainController.AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
var
  TrackProtocolFlags: Word;
begin

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

  AssignTrainByOpenLCB(SearchString, TrackProtocolFlags);
end;

procedure TLccTrainController.AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
var
  LocalSearchCriteria: DWORD;
begin
  ReleaseTrain;
  LocalSearchCriteria := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, LocalSearchCriteria);
  AssignedTrain.SearchCriteriaPending := LocalSearchCriteria;
  // Search for that train... the reply to this is handled in the Process Message MTI_PRODUCER_IDENTIFIED_XXXXX
  WorkerMessage.LoadTractionSearch(NodeID, AliasID, LocalSearchCriteria);
  SendMessageFunc(Self, WorkerMessage);
end;

procedure TLccTrainController.ListenerAttach(TrainObject: TLccTractionObject);
begin
  WorkerMessage.LoadTractionListenerAttach(NodeID, AliasID, TrainObject.NodeID, TrainObject.NodeAlias, TRACTION_LISTENER_FLAG_HIDDEN, NodeID);
  SendMessageFunc(Self, WorkerMessage);
end;

procedure TLccTrainController.ListenerDetach(TrainObject: TLccTractionObject);
begin
  WorkerMessage.LoadTractionListenerDetach(NodeID, AliasID, TrainObject.NodeID, TrainObject.NodeAlias, 0, NodeID);
  SendMessageFunc(Self, WorkerMessage);
end;

procedure TLccTrainController.ListenerDetachFromAssignedTrain;
begin
  if AssignedTrain.IsAssigned then
  begin
    WorkerMessage.LoadTractionListenerDetach(NodeID, AliasID, AssignedTrain.Train.NodeID, AssignedTrain.Train.Alias, 0, NodeID);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

function TLccTrainController.GetCdiFile: string;
begin
  Result := CDI_XML_CONTROLLER;
end;

end.

