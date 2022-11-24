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

  { TAssignedTrainState }

  TAssignedTrainState = class
  private
    FSearchCriteriaPending: DWORD;
    FTrain: TNodeIdentification;
    FSearchCriteria: DWORD;
  public
    property Train: TNodeIdentification read FTrain write FTrain;
    property SearchCriteriaPending: DWORD read FSearchCriteriaPending write FSearchCriteriaPending;
  //  property SearchCriteria: DWORD read FSearchCriteria write FSearchCriteria;

    procedure AssignTrain(ANodeID: TNodeId; AnAlias: Word);
    procedure AcceptSearchCriteriaPending;
    procedure ClearTrain;
    function IsAssigned: Boolean;
    function IsEqual(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
  end;

  // ******************************************************************************

  { TLccTrainController }

  TLccTrainController = class(TLccNode)
  private
    FAssignedTrain: TAssignedTrainState;

  protected
    function GetCdiFile: string; override;
    procedure BeforeLogin; override;

    procedure DoControllerTakeOverRequest(var AllowTakeOver: Boolean);

    function MessageFromControlledTrain(ALccMessage: TLccMessage): Boolean;
  public

    property AssignedTrain: TAssignedTrainState read FAssignedTrain write FAssignedTrain;
    property TractionServer;

    constructor Create(ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean); override;
    destructor Destroy; override;

    procedure AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);

    procedure FindAllTrains;
    procedure ReleaseTrain;
    function IsTrainAssigned: Boolean;

    function ProcessMessageLCC(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

{ TAssignedTrainState }

procedure TAssignedTrainState.AssignTrain(ANodeID: TNodeId; AnAlias: Word);
begin
  FTrain.NodeID := ANodeID;
  FTrain.Alias := AnAlias;
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

{ TLccTrainController }

function TLccTrainController.ProcessMessageLCC(SourceMessage: TLccMessage): Boolean;
var
  AllowTakeOver: Boolean;
  AliasMapping: TLccAliasMapping;
begin
  Result :=inherited ProcessMessageLcc(SourceMessage);

  // We only are dealing with messages with destinations for us from here on
  if SourceMessage.HasDestination then
  begin
    if not EqualNode(NodeID,  AliasID, SourceMessage.DestID, SourceMessage.CAN.DestAlias, True) then
      Exit;
  end;

  // We can snoop here on all train nodes and try to keep the database updated.
  // The works until they are on a different segment and messages don't get routed
  // to this segment.  When will that every occur?  Who nows

 // TrainServer.;


  case SourceMessage.MTI of
    MTI_PRODUCER_IDENTIFIED_CLEAR,
    MTI_PRODUCER_IDENTIFIED_SET,
    MTI_PRODUCER_IDENTIFIED_UNKNOWN :
      begin
        if SourceMessage.TractionIsSearchEvent and (SourceMessage.TractionSearchExtractSearchData = AssignedTrain.SearchCriteriaPending) then
        begin
          // Move from Pending the this is the Search Critera used.
          AssignedTrain.AcceptSearchCriteriaPending;
          if GridConnect then
          begin
            AliasMapping := AliasServer.FindMapping(SourceMessage.CAN.SourceAlias);
            Assert(Assigned(AliasMapping), 'TLccTrainController.ProcessMessageLCC: Alias Mapping was nil');
            WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, AliasMapping.NodeID, AliasMapping.NodeAlias, NodeID);
          end else
            WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, NodeID);
          SendMessageFunc(Self, WorkerMessage);    // The reply will fill in the assigned train if successful
        end;
      end;
    MTI_TRACTION_REPLY :
      begin
        case SourceMessage.DataArray[0] of
          TRACTION_CONTROLLER_CONFIG :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_CONTROLLER_CONFIG_ASSIGN :
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
              end;
            end

        end;
      end;
    MTI_TRACTION_REQUEST :
      begin
        case SourceMessage.DataArray[0] of
          TRACTION_CONTROLLER_CONFIG :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_CONTROLLER_CONFIG_CHANGING_NOTIFY :
                begin
                  // Only care if coming from our Assigned Train
                    if MessageFromControlledTrain(SourceMessage) then
                    begin
                      AllowTakeover := True;
                      DoControllerTakeOverRequest(AllowTakeover);
                      if AllowTakeover then
                      begin
                        ReleaseTrain;
                        WorkerMessage.LoadTractionControllerChangedReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, True);
                        SendMessageFunc(Self, WorkerMessage);
                 //       DoTrainReleased;
                      end else
                      begin
                        WorkerMessage.LoadTractionControllerChangedReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, False );
                        SendMessageFunc(Self, WorkerMessage);
                      end;
                    end;
                end;
              end;
            end;
        end
      end;
  end;
end;


procedure TLccTrainController.BeforeLogin;
begin
 { ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
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
  ProtocolMemoryOptions.LowSpace := MSI_TRACTION_FUNCTION_CONFIG;       }
end;

procedure TLccTrainController.DoControllerTakeOverRequest(var AllowTakeOver: Boolean);
begin
  AllowTakeOver := True;
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
  FAssignedTrain := TAssignedTrainState.Create;
end;

destructor TLccTrainController.Destroy;
begin
  FreeAndNil(FAssignedTrain);
  inherited Destroy;
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

function TLccTrainController.GetCdiFile: string;
begin
  Result := CDI_XML_CONTROLLER;
end;

end.

