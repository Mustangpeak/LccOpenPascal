unit lcc_node_traindatabase;

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
  lcc_protocol_utilities,
  lcc_defines,
  lcc_node_messages,
  lcc_utilities,
  lcc_node,
  lcc_alias_server,
  lcc_train_server;

type
  TLccEngineAllocateTrain = class;
  TLccEngineSearchTrain = class;

  TOnEngineAllocateTrainCallback = procedure(EngineAllocateTrain: TLccEngineAllocateTrain) of object;
  TLccEngineSearchTrainCallback = procedure(EngineSearchTrain: TLccEngineSearchTrain) of object;


  { TLccEngineSearchTrain }

  TLccEngineSearchTrain = class(TLccEngineBase)
  private
    FCallback: TLccEngineSearchTrainCallback;
    FSearchCriteriaPending: DWORD;
  protected
    property Callback: TLccEngineSearchTrainCallback read FCallback write FCallback;

    procedure HandleProducerIdentified(SourceMessage: TLccMessage);
  public
    property SearchCriteriaPending: DWORD read FSearchCriteriaPending;

    procedure Start;
    procedure Reset;
    procedure Process(SourceMessage: TLccMessage);
    procedure Assign(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TLccEngineSearchTrainCallback); overload;
    procedure Assign(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TLccEngineSearchTrainCallback); overload;
    procedure Assign(SearchString: string; TrackProtocolFlags: Word; ACallback: TLccEngineSearchTrainCallback); overload;
  end;

  { TLccEngineAllocateTrain }

  TLccEngineAllocateTrain = class(TLccEngineBase)
  private
    FCallback: TOnEngineAllocateTrainCallback;
    FEngineSearchTrain: TLccEngineSearchTrain;

  protected
    property Callback: TOnEngineAllocateTrainCallback read FCallback write FCallback;
    property EngineSearchTrain: TLccEngineSearchTrain read FEngineSearchTrain write FEngineSearchTrain;
  public

    constructor Create(AnOwner: TLccNode); override;
    destructor Destroy; override;
    procedure Start;
    procedure Reset;
    procedure Process(SourceMessage: TLccMessage);
    procedure Assign(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnEngineAllocateTrainCallback); overload;
    procedure Assign(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnEngineAllocateTrainCallback); overload;
    procedure Assign(SearchString: string; TrackProtocolFlags: Word; ACallback: TOnEngineAllocateTrainCallback); overload;
  end;

  { TLccTractionServerNode }

  TLccTractionServerNode = class(TLccNode)
  private
    FEngineAllocateTrain: TLccEngineAllocateTrain;
    FEngineSearchTrain: TLccEngineSearchTrain;
    FTractionServer: TLccTractionServer;
  protected
    procedure DoMemorySpaceReadEngineDone(MemoryReadEngine: TLccEngineMemorySpaceRead); override;
  public
    property EngineAllocateTrain: TLccEngineAllocateTrain read FEngineAllocateTrain write FEngineAllocateTrain;
    property EngineCreateTrain: TLccEngineSearchTrain read FEngineSearchTrain write FEngineSearchTrain;
    property TractionServer: TLccTractionServer read FTractionServer;

    constructor Create(ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean); override;
    destructor Destroy; override;

    function ProcessMessageLCC(SourceMessage: TLccMessage): Boolean; override;
  end;

implementation

{ TLccEngineSearchTrain }

procedure TLccEngineSearchTrain.HandleProducerIdentified(SourceMessage: TLccMessage);
begin
  if SourceMessage.TractionIsSearchEvent and (SourceMessage.TractionSearchExtractSearchData = SearchCriteriaPending) then
  begin
    Callback(Self)
  end;
end;

procedure TLccEngineSearchTrain.Start;
begin
  // Search for that train... the reply to this is handled in the Process Message MTI_PRODUCER_IDENTIFIED_XXXXX
  WorkerMessage.LoadTractionSearch(OwnerNode.NodeID, OwnerNode.AliasID, SearchCriteriaPending);
  OwnerNode.SendMessageFunc(OwnerNode, WorkerMessage);
end;

procedure TLccEngineSearchTrain.Reset;
begin
  FSearchCriteriaPending := 0;
  FCallback := nil;
end;

procedure TLccEngineSearchTrain.Process(SourceMessage: TLccMessage);
begin
  case SourceMessage.MTI of
     MTI_PRODUCER_IDENTIFIED_CLEAR   : HandleProducerIdentified(SourceMessage);
     MTI_PRODUCER_IDENTIFIED_SET     : HandleProducerIdentified(SourceMessage);
     MTI_PRODUCER_IDENTIFIED_UNKNOWN : HandleProducerIdentified(SourceMessage);
  end;
end;

procedure TLccEngineSearchTrain.Assign(DccAddress: Word;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep;
  ACallback: TLccEngineSearchTrainCallback);
begin
  Assign(IntToStr(DccAddress), IsLongAddress, SpeedSteps, ACallback);
end;

procedure TLccEngineSearchTrain.Assign(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TLccEngineSearchTrainCallback);
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

  Assign(SearchString, TrackProtocolFlags, ACallback);
end;

procedure TLccEngineSearchTrain.Assign(SearchString: string; TrackProtocolFlags: Word; ACallback: TLccEngineSearchTrainCallback);
var
  LocalSearchCriteria: DWORD;
begin
  FCallback := ACallback;
//  ReleaseTrain;
  LocalSearchCriteria := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, LocalSearchCriteria);
  FSearchCriteriaPending := LocalSearchCriteria;
end;

{ TLccEngineAllocateTrain }

constructor TLccEngineAllocateTrain.Create(AnOwner: TLccNode);
begin
  inherited Create(AnOwner);
  FEngineSearchTrain := TLccEngineSearchTrain.Create(AnOwner)
end;

destructor TLccEngineAllocateTrain.Destroy;
begin
  FreeAndNil(FEngineSearchTrain);
  inherited Destroy;
end;

procedure TLccEngineAllocateTrain.Start;
begin

end;

procedure TLccEngineAllocateTrain.Reset;
begin

end;

procedure TLccEngineAllocateTrain.Process(SourceMessage: TLccMessage);
begin
  EngineSearchTrain.Process(SourceMessage);
end;

procedure TLccEngineAllocateTrain.Assign(DccAddress: Word;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep;
  ACallback: TOnEngineAllocateTrainCallback);
begin

end;

procedure TLccEngineAllocateTrain.Assign(SearchString: string;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep;
  ACallback: TOnEngineAllocateTrainCallback);
begin
end;

procedure TLccEngineAllocateTrain.Assign(SearchString: string;
  TrackProtocolFlags: Word; ACallback: TOnEngineAllocateTrainCallback);
begin
end;


{ TLccTractionServerNode }

procedure TLccTractionServerNode.DoMemorySpaceReadEngineDone(MemoryReadEngine: TLccEngineMemorySpaceRead);
var
  TractionObject: TLccTractionObject;
begin
  inherited DoMemorySpaceReadEngineDone(MemoryReadEngine);
  if TractionServer.Enabled then
  begin
    TractionObject := TractionServer.Find(MemoryReadEngine.TargetNode);
    if Assigned(TractionObject) then
      TractionObject.NodeCDI.CDI := MemoryReadEngine.StreamAsString;
  end;
end;

constructor TLccTractionServerNode.Create(ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean);
begin
  inherited Create(ANodeManager, CdiXML, GridConnectLink);
  FTractionServer := TLccTractionServer.Create(GridConnectLink);
  FEngineAllocateTrain := TLccEngineAllocateTrain.Create(Self);
  FEngineSearchTrain := TLccEngineSearchTrain.Create(Self);
end;

destructor TLccTractionServerNode.Destroy;
begin
  FreeAndNil(FEngineSearchTrain);
  FreeAndNil(FEngineAllocateTrain);
  FreeAndNil(FTractionServer);
  inherited Destroy;
end;

function TLccTractionServerNode.ProcessMessageLCC(SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited ProcessMessageLCC(SourceMessage);

  if not Result then
  begin
    if TractionServer.Enabled then
      TractionServer.ProcessMessageLCC(Self, SourceMessage);
    EngineAllocateTrain.Process(SourceMessage);
    EngineCreateTrain.Process(SourceMessage);
  end;
end;

end.

