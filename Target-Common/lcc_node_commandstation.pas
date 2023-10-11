unit lcc_node_commandstation;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils,
  Classes,
  {$IFDEF LCC_FPC}
  {$ELSE}
    System.Types,
    FMX.Types,
    System.Generics.Collections,
  {$ENDIF}
  lcc_defines,
  lcc_node_messages,
  lcc_node,
  lcc_node_train,
  lcc_node_traindatabase,
  lcc_utilities;

const
  CDI_XML_COMMANDSTATION: string = (
  '<?xml version="1.0" encoding="utf-8"?>'+
  '<?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>'+
  '<cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">'+
         '<identification>'+
       	  '<manufacturer>Mustangpeak</manufacturer>'+
       	  '<model>CSN1000</model>'+
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
  '</cdi>'
  );

type

  { TLccCommandStationNode }

  TLccCommandStationNode = class(TLccTractionServerNode)
  protected
    function GetCdiFile: string; override;
    procedure BeforeLogin; override;

  public
    property TractionServer;

    constructor Create(AOwner: TComponent; CdiXML: string); override;
    function AddTrain(CdiXML: string; ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep): TLccTrainDccNode;  // Null CDI for default train node CDI
    procedure ClearTrains;
    function FindTrainByLccNodeID(ANodeID: TNodeID): TLccTrainDccNode;
    function FindTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean): TLccTrainDccNode;
    function ProcessMessageLCC(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccCommandStationNodeClass = class of TLccCommandStationNode;


implementation

uses
  lcc_node_manager;

function TLccCommandStationNode.AddTrain(CdiXML: string; ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep): TLccTrainDccNode;
begin
  Result := ((Owner as TLccNodeManager) as TLccNodeManager).AddNodeByClass(CdiXML, TLccTrainDccNode, False, NULL_NODE_ID) as TLccTrainDccNode;
  if Assigned(Result) then
  begin
    Result.DccAddress := ADccAddress;
    Result.DccLongAddress := ALongAddress;
    Result.DccSpeedStep := ASpeedStep;
  end;
end;

procedure TLccCommandStationNode.BeforeLogin;
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.MemConfig := True;
  ProtocolSupportedProtocols.Datagram := True;
  ProtocolSupportedProtocols.EventExchange := True;
  ProtocolSupportedProtocols.SimpleNodeInfo := True;
  ProtocolSupportedProtocols.TractionControl := True;

  ProtocolEventConsumed.Add(EVENT_EMERGENCY_STOP, evs_InValid);

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

constructor TLccCommandStationNode.Create(AOwner: TComponent; CdiXML: string);
begin
  inherited Create(AOwner, CdiXML);
  TractionServer.Enabled := True;
end;

procedure TLccCommandStationNode.ClearTrains;
var
  i: Integer;
  LocalNode: TLccNode;
  LocalNodeList: TList;
begin
  LocalNodeList := (Owner as TLccNodeManager).Nodes;
  for i := LocalNodeList.Count - 1 downto 0 do
   begin
     LocalNode := TLccNode(LocalNodeList[i]);
     if LocalNode is TLccTrainDccNode then
     begin
       LocalNodeList.Delete(i);
       LocalNode.Free;
     end;
   end;
end;

function TLccCommandStationNode.FindTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean): TLccTrainDccNode;
var
  i: Integer;
  LocalTrainNode: TLccTrainDccNode;
  LocalNodeList: TList;
begin
  Result := nil;
  LocalNodeList := (Owner as TLccNodeManager).Nodes;
  for i := LocalNodeList.Count - 1 downto 0 do
   begin
     if TLccNode(LocalNodeList[i]) is TLccTrainDccNode then
     begin
       LocalTrainNode := TLccTrainDccNode(LocalNodeList[i]);
       if (DccAddress = LocalTrainNode.DccAddress) and (IsLongAddress = LocalTrainNode.DccLongAddress) then
       begin
         Result := LocalTrainNode;
         Break;
       end;
     end;
   end;
end;

function TLccCommandStationNode.FindTrainByLccNodeID(ANodeID: TNodeID): TLccTrainDccNode;
var
  i: Integer;
  LocalTrainNode: TLccTrainDccNode;
  LocalNodeList: TList;
begin
  Result := nil;
  LocalNodeList := (Owner as TLccNodeManager).Nodes;
  for i := LocalNodeList.Count - 1 downto 0 do
   begin
     if TLccNode(LocalNodeList[i]) is TLccTrainDccNode then
     begin
       LocalTrainNode := TLccTrainDccNode(LocalNodeList[i]);
       if EqualNodeID(ANodeID, LocalTrainNode.NodeID, False) then
       begin
         Result := LocalTrainNode;
         Break;
       end;
     end;
   end;
end;

function TLccCommandStationNode.GetCdiFile: string;
begin
  Result := CDI_XML_COMMANDSTATION;
end;

function TLccCommandStationNode.ProcessMessageLCC(SourceMessage: TLccMessage): Boolean;
var
  NMRA_SpeedStep: TLccDccSpeedStep;
  SearchStr: string;
  SearchDccAddress: LongInt;
  LongAddressOnly, IsDCC: Boolean;
  ATrain: TLccTrainDccNode;
  ReturnEvent: TEventID;
begin
  Result := inherited ProcessMessageLcc(SourceMessage);

  case SourceMessage.MTI of
    MTI_PRODUCER_IDENDIFY :
      begin
        if SourceMessage.TractionIsSearchEvent then    // Is the the event for for traction search?
        begin
          SearchStr := SourceMessage.TractionSearchDecodeSearchString;

          if SearchStr <> '' then                       // Gaurd against an empty string
          begin
            SearchDccAddress := StrToInt(SearchStr);
            LongAddressOnly := False;
            NMRA_SpeedStep := ldss14;

            if SourceMessage.TractionSearchIsProtocolAny then
            begin
              NMRA_SpeedStep := ldss14;
              LongAddressOnly := False;
              IsDcc := True;
            end else
            if SourceMessage.TractionSearchIsProtocolDCC(LongAddressOnly, NMRA_SpeedStep) then
            begin
              if NMRA_SpeedStep = ldssDefault then
                NMRA_SpeedStep := ldss14;
              IsDCC := True;
            end else
              IsDCC := False;                 //    IF I CHANGE TO LONG ADDRESS OR CHANGE SPEED STEP SHOULD THAT BE A NEW TRAIN NODE???????

            if IsDCC then
            begin
              // Look for an existing Train
              ATrain := FindTrainByDccAddress(SearchDccAddress, LongAddressOnly);
              ReturnEvent := SourceMessage.ExtractDataBytesAsEventID(0); // Return the exact same event

              if Assigned(ATrain) then
              begin
                // A Train has been already created by this Search Event ID once, it may have not yet finished logging in before
                // some impatient controller trys again so don't create another train node if it is not finished yet just wait.
                if ATrain.Permitted then  // Once it logs in and becomes permitted it will send this message if not permitted yet
                begin
                  WorkerMessage.LoadProducerIdentified(ATrain.NodeID, ATrain.AliasID, ReturnEvent, evs_Valid);
                  SendMessage(WorkerMessage);
                end;
              end else
              if SourceMessage.TractionSearchIsForceAllocate then
              begin
                ATrain := AddTrain('', SearchDccAddress, LongAddressOnly, NMRA_SpeedStep);
                ATrain.SearchEvent := ReturnEvent; // Store to send later as we don't have an alias or a login for the node yet
                ATrain.Login(NULL_NODE_ID);
              end
            end
          end
        end;
      end;
  end;
end;

end.

