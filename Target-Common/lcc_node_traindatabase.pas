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

  lcc_defines,
  lcc_node_messages,
  lcc_alias_server,
  lcc_train_server,
  lcc_node;

type

  { TLccTrainDatabaseNode }

  TLccTrainDatabaseNode = class(TLccNode)
  private
    FTractionServer: TLccTractionServer;
  public
    property TractionServer: TLccTractionServer read FTractionServer;

    constructor Create(ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean); override;
    destructor Destroy; override;

    function ProcessMessageLCC(SourceMessage: TLccMessage): Boolean; override;
  end;


implementation

uses
  lcc_node_manager;

{ TLccTrainDatabaseNode }

constructor TLccTrainDatabaseNode.Create(ANodeManager: TObject; CdiXML: string; GridConnectLink: Boolean);
begin
  inherited Create(ANodeManager, CdiXML, GridConnectLink);
  FTractionServer := TLccTractionServer.Create;
end;

destructor TLccTrainDatabaseNode.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTractionServer);
end;

function TLccTrainDatabaseNode.ProcessMessageLCC(SourceMessage: TLccMessage): Boolean;
var
  LocalTractionObject: TLccTractionObject;
  LocalAliasMapping: TLccAliasMapping;
  LocalTractionNodeID: TNodeID;
begin
  Result := inherited ProcessMessageLCC(SourceMessage);

  if Result then
    Exit;

 case SourceMessage.MTI of
    MTI_SIMPLE_NODE_INFO_REPLY :
      begin
        LocalTractionObject := TractionServer.UpdateSNIP(SourceMessage);
        if Assigned(LocalTractionObject) then
          (NodeManager as INodeManagerTractionCallbacks).DoTractionUpdateSNIP(Self, LocalTractionObject);
      end;
    MTI_PRODUCER_IDENTIFIED_CLEAR,
    MTI_PRODUCER_IDENTIFIED_SET,
    MTI_PRODUCER_IDENTIFIED_UNKNOWN:
      begin
        if SourceMessage.IsEqualEventID(EVENT_IS_TRAIN) then
        begin
          if GridConnect then
          begin
            LocalAliasMapping := AliasServer.FindMapping(SourceMessage.CAN.SourceAlias);
            Assert(Assigned(LocalAliasMapping), 'Could not Assign Node AliasMapping in TLccTrainDatabaseNode.ProcessMessageLCC');
            LocalTractionNodeID := LocalAliasMapping.NodeID;
          end else
            LocalTractionNodeID := SourceMessage.SourceID;

          LocalTractionObject := TractionServer.Find(LocalTractionNodeID);
          if not Assigned(LocalTractionObject) then
          begin
            LocalTractionObject := TractionServer.Add(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
            (NodeManager as INodeManagerTractionCallbacks).DoTractionRegisteringChange(Self, LocalTractionObject, True);
          end;
          (NodeManager as INodeManagerTractionCallbacks).DoTractionProducerIsTrainIdentified(Self, LocalTractionObject);

          // Get some information about this train
          WorkerMessage.LoadSimpleNodeIdentInfoRequest(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
          SendMessageFunc(Self, WorkerMessage);
          WorkerMessage.LoadSimpleTrainNodeIdentInfoRequest(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
          SendMessageFunc(Self, WorkerMessage);
          WorkerMessage.LoadTractionListenerQueryCount(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
          SendMessageFunc(Self, WorkerMessage);
        end;
      end;
    MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY :
      begin
        LocalTractionObject := TractionServer.UpdateTrainSNIP(SourceMessage);
        if Assigned(LocalTractionObject) then
          (NodeManager as INodeManagerTractionCallbacks).DoTractionUpdateTrainSNIP(Self, LocalTractionObject);
      end;
   MTI_TRACTION_REQUEST :
     begin
       case SourceMessage.DataArray[0] of
         TRACTION_CONTROLLER_CONFIG_CHANGING_NOTIFY :
         begin
           if Assigned(LocalTractionObject) then
             (NodeManager as INodeManagerTractionCallbacks).DoTractionControllerChangingNotify(Self, LocalTractionObject);
         end;
       end;
     end;
   MTI_TRACTION_REPLY :
     begin
       case SourceMessage.DataArray[0] of
         TRACTION_QUERY_SPEED :
            begin
              LocalTractionObject := TractionServer.UpdateSpeed(SourceMessage);  // Map the sourceID to a Train
              if Assigned(LocalTractionObject) then
                (NodeManager as INodeManagerTractionCallbacks).DoTractionQuerySpeed(Self, LocalTractionObject);
            end;
          TRACTION_QUERY_FUNCTION :
            begin
              LocalTractionObject := TractionServer.UpdateFunction(SourceMessage);   // Map the sourceID to a Train
              if Assigned(LocalTractionObject) then
                (NodeManager as INodeManagerTractionCallbacks).DoTractionQueryFunction(Self, LocalTractionObject);
            end;
          TRACTION_CONTROLLER_CONFIG :
            begin
              case SourceMessage.DataArray[1] of
                 TRACTION_CONTROLLER_CONFIG_ASSIGN :
                   begin
                     LocalTractionObject := TractionServer.UpdateControllerAssign(SourceMessage);   // Map the sourceID to a Train
                     if Assigned(LocalTractionObject) then
                       (NodeManager as INodeManagerTractionCallbacks).DoTractionControllerAssign(Self, LocalTractionObject);
                   end;
                 TRACTION_CONTROLLER_CONFIG_RELEASE :
                   begin  // There is no reply to Release
                   end;
                 TRACTION_CONTROLLER_CONFIG_QUERY :
                   begin
                     LocalTractionObject := TractionServer.UpdateControllerQuery(SourceMessage);   // Map the sourceID to a Train
                     if Assigned(LocalTractionObject) then
                       (NodeManager as INodeManagerTractionCallbacks).DoTractionControllerQuery(Self, LocalTractionObject);
                   end;
                 TRACTION_CONTROLLER_CONFIG_CHANGED_NOTIFY :
                   begin
                     LocalTractionObject := TractionServer.Find(SourceMessage.SourceID);
                     if Assigned(LocalTractionObject) then
                       (NodeManager as INodeManagerTractionCallbacks).DoTractionControllerChangedNotify(Self, LocalTractionObject);
                   end;
              end;
            end;
          TRACTION_LISTENER :
           begin
             case SourceMessage.DataArray[1] of
               TRACTION_LISTENER_ATTACH :
                 begin

                 end;
               TRACTION_LISTENER_DETACH :
                 begin
                  end;
               TRACTION_LISTENER_QUERY :
                 begin
                   LocalTractionObject := TractionServer.UpdateListenerCount(SourceMessage);
                   if Assigned(LocalTractionObject) then
                     (NodeManager as INodeManagerTractionCallbacks).DoTractionUpdateListenerCount(Self, LocalTractionObject);
                 end;
             end;
           end;
        TRACTION_MANAGE :
          begin
            (NodeManager as INodeManagerTractionCallbacks).DoTractionManage(Self, SourceMessage, True);
          end;
        end;
     end;
  end; // case
end;

end.

