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

  { TLccTractionServerNode }

  TLccTractionServerNode = class(TLccNode)
  private
    FTractionServer: TLccTractionServer;
  public
    property TractionServer: TLccTractionServer read FTractionServer;

    constructor Create(ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean); override;
    destructor Destroy; override;

    function ProcessMessageLCC(SourceMessage: TLccMessage): Boolean; override;
  end;

implementation


{ TLccTractionServerNode }

constructor TLccTractionServerNode.Create(ANodeManager: TObject; CdiXML: string; GridConnectLink: Boolean);
begin
  inherited Create(ANodeManager, CdiXML, GridConnectLink);
  FTractionServer := TLccTractionServer.Create(GridConnectLink);
end;

destructor TLccTractionServerNode.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTractionServer);
end;

function TLccTractionServerNode.ProcessMessageLCC(SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited ProcessMessageLCC(SourceMessage);

  if not Result then
  begin
    if TractionServer.Enabled then
      TractionServer.ProcessMessageLCC(Self, SourceMessage);
  end;
end;

end.

