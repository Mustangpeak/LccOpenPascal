unit unit_subdivision;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  Buttons,
  ComCtrls,
  ActnList,
  Menus,
  my_fgl,
  unit_popup_menus,
  unit_base_location,
  unit_switchmachine,
  unit_turnout,
  unit_yard,
  unit_station,
  unit_defines,
  unit_types,
  unit_junction;

type

  { TSubDivision }

  TSubDivision = class(TBaseLocation)

  private
    FJunctions: TJunctionList;
    FStations: TStationList;
    FYards: TFreightYardList;

  protected

  public
    property Junctions: TJunctionList read FJunctions write FJunctions;
    property Stations: TStationList read FStations write FStations;
    property Yards: TFreightYardList read FYards write FYards;

    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;

  TSubDivisionList = specialize TFPGInterfacedObjectList<TSubDivision>;

implementation

{ TSubDivision }

procedure TSubDivision.BuildMenu(Pallet: TPanel);
begin

end;


constructor TSubDivision.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_SUBDIVISION);
  FJunctions := TJunctionList.Create;
  FYards := TFreightYardList.Create;
end;

destructor TSubDivision.Destroy;
begin
  Junctions.Free;
  Stations.Free;
  Yards.Free;
  inherited Destroy;
end;

end.
