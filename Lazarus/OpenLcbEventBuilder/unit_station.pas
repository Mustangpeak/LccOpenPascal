unit unit_station;

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
  unit_defines,
  unit_base_location,
  unit_types,
  unit_turnout;

type

  { TStation }

  TStation = class(TBaseLocation)

  private

  protected

  public

    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;

  TStationList = specialize TFPGInterfacedObjectList<TStation>;

implementation

{ TStation }

procedure TStation.BuildMenu(Pallet: TPanel);
begin
  inherited BuildMenu(Pallet); ;
end;

constructor TStation.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_STATION);
end;

destructor TStation.Destroy;
begin
  inherited Destroy;
end;

end.
