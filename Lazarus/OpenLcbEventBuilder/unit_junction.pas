unit unit_junction;

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
  unit_switchmachine,
  unit_turnout,
  unit_defines,
  unit_types,
  unit_base_location;

type

  { TJunction }

  TJunction = class(TBaseLocation)

  protected

  public

    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;

  TJunctionList = specialize TFPGInterfacedObjectList<TJunction>;

implementation

{ TJunction }

procedure TJunction.BuildMenu(Pallet: TPanel);
begin
  inherited BuildMenu(Pallet); ;
end;

constructor TJunction.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_JUNCTION);
end;

destructor TJunction.Destroy;
begin

  inherited Destroy;
end;

end.
