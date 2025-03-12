unit unit_yard;

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
  unit_types,
  unit_base_location,
  unit_turnout;

type

  { TStation }

  { TFreightYard }

  TFreightYard = class(TBaseLocation)

  private

  protected

  public

    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;

  TFreightYardList = specialize TFPGInterfacedObjectList<TFreightYard>;

implementation

{ TFreightYard }

procedure TFreightYard.BuildMenu(Pallet: TPanel);
begin
  inherited BuildMenu(Pallet);
end;

constructor TFreightYard.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_FREIGHTYARD);
end;

destructor TFreightYard.Destroy;
begin
  inherited Destroy;
end;

end.
