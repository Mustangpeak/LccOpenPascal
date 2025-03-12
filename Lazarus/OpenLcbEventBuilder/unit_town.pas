unit unit_town;

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

  { TTown }

  TTown = class(TBaseLocation)

  protected

  public

    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;

  TTownList = specialize TFPGInterfacedObjectList<TTown>;

implementation

{ TTown }

procedure TTown.BuildMenu(Pallet: TPanel);
begin
  inherited BuildMenu(Pallet);
end;

constructor TTown.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_TOWN);
end;

destructor TTown.Destroy;
begin
  inherited Destroy;
end;

end.

