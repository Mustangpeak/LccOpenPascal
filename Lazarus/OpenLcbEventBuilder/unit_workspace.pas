unit unit_workspace;

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
  unit_popup_menus,
  my_fgl,
  unit_division,
  unit_turnout,
  unit_base_layout_item,
  unit_layout,
  unit_defines,
  unit_types,
  unit_switchmachine;

type

  { TLayout }

  { TWorkspace }

  TWorkspace = class(TBaseLayoutItem)

  private
    FLayouts: TLayoutList;

  protected

  public
    property Layouts: TLayoutList read FLayouts write FLayouts;

    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;

implementation

{ TWorkspace }

procedure TWorkspace.BuildMenu(Pallet: TPanel);
begin

end;


constructor TWorkspace.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  FLayouts := TLayoutList.Create;
  PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_WORKSPACE);
end;

destructor TWorkspace.Destroy;
begin
  Layouts.Free;
  inherited Destroy;
end;

end.

