unit unit_layout;

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
  unit_defines,
  unit_types,
  unit_base_layout_item,
  unit_switchmachine;

type

  { TLayout }

  TLayout = class(TBaseLayoutItem)

  private
    FDivisions: TDivisionList;
  protected


  public
    property Divisions: TDivisionList read FDivisions write FDivisions;

    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;

  TLayoutList = specialize TFPGInterfacedObjectList<TLayout>;

implementation

{ TLayout }

procedure TLayout.BuildMenu(Pallet: TPanel);
begin

end;


constructor TLayout.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  FDivisions := TDivisionList.Create;
  PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_LAYOUT);

end;

destructor TLayout.Destroy;
begin
  Divisions.Free;
  inherited Destroy;
end;

end.
