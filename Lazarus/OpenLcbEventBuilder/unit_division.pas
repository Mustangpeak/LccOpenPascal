unit unit_division;

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
  unit_subdivision,
  unit_base_layout_item,
  unit_defines,
  unit_types,
  unit_turnout;

type

  { TDivision }

  TDivision = class(TBaseLayoutItem)

  private
    FSubDivisions: TSubDivisionList;

  protected

  public
    property SubDivisions: TSubDivisionList read FSubDivisions write FSubDivisions;

    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;

  TDivisionList = specialize TFPGInterfacedObjectList<TDivision>;

implementation

procedure TDivision.BuildMenu(Pallet: TPanel);
begin

end;


constructor TDivision.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
  FSubDivisions := TSubDivisionList.Create;
  PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_DIVISION);
end;

destructor TDivision.Destroy;
begin
  SubDivisions.Free;
  inherited Destroy;
end;

end.
