unit unit_base_layout_item;

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
  unit_types,
  unit_defines;

type

  { TBaseLayoutItem }

  TBaseLayoutItem = class(TInterfacedObject)

  private
    FName: string;
    FPopupMenu: TPopupMenu;
    procedure SetPopupMenu(AValue: TPopupMenu);
  protected

  public

    property Name: string read FName write FName;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;

    constructor Create(APopupMenuFactory: TPopupMenuFactory); reintroduce; virtual;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); virtual; abstract;

  end;

implementation

{ TBaseLayoutItem }

procedure TBaseLayoutItem.SetPopupMenu(AValue: TPopupMenu);
begin
  if FPopupMenu=AValue then Exit;
  FPopupMenu:=AValue;
end;

constructor TBaseLayoutItem.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create;
end;

destructor TBaseLayoutItem.Destroy;
begin
  inherited Destroy;
end;



end.
