unit unit_animations;

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
  unit_defines,
  unit_types,
  unit_popup_menus,
  unit_base_layout_item,
  my_fgl;

type

  { TAnimation }

  TAnimation = class(TBaseLayoutItem)

  protected

  public
    constructor Create(APopupMenuFactory: TPopupMenuFactory); override;
    destructor Destroy; override;

    procedure BuildMenu(Pallet: TPanel); override;

  end;

  { TAnimationCrossing }

  TAnimationCrossing = class(TAnimation)

  end;

  { TAnimationLights }

  TAnimationLights = class(TAnimation)


  end;

  TAnimationList = specialize TFPGInterfacedObjectList<TAnimation>;

implementation

{ TAnimation }

procedure TAnimation.BuildMenu(Pallet: TPanel);
begin

end;

constructor TAnimation.Create(APopupMenuFactory: TPopupMenuFactory);
begin
  inherited Create(APopupMenuFactory);
 // PopupMenu := APopupMenuFactory.GetPopupMenu(POPUP_);
end;

destructor TAnimation.Destroy;
begin
  inherited Destroy;
end;

end.
