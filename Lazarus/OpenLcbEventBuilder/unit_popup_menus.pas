unit unit_popup_menus;

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
  unit_types;

const

  POPUP_WORKSPACE = 0;
  POPUP_LAYOUT = 1;
  POPUP_DIVISION = 2;
  POPUP_SUBDIVISION = 3;
  POPUP_TOWN = 4;
  POPUP_JUNCTION = 5;
  POPUP_STATION = 6;
  POPUP_FREIGHTYARD = 7;
  POPUP_TURNOUT = 8;
  POPUP_ACUTACTOR = 9;
  POPUP_SENSOR = 10;
  POPUP_PANEL = 11;
  POPUP_PUSHBUTTON = 12;
  POPUP_LIGHT = 13;

  POPUP_COUNT = 14;


type

  TPopupMenuArray = array of TPopupMenu;

  TMenuItemTagIndexArray = array of integer;
  // Value that matches the Tag field in the ActionList associated with this menu Item
  TMenuItemArray = array of TMenuItemTagIndexArray;


  { TPopupMenuFactory }

  TPopupMenuFactory = class(TInterfacedObject)

  private
    FMenuItems: TMenuItemArray;
    FPopupMenus: TPopupMenuArray;

  public

    property PopupMenus: TPopupMenuArray read FPopupMenus write FPopupMenus;
    property MenuItems: TMenuItemArray read FMenuItems write FMenuItems;

    constructor Create(AnOwner: TComponent; Actions: TActionList);

    function GetPopupMenu(PopupIndex: Integer): TPopupMenu;   // POPUP_XXXXX constants

  end;

implementation

{ TPopupMenuFactory }

constructor TPopupMenuFactory.Create(AnOwner: TComponent; Actions: TActionList);
var
  LocalMenuItems: array of TMenuItem;
  iPopupMenu, iMenuItem, iActionItem: integer;
begin

  SetLength(FMenuItems, POPUP_COUNT);

  SetLength(MenuItems[POPUP_WORKSPACE], 1);
  MenuItems[POPUP_WORKSPACE][0] := POPUP_LAYOUT;

  SetLength(MenuItems[POPUP_LAYOUT], 2);
  MenuItems[POPUP_LAYOUT][0] := POPUP_DIVISION;
  MenuItems[POPUP_LAYOUT][1] := POPUP_SUBDIVISION;

  SetLength(MenuItems[POPUP_DIVISION], 5);
  MenuItems[POPUP_DIVISION][0] := POPUP_SUBDIVISION;
  MenuItems[POPUP_DIVISION][1] := POPUP_TOWN;
  MenuItems[POPUP_DIVISION][POPUP_DIVISION] :=
    POPUP_JUNCTION;
  MenuItems[POPUP_DIVISION][3] := POPUP_FREIGHTYARD;
  MenuItems[POPUP_DIVISION][4] := POPUP_STATION;


  SetLength(MenuItems[POPUP_SUBDIVISION], 5);
  MenuItems[POPUP_SUBDIVISION][0] := POPUP_TOWN;
  MenuItems[POPUP_SUBDIVISION][1] := POPUP_JUNCTION;
  MenuItems[POPUP_SUBDIVISION][2] := POPUP_FREIGHTYARD;
  MenuItems[POPUP_SUBDIVISION][POPUP_SUBDIVISION] :=
    POPUP_STATION;
  MenuItems[POPUP_SUBDIVISION][4] := POPUP_TURNOUT;

  SetLength(MenuItems[POPUP_TOWN], POPUP_TOWN);
  MenuItems[POPUP_TOWN][0] := POPUP_JUNCTION;
  MenuItems[POPUP_TOWN][1] := POPUP_FREIGHTYARD;
  MenuItems[POPUP_TOWN][2] := POPUP_STATION;
  MenuItems[POPUP_TOWN][3] := POPUP_TURNOUT;

  SetLength(MenuItems[POPUP_JUNCTION], 4);
  MenuItems[POPUP_JUNCTION][0] := POPUP_JUNCTION;
  MenuItems[POPUP_JUNCTION][1] := POPUP_FREIGHTYARD;
  MenuItems[POPUP_JUNCTION][2] := POPUP_STATION;
  MenuItems[POPUP_JUNCTION][3] := POPUP_TURNOUT;

  SetLength(MenuItems[POPUP_STATION], 4);
  MenuItems[POPUP_STATION][0] := POPUP_JUNCTION;
  MenuItems[POPUP_STATION][1] := POPUP_FREIGHTYARD;
  MenuItems[POPUP_STATION][2] := POPUP_STATION;
  MenuItems[POPUP_STATION][3] := POPUP_TURNOUT;

  SetLength(MenuItems[POPUP_FREIGHTYARD], 4);
  MenuItems[POPUP_FREIGHTYARD][0] := POPUP_JUNCTION;
  MenuItems[POPUP_FREIGHTYARD][1] := POPUP_FREIGHTYARD;
  MenuItems[POPUP_FREIGHTYARD][2] := POPUP_STATION;
  MenuItems[POPUP_FREIGHTYARD][3] := POPUP_TURNOUT;

  SetLength(MenuItems[POPUP_TURNOUT], 2);
  MenuItems[POPUP_TURNOUT][0] := POPUP_ACUTACTOR;
  MenuItems[POPUP_TURNOUT][1] := POPUP_SENSOR;

  SetLength(MenuItems[POPUP_ACUTACTOR], 0);

  SetLength(MenuItems[POPUP_SENSOR], 0);

  SetLength(MenuItems[POPUP_PANEL], 2);
  MenuItems[POPUP_PANEL][0] := POPUP_PUSHBUTTON;
  MenuItems[POPUP_PANEL][1] := POPUP_LIGHT;

  SetLength(MenuItems[POPUP_PUSHBUTTON], 1);
  MenuItems[POPUP_PUSHBUTTON][1] := POPUP_LIGHT;

  SetLength(MenuItems[POPUP_LIGHT], 0);


  SetLength(FPopupMenus, POPUP_COUNT);

  for iPopupMenu := 0 to POPUP_COUNT - 1 do
  begin
    PopupMenus[iPopupmenu] := TPopupMenu.Create(AnOwner);

    LocalMenuItems := nil;
    SetLength(LocalMenuItems, Length(MenuItems[iPopupmenu]));
    for iMenuItem := 0 to Length(MenuItems[iPopupmenu])- 1 do
    begin
      LocalMenuItems[iMenuItem] := TMenuItem.Create(PopupMenus[iPopupMenu]);


      for iActionItem := 0 to Actions.ActionCount - 1 do
      begin
        if Actions.Actions[iActionItem].Tag = MenuItems[iPopupMenu][iMenuItem] then
        begin
          LocalMenuItems[iMenuItem].Action := Actions.Actions[iActionItem];
          Break;
        end;
      end;

    end;

    PopupMenus[iPopupMenu].Items.Add(LocalMenuItems);

  end;

end;

function TPopupMenuFactory.GetPopupMenu(PopupIndex: Integer): TPopupMenu;
begin
  if (PopupIndex >= 0) and (PopupIndex < Length(PopupMenus)) then
    Result := PopupMenus[PopupIndex]
  else
    Result := nil;
end;


end.
