unit frame_workspace;

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
  StdCtrls,
  ActnList,
  Menus,
  TreeFilterEdit,
  fgl,
  unit_base_frame,
  unit_base_layout_item,
  unit_switchmachine,
  unit_popup_menus,
  unit_turnout,
  unit_workspace,
  unit_defines,
  unit_types,
  unit_layout;

type




  TLayoutLocation = class(TInterfacedObject)
    // Location
    //   Turnouts
    //     Actuator
    //        Stall Motor
    //        Solonoid
    //        Servo Motor
    //     Postion Feedback

    //   Control Panels
    //      PushButtons

    //   Sensor
    //      Sense Coil
    //      Digital

    //   Signals
    //      Signal Masts

    //   Animations
    //      Crossing Gates
    //      Lighting


  end;

  { TFrameWorkspace }

  TFrameWorkspace = class(TBaseFrame)
    ActionListWorkspace: TActionList;
    Action_create_lightbulb: TAction;
    Action_create_pushbutton: TAction;
    Action_create_control_panel: TAction;
    Action_create_sensor: TAction;
    Action_create_actuator: TAction;
    Action_create_junction: TAction;
    Action_create_station: TAction;
    Action_create_freightyard: TAction;
    Action_create_turnout: TAction;
    Action_create_town: TAction;
    Action_create_subdivision: TAction;
    Action_create_division: TAction;
    Action_create_layout: TAction;
    BitBtn_create_layout: TBitBtn;
    ImageListWorkspace: TImageList;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    PopupMenuControlPanel: TPopupMenu;
    PopupMenuTown: TPopupMenu;
    PopupMenuStation: TPopupMenu;
    PopupMenuFreightYard: TPopupMenu;
    Separator2: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Separator1: TMenuItem;
    Panel_treeview: TPanel;
    Panel_treeview_header: TPanel;
    PopupMenuSubDivision: TPopupMenu;
    PopupMenuDivision: TPopupMenu;
    PopupMenuLayout: TPopupMenu;
    PopupMenuWorkspace: TPopupMenu;
    TreeView_turnouts: TTreeView;
    procedure Action_create_divisionExecute(Sender: TObject);
    procedure Action_create_layoutExecute(Sender: TObject);
    procedure Action_create_subdivisionExecute(Sender: TObject);
    procedure TreeView_turnoutsEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure TreeView_turnoutsSelectionChanged(Sender: TObject);

  private
    FLayouts: TLayoutList;
    FMenuItemIndexes: TPopupMenuFactory;
    FPopupMenus: TPopupMenuFactory;
    FWorkspace: TWorkspace;

  protected
    property MenuItemIndexes: TPopupMenuFactory read FMenuItemIndexes write FMenuItemIndexes;
    property PopupMenuFactory: TPopupMenuFactory read FPopupMenus write FPopupMenus;

  public
    property Layouts: TLayoutList read FLayouts write FLayouts;
    property Workspace: TWorkspace read FWorkspace write FWorkspace;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TFrameTuroutClass = class of TFrameWorkspace;

implementation

{$R *.lfm}

{ TFrameWorkspace }


procedure TFrameWorkspace.Action_create_layoutExecute(Sender: TObject);
var
  TreeNode: TTreeNode;
  Layout: TLayout;
begin
  Layout := TLayout.Create(PopupMenuFactory);
  Layout.Name := 'New Layout';
  Workspace.Layouts.Add(Layout);

  TreeNode := TreeView_turnouts.Items.AddObject(nil, Layout.Name, Layout);
  TreeNode.MakeVisible;
  TreeView_turnouts.ClearSelection(False);
  TreeNode.Selected := True;
  TreeNode.EditText;
end;

procedure TFrameWorkspace.Action_create_subdivisionExecute(Sender: TObject);
begin

end;

procedure TFrameWorkspace.Action_create_divisionExecute(Sender: TObject);
begin

end;

procedure TFrameWorkspace.TreeView_turnoutsEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  TBaseLayoutItem( Node.Data).Name := S;
end;

procedure TFrameWorkspace.TreeView_turnoutsSelectionChanged(Sender: TObject);
var
  TreeNode: TTreeNode;
begin
  TreeNode := TreeView_turnouts.Selected;
  if Assigned(TreeNode) and (TreeView_turnouts.SelectionCount = 1) then
    TreeView_turnouts.PopupMenu := TBaseLayoutItem( TreeNode.Data).PopupMenu
  else
    TreeView_turnouts.PopupMenu := nil;
end;

constructor TFrameWorkspace.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  PopupMenuFactory := TPopupMenuFactory.Create(Self, ActionListWorkspace);
  FLayouts := TLayoutList.Create;
  FWorkspace := TWorkspace.Create(PopupMenuFactory);
  (ActionListWorkspace.ActionByName('Action_create_layout') as TAction).ImageIndex := 1;

  BitBtn_create_layout.Action := (ActionListWorkspace.ActionByName('Action_create_layout') as TAction);
end;

destructor TFrameWorkspace.Destroy;
begin
  Layouts.Free;
  PopupMenuFactory.Free;
  Workspace.Free;
  inherited Destroy;
end;

end.
