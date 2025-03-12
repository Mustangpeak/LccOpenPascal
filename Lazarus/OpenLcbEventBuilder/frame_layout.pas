unit frame_layout;

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
  unit_base_layout,
  unit_switchmachine,
  unit_turnout,
  unit_workspace,
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

  { TFrameLayout }

  TFrameLayout = class(TBaseFrame)
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
    ActionListLayout: TActionList;
    BitBtn_create_layout: TBitBtn;
    ImageListLayout: TImageList;
    MenuItem1: TMenuItem;
    MenuItem_layout_create_division: TMenuItem;
    Panel_treeview: TPanel;
    Panel_treeview_header: TPanel;
    PopupMenu_division: TPopupMenu;
    PopupMenu_subdivision: TPopupMenu;
    PopupMenu3: TPopupMenu;
    PopupMenu4: TPopupMenu;
    PopupMenu_layout: TPopupMenu;
    TreeView_turnouts: TTreeView;
    procedure Action_create_layoutExecute(Sender: TObject);
    procedure TreeView_turnoutsEdited(Sender: TObject; Node: TTreeNode;
      var S: string);

  private
    FLayouts: TLayoutList;

  public
    property Layouts: TLayoutList read FLayouts write FLayouts;

    constructor Create(TheOwner: TComponent; AWorkspace: TWorkspace); override;
    destructor Destroy; override;
  end;

  TFrameTuroutClass = class of TFrameLayout;

implementation

{$R *.lfm}

{ TFrameLayout }


procedure TFrameLayout.Action_create_layoutExecute(Sender: TObject);
var
  TreeNode: TTreeNode;
  Layout: TLayout;
begin
  Layout := TLayout.Create;
  Layout.Name := 'New Layout';
  Workspace.Layouts.Add(Layout);

  TreeNode := TreeView_turnouts.Items.AddObject(nil, Layout.Name, Layout);
  TreeNode.MakeVisible;
  TreeNode.Selected := True;
  TreeNode.EditText;
end;

procedure TFrameLayout.TreeView_turnoutsEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  TBaseLayout( Node.Data).Name := S;
end;

constructor TFrameLayout.Create(TheOwner: TComponent; AWorkspace: TWorkspace);
begin
  inherited Create(TheOwner, AWorkspace);
  FLayouts := TLayoutList.Create;
end;

destructor TFrameLayout.Destroy;
begin
  Layouts.Free;
  inherited Destroy;
end;

end.
