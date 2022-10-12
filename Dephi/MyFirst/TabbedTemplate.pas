unit TabbedTemplate;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, FMX.Controls.Presentation,

//  lcc_comport,
  lcc_ethernet_client,
  lcc_ethernet_common,
  lcc_ethernet_http,
  lcc_ethernet_server,
  lcc_ethernet_tcp,
 // lcc_ethernet_websocket,

  lcc_cdi_parser_2,
  lcc_common_classes,
  lcc_gridconnect,
  lcc_node_messages_can_assembler_disassembler,
  lcc_threaded_circulararray,
  lcc_threaded_stringlist,
  lcc_xmlutilities,

  lcc_utilities,
  lcc_train_server,
  lcc_node,
  lcc_node_train,
  lcc_node_messages,
  lcc_node_manager,
  lcc_node_controller,
  lcc_node_commandstation,
  lcc_math_float16,
  lcc_defines,
  lcc_alias_server,
  lcc_alias_mappings,

  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

type
  TTabbedForm = class(TForm)
    HeaderToolBar: TToolBar;
    ToolBarLabel: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    GestureManager1: TGestureManager;
    Button1: TButton;
    IdTCPClient1: TIdTCPClient;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TabbedForm: TTabbedForm;

implementation

{$R *.fmx}

procedure TTabbedForm.Button1Click(Sender: TObject);
begin
  IdTCPClient1.Connect('192.168.0.38', 12021)
end;

procedure TTabbedForm.Button2Click(Sender: TObject);
begin
  IdTCPClient1.Disconnect;
end;

procedure TTabbedForm.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControl1.ActiveTab := TabItem1;
end;

procedure TTabbedForm.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
{$IFDEF ANDROID}
  case EventInfo.GestureID of
    sgiLeft:
    begin
      if TabControl1.ActiveTab <> TabControl1.Tabs[TabControl1.TabCount-1] then
        TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex+1];
      Handled := True;
    end;

    sgiRight:
    begin
      if TabControl1.ActiveTab <> TabControl1.Tabs[0] then
        TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex-1];
      Handled := True;
    end;
  end;
{$ENDIF}
end;

end.
