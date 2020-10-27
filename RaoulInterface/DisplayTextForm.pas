unit DisplayTextForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  {dev express}
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, dxSkinsCore, dxSkinDarkRoom,cxProgressBar, cxLabel, dxGDIPlusClasses;

type
  TTalkativeBox = class(TForm)
    Panel2: TPanel;
    Panel4: TPanel;
    FreePanel: TPanel;
    LabelPanel: TPanel;
    cxLabel1: TcxLabel;
    Panel1: TPanel;
    SoundProgress: TcxProgressBar;
    cxLabel2: TcxLabel;
    Panel3: TPanel;
    IVerte: TImage;
    IRouge: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure Initialize;
  public
    procedure DisplayToMonitor;
  end;

var
  TalkativeBox: TTalkativeBox;

implementation

{$R *.dfm}

uses
  main, uRaoulDisplay, ScreenDlg, EliteBindingsTools;

procedure TTalkativeBox.FormShow(Sender: TObject);
begin
  TTalkativeFacade.RetrieveRegValue;   { --- Reload properties values from registry}
  DisplayToMonitor;                    { --- Set windows size on active monitor }
  SetForegroundWindow(Handle)          { --- Display windows on foreground  }
end;

procedure TTalkativeBox.Initialize;
begin
  TalkativeText  := cxLabel1;
  TalkativeGauge := SoundProgress;
  TalkativeFree  := FreePanel;
  TalkativeSepa  := Panel1;
  TalkativePate  := LabelPanel;
  TalkativeCmd   := cxLabel2;
  TalkativeSepb  := Panel3;
  TalkativeGreen := IVerte;
  TalkativeRed   := IRouge;
end;

procedure TTalkativeBox.FormCreate(Sender: TObject);
begin
  Initialize
end;

procedure TTalkativeBox.DisplayToMonitor;
begin
  with screen, ToolsDisplay, Monitors[IndexMonitor] do begin
    Self.Left   := Left + 3;
    Self.Width  := Width - 6;
    Self.Height := 150; 
    Self.top    := Top  + Integer(DisplayVertic) * (Height - Self.Height)
  end
end;

var
  ExtendedVKeys : set of byte = [
      VK_Up,    VK_Down,     VK_Left,    VK_Right,    VK_Home,
      VK_End,   VK_Prior,    VK_Next,    VK_Insert,   VK_Delete
  ];

procedure SendTest; begin
    try EliteForeGround; except end;
    keybd_event(VK_RCONTROL, MapVirtualKey(VK_RCONTROL,   0), KEYEVENTF_KEYDOWN, 0);
    keybd_event(VK_LMENU, MapVirtualKey(VK_LMENU,   0), KEYEVENTF_KEYDOWN, 0);
    keybd_event(VkKeyScan('T'), MapVirtualKey(VkKeyScan('T'),   0), KEYEVENTF_KEYDOWN, 0);
    Sleep( 10 );
    keybd_event(VkKeyScan('T'), MapVirtualKey(VkKeyScan('T'),   0), KEYEVENTF_KEYUP, 0);
    keybd_event(VK_LMENU, MapVirtualKey(VK_LMENU,   0), KEYEVENTF_KEYUP, 0);
    keybd_event(VK_RCONTROL, MapVirtualKey(VK_RCONTROL,   0), KEYEVENTF_KEYUP, 0)
  end;

end.
