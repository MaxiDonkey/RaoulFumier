unit ScreenDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  {Vocal}
  uNewRecorder,
  {dev express}
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, dxSkinsCore, dxSkinDarkRoom,cxProgressBar, cxLabel, dxGDIPlusClasses,
  StdCtrls;

type
  TScreenDialog = class(TForm)
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure Initialize;
  public
    procedure DisplayToMonitor;
  end;

var
  ScreenDialog: TScreenDialog;

implementation

{$R *.dfm}

uses
  uRegistry, uRaoulDisplay, main, DisplayTextForm;

{ TScreenDialog }

procedure TScreenDialog.FormShow(Sender: TObject);
begin
  DisplayToMonitor;                    { --- Set windows size on active monitor }
  SetForegroundWindow(Handle)          { --- Display windows on foreground      }
end;

procedure TScreenDialog.DisplayToMonitor;
begin
  AlphaBlend      := True;
  AlphaBlendValue := 180;
  with screen, ToolsDisplay, Monitors[IndexMonitor] do begin
    Self.Left := Left + Abs(Width  - Self.Width)  div 2;
    Self.top  := Top  + Abs(Height - Self.Height) div 2
  end
end;

procedure TScreenDialog.Initialize;
begin
  FormStyle := fsStayOnTop;
  Position  := poDefaultPosOnly
end;

procedure TScreenDialog.FormCreate(Sender: TObject);
begin
  Initialize
end;

procedure TScreenDialog.FormClick(Sender: TObject);
begin
  with MainForm do SetFocus
end;

end.
