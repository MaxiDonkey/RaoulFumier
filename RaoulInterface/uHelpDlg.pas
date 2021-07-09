unit uHelpDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, uDosUtils, WebAPIs,

  {DevExpress}
  cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore,
  dxSkinDarkRoom, cxCheckBox;

type
  THelpDlg = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    Memo1: TRichEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Label4: TLabel;
    Panel5: TPanel;
    Panel6: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    Panel7: TPanel;
    Panel8: TPanel;
    Label6: TLabel;
    cxCheckBox1: TcxCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure RetrieveFiles(const index: Integer; var TextFileName, DatFileName: string);
    procedure LoadHelpFiles;
    procedure Initialize;
    procedure AutoOpen;
    function  IndexMonitor: Integer;
    function  ActualWidth: Integer;
    function  ActualHeight: Integer;
    procedure WindowSizeAdaptator;

  public
    class procedure HelpReload;
    class procedure HelpDefine;
    class function RetrieveBootProcess: Boolean;  { --- Calcul delay Boot process }
    class function IsBootProcess: Boolean;
  end;

var
  HelpDlg: THelpDlg = nil;

implementation

{$R *.dfm}

uses
  uEliteHelp, uRaoulDisplay, uNewRecorder, uWebTools, StrCopyUtils, uRegistry,
  uEliteUtils, uStatusReader;

const
  BasicWidth        = 1700;
  BasicHeight       = 732;
  BasicScreenWidth  = 1920;
  BasicScreenHeight = 1080;

procedure THelpDlg.FormCreate(Sender: TObject);
begin
  AlphaBlend      := True;                    
  AlphaBlendValue := 160;
  with cxCheckBox1 do Checked := KeyReadBoolean(AppKey, 'HideAutoOpen');
end;

procedure THelpDlg.FormShow(Sender: TObject);
begin
  if IsEliteRunningUTLS then begin
      if EliteStatus.Docked or EliteStatus.Docked
        then AlphaBlendValue := 210
        else AlphaBlendValue := 100
    end else AlphaBlendValue := 180;
  WindowSizeAdaptator;
  SetForegroundWindow( HelpDlg.Handle );
  Recorder.HelpActivate
end;

procedure THelpDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Recorder.HelpDeactivate;
  with TalkativeFacade do if isEliteMode then EliteForeGround
end;

procedure THelpDlg.Initialize;
begin
  Helps := THelps.Create;
  { --- False -> not ReadOnly }
  THelpView.Initialize(ListBox1, ListBox2, Memo1, Label1, Label2, cxCheckBox1 (*,False*));
  Helps.Current := hk_main;
  HelpView.SetFormDlg(HelpDlg);
  HelpView.FirstDisplay;
  AutoOpen
end;

class procedure THelpDlg.HelpDefine;
begin
  if Assigned(HelpDlg) then with HelpDlg do Initialize
end;

const
  HelpDistFolder = 'http://www.maxidonkey.com/raoul/help';

function HelpFolder:string;
begin
  Result := Format('%s\Help', [ExtractFileDir(Application.ExeName)])
end;

procedure THelpDlg.LoadHelpFiles;
{ TODO move to uEliteHelp }
var
  i        : Integer;
  Txt, Dat : string;
begin
  try MkDir('Help') except end;
  try
    for i := Integer(Low(THelpKind)) to Integer(High(THelpKind)) do
    try
      RetrieveFiles(i, Txt, Dat);
       TWebTools.DownloadFromHttp(
         Format('%s/%s', [HelpDistFolder, Txt]) ,
         Format('%s/%s', [HelpFolder,     Txt]) );
       TWebTools.DownloadFromHttp(
         Format('%s/%s', [HelpDistFolder, Dat]) ,
         Format('%s/%s', [HelpFolder,     Dat]) );
    except
    end;

  finally
  end
end;

procedure THelpDlg.RetrieveFiles(const index: Integer; var TextFileName,
  DatFileName: string);
var
  Buffer : string;
begin
  Buffer := HelpSrc[ THelpKind(index) ];
  TextFileName := GetBeforStr(Buffer, ';');
  DatFileName  := GetAfterStr(Buffer, ';');
end;

class procedure THelpDlg.HelpReload;
begin
  if Assigned(HelpDlg) then with HelpDlg do
    if InternetCheck then LoadHelpFiles;
end;

procedure THelpDlg.AutoOpen;
begin
  with HelpView do AutoOpen;
end;

class function THelpDlg.RetrieveBootProcess: Boolean;
begin
  { --- less than 3 minutes }
  Result := DelaySinceBoot < EncodeTime(0,0,30,0);
  KeyWrite(AppKey, 'BootState', Result)
end;

class function THelpDlg.IsBootProcess: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'BootState')
end;

function THelpDlg.IndexMonitor: Integer;
begin
  Result := KeyReadInt(BufferKey, 'IndexMonitor', 0)
end;

function THelpDlg.ActualWidth: Integer;
begin
  Result := Screen.Monitors[IndexMonitor].Width;
end;

function THelpDlg.ActualHeight: Integer;
begin
  Result := Screen.Monitors[IndexMonitor].Height;
end;

procedure THelpDlg.WindowSizeAdaptator;
var
  ATop  : Integer;
  ALeft : Integer;
begin
  Self.Width  := Trunc( (ActualWidth  / BasicScreenWidth)  * BasicWidth  );
  Self.Height := Trunc( (ActualHeight / BasicScreenHeight) * BasicHeight );
  ALeft       := (ActualWidth -  Self.Width)  div 2;
  ATop        := (ActualHeight - Self.Height) div 2;
  with Screen.Monitors[IndexMonitor] do begin
    Self.Left := Left + ALeft;
    Self.Top  := Top + ATop;
  end;
end;

initialization
  KeyWrite(AppKey, 'HelpVisible', 0)
finalization
end.
