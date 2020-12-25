unit uGaussDisplay;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, uRegistry, dxGDIPlusClasses;

type
  TGaussDisplayForm = class(TForm)
    Panel2: TPanel;
    Panel6: TPanel;
    Label4: TLabel;
    Panel9: TPanel;
    Label1: TLabel;
    Panel12: TPanel;
    Panel7: TPanel;
    Label5: TLabel;
    Panel10: TPanel;
    Label2: TLabel;
    Panel13: TPanel;
    Panel8: TPanel;
    Label6: TLabel;
    Panel11: TPanel;
    Label3: TLabel;
    Panel14: TPanel;
    Panel1: TPanel;
    RichEdit: TRichEdit;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Image1: TImage;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FClearIndic : Boolean;
    function GetCalcFormVisible: Boolean;
    procedure SetCalcFormVisible(const Value: Boolean);
  public
    procedure UpdateCadenas;
    procedure Clear;
    procedure Restore;
    property  CalcFormVisible: Boolean read GetCalcFormVisible write SetCalcFormVisible;
  end;

var
  GaussDisplayForm: TGaussDisplayForm;

implementation

{$R *.dfm}

uses
  uGauss, uRaoulDisplay, uEliteUtils;

procedure TGaussDisplayForm.FormCreate(Sender: TObject);
begin
  RichEdit.Clear;
  with RichEdit do begin
    ReadOnly := True;
    Color    := $00141414;
  end;
  AlphaBlendValue := 180;
  AlphaBlend      := True;
  Ctl3D           := False;
  Color           := $00141414;
  FormStyle       := fsStayOnTop;
  with Font do begin
    Color := clGray;
    Size  := 12;
    Style := [fsBold];
  end;
  Position := poDesigned;
  Left    := 1260;
  Height  := 725;
  Top     := 250;
  Width   := 621;
end;

procedure TGaussDisplayForm.FormShow(Sender: TObject);
begin
  if IsEliteRunningUTLS then AlphaBlendValue := 120 else AlphaBlendValue := 180;
  FClearIndic := False;
  XVarCont := Label4;
  YVarCont := Label5;
  ZVarCont := Label6;
  with Label4 do Caption := KeyReadString(BufferKey, 'XData');
  with Label5 do Caption := KeyReadString(BufferKey, 'YData');
  with Label6 do Caption := KeyReadString(BufferKey, 'ZData');

  UpdateCadenas;
  with RichEdit, Lines do try LoadFromFile('Calc.txt') except end;
  { --- Placer Elite en premier si le jeu est lancé et que le mode Elite est actif }
  with TalkativeFacade do if isEliteMode then EliteForeGround;
  { --- Placer la fenêtre au premier plan }
  if not CalcFormVisible then begin
    CalcFormVisible := True;
    SetForegroundWindow(Handle);
  end;
  Windows.SetFocus(0)
end;

procedure TGaussDisplayForm.UpdateCadenas;
begin
  Image1.Visible := KeyReadInt(BufferKey, 'IndexMetier') <> 2
end;


procedure TGaussDisplayForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  CalcFormVisible := False;
  RichEdit.Lines.SaveToFile('Calc.txt');
  EliteSetFocus;
end;

procedure TGaussDisplayForm.Clear;
begin
  with RichEdit do begin
    Clear;
    FClearIndic := True
  end
end;

procedure TGaussDisplayForm.Restore;
begin
  if FClearIndic then with RichEdit, Lines do
    try LoadFromFile('Calc.txt') except end
end;

function TGaussDisplayForm.GetCalcFormVisible: Boolean;
begin
  Result := KeyReadBoolean(BufferKey, 'CalcFormVisible')
end;

procedure TGaussDisplayForm.SetCalcFormVisible(const Value: Boolean);
begin
  KeyWrite(BufferKey, 'CalcFormVisible', Value)
end;

initialization
  KeyWrite(BufferKey, 'CalcFormVisible', False);
  KeyWrite(BufferKey, 'MetierBeforeGauss', 0);
finalization
end.
