unit uRaoulDisplay;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uRegistry, uEmbendedAncestor, uPickerForm,
  StrUtils,
  {Help}
  uEliteHelp,
  {vocal}
  uNewRecorder,
  {Elite bindings}
  uEliteManager,
  {ShortCuts}
  SendKey32,
  {versioning}
  uVersion,
  {dev express}
  cxLabel, cxProgressBar;

type
  TDisplayVertic = (dv_top, dv_bottom);
  TDisplayHoriz  = (dh_left, dh_right);

  TToolsDisplay = class
  private
    FStarter: Boolean;
    FIndexMonitor: Integer;
    FDisplayVertic: TDisplayVertic;
    FDisplayHoriz: TDisplayHoriz;
    procedure SetIndexMonitor(const Value: Integer);
    procedure GoToMonitor(const index: Integer);
    procedure SetDisplayVertic(const Value: TDisplayVertic);
    procedure SetDisplayHoriz(const Value: TDisplayHoriz);
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    procedure KeyInitialize(const KeyButton: TButton);
    procedure EnlargeDisplay;
    procedure GotoNextMonitor;
    procedure GotoPrevMonitor;

    property IndexMonitor: Integer read FIndexMonitor write SetIndexMonitor;
    property DisplayVertic: TDisplayVertic read FDisplayVertic write SetDisplayVertic;
    property DisplayHoriz: TDisplayHoriz read FDisplayHoriz write SetDisplayHoriz;

    constructor Create;
    class procedure Initialize;
  end;

  TTalkativeFacade = class
  private
    FStarter: Boolean;
    function  GetText: string;
    procedure SetText(const Value: string);
    function  GetActiveSoundView: Boolean;
    procedure SetActiveSoundView(const Value: Boolean);
    function  GetFreePanelView: Boolean;
    procedure SetFreePanelView(const Value: Boolean);
    procedure SetSoundLevel(const Value: Integer);
    function  GetTextView: Boolean;
    procedure SetTextView(const Value: Boolean);
    procedure SetTryRecognize(const Value: Boolean);
    function  GetTryRecognize: Boolean;
    function  GetCommandText: string;
    procedure SetCommandText(const Value: string);
    function  GetCommandView: Boolean;
    procedure SetCommandView(const Value: Boolean);
    function  GetDisplayVertic: TDisplayVertic;
    procedure SetDisplayVertic(const Value: TDisplayVertic);
    function  GetDisplayHoriz: TDisplayHoriz;
    procedure SetDisplayHoriz(const Value: TDisplayHoriz);
    function  GetIndexMonitor: Integer;
    procedure SetIndexMonitor(const Value: Integer);
    function  GetTalkativeView: Boolean;
    procedure SetTalkativeView(const Value: Boolean);
    function  GetTextColor: TColor;
    procedure SetTextColor(const Value: TColor);

  private
    procedure ShowForm(const DisplayedForm, ContainerForm: TForm);
    procedure AlignLeft;
    procedure AlignRight;
    procedure AlignTop;
    procedure AlignBottom;

  public
    procedure Initialize_;
    procedure PostInitialize;
    procedure TextColoration(const Taux: Double; const ASt: string);
    function  isPickerGridVisible:Boolean;
    function  isPickerPointVisible:Boolean;
    function  isEliteMode:Boolean;

    procedure FocusToRaoul;
    procedure ShowDialog(const Message: string; MessageType: TResponseType);
    procedure ShowSensibility;
    procedure ShowClosePanel;
    procedure ShowHelp;

    property TalkativeView: Boolean read GetTalkativeView write SetTalkativeView; {REG}
    property Text: string read GetText write SetText;
    property TextColor: TColor read GetTextColor write SetTextColor;
    property TextView: Boolean read GetTextView write SetTextView; {REG}
    property CommandText: string read GetCommandText write SetCommandText; {REG}
    property CommandView: Boolean read GetCommandView write SetCommandView;  {REG}
    property SoundView: Boolean read GetActiveSoundView write SetActiveSoundView; {REG}
    property FreePanelView: Boolean read GetFreePanelView write SetFreePanelView; {REG}
    property SoundLevel: Integer write SetSoundLevel;
    property TryRecognize: Boolean read GetTryRecognize write SetTryRecognize;
    property DisplayVertic: TDisplayVertic read GetDisplayVertic write SetDisplayVertic; {REG}
    property DisplayHoriz: TDisplayHoriz read GetDisplayHoriz write SetDisplayHoriz; {REG}

    { --- with SetIndexMoniotor GotoMonitor invoked }
    property IndexMonitor: Integer read GetIndexMonitor write SetIndexMonitor; {REG}

    constructor Create;
    class procedure RetrieveRegValue;
  end;

  TFunctionment = class
  private
    procedure DoAppClose(Sender: TObject);
    procedure DoSensibility(Sender: TObject);
    procedure DoOk(Sender: TObject);
    procedure DoCancel(Sender: TObject);
    procedure DoRetour(Sender: TObject);
    procedure DoYes(Sender: TObject);
    procedure DoNo(Sender: TObject);
    procedure DoEchap(Sender: TObject);
    procedure DoMouseLeft(Sender: TObject);
    procedure DoMouseRight(Sender: TObject);
    procedure DoMouseDouble(Sender: TObject);
    procedure DoMouseMiddle(Sender: TObject);

    procedure DoAppVersionShow(Sennder: TObject);

    procedure DoHautesPerformances(Sender: TObject);
    procedure DoMicroCasque(Sender: TObject);
    procedure DoMicroStudio(Sender: TObject);
    procedure DoMicroEnceinte(Sender: TObject);
    procedure DoPickerGridShow(Sender: TObject);
    procedure DoPickerGridClose(Sender: TObject);
    procedure DoPickerGridMonitorPrev(Sender: TObject);
    procedure DoPickerGridMonitorNext(Sender: TObject);
    procedure DoPickerGridPointShow(Sender: TObject);
    procedure DoPickerGridPointHide(Sender: TObject);
    procedure DoPickerGridPointPrev(Sender: TObject);
    procedure DoPickerGridPointNext(Sender: TObject);
    procedure DoPickerGridPointSelect(Sender: TObject);
    procedure DoPickerGridPointAdd(Sender: TObject);
    procedure DoPickerGridPointClear(Sender: TObject);

    procedure DoPickerGridWhite(Sender: TObject);
    procedure DoPickerGridGray(Sender: TObject);
    procedure DoPickerGridBlack(Sender: TObject);
    procedure DoPickerGridRed(Sender: TObject);
    procedure DoPickerGridGreen(Sender: TObject);
    procedure DoPickerGridBlue(Sender: TObject);
    procedure DoPickerGridYellow(Sender: TObject);
    procedure DoPickerGridOrange(Sender: TObject);
    procedure DoPickerGridPink(Sender: TObject);

    procedure DoRaoulIntRight(Sender: TObject);
    procedure DoRaoulIntLeft(Sender: TObject);
    procedure DoRaoulIntTop(Sender: TObject);
    procedure DoRaoulIntBottom(Sender: TObject);
    procedure DoRaoulIntCmdShow(Sender: TObject);
    procedure DoRaoulIntCmdHide(Sender: TObject);
    procedure DoRaoulIntTextShow(Sender: TObject);
    procedure DoRaoulIntTextHide(Sender: TObject);
    procedure DoRaoulIntVumetreShow(Sender: Tobject);
    procedure DoRaoulIntVumetreHide(Sender: TObject);

  public
    procedure Initialize_;

    constructor Create;
    class procedure Initialize;
    class procedure Finalize;
  end;

{gloab acces}
var
  ToolsDisplay    : TToolsDisplay;
  TalkativeFacade : TTalkativeFacade;
  Functionment    : TFunctionment;

{Intern acces}
var
  KeyBackFunc       : TNotifyEvent = nil;
  KeyReturnFunc     : TNotifyEvent = nil;
  KeyCtrlReturnFunc : TNotifyEvent = nil;
  TalkativeText     : TcxLabel;
  TalkativeSepa     : TPanel;
  TalkativePate     : TPanel;
  TalkativeGauge    : TcxProgressBar;
  TalkativeFree     : TPanel;
  TalkativeCmd      : TcxLabel;
  TalkativeSepb     : TPanel;
  TalkativeGreen    : TImage;
  TalkativeRed      : TImage;

implementation

uses
  Main, DisplayTextForm, ScreenDlg,
  uEmbendedDlg, uEmbendedSensibility, cxEdit, uScreenTools, uEliteUtils,
  {acces SQL distant}                                                         
  uRaoulDB;

function ModVal(const Value: Integer; Divisor: Integer): Integer;
begin
  Result := Value mod Divisor
end;

function SuccModVal(const Value: Integer; Divisor: Integer): Integer;
begin
  Result := (Value + 1) mod Divisor
end;

function PredModVal(const Value: Integer; Divisor: Integer): Integer;
begin
  Result := Value - 1;
  if Result < 0 then Result := Divisor - 1
end;

{ TToolsDisplay }

constructor TToolsDisplay.Create;
begin
  inherited Create;
  FDisplayVertic := dv_top;
  FStarter       := True;
end;

procedure TToolsDisplay.EnlargeDisplay;
begin
  { --- not visible windows but active }
  with Application.MainForm do begin
    Top    := 0;  Left   := 0;
    Width  := 0;  Height := 0;
  end
end;

procedure TToolsDisplay.GoToMonitor(const index: Integer);
var
  Value: Integer;
begin
  Value := Abs(index);
  with Screen do if index < MonitorCount then begin
    FIndexMonitor := Value;
    KeyWrite(BufferKey, 'IndexMonitor', FIndexMonitor);
    TSQLRaoul.UserDataExport;
    { --- For each dialog box of Raoul, add new dialog here !!! }
    with TalkativeBox do DisplayToMonitor;
    with ScreenDialog do DisplayToMonitor;
  end
end;

procedure TToolsDisplay.GotoNextMonitor;
begin
  with Screen do GoToMonitor( SuccModVal(IndexMonitor, MonitorCount) )
end;

procedure TToolsDisplay.GotoPrevMonitor;
begin
  with Screen do GoToMonitor( PredModVal( IndexMonitor, MonitorCount ) )
end;

class procedure TToolsDisplay.Initialize;
begin
  with ToolsDisplay do if FStarter then begin
    IndexMonitor := KeyReadInt(BufferKey, 'IndexMonitor', 0);
    FStarter     := False;
  end
end;

procedure TToolsDisplay.KeyInitialize(const KeyButton: TButton);
begin
  with KeyButton do begin
    Top     := -100;
    OnKeyUp := KeyUp;
  end
end;

procedure TToolsDisplay.KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { AR et penser à gérer la réponse avec ScreenDlg }
  case key of
    VK_BACK   : if Assigned(KeyBackFunc) then KeyBackFunc(Sender); //ok
    VK_RETURN : if ssctrl in Shift then begin if Assigned(KeyCtrlReturnFunc) then KeyCtrlReturnFunc(Sender) end
                  else if Assigned(KeyReturnFunc) then KeyReturnFunc(Sender);

//    VK_F2     : with TalkativeFacade do FreePanelView := not FreePanelView;  Utilité ???
    VK_F3     : with TalkativeFacade do if ssctrl in Shift then CommandView := False else CommandView := True; //ok
    VK_F5     : with TalkativeFacade do if ssctrl in Shift then TextView := False else TextView := True;       //ok
    VK_F8     : with TalkativeFacade do if ssctrl in Shift then SoundView := False  else SoundView := True;    //ok

    VK_F9     : GotoPrevMonitor; //ok
    VK_F10    : GotoNextMonitor; //ok
    VK_F11    : if ssctrl in Shift then DisplayHoriz  := dh_left else DisplayHoriz := dh_right;  //ok
    VK_F12    : if ssctrl in Shift then DisplayVertic := dv_bottom else DisplayVertic := dv_top; //ok
  end
end;

procedure TToolsDisplay.SetDisplayHoriz(const Value: TDisplayHoriz);
begin
  FDisplayHoriz := Value;
  KeyWrite(BufferKey, 'AlignmentHoriz', Integer(Value));
  case Value of
    dh_left   : TalkativeFacade.AlignLeft;
    dh_right  : TalkativeFacade.AlignRight;
  end;
  TSQLRaoul.UserDataExport
end;

procedure TToolsDisplay.SetDisplayVertic(const Value: TDisplayVertic);
begin
  FDisplayVertic := Value;
  KeyWrite(BufferKey, 'AlignmentVertic', Integer(Value));
  case Value of
    dv_top    : TalkativeFacade.AlignTop;
    dv_bottom : TalkativeFacade.AlignBottom;
  end;
  GoToMonitor( IndexMonitor )
end;

procedure TToolsDisplay.SetIndexMonitor(const Value: Integer);
begin
  FIndexMonitor := Value;
  with Screen do
    if FIndexMonitor >= MonitorCount then FIndexMonitor := MonitorCount - 1;
  KeyWrite(BufferKey, 'IndexMonitor', FIndexMonitor);
  EnlargeDisplay;
end;


{ TTalkativeFacade }

procedure TTalkativeFacade.AlignBottom;
begin
  with TalkativeText  do Properties.Alignment.Vert := taBottomJustify;
  with TalkativeCmd   do Properties.Alignment.Vert := taBottomJustify;
  with TalkativeGreen do Top := 72;
  with TalkativeRed   do Top := 72;
end;

procedure TTalkativeFacade.AlignLeft;
begin
  with TalkativePate  do Align := alLeft;
  with TalkativeSepa  do Align := alLeft;
  with TalkativeGauge do Align := alLeft;
  with TalkativeText  do Properties.Alignment.Horz := taLeftJustify;
  with TalkativeSepb  do Align := alRight;
  with TalkativeCmd   do Properties.Alignment.Horz := taRightJustify;
end;

procedure TTalkativeFacade.AlignRight;
begin
  with TalkativePate  do Align := alRight;
  with TalkativeSepa  do Align := alRight;
  with TalkativeGauge do Align := alRight;
  with TalkativeText  do Properties.Alignment.Horz := taRightJustify;
  with TalkativeSepb  do Align := alLeft;
  with TalkativeCmd   do Properties.Alignment.Horz := taLeftJustify;
end;

procedure TTalkativeFacade.AlignTop;
begin
  with TalkativeText  do Properties.Alignment.Vert := taTopJustify;
  with TalkativeCmd   do Properties.Alignment.Vert := taTopJustify;
  with TalkativeGreen do Top := 6;
  with TalkativeRed   do Top := 6;
end;

constructor TTalkativeFacade.Create;
begin
  inherited Create;
  FStarter := True
end;

procedure TTalkativeFacade.FocusToRaoul;
begin
  with Application.MainForm do try SetFocus except end
end;

function TTalkativeFacade.GetActiveSoundView: Boolean;
begin
  with TalkativeGauge do Result := Visible
end;

function TTalkativeFacade.GetCommandText: string;
begin
  with TalkativeCmd do Result := Caption
end;

function TTalkativeFacade.GetCommandView: Boolean;
begin
  with TalkativeCmd do Result := Visible
end;

function TTalkativeFacade.GetDisplayHoriz: TDisplayHoriz;
begin
  with ToolsDisplay do Result := DisplayHoriz
end;

function TTalkativeFacade.GetDisplayVertic: TDisplayVertic;
begin
  with ToolsDisplay do Result := DisplayVertic
end;

function TTalkativeFacade.GetFreePanelView: Boolean;
begin
  with TalkativeFree do Result := Visible
end;

function TTalkativeFacade.GetIndexMonitor: Integer;
begin
  with ToolsDisplay do Result := IndexMonitor
end;

function TTalkativeFacade.GetTalkativeView: Boolean;
begin
  with TalkativeBox do Result := Visible
end;

function TTalkativeFacade.GetText: string;
begin
  with TalkativeText do Result := Caption
end;

function TTalkativeFacade.GetTextColor: TColor;
begin
  with TalkativeText.Style do Result := TextColor
end;

function TTalkativeFacade.GetTextView: Boolean;
begin
  with TalkativeText do Result := Visible
end;

function TTalkativeFacade.GetTryRecognize: Boolean;
begin
  with TalkativeRed do Result := Visible
end;

procedure TTalkativeFacade.Initialize_;
begin
  TextView      := KeyReadBoolean(BufferKey, 'TalkativeView', True);
  CommandView   := KeyReadBoolean(BufferKey, 'TalkativeCommandView', True);
  FreePanelView := KeyReadBoolean(BufferKey, 'TalkativeFreePanelView', True);
  DisplayVertic := TDisplayVertic( KeyReadInt(BufferKey, 'AlignmentVertic', 0) );
  DisplayHoriz  := TDisplayHoriz ( KeyReadInt(BufferKey, 'AlignmentHoriz', 0 ) );
end;

function TTalkativeFacade.isEliteMode: Boolean;
begin
  with Recorder do Result := IsEliteMode
end;

function TTalkativeFacade.isPickerGridVisible: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'PickerGridVisible')
end;

function TTalkativeFacade.isPickerPointVisible: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'PickerGridPointVisible')
end;

procedure TTalkativeFacade.PostInitialize;
begin
  SoundView := KeyReadBoolean(BufferKey, 'TalkativeSoundView', True)
end;

class procedure TTalkativeFacade.RetrieveRegValue;
begin
  with TalkativeFacade do Initialize_
end;

procedure TTalkativeFacade.SetActiveSoundView(const Value: Boolean);
begin
  with TalkativeGauge do Visible := Value;
  KeyWrite(BufferKey, 'TalkativeSoundView', Value)
end;

procedure TTalkativeFacade.SetCommandText(const Value: string);
var
  ASt : string;
begin
  if KeyReadInt(Appkey, 'UpdateAction') = 1
    then ASt := Format('%s (mise à jour en attente)', [Value])
    else
  if KeyReadInt(Appkey, 'UpdateAction') = 2
    then ASt := Format('%s (version beta)', [Value])
    else ASt := Value;

  with TalkativeCmd do Caption := ASt;
  KeyWrite(BufferKey, 'TalkativeCommandText', ASt)
end;

procedure TTalkativeFacade.SetCommandView(const Value: Boolean);
begin
  with TalkativeCmd do Visible := Value;
  KeyWrite(BufferKey, 'TalkativeCommandView', Value)
end;

procedure TTalkativeFacade.SetDisplayHoriz(const Value: TDisplayHoriz);
begin
  with ToolsDisplay do DisplayHoriz := Value
end;

procedure TTalkativeFacade.SetDisplayVertic(const Value: TDisplayVertic);
begin
  with ToolsDisplay do DisplayVertic := Value
end;

procedure TTalkativeFacade.SetFreePanelView(const Value: Boolean);
begin
  with TalkativeFree do Visible := Value;
  KeyWrite(BufferKey, 'TalkativeFreePanelView', Value)
end;

procedure TTalkativeFacade.SetIndexMonitor(const Value: Integer);
begin
  with ToolsDisplay  do GoToMonitor(Value)
end;

procedure TTalkativeFacade.SetSoundLevel(const Value: Integer);
begin
  with TalkativeGauge do Position := Value
end;

procedure TTalkativeFacade.SetTalkativeView(const Value: Boolean);
begin
 with TalkativeBox do case Visible of
    True : Hide;
    else Show
  end;
  KeyWrite(BufferKey, 'TalkativeDialogBoxView', Value)
end;

procedure TTalkativeFacade.SetText(const Value: string);
begin
  with TalkativeText do Caption := Value
end;

procedure TTalkativeFacade.SetTextColor(const Value: TColor);
begin
  with TalkativeText.Style do TextColor := Value
end;

procedure TTalkativeFacade.SetTextView(const Value: Boolean);
begin
  with TalkativeText do Visible := Value;
  KeyWrite(BufferKey, 'TalkativeView', Value)
end;

procedure TTalkativeFacade.SetTryRecognize(const Value: Boolean);
begin
  if Value then begin
    with TalkativeGreen do Visible := False;
    with TalkativeRed   do Visible := True
  end else begin
    with TalkativeGreen do Visible := True;
    with TalkativeRed   do Visible := False
  end
end;

procedure TTalkativeFacade.ShowClosePanel;
begin
  with Recorder do ModeFonctionnement := mf_closeapp;
  ShowDialog('Tu veux quitter Raoul ?!!', rt_yes_no)
end;

procedure TTalkativeFacade.ShowDialog(const Message: string;
  MessageType: TResponseType);
begin
  ShowForm(EmbendedDlg, ScreenDialog);
  with EmbendedDlg do begin
    ResponseKind := MessageType;
    Caption      := Message
  end
end;

procedure TTalkativeFacade.ShowForm(const DisplayedForm, ContainerForm: TForm);
begin
  try
    DisplayedForm.Show;
    with ContainerForm do Show
  except
    raise Exception.Create('Pb : de fenêtre embarquée');
  end
end;

procedure TTalkativeFacade.ShowHelp;
begin
  FocusToRaoul;
  HelpView.Show
end;

procedure TTalkativeFacade.ShowSensibility;
begin
  FocusToRaoul;
  Recorder.ModeFonctionnement := mf_sensibility;
  ShowForm(EmbendedSensibility, ScreenDialog);
  with EmbendedSensibility do ResponseKind := rt_ok
end;

procedure TTalkativeFacade.TextColoration(const Taux: Double;
  const ASt: string);
var
  AHigh, AMiddle: Double;
begin
  TSensibilitySetting.Floors(AHigh, AMiddle);
  if Taux > AHigh then TextColor := $00B9B9B9
    else
  if Taux > AMiddle then TextColor := $00C4FBFA
    else TextColor := $0042A0FF;
  Text := ASt;
end;

{ TFunctionment }

constructor TFunctionment.Create;
begin
  inherited Create;
  Initialize_
end;

procedure TFunctionment.DoAppClose(Sender: TObject);
begin
  with TalkativeFacade do ShowClosePanel
end;

procedure TFunctionment.DoCancel(Sender: TObject);
begin
  with Recorder do begin
    case ModeFonctionnement of
      mf_sensibility : EmbendedSensibility.Hide;
      mf_closeapp    : EmbendedDlg.Hide;
      else with TalkativeFacade do begin
        if isPickerGridVisible and not isPickerPointVisible then
          ScreenGrid.BackHome
      end
    end
  end
end;

procedure TFunctionment.DoHautesPerformances(Sender: TObject);
begin
  with EmbendedSensibility do HautePerformances;
end;

procedure TFunctionment.DoMicroCasque(Sender: TObject);
begin
  with EmbendedSensibility do MicroCasque;
end;

procedure TFunctionment.DoMicroEnceinte(Sender: TObject);
begin
  with EmbendedSensibility do MicroEnceinte;
end;

procedure TFunctionment.DoMicroStudio(Sender: TObject);
begin
  with EmbendedSensibility do MicroDeStudio;
end;

procedure TFunctionment.DoNo(Sender: TObject);
begin
  with Recorder do begin
    case ModeFonctionnement of
      mf_sensibility : ;
      mf_closeapp    : begin
          with MainForm do CanCloseIdx := 0;
          EmbendedDlg.Hide
      end
    end
  end
end;

procedure TFunctionment.DoOk(Sender: TObject);
begin
  with Recorder do begin
    case ModeFonctionnement of
      mf_sensibility : EmbendedSensibility.Hide;
      mf_closeapp    : with Application.MainForm do Close;
    end
  end
end;

procedure TFunctionment.DoPickerGridBlue(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible and not isPickerPointVisible then
    with ScreenGrid do SelectStrip(6)
end;

procedure TFunctionment.DoPickerGridBlack(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible and not isPickerPointVisible then
    with ScreenGrid do SelectStrip(3)
end;

procedure TFunctionment.DoPickerGridClose(Sender: TObject);
begin
  { --- Disable Grid Grammar }
  with Recorder do GridDeactivate;
  { --- Hide Grid }
  with PickerGridForm, Grid do GridClose;
  with TalkativeFacade do if isEliteMode then EliteForeGround
end;

procedure TFunctionment.DoPickerGridGray(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible and not isPickerPointVisible then
    with ScreenGrid do SelectStrip(2)
end;

procedure TFunctionment.DoPickerGridGreen(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible and not isPickerPointVisible then
    with ScreenGrid do SelectStrip(5)
end;

procedure TFunctionment.DoPickerGridMonitorNext(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible
    then with PickerGridForm, Grid do GridMonitorNext
    else with ToolsDisplay do GotoNextMonitor
end;

procedure TFunctionment.DoPickerGridMonitorPrev(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible
    then with PickerGridForm, Grid do GridMonitorPrev
    else with ToolsDisplay do GotoPrevMonitor
end;

procedure TFunctionment.DoPickerGridPink(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible and not isPickerPointVisible then
    with ScreenGrid do SelectStrip(9)
end;

procedure TFunctionment.DoPickerGridOrange(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible and not isPickerPointVisible then
    with ScreenGrid do SelectStrip(8)
end;

procedure TFunctionment.DoPickerGridPointAdd(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible and not isPickerPointVisible then
    with PickerGridForm, Grid do GridPointAdd
end;

procedure TFunctionment.DoPickerGridPointHide(Sender: TObject);
begin
  { --- Disable Grid Grammar }
  with Recorder do GridDeactivate;
  { --- Hide Points and Grid }
  if KeyReadBoolean(BufferKey, 'DirectGridOpenMode') then begin
    DoPickerGridClose(nil);
    KeyWrite(BufferKey, 'DirectGridOpenMode', False)
  end else begin
    with TalkativeFacade do if isPickerGridVisible then
      with PickerGridForm, Grid do GridPointHide
  end
end;

procedure TFunctionment.DoPickerGridPointNext(Sender: TObject);
begin
  with TalkativeFacade do with PickerGridForm, Grid do GridPointNext
end;

procedure TFunctionment.DoPickerGridPointPrev(Sender: TObject);
begin
  with TalkativeFacade do with PickerGridForm, Grid do GridPointPrev
end;

procedure TFunctionment.DoPickerGridPointSelect(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible and not isPickerPointVisible then
    with PickerGridForm, Grid do begin
      with Recorder do GridDeactivate;
      GridPointSelect
    end
end;

procedure TFunctionment.DoPickerGridPointShow(Sender: TObject);
begin
  { --- Enable Grid Grammar }
  with Recorder do GridActivate;
  { --- Display Grid ans Points }
  with PickerGridForm, Grid do begin
    with TalkativeFacade do KeyWrite(BufferKey, 'DirectGridOpenMode', not isPickerGridVisible);
    GridPickerMode := gm_point;    {  --- Specify display mode }
    with TalkativeFacade do if isPickerGridVisible then GridPointShow else PickerGridForm.Show
  end
end;

procedure TFunctionment.DoPickerGridRed(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible and not isPickerPointVisible then
    with ScreenGrid do SelectStrip(4)
end;

procedure TFunctionment.DoPickerGridShow(Sender: TObject);
begin
  { --- Enable Grid Grammar }
  with Recorder do GridActivate;
  { --- Initialize Level for Flist }
  ScreenGrid.ResetOnOpen;
  { --- Spécifier ici  le type d'affichage }
  with PickerGridForm, Grid do GridPickerMode := gm_grid;
  { --- Affichage du formulaire supportant la grille }
  PickerGridForm.Show;
end;

procedure TFunctionment.DoPickerGridYellow(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible and not isPickerPointVisible then
    with ScreenGrid do SelectStrip(7)
end;

procedure TFunctionment.DoPickerGridWhite(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible and not isPickerPointVisible then
    with ScreenGrid do SelectStrip(1)
end;

procedure TFunctionment.DoRetour(Sender: TObject);
begin
  with TalkativeFacade do
    if isPickerGridVisible and not isPickerPointVisible
      then ScreenGrid.BackLevel
      else
    if isEliteMode then EliteManager.UIBack
end;

procedure TFunctionment.DoSensibility(Sender: TObject);
begin
  with TalkativeFacade do ShowSensibility
end;

procedure TFunctionment.DoYes(Sender: TObject);
begin
  with Recorder do begin
    case ModeFonctionnement of
      mf_sensibility : ;
      mf_closeapp    : with MainForm do begin
        CanCloseIdx := 1;
        Close
      end
    end
  end
end;

class procedure TFunctionment.Finalize;
begin
  Functionment.Free
end;

class procedure TFunctionment.Initialize;
begin
  Functionment := TFunctionment.Create
end;

procedure TFunctionment.Initialize_;
begin
  AppCloseFunc               := DoAppClose;
  SensibilityFunc            := DoSensibility;
  OkFunc                     := DoOk;
  RetourFunc                 := DoRetour;
  CancelFunc                 := DoCancel;
  YesFunc                    := DoYes;
  NoFunc                     := DoNo;
  EchapFunc                  := DoEchap;
  MouseLeftFunc              := DoMouseLeft;
  MouseRightFunc             := DoMouseRight;
  MouseDoubleFunc            := DoMouseDouble;
  MouseMiddleFunc            := DoMouseMiddle;
  AppVersionShowFunc         := DoAppVersionShow;

  HautesperformancesFunc     := DoHautesperformances;
  MicroCasqueFunc            := DoMicroCasque;
  MicroStudioFunc            := DoMicroStudio;
  MicroEnceinteFunc          := DoMicroEnceinte;
  PickerGridShowFunc         := DoPickerGridShow;
  PickerGridCloseFunc        := DoPickerGridClose;
  PickerGridMonitorPrevFunc  := DoPickerGridMonitorPrev;
  PickerGridMonitorNextFunc  := DoPickerGridMonitorNext;
  PickerGridPointsShowFunc   := DoPickerGridPointShow;
  PickerGridPointsHideFunc   := DoPickerGridPointHide;
  PickerGridPointPrevFunc    := DoPickerGridPointPrev;
  PickerGridPointNextFunc    := DoPickerGridPointNext;
  PickerGridPointSelectFunc  := DoPickerGridPointSelect;
  PickerGridPointAddFunc     := DoPickerGridPointAdd;
  PickerGridPointClearFunc   := DoPickerGridPointClear;

  PickerGridWhiteFunc        := DoPickerGridWhite;
  PickerGridGrayFunc         := DoPickerGridGray;
  PickerGridBlackFunc        := DoPickerGridBlack;
  PickerGridRedFunc          := DoPickerGridRed;
  PickerGridGreenFunc        := DoPickerGridGreen;
  PickerGridBlueFunc         := DoPickerGridBlue;
  PickerGridYellowFunc       := DoPickerGridYellow;
  PickerGridOrangeFunc       := DoPickerGridOrange;
  PickerGridPinkFunc         := DoPickerGridPink;

  RaoulIntRightFunc          := DoRaoulIntRight;
  RaoulIntLeftFunc           := DoRaoulIntLeft;
  RaoulIntTopFunc            := DoRaoulIntTop;
  RaoulIntBottomFunc         := DoRaoulIntBottom;
  RaoulIntCmdShowFunc        := DoRaoulIntCmdShow;
  RaoulIntCmdHideFunc        := DoRaoulIntCmdHide;
  RaoulIntTextShowFunc       := DoRaoulIntTextShow;
  RaoulIntTextHideFunc       := DoRaoulIntTextHide;
  RaoulIntVumetreShowFunc    := DoRaoulIntVumetreShow;
  RaoulIntVumetreHideFunc    := DoRaoulIntVumetreHide;
end;

procedure TFunctionment.DoRaoulIntRight(Sender: TObject);
begin
  with ToolsDisplay do DisplayHoriz  := dh_right;
end;

procedure TFunctionment.DoRaoulIntLeft(Sender: TObject);
begin
  with ToolsDisplay do DisplayHoriz  := dh_left;
end;

procedure TFunctionment.DoRaoulIntTop(Sender: TObject);
begin
  with ToolsDisplay do DisplayVertic := dv_top;
end;

procedure TFunctionment.DoRaoulIntBottom(Sender: TObject);
begin
  with ToolsDisplay do DisplayVertic := dv_bottom;
end;

procedure TFunctionment.DoRaoulIntCmdShow(Sender: TObject);
begin
  with TalkativeFacade do CommandView := True;
end;

procedure TFunctionment.DoRaoulIntCmdHide(Sender: TObject);
begin
  with TalkativeFacade do CommandView := False;
end;

procedure TFunctionment.DoRaoulIntTextShow(Sender: TObject);
begin
  with TalkativeFacade do TextView := True;
end;

procedure TFunctionment.DoRaoulIntTextHide(Sender: TObject);
begin
  with TalkativeFacade do TextView := False;
end;

procedure TFunctionment.DoRaoulIntVumetreShow(Sender: Tobject);
begin
  with TalkativeFacade do SoundView := True;
end;

procedure TFunctionment.DoRaoulIntVumetreHide(Sender: TObject);
begin
  with TalkativeFacade do SoundView := False;
end;

procedure TFunctionment.DoMouseLeft(Sender: TObject);
begin
  MouseLeftClic
end;

procedure TFunctionment.DoMouseRight(Sender: TObject);
begin
  MouseRightClic;
end;

procedure TFunctionment.DoEchap(Sender: TObject);
begin
  if HelpView.Visible then HelpView.Close
    else
  if TalkativeFacade.isPickerGridVisible
    then DoRetour(nil)
    else
  SendKey(VK_ESCAPE, 10, [])
end;

procedure TFunctionment.DoPickerGridPointClear(Sender: TObject);
begin
  with TalkativeFacade do if isPickerGridVisible then
    with PickerGridForm, Grid do GridPointClear
end;

procedure TFunctionment.DoAppVersionShow(Sennder: TObject);
begin
  with TalkativeFacade do begin
    TextView := True;
    Text := ApplicationVersion;
  end
end;

procedure TFunctionment.DoMouseDouble(Sender: TObject);
begin
  MouseDoubleClic
end;

procedure TFunctionment.DoMouseMiddle(Sender: TObject);
begin
  MouseMiddleClic
end;

initialization
  ToolsDisplay    := TToolsDisplay.Create;
  TalkativeFacade := TTalkativeFacade.Create;
finalization
  ToolsDisplay.Free;
  TalkativeFacade.Free
end.
