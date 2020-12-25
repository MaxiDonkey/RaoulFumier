unit uEliteHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrCopyUtils, StdCtrls, ComCtrls, uRegistry,
  {devexpress}
  cxCheckBox;

type
  TStamps = class
  { --- root datas }
  private
    FBranch   : string;
    FStickers : string;
  public
    procedure SetText(const Value: string);
    constructor Create(const Value: string);
  end;

  Str255 = string[255];

  TRubrikRecord = record
  { --- help text datas }
    Header   : Str255;
    Body     : string;
  end;
  ArrayOfRubriks = array of TRubrikRecord;

  TRubriks = class
  private
    FRootFileName   : string;
    FRubrikFileName : string;
    Datas           : ArrayOfRubriks;
    LIdx            : TStringList;
    RBKFile         : TFileStream;
    FRoot           : TStringList;
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);
    function  GetItem(index: Integer): TRubrikRecord;
    function  GetText(AHeader: string): string;
    function  GetItemIndex(AHeader: string): Integer;
    function  RootInitialize: string;
    function  GetRoot: string;
    procedure RootReset;
    function  GetSticker(const index: Integer): string;
    property  Item[index: Integer]:TRubrikRecord read GetItem;
  public
    function  AddRubrik(const AHeader: Str255; ABody: string):TRubrikRecord;
    procedure Save;
    procedure Reload;

    property Text[AHeader: string]: string read GetText;
    property ItemIndex[AHeader: string]: Integer read GetItemIndex;
    property RootText: string read GetRoot;
    property Sticker[const index: Integer]:string read GetSticker;

    constructor Create(const FileNames: string);
    destructor Destroy; override;
  end;

  THelpKind = (hk_none, hk_main, hk_elite, hk_navigation);
  TListRubriks = array[THelpKind] of TRubriks;

  THelps = class
  private
    FRbkList        : TListRubriks;
    FCurrent        : THelpKind;
    FRubrik         : TRubriks;
    FStrupdt        : TGetStrProc;
    procedure Initialize;
    procedure Finalize;
    procedure SetCurrent(const Value: THelpKind);
  public
    property Current: THelpKind read FCurrent write SetCurrent;
    property Rubrik: TRubriks read FRubrik;
    property StrUpdate: TGetStrProc read FStrupdt write FStrupdt;

    constructor Create;
    destructor Destroy; override;
  end;

  THelpView = class
  private
    FFormDlg        : TForm;
    FTitle          : TListBox;
    FSubTitle       : TListBox;
    FEditor         : TRichEdit;
    FCaption        : TLabel;
    FHelpType       : TLabel;
    FReadOnly       : Boolean;
    FTitleIndex     : Integer;
    FCheckBox       : TcxCheckBox;
    procedure SetReadOnly(const Value: Boolean);
    function  GetVisible: Boolean;
    function  GetHideAutoOpen: Boolean;
    procedure SetHideAutoOpen(const Value: Boolean);
    function  GetBootState: Boolean;
    procedure SetBootState(const Value: Boolean);
  private
    { --- Events }
    procedure TitleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TitleClick(Sender: TObject);
    procedure SubTitleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SubTitleClick(Sender: TObject);
    procedure EditorEnter(Sender: TObject);
    procedure CheckBoxClic(Sender: TObject);
  private
    function  UpdateRubrik(Full: Boolean = True):string;
    procedure RbkFormat(const Memo: TRichEdit; const ASt: string);
    procedure DoBack;
    procedure DisplayFormating;
    procedure HelpProc(const S: string);
  public
    { --- modifications }
    procedure FirstDisplay;
    procedure Completion(const ASt: string);
    procedure CtrlBackOn;
    procedure BackOn;
  public
    procedure SetTitle(const Value: TListBox);
    procedure SetSubTitle(const Value: TListBox);
    procedure SetEditor(const Value: TRichEdit);
    procedure SetCaption(const Value: TLabel);
    procedure SetHelpType(const Value: TLabel);
    procedure SetCheckBox(const Value: TcxCheckBox);
    procedure SetFormDlg(const AForm: TForm);
    procedure Save;
    procedure Show;
    procedure Close;
    procedure AutoOpen;

    procedure TitleSelect(const index: Integer);
    procedure SubTitleSelect(const index: Integer);

    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Form: TForm read FFormDlg;
    property Visible: Boolean read GetVisible;
    property HideAutoOpen: Boolean read GetHideAutoOpen write SetHideAutoOpen;
    property BootState:Boolean read GetBootState write SetBootState;

    constructor Create;
    class procedure Initialize(const ATitle, ASubTitle: TListBox;
      const AEditor: TRichEdit; const ACaption, AHelpType: TLabel;
      const ACheckBox : TcxCheckBox; AReadOnly: Boolean = True);
    class procedure Finalize;
  end;

var
  HelpView : THelpView = nil;
  Helps    : THelps;
  HelpSrc  : array[THelpKind] of string =
   ( '',
     'MainRoot.roo;MainRubriks.dat',
     'EliteRoot.roo;EliteRubriks.dat',
     'NavRoot.roo;NavRubriks.dat');

implementation

function LoadRoots(const FileName: string): string;
{ --- Retrieve text from a text file }
begin
  with TStringList.Create do
  try
    try
      LoadFromFile( FileName );
      Result := Trim( Text )
    except
      Result := EmptyStr
    end
  finally
    Free
  end
end;

function DelLineReturnStr(const ASt: string): string;
var
  i : Integer;
begin
  Result := EmptyStr;
  for i := 1 to Length(ASt) do
    { --- Subtract LF of a string }
    if ASt[i] <> #10 then Result := Result + ASt[i]
end;

function DelLineReturnText(const ASt: string): string;
{ --- Subtract LF of a text }
var
  i : Integer;
begin
  with TStringList.Create do
  try
    Text := ASt;
    for i := 0 to Pred(Count) do Strings[i] := DelLineReturnStr(Strings[i]);
    Result := Text
  finally
    Free
  end
end;

function AddNumero(const Prefixe, ASt: string):string;
var
  i : Integer;
  p : string;
begin
  with TStringList.Create do
  try
    Text := ASt;
    for i := 0 to Pred(Count) do begin
      p := Format('%s%d.', [Prefixe, i+1]);
      Strings[i] := Format('%5s %s', [p, Strings[i]])
    end;
    Result := text
  finally
    Free
  end
end;



{ TStamps }

constructor TStamps.Create(const Value: string);
begin
  inherited Create;
  SetText( Value )
end;

procedure TStamps.SetText(const Value: string);
begin
  FBranch   := GetBeforStr(Value, '#');
  FStickers := GetAfterStr(Value, '#');
  with TStringList.Create do
  try
    Delimiter := ';';
    QuoteChar := '"';
    DelimitedText := FStickers;
    FStickers     := AnsiUpperCase( Trim( Text ) )
  finally
    Free
  end
end;

{ TRubriks }

function TRubriks.AddRubrik(const AHeader: Str255;
  ABody: string): TRubrikRecord;
var
  index : Integer;
begin
  with LIdx do begin
    index := IndexOf( AHeader );
    if index = -1 then begin
      with Result do begin
        Header := AHeader;
        Body   := ABody
      end;
      SetLength(Datas, Length(Datas) + 1);
      Datas[High(Datas)] := Result;
      Add( AHeader )
    end else begin
      Datas[index].Body := ABody;
      Result := Datas[index]
    end
  end
end;

constructor TRubriks.Create(const FileNames: string);
var
  ARootFileName, ARubrikFileName: string;
begin
  inherited Create;
  ARootFileName   := GetBeforStr(FileNames, ';');
  ARubrikFileName := GetAfterStr(FileNames, ';');
  FRoot           := TStringList.Create;
  FRootFileName   := Format('Help\%s', [ARootFileName]);
  RootInitialize;
  FRubrikFileName := Format('Help\%s', [ARubrikFileName]);
  LIdx            := TStringList.Create;
  if FileExists( FRubrikFileName ) then LoadFromFile( FRubrikFileName )
end;

destructor TRubriks.Destroy;
begin
  RootReset;
  LIdx.Free;
  inherited
end;

function TRubriks.GetItem(index: Integer): TRubrikRecord;
begin
  Result := Datas[ index ]
end;

function TRubriks.GetItemIndex(AHeader: string): Integer;
begin
  with LIdx do Result := IndexOf(AHeader)
end;

function TRubriks.GetRoot: string;
begin
  Result := AddNumero('B', AnsiUpperCase( Trim( FRoot.Text ) ) )
end;

function TRubriks.GetSticker(const index: Integer): string;
begin
  with FRoot do
  try
    Result := TStamps( Objects[index] ).FStickers
  except
    Result := EmptyStr
  end
end;

function TRubriks.GetText(AHeader: string): string;
var
  index : Integer;
begin
  with LIdx do index := IndexOf(AHeader);
  try
    Result := Item[index].Body
  except
    Result := EmptyStr
  end
end;

procedure TRubriks.LoadFromFile(const AFileName: string);
var
  len     : Integer;
  AHeader : str255;
  ABody   : string;
begin
  SetLength(Datas, 0);
  { --- Indexes list clear }
  LIdx.Clear;
  RBKFile := TFileStream.Create(TFileName(AFileName), fmOpenRead or fmShareDenyWrite);
  with RBKFile do
  try
    Position := 0;
    while Position < RBKFile.Size do begin
      { --- Read record from file }
      ReadBuffer (AHeader,  SizeOf(Str255));
      ReadBuffer (len,      SizeOf(len));
      SetLength  (ABody,    len);
      ReadBuffer (ABody[1], len);
      { --- Add record to array }
      AddRubrik  (AHeader,  ABody)
    end
  finally
    Free
  end
end;

procedure TRubriks.Reload;
begin
  LoadFromFile( FRubrikFileName )
end;

function TRubriks.RootInitialize:string;

  procedure AddValue(const ASt: string); begin
    with FRoot do AddObject( GetBeforStr(ASt, '#'), TStamps.Create( ASt ) )
  end;

begin
  Result := LoadRoots( FRootFileName );
  with TStringList.Create do
  try
    Text := Result;
    with GetEnumerator do
    try
      while MoveNext do AddValue( Current )
    finally
      Free
    end
  finally
    Free
  end
end; {RootInitialize}

procedure TRubriks.RootReset;
var
  i : Integer;
begin
  with FRoot do for i := Pred( Count ) to 0 do begin
    Objects[i].Free;
    Delete(i)
  end
end;

procedure TRubriks.Save;
begin
  SaveToFile( FRubrikFileName )
end;

procedure TRubriks.SaveToFile(const AFileName: string);
var
  i, len : Integer;
begin
  RBKFile := TFileStream.Create(TFileName(AFileName), fmCreate or fmShareExclusive);
  with RBKFile do
  try
    Position := 0;
    for i := Low(Datas) to High(Datas) do begin
      { --- Length of string type }
      len := Length( Datas[i].Body );
      { --- Write record on file }
      WriteBuffer (Datas[i].Header,  SizeOf(Str255));
      WriteBuffer (len,              SizeOf(len));
      WriteBuffer (Datas[i].Body[1], len)
    end
  finally
    Free
  end
end;

{ THelps }

constructor THelps.Create;
begin
  inherited Create;
  Current   := hk_none;
  StrUpdate := nil;
  Initialize
end;

destructor THelps.Destroy;
begin
  Finalize;
  inherited
end;

procedure THelps.Finalize;
var
  i : Integer;
begin
  for i := 1 to Integer( High(THelpKind) ) do FRbkList[ THelpKind(i) ].Free
end;

procedure THelps.Initialize;
var
  i : Integer;
begin
  FRbkList[ hk_none ] := nil;
  for i := 1 to Integer( High(THelpKind) ) do
    FRbkList[ THelpKind(i) ] := TRubriks.Create( HelpSrc[ THelpKind(i) ] )
end;

function HelpToStr(const Value: THelpKind):string;
begin
  case Value of
    hk_main       : Result := 'Aide RaoulFumier';
    hk_elite      : Result := 'Aide Elite Dangerous';
    hk_navigation : Result := 'Aide navigation';
    else Result := EmptyStr
  end
end;

procedure THelps.SetCurrent(const Value: THelpKind);
begin
  FCurrent := Value;
  case FCurrent of
    hk_none : FRubrik := nil;
    else FRubrik := FRbkList[FCurrent]
  end;
  if Assigned(FStrupdt) then FStrupdt( HelpToStr(FCurrent) )
end;

{ THelpView }

procedure THelpView.AutoOpen;
begin
  if not HideAutoOpen then begin
    if not BootState then begin
      TitleSelect(1);
      Show
    end
  end
end;

procedure THelpView.BackOn;
begin
  DoBack
end;

procedure THelpView.CheckBoxClic(Sender: TObject);
begin
  with FCheckBox do HideAutoOpen := Checked
end;

procedure THelpView.Close;
begin
  if Assigned(FFormDlg) then FFormDlg.Close;
  KeyWrite(AppKey, 'HelpVisible', False)
end;

procedure THelpView.Completion(const ASt: string);
var
  x : Integer;
begin
  if FEditor.Focused then with FEditor, Lines do begin
    x := SelStart;
    Text := Copy(Text, 1, SelStart) + ASt + Copy(Text, SelStart + 1, Length(Text));
    SelStart := x + 1
  end
end;

constructor THelpView.Create;
begin
  if Assigned( HelpView ) then raise Exception.Create('HelpView is a singleton');
  inherited Create;
  FTitleIndex := -1
end;

procedure THelpView.CtrlBackOn;
begin
  DoBack
end;

var
  WTOP    : Integer = 70;
  WHEIGHT : Integer = 551; 

procedure THelpView.DisplayFormating;
{ --- formatting of the help form components }
begin
  if Assigned(FTitle) then with FTitle do begin
    Color := $00141414;
    with Font do begin
      Size  := 14;
      Color := clGray;
      Style := [fsBold]
    end;
    Left   := 8;
    Top    := WTOP;
    Width  := 536;
    Height := WHEIGHT;
    Anchors := [akLeft,akTop,akBottom]
  end;
  if Assigned(FSubTitle) then with FSubTitle do begin
    Color := $00141414;
    with Font do begin
      Size  := 14;
      Color := clGray;
      Style := [fsBold]
    end;
    Left   := 544;
    Top    := WTOP;
    Width  := 390;
    Height := WHEIGHT;
    Anchors := [akLeft,akTop,akBottom]
  end;
  if Assigned(FEditor) then with FEditor do begin
    Color := $00141414;
    with Font do begin
      Size  := 15;
      Color := $000080FF;
      Style := [fsBold]
    end;
    Left   := 975;
    Top    := WTOP;
    Width  := 725; 
    Height := WHEIGHT;
    Anchors := [akTop,akRight,akBottom]
  end;
  if Assigned(FCaption) then with FCaption do begin
    Transparent := True;
    with Font do begin
      Size  := 16;
      Color := $000080FF;
      Style := [fsBold]
    end;
    AutoSize := False;
    Layout   := tlBottom;
    Left     := 8;
    Top      := 0;
    Width    := 1200;
    Height   := 33
  end;
  if Assigned(FHelpType) then with FHelpType do begin
    Transparent := True;
    with Font do begin
      Size  := 8;
      Color := clWhite;
      Style := [fsBold]
    end;
    AutoSize := False;
    Layout   := tlTop;
    Left     := 8;
    Top      := 33;
    Width    := 400;
    Height   := 13
  end
end;

procedure THelpView.DoBack;
{ --- Operation when VK_BACK pressed }
begin
  if FEditor.Focused then FSubTitle.SetFocus
    else
  if FSubTitle.Focused then FTitle.SetFocus
end;

procedure THelpView.EditorEnter(Sender: TObject);
{ --- Immediate exit from editor upon entry on readonly if readonly }
begin
  if ReadOnly then FSubTitle.SetFocus
end;

class procedure THelpView.Finalize;
begin
  if Assigned(HelpView) then HelpView.Free
end;

procedure THelpView.FirstDisplay;
{ --- initialize display help }
begin
  with FTitle, items do
  try
    FTitleIndex := -1;
    Text := Helps.Rubrik.RootText;
    ItemIndex   := 0;
    TitleClick(nil)
  except
  end
end;

function THelpView.GetBootState: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'BootState')
end;

function THelpView.GetHideAutoOpen: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'HideAutoOpen')
end;

function THelpView.GetVisible: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'HelpVisible')
end;

procedure THelpView.HelpProc(const S: string);
{ --- update text of help kind }
begin
  with FHelpType do Caption := S
end;

class procedure THelpView.Initialize(const ATitle, ASubTitle: TListBox;
  const AEditor: TRichEdit; const ACaption, AHelpType: TLabel;
  const ACheckBox : TcxCheckBox; AReadOnly: Boolean);
begin
  if not Assigned(HelpView) then begin
    HelpView := THelpView.Create;
    with HelpView do begin
      SetTitle     ( ATitle    );
      SetSubTitle  ( ASubTitle );
      SetEditor    ( AEditor   );
      SetCaption   ( ACaption  );
      SetCheckBox  ( ACheckBox );
      SetHelpType  ( AHelpType );
      Helps.StrUpdate := HelpProc;
      ReadOnly := AReadOnly;
      DisplayFormating;
      FirstDisplay
    end
  end
end;

procedure THelpView.RbkFormat(const Memo: TRichEdit; const ASt: string);
{ --- "strickers" formatting }
var
  Buffer : string;

  procedure WriteNorm(const S: string); begin
    if S <> EmptyStr then with Memo do begin
      SelAttributes.Size  := 15;
      SelAttributes.Color := $000080FF;
      SelAttributes.Style := [fsBold];
      SelText := Format('%s', [S])
    end
  end;

  procedure WriteFunction(const S: string); begin
    if S <> EmptyStr then with Memo do begin
      SelAttributes.Size  := 12;
      SelAttributes.Color := clSilver;
      SelAttributes.Style := [fsBold];
      SelText := Format(' {%s} ', [S])
    end
  end;

  procedure WriteNote(const S, Prefixe: string; couleur : TColor = clHighlight); begin
    if S <> EmptyStr then with Memo do begin
      SelAttributes.Size  := 11;
      SelAttributes.Color := couleur;
      SelAttributes.Style := [fsBold];
      SelText := Format('%s%s ', [Prefixe,S])
    end
  end;


  procedure WriteAlt(const S: string); begin
    if AnsiPos('Note : ', S) > 0 then WriteNote( GetAfterStr(S, 'Note :'), EmptyStr )
      else
    if AnsiPos('Note1 : ', S) > 0 then WriteNote( GetAfterStr(S, 'Note1 :'), ' ', $0094DF6A )
      else WriteFunction(S)
  end;

  function LineProcess(const S: string):string; begin
    Result := S;
    while AnsiPos('{', Result) > 0 do begin
      Buffer := GetBeforStr(Result, '{');
      WriteNorm(Buffer);
      Result := GetAfterStr(Result, '{');
      Buffer := GetBeforStr(Result, '}');
      Result := GetAfterStr(Result, '}');
      WriteAlt(Buffer);
    end;
    WriteNorm(Result + #10)
  end;

begin
  Memo.Clear;
  with TStringList.Create do
  try
    Text := ASt;
    with GetEnumerator do
    try
      while MoveNext do LineProcess( Current )
    finally
      Free
    end
  finally
    Free
  end
end; {RbkFormat}

procedure THelpView.Save;
var
  S   : string;
  Csr : Integer;
begin
  if not ReadOnly then with Helps.Rubrik do begin
    with FEditor, Lines do begin
      BeginUpdate;
      try
        Csr := SelStart;
        S   := DelLineReturnText(Text);
        AddRubrik( UpdateRubrik(False), S );
        Save
      finally
        Text     := S;
        SelStart := Csr;
        EndUpdate
      end
    end
  end
end;

procedure THelpView.SetBootState(const Value: Boolean);
begin
  KeyWrite(AppKey, 'BootState', Value)
end;

procedure THelpView.SetCaption(const Value: TLabel);
begin
  FCaption := Value
end;

procedure THelpView.SetCheckBox(const Value: TcxCheckBox);
begin
  FCheckBox := Value;
  if Assigned(FCheckBox) then with FCheckBox do OnClick := CheckBoxClic
end;

procedure THelpView.SetEditor(const Value: TRichEdit);
begin
  FEditor := Value;
  if Assigned(FEditor) then with FEditor do OnEnter := EditorEnter
end;

procedure THelpView.SetFormDlg(const AForm: TForm);
begin
  FFormDlg := AForm
end;

procedure THelpView.SetHelpType(const Value: TLabel);
begin
  FHelpType := Value
end;

procedure THelpView.SetHideAutoOpen(const Value: Boolean);
begin
  KeyWrite(AppKey, 'HideAutoOpen', Value);
  if Assigned(FCheckBox) then with FCheckBox do Checked := Value
end;

procedure THelpView.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  with FEditor do ReadOnly := FReadOnly
end;

procedure THelpView.SetSubTitle(const Value: TListBox);
begin
  FSubTitle := Value;
  if Assigned(FSubTitle) then with FSubTitle do begin
    OnKeyDown := SubTitleKeyDown;
    OnClick   := SubTitleClick
  end
end;

procedure THelpView.SetTitle(const Value: TListBox);
begin
  FTitle := Value;
  if Assigned(FTitle) then with FTitle do begin
    OnKeyDown := TitleKeyDown;
    OnClick   := TitleClick
  end
end;

procedure THelpView.Show;
begin
  if not BootState then begin
    if Assigned(FFormDlg) then FFormDlg.Show;
    KeyWrite(AppKey, 'HelpVisible', True)
  end
end;

procedure THelpView.SubTitleClick(Sender: TObject);
begin
  UpdateRubrik
end;

procedure THelpView.SubTitleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RIGHT : if not ReadOnly then begin
      with FEditor do SetFocus;
      with FSubTitle do ItemIndex := ItemIndex - 1
    end
  end
end;

procedure THelpView.SubTitleSelect(const index: Integer);
begin
  if not BootState then with FSubTitle do
  try
    ItemIndex := index;
    SubTitleClick(nil)
  except
    ItemIndex := 0
  end
end;

procedure THelpView.TitleClick(Sender: TObject);
begin
  with FTitle do if (ItemIndex > -1) and (ItemIndex <> FTitleIndex) then begin
    FSubTitle.Items.Text := AddNumero('H', Helps.Rubrik.Sticker[ ItemIndex ] );
    with FSubTitle do if ItemIndex < 0 then ItemIndex := 0;
    FTitleIndex := ItemIndex
  end;
  UpdateRubrik
end;

procedure THelpView.TitleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RIGHT : begin
      with FSubTitle do
      try
        SetFocus;
        if ItemIndex = -1 then ItemIndex := 0;
        TitleClick(nil)
      except
      end;
      with FTitle do ItemIndex := ItemIndex - 1
    end;
    VK_LEFT : with FTitle do ItemIndex := ItemIndex + 1
  end
end;

function StrWithOuttNum(const Value: string):string;
begin
  if AnsiPos('.', Value) > 0 then Result := GetAfterStr(Value, '.')
    else Result := Value
end;

procedure THelpView.TitleSelect(const index: Integer);
begin
  if not BootState then with FTitle do
  try
    ItemIndex := index;
    TitleClick(nil)
  except
    ItemIndex := 0
  end
end;

function THelpView.UpdateRubrik(Full: Boolean): string;
var
  Ast1, ASt2: string;
begin
  with FTitle do if ItemIndex > -1 then ASt1 := StrWithOuttNum(Items[ItemIndex]) else ASt1 := EmptyStr;
  with FSubTitle do if ItemIndex > -1 then ASt2 := StrWithOuttNum(Items[ItemIndex]) else ASt2 := EmptyStr;
  if ASt2 = EmptyStr then Result := ASt1 else Result := Format('%s / %s', [ASt1, ASt2]);
  with FCaption do Caption := Result;
  if Full then with Helps.Rubrik do
    case ReadOnly of
      True : RbkFormat(FEditor, DelLineReturnText(Text[ Result ]));
      else FEditor.Lines.Text := Text[ Result ]
    end  
end;

initialization
finalization
  THelpView.Finalize;
  Helps.Free
end.
