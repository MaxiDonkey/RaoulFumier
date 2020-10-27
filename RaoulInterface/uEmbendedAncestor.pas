unit uEmbendedAncestor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  {Vocal}
  uNewRecorder;

var
  DISABLECOLOR : TColor = $00343434;
  ENABLECOLOR  : TColor = $009B6900;
  DISABLETEXT  : TColor = clGray;
  ENABLETEXT   : TColor = clWhite;

type
  TResponseKind = (rk_none, rk_ok, rk_cancel, rk_yes, rk_no);
  TResponseType = (rt_none, rt_ok, rt_ok_cancel, rt_yes_no);

var
  StrResponseKind : array[TResponseKind] of string = ('', 'Ok', 'Annuler', 'Oui', 'Non');


type
  TResponseManager = class;


  TAncestorEmbendedFormClass = class of TAncestorEmbendedForm;
  TAncestorEmbendedForm = class(TForm)
    Panel5: TPanel;
    Panel1: TPanel;
    Label2: TLabel;
    Panel2: TPanel;
    Label3: TLabel;
    Panel3: TPanel;
    Label4: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    {Buttons}
    FResponseManager : TResponseManager;
    FResponseKind    : TResponseType;

    procedure EventsInitialize;
    procedure SetResponseKind(const Value: TResponseType);

  private
    procedure Initialize;
    procedure DoEmbendedShow; virtual;
    procedure DoEmbendedHide; virtual;
  protected
    { --- Surcharge des events de la fenêtre }
    procedure DoOnShow; virtual;
    procedure DoOnHide; virtual;
    procedure DoOnCreate; virtual;
    procedure DoOnDestroy; virtual;

    procedure DoFormClic(Sender: TObject); virtual;
  public
    property ResponseKind: TResponseType read FResponseKind write SetResponseKind;
  end;

  TResponseButton = class
  private
    FResponse : TPanel;
    FText     : TLabel;
    procedure SetCaption(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
    procedure Activate;
    procedure DisActivate;
    procedure Initialize(const ACaption: string);
    function  GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
  private
    procedure MouseEnter(Sender: TObject);
    procedure MouseLeave(Sender: TObject);
  public
    property Visible: Boolean read GetVisible write SetVisible;
    constructor Create(const APanel: TPanel; const AText: TLabel; ACaption: string);
  end;

  TResponseManager = class
  private
    FOwner          : TAncestorEmbendedForm;
    FLeftResponse   : TResponseButton;
    FMiddleResponse : TResponseButton;
    FRightResponse  : TResponseButton;
    FResponseType   : TResponseType;
    procedure Initialize;
    procedure DoResponse(const MiddleText: string); overload;
    procedure DoResponse(const LeftText, RightText: string); overload;
    procedure NoResponse;
    procedure OkResponse;
    procedure OkCancelResponse;
    procedure YesNoResponse;
  public
    procedure SetResponseType(const Value: TResponseType);

    constructor Create(const AOwner: TAncestorEmbendedForm);
  end;


var
  AncestorEmbendedForm: TAncestorEmbendedForm;

implementation

uses
  ScreenDlg, Main;

{$R *.dfm}

function NoneStr:string;
begin
  Result := EmptyStr
end;

function OkStr:string;
begin
  Result := StrResponseKind[rk_ok]
end;

function CancelStr:string;
begin
  Result := StrResponseKind[rk_cancel]
end;

function YesStr:string;
begin
  Result := StrResponseKind[rk_yes]
end;

function NoStr:string;
begin
  Result := StrResponseKind[rk_no]
end;

{ TResponseButton }

procedure TResponseButton.Activate;
begin
  SetColor(ENABLECOLOR);
  SetTextColor(ENABLETEXT)
end;

constructor TResponseButton.Create(const APanel: TPanel; const AText: TLabel;
  ACaption: string);
begin
  inherited Create;
  FResponse := APanel;
  FText     := AText;
  Visible   := False;
  Initialize(ACaption);
end;

procedure TResponseButton.DisActivate;
begin
  SetColor(DISABLECOLOR);
  SetTextColor(DISABLETEXT)
end;

function TResponseButton.GetVisible: Boolean;
begin
  with FResponse do Result := Visible
end;

procedure TResponseButton.Initialize(const ACaption: string);
begin
  SetColor(DISABLECOLOR);
  SetTextColor(DISABLETEXT);
  SetCaption(ACaption);
  with FText do begin
    Transparent  := True;
    Cursor       := crHandPoint;
    OnMouseEnter := MouseEnter;
    OnMouseLeave := MouseLeave
  end
end;

procedure TResponseButton.MouseEnter(Sender: TObject);
begin
  Activate
end;

procedure TResponseButton.MouseLeave(Sender: TObject);
begin
  DisActivate
end;

procedure TResponseButton.SetCaption(const Value: string);
begin
  with FText do Caption := Value
end;

procedure TResponseButton.SetColor(const Value: TColor);
begin
  with FResponse do Color := Value
end;

procedure TResponseButton.SetTextColor(const Value: TColor);
begin
  with FText.Font do Color := Value
end;

procedure TResponseButton.SetVisible(const Value: Boolean);
begin
  with FResponse do Visible := Value
end;

{ TResponseManager }

constructor TResponseManager.Create(const AOwner: TAncestorEmbendedForm);
begin
  inherited create;
  FOwner := AOwner;
  Initialize
end;

procedure TResponseManager.DoResponse(const MiddleText: string);
begin
  NoResponse;
  with FMiddleResponse do begin
    Visible := True;
    SetCaption(MiddleText);
  end
end;

procedure TResponseManager.DoResponse(const LeftText, RightText: string);
begin
  NoResponse;
  with FLeftResponse do begin
    Visible := True;
    SetCaption(LeftText);
  end;
  with FRightResponse do begin
    Visible := True;
    SetCaption(RightText);
  end
end;

procedure TResponseManager.Initialize;
begin
  with FOwner do begin
    FLeftResponse   := TResponseButton.Create(Panel1, Label2, EmptyStr);
    FMiddleResponse := TResponseButton.Create(Panel2, Label3, EmptyStr);
    FRightResponse  := TResponseButton.Create(Panel3, Label4, EmptyStr); 
  end
end;

procedure TResponseManager.NoResponse;
begin
  with FLeftResponse   do Visible := False;
  with FMiddleResponse do Visible := False;
  with FRightResponse  do Visible := False;
end;

procedure TResponseManager.OkCancelResponse;
begin
  DoResponse(OkStr, CancelStr)
end;

procedure TResponseManager.OkResponse;
begin
  DoResponse( OkStr )
end;

procedure TResponseManager.SetResponseType(const Value: TResponseType);
begin
  case Value of
    rt_none      : NoResponse;
    rt_ok        : OkResponse;
    rt_ok_cancel : OkCancelResponse;
    rt_yes_no    : YesNoResponse;
  end
end;

procedure TResponseManager.YesNoResponse;
begin
  DoResponse(YesStr, NoStr)
end;

{ TAncestorEmbendedForm }

procedure TAncestorEmbendedForm.DoEmbendedHide;
begin
  with Recorder do ModeFonctionnement := mf_none;
  SetParent(nil)
end;

procedure TAncestorEmbendedForm.Initialize;
begin
  BorderStyle := bsNone;
  FormStyle   := fsStayOnTop;
  Color       := $002E2E2E;
  Position    := poDefaultPosOnly;
  OnClick     := DoFormClic;
end;

procedure TAncestorEmbendedForm.DoEmbendedShow;
begin
  with ScreenDialog do begin
    Width  := Self.Width;
    Height := Self.Height;
  end;
  SetParent(ScreenDialog)
end;

procedure TAncestorEmbendedForm.FormShow(Sender: TObject);
begin
  DoEmbendedShow;
  DoOnShow
end;

procedure TAncestorEmbendedForm.FormHide(Sender: TObject);
begin
  with TForm(Self.Parent) do try Hide except end;
  DoOnHide;
  DoEmbendedHide
end;

procedure TAncestorEmbendedForm.FormCreate(Sender: TObject);
begin
  Initialize;
  DoOnCreate
end;

procedure TAncestorEmbendedForm.DoOnShow;
begin

end;

procedure TAncestorEmbendedForm.DoOnHide;
begin

end;

procedure TAncestorEmbendedForm.DoOnCreate;
begin
  FResponseManager := TResponseManager.Create(Self);
  EventsInitialize;
end;

procedure TAncestorEmbendedForm.DoOnDestroy;
begin
  FreeAndNil( FResponseManager )
end;

procedure TAncestorEmbendedForm.FormDestroy(Sender: TObject);
begin
  DoOnDestroy
end;

procedure TAncestorEmbendedForm.DoFormClic(Sender: TObject);
begin
  with MainForm do SetFocus
end;

procedure TAncestorEmbendedForm.EventsInitialize;
begin
  with Label2 do OnClick := DoFormClic;
  with Label3 do OnClick := DoFormClic;
  with Label4 do OnClick := DoFormClic;
  with Panel1 do OnClick := DoFormClic;
  with Panel2 do OnClick := DoFormClic;
  with Panel3 do OnClick := DoFormClic;
  with Panel5 do OnClick := DoFormClic;
end;

procedure TAncestorEmbendedForm.SetResponseKind(const Value: TResponseType);
begin
  FResponseKind := Value;
  FResponseManager.SetResponseType( Value )
end;

end.
