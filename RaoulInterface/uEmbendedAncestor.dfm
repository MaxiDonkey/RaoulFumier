object AncestorEmbendedForm: TAncestorEmbendedForm
  Left = 0
  Top = 0
  Width = 828
  Height = 542
  Caption = 'AncestorEmbendedForm'
  Color = 3421236
  TransparentColor = True
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clGray
  Font.Height = -32
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 39
  object Panel5: TPanel
    Left = 0
    Top = 0
    Width = 812
    Height = 504
    Align = alClient
    BevelOuter = bvNone
    Color = 3421236
    TabOrder = 0
    DesignSize = (
      812
      504)
    object Panel1: TPanel
      Left = 36
      Top = 440
      Width = 185
      Height = 41
      Anchors = [akLeft, akBottom]
      Color = 3421236
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object Label2: TLabel
        Left = 1
        Top = 1
        Width = 183
        Height = 39
        Align = alClient
        Alignment = taCenter
        Caption = 'Oui'
        Transparent = True
        Layout = tlCenter
      end
    end
    object Panel2: TPanel
      Left = 320
      Top = 440
      Width = 185
      Height = 41
      Anchors = [akLeft, akBottom]
      Color = 3421236
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object Label3: TLabel
        Left = 1
        Top = 1
        Width = 183
        Height = 39
        Align = alClient
        Alignment = taCenter
        Caption = 'Ok'
        Transparent = True
        Layout = tlCenter
      end
    end
    object Panel3: TPanel
      Left = 596
      Top = 440
      Width = 185
      Height = 41
      Anchors = [akLeft, akBottom]
      Color = 3421236
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      object Label4: TLabel
        Left = 1
        Top = 1
        Width = 183
        Height = 39
        Align = alClient
        Alignment = taCenter
        Caption = 'Non'
        Transparent = True
        Layout = tlCenter
      end
    end
  end
end
