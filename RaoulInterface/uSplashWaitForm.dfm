object SplashWaitForm: TSplashWaitForm
  Left = 0
  Top = 0
  AlphaBlend = True
  AlphaBlendValue = 210
  BorderStyle = bsNone
  Caption = 'SplashWaitForm'
  ClientHeight = 212
  ClientWidth = 386
  Color = 2434341
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -27
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    386
    212)
  PixelsPerInch = 96
  TextHeight = 33
  object Label1: TLabel
    Left = 0
    Top = 68
    Width = 386
    Height = 33
    Alignment = taCenter
    Anchors = [akLeft, akRight]
    AutoSize = False
    Caption = 'My message'
    Transparent = True
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 0
    Top = 112
    Width = 386
    Height = 33
    Alignment = taCenter
    Anchors = [akLeft, akRight]
    AutoSize = False
    Caption = '                    Please wait...'
    Color = 2302755
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    Layout = tlCenter
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 386
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Color = 2105376
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 171
    Width = 386
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Color = 2105376
    TabOrder = 1
  end
end
