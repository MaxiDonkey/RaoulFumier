object HelpDlg: THelpDlg
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'HelpDlg'
  ClientHeight = 732
  ClientWidth = 1700
  Color = 1315860
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clGray
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    1700
    732)
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 8
    Top = 56
    Width = 281
    Height = 319
    Anchors = [akLeft, akTop, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = 1315860
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Pitch = fpFixed
    Font.Style = [fsBold]
    ItemHeight = 23
    ParentFont = False
    ScrollWidth = 10
    TabOrder = 0
  end
  object ListBox2: TListBox
    Left = 312
    Top = 56
    Width = 177
    Height = 319
    Anchors = [akLeft, akTop, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = 1315860
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ItemHeight = 23
    ParentFont = False
    TabOrder = 1
  end
  object Memo1: TRichEdit
    Left = 507
    Top = 56
    Width = 246
    Height = 319
    TabStop = False
    Anchors = [akTop, akRight, akBottom]
    BorderStyle = bsNone
    Color = 1315860
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 33023
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    WantTabs = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 683
    Width = 1700
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    Color = 2960685
    ParentBackground = False
    TabOrder = 3
    object Label5: TLabel
      Left = 216
      Top = 0
      Width = 681
      Height = 38
      AutoSize = False
      Caption = 
        'Prononcer "AIDE RAOUL", "AIDE ELITE" ou "AIDE NAVIGATION" pour c' +
        'hanger de cat'#233'gorie'
      Color = 2960685
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = True
      Layout = tlBottom
    end
    object Panel2: TPanel
      Left = 281
      Top = 0
      Width = 1127
      Height = 49
      Align = alClient
      BevelOuter = bvNone
      Color = 789516
      TabOrder = 0
      object Label3: TLabel
        Left = 168
        Top = 0
        Width = 817
        Height = 38
        AutoSize = False
        Caption = 
          'Prononcer "AIDE RAOUL", "AIDE ELITE" , "AIDE NAVIGATION" ou "AID' +
          'E ODYSSEY" pour changer de cat'#233'gorie'
        Color = 2960685
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 33023
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = True
        Layout = tlBottom
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 281
      Height = 49
      Align = alLeft
      BevelOuter = bvNone
      Color = 789516
      TabOrder = 1
      object Label4: TLabel
        Left = 8
        Top = 0
        Width = 273
        Height = 38
        AutoSize = False
        Caption = 'Prononcer B ou H suivi du num'#233'ro'
        Color = 2960685
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 33023
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = True
        Layout = tlBottom
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 8
        Height = 49
        Align = alLeft
        BevelOuter = bvNone
        Color = 789516
        TabOrder = 0
      end
    end
    object Panel7: TPanel
      Left = 1408
      Top = 0
      Width = 292
      Height = 49
      Align = alRight
      BevelOuter = bvNone
      Color = 789516
      TabOrder = 2
      object Label6: TLabel
        Left = 0
        Top = -11
        Width = 273
        Height = 49
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Prononcer "Echap" pour refermer'
        Color = 2960685
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 33023
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = True
        Layout = tlBottom
      end
      object Panel8: TPanel
        Left = 284
        Top = 0
        Width = 8
        Height = 49
        Align = alRight
        BevelOuter = bvNone
        Color = 789516
        TabOrder = 0
      end
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 0
    Width = 1700
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    Color = 789516
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 0
      Width = 745
      Height = 33
      AutoSize = False
      Caption = 'Label1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 33023
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
      Layout = tlBottom
    end
    object Label2: TLabel
      Left = 10
      Top = 28
      Width = 115
      Height = 13
      Caption = 'Aide Elite dangerous'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
  end
  object cxCheckBox1: TcxCheckBox
    Left = 8
    Top = 652
    Anchors = [akLeft, akBottom]
    Caption = 'Ne pas ouvrir au d'#233'marrage'
    ParentFont = False
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clGray
    Style.Font.Height = -16
    Style.Font.Name = 'Tahoma'
    Style.Font.Style = [fsBold]
    Style.LookAndFeel.SkinName = 'Darkroom'
    Style.IsFontAssigned = True
    StyleDisabled.LookAndFeel.SkinName = 'Darkroom'
    StyleFocused.LookAndFeel.SkinName = 'Darkroom'
    StyleHot.LookAndFeel.SkinName = 'Darkroom'
    TabOrder = 5
    Width = 257
  end
end
