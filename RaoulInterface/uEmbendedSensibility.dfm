inherited EmbendedSensibility: TEmbendedSensibility
  BorderStyle = bsNone
  Caption = 'EmbendedSensibility'
  ClientHeight = 486
  ClientWidth = 828
  PixelsPerInch = 96
  TextHeight = 39
  inherited Panel5: TPanel
    Width = 828
    Height = 486
    inherited Panel1: TPanel
      Top = 412
    end
    inherited Panel2: TPanel
      Top = 412
    end
    inherited Panel3: TPanel
      Top = 412
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 665
    Height = 401
    BevelOuter = bvNone
    Color = 3421236
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      665
      401)
    object Label5: TLabel
      Left = 212
      Top = 25
      Width = 341
      Height = 39
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Hautes performances'
    end
    object Label8: TLabel
      Left = 343
      Top = 125
      Width = 210
      Height = 39
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Micro casque'
    end
    object Label9: TLabel
      Left = 307
      Top = 224
      Width = 246
      Height = 39
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Micro de studio'
    end
    object Label10: TLabel
      Left = 288
      Top = 323
      Width = 265
      Height = 39
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Micro d'#39'enceinte'
    end
    object cxTrackBar1: TcxTrackBar
      Left = 584
      Top = 34
      Anchors = [akTop, akRight]
      Properties.Max = 3
      Properties.Orientation = tboVertical
      Properties.SelectionColor = 33023
      Properties.SelectionEnd = 3
      Properties.ThumbColor = clGray
      Properties.TickColor = clGray
      Properties.TickMarks = cxtmBoth
      Properties.TickSize = 18
      Properties.TrackColor = 5131854
      Properties.OnChange = cxTrackBar1PropertiesChange
      Style.LookAndFeel.SkinName = ''
      StyleDisabled.LookAndFeel.SkinName = ''
      StyleFocused.LookAndFeel.SkinName = ''
      StyleHot.LookAndFeel.SkinName = ''
      TabOrder = 0
      Height = 326
      Width = 60
    end
    object cxLabel1: TcxLabel
      Left = 120
      Top = 16
      AutoSize = False
      Caption = 'SENSIBILITE      '
      ParentColor = False
      ParentFont = False
      Style.Color = 2434341
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clGray
      Style.Font.Height = -13
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = [fsBold]
      Style.TextColor = 33023
      Style.IsFontAssigned = True
      Properties.Angle = 90
      Height = 385
      Width = 25
    end
  end
end
