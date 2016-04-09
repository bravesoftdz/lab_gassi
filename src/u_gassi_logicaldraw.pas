unit u_gassi_logicaldraw;

//************************************************************
//
//    Модуль компонента Graphic Assi Control
//    Copyright (c) 2013  Pichugin M.
//    ver. 0.5
//    Разработчик: Pichugin M. (e-mail: pichugin_m@mail.ru)
//
//************************************************************

interface

uses
{$IFNDEF FPC}

{$ELSE}

{$ENDIF}
  SysUtils, Variants, Classes, graphics, u_gassi_const;

type

  { Standard events }

  TSetFontStyleCustomDrawEvent = procedure (FontName: AnsiString; FontSize: Double; FontStyle: TFontStyles) of object;
  TGetTextWidthEvent = procedure (Text: AnsiString; var Width: Double) of object;
  TGetTextHeightEvent = procedure (Text: AnsiString; var Height: Double) of object;

  TSetStyleCustomDrawEvent = procedure(LineType:String; LineWidth:TgaLineWeight; Color:TgaColor) of object;
  TPointCustomDrawEvent = procedure(X, Y: Double) of object;
  TLineCustomDrawEvent = procedure(X1, Y1, X2, Y2: Double) of object;
  TRectangelCustomDrawEvent = procedure(TopLeftX, TopLeftY, BottomRightX, BottomRightY: Double) of object;
  TCircleCustomDrawEvent = procedure(X, Y, Radius: Double) of object;

  TEllipseCustomDrawEvent = procedure(X0, Y0, AxleX, AxleY: Double) of object;
  TArcCustomDrawEvent = procedure(X0, Y0, X1, Y1, X2, Y2, Radius: Double) of object;
  TTextCustomDrawEvent = procedure(X0, Y0, AWidth, AHeight: Double; ARotate:integer; AText:String; AAlign:TgaAttachmentPoint) of object;
  TVertexCustomDrawEvent = procedure(X, Y: Double; ATypeVertex:Integer) of object;

  TMinMaxPoint = record
    Xmin, Ymin, Zmin: Double;
    Xmax, Ymax, Zmax: Double;
  end;

  TLogicalDraw = class
  private
    FDevelop          : Boolean; //Режим отладки
    FOnSetStyle       : TSetStyleCustomDrawEvent;
    FOnSetFontStyle   : TSetFontStyleCustomDrawEvent;
    FOnPointDraw      : TPointCustomDrawEvent;
    FOnLineDraw       : TLineCustomDrawEvent;
    FOnRectangelDraw  : TRectangelCustomDrawEvent;
    FOnCircleDraw     : TCircleCustomDrawEvent;
    FOnEllipseDraw    : TEllipseCustomDrawEvent;
    FOnArcDraw        : TArcCustomDrawEvent;
    FOnTextDraw       : TTextCustomDrawEvent;
    FOnVertexDraw     : TVertexCustomDrawEvent;
    FOnGetTextWidth   : TGetTextWidthEvent;
    FOnGetTextHeight  : TGetTextHeightEvent;
  protected

  public
    property Develop          : Boolean read FDevelop write FDevelop;
    property OnSetStyle       : TSetStyleCustomDrawEvent read FOnSetStyle write FOnSetStyle;
    property OnSetFontStyle   : TSetFontStyleCustomDrawEvent read FOnSetFontStyle write FOnSetFontStyle;
    property OnPointDraw      : TPointCustomDrawEvent read FOnPointDraw write FOnPointDraw;
    property OnLineDraw       : TLineCustomDrawEvent read FOnLineDraw write FOnLineDraw;
    property OnRectangelDraw  : TRectangelCustomDrawEvent read FOnRectangelDraw write FOnRectangelDraw;
    property OnCircleDraw     : TCircleCustomDrawEvent read FOnCircleDraw write FOnCircleDraw;
    property OnEllipseDraw    : TEllipseCustomDrawEvent read FOnEllipseDraw write FOnEllipseDraw;
    property OnArcDraw        : TArcCustomDrawEvent read FOnArcDraw write FOnArcDraw;
    property OnTextDraw       : TTextCustomDrawEvent read FOnTextDraw write FOnTextDraw;
    property OnVertexDraw     : TVertexCustomDrawEvent read FOnVertexDraw write FOnVertexDraw;
    property OnGetTextWidth   : TGetTextWidthEvent read FOnGetTextWidth write FOnGetTextWidth;
    property OnGetTextHeight  : TGetTextHeightEvent read FOnGetTextHeight write FOnGetTextHeight;

    procedure SetStyleDraw(LineType:String; LineWidth:TgaLineWeight; Color:TgaColor);
    procedure SetFontStyleDraw(FontName: AnsiString; FontSize: Double; FontStyle: TFontStyles);
    procedure PointDraw(X, Y: Double);
    procedure LineDraw(X1, Y1, X2, Y2: Double);
    procedure RectangelDraw(TopLeftX, TopLeftY, BottomRightX, BottomRightY: Double);
    procedure CircleDraw(X, Y, Radius: Double);

    procedure EllipseDraw(X0, Y0, AxleX, AxleY: Double);
    procedure ArcDraw(X0, Y0, X1, Y1, X2, Y2, Radius: Double);
    procedure TextDraw(X0, Y0, Width, Height: Double; Rotate:integer; Text:String; Align:TgaAttachmentPoint);
    procedure GetTextWidth(Text: AnsiString; var Width: Double);
    procedure GetTextHeight(Text: AnsiString; var Height: Double);

    procedure VertexDraw(X, Y: Double; ATypeVertex:Integer);
  end;

implementation

{ TLogicalDraw }

procedure TLogicalDraw.ArcDraw(X0, Y0, X1, Y1, X2, Y2, Radius: Double);
begin
    if Assigned(FOnArcDraw) then FOnArcDraw(X0, Y0, X1, Y1, X2, Y2, Radius);
end;

procedure TLogicalDraw.CircleDraw(X, Y, Radius: Double);
begin
    if Assigned(FOnCircleDraw) then FOnCircleDraw(X, Y, Radius);
end;

procedure TLogicalDraw.EllipseDraw(X0, Y0, AxleX, AxleY: Double);
begin
    if Assigned(FOnEllipseDraw) then FOnEllipseDraw(X0, Y0, AxleX, AxleY);
end;

procedure TLogicalDraw.GetTextHeight(Text: AnsiString;
  var Height: Double);
begin
    if Assigned(OnGetTextHeight) then OnGetTextHeight(Text, Height);
end;

procedure TLogicalDraw.GetTextWidth(Text: AnsiString; var Width: Double);
begin
    if Assigned(OnGetTextWidth) then OnGetTextWidth(Text, Width);
end;

procedure TLogicalDraw.LineDraw(X1, Y1, X2, Y2: Double);
begin
    if Assigned(FOnLineDraw) then FOnLineDraw(X1, Y1, X2, Y2);
end;

procedure TLogicalDraw.PointDraw(X, Y: Double);
begin
    if Assigned(FOnPointDraw) then FOnPointDraw(X, Y);
end;


procedure TLogicalDraw.RectangelDraw(TopLeftX, TopLeftY, BottomRightX, BottomRightY: Double);
begin
    if Assigned(FOnSetFontStyle) then
    FOnRectangelDraw(TopLeftX, TopLeftY, BottomRightX, BottomRightY);
end;


procedure TLogicalDraw.SetFontStyleDraw(FontName: AnsiString; FontSize: Double;
  FontStyle: TFontStyles);
begin
    if Assigned(FOnSetFontStyle) then
    FOnSetFontStyle(FontName,FontSize,FontStyle);
end;

procedure TLogicalDraw.SetStyleDraw(LineType:String; LineWidth:TgaLineWeight; Color:TgaColor);
begin
    if Assigned(FOnSetStyle) then
    FOnSetStyle(LineType,LineWidth,Color);
end;

procedure TLogicalDraw.TextDraw(X0, Y0, Width, Height: Double; Rotate:integer; Text:String; Align:TgaAttachmentPoint);
begin
    if Assigned(FOnTextDraw) then
    FOnTextDraw(X0, Y0, Width, Height, Rotate, Text, Align);
end;

procedure TLogicalDraw.VertexDraw(X, Y: Double; ATypeVertex: Integer);
begin
    if Assigned(FOnVertexDraw) then
    FOnVertexDraw(X, Y, ATypeVertex);
end;

end.

