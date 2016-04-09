unit u_gassi_drawcontrol;

//************************************************************
//
//    Модуль компонента Graphic Assi Control
//    Copyright (c) 2013  Pichugin M.
//    ver. 0.16
//    Разработчик: Pichugin M. (e-mail: pichugin_m@mail.ru)
//
//************************************************************

{$mode objfpc}{$H+}

interface

uses
{$IFNDEF FPC}

{$ELSE}
  Windows,//LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, forms,Math,
  Dialogs, ExtCtrls, u_gassi_logicaldraw,u_gassi_const,u_gassi_visualobjects;

// ver. 0.16
// Добавлена функция создания рамки вокруг экрана с текстом в углу.
// Новые процедуры:
// - FrameViewModeSet
// - FrameViewModeClear
//
// ver. 0.15
// Обработка событий OnEvent переработана
//
// ver. 0.14
// Измена архитектура получения доступа объектов к классу TDrawDocumentCustom
// $mode objfpc
//
// ver. 0.13
// Исправлена ошибка указателя при выполнении Destroy
//
// ver. 0.12
// Добавлен Rotate в процедурах вывода текста
// Объявлены свойства цветов
// Новые процедуры:
// - OnBeforeDrawEvent
// - OnAfterDrawEvent
//
// ver. 0.11
// Добавлена функция вывода сообщений AddMessageToUser();
//
// ver. 0.10
// Новые процедуры:
// - OnEntityBeforeDrawEvent
// - OnEntityAfterDrawEvent

{  TODO LIST  }

 //todo: Не реализована функция поворота объектов по свойству Rotate
 //todo: Примитивно сделана арка
 //todo: Скорость скроллинга надо улучшить
 //todo: Выбор в лево и выбор в право не работает на блоках
 //      это связано с свойствами (SelectLeftColor, SelectRightColor)

type

  { Data types }

  TEntitySelectEvent         = procedure(Sender: TObject; AEntity:TEntity;
                               var CanSelect:Boolean) of object;
  TEntityBeforeDrawEvent     = procedure(Sender: TObject; AEntity:TEntity;
                               var CanDraw:Boolean) of object;
  TEntityAfterDrawEvent      = procedure(Sender: TObject;
                               AEntity:TEntity) of object;
  TExtHintBeforeDrawEvent    = procedure(Sender: TObject;
                               var AWidth, AHeight:integer;
                               ACanvas:TCanvas) of object;
  TMouseMoveUnderEntityEvent = procedure(Sender: TObject;
                               AEntity:TEntity) of object;

  TEntityEditMode            = (eemCanAll, eemReadOnly, eemSelectOnly);
  TgaControlAction           = set of (caNone,caZoomToFit,caMoveSpace,
                                   caSelectObject,caClickLeft,
                                   caClickRight,caMoveVertex);
  TAffectedAreaSelectOptions = set of (aasoOUTSIDE,aasoBASEPOINT,
                                   aasoVERTEX,aasoINSIDE,aasoBORDER);

  { Forward Declarartions }

  TAssiDrawControl = class;

  { TAssiDrawDocument }

  TAssiDrawDocument = class(TDrawDocumentCustom)
  private
    FDrawControl        :TAssiDrawControl;
    FOnChange           :TNotifyEvent;
    FOnSelectListChange :TNotifyEvent;
    function GetDocument:TDrawDocumentCustom;
  protected
    FEditMode           :TEntityEditMode;
    FSelectList         :TList;
    FMVertArray         :TModifyVertexArray;

    FViewScale          :Integer;  //100 def min 0 max 99999999...9
    FViewScaleK         :Integer;
    FViewPos            :TFloatPoint;
    FPointUnit          :TPointUnit; // Единици измерения
    FPointPrecision     :Integer; //Точность
    FDefaultColor       :TgaColor;
    FDefaultLineWeight  :TgaLineWeight;
  published

    property  EditMode:TEntityEditMode read FEditMode write FEditMode;
    property  DefaultColor: TgaColor read FDefaultColor write FDefaultColor;
    property  DefaultLineWeight: TgaLineWeight read FDefaultLineWeight
                                               write FDefaultLineWeight;
    property  ViewScale: integer read FViewScale write FViewScale;

  public
    constructor Create(AOwner: TComponent);  virtual;
    destructor Destroy; override;

    property  ViewPos: TFloatPoint read FViewPos
                                   write FViewPos;
    property  OnChange: TNotifyEvent read FOnChange
                                     write FOnChange;
    property  OnSelectListChange:TNotifyEvent read FOnSelectListChange
                                              write FOnSelectListChange;
    property  PointUnit: TPointUnit read FPointUnit
                                    write FPointUnit;
    property  PointPrecision: Integer read FPointPrecision
                                      write FPointPrecision;
    property  SelectList :TList read FSelectList
                                write FSelectList;
    property  DrawControl :TAssiDrawControl read FDrawControl
                                            write FDrawControl;
    property  ModelSpace  :TWorkSpace read FModelSpace
                                      write FModelSpace;
    property  Blocks      :TBlockList read FBlockList
                                      write FBlockList;

    function GetDeltaVertex:Double; override;
    procedure MVertArray(Value:TModifyVertex);
    procedure DeselectAll; override;
    procedure ZoomToFit;
  end;

  { TAssiDrawControl }

  TAssiDrawControl = class(TPaintBox)
  protected
    FOnSelectListChange              :TNotifyEvent;
    FOnEntitySelectEvent             :TEntitySelectEvent;
    FOnEntityBeforeDrawEvent         :TEntityBeforeDrawEvent;
    FOnEntityAfterDrawEvent          :TEntityAfterDrawEvent;
    FOnBeforeDrawEvent               :TNotifyEvent;
    FOnAfterDrawEvent                :TNotifyEvent;
    FOnExtHintBeforeDrawEvent        :TExtHintBeforeDrawEvent;
    FMouseMoveUnderEntityEvent       :TMouseMoveUnderEntityEvent;
    FMessagesLast                    :String;
    FMessagesList                    :TStringList;
    FTimerMessage                    :TTimer;
    // Настройки
    FDevelop                         :Boolean; //Режим отладки
    FDeltaCord                       :Integer; //Размеры вершин
    FShowAxes                        :Boolean; // Отображать нулевую точку
    FCursorColor                     :TColor;
    FVertexBasePointColor            :TColor;
    FVertexCustomColor               :TColor;
    FVertexSelectColor               :TColor;
    FBackgroundColor                 :TColor;
    FDrawCursor                      :Boolean; // Отображать свой курсор
    FSelectLeftColor                 :TColor;
    FSelectRightColor                :TColor;
    FDefaultFont                     :TFont;
    FSelectStyle                     :TAffectedAreaSelectOptions;
    //Хранилище
    FDocument                        :TAssiDrawDocument;
    //Переменные состоянния
    FDrawFont                        :Boolean;
    FMouseButtonPressed              :Boolean;
    FMouseButtonUpPos                :TPoint;  //Запоминаем положение при отпускании кнопки
    FMouseButtonUp                   :TMouseButton;
    FMouseButtonDownPos              :TPoint; //Запоминаем положение при нажатии кнопки
    FMouseButtonDown                 :TMouseButton;
    FMouseButtonUpShift              :TShiftState;
    FMouseButtonDownShift            :TShiftState;
    FClickCount                      :SmallInt;
    FtmpViewPos                      :TFloatPoint;
    FControlAction                   :TgaControlAction;
    FKStep                           :Integer;
    FCurSec                          :Integer;

    FCursorPos                       :TPoint;
    vbmHeight, vbpWidth              :Integer;
    //Виртуальные области кеширования
    FFormWindowProc                  :TWndMethod;
    FLogicalDraw                     :TLogicalDraw;
    FVirtualBitMap                   :TBitMap;
    FVirtualCanvas                   :TCanvas;

    FEntityFirstDrawBitMap           :TBitMap;
    FHintDrawBitMap                  :TBitMap;

    FSelfOnClick                     :TNotifyEvent;
    FSelfOnContextPopup              :TContextPopupEvent;
    FSelfOnDblClick                  :TNotifyEvent;
    FSelfOnMouseDown                 :TMouseEvent;
    //FSelfOnMouseEnter                :TNotifyEvent;
    //FSelfOnMouseLeave                :TNotifyEvent;
    FSelfOnMouseMove                 :TMouseMoveEvent;
    FSelfOnMouseUp                   :TMouseEvent;
    FSelfOnMouseWheel                :TMouseWheelEvent;
    FSelfOnMouseWheelDown            :TMouseWheelUpDownEvent;
    FSelfOnMouseWheelUp              :TMouseWheelUpDownEvent;
    FSelfOnPaint                     :TNotifyEvent;

    FFrameViewModeText               :String;
    FFrameViewModeColor              :TColor;
  private
    FOnEditingDone: TNotifyEvent;
    // Рисование интерфейса
    procedure SuperVirtualPaint(Sender: TObject);
    procedure SuperCursorPaint(Sender: TObject);
    procedure SuperExtHintPaint(Sender: TObject);
    procedure SuperControlPaint(Sender: TObject);
    procedure SuperPaint(Sender: TObject);
    procedure SuperMessagesPaint(Sender: TObject);
    procedure SuperFrameViewModePaint(Sender: TObject);

    procedure ZeroPointCSPaint;
    procedure VertexPaint(X,Y: Double); overload;
    procedure VertexPaint(X,Y: Integer); overload;
    procedure SelectRectDoPaint(Sender: TObject);
    procedure SelectRectPaint(X1, Y1, X2, Y2: Integer);
    procedure VertexDraw(X, Y: Double; ATypeVertex: Integer);

    // Рисование примитивов
    procedure Repaint;
    procedure RepaintVertex;
    //
    procedure SetFontStyleDraw(FontName: AnsiString;FontSize: Double;FontStyle: TFontStyles);
    procedure SetStyleDraw(LineType:String; LineWidth:TgaLineWeight; AColor:TgaColor);
    procedure LineDraw(X1, Y1, X2, Y2: Double);
    procedure RectangelDraw(TopLeftX, TopLeftY, BottomRightX, BottomRightY: Double);
    procedure CircleDraw(X, Y, Radius: Double);
    procedure ArcDraw(X0, Y0, X1, Y1, X2, Y2, Radius: Double);
    procedure PointDraw(X, Y: Double);
    procedure EllipseDraw(X0, Y0, AxleX, AxleY: Double);
    procedure TextOutTransperent(X,Y:Integer;AText:String);
    procedure TextDraw(X0, Y0, AWidth, AHeight: Double; Rotate:integer; AText:String; AAlign:TgaAttachmentPoint);

    // Отклики
    procedure EndSelecting(Sender: TObject);
    procedure SuperClick(Sender: TObject);
    procedure SuperContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure SuperDblClick(Sender: TObject);
    procedure SuperLeftButtonClick(Sender: TObject);
    procedure SuperEditingDone(Sender: TObject);
    procedure SuperMiddleButtonDblClick(Sender: TObject);
    //procedure SuperMouseEnter(Sender: TObject);
    //procedure SuperMouseLeave(Sender: TObject);
    procedure SuperMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SuperMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SuperMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SuperMouseWheel(Sender: TObject; Shift: TShiftState;WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SuperMouseWheelUp(Sender: TObject; Shift: TShiftState;MousePos: TPoint; var Handled: Boolean);
    procedure SuperMouseWheelDown(Sender: TObject; Shift: TShiftState;MousePos: TPoint; var Handled: Boolean);
    procedure gaMouseAction(Sender: TObject);
    procedure EndMoveVertex(Sender: TObject);
    procedure SuperWndProc(var Message: TMessage);
    procedure gaMoveVertexAction(Sender: TObject);
    procedure RefreshEntityDraw;
    procedure TimerMessageOnTimer(Sender: TObject);

  published
    property DevelopMode        :Boolean read FDevelop write FDevelop;
    property DrawCursor         :Boolean read FDrawCursor write FDrawCursor;
    property ShowAxes           :Boolean read FShowAxes write FShowAxes;

    property BackgroundColor      :TColor read FBackgroundColor write FBackgroundColor;
    property CursorColor          :TColor read FCursorColor write FCursorColor;
    property VertexBasePointColor :TColor read FVertexBasePointColor write FVertexBasePointColor;
    property VertexCustomColor    :TColor read FVertexCustomColor write FVertexCustomColor;
    property VertexSelectColor    :TColor read FVertexSelectColor write FVertexSelectColor;
    property SelectLeftColor      :TColor read FSelectLeftColor write FSelectLeftColor;
    property SelectRightColor     :TColor read FSelectRightColor write FSelectRightColor;

    property DefaultFont          :TFont read FDefaultFont write FDefaultFont;

    //Переопределяемые свойства
    property OnEditingDone: TNotifyEvent read FOnEditingDone write FOnEditingDone;
    property OnContextPopup: TContextPopupEvent read FSelfOnContextPopup write FSelfOnContextPopup;
    property OnDblClick: TNotifyEvent read FSelfOnDblClick write FSelfOnDblClick;
    property OnMouseDown: TMouseEvent read FSelfOnMouseDown write FSelfOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FSelfOnMouseMove write FSelfOnMouseMove;
    property OnMouseUp: TMouseEvent read FSelfOnMouseUp write FSelfOnMouseUp;
    //property OnMouseEnter: TNotifyEvent read FSelfOnMouseEnter write FSelfOnMouseEnter;
    //property OnMouseLeave: TNotifyEvent read FSelfOnMouseLeave write FSelfOnMouseLeave;
    property OnMouseWheel: TMouseWheelEvent read FSelfOnMouseWheel write FSelfOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FSelfOnMouseWheelDown write FSelfOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FSelfOnMouseWheelUp write FSelfOnMouseWheelUp;
    //
    property OnBeforeDrawEvent:TNotifyEvent read FOnBeforeDrawEvent write FOnBeforeDrawEvent;
    property OnAfterDrawEvent:TNotifyEvent read FOnAfterDrawEvent write FOnAfterDrawEvent;

    property OnEntityBeforeDrawEvent:TEntityBeforeDrawEvent read FOnEntityBeforeDrawEvent write FOnEntityBeforeDrawEvent;
    property OnEntityAfterDrawEvent:TEntityAfterDrawEvent read FOnEntityAfterDrawEvent write FOnEntityAfterDrawEvent;
    property OnExtHintBeforeDrawEvent:TExtHintBeforeDrawEvent read FOnExtHintBeforeDrawEvent write FOnExtHintBeforeDrawEvent;

    property OnEntitySelectEvent:TEntitySelectEvent read FOnEntitySelectEvent write FOnEntitySelectEvent;
    property OnSelectListChange:TNotifyEvent read FOnSelectListChange write FOnSelectListChange;

    property OnMouseMoveUnderEntityEvent:TMouseMoveUnderEntityEvent read FMouseMoveUnderEntityEvent write FMouseMoveUnderEntityEvent;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ActiveDocument:TAssiDrawDocument read FDocument write FDocument;
    property SelectStyle:TAffectedAreaSelectOptions read FSelectStyle write FSelectStyle;

    procedure SetDefaultSettings;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SelectObjectRect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean);

    procedure LoadFromFile(FileName:String);
    procedure SaveToFile(FileName:String);

    procedure MsgMouseWheel(var Msg: TWMMouseWheel);
    function PointSCSToPointWCS(X,Y:Integer):TFloatPoint;
    function PointWCSToPointSCS(X,Y:Double):TPoint;
    function ValWCSToValSCS(X:Double):Integer;
    function ValLineWeightToValPixel(X:TgaLineWeight):Integer;
    function ValgaColorToValColor(X:TgaColor):TColor;

    procedure AddMessageToUser(AText:String);
    procedure SetMessageToUser(AText:String);

    procedure FrameViewModeSet(AText: String; AColor:TColor);
    procedure FrameViewModeClear;

  end;

implementation

procedure BeginScreenUpdate(hwnd: THandle);
begin
  try
  SendMessage(hwnd, WM_SETREDRAW, 0, 0);
  finally

  end;
end;

procedure EndScreenUpdate(hwnd: THandle; erase: Boolean);
begin
  try
  SendMessage(hwnd, WM_SETREDRAW, 1, 0);
  {RedrawWindow(hwnd, nil, 0, DW_FRAME + RDW_INVALIDATE +
    RDW_ALLCHILDREN + RDW_NOINTERNALPAINT);  
  if (erase) then
    Windows.InvalidateRect(hwnd, nil, True); }

  finally

  end;
end;

{ TAssiDrawControl }

procedure TAssiDrawControl.BeginUpdate;
begin
     BeginScreenUpdate(Parent.Handle);
end;

procedure TAssiDrawControl.EndUpdate;
begin
     EndScreenUpdate(Parent.Handle,false);
     Invalidate;
end;

constructor TAssiDrawControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument                          :=nil;
  FDevelop                           :=true;//информация для разработчика
  FShowAxes                          :=true;//Показывать нулевую точку с осями
  FDeltaCord                         :=5; //Размеры вершин
  FSelectStyle                       :=[aasoBASEPOINT]+[aasoVERTEX]+[aasoBORDER];
  ActiveDocument                     :=TAssiDrawDocument.Create(self);
  FDefaultFont                       :=TFont.Create;
  if Assigned(ActiveDocument)then
  begin
       ActiveDocument.OnChange       :=@SuperControlPaint;
  end;

  FMessagesList                      :=TStringList.Create;
  FTimerMessage                      :=TTimer.Create(AOwner);
  FTimerMessage.Name                 :=Name+'TimerMessage';
  FTimerMessage.Enabled              :=False;
  FTimerMessage.OnTimer              :=@TimerMessageOnTimer;

  FFrameViewModeText                 :='';
  FFrameViewModeColor                :=clGreen;

  FDrawFont                          :=True;
  FMouseButtonPressed                :=False;
  FMouseButtonUpPos.X                :=0;
  FMouseButtonUpPos.Y                :=0;
  FMouseButtonDownPos.X              :=0;
  FMouseButtonDownPos.Y              :=0;
  FClickCount                        :=0;
  FControlAction                     :=[caNone];

  FKStep                             :=1;
  FCurSec                            :=0;

  FLogicalDraw                      :=TLogicalDraw.create;
  FVirtualBitMap                    :=TBitMap.Create;
  FVirtualCanvas                    :=FVirtualBitMap.Canvas;
  FVirtualBitMap.Transparent        :=false;

  FHintDrawBitMap                   :=TBitMap.Create;
  FHintDrawBitMap.Transparent       :=False;

  FEntityFirstDrawBitMap            :=TBitMap.Create;
  FEntityFirstDrawBitMap.Transparent:=False;

  FFormWindowProc                   :=TCustomForm(AOwner).WindowProc;
  TCustomForm(AOwner).WindowProc    :=@SuperWndProc;//WindowProc

    FSelfOnClick                     :=nil;
    FSelfOnContextPopup              :=nil;
    FSelfOnDblClick                  :=nil;
    FSelfOnMouseDown                 :=nil;
    //FSelfOnMouseEnter                :=nil;
    //FSelfOnMouseLeave                :=nil;
    FSelfOnMouseMove                 :=nil;
    FSelfOnMouseUp                   :=nil;
    FSelfOnMouseWheel                :=nil;
    FSelfOnMouseWheelDown            :=nil;
    FSelfOnMouseWheelUp              :=nil;
    FSelfOnPaint                     :=nil;

   inherited OnClick                 :=@SuperClick;
   inherited OnContextPopup          :=@SuperContextPopup;
   inherited OnDblClick              :=@SuperDblClick;
   inherited OnMouseDown             :=@SuperMouseDown;
   //inherited OnMouseEnter          :=@SuperMouseEnter;
   //inherited OnMouseLeave          :=@SuperMouseLeave;
   inherited OnMouseMove             :=@SuperMouseMove;
   inherited OnMouseUp               :=@SuperMouseUp;
   inherited OnMouseWheel            :=@SuperMouseWheel;
   inherited OnMouseWheelDown        :=@SuperMouseWheelDown;
   inherited OnMouseWheelUp          :=@SuperMouseWheelUp;
   inherited OnPaint                 :=@SuperPaint;

   FLogicalDraw.OnSetStyle           :=@SetStyleDraw;
   FLogicalDraw.OnSetFontStyle       :=@SetFontStyleDraw;
   FLogicalDraw.OnLineDraw           :=@LineDraw;
   FLogicalDraw.OnCircleDraw         :=@CircleDraw;
   FLogicalDraw.OnArcDraw            :=@ArcDraw;
   FLogicalDraw.OnPointDraw          :=@PointDraw;
   FLogicalDraw.OnEllipseDraw        :=@EllipseDraw;
   FLogicalDraw.OnTextDraw           :=@TextDraw;
   FLogicalDraw.OnVertexDraw         :=@VertexDraw;
   FLogicalDraw.OnRectangelDraw      :=@RectangelDraw;

   FOnSelectListChange               :=nil;
   FOnEntitySelectEvent              :=nil;
   FOnEntityBeforeDrawEvent          :=nil;
   FOnEntityAfterDrawEvent           :=nil;
   FOnBeforeDrawEvent                :=nil;
   FOnAfterDrawEvent                 :=nil;
end;

destructor TAssiDrawControl.Destroy;
begin
  if Assigned(Owner) then
  begin
       TCustomForm(Owner).WindowProc :=FFormWindowProc;
  end;
  FFormWindowProc :=nil;

  if Assigned(FDocument) then
  begin
     FDocument.Free;
     FDocument:=nil;
  end;
  FMessagesList.Free;
  FLogicalDraw.Free;
  FVirtualBitMap.Free;
  FEntityFirstDrawBitMap.Free;
  FHintDrawBitMap.Free;
  FDefaultFont.Free;
  inherited Destroy;
end;

procedure TAssiDrawControl.ZeroPointCSPaint;
var
  rsize,lsize:integer;
  ZeroPointCS:TPoint;
begin
  if FShowAxes then
  begin
   ZeroPointCS                              :=PointWCSToPointSCS(0,0);

   rsize                                    :=5;
   lsize                                    :=30;
   // Рисуем нулевую точку координатной системы
   FVirtualCanvas.Font.Assign(FDefaultFont);
   FVirtualCanvas.Brush.Color               :=FCursorColor;
   FVirtualCanvas.Brush.Style               :=bsClear;
   FVirtualCanvas.Pen.Style                 :=psSolid;
   FVirtualCanvas.Pen.Color                 :=FCursorColor;
   FVirtualCanvas.Pen.Mode                  :=pmNot;

   FVirtualCanvas.Pen.Width                 :=1;
   FVirtualCanvas.MoveTo (ZeroPointCS.X,ZeroPointCS.Y);
   FVirtualCanvas.LineTo (ZeroPointCS.X+lsize,ZeroPointCS.Y);

   FVirtualCanvas.MoveTo (ZeroPointCS.X,ZeroPointCS.Y);
   FVirtualCanvas.LineTo (ZeroPointCS.X,ZeroPointCS.Y-lsize);


   FVirtualCanvas.MoveTo (ZeroPointCS.X-rsize,ZeroPointCS.Y-rsize);
   FVirtualCanvas.LineTo (ZeroPointCS.X+rsize,ZeroPointCS.Y-rsize);
   FVirtualCanvas.LineTo (ZeroPointCS.X+rsize,ZeroPointCS.Y+rsize);
   FVirtualCanvas.LineTo (ZeroPointCS.X-rsize,ZeroPointCS.Y+rsize);
   FVirtualCanvas.LineTo (ZeroPointCS.X-rsize,ZeroPointCS.Y-rsize);

   FVirtualCanvas.Brush.Color               :=FBackgroundColor;
   FVirtualCanvas.Brush.Style               :=bsclear;
   FVirtualCanvas.Pen.Color                 :=FBackgroundColor;
   FVirtualCanvas.Font.Color                :=clWhite;
   FVirtualCanvas.TextOut(ZeroPointCS.X+lsize,ZeroPointCS.Y-5,'X');
   FVirtualCanvas.TextOut(ZeroPointCS.X,ZeroPointCS.Y-lsize-15,'Y');

   //TextOutTransperent(ZeroPointCS.X+lsize,ZeroPointCS.Y-5,'X');
   //TextOutTransperent(ZeroPointCS.X,ZeroPointCS.Y-lsize-15,'Y');
  end;
end;

procedure TAssiDrawControl.SelectRectPaint(X1, Y1, X2, Y2: Integer);
var
    ARect:TRect;
begin

if X1<=X2 then
begin
  FVirtualCanvas.Pen.Color:=FSelectRightColor;
  FVirtualCanvas.Pen.Style:=psSolid;
  if Y1<=Y2 then
  begin
    ARect:=Rect(X1,Y1,X2,Y2);
    //ARect:=Rect(Point(X1,Y1),Point(X2,Y2));
  end
  else begin
    ARect:=Rect(X1,Y2,X2,Y1);
  end;
end
else begin
  FVirtualCanvas.Pen.Color:=FSelectLeftColor;
  FVirtualCanvas.Pen.Style:=psDot;
  if Y1<=Y2 then
  begin
    ARect:=Rect(X2,Y1,X1,Y2);
  end
  else begin
    ARect:=Rect(X2,Y2,X1,Y1);
  end;
end;

   FVirtualCanvas.Brush.Color := clblack;
   FVirtualCanvas.Pen.Mode:=pmCopy;
   //FVirtualCanvas.Pen.Color:=clred;
   FVirtualCanvas.Pen.Width:=1;
   //FVirtualCanvas.FrameRect(ARect);
   FVirtualCanvas.MoveTo (ARect.TopLeft.X,ARect.TopLeft.Y);
   FVirtualCanvas.LineTo (ARect.BottomRight.X,ARect.TopLeft.Y);
   FVirtualCanvas.LineTo (ARect.BottomRight.X,ARect.BottomRight.Y);
   FVirtualCanvas.LineTo (ARect.TopLeft.X,ARect.BottomRight.Y);
   FVirtualCanvas.LineTo (ARect.TopLeft.X,ARect.TopLeft.Y);

end;

procedure TAssiDrawControl.SetDefaultSettings;
begin
    FCursorColor                              :=clBlack;
    FSelectLeftColor                          :=clLime;
    FSelectRightColor                         :=clBlue;
    FVertexSelectColor                        :=clRed;
    FVertexBasePointColor                     :=clBlue;
    FVertexCustomColor                        :=clNavy;
    FBackgroundColor                          :=$00BBA7A2;//$00C5ADA7;
    FDrawCursor                               :=true;
    FDefaultFont.Color                        :=not FBackgroundColor;
    FDefaultFont.Size                         :=8;
    FDefaultFont.Style                        :=[];
    FDefaultFont.Name                         :='Arial';
    FLogicalDraw.Develop                      :=FDevelop;
end;

procedure TAssiDrawControl.SelectObjectRect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean);
var
  i,index,Answer   :integer;
  Item             :TEntity;
  MVertx           :TModifyVertex;
  CanSelect,
  CallOnSelectListChange:Boolean;
begin
  CallOnSelectListChange:=False;
  for I := 0 to ActiveDocument.ModelSpace.Objects.Count - 1 do
  begin
      Item:=ActiveDocument.ModelSpace.Objects.Items[i];
      if not (esCreating in Item.State) then
      begin
          Item.State:=Item.State-[esSelected];
          Answer:=Item.GetSelect(TopLeft, BottomRight, AllVertexInRect,MVertx);
          if Answer<>AFFA_OUTSIDE then
          begin
              CanSelect:=True;
              if Assigned(ActiveDocument.FSelectList) then
              begin
                  index:=ActiveDocument.FSelectList.IndexOf(Item);
                  if index>-1 then
                  begin
                     if ssShift in FMouseButtonUpShift then
                     begin
                        ActiveDocument.FSelectList.Remove(Item);
                     end
                     else if (((Answer=AFFA_VERTEX)and(aasoVERTEX in FSelectStyle))
                     or((Answer=AFFA_BASEPOINT)and(aasoBASEPOINT in FSelectStyle)))and(not AllVertexInRect) then
                     begin
                        if not (ssShift in FMouseButtonUpShift) then
                            SetLength(ActiveDocument.FMVertArray,0);
                        ActiveDocument.MVertArray(MVertx);
                     end;
                  end
                  else if (((Answer=AFFA_VERTEX)and(aasoVERTEX in FSelectStyle))
                  or((Answer=AFFA_BASEPOINT)and(aasoBASEPOINT in FSelectStyle))
                  or((Answer=AFFA_INSIDE)and(aasoINSIDE in FSelectStyle))
                  or((Answer=AFFA_BORDER)and(aasoBORDER in FSelectStyle))) then
                  begin
                    if Assigned(FOnEntitySelectEvent) then
                       FOnEntitySelectEvent(Self,Item,CanSelect);
                    if CanSelect then
                    begin
                       ActiveDocument.FSelectList.Add(Item);
                       Item.State:=Item.State+[esSelected];
                    end;
                  end;
                  CallOnSelectListChange:=True;
              end;
          end;
      end;
  end;
  if Assigned(OnSelectListChange)and CallOnSelectListChange then
     OnSelectListChange(self);
end;

{ Super section}

procedure TAssiDrawControl.SuperClick(Sender: TObject);
begin
    if Assigned(FSelfOnClick) then
    FSelfOnClick(Sender);
end;

procedure TAssiDrawControl.SuperContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
    if Assigned(FSelfOnContextPopup) then
    FSelfOnContextPopup(Sender,MousePos,Handled);
end;

procedure TAssiDrawControl.SuperDblClick(Sender: TObject);
begin
    if Assigned(FSelfOnDblClick) then
    FSelfOnDblClick(Sender);
end;

procedure TAssiDrawControl.EndMoveVertex(Sender: TObject);
begin
  if caMoveVertex in FControlAction then
  begin
      SetLength(ActiveDocument.FMVertArray,0);
  end;
end;

procedure TAssiDrawControl.EndSelecting(Sender: TObject);
var
  tmpWCSPoint1,tmpWCSPoint2:TFloatPoint;
  ARect:TRect;
  X1,X2,Y1,Y2:integer;
  AllVertexInRect:boolean;
begin
  
  if (FControlAction=[caSelectObject]) then
  begin
      AllVertexInRect:=false;
      X1:=FMouseButtonDownPos.X;
      Y1:=FMouseButtonDownPos.Y;
      X2:=FMouseButtonUpPos.X;
      Y2:=FMouseButtonUpPos.Y;
      if X1<=X2 then
      begin
        AllVertexInRect:=true;
        if Y1<=Y2 then
        begin
          ARect:=Rect(X1,Y1,X2,Y2);
        end
        else begin
          ARect:=Rect(X1,Y2,X2,Y1);
        end;
      end
      else begin
        AllVertexInRect:=false;
        if Y1<=Y2 then
        begin
          ARect:=Rect(X2,Y1,X1,Y2);
        end
        else begin
          ARect:=Rect(X2,Y2,X1,Y1);
        end;
      end;
      tmpWCSPoint1:=PointSCSToPointWCS(ARect.TopLeft.X,ARect.TopLeft.Y);
      tmpWCSPoint2:=PointSCSToPointWCS(ARect.BottomRight.X,ARect.BottomRight.Y);
      SelectObjectRect(tmpWCSPoint1,tmpWCSPoint2,AllVertexInRect);
  end;
end;

procedure TAssiDrawControl.SuperLeftButtonClick(Sender: TObject);
var
  tmpWCSPoint1,tmpWCSPoint2:TFloatPoint;
begin
  if (FControlAction=[caClickLeft]) then
  begin
      tmpWCSPoint1:=PointSCSToPointWCS(FCursorPos.X,FCursorPos.Y);
      tmpWCSPoint2:=PointSCSToPointWCS(FCursorPos.X,FCursorPos.Y);
      SelectObjectRect(tmpWCSPoint1,tmpWCSPoint2,false);
  end;
end;

procedure TAssiDrawControl.SuperEditingDone(Sender: TObject);
begin
  if Assigned(FOnEditingDone) then
  FOnEditingDone(Sender);
end;

procedure TAssiDrawControl.gaMoveVertexAction(Sender: TObject);
var
  i,count:integer;
  Item:TEntity;
  NewCord:TFloatPoint;
begin
  if caMoveVertex in FControlAction then
  begin
      Count:=Length(ActiveDocument.FMVertArray);
      for I := 0 to  count - 1 do
      begin
          Item:=ActiveDocument.FMVertArray[i].Item;
          NewCord:=PointSCSToPointWCS(FCursorPos.X,FCursorPos.Y);
          Item.MoveVertex(ActiveDocument.FMVertArray[i].VertexIndex,NewCord);
          ActiveDocument.FMVertArray[i].VertexPos:=NewCord;
      end;
  end;
end;

procedure TAssiDrawControl.SuperMiddleButtonDblClick(Sender: TObject);
begin
  ActiveDocument.ZoomToFit;
end;

procedure TAssiDrawControl.gaMouseAction(Sender: TObject);
begin
    if Assigned(ActiveDocument) then
    begin
    if (FMouseButtonUp=mbMiddle)and(FMouseButtonDown=mbMiddle)and(not FMouseButtonPressed)then //mbMiddle
    begin

        if FControlAction=[caMoveSpace] then
        begin
            FControlAction:=[caNone];
        end
        else if FControlAction=[caNone] then
        begin
            FControlAction:=[caMoveSpace];
            //Procedure(self);
        end;

        if FClickCount=0 then
        begin

        end
        else if FClickCount=2 then
        begin
            if FControlAction=[caNone] then
            begin
              FControlAction:=[caZoomToFit];
              SuperMiddleButtonDblClick(self);
              FControlAction:=[caNone];
            end;
        end;
    end
    else if (FMouseButtonDown=mbMiddle)and(FMouseButtonPressed)then //mbMiddle
    begin
        if FControlAction=[caNone] then
        begin
            FControlAction:=[caMoveSpace];
            //Procedure(self);
        end;
    end
    else if (FMouseButtonUp=mbLeft)and(FMouseButtonDown=mbLeft)and(not FMouseButtonPressed)then //mbLeft
    begin
        if (FControlAction=[caSelectObject])or(FControlAction=[caNone]) then
        begin
            if (FMouseButtonDownPos.X=FMouseButtonUpPos.X)and(FMouseButtonDownPos.Y=FMouseButtonUpPos.Y) then
            begin
              FControlAction:=[caClickLeft];
              SuperLeftButtonClick(self);  //Быстрый клик
              FControlAction:=[caNone];
            end
            else begin
              EndSelecting(self);
              FControlAction:=[caNone];
            end;
        end;
        if caMoveVertex in FControlAction then
        begin
           EndMoveVertex(self);
           FControlAction:=[caNone];
           SuperEditingDone(self);
        end;
    end
    else if (FMouseButtonDown=mbLeft)and(FMouseButtonPressed)then //mbLeft
    begin
          if (FControlAction=[caNone])and not(eemReadOnly = ActiveDocument.EditMode) then //(not ActiveDocument.ReadOnlyMode)
          begin
            if Length(ActiveDocument.FMVertArray)=0 then
              FControlAction:=[caSelectObject]
            else if (not(eemSelectOnly = ActiveDocument.EditMode)) then
              FControlAction:=[caMoveVertex]
            else
              FControlAction:=[caSelectObject];
              // Обработка перемещения в MouseMove
          end;
    end
    else if (FMouseButtonUp=mbRight)and(FMouseButtonDown=mbRight)then //mbRight
    begin
        if FControlAction = [caNone] then
        begin
            FControlAction:=[caClickRight];
            //SuperRightButtonClick(self);
            FControlAction:=[caNone];
        end;
    end
    else if (FMouseButtonDown=mbRight)and(FMouseButtonPressed)then //mbRight
    begin
          if FControlAction=[caNone] then
          begin

          end;
    end;
    end;
end;

procedure TAssiDrawControl.SuperMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    FMouseButtonDownPos.X:=X;
    FMouseButtonDownPos.Y:=Y;
    FMouseButtonDown:=Button;//mbLeft, mbRight, mbMiddle
    FMouseButtonPressed:=true;
    FMouseButtonDownShift:=Shift;
    if Assigned(ActiveDocument) then
    FtmpViewPos:=ActiveDocument.FViewPos;
    gaMouseAction(self);

    if Assigned(FSelfOnMouseDown) then
    FSelfOnMouseDown(Sender,Button,Shift,X,Y);
end;

procedure TAssiDrawControl.SuperMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    FMouseButtonUpPos.X     :=X;
    FMouseButtonUpPos.Y     :=Y;
    FMouseButtonUp          :=Button;//mbLeft, mbRight, mbMiddle
    FMouseButtonPressed     :=false;
    FMouseButtonUpShift     :=Shift;

    if (FMouseButtonUp=FMouseButtonDown)
    and(FMouseButtonUpPos.X=FMouseButtonDownPos.X)
    and(FMouseButtonUpPos.Y=FMouseButtonDownPos.Y) then
    begin
        FClickCount:=FClickCount+1;
    end
    else
        FClickCount:=0;

    gaMouseAction(self);
    SuperControlPaint(self);

    if Assigned(FSelfOnMouseUp) then
    FSelfOnMouseUp(Sender,Button,Shift,X,Y);
end;

procedure TAssiDrawControl.SuperMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  tX,tY:Double;
  WCSMouseButtonDownPos,WCSCursorPos:TFloatPoint;
begin

  FCursorPos.X:=x;  //ScreenToClient(mouse.CursorPos).X
  FCursorPos.Y:=y;

  //вычисление смещения
    //if (FMouseButtonDown=mbMiddle)then //mbLeft, mbRight, mbMiddle
    //begin
        if caMoveSpace in FControlAction{(FClickCount=0)and(FMouseButtonPressed)} then
        begin
            WCSMouseButtonDownPos:=PointSCSToPointWCS(FMouseButtonDownPos.X,FMouseButtonDownPos.Y);
            WCSCursorPos:=PointSCSToPointWCS(FCursorPos.X,FCursorPos.Y);

            tX:=WCSCursorPos.X-WCSMouseButtonDownPos.X;
            tY:=WCSCursorPos.Y-WCSMouseButtonDownPos.Y;

            ActiveDocument.FViewPos.X:=FtmpViewPos.X+tX;
            ActiveDocument.FViewPos.y:=FtmpViewPos.y+tY;
        end
        else if caMoveVertex in FControlAction then
        begin
            gaMoveVertexAction(self);
        end;
    //end;

  if Assigned(FMouseMoveUnderEntityEvent) then
  begin
     //todo: объекты при отрисовке должны проверять не находится ли курсор над ними
     //если находится, то добавлять себя в список объектов MoveUnderList
     //FMouseMoveUnderEntityEvent(Self,);
  end;

  SuperControlPaint(self);

  if Assigned(FSelfOnMouseMove) then
  FSelfOnMouseMove(Sender,Shift,X,Y);
end;

procedure TAssiDrawControl.SuperMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
   tmpViewPos1,tmpViewPos2:TFloatPoint;
   t:TSystemTime;
   K:Integer;
begin
  if Assigned(ActiveDocument) then
  begin
  getLocaltime(t);
  if t.second = FCurSec then
  begin
     inc(FKStep);
  end
  else begin
     FCurSec:=t.second;
     FKStep:=1;
  end;
  K:=ActiveDocument.FViewScaleK;
  tmpViewPos1:=PointSCSToPointWCS(FCursorPos.X,FCursorPos.Y);
  if WheelDelta<0 then
  begin
      if ActiveDocument.FViewScale>0 then
      begin
        if FKStep>5 then
        begin
          ActiveDocument.FViewScale:=ActiveDocument.FViewScale+K+Trunc(FKStep*10);
          ActiveDocument.FViewScaleK:=K+1;
        end
        else begin
          ActiveDocument.FViewScale:=ActiveDocument.FViewScale+K;
          ActiveDocument.FViewScaleK:=K+1;
        end;
        //Определение положения курсора и компенсация сдвига масштаба
        tmpViewPos2:=PointSCSToPointWCS(FCursorPos.X,FCursorPos.Y);
        tmpViewPos2.X:=tmpViewPos2.X-tmpViewPos1.X;
        tmpViewPos2.Y:=tmpViewPos2.Y-tmpViewPos1.Y;
        ActiveDocument.FViewPos.X:=ActiveDocument.FViewPos.X+tmpViewPos2.X;
        ActiveDocument.FViewPos.Y:=ActiveDocument.FViewPos.Y+tmpViewPos2.Y;
      end;
      SuperControlPaint(self);
  end
  else begin
      if (ActiveDocument.FViewScale>5)and(ActiveDocument.FViewScale-ActiveDocument.FViewScaleK>1) then
      begin
        if FKStep>5 then
        begin
          ActiveDocument.FViewScale:=ActiveDocument.FViewScale-K-Trunc(FKStep*10);;
          if ActiveDocument.FViewScale<=1 then ActiveDocument.FViewScale:=2;
          if ActiveDocument.FViewScaleK-1>0 then
          ActiveDocument.FViewScaleK:=K-1;
        end
        else begin
          ActiveDocument.FViewScale:=ActiveDocument.FViewScale-K;
          if ActiveDocument.FViewScaleK-1>0 then
          ActiveDocument.FViewScaleK:=K-1;
        end;
        //Определение положения курсора и компенсация сдвига масштаба
        tmpViewPos2:=PointSCSToPointWCS(FCursorPos.X,FCursorPos.Y);
        tmpViewPos2.X:=tmpViewPos2.X-tmpViewPos1.X;
        tmpViewPos2.Y:=tmpViewPos2.Y-tmpViewPos1.Y;
        ActiveDocument.FViewPos.X:=ActiveDocument.FViewPos.X+tmpViewPos2.X;
        ActiveDocument.FViewPos.Y:=ActiveDocument.FViewPos.Y+tmpViewPos2.Y;
      end;
      SuperControlPaint(self);
  end;

  FClickCount:=0;
  end;

  if Assigned(FSelfOnMouseWheel) then
  FSelfOnMouseWheel(Sender,Shift,WheelDelta,MousePos,Handled);

end;

procedure TAssiDrawControl.SuperMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FSelfOnMouseWheelDown) then
    FSelfOnMouseWheelDown(Sender,Shift,MousePos,Handled);
end;

procedure TAssiDrawControl.SuperMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FSelfOnMouseWheelUp) then
    FSelfOnMouseWheelUp(Sender,Shift,MousePos,Handled);
end;

procedure TAssiDrawControl.SuperPaint(Sender: TObject);
begin
  SuperControlPaint(self);

  if Assigned(FSelfOnPaint) then
    FSelfOnPaint(Sender);
end;

procedure TAssiDrawControl.SuperMessagesPaint(Sender: TObject);
var
  PosX, PosY:integer;
  DevString:string;
begin
   if FMessagesLast='' then exit;
   // Рисуем сообщение
   FVirtualCanvas.Pen.Style:=psSolid;
   FVirtualCanvas.Pen.Color:=FCursorColor;
   FVirtualCanvas.Pen.Mode:=pmNot;
   FVirtualCanvas.Brush.Color := FBackgroundColor;
   FVirtualCanvas.Brush.Style := bsDiagCross;//bsClear
   FVirtualCanvas.Font.Assign(FDefaultFont);
   FVirtualCanvas.Font.Size := 10; // от 10 до 16
   FVirtualCanvas.Font.Color := FCursorColor;

   PosX:=Width div 2;
   PosY:=Height-50;

   FVirtualCanvas.TextOut(PosX,PosY,FMessagesLast);
end;

procedure TAssiDrawControl.SuperFrameViewModePaint(Sender: TObject);
var
  PosX, PosY, w, TextWidth:integer;
  DevString:string;
begin
   if FFrameViewModeText='' then exit;

   FVirtualCanvas.Pen.Style     :=psSolid;
   FVirtualCanvas.Pen.Color     :=FFrameViewModeColor;
   FVirtualCanvas.Pen.Mode      :=pmCopy;
   w                            :=FVirtualCanvas.Pen.Width;
   FVirtualCanvas.Pen.Width     :=10;

   FVirtualCanvas.Brush.Color   :=FFrameViewModeColor;
   FVirtualCanvas.Brush.Style   :=bsSolid;

   FVirtualCanvas.Font.Assign(FDefaultFont);
   FVirtualCanvas.Font.Size     :=10; // от 10 до 16
   FVirtualCanvas.Font.Bold     :=True;
   FVirtualCanvas.Font.Color    :=FBackgroundColor;

   FVirtualCanvas.Frame(0,0,Width,Height);
   PosX:=10;
   PosY:=5;
   TextWidth:=FVirtualCanvas.GetTextWidth(FFrameViewModeText)+25;
   FVirtualCanvas.FillRect(0,0,TextWidth,25);
   FVirtualCanvas.TextOut(PosX,PosY,FFrameViewModeText);
   FVirtualCanvas.Pen.Width     :=w;
end;

//World Coordinate System (WCS), Screen Coordinate System (SCS)
function TAssiDrawControl.PointSCSToPointWCS(X,Y:Integer):TFloatPoint;
var
  r:TFloatPoint;
  k1,k2,X2,Y2:Double;
begin
   if Assigned(ActiveDocument) then
   begin
   //try
     k2:=ActiveDocument.FViewScale;
     k2:=k2/100;
     k2:=SimpleRoundTo(k2,-2);
     //получаем коэффициент точности
     k1:=Math.Power(10,Activedocument.PointPrecision);
     //vbmHeight,vbpWidth:Integer;
     X2:=(X/k1)*k2;
     Y2:=((vbmHeight-Y)/k1)*k2;

     r.Z:=0;
     r.X:=X2-(0{mmPoint.Xmin*-1})-ActiveDocument.FViewPos.X;
     r.Y:=Y2-(0{mmPoint.Ymin*-1})-ActiveDocument.FViewPos.Y;
     result:=r;
   //except

   //end;
   end;
end;

//World Coordinate System (WCS), Screen Coordinate System (SCS)
function TAssiDrawControl.PointWCSToPointSCS(X,Y:Double):TPoint;
var
  r:TPoint;
  X2,Y2,k1,k2:Double;
begin
   r.X:=0;
   r.Y:=0;
   if Assigned(ActiveDocument) then
   begin
   //try
     //коэффициент масштабирования вида
     k2:=ActiveDocument.FViewScale;
     k2:=k2/100;
     k2:=SimpleRoundTo(k2,-2);
     //получаем коэффициент точности
     k1:=Math.Power(10,Activedocument.PointPrecision);
     //vbmHeight,vbpWidth:Integer;
     X2:=X+(0{mmPoint.Xmin*-1})+ActiveDocument.FViewPos.X;
     Y2:=Y+(0{mmPoint.Ymin*-1})+ActiveDocument.FViewPos.Y;
     r.X:=Trunc((X2/k2)*k1);
     r.Y:=vbmHeight-Trunc((Y2/k2)*k1);
   //except

   //end;
   end;
   Result:=r;
end;

procedure TAssiDrawControl.RefreshEntityDraw;
begin
  Repaint;
  if Assigned(ActiveDocument) then
  begin
       if not(eemReadOnly = ActiveDocument.EditMode) then
       RepaintVertex;
  end;
end;

procedure TAssiDrawControl.TimerMessageOnTimer(Sender: TObject);
begin
  if FMessagesList.Count>0 then
  begin
     if FMessagesList.Count=1 then
     begin
        FTimerMessage.Interval:=1500;
     end
     else begin
        FTimerMessage.Interval:=1000;
     end;
     FMessagesLast:=FMessagesList.Strings[0];
     FMessagesList.Delete(0);
  end
  else begin
     FMessagesLast:='';
     FTimerMessage.Enabled:=False;
  end;
  Refresh;
end;

procedure TAssiDrawControl.Repaint;
var
  i,
  index      :integer;
  Item       :TEntity;
  Doc        :TAssiDrawDocument;
  DrawObject :Boolean;
begin
  Doc:=ActiveDocument;
  if Assigned(Doc) then
  begin
  for I := 0 to Doc.ModelSpace.Objects.Count - 1 do
  begin
          DrawObject:=true;
          Item:=Doc.ModelSpace.Objects.Items[i];

          if Assigned(FOnEntityBeforeDrawEvent) then
             FOnEntityBeforeDrawEvent(Self, Item, DrawObject);

          if DrawObject then
          begin
            if Assigned(Doc.FSelectList) then
            begin
              index:=Doc.FSelectList.IndexOf(Item);
              if index>-1 then
               Item.Repaint(FLogicalDraw,[edsSelected])
              else
                Item.Repaint(FLogicalDraw,[edsNormal]);
            end
            else
              Item.Repaint(FLogicalDraw,[edsNormal]);

            if Assigned(FOnEntityAfterDrawEvent) then
             FOnEntityAfterDrawEvent(Self, Item);
          end;
  end;
  end;
end;

procedure TAssiDrawControl.RepaintVertex;
var
  i,count:integer;
  Item:TEntity;
  Doc:TAssiDrawDocument;
begin
  Doc:=ActiveDocument;
  for I := 0 to Doc.ModelSpace.Objects.Count - 1 do
  begin
          if Assigned(Doc.FSelectList) then
          begin
            Item:=Doc.ModelSpace.Objects.Items[i];
            if Doc.FSelectList.IndexOf(Item)>-1 then
              Item.RepaintVertex(FLogicalDraw);
          end;
  end;
  
  Count:=Length(Doc.FMVertArray);
  if not(eemSelectOnly = ActiveDocument.EditMode) then
  for I := 0 to Count - 1 do
  begin
       VertexDraw(Doc.FMVertArray[i].VertexPos.X,Doc.FMVertArray[i].VertexPos.Y,VERTEXMARKER_VERTEX_SEL);
  end;
end;

//World Coordinate System (WCS), Screen Coordinate System (SCS)
function TAssiDrawControl.ValWCSToValSCS(X:Double):Integer;
var
  k1,k2:Double;
begin
   //коэффициент масштабирования вида
   k2:=Activedocument.FViewScale/100;
   //получаем коэффициент точности
   k1:=Math.Power(10,Activedocument.PointPrecision);
   result:=Trunc((X/k2)*k1);
end;

function TAssiDrawControl.ValLineWeightToValPixel(X:TgaLineWeight):Integer;
begin

  case x of
  gaLnWt000:
  Result:=1;
  gaLnWt005:
  Result:=ValWCSToValSCS(0.05);
  gaLnWt009:
  Result:=ValWCSToValSCS(0.09);
  gaLnWt013:
  Result:=ValWCSToValSCS(0.13);
  gaLnWt015:
  Result:=ValWCSToValSCS(0.15);
  gaLnWt018:
  Result:=ValWCSToValSCS(0.18);
  gaLnWt020:
  Result:=ValWCSToValSCS(0.20);
  gaLnWt025:
  Result:=ValWCSToValSCS(0.25);
  gaLnWt030:
  Result:=ValWCSToValSCS(0.30);
  gaLnWt035:
  Result:=ValWCSToValSCS(0.35);
  gaLnWt040:
  Result:=ValWCSToValSCS(0.40);
  gaLnWt050:
  Result:=ValWCSToValSCS(0.50);
  gaLnWt053:
  Result:=ValWCSToValSCS(0.53);
  gaLnWt060:
  Result:=ValWCSToValSCS(0.60);
  gaLnWt070:
  Result:=ValWCSToValSCS(0.70);
  gaLnWt080:
  Result:=ValWCSToValSCS(0.80);
  gaLnWt090:
  Result:=ValWCSToValSCS(0.90);
  gaLnWt100:
  Result:=ValWCSToValSCS(1.00);
  gaLnWt106:
  Result:=ValWCSToValSCS(1.06);
  gaLnWt120:
  Result:=ValWCSToValSCS(1.20);
  gaLnWt140:
  Result:=ValWCSToValSCS(1.40);
  gaLnWt158:
  Result:=ValWCSToValSCS(1.58);
  gaLnWt200:
  Result:=ValWCSToValSCS(2.00);
  gaLnWt211:
  Result:=ValWCSToValSCS(2.11);
  gaLnWtByLayer:
  Result:=1;
  gaLnWtByBlock:
  Result:=1;
  gaLnWtByLwDefault:
  Result:=1;
  else begin
     Result:=1;
  end;
  end;

end;

function TAssiDrawControl.ValgaColorToValColor(X:TgaColor):TColor;
begin

  case x of
    gaByBlock     : Result:=clBlack;
    gaByLayer     : Result:=clBlack;
    gaRed         : Result:=clRed;
    gaYellow      : Result:=clYellow;
    gaGreen       : Result:=clGreen;
    gaCyan        : Result:=clTeal;
    gaBlue        : Result:=clBlue;
    gaMagenta     : Result:=clPurple;
    gaWhite       : Result:=clWhite;
    //0  : Result:=RGB(0,0,0);
    //1  : Result:=RGB(255,0,0);
    //2  : Result:=RGB(255,255,0);
    //3  : Result:=RGB(0,255,0);
    //4  : Result:=RGB(0,255,255);
    //5  : Result:=RGB(0,0,255);
    //6  : Result:=RGB(255,0,255);
    //7  : Result:=RGB(255,255,255);
    8  : Result:=RGB(128,128,128);
    9  : Result:=RGB(192,192,192);
    10  : Result:=RGB(255,0,0);
    11  : Result:=RGB(255,127,127);
    12  : Result:=RGB(204,0,0);
    13  : Result:=RGB(204,102,102);
    14  : Result:=RGB(153,0,0);
    15  : Result:=RGB(153,76,76);
    16  : Result:=RGB(127,0,0);
    17  : Result:=RGB(127,63,63);
    18  : Result:=RGB(76,0,0);
    19  : Result:=RGB(76,38,38);
    20  : Result:=RGB(255,63,0);
    21  : Result:=RGB(255,159,127);
    22  : Result:=RGB(204,51,0);
    23  : Result:=RGB(204,127,102);
    24  : Result:=RGB(153,38,0);
    25  : Result:=RGB(153,95,76);
    26  : Result:=RGB(127,31,0);
    27  : Result:=RGB(127,79,63);
    28  : Result:=RGB(76,19,0);
    29  : Result:=RGB(76,47,38);
    30  : Result:=RGB(255,127,0);
    31  : Result:=RGB(255,191,127);
    32  : Result:=RGB(204,102,0);
    33  : Result:=RGB(204,153,102);
    34  : Result:=RGB(153,76,0);
    35  : Result:=RGB(153,114,76);
    36  : Result:=RGB(127,63,0);
    37  : Result:=RGB(127,95,63);
    38  : Result:=RGB(76,38,0);
    39  : Result:=RGB(76,57,38);
    40  : Result:=RGB(255,191,0);
    41  : Result:=RGB(255,223,127);
    42  : Result:=RGB(204,153,0);
    43  : Result:=RGB(204,178,102);
    44  : Result:=RGB(153,114,0);
    45  : Result:=RGB(153,133,176);
    46  : Result:=RGB(127,95,0);
    47  : Result:=RGB(127,111,63);
    48  : Result:=RGB(76,57,0);
    49  : Result:=RGB(76,66,38);
    50  : Result:=RGB(255,255,0);
    51  : Result:=RGB(255,255,127);
    52  : Result:=RGB(204,204,0);
    53  : Result:=RGB(204,204,102);
    54  : Result:=RGB(153,153,0);
    55  : Result:=RGB(153,153,76);
    56  : Result:=RGB(127,127,0);
    57  : Result:=RGB(127,127,63);
    58  : Result:=RGB(76,76,0);
    59  : Result:=RGB(76,76,38);
    60  : Result:=RGB(191,255,0);
    61  : Result:=RGB(223,255,127);
    62  : Result:=RGB(153,204,0);
    63  : Result:=RGB(178,204,102);
    64  : Result:=RGB(114,153,0);
    65  : Result:=RGB(133,153,76);
    66  : Result:=RGB(95,127,0);
    67  : Result:=RGB(111,127,63);
    68  : Result:=RGB(57,76,0);
    69  : Result:=RGB(66,76,38);
    70  : Result:=RGB(127,255,0);
    71  : Result:=RGB(191,255,127);
    72  : Result:=RGB(102,204,0);
    73  : Result:=RGB(153,204,102);
    74  : Result:=RGB(76,153,0);
    75  : Result:=RGB(114,153,76);
    76  : Result:=RGB(63,127,0);
    77  : Result:=RGB(95,127,63);
    78  : Result:=RGB(38,76,0);
    79  : Result:=RGB(57,76,38);
    80  : Result:=RGB(63,255,0);
    81  : Result:=RGB(159,255,127);
    82  : Result:=RGB(51,204,0);
    83  : Result:=RGB(127,204,102);
    84  : Result:=RGB(38,153,0);
    85  : Result:=RGB(95,153,76);
    86  : Result:=RGB(31,127,0);
    87  : Result:=RGB(79,127,63);
    88  : Result:=RGB(19,76,0);
    89  : Result:=RGB(47,76,38);
    90  : Result:=RGB(0,255,0);
    91  : Result:=RGB(127,255,127);
    92  : Result:=RGB(0,204,0);
    93  : Result:=RGB(102,204,102);
    94  : Result:=RGB(0,153,0);
    95  : Result:=RGB(76,153,76);
    96  : Result:=RGB(0,127,0);
    97  : Result:=RGB(63,127,63);
    98  : Result:=RGB(0,76,0);
    99  : Result:=RGB(38,76,38);
    //100  : Result:=RGB(0,255,63);
    101  : Result:=RGB(127,255,159);
    102  : Result:=RGB(0,204,51);
    103  : Result:=RGB(102,204,127);
    104  : Result:=RGB(0,153,38);
    105  : Result:=RGB(76,153,95);
    106  : Result:=RGB(0,127,31);
    107  : Result:=RGB(63,127,79);
    108  : Result:=RGB(0,76,19);
    109  : Result:=RGB(38,76,47);
    110  : Result:=RGB(0,255,127);
    111  : Result:=RGB(127,255,191);
    112  : Result:=RGB(0,204,102);
    113  : Result:=RGB(102,204,153);
    114  : Result:=RGB(0,153,76);
    115  : Result:=RGB(76,153,114);
    116  : Result:=RGB(0,127,63);
    117  : Result:=RGB(63,127,95);
    118  : Result:=RGB(0,76,38);
    119  : Result:=RGB(38,76,57);
    120  : Result:=RGB(0,255,191);
    121  : Result:=RGB(127,255,223);
    122  : Result:=RGB(0,204,153);
    123  : Result:=RGB(102,204,178);
    124  : Result:=RGB(0,153,114);
    125  : Result:=RGB(76,153,133);
    126  : Result:=RGB(0,127,95);
    127  : Result:=RGB(63,127,111);
    128  : Result:=RGB(0,76,57);
    129  : Result:=RGB(38,76,66);
    130  : Result:=RGB(0,255,255);
    131  : Result:=RGB(127,255,255);
    132  : Result:=RGB(0,204,204);
    133  : Result:=RGB(102,204,204);
    134  : Result:=RGB(0,153,153);
    135  : Result:=RGB(76,153,153);
    136  : Result:=RGB(0,127,127);
    137  : Result:=RGB(63,127,127);
    138  : Result:=RGB(0,76,76);
    139  : Result:=RGB(38,76,76);
    140  : Result:=RGB(0,191,255);
    141  : Result:=RGB(127,223,255);
    142  : Result:=RGB(0,153,204);
    143  : Result:=RGB(102,178,204);
    144  : Result:=RGB(0,114,153);
    145  : Result:=RGB(76,133,153);
    146  : Result:=RGB(0,95,127);
    147  : Result:=RGB(63,111,127);
    148  : Result:=RGB(0,57,76);
    149  : Result:=RGB(38,66,76);
    150  : Result:=RGB(0,127,255);
    151  : Result:=RGB(127,191,255);
    152  : Result:=RGB(0,102,204);
    153  : Result:=RGB(102,153,204);
    154  : Result:=RGB(0,76,153);
    155  : Result:=RGB(76,114,153);
    156  : Result:=RGB(0,63,127);
    157  : Result:=RGB(63,95,127);
    158  : Result:=RGB(0,38,76);
    159  : Result:=RGB(38,57,76);
    160  : Result:=RGB(0,63,255);
    161  : Result:=RGB(127,159,255);
    162  : Result:=RGB(0,51,204);
    163  : Result:=RGB(102,127,204);
    164  : Result:=RGB(0,38,153);
    165  : Result:=RGB(76,95,153);
    166  : Result:=RGB(0,31,127);
    167  : Result:=RGB(63,79,127);
    168  : Result:=RGB(0,19,76);
    169  : Result:=RGB(38,47,76);
    170  : Result:=RGB(0,0,255);
    171  : Result:=RGB(127,127,255);
    172  : Result:=RGB(0,0,204);
    173  : Result:=RGB(102,102,204);
    174  : Result:=RGB(0,0,153);
    175  : Result:=RGB(76,76,153);
    176  : Result:=RGB(0,0,127);
    177  : Result:=RGB(63,63,127);
    178  : Result:=RGB(0,0,76);
    179  : Result:=RGB(38,38,76);
    180  : Result:=RGB(63,0,255);
    181  : Result:=RGB(159,127,255);
    182  : Result:=RGB(51,0,204);
    183  : Result:=RGB(127,102,204);
    184  : Result:=RGB(38,0,153);
    185  : Result:=RGB(95,76,153);
    186  : Result:=RGB(31,0,127);
    187  : Result:=RGB(79,63,127);
    188  : Result:=RGB(19,0,76);
    189  : Result:=RGB(47,38,76);
    190  : Result:=RGB(127,0,255);
    191  : Result:=RGB(191,127,255);
    192  : Result:=RGB(102,0,204);
    193  : Result:=RGB(153,102,204);
    194  : Result:=RGB(76,0,153);
    195  : Result:=RGB(114,76,153);
    196  : Result:=RGB(63,0,127);
    197  : Result:=RGB(95,63,127);
    198  : Result:=RGB(38,0,76);
    199  : Result:=RGB(57,38,76);
    200  : Result:=RGB(191,0,255);
    201  : Result:=RGB(223,127,255);
    202  : Result:=RGB(153,0,204);
    203  : Result:=RGB(178,102,204);
    204  : Result:=RGB(114,0,153);
    205  : Result:=RGB(133,76,153);
    206  : Result:=RGB(95,0,127);
    207  : Result:=RGB(111,63,127);
    208  : Result:=RGB(57,0,76);
    209  : Result:=RGB(66,38,76);
    210  : Result:=RGB(255,0,255);
    211  : Result:=RGB(255,127,255);
    212  : Result:=RGB(204,0,204);
    213  : Result:=RGB(204,102,204);
    214  : Result:=RGB(153,0,153);
    215  : Result:=RGB(153,76,153);
    216  : Result:=RGB(127,0,127);
    217  : Result:=RGB(127,63,79);
    218  : Result:=RGB(76,0,76);
    219  : Result:=RGB(76,38,76);
    220  : Result:=RGB(255,0,191);
    221  : Result:=RGB(255,127,223);
    222  : Result:=RGB(204,0,153);
    223  : Result:=RGB(204,102,178);
    224  : Result:=RGB(153,0,114);
    225  : Result:=RGB(153,76,133);
    226  : Result:=RGB(127,0,95);
    227  : Result:=RGB(127,63,111);
    228  : Result:=RGB(76,0,57);
    229  : Result:=RGB(76,38,66);
    230  : Result:=RGB(255,0,127);
    231  : Result:=RGB(255,127,191);
    232  : Result:=RGB(204,0,102);
    233  : Result:=RGB(204,102,153);
    234  : Result:=RGB(153,0,76);
    235  : Result:=RGB(153,76,114);
    236  : Result:=RGB(127,0,63);
    237  : Result:=RGB(127,63,95);
    238  : Result:=RGB(76,0,38);
    239  : Result:=RGB(76,38,57);
    240  : Result:=RGB(255,0,63);
    241  : Result:=RGB(255,127,159);
    242  : Result:=RGB(204,0,51);
    243  : Result:=RGB(204,102,127);
    244  : Result:=RGB(153,0,38);
    245  : Result:=RGB(153,76,95);
    246  : Result:=RGB(127,0,31);
    247  : Result:=RGB(127,63,79);
    248  : Result:=RGB(76,0,19);
    249  : Result:=RGB(76,38,47);
    250  : Result:=RGB(51,51,51);
    251  : Result:=RGB(91,91,91);
    252  : Result:=RGB(132,132,132);
    253  : Result:=RGB(173,173,173);
    254  : Result:=RGB(214,214,214);
    255  : Result:=RGB(255,255,255);
  else begin
     Result       :=clBlack;
  end;

  end;

end;

procedure TAssiDrawControl.SetMessageToUser(AText: String);
begin
  FMessagesList.Clear;
  FMessagesLast:=AText;
  FTimerMessage.Interval:=1000;
  FTimerMessage.Enabled:=True;
  Refresh;
end;

procedure TAssiDrawControl.FrameViewModeSet(AText: String; AColor: TColor);
begin
  FFrameViewModeText                 :=AText;
  FFrameViewModeColor                :=AColor;
end;

procedure TAssiDrawControl.FrameViewModeClear;
begin
  FFrameViewModeText                 :='';
  FFrameViewModeColor                :=clGreen;
end;

procedure TAssiDrawControl.AddMessageToUser(AText: String);
begin
FMessagesList.Add(AText);
FMessagesLast:=AText;
if FTimerMessage.Enabled=False then
begin
  FTimerMessage.Interval:=100;
  FTimerMessage.Enabled:=True;
end;
Refresh;
end;

procedure TAssiDrawControl.SuperVirtualPaint(Sender: TObject);
begin
  vbmHeight:=Height;
  vbpWidth:=Width;

  FVirtualBitMap.SetSize(vbpWidth,vbmHeight);
  if Assigned(FOnBeforeDrawEvent) then
     FOnBeforeDrawEvent(Self);
  //Рисуем фон
  FVirtualCanvas.Pen.Color    := FBackgroundColor;
  FVirtualCanvas.Brush.Style  := bsSolid;
  if Assigned(ActiveDocument) then
  FVirtualCanvas.Brush.Color  := FBackgroundColor
  else
  FVirtualCanvas.Brush.Color  := clSilver;
  FVirtualCanvas.FillRect(rect(0,0,vbpWidth,vbmHeight));
  //Рисуем объекты
  RefreshEntityDraw;
  if Assigned(FOnAfterDrawEvent) then
     FOnAfterDrawEvent(Self);
end;

procedure TAssiDrawControl.SuperWndProc(var Message: TMessage);
var
  Pos: TPoint;
  KeyState: TKeyboardState;
  WheelMsg: TCMMouseWheel;
  Handler: Boolean;
begin
    case Message.Msg of
      WM_MOUSEWHEEL:
      begin
            GetKeyboardState(KeyState);
            WheelMsg.Msg := TWMMouseWheel(Message).Msg;
            {$IFNDEF FPC}
            WheelMsg.ShiftState := KeyboardStateToShiftState(KeyState);
            WheelMsg.WheelDelta := TWMMouseWheel(Message).WheelDelta;
            {$ELSE}
            WheelMsg.ShiftState := GetKeyShiftState;
            WheelMsg.WheelData := TWMMouseWheel(Message).WheelDelta;
            {$ENDIF}
            WheelMsg.Pos.x := TWMMouseWheel(Message).Pos.x;
            WheelMsg.Pos.y := TWMMouseWheel(Message).Pos.y;
            pos.X:=mouse.CursorPos.x;
            pos.Y:=mouse.CursorPos.Y;
            {$IFNDEF FPC}
            SuperMouseWheel(Self,WheelMsg.ShiftState,WheelMsg.WheelDelta,pos,Handler);
            {$ELSE}
            SuperMouseWheel(Self,WheelMsg.ShiftState,WheelMsg.WheelData,pos,Handler);
            {$ENDIF}

      end;
      WM_MBUTTONDBLCLK:
      begin

      end;
    end;

  //inherited WindowProc(Message);
  //Dispatch(Message);
  if Assigned(FFormWindowProc) then
  FFormWindowProc(Message);
end;

procedure TAssiDrawControl.SuperControlPaint(Sender: TObject);
var
  x1,x,x0,a,y1,y,y0,ax,ay:double;
  i:integer;
begin
  FVirtualCanvas.Font.Assign(FDefaultFont);
  FVirtualCanvas.Pen.Mode:=pmCopy;
  FVirtualCanvas.Pen.Width:=1;
  FLogicalDraw.Develop:=FDevelop;

  SuperVirtualPaint(self); //Отрисовка Объектов чертежа

  ZeroPointCSPaint;        // Отрисовка нулевой точки
  SelectRectDoPaint(self);//Отрисовка рамки выбора
  SuperCursorPaint(self); //Отрисовка курсора
  SuperExtHintPaint(self);
  SuperMessagesPaint(Sender);
  SuperFrameViewModePaint(Sender);

  Canvas.Draw(0,0,FVirtualBitMap); //Вывод на канву

end;

procedure TAssiDrawControl.SelectRectDoPaint(Sender: TObject);
begin
  if (FControlAction=[caSelectObject])and not(eemReadOnly = ActiveDocument.EditMode) then
  begin
      SelectRectPaint(FMouseButtonDownPos.X, FMouseButtonDownPos.Y, FCursorPos.X, FCursorPos.Y);
  end;
end;

procedure TAssiDrawControl.SuperCursorPaint(Sender: TObject);
var
  rsize,lsize:integer;
  xpoint:TFloatPoint;
  DevString:string;
begin

   FVirtualCanvas.Font.Assign(FDefaultFont);
   FVirtualCanvas.Brush.Color :=FCursorColor;
   FVirtualCanvas.Pen.Style   :=psSolid;
   FVirtualCanvas.Pen.Color   :=FCursorColor;
   FVirtualCanvas.Pen.Mode    :=pmNot;

   if FDrawCursor then
   begin
     // Рисуем курсор
     Cursor:=crNone;
     rsize:=5;
     lsize:=30;
     FVirtualCanvas.Pen.Width:=1;
     FVirtualCanvas.MoveTo(FCursorPos.X-lsize,FCursorPos.Y);
     FVirtualCanvas.LineTo (FCursorPos.X+lsize,FCursorPos.Y);
     FVirtualCanvas.MoveTo(FCursorPos.X,FCursorPos.Y-lsize);
     FVirtualCanvas.LineTo (FCursorPos.X,FCursorPos.Y+lsize);

     FVirtualCanvas.MoveTo(FCursorPos.X-rsize,FCursorPos.Y-rsize);
     FVirtualCanvas.LineTo (FCursorPos.X+rsize,FCursorPos.Y-rsize);
     FVirtualCanvas.LineTo (FCursorPos.X+rsize,FCursorPos.Y+rsize);
     FVirtualCanvas.LineTo (FCursorPos.X-rsize,FCursorPos.Y+rsize);
     FVirtualCanvas.LineTo (FCursorPos.X-rsize,FCursorPos.Y-rsize);
   end;

   //Рисуем динамический ввод
   FVirtualCanvas.Brush.Color := FCursorColor;
   FVirtualCanvas.Brush.Style := bsSolid;
   FVirtualCanvas.Font.Assign(FDefaultFont);
   FVirtualCanvas.Font.Size := 10; // от 10 до 16
   FVirtualCanvas.Font.Color := FBackgroundColor;
   xpoint:=PointSCStoPointWCS(FCursorPos.X,FCursorPos.Y);

   if FDevelop then
   begin

      DevString:='  Pos X:'+FloatToStr(xpoint.X)+' Y:'+floattostr(xpoint.Y);
      if Assigned(ActiveDocument) then
      begin
      DevString:=DevString+'  Scale:'+floattostr(ActiveDocument.FViewScale);
      DevString:=DevString+'  ScaleK:'+inttostr(ActiveDocument.FViewScaleK);
      DevString:=DevString+'  ViewPos+ X:'+floattostr(ActiveDocument.FViewPos.X)+' Y:'+floattostr(ActiveDocument.FViewPos.Y);
      end;
      DevString:=DevString+'  FControlAction:';

         if FControlAction=[caNone] then
         begin
         DevString:=DevString+'caNone';
         end;
         if FControlAction=[caZoomToFit] then
         begin
         DevString:=DevString+'caZoomToFit';
         end;
         if FControlAction=[caMoveSpace] then
         begin
         DevString:=DevString+'caMoveSpace';
         end;
         if FControlAction=[caSelectObject]then
         begin
         DevString:=DevString+'caSelectObject';
         end;
         if FControlAction=[caClickLeft]  then
         begin
         DevString:=DevString+'caClickLeft';
         end;
         if FControlAction=[caClickRight] then
         begin
         DevString:=DevString+'caClickRight';
         end;

         if Assigned(ActiveDocument) then
         begin
         DevString:=DevString+'  EntityCount:'+inttostr(ActiveDocument.ModelSpace.Objects.Count);
         DevString:=DevString+'  SelectList.Count:'+inttostr(ActiveDocument.SelectList.Count);
         DevString:=DevString+'  SelectedEntity.Count:'+inttostr(ActiveDocument.ModelSpace.SelectedEntityList.Count);
         end;
         FVirtualCanvas.TextOut(0,0,DevString);
   end;
end;

procedure TAssiDrawControl.SuperExtHintPaint(Sender: TObject);
const
  HINT_PADDING_TOP = 16;
  HINT_PADDING_LEFT = 16;
var
  CanvasWidth, CanvasHeight:integer;
begin
  if Assigned(FOnExtHintBeforeDrawEvent) then
  begin
       CanvasHeight:=FHintDrawBitMap.Height;
       CanvasWidth :=FHintDrawBitMap.Width;
       FOnExtHintBeforeDrawEvent(Self, CanvasWidth, CanvasHeight,
                               FHintDrawBitMap.Canvas);
       if (CanvasWidth>0)and(CanvasHeight>0) then
       begin
       FHintDrawBitMap.SetSize(CanvasWidth, CanvasHeight);
       FVirtualCanvas.Draw(FCursorPos.X+HINT_PADDING_LEFT,
                               FCursorPos.Y+HINT_PADDING_TOP, FHintDrawBitMap);
       end;
  end;
end;

{LogicalCanvasDrawing}

procedure TAssiDrawControl.TextOutTransperent(X, Y: Integer; AText: String);
var
  Pic:TBitmap;
  OldBkMode:integer;
  PicRect,TarRect:trect;
begin
      Pic := TBitmap.Create;
      Pic.Canvas.Font := FVirtualCanvas.Font;
      Pic.Canvas.Font.Color := clgreen;
      Pic.Canvas.Pen := FVirtualCanvas.Pen;
      Pic.Width := FVirtualCanvas.TextWidth(AText);
      Pic.Height := FVirtualCanvas.TextWidth(AText)+3;
      //PicRect := Rect(0, 0, Pic.Width, Pic.Height);
      //TarRect := Rect(X, Y, X + Pic.Width, Y + Pic.Height);
      //pic.Canvas.CopyRect(PicRect, FVirtualCanvas, TarRect);
      //pic.Canvas.FillRect(PicRect);
      //Pic.Transparent:=true;
      //Pic.TransparentColor:=Pic.Canvas.Pixels[1,1];
      //Pic.TransparentMode:=tmFixed;
      //SetBkMode(Pic.Canvas.Handle, Transparent);
      //Pic.Canvas.TextOut(0, 0, Text);
      //Pic.TransparentColor:=Pic.Canvas.Pixels[1,1];

      Pic.Canvas.Brush.Color := clRed;
      PicRect := Rect(0, 0, Pic.Width, Pic.Height);
      TarRect := Rect(X, Y, X + Pic.Width, Y + Pic.Height);
      pic.Canvas.CopyRect(PicRect, FVirtualCanvas, TarRect);
      //pic.Canvas.FillRect(PicRect);
      Pic.Canvas.Brush.Color := clnone;
      Pic.Canvas.Brush.Style := bsclear;
      Pic.Canvas.TextOut(0, 0, AText);
      OldBkMode := SetBkMode(Pic.Handle, TRANSPARENT);
      Pic.Canvas.TextOut(0, 0, AText);
      SetBkMode(Pic.Handle, OldBkMode);

      FVirtualCanvas.Draw(X, Y,Pic);
      Pic.Free;
end;

procedure TAssiDrawControl.VertexDraw(X, Y: Double; ATypeVertex: Integer);
var
   PointSCS1,PointSCS2:TPoint;
begin

  if ATypeVertex=-1 then //all
  begin
      VertexPaint(X,Y);
  end
  else if ATypeVertex=VERTEXMARKER_BASEPOINT then //base
  begin
      PointSCS1:=PointWCSToPointSCS(X,Y);
      PointSCS1.X:=PointSCS1.X-FDeltaCord;
      PointSCS1.Y:=PointSCS1.Y-FDeltaCord;
      PointSCS2:=PointWCSToPointSCS(X,Y);
      PointSCS2.X:=PointSCS2.X+FDeltaCord;
      PointSCS2.Y:=PointSCS2.Y+FDeltaCord;

      FVirtualCanvas.Pen.Color:=clSilver;
      FVirtualCanvas.Pen.Mode:=pmNot;
      FVirtualCanvas.Pen.Width:=2;
      FVirtualCanvas.Brush.Color := FVertexBasePointColor;
      FVirtualCanvas.Brush.Style  := bsSolid;
      FVirtualCanvas.FillRect(rect(PointSCS1.x,PointSCS1.y,PointSCS2.x,PointSCS2.y));
  end
  else if ATypeVertex=VERTEXMARKER_VERTEX then  //vertex
  begin
      VertexPaint(X,Y);
  end
  else if ATypeVertex=VERTEXMARKER_CENTER then //center
  begin
      PointSCS1:=PointWCSToPointSCS(X,Y);
      PointSCS1.X:=PointSCS1.X-FDeltaCord;
      PointSCS1.Y:=PointSCS1.Y-FDeltaCord;
      PointSCS2:=PointWCSToPointSCS(X,Y);
      PointSCS2.X:=PointSCS2.X+FDeltaCord;
      PointSCS2.Y:=PointSCS2.Y+FDeltaCord;

      FVirtualCanvas.Brush.Color := FVertexCustomColor;
      FVirtualCanvas.Brush.Style := bsClear;
      FVirtualCanvas.Pen.Color:=FVertexCustomColor;
      FVirtualCanvas.Pen.Mode:=pmCopy;
      FVirtualCanvas.Pen.Width:=2;
      FVirtualCanvas.Ellipse(PointSCS1.X,PointSCS1.Y,PointSCS2.X,PointSCS2.Y);
  end
  else if ATypeVertex=VERTEXMARKER_VERTEX_SEL then //selected
  begin
      PointSCS1:=PointWCSToPointSCS(X,Y);
      PointSCS1.X:=PointSCS1.X-FDeltaCord;
      PointSCS1.Y:=PointSCS1.Y-FDeltaCord;
      PointSCS2:=PointWCSToPointSCS(X,Y);
      PointSCS2.X:=PointSCS2.X+FDeltaCord;
      PointSCS2.Y:=PointSCS2.Y+FDeltaCord;

      FVirtualCanvas.Brush.Color := FVertexSelectColor;
      FVirtualCanvas.Pen.Color:=clSilver;
      FVirtualCanvas.Pen.Mode:=pmNot;
      FVirtualCanvas.Pen.Width:=2;
      FVirtualCanvas.Brush.Style  := bsSolid;
      FVirtualCanvas.FillRect(rect(PointSCS1.x,PointSCS1.y,PointSCS2.x,PointSCS2.y));
  end;
end;

procedure TAssiDrawControl.VertexPaint(X, Y: Double);
var
  PointSCS:TPoint;
begin
    PointSCS:=PointWCSToPointSCS(X,Y);
    VertexPaint(PointSCS.X,PointSCS.Y);
end;

procedure TAssiDrawControl.VertexPaint(X, Y: Integer);
begin

   FVirtualCanvas.Font.Assign(FDefaultFont);
   FVirtualCanvas.Brush.Style := bsclear;
   FVirtualCanvas.Brush.Color := FVertexCustomColor;
   FVirtualCanvas.Pen.Color:=FVertexCustomColor;
   FVirtualCanvas.Pen.Mode:=pmCopy;
   FVirtualCanvas.Pen.Width:=1;


   FVirtualCanvas.MoveTo(X-FDeltaCord,Y-FDeltaCord);
   FVirtualCanvas.LineTo (X+FDeltaCord,Y-FDeltaCord);
   FVirtualCanvas.LineTo (X+FDeltaCord,Y+FDeltaCord);
   FVirtualCanvas.LineTo (X-FDeltaCord,Y+FDeltaCord);
   FVirtualCanvas.LineTo (X-FDeltaCord,Y-FDeltaCord);
end;

procedure TAssiDrawControl.SetStyleDraw(LineType:String; LineWidth:TgaLineWeight; AColor:TgaColor);
begin    
   FVirtualCanvas.Brush.Color := clNone;
   FVirtualCanvas.Brush.Style := bsclear;

   FVirtualCanvas.Pen.Mode  := pmCopy;
   FVirtualCanvas.Pen.Color := ValgaColorToValColor(AColor);
   FVirtualCanvas.Pen.Width := ValLineWeightToValPixel(LineWidth);
   if LineType<>LINETYPE_SELECTED then
        FVirtualCanvas.Pen.Style:=psSolid
   else
        FVirtualCanvas.Pen.Style:=psDot;
        FVirtualCanvas.Font.Assign(FDefaultFont);
       
end;

procedure TAssiDrawControl.SetFontStyleDraw(FontName: AnsiString;
  FontSize: Double; FontStyle: TFontStyles);
var
  i:integer;
begin
    i:=ValWCSToValSCS(FontSize);
    if i>0 then
    begin
      FDrawFont                   :=True;
      FVirtualCanvas.Font.Size    :=i;
      FVirtualCanvas.Font.Style   :=FontStyle;
      FVirtualCanvas.Font.Name    :=FontName;
      FVirtualCanvas.Font.Color   :=FVirtualCanvas.Pen.Color;
    end
    else begin
      FDrawFont                   :=False;
    end;
end;

procedure TAssiDrawControl.PointDraw(X, Y: Double);
var
  PointSCS:TPoint;
begin
   PointSCS:=PointWCSToPointSCS(X,Y);
   FVirtualCanvas.Pixels[PointSCS.X,PointSCS.Y]:=FVirtualCanvas.Pen.Color;
end;

procedure TAssiDrawControl.LineDraw(X1, Y1, X2, Y2: Double);
var
  PointSCS:TPoint;
  PointWCS:TFloatPoint;
begin
   PointWCS.X:=X1;
   PointWCS.Y:=Y1;
   PointSCS:=PointWCSToPointSCS(PointWCS.X,PointWCS.Y);
   FVirtualCanvas.MoveTo(PointSCS.X,PointSCS.Y);
   PointWCS.X:=X2;
   PointWCS.Y:=Y2;
   PointSCS:=PointWCSToPointSCS(PointWCS.X,PointWCS.Y);
   FVirtualCanvas.LineTo (PointSCS.X,PointSCS.Y);
end;

procedure TAssiDrawControl.RectangelDraw(TopLeftX, TopLeftY, BottomRightX,
  BottomRightY: Double);
var
  PointSCS:TPoint;
  PointWCS:TFloatPoint;
begin
   PointSCS:=PointWCSToPointSCS(TopLeftX,TopLeftY);
   FVirtualCanvas.MoveTo(PointSCS.X,PointSCS.Y);
   PointSCS:=PointWCSToPointSCS(BottomRightX,TopLeftY);
   FVirtualCanvas.LineTo (PointSCS.X,PointSCS.Y);
   PointSCS:=PointWCSToPointSCS(BottomRightX,BottomRightY);
   FVirtualCanvas.LineTo (PointSCS.X,PointSCS.Y);
   PointSCS:=PointWCSToPointSCS(TopLeftX,BottomRightY);
   FVirtualCanvas.LineTo (PointSCS.X,PointSCS.Y);
   PointSCS:=PointWCSToPointSCS(TopLeftX,TopLeftY);
   FVirtualCanvas.LineTo (PointSCS.X,PointSCS.Y);
end;

procedure TAssiDrawControl.EllipseDraw(X0, Y0, AxleX, AxleY: Double);
var
  PointSCS1,PointSCS2:TPoint;
begin
   FVirtualCanvas.Brush.Style:=bsClear;

   PointSCS1:=PointWCSToPointSCS(X0-AxleX,Y0-AxleY);
   PointSCS2:=PointWCSToPointSCS(X0+AxleX,Y0+AxleY);
   FVirtualCanvas.Ellipse(PointSCS1.X,PointSCS1.Y,PointSCS2.X,PointSCS2.Y);
end;

procedure TAssiDrawControl.CircleDraw(X, Y, Radius: Double);
var
  PointSCS1,PointSCS2:TPoint;
begin
   FVirtualCanvas.Brush.Style:=bsClear;

   PointSCS1:=PointWCSToPointSCS(X-Radius,Y-Radius);
   PointSCS2:=PointWCSToPointSCS(X+Radius,Y+Radius);
   FVirtualCanvas.Ellipse(PointSCS1.X,PointSCS1.Y,PointSCS2.X,PointSCS2.Y);
end;

procedure TAssiDrawControl.ArcDraw(X0, Y0, X1, Y1, X2, Y2, Radius: Double);
var
  PointSCS1,PointSCS2,PointSCS3,PointSCS4:TPoint;
  BasePointWCS:TFloatPoint;
begin
{
  Рисует дугу.
  Параметры x1, y1, x2 и y2 задают эллипс, частью которого является дуга, параметры
  x3, y3, x4 и y4 ― начальную и конечную точку дуги. Цвет дуги определяет свойство Pen.Color.
}
   FVirtualCanvas.Brush.Style:=bsClear;

   BasePointWCS.X:=X0;
   BasePointWCS.Y:=Y0;
   //определяем габарит элипса
   PointSCS1:=PointWCSToPointSCS(BasePointWCS.X-Radius,BasePointWCS.Y-Radius);
   PointSCS2:=PointWCSToPointSCS(BasePointWCS.X+Radius,BasePointWCS.Y+Radius);
   //определяем точки концов дуги
   PointSCS3:=PointWCSToPointSCS(X1,Y1);
   PointSCS4:=PointWCSToPointSCS(X2,Y2);

   FVirtualCanvas.Arc(PointSCS1.X,PointSCS1.Y,PointSCS2.X,PointSCS2.Y,PointSCS3.X,PointSCS3.Y,PointSCS4.X,PointSCS4.Y);
end;

procedure TAssiDrawControl.TextDraw(X0, Y0, AWidth, AHeight: Double; Rotate:integer;
  AText: String; AAlign: TgaAttachmentPoint);
var
  PointSCS1,
  fpcPoint1,
  fpcPoint2:TPoint;
  TopLeftPointWCS,
  BottomRightPointWCS:TFloatPoint;
  ARect:TRect;
  W,H:Integer;
  iW,iH:integer;
  PosX,PosY,
  iRbuff,
  iR,iA,iRx:integer;
  Pixel:TColor;
  //Pic: TBitmap;
  //PicRect, TarRect: TRect;
begin
   //todo: Через FEntityFirstDrawBitMap надо реализовать Rotate

   if FDrawFont then
   begin
   if (AWidth<=0)or(AHeight<=0) then
   begin
      W:=FVirtualCanvas.TextWidth(AText);
      H:=FVirtualCanvas.TextHeight(AText);

      case AAlign of
      gaAttachmentPointTopLeft:
      begin
          PointSCS1:=PointWCSToPointSCS(X0,Y0);
      end;
      gaAttachmentPointTopCenter:
      begin
          PointSCS1:=PointWCSToPointSCS(X0,Y0);
          PointSCS1.X:=PointSCS1.X-W div 2;
      end;
      gaAttachmentPointTopRight:
      begin
          PointSCS1:=PointWCSToPointSCS(X0,Y0);
          PointSCS1.X:=PointSCS1.X-W;
      end;
      gaAttachmentPointMiddleLeft:
      begin
          PointSCS1:=PointWCSToPointSCS(X0,Y0);
          PointSCS1.Y:=PointSCS1.Y-H div 2;
      end;
      gaAttachmentPointMiddleCenter:
      begin
          PointSCS1:=PointWCSToPointSCS(X0,Y0);
          PointSCS1.X:=PointSCS1.X-W div 2;
          PointSCS1.Y:=PointSCS1.Y-H div 2;
      end;
      gaAttachmentPointMiddleRight:
      begin
          PointSCS1:=PointWCSToPointSCS(X0,Y0);
          PointSCS1.X:=PointSCS1.X-W;
          PointSCS1.Y:=PointSCS1.Y-H div 2;
      end;
      gaAttachmentPointBottomLeft:
      begin
          PointSCS1:=PointWCSToPointSCS(X0,Y0);
          PointSCS1.X:=PointSCS1.X;
          PointSCS1.Y:=PointSCS1.Y-H;
      end;
      gaAttachmentPointBottomCenter:
      begin
          PointSCS1:=PointWCSToPointSCS(X0,Y0);
          PointSCS1.X:=PointSCS1.X-W div 2;
          PointSCS1.Y:=PointSCS1.Y-H;
      end;
      gaAttachmentPointBottomRight:
      begin
          PointSCS1:=PointWCSToPointSCS(X0,Y0);
          PointSCS1.X:=PointSCS1.X-W;
          PointSCS1.Y:=PointSCS1.Y-H;
      end;
      end;

      FVirtualCanvas.Brush.Style:=bsClear;//Прозрачный текст
      FVirtualCanvas.TextOut(PointSCS1.X,PointSCS1.Y,AText);
   end
   else begin

      case AAlign of
      gaAttachmentPointTopLeft:
      begin
          TopLeftPointWCS.X:=X0;
          TopLeftPointWCS.Y:=Y0;
          BottomRightPointWCS.X:=X0+AWidth;
          BottomRightPointWCS.Y:=Y0-AHeight;
          PointSCS1:=PointWCSToPointSCS(X0,Y0);
      end;
      gaAttachmentPointTopCenter:
      begin
          TopLeftPointWCS.X:=X0-Width/2;
          TopLeftPointWCS.Y:=Y0;
          BottomRightPointWCS.X:=X0+AWidth/2;
          BottomRightPointWCS.Y:=Y0-AHeight;
          PointSCS1:=PointWCSToPointSCS(X0-AWidth/2,Y0);
      end;
      gaAttachmentPointTopRight:
      begin
          TopLeftPointWCS.X:=X0-AWidth;
          TopLeftPointWCS.Y:=Y0;
          BottomRightPointWCS.X:=X0;
          BottomRightPointWCS.Y:=Y0-AHeight;
          PointSCS1:=PointWCSToPointSCS(X0-AWidth,Y0);
      end;
      gaAttachmentPointMiddleLeft:
      begin
          TopLeftPointWCS.X:=X0;
          TopLeftPointWCS.Y:=Y0+AHeight/2;
          BottomRightPointWCS.X:=X0+AWidth;
          BottomRightPointWCS.Y:=Y0-AHeight/2;
          PointSCS1:=PointWCSToPointSCS(X0,Y0+AHeight/2);
      end;
      gaAttachmentPointMiddleCenter:
      begin
          TopLeftPointWCS.X:=X0-AWidth/2;
          TopLeftPointWCS.Y:=Y0+AHeight/2;
          BottomRightPointWCS.X:=X0+AWidth/2;
          BottomRightPointWCS.Y:=Y0-AHeight/2;
          PointSCS1:=PointWCSToPointSCS(X0-AWidth/2,Y0+AHeight/2);
      end;
      gaAttachmentPointMiddleRight:
      begin
          TopLeftPointWCS.X:=X0-AWidth;
          TopLeftPointWCS.Y:=Y0+AHeight/2;
          BottomRightPointWCS.X:=X0;
          BottomRightPointWCS.Y:=Y0-AHeight/2;
          PointSCS1:=PointWCSToPointSCS(X0-AWidth,Y0+AHeight/2);
      end;
      gaAttachmentPointBottomLeft:
      begin
          TopLeftPointWCS.X:=X0;
          TopLeftPointWCS.Y:=Y0+AHeight;
          BottomRightPointWCS.X:=X0+AWidth;
          BottomRightPointWCS.Y:=Y0;
          PointSCS1:=PointWCSToPointSCS(X0,Y0+AHeight);
      end;
      gaAttachmentPointBottomCenter:
      begin
          TopLeftPointWCS.X:=X0-AWidth/2;
          TopLeftPointWCS.Y:=Y0+AHeight;
          BottomRightPointWCS.X:=X0+AWidth/2;
          BottomRightPointWCS.Y:=Y0;
          PointSCS1:=PointWCSToPointSCS(X0-AWidth/2,Y0+AHeight);
      end;
      gaAttachmentPointBottomRight:
      begin
          TopLeftPointWCS.X:=X0-AWidth;
          TopLeftPointWCS.Y:=Y0+AHeight;
          BottomRightPointWCS.X:=X0;
          BottomRightPointWCS.Y:=Y0;
          PointSCS1:=PointWCSToPointSCS(X0-AWidth,Y0+AHeight);
      end;
      end;
      fpcPoint1:=PointWCSToPointSCS(TopLeftPointWCS.X,TopLeftPointWCS.Y);
      fpcPoint2:=PointWCSToPointSCS(BottomRightPointWCS.X,BottomRightPointWCS.Y);
      ARect:=Rect(fpcPoint1.x,fpcPoint1.y,fpcPoint2.x,fpcPoint2.y);

      FVirtualCanvas.Brush.Style:=bsClear;//Прозрачный текст
      FVirtualCanvas.TextRect(ARect,PointSCS1.X,PointSCS1.Y-ValWCSToValSCS(2.15),AText);

      {
      //Прозрачный текст первая реализация
      Pic := TBitmap.Create;
      Pic.Canvas.Font := FVirtualCanvas.Font;
      Pic.Canvas.Pen := FVirtualCanvas.Pen;
      Pic.Width := ARect.Right-ARect.Left;
      Pic.Height := ARect.Bottom-ARect.Top;
      PicRect := Rect(0, 0, Pic.Width, Pic.Height);
      TarRect := Rect(PointSCS1.X, PointSCS1.Y, PointSCS1.X + Pic.Width, PointSCS1.Y + Pic.Height);
      //pic.Canvas.CopyRect(PicRect, FVirtualCanvas, TarRect);
      //pic.Canvas.FillRect(PicRect);
      Pic.Transparent:=true;
      Pic.TransparentColor:=Pic.Canvas.Pixels[0,0];
      //Pic.TransparentMode:=tmFixed;
      //SetBkMode(Pic.Canvas.Handle, Transparent);
      Pic.Canvas.TextOut(0, 0, Text);
      Pic.TransparentColor:=Pic.Canvas.Pixels[0,0];
      FVirtualCanvas.Draw(PointSCS1.X, PointSCS1.Y,Pic);
      Pic.Free;
      }
      if FDevelop then
      begin
          FVirtualCanvas.Pen.Mode:=pmcopy;
          FVirtualCanvas.Brush.Color:=clSilver;
          FVirtualCanvas.FrameRect(ARect);//прямоугольник  вокруг текста
      end;

   end;
   end;//FDrawFont
end;

procedure TAssiDrawControl.MsgMouseWheel(var Msg: TWMMouseWheel);
begin

end;

{Load/Save}

procedure TAssiDrawControl.SaveToFile(FileName: String);
begin

end;


procedure TAssiDrawControl.LoadFromFile(FileName: String);
begin

end;

{ TAssiDrawDocument }

function TAssiDrawDocument.GetDeltaVertex: Double;
var
  k1,k2,X2:Double;
begin
     k2:=FViewScale;
     k2:=k2/100;
     k2:=SimpleRoundTo(k2,-2);
     //получаем коэффициент точности
     k1:=Math.Power(10,PointPrecision);
     //vbmHeight,vbpWidth:Integer;
     X2:=(DELTASELECTVERTEX/k1)*k2;
     Result:=X2;
end;

function TAssiDrawDocument.GetDocument: TDrawDocumentCustom;
begin
  Result:=Self;
end;

constructor TAssiDrawDocument.Create(AOwner: TComponent);
begin
  inherited Create;
  FEditMode                 :=eemCanAll;
  FModelSpace               :=TWorkSpace.Create; //Создание рабочего пространства
  FModelSpace.OnGetDocument :=@GetDocument;
  FBlockList                :=TBlockList.Create; // Создание списка пространств блоков
  FBlockList.OnGetDocument  :=@GetDocument;

  FSelectList                    :=TList.Create;
  ModelSpace.SelectedEntityList  :=FSelectList;
  FDrawControl                   :=TAssiDrawControl(AOwner);

  //Предустановки параметров
  FPointUnit                     :=puMillimetre;
  FPointPrecision                :=2;
  FViewScale                     :=5000;
  FViewScaleK                    :=1000;
  FDefaultLineWeight             :=gaLnWtByLayer;
  FDefaultColor                  :=gaByLayer;

  FViewPos.X                     :=50/math.power(10,FPointPrecision);
  FViewPos.Y                     :=50/math.power(10,FPointPrecision);
  FViewPos.Z                     :=50/math.power(10,FPointPrecision);

end;

procedure TAssiDrawDocument.DeselectAll;
begin
  FSelectList.Clear;
  if Assigned(FDrawControl.OnSelectListChange) then
     FDrawControl.OnSelectListChange(FDrawControl);
  DrawControl.Refresh;
end;

destructor TAssiDrawDocument.Destroy;
begin
  FDrawControl:=nil;
  SetLength(FMVertArray,0);
  FModelSpace.Free;
  FBlockList.Free;
  FSelectList.Free;
  inherited Destroy;
end;

procedure TAssiDrawDocument.MVertArray(Value: TModifyVertex);
var
  L,Counter,Count,i:Integer;
  TrueCopy:boolean;
  Temp:TModifyVertexArray;
begin
  Count:=Length(FMVertArray);
  TrueCopy:=false;
  Counter:=0;
  SetLength(Temp,Counter);
  for I := 0 to Count-1 do
  begin
    if (FMVertArray[i].Item<>Value.Item)
    and(FMVertArray[i].VertexPos.X<>Value.VertexPos.X)
    and(FMVertArray[i].VertexPos.Y<>Value.VertexPos.Y)
    and(FMVertArray[i].VertexPos.Z<>Value.VertexPos.Z) then
    begin
       Counter:=Counter+1;
       SetLength(Temp,Counter);
       Temp[Counter-1]:=FMVertArray[i];
    end
    else TrueCopy:=true;
  end;

  if not TrueCopy then
  begin
    Counter:=Counter+1;
    SetLength(Temp,Counter);
    Temp[Counter-1].Item:=Value.Item;
    Temp[Counter-1].VertexPos:=Value.VertexPos;
    Temp[Counter-1].VertexIndex:=Value.VertexIndex;
  end;

  SetLength(FMVertArray,Counter);
  for I := 0 to Counter-1 do
  begin
    FMVertArray[i]:=Temp[i];
  end;

end;

procedure TAssiDrawDocument.ZoomToFit;
var
  minPoint,maxPoint:TPoint;
  tmpViewPos1,tmpViewPos2:TFloatPoint;
  tmpTopLeft,tmpBottomRigth:TFloatPoint;
  H,W,Kh,Kw:Float;
begin

  FViewScale:=100;
  FViewScaleK:=1;
  // Получаем крайние точки чертежа
  GetEntityListRectVertex(ModelSpace.Objects,tmpTopLeft,tmpBottomRigth);

  minPoint:=FDrawControl.PointWCSToPointSCS(tmpTopLeft.X,tmpBottomRigth.Y);
  maxPoint:=FDrawControl.PointWCSToPointSCS(tmpBottomRigth.X,tmpTopLeft.Y);

  if (minPoint.X<0)or(minPoint.Y<0)or(maxPoint.X>FDrawControl.Width)
  or(maxPoint.Y>FDrawControl.Height) then
  begin
       // Ищем центр
       tmpViewPos1.x := tmpTopLeft.X + (tmpBottomRigth.X-tmpTopLeft.X) / 2;
       tmpViewPos1.y := tmpBottomRigth.Y + (tmpTopLeft.Y - tmpBottomRigth.Y) / 2;

      //while (minPoint.X<0)or(minPoint.Y<0)or(maxPoint.X>FDrawControl.Width)or(maxPoint.Y>FDrawControl.Height) do
      W:=(maxPoint.X-minPoint.X);
      if W<0 then
      W:=W*-1;
      H:=(maxPoint.X-minPoint.X);
      if H<0 then
      H:=H*-1;

      if W=0 then W:=1;
      Kw:=FDrawControl.Width/W;
      if H=0 then H:=1;
      Kh:=FDrawControl.Height/H;

      while (Kw<1.25)or(Kh<1.25) do
      begin
        FViewScale:=FViewScale+FViewScaleK;
        FViewScaleK:=FViewScaleK+1;
        //Определение положения курсора и компенсация сдвига масштаба
        tmpViewPos2:=FDrawControl.PointSCSToPointWCS(FDrawControl.Width div 2,FDrawControl.Height div 2);
        tmpViewPos2.X:=tmpViewPos2.X-tmpViewPos1.X;
        tmpViewPos2.Y:=tmpViewPos2.Y-tmpViewPos1.Y;
        FViewPos.X:=FViewPos.X+tmpViewPos2.X;
        FViewPos.Y:=FViewPos.Y+tmpViewPos2.Y;
        minPoint:=FDrawControl.PointWCSToPointSCS(tmpTopLeft.X,tmpBottomRigth.Y);
        maxPoint:=FDrawControl.PointWCSToPointSCS(tmpBottomRigth.X,tmpTopLeft.Y);

        W:=(maxPoint.X-minPoint.X);
        if W<0 then
        W:=W*-1;
        H:=(maxPoint.Y-minPoint.Y);
        if H<0 then
        H:=H*-1;

        if W=0 then W:=1;
        if H=0 then H:=1;

        Kw:=FDrawControl.Width/W;
        Kh:=FDrawControl.Height/H;
      end;
  end;

  FDrawControl.SuperControlPaint(FDrawControl);

end;

end.
