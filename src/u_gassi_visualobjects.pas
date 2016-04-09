unit u_gassi_visualobjects;

//************************************************************
//
//    Модуль компонента Graphic Assi Control
//    Copyright (c) 2013  Pichugin M.
//    ver. 0.8
//    Разработчик: Pichugin M. (e-mail: pichugin_m@mail.ru)
//
//************************************************************

interface

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Contnrs, Classes, SysUtils, controls, Graphics,
  u_gassi_logicaldraw, u_gassi_const, u_gassi_geometry;

// ver. 0.8
// - Измена архитектура получения доступа объектов к классу TDrawDocumentCustom
// - Добавлена обработка блок в блоке
//
// ver. 0.6
// Новое:
// property  Tag  : integer;

type

  { Forward Declarartions }

  TDrawDocumentCustom = class;

  TEntity      = class;
  TWorkSpace   = class;
  TBlockItem   = class;
  TBlockList   = class;

  { Data types }

  TEntityID = ShortString;

  TEntityState = set of (esNone,esCreating,esEditing,esMoving,esSelected);
  TEntityDrawStyle = set of (edsNone,edsNormal,edsSelected,edsEditing,edsMoving,edsCreating);

  TGetDocumentEvent = function :TDrawDocumentCustom of object;

  { Record Declarartions }

  // Логические координаты
  PFloatPoint = ^TFloatPoint;
  TFloatPoint = record
    X, Y, Z :Double;
  end;

  PTFloatRect = ^TFloatRect;
  TFloatRect = record
    TopX, TopY, TopZ           :Double;
    BottomX, BottomY, BottomZ  :Double;
  end;

  TModifyVertex = record
    Item        : TEntity;
    VertexIndex : Integer;
    VertexPos   : TFloatPoint;
  end;

  // Массив точек
  TPointsArray                = array of TFloatPoint;
  TModifyVertexArray          = array of TModifyVertex;

  { TFloatPointList }

  TFloatPointList = class
  private
         FList   : TList;
         function GetCount: Integer;
         function GetPoint(Index: Integer): TFloatPoint;
         procedure SetPoint(Index: Integer; const Value: TFloatPoint);
         function  NewPoint(X, Y, Z: Double): PFloatPoint;
  public
         constructor Create; virtual;
         destructor Destroy; override;
         procedure Add(X, Y, Z: Double);
         procedure Insert(Index: Integer; X, Y, Z: Double);
         procedure Delete(Index: Integer);
         function  Extract(Index: Integer): PFloatPoint;
         property  Count: Integer read GetCount;
         property  Items[Index: Integer]: TFloatPoint read GetPoint write SetPoint;
  end;

  { TDrawDocumentCustom }

  TDrawDocumentCustom = class
  protected
    FModelSpace     :TWorkSpace;
    FBlockList      :TBlockList;
  public

    function GetDeltaVertex:Double; virtual; abstract;
    procedure DeselectAll; virtual; abstract;
  end;

// Эллементы чертежа

   { TEntityList }

   TEntityList      = class(TList)
   private
        FID                 : TEntityID;
        FModelSpace         : TWorkSpace;
        procedure SetEntityLinkVar(AEntity:TEntity);
        procedure ChangeCordVertex(const AVertCord:TFloatPoint);
        function GetCount: Integer;
        function GetItem(Index: Integer): TEntity;
        procedure SetItem(Index: Integer; const Value: TEntity);
   protected

   public
       constructor Create; virtual;
       destructor Destroy; override;
       // Cобытия
       procedure Add(AEntity: TEntity); overload;
       function  Add(AParentID:TEntityID): TEntity; overload;
       procedure Insert(Index: Integer; AEntity: TEntity);
       procedure Delete(Index: Integer);
       procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);
       procedure RepaintVertex(LogicalDrawing: TLogicalDraw);
       function  GetEntityByID(AID:TEntityID): TEntity;
       //procedure SelectObjectRect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect:Boolean);
       property  ID: TEntityID read FID write FID;
       //property  BlockList: TEntityList read FBlockList write FBlockList;
       //property  EntityList: TEntityList read FEntityList write FEntityList;

       property  Count: Integer read GetCount;
       property  Items[Index: Integer]: TEntity read GetItem write SetItem;
   end;

   { TWorkSpaceCustom }

      TWorkSpaceCustom      = class
      private
           FTopLeft            : TFloatPoint;
           FBottomRight        : TFloatPoint;
           FSelectedEntityList : TList;
           FEntityList         : TEntityList;
           FOnGetDocumentEvent : TGetDocumentEvent;

           //Временное значение
           FByBlockColor        :TgaColor;
           FByBlockLineWeight   :TgaLineWeight;
           function GetDocument: TDrawDocumentCustom;
      protected

      public
          constructor Create; virtual; overload;
          destructor Destroy; override;
          // Cобытия

          procedure Repaint(Xshift,Yshift,AScale:Double;
                     LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);
          procedure RepaintVertex(LogicalDrawing: TLogicalDraw);
          procedure DeselectAll;
          //procedure ResetMinMaxPoint; overload;
          //function  GetMinMaxPoint:TMinMaxPoint; overload;
          //function  GetColor(AParentID:TEntityID; AColor:TgaColor):TgaColor;
          //function  GetLineWeight(AParentID:TEntityID; LineWeight:TgaLineWeight):TgaLineWeight;
          function  GetColor(AColor:TgaColor):TgaColor;
          function  GetLineWeight(LineWeight:TgaLineWeight):TgaLineWeight;
          procedure GetRectVertex(var ATopLeft, ABottomRight: TFloatPoint);
          //Действия
          //Перемещение группы объектов
          procedure MoveEntityGroup(AOwnerGroup: TEntityID; APoint: TFloatPoint);

          property  ThisDocument : TDrawDocumentCustom read GetDocument;
          property  OnGetDocument: TGetDocumentEvent read FOnGetDocumentEvent
                                                     write FOnGetDocumentEvent;
          property  SelectedEntityList: TList read FSelectedEntityList
                                              write FSelectedEntityList;
          property  Objects: TEntityList read FEntityList write FEntityList;
      end;

  { TWorkSpace }

   TWorkSpace      = class(TWorkSpaceCustom);

   { TBlockItem }

   TBlockItem      = class(TWorkSpaceCustom)
   private
      FName        : AnsiString;    // Идентификатор
   protected

   public
      property  Name: AnsiString read FName write FName;
   end;

   { TBlockList }

   TBlockList      = class(TObjectList)
   private
       FOnGetDocumentEvent : TGetDocumentEvent;
       function GetItem(Index: Integer): TBlockItem;
       function GetBlock(Name: AnsiString): TBlockItem;
       procedure SetItem(Index: Integer; const Value: TBlockItem);
       procedure SetBlock(Name: AnsiString; AValue: TBlockItem);
   protected

   public
       constructor Create; virtual; overload;
       function Add(AObject: TBlockItem): Integer;
       procedure Insert(Index: Integer; AObject: TBlockItem);
       property  Block[Name: AnsiString]: TBlockItem read GetBlock write SetBlock;
       property  Items[Index: Integer]: TBlockItem read GetItem write SetItem;
       property  OnGetDocument: TGetDocumentEvent read FOnGetDocumentEvent
                                                     write FOnGetDocumentEvent;
   end;

   { TEntityBasic }

   TEntityBasic = class // Базовый класс
   private
       FBlocked     : Boolean;
       FParentList  : TEntityList;   // Основной список эллементов чертежа
       FBlockList   : TEntityList;   // Список составных частей блока
       FOnGetDocumentEvent : TGetDocumentEvent;
       //свойства
   protected
       FID           : TEntityID;    // Уникальный идентификатор эллемента чертежа
       FState        : TEntityState;
       FVertex       : TFloatPointList;
       FLineWeight   : LongWord;     // Толщина линий
       FColor        : LongWord;     // Цвет объекта
       FData         : Pointer;
       FTag          : integer;
       FGroupOwner   : TEntityID;

       //procedure Change;
       function  GetVertexCount: Integer; virtual; abstract;
       function  GetVertex(Index: Integer): TFloatPoint; virtual; abstract;
       procedure SetVertex(Index: Integer; const Value: TFloatPoint); virtual; abstract;
   public
       constructor Create; virtual;
       destructor Destroy; override;
       procedure Created;
       procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle); overload; virtual; abstract;
       procedure Repaint(LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);  overload;virtual; abstract;
       procedure RepaintVertex(LogicalDrawing: TLogicalDraw); virtual; abstract;
       function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean):Integer; overload;virtual; abstract;
       function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean; var MVertx:TModifyVertex):Integer; overload;virtual; abstract;
       function GetColor(AColor:TgaColor):TgaColor; overload;
       function GetLineWeight(ALineWeight:TgaLineWeight):TgaLineWeight; overload;
       function GetColor:TgaColor; overload;
       function GetLineWeight:TgaLineWeight; overload;
       // Методы блокировки/разблокировки
       procedure BeginUpdate;
       procedure EndUpdate;
       // Свойства/события
       procedure AddVertex(X, Y, Z: Double); virtual; abstract;
       procedure InsertVertex(Index: Integer; X, Y, Z: Double);  virtual; abstract;
       procedure DeleteVertex(Index: Integer); virtual; abstract;

       property  VertexCount: Integer read GetVertexCount;
       property  Vertex[Index: Integer]: TFloatPoint read GetVertex write SetVertex;
       property  State: TEntityState read FState write FState;
       property  ID: TEntityID read FID write FID;

       property  OnGetDocument: TGetDocumentEvent read FOnGetDocumentEvent
                                                  write FOnGetDocumentEvent;
       //property  ParentID: TEntityID read FParentID write FParentID;
       //property OnChange: TNotifyEvent read FOnChange write FOnChange;
   end;

   TEntity      = class(TEntityBasic)  // Общий предок класс
   private
       function GetDocument: TDrawDocumentCustom;
   protected
       function GetVertexCount: Integer; override;
       function GetVertex(Index: Integer): TFloatPoint; override;
       procedure SetVertex(Index: Integer; const Value: TFloatPoint); override;
       function GetVertexAxleX(Index: Integer): Double;
       function GetVertexAxleY(Index: Integer): Double;
       function GetVertexAxleZ(Index: Integer): Double;
       procedure SetVertexAxleX(Index: Integer; const Value: Double);virtual;
       procedure SetVertexAxleY(Index: Integer; const Value: Double);virtual;
       procedure SetVertexAxleZ(Index: Integer; const Value: Double);virtual;

       procedure AddVertex(X, Y, Z: Double); override;
       procedure InsertVertex(Index: Integer; X, Y, Z: Double); override;
       procedure DeleteVertex(Index: Integer); override;

       property  VertexCount: Integer read GetVertexCount;
       property  Vertex[Index: Integer]: TFloatPoint read GetVertex write SetVertex;
       property  VertexAxleX[Index: Integer]: Double read GetVertexAxleX write SetVertexAxleX;
       property  VertexAxleY[Index: Integer]: Double read GetVertexAxleY write SetVertexAxleY;
       property  VertexAxleZ[Index: Integer]: Double read GetVertexAxleZ write SetVertexAxleZ;
   published
       property  LineWeight: TgaLineWeight read FLineWeight write FLineWeight;
       property  Color: TgaColor read FColor write FColor;
       property  Tag  : integer read FTag write FTag;
   public
       constructor Create; override;
       destructor Destroy; override;
       function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean):Integer;overload;virtual;
       function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean; var MVertx:TModifyVertex):Integer; overload;virtual;
       procedure Repaint(LogicalDrawing: TLogicalDraw; Style:TEntityDrawStyle); override;

       //function GetMinMaxPoint:TMinMaxPoint;
       procedure GetRectVertex(var ATopLeft,ABottomRight:TFloatPoint);virtual;

       // Свойства/события
       property  ThisDocument : TDrawDocumentCustom read GetDocument;
       property  Data : Pointer read FData write FData;
       property  GroupOwner : TEntityID read FGroupOwner write FGroupOwner;

       procedure MoveVertex(Index:integer; NewVertex:TFloatPoint);virtual;

       property  ParentList: TEntityList read FParentList write FParentList;
       property  BlockList: TEntityList read FBlockList write FBlockList;
   end;

   TEntityLineBasic      = class(TEntity)
   protected
   public
       property  Vertex[Index: Integer]: TFloatPoint read GetVertex write SetVertex;
       procedure RepaintVertex(LogicalDrawing: TLogicalDraw); override;
       function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean):Integer; overload; override;
       function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean; var MVertx:TModifyVertex):Integer; overload; override;
   end;

   { TEntityEllipseBasic }

   TEntityEllipseBasic      = class(TEntity)
   private
        FAxleY: Double;
        FAxleX: Double;
        function GetRadius: Double; virtual; abstract;
        procedure SetRadius(const Value: Double); virtual;  abstract;
        function GetDiameter: Double; virtual; abstract;
        procedure SetDiameter(const Value: Double); virtual; abstract;
        function GetBasePoint: TFloatPoint; virtual;
        procedure SetBasePoint(const Value: TFloatPoint); virtual;
   protected
        function GetAxleX: Double; virtual; abstract;
        function GetAxleY: Double; virtual; abstract;
        procedure SetAxleX(const Value: Double); virtual; abstract;
        procedure SetAxleY(const Value: Double); virtual; abstract;
   public
        property  AxleY: Double read GetAxleY write SetAxleY;
        property  AxleX: Double read GetAxleX write SetAxleX;
        property  Radius: Double read GetRadius write SetRadius;
        property  Diameter: Double read GetDiameter write SetDiameter;
        property  BasePoint: TFloatPoint read GetBasePoint write SetBasePoint;
        procedure GetRectVertex(var ATopLeft,ABottomRight:TFloatPoint);override;
        function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean):Integer;override;
        function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean; var MVertx:TModifyVertex):Integer; overload; override;
        procedure MoveVertex(Index:integer; NewVertex:TFloatPoint);override;
   end;

  { TEntityBlockBasic }

  TEntityBlockBasic      = class(TEntity)
  private
      FBlockID     : AnsiString;
      FScale       : Double;
      function GetBasePoint: TFloatPoint; virtual;
      procedure SetBasePoint(const Value: TFloatPoint); virtual;
      procedure AddVertex(X, Y, Z: Double); override;
      procedure InsertVertex(Index: Integer; X, Y, Z: Double); override;
   public
      constructor Create; override;
      property  BasePoint: TFloatPoint read GetBasePoint write SetBasePoint;
      property  BlockID: AnsiString read FBlockID write FBlockID;
      procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle); override;
      procedure RepaintVertex(LogicalDrawing: TLogicalDraw); override;
      function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean):Integer;  overload;override;
      function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean; var MVertx:TModifyVertex):Integer; overload;override;
      procedure GetRectVertex(var ATopLeft,ABottomRight:TFloatPoint);override;
   end;

  { TEntityTextBasic }

  TEntityTextBasic      = class(TEntity)
  private
      FRotate       : integer;
      function GetBasePoint: TFloatPoint; virtual;
      procedure SetBasePoint(const Value: TFloatPoint); virtual;
   public
      constructor Create; override;
      property  BasePoint: TFloatPoint read GetBasePoint write SetBasePoint;
      procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle); override;
      procedure RepaintVertex(LogicalDrawing: TLogicalDraw); override;
      function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean):Integer;  overload;override;
      function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean; var MVertx:TModifyVertex):Integer; overload;override;

  end;
   
   TGraphicPoint      = class(TEntity)  // Точка
   private

   public
       // Свойства/события
       procedure AddVertex(X, Y, Z: Double); override;
       procedure InsertVertex(Index: Integer; X, Y, Z: Double); override;
       procedure Draw(APoint:TFloatPoint);overload;
       procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle); override;
       procedure RepaintVertex(LogicalDrawing: TLogicalDraw); override;
       function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean):Integer;  overload;override;
       function GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean; var MVertx:TModifyVertex):Integer; overload;override;
   end;

   TGraphicLine                = class(TEntityLineBasic)  // Линия
   public
       procedure Draw(APoint1,APoint2:TFloatPoint);overload;
       procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle); override;
   end;

   TGraphicPolyline            = class(TEntityLineBasic)  // Полилиния
   private
      FClosed:Boolean;
   public
      property  Closed: Boolean read FClosed write FClosed;
      procedure Draw(APoints:TPointsArray;AClosed: Boolean);overload;
      procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle); override;
      procedure RepaintVertex(LogicalDrawing: TLogicalDraw); override;
   end;

   TGraphicEllipse              = class(TEntityEllipseBasic)  // Элипс
   private
      function GetDiameter: Double; override;
      procedure SetDiameter(const Value: Double); override;
      function GetRadius: Double;override;
      procedure SetRadius(const Value: Double); override;
      function GetAxleX: Double; override;
      function GetAxleY: Double; override;
      procedure SetAxleX(const Value: Double); override;
      procedure SetAxleY(const Value: Double); override;
   public
       property  Radius: Double read GetRadius write SetRadius;
       property  Diameter: Double read GetDiameter write SetDiameter;
       property  AxleY: Double read GetAxleY write SetAxleY;
       property  AxleX: Double read GetAxleX write SetAxleX;
       procedure Draw(ABasePoint:TFloatPoint;AAxleY,AAxleX,ARotate:integer);overload;
       procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle); override;
       procedure RepaintVertex(LogicalDrawing: TLogicalDraw); override;
   end;

   TGraphicCircle              = class(TEntityEllipseBasic)  // Круг
   private
        function GetDiameter: Double; override;
        procedure SetDiameter(const Value: Double); override;
        function GetRadius: Double;override;
        procedure SetRadius(const Value: Double); override;
        function GetAxleX: Double; override;
        function GetAxleY: Double; override;
        procedure SetAxleX(const Value: Double); override;
        procedure SetAxleY(const Value: Double); override;
   public
        property  AxleY: Double read GetAxleY write SetAxleY;
        property  AxleX: Double read GetAxleX write SetAxleX;
        property  Radius: Double read GetRadius write SetRadius;
        property  Diameter: Double read GetDiameter write SetDiameter;
        procedure Draw(ABasePoint:TFloatPoint;ARadius:Double);overload;
        procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle); override;
        procedure RepaintVertex(LogicalDrawing: TLogicalDraw); override;
   end;

   TGraphicArc                 = class(TEntityEllipseBasic)  // Дуга
   private
      function GetDiameter: Double; override;
      procedure SetDiameter(const Value: Double); override;
      function  GetRadius: Double;override;
      procedure SetRadius(const Value: Double); override;
   public
      procedure Draw(ABasePoint,APoint1,APoint2:TFloatPoint;ARadius:Double);overload;
      procedure Draw(APoint1,APoint2,APoint3:TFloatPoint);overload;
      procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle); override;
      procedure RepaintVertex(LogicalDrawing: TLogicalDraw); override;
      //function  GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean):Integer; override;
      //function  GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean; var MVertx:TModifyVertex):Integer; overload; override;
      procedure MoveVertex(Index:integer; NewVertex:TFloatPoint);override;
   end;

   { TGraphicText }

   TGraphicText                = class(TEntityTextBasic)  // Текс
   private
      FWidth: Double;
      FHeight: Double;
      FAlign: TgaAttachmentPoint;
      FText: String;

      FFontSize: Double;
      FFontStyle: TFontStyles;
      FFontName: AnsiString;
      function GetHeight: Double;
      function GetWidth: Double;
      procedure SetHeight(const Value: Double);
      procedure SetWidth(const Value: Double);
   public
       constructor Create; override;
       destructor Destroy; override;
       property  Width: Double read GetWidth write SetWidth;
       property  Height: Double read GetHeight write SetHeight;
       property  Rotate: integer read FRotate write FRotate;

       property  Text: String read FText write FText;
       property  Align: TgaAttachmentPoint read FAlign write FAlign;

       property  FontSize: Double read FFontSize write FFontSize;
       property  FontStyle: TFontStyles read FFontStyle write FFontStyle;
       property  FontName: AnsiString read FFontName write FFontName;

       procedure Draw(ABasePoint:TFloatPoint; AText: String; AAlign: TgaAttachmentPoint; ARotate:integer);overload;
       procedure Draw(ABasePoint:TFloatPoint; AText: String; AAlign: TgaAttachmentPoint; AWidth,AHeight,ARotate:integer);overload;

       procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle); override;
       procedure RepaintVertex(LogicalDrawing: TLogicalDraw); override;
       function  GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean):Integer; override;
       function  GetSelect(TopLeft, BottomRight: TFloatPoint; AllVertexInRect: Boolean; var MVertx:TModifyVertex):Integer; overload; override;
       procedure MoveVertex(Index:integer; NewVertex:TFloatPoint);override;
       procedure GetRectVertex(var ATopLeft,ABottomRight:TFloatPoint);override;
   end;

   TGraphicBlock               = class(TEntityBlockBasic)  // Блок
   public
       procedure Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle); override;
       procedure RepaintVertex(LogicalDrawing: TLogicalDraw);  override;
       procedure Draw(ABasePoint:TFloatPoint; ABlockID:string; AScale,ARotate:integer);overload;
       property  Scale: Double read FScale write FScale;
   end;

// Дополнительные типы

   {
   TGraphicConnectionline      = class(TEntity);  // Соединительная линия
   TGraphicHatch               = class(TEntity);  // Штриховка
   }

const

  DELTASELECTVERTEX            = 10;

  //the affected area
  AFFA_OUTSIDE                 =-1; //Вне периметра
  AFFA_BASEPOINT               =0; //Базовая точка
  AFFA_VERTEX                  =1; //Вершина
  AFFA_INSIDE                  =2; //В периметре
  AFFA_BORDER                  =3; //Граница

  VERTEXMARKER_BASEPOINT_SEL   =-2; //Базовая точка
  VERTEXMARKER_VERTEX_SEL      =-3; //Вершина

  VERTEXMARKER_OUTSIDE         =-1; //Вне периметра
  VERTEXMARKER_BASEPOINT       =0; //Базовая точка
  VERTEXMARKER_VERTEX          =1; //Вершина
  VERTEXMARKER_INSIDE          =2; //В периметре
  VERTEXMARKER_BORDER          =3; //Граница
  VERTEXMARKER_CENTER          =4; //Центр

  LINETYPE_SOLID               ='LT_SOLID';
  LINETYPE_SELECTED            ='LT_SELECTED';

var
  IDIndex : Integer;

procedure GetEntityListRectVertex(AEntityList:TEntityList; var ATopLeft, ABottomRight: TFloatPoint);

implementation

function SetNullToFloatPoint:TFloatPoint;
var
  APoint:TFloatPoint;
begin
  APoint.X:=0;
  APoint.Y:=0;
  APoint.Z:=0;
  Result:=APoint;
end;

function FloatPoint(X,Y,Z:Double):TFloatPoint;
var
  APoint:TFloatPoint;
begin
  APoint.X:=X;
  APoint.Y:=Y;
  APoint.Z:=Z;
  Result:=APoint;
end;

procedure SetDeltaToRectPoint(var TopLeft, BottomRight:TFloatPoint; DeltaVertex:Double);
begin
      //DeltaVertex:=FDeltaCord+5;
      TopLeft.X:=TopLeft.X-DeltaVertex;
      TopLeft.Y:=TopLeft.Y+DeltaVertex;
      BottomRight.X:=BottomRight.X+DeltaVertex;
      BottomRight.Y:=BottomRight.Y-DeltaVertex;
end;

function PointIn2DRect(Point, RectTopLeft, RectBottomRight: TFloatPoint): Boolean;
begin
  Result:=PointInRect2D(Point.X,Point.Y,RectTopLeft.X,RectTopLeft.Y,RectBottomRight.X,RectBottomRight.Y);
end;

function CordEqualIn2D(APoint,BPoint: TFloatPoint):boolean;
begin
  if (APoint.X=BPoint.X)and(APoint.Y=BPoint.Y) then
      Result:=true
  else
      Result:=false;
end;

function CordEqualIn3D(APoint,BPoint: TFloatPoint):boolean;
begin
  if (APoint.X=BPoint.X)and(APoint.Y=BPoint.Y)and(APoint.Z=BPoint.Z) then
      Result:=true
  else
      Result:=false;
end;

procedure GetRectCord(const Align:TgaAttachmentPoint; X0,Y0,AWidth,AHeight:Double; var TopLeftPointWCS,BottomRightPointWCS: TFloatPoint);
begin
      case Align of
      gaAttachmentPointTopLeft:
      begin
          TopLeftPointWCS.X:=X0;
          TopLeftPointWCS.Y:=Y0;
          BottomRightPointWCS.X:=X0+AWidth;
          BottomRightPointWCS.Y:=Y0-AHeight;
      end;
      gaAttachmentPointTopCenter:
      begin
          TopLeftPointWCS.X:=X0-AWidth/2;
          TopLeftPointWCS.Y:=Y0;
          BottomRightPointWCS.X:=X0+AWidth/2;
          BottomRightPointWCS.Y:=Y0-AHeight;
      end;
      gaAttachmentPointTopRight:
      begin
          TopLeftPointWCS.X:=X0-AWidth;
          TopLeftPointWCS.Y:=Y0;
          BottomRightPointWCS.X:=X0;
          BottomRightPointWCS.Y:=Y0-AHeight;
      end;
      gaAttachmentPointMiddleLeft:
      begin
          TopLeftPointWCS.X:=X0;
          TopLeftPointWCS.Y:=Y0+AHeight/2;
          BottomRightPointWCS.X:=X0+AWidth;
          BottomRightPointWCS.Y:=Y0-AHeight/2;
      end;
      gaAttachmentPointMiddleCenter:
      begin
          TopLeftPointWCS.X:=X0-AWidth/2;
          TopLeftPointWCS.Y:=Y0+AHeight/2;
          BottomRightPointWCS.X:=X0+AWidth/2;
          BottomRightPointWCS.Y:=Y0-AHeight/2;
      end;
      gaAttachmentPointMiddleRight:
      begin
          TopLeftPointWCS.X:=X0-AWidth;
          TopLeftPointWCS.Y:=Y0+AHeight/2;
          BottomRightPointWCS.X:=X0;
          BottomRightPointWCS.Y:=Y0-AHeight/2;
      end;
      gaAttachmentPointBottomLeft:
      begin
          TopLeftPointWCS.X:=X0;
          TopLeftPointWCS.Y:=Y0+AHeight;
          BottomRightPointWCS.X:=X0+AWidth;
          BottomRightPointWCS.Y:=Y0;
      end;
      gaAttachmentPointBottomCenter:
      begin
          TopLeftPointWCS.X:=X0-AWidth/2;
          TopLeftPointWCS.Y:=Y0+AHeight;
          BottomRightPointWCS.X:=X0+AWidth/2;
          BottomRightPointWCS.Y:=Y0;
      end;
      gaAttachmentPointBottomRight:
      begin
          TopLeftPointWCS.X:=X0-AWidth;
          TopLeftPointWCS.Y:=Y0+AHeight;
          BottomRightPointWCS.X:=X0;
          BottomRightPointWCS.Y:=Y0;
      end;
      end;
end;

procedure GetEntityListRectVertex(AEntityList:TEntityList; var ATopLeft, ABottomRight: TFloatPoint);
var
   x1TopLeft,x1BottomRight: TFloatPoint;
   x2TopLeft,x2BottomRight: TFloatPoint;
   i:integer;
begin
  x1TopLeft:=SetNullToFloatPoint;
  x2TopLeft:=SetNullToFloatPoint;
  x1BottomRight:=SetNullToFloatPoint;
  x2BottomRight:=SetNullToFloatPoint;

  if AEntityList.Count>0 then
  begin
       AEntityList.Items[0].GetRectVertex(x1TopLeft,x1BottomRight);
       x2TopLeft.X:=x1TopLeft.X;
       x2TopLeft.Y:=x1TopLeft.Y;
       x2BottomRight.X:=x1BottomRight.X;
       x2BottomRight.Y:=x1BottomRight.Y;
  end;

  for i:=1 to AEntityList.Count-1 do
  begin
           AEntityList.Items[i].GetRectVertex(x1TopLeft,x1BottomRight);
           if x1TopLeft.X<x2TopLeft.X then x2TopLeft.X:=x1TopLeft.X;
           if x1TopLeft.Y>x2TopLeft.Y then x2TopLeft.Y:=x1TopLeft.Y;

           if x1BottomRight.X>x2BottomRight.x then x2BottomRight.X:=x1BottomRight.X;
           if x1BottomRight.Y<x2BottomRight.Y then x2BottomRight.Y:=x1BottomRight.Y;
  end;

  ATopLeft:=x2TopLeft;
  ABottomRight:=x2BottomRight;
end;

{ TBlockList }

function TBlockList.GetItem(Index: Integer): TBlockItem;
begin
   Result:=TBlockItem(inherited GetItem(Index));
end;

function TBlockList.GetBlock(Name: AnsiString): TBlockItem;
var
  i:integer;
  Item:TBlockItem;
begin
  Result:=nil;
  for i:=0 to Count-1 do
  begin
     Item:=TBlockItem(inherited GetItem(i));
     if Item.Name=Name then
     begin
          Result:=Item;
          break;
     end;
  end;
end;

procedure TBlockList.SetItem(Index: Integer; const Value: TBlockItem);
begin
   inherited SetItem(Index, Value);
end;

procedure TBlockList.SetBlock(Name: AnsiString; AValue: TBlockItem);
var
   i:integer;
   Item:TBlockItem;
begin
  for i:=0 to Count-1 do
  begin
     Item:=TBlockItem(inherited GetItem(i));
     if Item.Name=Name then
     begin
          inherited SetItem(i,AValue);
          break;
     end;
  end;
end;

constructor TBlockList.Create;
begin
  inherited Create;
  FOnGetDocumentEvent:=nil;
end;

function TBlockList.Add(AObject: TBlockItem): Integer;
begin
  Result:=inherited Add(AObject);
  AObject.FOnGetDocumentEvent:=FOnGetDocumentEvent;
end;

procedure TBlockList.Insert(Index: Integer; AObject: TBlockItem);
begin
  inherited Insert(Index, AObject);
  AObject.FOnGetDocumentEvent:=FOnGetDocumentEvent;
end;

{ TWorkSpaceCustom }

function TWorkSpaceCustom.GetDocument: TDrawDocumentCustom;
begin
  if Assigned(FOnGetDocumentEvent) then
      Result:=FOnGetDocumentEvent()
  else
      Result:=nil;
end;

constructor TWorkSpaceCustom.Create;
begin
    inherited Create;
    FOnGetDocumentEvent       :=nil;
    FSelectedEntityList       :=nil;
    IDIndex                   :=0;
    FEntityList               :=TEntityList.Create;
    FEntityList.ID            :=ENTITYLIST_ID;
    FEntityList.FModelSpace   :=TWorkSpace(Self);

    FBottomRight              :=SetNullToFloatPoint;
    FTopLeft                  :=SetNullToFloatPoint;
end;

destructor TWorkSpaceCustom.Destroy;
begin
     FSelectedEntityList:=nil;
     FEntityList.Free;
     inherited Destroy;
end;

procedure TWorkSpaceCustom.Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);
begin
     FEntityList.Repaint(Xshift,Yshift,AScale,LogicalDrawing,AStyle);
end;

procedure TWorkSpaceCustom.RepaintVertex(LogicalDrawing: TLogicalDraw);
begin
     FEntityList.RepaintVertex(LogicalDrawing);
end;

procedure TWorkSpaceCustom.DeselectAll;
begin
  if Assigned(ThisDocument) then
     ThisDocument.DeselectAll;
end;

function TWorkSpaceCustom.GetColor(AColor: TgaColor): TgaColor;
begin
      if AColor=gaByBlock then
      begin
          Result:=FByBlockColor;
      end
      else begin
          Result:=AColor;
      end;
end;

function TWorkSpaceCustom.GetLineWeight(LineWeight: TgaLineWeight): TgaLineWeight;
begin
      if (LineWeight=gaLnWtByBlock) then
      begin
          Result:=FByBlockLineWeight;
      end
      else begin
          Result:=LineWeight;
      end;
end;

procedure TWorkSpaceCustom.GetRectVertex(var ATopLeft, ABottomRight: TFloatPoint);
begin
  GetEntityListRectVertex(FEntityList,ATopLeft,ABottomRight);
end;

procedure TWorkSpaceCustom.MoveEntityGroup(AOwnerGroup: TEntityID; APoint: TFloatPoint);
var
   i,c:integer;
   dX,dY,dZ:Double;
   NewVertex: TFloatPoint;
begin
   dX:=0;
   dY:=0;
   dZ:=0;
   c:=0;
   for i:=0 to Objects.Count-1 do
   begin
      if Objects.Items[i].ID=AOwnerGroup then
      begin
         dX:=APoint.X-Objects.Items[i].VertexAxleX[0];
         dY:=APoint.Y-Objects.Items[i].VertexAxleY[0];
         dZ:=APoint.Z-Objects.Items[i].VertexAxleZ[0];
         Objects.Items[i].MoveVertex(0,APoint);
         inc(c);
         break;
      end;
   end;
   if c>0 then
   begin
   for i:=0 to Objects.Count-1 do
   begin
      if Objects.Items[i].GroupOwner=AOwnerGroup then
      begin
         NewVertex.X:=Objects.Items[i].VertexAxleX[0]+dX;
         NewVertex.Y:=Objects.Items[i].VertexAxleY[0]+dY;
         NewVertex.Z:=Objects.Items[i].VertexAxleZ[0]+dZ;
         Objects.Items[i].MoveVertex(0,NewVertex);
      end;
   end;
   end;
end;

{ TFloatPointList }

function TFloatPointList.GetCount: Integer;
begin
     Result:=Flist.Count;
end;

function TFloatPointList.GetPoint(Index: Integer): TFloatPoint;
begin
try
     Result:=TFloatPoint(PFloatPoint(FList.Items[Index])^);
except
     abort;
end;
end;

function TFloatPointList.NewPoint(X, Y, Z: Double): PFloatPoint;
var
  NPoint: PFloatPoint;
begin
  // Выделяем память под новую точку
  New(NPoint);
  NPoint^.X := X;
  NPoint^.Y := Y;
  NPoint^.Z := Z;
  Result    := NPoint;
end;

procedure TFloatPointList.SetPoint(Index: Integer; const Value: TFloatPoint);
begin
try
  PFloatPoint(FList.Items[Index])^:=Value;
except

end;
end;

constructor TFloatPointList.Create;
begin
  inherited Create;
  FList:=TList.Create;
end;

destructor TFloatPointList.Destroy;
var
  i: Integer;
begin
  // Перед уничтожением списка, освобождаем память
  for i := Count - 1 downto 0  do Delete(i);
  FList.Free;
  inherited Destroy;
end;

procedure TFloatPointList.Add(X, Y, Z: Double);
begin
     FList.Add(NewPoint(X,Y,Z));
end;

procedure TFloatPointList.Insert(Index: Integer; X, Y, Z: Double);
begin
  FList.Insert(Index,NewPoint(X,Y,Z));
end;

procedure TFloatPointList.Delete(Index: Integer);
begin
  Dispose(PFloatPoint(FList.items[index]));
  FList.Delete(Index);
end;

function TFloatPointList.Extract(Index: Integer): PFloatPoint;
var
  APoint: PFloatPoint;
begin
  APoint:=PFloatPoint(FList.items[index]);
  FList.Delete(Index);
  Result:=APoint;
end;


{ TEntityBasic }

{
procedure TEntityBasic.Change;
begin
     if Assigned(FOnChange)and(not FBlocked) then FOnChange(self);
end;
}

constructor TEntityBasic.Create;
begin
     inherited Create;
     inc(IDIndex);
     Randomize;
     sleep(1);
     //todo: Изменить алгоритм генерации номеров

     FID          :='GAC$'+IntToHex(random(255),2)+IntToHex(random(255),2)
                  +IntToHex(random(255),2)+IntToHex(IDIndex,12);

     FOnGetDocumentEvent :=nil;
     FState              :=[esCreating];
     FBlocked            :=false;
     FLineWeight         :=gaLnWtByBlock;
     FColor              :=gaByBlock;
     FTag                :=0;
     FData               :=nil;
end;

procedure TEntityBasic.Created;
begin
  FState:=[esNone];
end;

destructor TEntityBasic.Destroy;
begin
    inherited Destroy;
end;

procedure TEntityBasic.BeginUpdate;
begin
     FBlocked:=true;
end;

procedure TEntityBasic.EndUpdate;
begin
     FBlocked:=false;
end;

function TEntityBasic.GetColor(AColor: TgaColor): TgaColor;
begin
try
    if (AColor=gaByBlock)or(AColor=gaByLayer) then
    begin
      Result:=FParentList.FModelSpace.GetColor(AColor);
    end
    else begin
      Result:=AColor;
    end;

except
  // FParentList=nil
end;
end;

function TEntityBasic.GetLineWeight(ALineWeight: TgaLineWeight): TgaLineWeight;
begin
try
    if (ALineWeight=gaLnWtByLayer)or(ALineWeight=gaLnWtByLwDefault)or(ALineWeight=gaLnWtByBlock) then
    begin
      Result:=FParentList.FModelSpace.GetLineWeight(ALineWeight);
    end
    else begin
      Result:=ALineWeight;
    end;
except
   // FParentList=nil
end;
end;

function TEntityBasic.GetColor: TgaColor;
begin
  Result:=FColor;
end;

function TEntityBasic.GetLineWeight: TgaLineWeight;
begin
  Result:=FLineWeight;
end;

{ TEntity }

procedure TEntity.AddVertex(X, Y, Z: Double);
begin
  FVertex.Add(x,y,z);
  if assigned(FParentList) then
  FParentList.ChangeCordVertex(FloatPoint(x,y,z));
end;

constructor TEntity.Create;
begin
     inherited Create;
     FVertex   :=TFloatPointList.Create;
end;

procedure TEntity.DeleteVertex(Index: Integer);
begin
  FVertex.Delete(Index);
end;

destructor TEntity.Destroy;
begin
  FVertex.Free;
  inherited Destroy;
end;
{
function TEntity.GetMinMaxPoint: TMinMaxPoint;
var
  i:integer;
  tmpVar2:TMinMaxPoint;
begin

  tmpVar2.Xmin:=0;
  tmpVar2.Xmax:=0;
  tmpVar2.Zmin:=0;
  tmpVar2.Zmax:=0;
  tmpVar2.Ymin:=0;
  tmpVar2.Ymax:=0;
  for I := 0 to VertexCount - 1 do
  begin

      if tmpVar2.Xmin>Vertex[i].X then tmpVar2.Xmin:=Vertex[i].X;
      if tmpVar2.Ymin>Vertex[i].Y then tmpVar2.Ymin:=Vertex[i].Y;
      if tmpVar2.Zmin>Vertex[i].Z then tmpVar2.Zmin:=Vertex[i].Z;

      if tmpVar2.Xmax<Vertex[i].X then tmpVar2.Xmax:=Vertex[i].X;
      if tmpVar2.Ymax<Vertex[i].Y then tmpVar2.Ymax:=Vertex[i].Y;
      if tmpVar2.Zmax<Vertex[i].Z then tmpVar2.Zmax:=Vertex[i].Z;
  end;
  Result:=tmpVar2;
end;
}

procedure TEntity.GetRectVertex(var ATopLeft, ABottomRight: TFloatPoint);
var
  i:integer;
  tmpTopLeft, tmpBottomRight: TFloatPoint;
begin
  tmpTopLeft:=SetNullToFloatPoint;
  tmpBottomRight:=SetNullToFloatPoint;
  for i:=0 to VertexCount-1 do
  begin
       if Vertex[i].X<tmpTopLeft.X then tmpTopLeft.X:=Vertex[i].X;
       if Vertex[i].Y>tmpTopLeft.Y then tmpTopLeft.Y:=Vertex[i].Y;
       if Vertex[i].X>tmpBottomRight.X then tmpBottomRight.X:=Vertex[i].X;
       if Vertex[i].Y<tmpBottomRight.Y then tmpBottomRight.Y:=Vertex[i].Y;
  end;
  ATopLeft:=tmpTopLeft;
  ABottomRight:=tmpBottomRight;
end;

function TEntity.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean; var MVertx: TModifyVertex): Integer;
var
  i,CountVertexInRect:integer;
begin
  SetDeltaToRectPoint(TopLeft, BottomRight, ThisDocument.GetDeltaVertex);

  Result:=AFFA_OUTSIDE; //Вне периметра
  CountVertexInRect:=0;
  for I := 0 to VertexCount - 1 do
  begin
      if PointIn2DRect(Vertex[i],TopLeft, BottomRight) then
      begin
        CountVertexInRect:=CountVertexInRect+1;
        MVertx.Item:=self;
        MVertx.VertexIndex:=i;
        MVertx.VertexPos:=Vertex[MVertx.VertexIndex];
      end;
  end;

  if (AllVertexInRect)and(CountVertexInRect=VertexCount) then
    Result:=AFFA_VERTEX
  else if (not AllVertexInRect)and(CountVertexInRect>0) then
    Result:=AFFA_VERTEX;
end;

function TEntity.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean): Integer;
var
  MVertx: TModifyVertex;
begin
  Result:=GetSelect(TopLeft, BottomRight,AllVertexInRect,MVertx);
end;

function TEntity.GetVertex(Index: Integer): TFloatPoint;
begin
  Result:=FVertex.Items[index];
end;

function TEntity.GetVertexAxleX(Index: Integer): Double;
begin
  Result:=Vertex[Index].X;
end;

function TEntity.GetVertexAxleY(Index: Integer): Double;
begin
  Result:=Vertex[Index].Y;
end;

function TEntity.GetVertexAxleZ(Index: Integer): Double;
begin
  Result:=Vertex[Index].Z;
end;

function TEntity.GetDocument: TDrawDocumentCustom;
begin
  if Assigned(FOnGetDocumentEvent) then
      Result:=FOnGetDocumentEvent()
  else
      Result:=nil;
end;

function TEntity.GetVertexCount: Integer;
begin
  Result:=FVertex.Count;
end;

procedure TEntity.InsertVertex(Index: Integer; X, Y, Z: Double);
begin
  FVertex.Insert(Index,X,Y,Z);
  if assigned(FParentList) then
  FParentList.ChangeCordVertex(FloatPoint(x,y,z));
end;

procedure TEntity.MoveVertex(Index:integer; NewVertex: TFloatPoint);
begin
        VertexAxleX[Index]:=NewVertex.X;
        VertexAxleY[Index]:=NewVertex.Y;
        VertexAxleZ[Index]:=NewVertex.Z;
end;

procedure TEntity.Repaint(LogicalDrawing: TLogicalDraw;
  Style: TEntityDrawStyle);
begin
  Repaint(0,0,1,LogicalDrawing,Style);
end;

procedure TEntity.SetVertex(Index: Integer; const Value: TFloatPoint);
begin
  FVertex.Items[Index]:=Value;
  if assigned(FParentList) then
  FParentList.ChangeCordVertex(FVertex.Items[Index]);
end;

procedure TEntity.SetVertexAxleX(Index: Integer; const Value: Double);
var
  A:TFloatPoint;
begin
  A:=FVertex.Items[Index];
  A.X:=Value;
  FVertex.Items[Index]:=A;
end;

procedure TEntity.SetVertexAxleY(Index: Integer; const Value: Double);
var
  A:TFloatPoint;
begin
  A:=FVertex.Items[Index];
  A.Y:=Value;
  FVertex.Items[Index]:=A;
end;

procedure TEntity.SetVertexAxleZ(Index: Integer; const Value: Double);
var
  A:TFloatPoint;
begin
  A:=FVertex.Items[Index];
  A.Z:=Value;
  FVertex.Items[Index]:=A;
end;

{ TGraphicLine }

procedure TGraphicLine.Draw(APoint1, APoint2: TFloatPoint);
begin
  if VertexCount=0 then
  begin
    AddVertex(APoint1.X,APoint1.Y,APoint1.Z);
    AddVertex(APoint2.X,APoint2.Y,APoint2.Z);
  end;
end;

procedure TGraphicLine.Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);
begin
  if (VertexCount>=2) then
  begin
      if AStyle=[edsSelected] then
        LogicalDrawing.SetStyleDraw(LINETYPE_SELECTED,GetLineWeight(FLineWeight),GetColor(FColor))
      else
        LogicalDrawing.SetStyleDraw(LINETYPE_SOLID,GetLineWeight(FLineWeight),GetColor(FColor));
      LogicalDrawing.LineDraw((Vertex[0].X*AScale)+Xshift,(Vertex[0].Y*AScale)+Yshift,(Vertex[1].X*AScale)+Xshift,(Vertex[1].Y*AScale)+Yshift);
  end;
end;

{ TGraphicPolyline }

procedure TGraphicPolyline.Draw(APoints: TPointsArray; AClosed: Boolean);
var
  i:integer;
begin
    for i:=0 to Length(APoints)-1 do
    begin
      AddVertex(APoints[i].X,APoints[i].Y,APoints[i].Z);
    end;

    if (AClosed)and(Length(APoints)>3)then
      FClosed:=true
    else
      FClosed:=false;
end;

procedure TGraphicPolyline.Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);
var
  i:integer;
  fpoint:TFloatPoint;
begin
  if VertexCount>1 then
  begin
      if AStyle=[edsSelected] then
        LogicalDrawing.SetStyleDraw(LINETYPE_SELECTED,GetLineWeight(FLineWeight),GetColor(FColor))
      else
        LogicalDrawing.SetStyleDraw(LINETYPE_SOLID,GetLineWeight(FLineWeight),GetColor(FColor));
    fpoint:=Vertex[0];
    for i:=1 to VertexCount-1 do
    begin
      LogicalDrawing.LineDraw((fpoint.X*AScale)+Xshift,(fpoint.Y*AScale)+Yshift,(Vertex[i].X*AScale)+Xshift,(Vertex[i].Y*AScale)+Yshift);
      fpoint:=Vertex[i];
    end;
    if (FClosed)and (VertexCount>2) then
      LogicalDrawing.LineDraw((fpoint.X*AScale)+Xshift,(fpoint.Y*AScale)+Yshift,(Vertex[0].X*AScale)+Xshift,(Vertex[0].Y*AScale)+Yshift);
  end;
end;

procedure TGraphicPolyline.RepaintVertex(LogicalDrawing: TLogicalDraw);
var
  i:integer;
begin
  if VertexCount>0 then
  begin
    for i:=0 to VertexCount-1 do
    begin
      LogicalDrawing.VertexDraw(Vertex[i].X,Vertex[i].Y,VERTEXMARKER_VERTEX);
    end;
  end;
end;

{ TEntityList }

procedure TEntityList.SetEntityLinkVar(AEntity: TEntity);
begin
  AEntity.ParentList:=Self;
  AEntity.FOnGetDocumentEvent:=FModelSpace.FOnGetDocumentEvent;
end;

function TEntityList.Add(AParentID: TEntityID): TEntity;
var
  AEntity:TEntity;
begin
  AEntity:=TEntity.Create;
  inherited Add(AEntity);
  SetEntityLinkVar(AEntity);
  Result:=AEntity;
end;

procedure TEntityList.Add(AEntity: TEntity);
begin
  inherited Add(AEntity);
  SetEntityLinkVar(AEntity);
end;

constructor TEntityList.Create;
begin
  inherited Create;
  //ResetMinMaxPoint;
end;

procedure TEntityList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

destructor TEntityList.Destroy;
var
  i:integer;
begin
  for I := Count - 1 downto 0 do
  begin
      Items[i].Free;
      inherited Delete(i);
  end;
  
  inherited Destroy;
end;

function TEntityList.GetCount: Integer;
begin
  result:= inherited Count;
end;

function TEntityList.GetItem(Index: Integer): TEntity;
begin
  Result:=TEntity(inherited Get(Index));
end;

procedure TEntityList.ChangeCordVertex(const AVertCord:TFloatPoint);
begin
      {
      if FModelSpace.FTopLeft.X>AVertCord.X then FModelSpace.FTopLeft.X:=AVertCord.X;
      if Data.Ymin>APoint.Y then Data.Ymin:=APoint.Y;
      if Data.Zmin>APoint.Z then Data.Zmin:=APoint.Z;

      if Data.Xmax<APoint.X then Data.Xmax:=APoint.X;
      if Data.Ymax<APoint.Y then Data.Ymax:=APoint.Y;
      if Data.Zmax<APoint.Z then Data.Zmax:=APoint.Z;
      }
end;

{
function TEntityList.GetMinMaxPoint: TMinMaxPoint;
var
  i:integer;
  tmpVar1,tmpVar2:TMinMaxPoint;
  EntityTopLeft,EntityBottomRight:TFloatPoint;
  GlbTopLeft,GlbBottomRight:TFloatPoint;
begin
  tmpVar2.Xmin:=0;
  tmpVar2.Xmax:=0;
  tmpVar2.Zmin:=0;
  tmpVar2.Zmax:=0;
  tmpVar2.Ymin:=0;
  tmpVar2.Ymax:=0;

  SetNullToFloatPoint(GlbTopLeft);
  SetNullToFloatPoint(GlbBottomRight);
  //if (FEntityMinMaxPoint.Xmin=0)and(FEntityMinMaxPoint.Xmax=0)and(FEntityMinMaxPoint.Ymin=0)and(FEntityMinMaxPoint.Ymax=0) then
  //begin
  for I := 0 to Count - 1 do
  begin
      Items[i].GetRectVertex(EntityTopLeft,EntityBottomRight);
      if EntityTopLeft.X<GlbTopLeft.X then GlbTopLeft.X:=EntityTopLeft.X;
      if EntityTopLeft.Y>GlbTopLeft.Y then GlbTopLeft.Y:=EntityTopLeft.Y;

      if EntityBottomRight.X>GlbBottomRight.X then GlbBottomRight.X:=EntityBottomRight.X;
      if EntityBottomRight.Y<GlbBottomRight.Y then GlbBottomRight.Y:=EntityBottomRight.Y;
      {
      tmpVar1:=Items[i].GetMinMaxPoint;
      if tmpVar2.Xmin>tmpVar1.Xmin then tmpVar2.Xmin:=tmpVar1.Xmin;
      if tmpVar2.Ymin>tmpVar1.Ymin then tmpVar2.Ymin:=tmpVar1.Ymin;
      if tmpVar2.Zmin>tmpVar1.Zmin then tmpVar2.Zmin:=tmpVar1.Zmin;

      if tmpVar2.Xmax<tmpVar1.Xmax then tmpVar2.Xmax:=tmpVar1.Xmax;
      if tmpVar2.Ymax<tmpVar1.Ymax then tmpVar2.Ymax:=tmpVar1.Ymax;
      if tmpVar2.Zmax<tmpVar1.Zmax then tmpVar2.Zmax:=tmpVar1.Zmax;
      }
  end;
  //FEntityMinMaxPoint:=tmpVar2;
  //end;
  FEntityMinMaxPoint.Xmax:=GlbBottomRight.X;
  FEntityMinMaxPoint.Xmin:=GlbTopLeft.X;
  FEntityMinMaxPoint.Ymax:=GlbTopLeft.Y;
  FEntityMinMaxPoint.Ymin:=GlbBottomRight.Y;
  Result:=FEntityMinMaxPoint;
end;
}
procedure TEntityList.Insert(Index: Integer; AEntity: TEntity);
begin
   inherited insert(Index,AEntity);
   SetEntityLinkVar(AEntity);
end;

procedure TEntityList.Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);
var
  i,index:integer;
  Item:TEntity;
begin
  for I := 0 to Count - 1 do
  begin
          if Assigned(FModelSpace.FSelectedEntityList) then
          begin
            Item:=Items[i];
            index:=FModelSpace.FSelectedEntityList.IndexOf(Item);
            if index>-1 then
             Item.Repaint(Xshift,Yshift,AScale,LogicalDrawing,[edsSelected])
            else
              Item.Repaint(Xshift,Yshift,AScale,LogicalDrawing,[edsNormal]);
          end
          else
            Items[i].Repaint(Xshift,Yshift,AScale,LogicalDrawing,[edsNormal]);
  end;
end;

procedure TEntityList.RepaintVertex(LogicalDrawing: TLogicalDraw);
var
  i:integer;
  Item:TEntity;
begin
  for I := 0 to Count - 1 do
  begin
          if Assigned(FModelSpace.FSelectedEntityList) then
          begin
            Item:=Items[i];
            if FModelSpace.FSelectedEntityList.IndexOf(Item)>-1 then
              Item.RepaintVertex(LogicalDrawing);
          end;
  end;
end;

function TEntityList.GetEntityByID(AID: TEntityID): TEntity;
var
  i:integer;
  Item:TEntity;
begin
  Result:=nil;
  for I := 0 to Count - 1 do
  begin
       Item:=Items[i];
       if Item.ID=AID then
       begin
           Result:=Item;
           break;
       end;
  end;
end;

{
procedure TEntityList.ResetMinMaxPoint;
begin
  FEntityMinMaxPoint.Xmin:=0;
  FEntityMinMaxPoint.Xmax:=0;
  FEntityMinMaxPoint.Zmin:=0;
  FEntityMinMaxPoint.Zmax:=0;
  FEntityMinMaxPoint.Ymin:=0;
  FEntityMinMaxPoint.Ymax:=0;
end;
}
procedure TEntityList.SetItem(Index: Integer; const Value: TEntity);
begin
   inherited put(Index,value);
   Value.ParentList:=Self;
end;
{
procedure TEntityList.SetMinMaxPoint(AParentID: TEntityID; const X, Y, Z: Double);
var
  p:TFloatPoint;
begin
  if AParentID=FID then
  begin
    p.X:=x;
    p.Y:=y;
    p.Z:=z;
    SetMinMaxPoint(p,FEntityMinMaxPoint);
  end;
end;
}
{ TGraphicElipse }

procedure TGraphicEllipse.Draw(ABasePoint: TFloatPoint; AAxleY, AAxleX,
  ARotate:integer);
begin
  if VertexCount=0 then
  AddVertex(ABasePoint.X,ABasePoint.Y,ABasePoint.Z);
  AxleY:=AAxleY;
  AxleX:=AAxleX;
  //FRotate:=ARotate;
end;

function TGraphicEllipse.GetAxleX: Double;
begin
  Result:=FAxleX;
end;

function TGraphicEllipse.GetAxleY: Double;
begin
  Result:=FAxleY;
end;

function TGraphicEllipse.GetDiameter: Double;
begin
  Result:=2*GetRadius;
end;

procedure TGraphicEllipse.SetAxleX(const Value: Double);
begin
  FAxleX:=Value;
  if VertexCount=1 then
  begin
      AddVertex(Vertex[0].X+FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X-FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X,Vertex[0].Y+FAxleY,0);
      AddVertex(Vertex[0].X,Vertex[0].Y-FAxleY,0);
  end
  else begin
      VertexAxleX[1]:=Vertex[0].X+FAxleX;
      VertexAxleY[1]:=Vertex[0].Y;
      VertexAxleX[2]:=Vertex[0].X-FAxleX;
      VertexAxleY[2]:=Vertex[0].Y;
      VertexAxleX[3]:=Vertex[0].X;
      VertexAxleY[3]:=Vertex[0].Y+FAxleY;
      VertexAxleX[4]:=Vertex[0].X;
      VertexAxleY[4]:=Vertex[0].Y-FAxleY;
  end;
end;

procedure TGraphicEllipse.SetAxleY(const Value: Double);
begin
  FAxleY:=Value;
  if VertexCount=1 then
  begin
      AddVertex(Vertex[0].X+FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X-FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X,Vertex[0].Y+FAxleY,0);
      AddVertex(Vertex[0].X,Vertex[0].Y-FAxleY,0);
  end
  else begin
      VertexAxleX[1]:=Vertex[0].X+FAxleX;
      VertexAxleY[1]:=Vertex[0].Y;
      VertexAxleX[2]:=Vertex[0].X-FAxleX;
      VertexAxleY[2]:=Vertex[0].Y;
      VertexAxleX[3]:=Vertex[0].X;
      VertexAxleY[3]:=Vertex[0].Y+FAxleY;
      VertexAxleX[4]:=Vertex[0].X;
      VertexAxleY[4]:=Vertex[0].Y-FAxleY;
  end;
end;

procedure TGraphicEllipse.SetDiameter(const Value: Double);
begin
  SetRadius(Value/2);
  if VertexCount =1 then
  begin
      AddVertex(Vertex[0].X+FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X-FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X,Vertex[0].Y+FAxleY,0);
      AddVertex(Vertex[0].X,Vertex[0].Y-FAxleY,0);
  end
  else begin
      VertexAxleX[1]:=Vertex[0].X+FAxleX;
      VertexAxleY[1]:=Vertex[0].Y;
      VertexAxleX[2]:=Vertex[0].X-FAxleX;
      VertexAxleY[2]:=Vertex[0].Y;
      VertexAxleX[3]:=Vertex[0].X;
      VertexAxleY[3]:=Vertex[0].Y+FAxleY;
      VertexAxleX[4]:=Vertex[0].X;
      VertexAxleY[4]:=Vertex[0].Y-FAxleY;
  end;
end;

function TGraphicEllipse.GetRadius: Double;
begin
  if FAxleY>=FAxleX then
      Result:=FAxleY
  else
      Result:=FAxleX;
end;

procedure TGraphicEllipse.SetRadius(const Value: Double);
begin
  FAxleX:=Value;
  FAxleY:=Value;
   if VertexCount=1 then
  begin
      AddVertex(Vertex[0].X+FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X-FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X,Vertex[0].Y+FAxleY,0);
      AddVertex(Vertex[0].X,Vertex[0].Y-FAxleY,0);
  end
  else begin
      VertexAxleX[1]:=Vertex[0].X+FAxleX;
      VertexAxleY[1]:=Vertex[0].Y;
      VertexAxleX[2]:=Vertex[0].X-FAxleX;
      VertexAxleY[2]:=Vertex[0].Y;
      VertexAxleX[3]:=Vertex[0].X;
      VertexAxleY[3]:=Vertex[0].Y+FAxleY;
      VertexAxleX[4]:=Vertex[0].X;
      VertexAxleY[4]:=Vertex[0].Y-FAxleY;
  end;
end;

procedure TGraphicEllipse.Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);
var
  i:integer;
  fpoint:TFloatPoint;
begin
  if VertexCount>0 then
  begin
      if AStyle=[edsSelected] then
        LogicalDrawing.SetStyleDraw(LINETYPE_SELECTED,GetLineWeight(FLineWeight),GetColor(FColor))
      else
        LogicalDrawing.SetStyleDraw(LINETYPE_SOLID,GetLineWeight(FLineWeight),GetColor(FColor));
    fpoint:=Vertex[0];
    LogicalDrawing.EllipseDraw((fpoint.X*AScale)+Xshift,(fpoint.Y*AScale)+Yshift,AxleX*AScale,AxleY*AScale);
  end;
end;

procedure TGraphicEllipse.RepaintVertex(LogicalDrawing: TLogicalDraw);
var
  i:integer;
begin
  if VertexCount>0 then
  begin
      LogicalDrawing.VertexDraw(Vertex[0].X,Vertex[0].Y,VERTEXMARKER_BASEPOINT);
      for i:=1 to VertexCount-1 do
      begin
        LogicalDrawing.VertexDraw(Vertex[i].X,Vertex[i].Y,VERTEXMARKER_VERTEX);
      end;
  end;
end;

{ TGraphicCircle }

procedure TGraphicCircle.Draw(ABasePoint: TFloatPoint; ARadius: Double);
begin
  if VertexCount=0 then
  AddVertex(ABasePoint.X,ABasePoint.Y,ABasePoint.Z);
  Radius:=ARadius;
end;

function TGraphicCircle.GetAxleX: Double;
begin
  Result:=GetRadius;
end;

function TGraphicCircle.GetAxleY: Double;
begin
  Result:=GetRadius;
end;

function TGraphicCircle.GetDiameter: Double;
begin
  Result:=FAxleX*2;
end;

function TGraphicCircle.GetRadius: Double;
begin
  Result:=FAxleX;
end;

procedure TGraphicCircle.SetAxleX(const Value: Double);
begin
  SetRadius(Value);
end;

procedure TGraphicCircle.SetAxleY(const Value: Double);
begin
  SetRadius(Value);
end;

procedure TGraphicCircle.SetDiameter(const Value: Double);
begin
  FAxleX:=Value/2;
  FAxleY:=Value/2;
  if VertexCount=1 then
  begin
      AddVertex(Vertex[0].X+FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X-FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X,Vertex[0].Y+FAxleY,0);
      AddVertex(Vertex[0].X,Vertex[0].Y-FAxleY,0);
  end
  else begin
      VertexAxleX[1]:=Vertex[0].X+FAxleX;
      VertexAxleY[1]:=Vertex[0].Y;
      VertexAxleX[2]:=Vertex[0].X-FAxleX;
      VertexAxleY[2]:=Vertex[0].Y;
      VertexAxleX[3]:=Vertex[0].X;
      VertexAxleY[3]:=Vertex[0].Y+FAxleY;
      VertexAxleX[4]:=Vertex[0].X;
      VertexAxleY[4]:=Vertex[0].Y-FAxleY;
  end;
end;

procedure TGraphicCircle.SetRadius(const Value: Double);
begin
  FAxleX:=Value;
  FAxleY:=Value;
  if VertexCount=1 then
  begin
      AddVertex(Vertex[0].X+FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X-FAxleX,Vertex[0].Y,0);
      AddVertex(Vertex[0].X,Vertex[0].Y+FAxleY,0);
      AddVertex(Vertex[0].X,Vertex[0].Y-FAxleY,0);
  end
  else begin
      VertexAxleX[1]:=Vertex[0].X+FAxleX;
      VertexAxleY[1]:=Vertex[0].Y;
      VertexAxleX[2]:=Vertex[0].X-FAxleX;
      VertexAxleY[2]:=Vertex[0].Y;
      VertexAxleX[3]:=Vertex[0].X;
      VertexAxleY[3]:=Vertex[0].Y+FAxleY;
      VertexAxleX[4]:=Vertex[0].X;
      VertexAxleY[4]:=Vertex[0].Y-FAxleY;
  end;
end;

procedure TGraphicCircle.Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);
var
  i:integer;
begin
  if VertexCount>=1 then
  begin
      if AStyle=[edsSelected] then
        LogicalDrawing.SetStyleDraw(LINETYPE_SELECTED,GetLineWeight(FLineWeight),GetColor(FColor))
      else
        LogicalDrawing.SetStyleDraw(LINETYPE_SOLID,GetLineWeight(FLineWeight),GetColor(FColor));
    LogicalDrawing.CircleDraw((Vertex[0].X*AScale)+Xshift,(Vertex[0].Y*AScale)+Yshift,GetRadius*AScale);
  end;
end;

procedure TGraphicCircle.RepaintVertex(LogicalDrawing: TLogicalDraw);
var
  i:integer;
begin
  if VertexCount>0 then
  begin
      LogicalDrawing.VertexDraw(Vertex[0].X,Vertex[0].Y,VERTEXMARKER_BASEPOINT);
      for i:=1 to VertexCount-1 do
      begin
        LogicalDrawing.VertexDraw(Vertex[i].X,Vertex[i].Y,VERTEXMARKER_VERTEX);
      end;
  end;
end;


{ TGraphicArc }

{
 Алгоритм работы дуги необходимо дорабатывать.
}

procedure TGraphicArc.Draw(ABasePoint, APoint1, APoint2: TFloatPoint; ARadius: Double);
begin

            AddVertex(0,0,0);
            AddVertex(0,0,0);
            AddVertex(0,0,0);
            //AddVertex(0,0,0);
            
            VertexAxleX[0]:=ABasePoint.X;
            VertexAxleY[0]:=ABasePoint.Y;
            VertexAxleZ[0]:=ABasePoint.Z;

            VertexAxleX[1]:=APoint1.X;
            VertexAxleY[1]:=APoint1.Y;
            VertexAxleZ[1]:=APoint1.Z;

            VertexAxleX[2]:=APoint2.X;
            VertexAxleY[2]:=APoint2.Y;
            VertexAxleZ[2]:=APoint2.Z;

            Radius:=ARadius;
end;

procedure TGraphicArc.Draw(APoint1, APoint2, APoint3: TFloatPoint);
begin
            abort;
            {
            VertexAxleX[0]:=ABasePoint.X;
            VertexAxleY[0]:=ABasePoint.Y;
            VertexAxleZ[0]:=ABasePoint.Z;

            VertexAxleX[1]:=APoint1.X;
            VertexAxleY[1]:=APoint1.Y;
            VertexAxleZ[1]:=APoint1.Z;

            VertexAxleX[2]:=APoint2.X;
            VertexAxleY[2]:=APoint2.Y;
            VertexAxleZ[2]:=APoint2.Z;

            VertexAxleX[3]:=ACenterPoint.X;
            VertexAxleY[3]:=ACenterPoint.Y;
            VertexAxleZ[3]:=ACenterPoint.Z;
            }
end;

function TGraphicArc.GetDiameter: Double;
begin
  Result:=FAxleX*2;
end;

function TGraphicArc.GetRadius: Double;
begin
  Result:=FAxleX;
end;

procedure TGraphicArc.MoveVertex(Index: integer; NewVertex: TFloatPoint);
var
  dX,dY,dZ:Double;
begin
        //todo
        dX:=NewVertex.X-Vertex[Index].X;
        dY:=NewVertex.Y-Vertex[Index].Y;
        dZ:=NewVertex.Z-Vertex[Index].Z;

        if Index=0 then
        begin
            VertexAxleX[Index]:=NewVertex.X;
            VertexAxleY[Index]:=NewVertex.Y;
            VertexAxleZ[Index]:=NewVertex.Z;

            VertexAxleX[1]:=VertexAxleX[1]+dX;
            VertexAxleY[1]:=VertexAxleY[1]+dY;
            VertexAxleZ[1]:=VertexAxleZ[1]+dZ;

            VertexAxleX[2]:=VertexAxleX[2]+dX;
            VertexAxleY[2]:=VertexAxleY[2]+dY;
            VertexAxleZ[2]:=VertexAxleZ[2]+dZ;
        end
        else if (Index=1) then
        begin
            VertexAxleX[1]:=VertexAxleX[1]+dX;
            VertexAxleY[1]:=VertexAxleY[1]+dY;
            VertexAxleZ[1]:=VertexAxleZ[1]+dZ;
        end
        else if (Index=2) then
        begin
            VertexAxleX[2]:=VertexAxleX[2]+dX;
            VertexAxleY[2]:=VertexAxleY[2]+dY;
            VertexAxleZ[2]:=VertexAxleZ[2]+dZ;
        end;
end;

procedure TGraphicArc.SetDiameter(const Value: Double);
begin
 FAxleX:=Value/2;
 FAxleY:=Value/2;
end;

procedure TGraphicArc.SetRadius(const Value: Double);
begin
 FAxleX:=Value;
 FAxleY:=Value;
end;

procedure TGraphicArc.Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);
begin
  if VertexCount>=3 then
  begin
      if AStyle=[edsSelected] then
        LogicalDrawing.SetStyleDraw(LINETYPE_SELECTED,GetLineWeight(FLineWeight),GetColor(FColor))
      else
        LogicalDrawing.SetStyleDraw(LINETYPE_SOLID,GetLineWeight(FLineWeight),GetColor(FColor));
    LogicalDrawing.ArcDraw((Vertex[0].X*AScale)+Xshift,(Vertex[0].Y*AScale)+Yshift,(Vertex[1].X*AScale)+Xshift,(Vertex[1].Y*AScale)+Yshift,(Vertex[2].X*AScale)+Xshift,(Vertex[2].Y*AScale)+Yshift,GetRadius*Ascale);
  end;
end;

procedure TGraphicArc.RepaintVertex(LogicalDrawing: TLogicalDraw);
begin
  if VertexCount>0 then
  begin
      LogicalDrawing.VertexDraw(Vertex[0].X,Vertex[0].Y,VERTEXMARKER_BASEPOINT);
      LogicalDrawing.VertexDraw(Vertex[1].X,Vertex[1].Y,VERTEXMARKER_VERTEX);
      LogicalDrawing.VertexDraw(Vertex[2].X,Vertex[2].Y,VERTEXMARKER_VERTEX);
  end;
end;


{ TEntityEllipseBasic }

function TEntityEllipseBasic.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean): Integer;
var
  MVertx: TModifyVertex;
begin
  Result:=GetSelect(TopLeft, BottomRight,AllVertexInRect,MVertx);
end;


function TEntityEllipseBasic.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean; var MVertx: TModifyVertex): Integer;
var
  i,CountVertexInRect :integer;
  APoint1,APoint2     :TFloatPoint;
  xq,yq,a             :Double;
begin

  Result:=AFFA_OUTSIDE; //Вне периметра

  // Проверка попадают ли вершины в зону выбора
  CountVertexInRect:=0;

  SetDeltaToRectPoint(TopLeft, BottomRight, ThisDocument.GetDeltaVertex);

  // Проверка попадает ли базовая точка в зону выбора
  if (VertexCount>0) then
  begin
      if PointIn2DRect(Vertex[0],TopLeft, BottomRight) then
      begin
        CountVertexInRect:=CountVertexInRect+1;
        MVertx.Item:=self;
        MVertx.VertexIndex:=0;
        MVertx.VertexPos:=Vertex[MVertx.VertexIndex];
      end;
      if (not AllVertexInRect)and(CountVertexInRect>0) then
      begin
        Result:=AFFA_BASEPOINT;
      end;
  end;

  // Проверка попадают ли вершины в зону выбора
  for I := 1 to VertexCount - 1 do
  begin
      if PointIn2DRect(Vertex[i],TopLeft, BottomRight) then
      begin
        CountVertexInRect:=CountVertexInRect+1;
        MVertx.Item:=self;
        MVertx.VertexIndex:=i;
        MVertx.VertexPos:=Vertex[MVertx.VertexIndex];
      end;
  end;

  if (AllVertexInRect)and(CountVertexInRect=VertexCount) then
  begin
    Result:=AFFA_VERTEX;
  end
  else if (not AllVertexInRect)and(CountVertexInRect>0) then
  begin
    Result:=AFFA_VERTEX;
  end;

  // Проверка попадают ли промежуточные точки в зону выбора

    if (not AllVertexInRect)and(VertexCount>=1)and(Result<>AFFA_VERTEX)and(Result<>AFFA_BASEPOINT) then
    begin
      for I := 0 to 180 do
      begin
        a:=i;
        APoint2:=APoint1;
        // уравнение эллипса с поворотом
        //xq := FAxleX*(0 - Vertex[0].x)*cos(a) - (0 - Vertex[0].y)*sin(a);
        //yq := FAxleY*(0 - Vertex[0].x)*sin(a) + (0 - Vertex[0].y)*cos(a);
        // уравнение эллипса
         xq:= Vertex[0].x+FAxleX*cos(a);
         yq:= Vertex[0].y+FAxleY*sin(a);
         APoint1.X:=xq;
         APoint1.Y:=yq;
        if PointInRect2D(xq,yq,TopLeft.X,TopLeft.Y,BottomRight.X,BottomRight.Y)then
        begin
            Result:=AFFA_BORDER;
            break;
        end;

        if i>0 then
        begin
          if isLinesHasIntersection(APoint1.X,APoint1.Y,APoint2.X,APoint2.Y,TopLeft.X,TopLeft.Y,BottomRight.X,BottomRight.Y) then
          begin
            Result:=AFFA_BORDER;
            break;
          end;
          if isLinesHasIntersection(APoint1.X,APoint1.Y,APoint2.X,APoint2.Y,BottomRight.X,TopLeft.Y,TopLeft.X,BottomRight.Y) then
          begin
            Result:=AFFA_BORDER;
            break;
          end;
        end;

      end; //for

      SetDeltaToRectPoint(TopLeft, BottomRight, ThisDocument.GetDeltaVertex*-1);

      // Проверка попадает ли зона выбора в периметр объекта
      if (Result=AFFA_OUTSIDE)and(not AllVertexInRect) then
      begin
          if (VertexCount>0) then
          begin
            APoint1.X:=Vertex[0].X-FAxleX;
            APoint1.Y:=Vertex[0].Y+FAxleY;
            APoint2.X:=Vertex[0].X+FAxleX;
            APoint2.Y:=Vertex[0].Y-FAxleY;
            if PointIn2DRect(TopLeft,APoint1, APoint2) then
              Result:=AFFA_INSIDE;
          end;
      end;

    end;
end;

procedure TEntityEllipseBasic.MoveVertex(Index:integer;  NewVertex: TFloatPoint);
var
  dX,dY,dZ:Double;
begin
        dX:=NewVertex.X-Vertex[Index].X;
        dY:=NewVertex.Y-Vertex[Index].Y;
        dZ:=NewVertex.Z-Vertex[Index].Z;

        if Index=0 then
        begin
            VertexAxleX[Index]:=NewVertex.X;
            VertexAxleY[Index]:=NewVertex.Y;
            VertexAxleZ[Index]:=NewVertex.Z;

            VertexAxleX[1]:=VertexAxleX[1]+dX;
            VertexAxleY[1]:=VertexAxleY[1]+dY;
            VertexAxleZ[1]:=VertexAxleZ[1]+dZ;
            
            VertexAxleX[2]:=VertexAxleX[2]+dX;
            VertexAxleY[2]:=VertexAxleY[2]+dY;
            VertexAxleZ[2]:=VertexAxleZ[2]+dZ;

            VertexAxleX[3]:=VertexAxleX[3]+dX;
            VertexAxleY[3]:=VertexAxleY[3]+dY;
            VertexAxleZ[3]:=VertexAxleZ[3]+dZ;

            VertexAxleX[4]:=VertexAxleX[4]+dX;
            VertexAxleY[4]:=VertexAxleY[4]+dY;
            VertexAxleZ[4]:=VertexAxleZ[4]+dZ;
        end
        else if (Index=1) then
        begin
            AxleX:=AxleX+dX;
        end
        else if (Index=2) then
        begin
            AxleX:=AxleX+dX*-1;
        end
        else if (Index=3) then
        begin
            AxleY:=AxleY+dY;
        end
        else if (Index=4) then
        begin
            AxleY:=AxleY+dY*-1;
        end;
end;

procedure TEntityEllipseBasic.SetBasePoint(const Value: TFloatPoint);
begin
  if VertexCount>0 then
  begin
    Vertex[0]:=Value;
  end
  else begin
    AddVertex(Value.X,Value.Y,Value.Z);
  end;
end;

procedure TEntityEllipseBasic.GetRectVertex(var ATopLeft,
  ABottomRight: TFloatPoint);
begin

  //inherited GetRectVertex(ATopLeft, ABottomRight);
  ATopLeft.X:=BasePoint.X-FAxleX;
  ATopLeft.Y:=BasePoint.Y+FAxleY;

  ABottomRight.X:=BasePoint.X+FAxleX;
  ABottomRight.Y:=BasePoint.Y-FAxleY;
end;

function TEntityEllipseBasic.GetBasePoint: TFloatPoint;
begin
  if VertexCount>0 then
  begin
    Result:=Vertex[0];
  end;
end;

{ TEntityTextBasic }

function TEntityTextBasic.GetBasePoint: TFloatPoint;
begin
  if VertexCount>1 then
  begin
    Result:=Vertex[0];
  end;
end;

procedure TEntityTextBasic.SetBasePoint(const Value: TFloatPoint);
begin
  if VertexCount>1 then
  begin
    Vertex[0]:=Value;
  end
  else begin
    AddVertex(Value.X,Value.Y,Value.Z);
  end;
end;

constructor TEntityTextBasic.Create;
begin
  inherited Create;
  FRotate:=0;
end;

function TEntityTextBasic.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean): Integer;
var
  MVertx: TModifyVertex;
begin
  Result:=GetSelect(TopLeft, BottomRight,AllVertexInRect,MVertx);
end;

function TEntityTextBasic.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean; var MVertx: TModifyVertex): Integer;
var
  i,CountVertexInRect :integer;
begin
  Result:=AFFA_OUTSIDE; //Вне периметра
  CountVertexInRect:=0;

  SetDeltaToRectPoint(TopLeft, BottomRight, ThisDocument.GetDeltaVertex);

      if PointIn2DRect(Vertex[0],TopLeft, BottomRight) then
      begin
        CountVertexInRect:=CountVertexInRect+1;
        MVertx.Item:=self;
        MVertx.VertexIndex:=0;
        MVertx.VertexPos:=Vertex[MVertx.VertexIndex];
      end;

  if (AllVertexInRect)and(CountVertexInRect=VertexCount) then
    Result:=AFFA_BASEPOINT
  else if (not AllVertexInRect)and(CountVertexInRect>0) then
    Result:=AFFA_BASEPOINT;
end;

procedure TEntityTextBasic.Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle: TEntityDrawStyle);
begin
  if VertexCount>0 then
  begin
      if AStyle=[edsSelected] then
        LogicalDrawing.SetStyleDraw(LINETYPE_SELECTED,GetLineWeight(FLineWeight),GetColor(FColor))
      else
        LogicalDrawing.SetStyleDraw(LINETYPE_SOLID,GetLineWeight(FLineWeight),GetColor(FColor));
      LogicalDrawing.PointDraw((Vertex[0].X*AScale)+Xshift,(Vertex[0].Y*AScale)+Yshift);
  end;
end;

procedure TEntityTextBasic.RepaintVertex(LogicalDrawing: TLogicalDraw);
begin
  if VertexCount>0 then
  begin
      LogicalDrawing.VertexDraw(Vertex[0].X,Vertex[0].Y,VERTEXMARKER_BASEPOINT);
  end;
end;

{ TEntityBlockBasic }

function TEntityBlockBasic.GetBasePoint: TFloatPoint;
begin
  if VertexCount>0 then
  begin
    Result:=Vertex[0];
  end;
end;

procedure TEntityBlockBasic.SetBasePoint(const Value: TFloatPoint);
begin
  if VertexCount>1 then
  begin
    Vertex[0]:=Value;
  end
  else begin
    AddVertex(Value.X,Value.Y,Value.Z);
  end;
end;

procedure TEntityBlockBasic.AddVertex(X, Y, Z: Double);
begin
  if VertexCount=0 then
      inherited AddVertex(X, Y, Z)
  else begin
      VertexAxleX[0]:=X;
      VertexAxleY[0]:=Y;
      VertexAxleZ[0]:=Z;
  end;
end;

constructor TEntityBlockBasic.Create;
begin
  inherited Create;
  FScale:=1;
end;

function TEntityBlockBasic.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean): Integer;
var
  MVertx: TModifyVertex;
begin
  Result:=GetSelect(TopLeft, BottomRight,AllVertexInRect,MVertx);
end;

function TEntityBlockBasic.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean; var MVertx: TModifyVertex): Integer;
var
  i,CountVertexInRect:integer;
  x1TopLeft,x1BottomRight: TFloatPoint;
begin
  x1TopLeft:=SetNullToFloatPoint;
  x1BottomRight:=SetNullToFloatPoint;
  GetRectVertex(x1TopLeft,x1BottomRight);

  Result:=AFFA_OUTSIDE; //Вне периметра
  CountVertexInRect:=0;

  if PointIn2DRect(TopLeft,x1TopLeft,x1BottomRight) and PointIn2DRect(BottomRight,x1TopLeft,x1BottomRight) then
  begin
      if not(AllVertexInRect)then
        Result:=AFFA_INSIDE;
  end;

  SetDeltaToRectPoint(TopLeft, BottomRight, ThisDocument.GetDeltaVertex);

  if PointIn2DRect(Vertex[0],TopLeft, BottomRight) then
  begin
       CountVertexInRect:=CountVertexInRect+1;
       MVertx.Item:=self;
       MVertx.VertexIndex:=0;
       MVertx.VertexPos:=Vertex[MVertx.VertexIndex];
  end;

  if (AllVertexInRect)and(CountVertexInRect=VertexCount) then
    Result:=AFFA_BASEPOINT
  else if (not AllVertexInRect)and(CountVertexInRect>0) then
    Result:=AFFA_BASEPOINT;

  if PointIn2DRect(x1TopLeft,TopLeft, BottomRight) and PointIn2DRect(x1BottomRight,TopLeft, BottomRight) then
  begin
      if (AllVertexInRect)then
        Result:=AFFA_VERTEX;
  end;

end;

procedure TEntityBlockBasic.GetRectVertex(var ATopLeft,
  ABottomRight: TFloatPoint);
var
   x2TopLeft,
   x2BottomRight  :TFloatPoint;
   i              :integer;
   BlkItem        :TBlockItem;
begin

  x2TopLeft:=SetNullToFloatPoint;
  x2BottomRight:=SetNullToFloatPoint;

  BlkItem:=ThisDocument.FBlockList.Block[FBlockID];
  BlkItem.GetRectVertex(x2TopLeft,x2BottomRight);

  x2TopLeft.X:=(x2TopLeft.X*FScale)+BasePoint.X;
  x2TopLeft.Y:=(x2TopLeft.Y*FScale)+BasePoint.Y;
  x2TopLeft.Z:=(x2TopLeft.Z*FScale)+BasePoint.Z;
  ATopLeft:=x2TopLeft;

  x2BottomRight.X:=(x2BottomRight.X*FScale)+BasePoint.X;
  x2BottomRight.Y:=(x2BottomRight.Y*FScale)+BasePoint.Y;
  x2BottomRight.Z:=(x2BottomRight.Z*FScale)+BasePoint.Z;
  ABottomRight:=x2BottomRight;
end;

procedure TEntityBlockBasic.InsertVertex(Index: Integer; X, Y, Z: Double);
begin
  if VertexCount=0 then
      inherited InsertVertex(Index, X, Y, Z)
  else begin
      VertexAxleX[0]:=X;
      VertexAxleY[0]:=Y;
      VertexAxleZ[0]:=Z;
  end;
end;

procedure TEntityBlockBasic.Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle: TEntityDrawStyle);
begin
  if VertexCount>0 then
  begin
      if AStyle=[edsSelected] then
        LogicalDrawing.SetStyleDraw(LINETYPE_SELECTED,GetLineWeight(FLineWeight),GetColor(FColor))
      else
        LogicalDrawing.SetStyleDraw(LINETYPE_SOLID,GetLineWeight(FLineWeight),GetColor(FColor));
      LogicalDrawing.PointDraw((Vertex[0].X*AScale)+Xshift,(Vertex[0].Y*AScale)+Yshift);
  end;
end;

procedure TEntityBlockBasic.RepaintVertex(LogicalDrawing: TLogicalDraw);
begin
  if VertexCount>0 then
  begin
      LogicalDrawing.VertexDraw(Vertex[0].X,Vertex[0].Y,VERTEXMARKER_BASEPOINT);
  end;
end;

{ TGraphicBlock }

procedure TGraphicBlock.Draw(ABasePoint:TFloatPoint; ABlockID:string; AScale,ARotate:integer);
begin
   if VertexCount=0 then
   AddVertex(ABasePoint.X,ABasePoint.Y,ABasePoint.Z);
   BlockID:=ABlockID;
   Scale:=AScale;
   //Rotate:=ARotate;
end;

procedure TGraphicBlock.Repaint(Xshift,Yshift,AScale:Double;
  LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);
var
  i:integer;
  tmpByBlockColor   :TgaColor;
  tmpByBlockLineWeight   :TgaLineWeight;
  AK,BK: TFloatPoint;
  BlkItem        :TBlockItem;
begin
  if VertexCount>0 then
  begin
      BlkItem                :=ThisDocument.FBlockList.Block[FBlockID];
      tmpByBlockColor        :=BlkItem.FByBlockColor;;
      tmpByBlockLineWeight   :=BlkItem.FByBlockLineWeight;

      BlkItem.FByBlockColor  :=FColor;
      BlkItem.FByBlockLineWeight:=FLineWeight;

      if AStyle=[edsSelected] then
        LogicalDrawing.SetStyleDraw(LINETYPE_SELECTED,
                                  GetLineWeight(FLineWeight),GetColor(FColor))
      else
        LogicalDrawing.SetStyleDraw(LINETYPE_SOLID,
                                  GetLineWeight(FLineWeight),GetColor(FColor));

      LogicalDrawing.PointDraw((Vertex[0].X*AScale)+Xshift,
                                 (Vertex[0].Y*AScale)+Yshift);

      // GetRectVertex testing
      if LogicalDrawing.Develop then
      begin
        GetRectVertex(Ak,BK);
        LogicalDrawing.SetStyleDraw(LINETYPE_SOLID,1,5);
        LogicalDrawing.RectangelDraw((Ak.X*AScale)+Xshift,
               (Ak.Y*AScale)+Yshift,(Bk.X*AScale)+Xshift,(Bk.Y*AScale)+Yshift);
      end;

      BlkItem.Repaint(Vertex[0].X*AScale+Xshift,Vertex[0].Y*AScale+Yshift,
                       FScale*AScale,LogicalDrawing,AStyle);

      BlkItem.FByBlockColor     :=tmpByBlockColor;
      BlkItem.FByBlockLineWeight:=tmpByBlockLineWeight;
  end;
end;

procedure TGraphicBlock.RepaintVertex(LogicalDrawing: TLogicalDraw);
begin
    inherited RepaintVertex(LogicalDrawing);
end;

{ TGraphicText }

constructor TGraphicText.Create;
begin
  inherited Create;
end;

destructor TGraphicText.Destroy;
begin
  inherited Destroy;
end;

procedure TGraphicText.Draw(ABasePoint: TFloatPoint; AText: String;
  AAlign: TgaAttachmentPoint; ARotate: integer);
begin
  if VertexCount=0 then
  AddVertex(ABasePoint.X,ABasePoint.Y,ABasePoint.Z);
  Align:=AAlign;
  FontSize:=2.5;
  FontStyle:=[];
  FontName:='Arial';
  Width:=0;
  Height:=0;
  Rotate:=ARotate;
  Text:=AText;
end;

procedure TGraphicText.Draw(ABasePoint: TFloatPoint; AText: String;
  AAlign: TgaAttachmentPoint; AWidth, AHeight, ARotate: integer);
begin
  if VertexCount=0 then
  AddVertex(ABasePoint.X,ABasePoint.Y,ABasePoint.Z);
  Align:=AAlign;
  FontSize:=2.5;
  FontStyle:=[];
  FontName:='Arial';
  Width:=AWidth;
  Height:=AHeight;
  Rotate:=ARotate;
  Text:=AText;
end;

function TGraphicText.GetHeight: Double;
begin
  Result:=FHeight;
end;

function TGraphicText.GetWidth: Double;
begin
  Result:=FWidth;
end;

procedure TGraphicText.MoveVertex(Index: integer; NewVertex: TFloatPoint);
var
  dX,dY,dZ:Double;
begin
        dX:=NewVertex.X-Vertex[Index].X;
        dY:=NewVertex.Y-Vertex[Index].Y;
        dZ:=NewVertex.Z-Vertex[Index].Z;

        if CordEqualIn2D(Vertex[0],Vertex[Index]) then index:=0;


        if Index=0 then
        begin
            VertexAxleX[Index]:=NewVertex.X;
            VertexAxleY[Index]:=NewVertex.Y;
            VertexAxleZ[Index]:=NewVertex.Z;

            if VertexCount>=4 then
            begin
            VertexAxleX[1]:=VertexAxleX[1]+dX;
            VertexAxleY[1]:=VertexAxleY[1]+dY;
            VertexAxleZ[1]:=VertexAxleZ[1]+dZ;

            VertexAxleX[2]:=VertexAxleX[2]+dX;
            VertexAxleY[2]:=VertexAxleY[2]+dY;
            VertexAxleZ[2]:=VertexAxleZ[2]+dZ;

            VertexAxleX[3]:=VertexAxleX[3]+dX;
            VertexAxleY[3]:=VertexAxleY[3]+dY;
            VertexAxleZ[3]:=VertexAxleZ[3]+dZ;

            VertexAxleX[4]:=VertexAxleX[4]+dX;
            VertexAxleY[4]:=VertexAxleY[4]+dY;
            VertexAxleZ[4]:=VertexAxleZ[4]+dZ;
            end;
        end
        else if (Index=1) then
        begin
            //todo
        end
        else if (Index=2) then
        begin
            //todo
        end
        else if (Index=3) then
        begin
            //todo
        end
        else if (Index=4) then
        begin
            //todo
        end;
end;

procedure TGraphicText.GetRectVertex(var ATopLeft, ABottomRight: TFloatPoint);
var
   X0, Y0: Double;
begin
  //inherited GetRectVertex(ATopLeft, ABottomRight);
   X0:=BasePoint.X;
   Y0:=BasePoint.Y;
   if (FWidth<=0)and(FHeight<=0) then
   begin
      {W:=self.ParentList..TextWidth(FText);
      H:=FVirtualCanvas.TextHeight(FText);

      case FAlign of
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
      end; }
   end
   else begin
      case FAlign of
      gaAttachmentPointTopLeft:
      begin
          ATopLeft.X:=X0;
          ATopLeft.Y:=Y0;
          ABottomRight.X:=X0+FWidth;
          ABottomRight.Y:=Y0-FHeight;
      end;
      gaAttachmentPointTopCenter:
      begin
          ATopLeft.X:=X0-FWidth/2;
          ATopLeft.Y:=Y0;
          ABottomRight.X:=X0+FWidth/2;
          ABottomRight.Y:=Y0-FHeight;
      end;
      gaAttachmentPointTopRight:
      begin
          ATopLeft.X:=X0-FWidth;
          ATopLeft.Y:=Y0;
          ABottomRight.X:=X0;
          ABottomRight.Y:=Y0-FHeight;
      end;
      gaAttachmentPointMiddleLeft:
      begin
          ATopLeft.X:=X0;
          ATopLeft.Y:=Y0+FHeight/2;
          ABottomRight.X:=X0+FWidth;
          ABottomRight.Y:=Y0-FHeight/2;
      end;
      gaAttachmentPointMiddleCenter:
      begin
          ATopLeft.X:=X0-FWidth/2;
          ATopLeft.Y:=Y0+FHeight/2;
          ABottomRight.X:=X0+FWidth/2;
          ABottomRight.Y:=Y0-FHeight/2;
      end;
      gaAttachmentPointMiddleRight:
      begin
          ATopLeft.X:=X0-FWidth;
          ATopLeft.Y:=Y0+FHeight/2;
          ABottomRight.X:=X0;
          ABottomRight.Y:=Y0-FHeight/2;
      end;
      gaAttachmentPointBottomLeft:
      begin
          ATopLeft.X:=X0;
          ATopLeft.Y:=Y0+FHeight;
          ABottomRight.X:=X0+FWidth;
          ABottomRight.Y:=Y0;
      end;
      gaAttachmentPointBottomCenter:
      begin
          ATopLeft.X:=X0-FWidth/2;
          ATopLeft.Y:=Y0+FHeight;
          ABottomRight.X:=X0+FWidth/2;
          ABottomRight.Y:=Y0;
      end;
      gaAttachmentPointBottomRight:
      begin
          ATopLeft.X:=X0-FWidth;
          ATopLeft.Y:=Y0+FHeight;
          ABottomRight.X:=X0;
          ABottomRight.Y:=Y0;
      end;
      end;

   end;
end;

procedure TGraphicText.SetHeight(const Value: Double);
var
  TopLeftTextRect,BottomRightTextRect: TFloatPoint;
begin
  FHeight:=Value;
  if VertexCount=1 then
  begin
      GetRectCord(FAlign,Vertex[0].X,Vertex[0].Y,FWidth,FHeight,TopLeftTextRect,BottomRightTextRect);
      AddVertex(TopLeftTextRect.X,TopLeftTextRect.Y,0);
      AddVertex(BottomRightTextRect.X,TopLeftTextRect.Y,0);
      AddVertex(BottomRightTextRect.X,BottomRightTextRect.Y,0);
      AddVertex(TopLeftTextRect.X,BottomRightTextRect.Y,0);
  end
  else begin
      GetRectCord(FAlign,Vertex[0].X,Vertex[0].Y,FWidth,FHeight,TopLeftTextRect,BottomRightTextRect);
      VertexAxleX[1]:=TopLeftTextRect.X;
      VertexAxleY[1]:=TopLeftTextRect.Y;
      VertexAxleX[2]:=BottomRightTextRect.X;
      VertexAxleY[2]:=TopLeftTextRect.Y;
      VertexAxleX[3]:=BottomRightTextRect.X;
      VertexAxleY[3]:=BottomRightTextRect.Y;
      VertexAxleX[4]:=TopLeftTextRect.X;
      VertexAxleY[4]:=BottomRightTextRect.Y;
  end;
end;

procedure TGraphicText.SetWidth(const Value: Double);
var
  TopLeftTextRect,BottomRightTextRect: TFloatPoint;
begin
  TopLeftTextRect:=SetNullToFloatPoint;
  BottomRightTextRect:=SetNullToFloatPoint;
  FWidth:=Value;
  if VertexCount=1 then
  begin
      GetRectCord(FAlign,Vertex[0].X,Vertex[0].Y,FWidth,FHeight,TopLeftTextRect,BottomRightTextRect);
      AddVertex(TopLeftTextRect.X,TopLeftTextRect.Y,0);
      AddVertex(BottomRightTextRect.X,TopLeftTextRect.Y,0);
      AddVertex(BottomRightTextRect.X,BottomRightTextRect.Y,0);
      AddVertex(TopLeftTextRect.X,BottomRightTextRect.Y,0);
  end
  else begin
      GetRectCord(FAlign,Vertex[0].X,Vertex[0].Y,FWidth,FHeight,TopLeftTextRect,BottomRightTextRect);
      VertexAxleX[1]:=TopLeftTextRect.X;
      VertexAxleY[1]:=TopLeftTextRect.Y;
      VertexAxleX[2]:=BottomRightTextRect.X;
      VertexAxleY[2]:=TopLeftTextRect.Y;
      VertexAxleX[3]:=BottomRightTextRect.X;
      VertexAxleY[3]:=BottomRightTextRect.Y;
      VertexAxleX[4]:=TopLeftTextRect.X;
      VertexAxleY[4]:=BottomRightTextRect.Y;
  end;
end;

function TGraphicText.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean): Integer;
var
  MVertx: TModifyVertex;
begin
  Result:=GetSelect(TopLeft, BottomRight, AllVertexInRect, MVertx);
end;

function TGraphicText.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean; var MVertx: TModifyVertex): Integer;
var
  i,CountVertexInRect:integer;
  APoint,APoint1,APoint2,TopLeftTextRect,BottomRightTextRect: TFloatPoint;
begin

  Result:=AFFA_OUTSIDE; //Вне периметра

  // Проверка попадают ли вершины в зону выбора
  CountVertexInRect:=0;

  SetDeltaToRectPoint(TopLeft, BottomRight, ThisDocument.GetDeltaVertex);

  // Проверка попадает ли базовая точка в зону выбора
  if (VertexCount>0) then
  begin
      if PointIn2DRect(Vertex[0],TopLeft, BottomRight) then
      begin
        CountVertexInRect:=CountVertexInRect+1;
        MVertx.Item:=self;
        MVertx.VertexIndex:=0;
        MVertx.VertexPos:=Vertex[MVertx.VertexIndex];
      end;
      if (not AllVertexInRect)and(CountVertexInRect>0) then
      begin
        Result:=AFFA_BASEPOINT;
      end;
  end;

  // Проверка попадают ли вершины в зону выбора
  for I := 1 to VertexCount - 1 do
  begin
      if PointIn2DRect(Vertex[i],TopLeft, BottomRight) then
      begin
        CountVertexInRect:=CountVertexInRect+1;
        MVertx.Item:=self;
        MVertx.VertexIndex:=i;
        MVertx.VertexPos:=Vertex[MVertx.VertexIndex];
      end;
  end;

  if (AllVertexInRect)and(CountVertexInRect=VertexCount) then
  begin
    Result:=AFFA_VERTEX;
  end
  else if (not AllVertexInRect)and(CountVertexInRect>0) then
  begin
    Result:=AFFA_VERTEX;
  end;

  // Проверка попадают ли промежуточные точки в зону выбора

    if (not AllVertexInRect)and(VertexCount>=1)and(Result<>AFFA_VERTEX)and(Result<>AFFA_BASEPOINT) then
    begin

    APoint:=Vertex[1];
    for I := 2 to VertexCount - 1 do
    begin
      //AC
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,TopLeft.X,TopLeft.Y,BottomRight.X,BottomRight.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      //BD
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,BottomRight.X,TopLeft.Y,TopLeft.X,BottomRight.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      //AB
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,TopLeft.X,TopLeft.Y,BottomRight.X,TopLeft.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      //BC
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,BottomRight.X,TopLeft.Y,BottomRight.X,BottomRight.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      //CD
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,BottomRight.X,BottomRight.Y,TopLeft.X,BottomRight.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      //DA
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,TopLeft.X,BottomRight.Y,TopLeft.X,TopLeft.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      APoint:=Vertex[i];
    end; //for

      SetDeltaToRectPoint(TopLeft, BottomRight, ThisDocument.GetDeltaVertex*-1);

      // Проверка попадает ли зона выбора в периметр объекта
      if (Result=AFFA_OUTSIDE)and(not AllVertexInRect) then
      begin
          if (FWidth>0)and(FHeight>0)and(VertexCount>0) then
          begin
            GetRectCord(FAlign,Vertex[0].X,Vertex[0].Y,FWidth,FHeight,TopLeftTextRect,BottomRightTextRect);
            if PointIn2DRect(TopLeft,TopLeftTextRect, BottomRightTextRect) then
              Result:=AFFA_INSIDE;
          end;
      end;

    end;
end;

procedure TGraphicText.Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle:TEntityDrawStyle);
begin
  if VertexCount>=1 then
  begin
      if edsSelected in AStyle then
        LogicalDrawing.SetStyleDraw(LINETYPE_SELECTED,GetLineWeight(FLineWeight),GetColor(FColor))
      else
        LogicalDrawing.SetStyleDraw(LINETYPE_SOLID,GetLineWeight(FLineWeight),GetColor(FColor));

    LogicalDrawing.SetFontStyleDraw(FFontName, FFontSize*AScale, FFontStyle);
    LogicalDrawing.TextDraw((Vertex[0].X*AScale)+Xshift,(Vertex[0].Y*AScale)+Yshift, FWidth*Ascale, FHeight*Ascale, FRotate, FText, FAlign);

    if edsSelected in AStyle then
    begin
      if (FWidth>0)and(FHeight>0) then
      begin
        //todo: draw text on selected todo
      end
      else begin
        //LogicalDrawing.GetTextWidth(FText); //todo:
        //LogicalDrawing.GetTextHeight(FText); //todo:
      end;
      //LogicalDrawing.LineDraw(TopLeftPointWCS.X,TopLeftPointWCS.Y,BottomRightPointWCS.X,TopLeftPointWCS.Y);
      //LogicalDrawing.LineDraw(TopLeftPointWCS.X,TopLeftPointWCS.Y,TopLeftPointWCS.X,BottomRightPointWCS.Y);
      //LogicalDrawing.LineDraw(TopLeftPointWCS.X,BottomRightPointWCS.Y,BottomRightPointWCS.X,BottomRightPointWCS.Y);
      //LogicalDrawing.LineDraw(BottomRightPointWCS.X,TopLeftPointWCS.Y,BottomRightPointWCS.X,BottomRightPointWCS.Y);
    end;
  end;
end;


procedure TGraphicText.RepaintVertex(LogicalDrawing: TLogicalDraw);
var
  i:integer;
begin
    LogicalDrawing.VertexDraw(Vertex[0].X,Vertex[0].Y,VERTEXMARKER_BASEPOINT);
    for i:=1 to VertexCount-1 do
    begin
      LogicalDrawing.VertexDraw(Vertex[i].X,Vertex[i].Y,VERTEXMARKER_VERTEX);
    end;
end;

{ TEntityLineBasic }

function TEntityLineBasic.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean; var MVertx: TModifyVertex): Integer;
var
  i,CountVertexInRect:integer;
  APoint: TFloatPoint;
begin

  Result:=AFFA_OUTSIDE; //Вне периметра

  SetDeltaToRectPoint(TopLeft, BottomRight, ThisDocument.GetDeltaVertex);

  // Проверка попадают ли вершины в зону выбора
  CountVertexInRect:=0;
  for I := 0 to VertexCount - 1 do
  begin
      if PointIn2DRect(Vertex[i],TopLeft, BottomRight) then
      begin
        CountVertexInRect:=CountVertexInRect+1;
        MVertx.Item:=self;
        MVertx.VertexIndex:=i;
        MVertx.VertexPos:=Vertex[MVertx.VertexIndex];
      end;
  end;

  if (AllVertexInRect)and(CountVertexInRect=VertexCount) then
  begin
    Result:=AFFA_VERTEX;
  end
  else if (not AllVertexInRect)and(CountVertexInRect>0) then
  begin
    Result:=AFFA_VERTEX;
  end;

  // Проверка попадают ли промежуточные точки в зону выбора
    //ABCD
    if (not AllVertexInRect)and(VertexCount>1)and(Result<>AFFA_VERTEX) then
    begin
    APoint:=Vertex[0];
    for I := 1 to VertexCount - 1 do
    begin
      //AC
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,TopLeft.X,TopLeft.Y,BottomRight.X,BottomRight.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      //BD
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,BottomRight.X,TopLeft.Y,TopLeft.X,BottomRight.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      //AB
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,TopLeft.X,TopLeft.Y,BottomRight.X,TopLeft.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      //BC
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,BottomRight.X,TopLeft.Y,BottomRight.X,BottomRight.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      //CD
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,BottomRight.X,BottomRight.Y,TopLeft.X,BottomRight.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      //DA
      if isLinesHasIntersection(APoint.X,APoint.Y,Vertex[i].X,Vertex[i].Y,TopLeft.X,BottomRight.Y,TopLeft.X,TopLeft.Y) then
      begin
        Result:=AFFA_BORDER;
        break;
      end;
      APoint:=Vertex[i];
    end;

    end;

end;

function TEntityLineBasic.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean): Integer;
var
  MVertx: TModifyVertex;
begin
  Result:=GetSelect(TopLeft, BottomRight,AllVertexInRect,MVertx);
end;

procedure TEntityLineBasic.RepaintVertex(LogicalDrawing: TLogicalDraw);
var
  i:integer;
begin
  if VertexCount>0 then
  begin
    for i:=0 to VertexCount-1 do
    begin
      LogicalDrawing.VertexDraw(Vertex[i].X,Vertex[i].Y,VERTEXMARKER_VERTEX);
    end;
  end;
end;

{ TGraphicPoint }

procedure TGraphicPoint.AddVertex(X, Y, Z: Double);
begin
  if VertexCount=0 then
      inherited AddVertex(X, Y, Z)
  else begin
      VertexAxleX[0]:=X;
      VertexAxleY[0]:=Y;
      VertexAxleZ[0]:=Z;
  end;
end;

procedure TGraphicPoint.Draw(APoint: TFloatPoint);
begin
   if VertexCount=0 then
   AddVertex(APoint.X,APoint.Y,APoint.Z);
end;

function TGraphicPoint.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean): Integer;
var
  MVertx: TModifyVertex;
begin
  Result:=GetSelect(TopLeft, BottomRight,AllVertexInRect,MVertx);
end;

function TGraphicPoint.GetSelect(TopLeft, BottomRight: TFloatPoint;
  AllVertexInRect: Boolean; var MVertx: TModifyVertex): Integer;
var
  CountVertexInRect:integer;
begin

  Result:=AFFA_OUTSIDE; //Вне периметра
  CountVertexInRect:=0;

  SetDeltaToRectPoint(TopLeft, BottomRight, ThisDocument.GetDeltaVertex);

      if PointIn2DRect(Vertex[0],TopLeft, BottomRight) then
      begin
        CountVertexInRect:=CountVertexInRect+1;
        MVertx.Item:=self;
        MVertx.VertexIndex:=0;
        MVertx.VertexPos:=Vertex[MVertx.VertexIndex];
      end;

  if (AllVertexInRect)and(CountVertexInRect=VertexCount) then
    Result:=AFFA_BASEPOINT
  else if (not AllVertexInRect)and(CountVertexInRect>0) then
    Result:=AFFA_BASEPOINT;
end;

procedure TGraphicPoint.InsertVertex(Index: Integer; X, Y, Z: Double);
begin
  if VertexCount=0 then
      inherited InsertVertex(Index, X, Y, Z)
  else begin
      VertexAxleX[0]:=X;
      VertexAxleY[0]:=Y;
      VertexAxleZ[0]:=Z;
  end;
end;

procedure TGraphicPoint.Repaint(Xshift,Yshift,AScale:Double; LogicalDrawing: TLogicalDraw; AStyle: TEntityDrawStyle);
begin
  if VertexCount>0 then
  begin
      if AStyle=[edsSelected] then
        LogicalDrawing.SetStyleDraw(LINETYPE_SELECTED,GetLineWeight(FLineWeight),GetColor(FColor))
      else
        LogicalDrawing.SetStyleDraw(LINETYPE_SOLID,GetLineWeight(FLineWeight),GetColor(FColor));
      LogicalDrawing.PointDraw((Vertex[0].X*AScale)+Xshift,(Vertex[0].Y*AScale)+Yshift);
  end;
end;

procedure TGraphicPoint.RepaintVertex(LogicalDrawing: TLogicalDraw);
begin
  if VertexCount>0 then
  begin
      LogicalDrawing.VertexDraw(Vertex[0].X,Vertex[0].Y,VERTEXMARKER_BASEPOINT);
  end;
end;

end.

