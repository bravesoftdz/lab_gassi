unit u_gassi_const;

//************************************************************
//
//    Модуль компонента Graphic Assi Control
//    Copyright (c) 2013  Pichugin M.
//
//    Разработчик: Pichugin M. (e-mail: pichugin_m@mail.ru)
//
//************************************************************

interface

uses
   Classes;

    
// Constants for enum Color
type
  TgaColor = LongWord;
const
  gaByBlock  = $00000000;
  gaRed      = $00000001;
  gaYellow   = $00000002;
  gaGreen    = $00000003;
  gaCyan     = $00000004;
  gaBlue     = $00000005;
  gaMagenta  = $00000006;
  gaWhite    = $00000007;
  gaByLayer  = $00000100;

// Constants for enum AttachmentPoint
type
  TgaAttachmentPoint = LongWord;
const
  gaAttachmentPointTopLeft = $00000001;
  gaAttachmentPointTopCenter = $00000002;
  gaAttachmentPointTopRight = $00000003;
  gaAttachmentPointMiddleLeft = $00000004;
  gaAttachmentPointMiddleCenter = $00000005;
  gaAttachmentPointMiddleRight = $00000006;
  gaAttachmentPointBottomLeft = $00000007;
  gaAttachmentPointBottomCenter = $00000008;
  gaAttachmentPointBottomRight = $00000009;

// Constants for enum LineWeight
type
  TgaLineWeight = LongWord;
const
  gaLnWt000 = $00000000;
  gaLnWt005 = $00000005;
  gaLnWt009 = $00000009;
  gaLnWt013 = $0000000D;
  gaLnWt015 = $0000000F;
  gaLnWt018 = $00000012;
  gaLnWt020 = $00000014;
  gaLnWt025 = $00000019;
  gaLnWt030 = $0000001E;
  gaLnWt035 = $00000023;
  gaLnWt040 = $00000028;
  gaLnWt050 = $00000032;
  gaLnWt053 = $00000035;
  gaLnWt060 = $0000003C;
  gaLnWt070 = $00000046;
  gaLnWt080 = $00000050;
  gaLnWt090 = $0000005A;
  gaLnWt100 = $00000064;
  gaLnWt106 = $0000006A;
  gaLnWt120 = $00000078;
  gaLnWt140 = $0000008C;
  gaLnWt158 = $0000009E;
  gaLnWt200 = $000000C8;
  gaLnWt211 = $000000D3;
  gaLnWtByLayer = $FFFFFFFF;
  gaLnWtByBlock = $FFFFFFFE;
  gaLnWtByLwDefault = $FFFFFFFD;
 type
 
   //SI multiples for metre (m)
TPointUnit = (puDefault,puYoctometre,puZeptometre,puAttometre,
    puFemtometre,puPicometre,puNanometre,puMicrometre,puMillimetre,
    puCentimeter,puDecimetre,puMeter,puDecametre,puHectometre,puKilometre,
    puMegametre,puGigametre,puTerametre,puPetametre,puExametre,puZettametre,
    puYottametre);

const
   BLOCKLIST_ID='BLOCKLISTID';
   ENTITYLIST_ID='';

implementation

end.

