{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 charcter encoding unit                  }
{                                                         }
{            Originally written by EgonHugeist            }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZEncoding;

interface

{$I ZCore.inc}

uses
  SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFDEF FPC}
    {$IFDEF UNIX}unixcp,{$ELSE}{$IFNDEF MSWINDOWS}cwstring,{$ENDIF}{$ENDIF}
  {$ENDIF}
  {$IFDEF WITH_LCONVENCODING}
  {$MACRO ON}
   LCLVersion, LConvEncoding,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  ZCompatibility,
  Math{bind me after ZCompatibility!};

const
  {code page identifiers https://msdn.microsoft.com/en-us/library/windows/desktop/dd317756%28v=vs.85%29.aspx}
  zCP_Binary = 0;
  zCP_DOS437 = 437; {IBM437/MS-DOS odepage 437 (US)}
  zCP_DOS708 = 708; {Arabic (ASMO 708)}
  zCP_DOS720 = 720; {Arabic (Transparent ASMO); Arabic (DOS)}
  zCP_DOS737 = 737; {OEM Greek (formerly 437G); Greek (DOS)}
  zCP_DOS775 = 775; {MS-DOS Codepage 775 (BaltRim)}
  zCP_DOS850 = 850;	{MS-DOS Codepage 850 (Multilingual Latin 1)}
  zCP_DOS852 = 852; {ibm852 852 east european(DOS)}
  zcp_DOS855 = 855; {OEM Cyrillic (primarily Russian)}
  zCP_DOS857 = 857;	{MS-DOS Codepage 857 (Multilingual Latin 5)}
  zCP_DOS858 = 858; {MS-DOS Codepage 858  Latin I + Euro symbol}
  zCP_DOS860 = 860;	{MS-DOS Codepage 860 (Portugal)}
  zCP_DOS861 = 861;	{MS-DOS Codepage 861 (Iceland)}
  zCP_DOS862 = 862;	{MS-DOS Codepage 862 (Israel)}
  zCP_DOS863 = 863;	{MS-DOS Codepage 863 (Canada (French))}
  zCP_DOS864 = 864;	{MS-DOS Codepage 864 (Arabic) without BOX DRAWINGS below 20}
  zCP_DOS865 = 865;	{MS-DOS Codepage 865 (Norway)}
  zCP_DOS866 = 866; {ibm866	866	Cyrl (DOS)}
  zCP_DOS869 = 869; {MS-DOS Codepage 869 (Greece)}

  zCP_WIN874 = 874; {ANSI/OEM Thai (same as 28605, ISO 8859-15); Thai (Windows)}
  zCP_MSWIN921 = 921;
  zCP_MSWIN923 = 923;

  zCP_SHIFTJS = 932; {ANSI/OEM Japanese; Japanese (Shift-JIS)}
  zCP_GB2312 = 936; {ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)}
  zCP_EUCKR = 949; {ANSI/OEM Korean (Unified Hangul Code)}
  zCP_Big5 = 950; {ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5)}

  zCP_UTF16 = {$IFDEF ENDIAN_BIG}1201{$ELSE}1200{$ENDIF}; {utf-16; Indicates the Unicode character set, Windows code page 1201/1200}
  zCP_WIN1250 = 1250; {Microsoft Windows Codepage 1250 (East European)}
  zCP_WIN1251 = 1251; {Microsoft Windows Codepage 1251 (Cyrl)}
  zCP_WIN1252 = 1252; {Microsoft Windows Codepage 1252 (ANSI), USASCCI}
  zCP_WIN1253 = 1253; {Microsoft Windows Codepage 1253 (Greek)}
  zCP_WIN1254 = 1254; {Microsoft Windows Codepage 1254 (Turk)}
  zCP_WIN1255 = 1255; {Microsoft Windows Codepage 1255 (Hebrew)}
  zCP_WIN1256 = 1256; {Microsoft Windows Codepage 1256 (Arab)}
  zCP_WIN1257 = 1257; {Microsoft Windows Codepage 1257 (BaltRim)}
  zCP_WIN1258 = 1258; {Microsoft Windows Codepage 1258 (Viet), TCVN-5712}
  ZCP_JOHAB = 1361; {Korean (Johab)}

  zCP_macintosh = 10000; {MAC Roman; Western European (Mac)}
  zCP_x_mac_ce = 10029; {MAC Latin 2; Central European (Mac)}
  zCP_utf32 = 12000; {Unicode UTF-32, little endian byte order; available only to managed applications}
  zCP_utf32BE = 12001; {Unicode UTF-32, big endian byte order; available only to managed applications}

  zCP_x_IA5_Swedish = 20107; {IA5 Swedish (7-bit)}
  zCP_us_ascii = 20127; {US-ASCII (7-bit)}
  zCP_KOI8R = 20866; {cskoi8r 20866 Cyrillic (KOI8-R)}
  zCP_EUC_JP = 20932; {Japanese (JIS 0208-1990 and 0121-1990)}
  zCP_KOI8U = 21866; {KOI8-U is an 8-bit character encoding, designed to cover Ukrainian, which uses the Cyrillic alphabet.}
  zCP_L1_ISO_8859_1 = 28591; {8-bit single-byte coded graphic character sets Part 1: Latin alphabet No. 1, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L2_ISO_8859_2 = 28592; {latin2 east european (ISO), 8-bit single-byte coded graphic character sets - Part 2: Latin alphabet No. 2, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L3_ISO_8859_3 = 28593; {ISO 8859-3 Latin 3}
  zCP_L4_ISO_8859_4 = 28594; {ISO 8859-4 Baltic}
  zCP_L5_ISO_8859_5 = 28595; {8bit single-byte coded graphic character sets - Part 5: Latin/Cyrillic alphabet, is part of the ISO/IEC 8859 series of ASCII-based standard character encodings}
  zCP_L6_ISO_8859_6 = 28596; {ISO 8859-6 Arabic}
  zCP_L7_ISO_8859_7 = 28597; {ISO 8859-7 Greek}
  zCP_L8_ISO_8859_8 = 28598; {ISO 8859-8 Hebrew; Hebrew (ISO-Visual)}
  zCP_L5_ISO_8859_9 = 28599; {ISO 8859-9 Turkish}
  zCP_L6_ISO_8859_10 = 28600; { ISO 8859-10, ECMA 144 Nordic }
  zCP_L7_ISO_8859_13 = 28603; {ISO 8859-13 Estonian}
  zCP_L8_ISO_8859_14 = 28604; { ISO 8859-14 Celtic }
  zCP_L9_ISO_8859_15 = 28605; {ISO 8859-15 Latin 9}
  zCP_L10_ISO_8859_16 = 28606;  { ISO 8859-16, ASRO SR 14111 Romanian }

  zCP_csISO2022JP = 50221; {ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)}
  zCP_euc_JP_win = 51932; {EUC Japanese}
  zCP_EUC_CN = 51936; {EUC Simplified Chinese; Chinese Simplified (EUC)}
  zCP_euc_kr = 51949; {EUC Korean}
  zCP_GB18030 = 54936; {Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030)}
  zCP_UTF7 = 65000;
  zCP_UTF8 = 65001;
  zCP_NONE = $ffff;

{$IFDEF WITH_LCONVENCODING}
const
  ZLConvCodepages: array[0..16] of Word = (
    28591,  //ISO_8859_1
    28592,  //ISO_8859_2
    1250,   //WIN1250
    1251,   //WIN1251
    1252,   //WIN1252
    1253,   //WIN1253
    1254,   //WIN1254
    1255,   //WIN1255
    1256,   //WIN1256
    1257,   //WIN1257
    1258,   //WIN1258
    437,    //CP437
    850,    //CP850
    852,    //CP852
    866,    //CP866
    874,    //CP874
    20866   //KOI8 (Russian)
    );

function IsLConvEncodingCodePage(const CP: Word): Boolean;
{$ENDIF}

type
  /// option set for RawUnicodeToUtf8() conversion
  TCharConversionFlags = set of (
    ccfNoTrailingZero, ccfReplacementCharacterForUnmatchedSurrogate);

/// <author>EgonHugeist.</author>
/// <summary>Convert a raw encoded string to a UnicodeString.</summary>
/// <param>"s" the source string to be converted.</param>
/// <param>"CP" the CodePage of the source string.</param>
/// <returns>A converted UnicodeString.</returns>
function ZRawToUnicode(const S: RawByteString; const CP: Word): UnicodeString; {$IF defined(WITH_INLINE) and not defined(WITH_LCONVENCODING)}inline; {$IFEND}

/// <author>EgonHugeist.</author>
/// <summary>Convert a raw encoded buffer to a UnicodeString.</summary>
/// <param>"Source" the source buffer to be converted.</param>
/// <param>"SourceBytes" the size of the buffer.</param>
/// <param>"CP" the CodePage of the source buffer.</param>
/// <returns>A converted UnicodeString.</returns>
function PRawToUnicode(Source: PAnsiChar; SourceBytes: LengthInt; CP: Word): UnicodeString; overload;

/// <author>EgonHugeist.</author>
/// <summary>Convert a raw encoded buffer to a UnicodeString.</summary>
/// <param>"Source" the source buffer to be converted.</param>
/// <param>"SourceBytes" the size of the buffer.</param>
/// <param>"CP" the CodePage of the source buffer.</param>
/// <param>"Result" A reference to the converted UnicodeString.</param>
procedure PRawToUnicode(Source: PAnsiChar; SourceBytes: LengthInt; CP: Word; var Result: UnicodeString); overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a raw encoded buffer to a UTF16 Buffer. The buffer must
///  have enough space reserved for this conversion.</summary>
/// <param>"Source" the source buffer to be converted.</param>
/// <param>"Dest" the Dest buffer we write in.</param>
/// <param>"SourceBytes" the size of the buffer.</param>
/// <param>"CP" the CodePage of the source buffer.</param>
/// <returns>The amount of converted words.</returns>
function PRaw2PUnicodeBuf(Source: PAnsiChar; Dest: Pointer; SourceBytes: LengthInt; CP: Word): LengthInt;

/// <author>EgonHugeist</author>
/// <summary>Convert MaxWords of a raw encoded buffer into a UTF16 Buffer.</summary>
/// <param>"Source" the source buffer to be converted.</param>
/// <param>"Dest" the Dest buffer we write in.</param>
/// <param>"CP" the CodePage of the source buffer.</param>
/// <param>"SourceBytes" the size of the source buffer in bytes.</param>
/// <param>"DestWords" the size of the destination buffer in words.</param>
/// <returns>The amount of converted words.</returns>
function PRaw2PUnicode(Source: PAnsiChar; Dest: PWideChar; CP: Word; SourceBytes, DestWords: LengthInt): LengthInt;

/// <author>EgonHugeist</author>
/// <summary>Convert a UnicodeString into a raw encoded string.</summary>
/// <param>"US" the UnicodeString to be converted.</param>
/// <param>"CP" the CodePage of the destination string.</param>
/// <returns>A raw encoded string.</returns>
function ZUnicodeToRaw(const US: UnicodeString; CP: Word): RawByteString; {$IF defined(WITH_INLINE) and not defined(WITH_LCONVENCODING)}inline; {$IFEND}

/// <author>EgonHugeist</author>
/// <summary>Convert a UTF16 Buffer into a raw encoded string.</summary>
/// <param>"Source" the buffer to be converted.</param>
/// <param>"SrcWords" the count of words to be converted.</param>
/// <param>"CP" the CodePage of the destination string.</param>
/// <returns>A raw encoded string.</returns>
function PUnicodeToRaw(Source: PWideChar; SrcWords: LengthInt; CP: Word): RawByteString; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a UTF16 Buffer into a raw encoded string.</summary>
/// <param>"Source" the buffer to be converted.</param>
/// <param>"SrcWords" the count of words to be converted.</param>
/// <param>"CP" the CodePage of the destination string.</param>
/// <param>"Result" A reference to the raw encoded string.</param>
procedure PUnicodeToRaw(Source: PWideChar; SrcWords: LengthInt; CP: Word; var Result: RawByteString); overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a UTF16 Buffer into a raw buffer.</summary>
/// <param>"Source" the buffer to be converted.</param>
/// <param>"Dest" the buffer we write in.</param>
/// <param>"SrcWords" the count of words to be converted.</param>
/// <param>"MaxDestBytes" the size in bytes of the destination buffer.</param>
/// <param>"CP" the CodePage of the destination buffer.</param>
/// <returns>The written count of bytes.</returns>
function PUnicode2PRawBuf(Source: PWideChar; Dest: PAnsiChar; SrcWords, MaxDestBytes: LengthInt; CP: Word): LengthInt;

/// <author>Arnaud Bouchez</author>
/// <summary>see: syncommons.pas in mORMot framework www.synopse.info
///  convert a RawUnicode UTF-16 PWideChar into a UTF-8 buffer
///  orgiginal named as RawUnicodeToUtf8()
///  - replace system.UnicodeToUtf8 implementation, which is rather slow
///   since Delphi 2009+
///  - will append a trailing #0 to the ending PUTF8Char, unless
///   ccfNoTrailingZero is set
///   - if ccfReplacementCharacterForUnmatchedSurrogate is set, this function will identify
///   unmatched surrogate pairs and replace them with EF BF BD // FFFD  Unicode
///   Replacement character - see https://en.wikipedia.org/wiki/Specials_(Unicode_block)
/// Changes by EgonHugeist:
///    - replace PUTF8Char to PAnsichar
///    - replace PtrInt to NativeUInt
///    - add hard word cast in the ascii-pair loop .. range-checks did make noise here
///    - replace the ansichar casts with Byte/word  values -> nextgen
///    - added three labels ( loop_ascii_pairs,done,next) to loop in code and test ascii-pairs
///      after each convertion again. so i commented the main repeat loop and all continue/break tests
/// </summary>
/// <param>"Dest" the buffer we write in.</param>
/// <param>"DestLen" the size in bytes of the destination buffer.</param>
/// <param>"Source" the buffer to be converted.</param>
/// <param>"SourceLen" the count of words to be converted.</param>
/// <param>"Flags" the conversion flags.</param>
/// <returns>The written count of bytes.</returns>
function PUnicodeToUtf8Buf(Dest: PAnsiChar; DestLen: NativeUint;
  Source: PWideChar; SourceLen: NativeUint; Flags: TCharConversionFlags): NativeUint;

/// <author>EgonHugeist</author>
/// <summary>Test if the given codepage is a MultiByte one.</summary>
/// <returns><c>True</c> if the codepage is a multibyte codepage;
///  <c>False</c> otherwise.</returns>
function IsMBCSCodePage(CP: Word): Boolean; {$IFDEF WITH_INLINE}inline;{$ENDIF}

Type
  TEncodeType = (etUSASCII, etUTF8, etANSI);
  TSBCSMapProc = procedure(Source: PByteArray; SourceBytes: LengthInt; Dest: PWordArray);
  TMBCSMapProc = function(Source: PAnsichar; SourceBytes: LengthInt; Dest: PWideChar): LengthInt;
  PSBCS_MAP = ^TSBCS_MAP;
  TSBCS_MAP = packed array[$00..$FF] of Word;

/// <author>EgonHugeist</author>
/// <summary>Detect the encoding of an given buffer.</summary>
/// <param>"Source" the buffer to be tested.</param>
/// <param>"Len" the length in bytes of the buffer.</param>
/// <returns><c>TEncodeType</c> of the buffer.</returns>
function ZDetectUTF8Encoding(Source: PAnsiChar; Len: NativeUInt): TEncodeType;

/// <author>EgonHugeist</author>
/// <summary>Convert a ASCII7 buffer to an UnicodeString. Each byte will be
///  widened to a word.</summary>
/// <param>"Source" the buffer to be converted.</param>
/// <param>"Len" the length in bytes of the buffer.</param>
/// <returns>a converted UnicodeString.</returns>
function USASCII7ToUnicodeString(Source: PAnsiChar; Len: NativeUInt): UnicodeString; overload;

/// <author>EgonHugeist</author>
/// <summary>Convert a ASCII7 rawbytestring to an UnicodeString. Each byte will
///  be widened to a word.</summary>
/// <param>"Source" the string to be converted.</param>
/// <returns>a converted UnicodeString.</returns>
function USASCII7ToUnicodeString(const Source: RawByteString): UnicodeString; overload;

{SBCS codepages $00..FF}
procedure AnsiSBCSToUTF16(Source: PAnsichar; SourceBytes: LengthInt;
  var Dest: UnicodeString; SBCS_MAP: PSBCS_MAP); overload;
procedure AnsiSBCSToUTF16(Source: PByteArray; Dest: PWordArray;
  SBCS_MAP: PSBCS_MAP; SourceBytes: LengthInt); overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure AnsiSBCSToUTF16(Source: PAnsichar; SourceBytes: LengthInt;
  const MapProc: TSBCSMapProc; var Dest: UnicodeString); overload;
procedure MapByteToUTF16(Source: PByteArray; SourceBytes: LengthInt;
  Dest: PWordArray); {$IFDEF WITH_INLINE}inline;{$ENDIF}

{MBCS codepages }
procedure AnsiMBCSToUTF16(Source: PAnsichar; SourceBytes: LengthInt;
  const MapProc: TMBCSMapProc; var Dest: UnicodeString);
function UTF8ToWideChar(Source: PAnsichar; SourceBytes: LengthInt; Dest: PWideChar): NativeUInt; overload;
function UTF8ToWideChar(source: PAnsiChar; dest: PWideChar; sourceBytes, DestWords: LengthInt): NativeUInt; overload;
function UTF8AsUTF16Words(Source: PAnsichar; SourceBytes: LengthInt): NativeUInt;
function CountOfUtf8Chars(Source: PAnsichar; SourceBytes: NativeUint): NativeUint;
function PRawToPRawBuf(Source, Dest: PAnsiChar; SourceBytes, MaxDestBytes: LengthInt; SrcCP, DestCP: Word): LengthInt;

procedure PRawToRawConvert(Source: PAnsiChar; SourceBytes: LengthInt; SrcCP, DestCP: Word; var Result: RawByteString);

const
  CP437ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9, $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP708ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $2502, $2524, $00E9, $00E2, $2561, $00E0, $2562, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $2556, $2555, $2563,
    $2551, $2557, $255D, $00F4, $255C, $255B, $00FB, $00F9, $2510, $2514, $009A, $009B, $009C, $009D, $009E, $009F,
    $F8C1, $2534, $252C, $251C, $00A4, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $060C, $2566, $00AB, $00BB,
    $2591, $2592, $2593, $2560, $2550, $256C, $2567, $2568, $2564, $2565, $2559, $061B, $2558, $2552, $2553, $061F,
    $256B, $0621, $0622, $0623, $0624, $0625, $0626, $0627, $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
    $0630, $0631, $0632, $0633, $0634, $0635, $0636, $0637, $0638, $0639, $063A, $2588, $2584, $258C, $2590, $2580,
    $0640, $0641, $0642, $0643, $0644, $0645, $0646, $0647, $0648, $0649, $064A, $064B, $064C, $064D, $064E, $064F,
    $0650, $0651, $0652, $F8C2, $F8C3, $F8C4, $F8C5, $F8C6, $F8C7, $256A, $2518, $250C, $00B5, $00A3, $25A0, $00A0);
  CP720ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $00E9, $00E2, $0084, $00E0, $0086, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $008D, $008E, $008F,
    $0090, $0651, $0652, $00F4, $00A4, $0640, $00FB, $00F9, $0621, $0622, $0623, $0624, $00A3, $0625, $0626, $0627,
    $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F, $0630, $0631, $0632, $0633, $0634, $0635, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $0636, $0637, $0638, $0639, $063A, $0641, $00B5, $0642, $0643, $0644, $0645, $0646, $0647, $0648, $0649, $064A,
    $2261, $064B, $064C, $064D, $064E, $064F, $0650, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP737ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F, $03A0,
    $03A1, $03A3, $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7, $03B8,
    $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF, $03C0, $03C1, $03C3, $03C2, $03C4, $03C5, $03C6, $03C7, $03C8,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03C9, $03AC, $03AD, $03AE, $03CA, $03AF, $03CC, $03CD, $03CB, $03CE, $0386, $0388, $0389, $038A, $038C, $038E,
    $038F, $00B1, $2265, $2264, $03AA, $03AB, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP775ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0106, $00FC, $00E9, $0101, $00E4, $0123, $00E5, $0107, $0142, $0113, $0156, $0157, $012B, $0179, $00C4, $00C5,
    $00C9, $00E6, $00C6, $014D, $00F6, $0122, $00A2, $015A, $015B, $00D6, $00DC, $00F8, $00A3, $00D8, $00D7, $00A4,
    $0100, $012A, $00F3, $017B, $017C, $017A, $201D, $00A6, $00A9, $00AE, $00AC, $00BD, $00BC, $0141, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $0104, $010C, $0118, $0116, $2563, $2551, $2557, $255D, $012E, $0160, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $0172, $016A, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $017D,
    $0105, $010D, $0119, $0117, $012F, $0161, $0173, $016B, $017E, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $00D3, $00DF, $014C, $0143, $00F5, $00D5, $00B5, $0144, $0136, $0137, $013B, $013C, $0146, $0112, $0145, $2019,
    $00AD, $00B1, $201C, $00BE, $00B6, $00A7, $00F7, $201E, $00B0, $2219, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0);
  CP850ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9, $00FF, $00D6, $00DC, $00F8, $00A3, $00D8, $00D7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $00AE, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $00C0, $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $00E3, $00C3, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
    $00F0, $00D0, $00CA, $00CB, $00C8, $0131, $00CD, $00CE, $00CF, $2518, $250C, $2588, $2584, $00A6, $00CC, $2580,
    $00D3, $00DF, $00D4, $00D2, $00F5, $00D5, $00B5, $00FE, $00DE, $00DA, $00DB, $00D9, $00FD, $00DD, $00AF, $00B4,
    $00AD, $00B1, $2017, $00BE, $00B6, $00A7, $00F7, $00B8, $00B0, $00A8, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0);
  CP852ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $016F, $0107, $00E7, $0142, $00EB, $0150, $0151, $00EE, $0179, $00C4, $0106,
    $00C9, $0139, $013A, $00F4, $00F6, $013D, $013E, $015A, $015B, $00D6, $00DC, $0164, $0165, $0141, $00D7, $010D,
    $00E1, $00ED, $00F3, $00FA, $0104, $0105, $017D, $017E, $0118, $0119, $00AC, $017A, $010C, $015F, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $011A, $015E, $2563, $2551, $2557, $255D, $017B, $017C, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $0102, $0103, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
    $0111, $0110, $010E, $00CB, $010F, $0147, $00CD, $00CE, $011B, $2518, $250C, $2588, $2584, $0162, $016E, $2580,
    $00D3, $00DF, $00D4, $0143, $0144, $0148, $0160, $0161, $0154, $00DA, $0155, $0170, $00FD, $00DD, $0163, $00B4,
    $00AD, $02DD, $02DB, $02C7, $02D8, $00A7, $00F7, $00B8, $00B0, $00A8, $02D9, $0171, $0158, $0159, $25A0, $00A0);
  CP855ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0452, $0402, $0453, $0403, $0451, $0401, $0454, $0404, $0455, $0405, $0456, $0406, $0457, $0407, $0458, $0408,
    $0459, $0409, $045A, $040A, $045B, $040B, $045C, $040C, $045E, $040E, $045F, $040F, $044E, $042E, $044A, $042A,
    $0430, $0410, $0431, $0411, $0446, $0426, $0434, $0414, $0435, $0415, $0444, $0424, $0433, $0413, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $0445, $0425, $0438, $0418, $2563, $2551, $2557, $255D, $0439, $0419, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $043A, $041A, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
    $043B, $041B, $043C, $041C, $043D, $041D, $043E, $041E, $043F, $2518, $250C, $2588, $2584, $041F, $044F, $2580,
    $042F, $0440, $0420, $0441, $0421, $0442, $0422, $0443, $0423, $0436, $0416, $0432, $0412, $044C, $042C, $2116,
    $00AD, $044B, $042B, $0437, $0417, $0448, $0428, $044D, $042D, $0449, $0429, $0447, $0427, $00A7, $25A0, $00A0);
  CP857ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $0131, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9, $0130, $00D6, $00DC, $00F8, $00A3, $00D8, $015E, $015F,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $011E, $011F, $00BF, $00AE, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $00C0, $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $00E3, $00C3, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
    $00BA, $00AA, $00CA, $00CB, $00C8, $F8BB, $00CD, $00CE, $00CF, $2518, $250C, $2588, $2584, $00A6, $00CC, $2580,
    $00D3, $00DF, $00D4, $00D2, $00F5, $00D5, $00B5, $F8BC, $00D7, $00DA, $00DB, $00D9, $00EC, $00FF, $00AF, $00B4,
    $00AD, $00B1, $F8BD, $00BE, $00B6, $00A7, $00F7, $00B8, $00B0, $00A8, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0);
  CP858ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9, $00FF, $00D6, $00DC, $00F8, $00A3, $00D8, $00D7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $00AE, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $00C0, $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $00E3, $00C3, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
    $00F0, $00D0, $00CA, $00CB, $00C8, $20AC, $00CD, $00CE, $00CF, $2518, $250C, $2588, $2584, $00A6, $00CC, $2580,
    $00D3, $00DF, $00D4, $00D2, $00F5, $00D5, $00B5, $00FE, $00DE, $00DA, $00DB, $00D9, $00FD, $00DD, $00AF, $00B4,
    $00AD, $00B1, $2017, $00BE, $00B6, $00A7, $00F7, $00B8, $00B0, $00A8, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0);
  CP860ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E3, $00E0, $00C1, $00E7, $00EA, $00CA, $00E8, $00CD, $00D4, $00EC, $00C3, $00C2,
    $00C9, $00C0, $00C8, $00F4, $00F5, $00F2, $00DA, $00F9, $00CC, $00D5, $00DC, $00A2, $00A3, $00D9, $20A7, $00D3,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $00D2, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP861ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00D0, $00F0, $00DE, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00FE, $00FB, $00DD, $00FD, $00D6, $00DC, $00F8, $00A3, $00D8, $20A7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00C1, $00CD, $00D3, $00DA, $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP862ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7, $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
    $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7, $05E8, $05E9, $05EA, $00A2, $00A3, $00A5, $20A7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP863ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00C2, $00E0, $00B6, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $2017, $00C0, $00A7,
    $00C9, $00C8, $00CA, $00F4, $00CB, $00CF, $00FB, $00F9, $00A4, $00D4, $00DC, $00A2, $00A3, $00D9, $00DB, $0192,
    $00A6, $00B4, $00F3, $00FA, $00A8, $00B8, $00B3, $00AF, $00CE, $2310, $00AC, $00BD, $00BC, $00BE, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP864ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00B0, $00B7, $2219, $221A, $2592, $2500, $2502, $253C, $2524, $252C, $251C, $2534, $2510, $250C, $2514, $2518,
    $03B2, $221E, $03C6, $00B1, $00BD, $00BC, $2248, $00AB, $00BB, $FEF7, $FEF8, $009B, $009C, $FEFB, $FEFC, $009F,
    $00A0, $00AD, $FE82, $00A3, $00A4, $FE84, $F8BE, $F8BF, $FE8E, $FE8F, $FE95, $FE99, $060C, $FE9D, $FEA1, $FEA5,
    $0660, $0661, $0662, $0663, $0664, $0665, $0666, $0667, $0668, $0669, $FED1, $061B, $FEB1, $FEB5, $FEB9, $061F,
    $00A2, $FE80, $FE81, $FE83, $FE85, $FECA, $FE8B, $FE8D, $FE91, $FE93, $FE97, $FE9B, $FE9F, $FEA3, $FEA7, $FEA9,
    $FEAB, $FEAD, $FEAF, $FEB3, $FEB7, $FEBB, $FEBF, $FEC1, $FEC5, $FECB, $FECF, $00A6, $00AC, $00F7, $00D7, $FEC9,
    $0640, $FED3, $FED7, $FEDB, $FEDF, $FEE3, $FEE7, $FEEB, $FEED, $FEEF, $FEF3, $FEBD, $FECC, $FECE, $FECD, $FEE1,
    $FE7D, $0651, $FEE5, $FEE9, $FEEC, $FEF0, $FEF2, $FED0, $FED5, $FEF5, $FEF6, $FEDD, $FED9, $FEF1, $25A0, $F8C0);
  CP865ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7, $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
    $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9, $00FF, $00D6, $00DC, $00F8, $00A3, $00D8, $20A7, $0192,
    $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA, $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00A4,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4, $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248, $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0);
  CP866ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417, $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
    $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427, $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
    $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437, $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556, $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B, $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447, $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
    $0401, $0451, $0404, $0454, $0407, $0457, $040E, $045E, $00B0, $2219, $00B7, $221A, $2116, $00A4, $25A0, $00A0);
  CP869ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0386, $0087, $00B7, $00AC, $00A6, $2018, $2019, $0388, $2015, $0389,
    $038A, $03AA, $038C, $0093, $0094, $038E, $03AB, $00A9, $038F, $00B2, $00B3, $03AC, $00A3, $03AD, $03AE, $03AF,
    $03CA, $0390, $03CC, $03CD, $0391, $0392, $0393, $0394, $0395, $0396, $0397, $00BD, $0398, $0399, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $039A, $039B, $039C, $039D, $2563, $2551, $2557, $255D, $039E, $039F, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $03A0, $03A1, $255A, $2554, $2569, $2566, $2560, $2550, $256C, $03A3,
    $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03B1, $03B2, $03B3, $2518, $250C, $2588, $2584, $03B4, $03B5, $2580,
    $03B6, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF, $03C0, $03C1, $03C3, $03C2, $03C4, $0384,
    $00AD, $00B1, $03C5, $03C6, $03C7, $00A7, $03C8, $0385, $00B0, $00A8, $03C9, $03CB, $03B0, $03CE, $25A0, $00A0);
  CP870ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $009C, $0009, $0086, $007F, $0097, $008D, $008E, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $009D, $0085, $0008, $0087, $0018, $0019, $0092, $008F, $001C, $001D, $001E, $001F,
    $0080, $0081, $0082, $0083, $0084, $000A, $0017, $001B, $0088, $0089, $008A, $008B, $008C, $0005, $0006, $0007,
    $0090, $0091, $0016, $0093, $0094, $0095, $0096, $0004, $0098, $0099, $009A, $009B, $0014, $0015, $009E, $001A,
    $0020, $00A0, $00E2, $00E4, $0163, $00E1, $0103, $010D, $00E7, $0107, $005B, $002E, $003C, $0028, $002B, $0021,
    $0026, $00E9, $0119, $00EB, $016F, $00ED, $00EE, $013E, $013A, $00DF, $005D, $0024, $002A, $0029, $003B, $005E,
    $002D, $002F, $00C2, $00C4, $02DD, $00C1, $0102, $010C, $00C7, $0106, $007C, $002C, $0025, $005F, $003E, $003F,
    $02C7, $00C9, $0118, $00CB, $016E, $00CD, $00CE, $013D, $0139, $0060, $003A, $0023, $0040, $0027, $003D, $0022,
    $02D8, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $015B, $0148, $0111, $00FD, $0159, $015F,
    $00B0, $006A, $006B, $006C, $006D, $006E, $006F, $0070, $0071, $0072, $0142, $0144, $0161, $00B8, $02DB, $00A4,
    $0105, $007E, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $015A, $0147, $0110, $00DD, $0158, $015E,
    $02D9, $0104, $017C, $0162, $017B, $00A7, $017E, $017A, $017D, $0179, $0141, $0143, $0160, $00A8, $00B4, $00D7,
    $007B, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $00AD, $00F4, $00F6, $0155, $00F3, $0151,
    $007D, $004A, $004B, $004C, $004D, $004E, $004F, $0050, $0051, $0052, $011A, $0171, $00FC, $0165, $00FA, $011B,
    $005C, $00F7, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $010F, $00D4, $00D6, $0154, $00D3, $0150,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $010E, $0170, $00DC, $0164, $00DA, $009F);
  CP874ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $0082, $0083, $0084, $2026, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0E01, $0E02, $0E03, $0E04, $0E05, $0E06, $0E07, $0E08, $0E09, $0E0A, $0E0B, $0E0C, $0E0D, $0E0E, $0E0F,
    $0E10, $0E11, $0E12, $0E13, $0E14, $0E15, $0E16, $0E17, $0E18, $0E19, $0E1A, $0E1B, $0E1C, $0E1D, $0E1E, $0E1F,
    $0E20, $0E21, $0E22, $0E23, $0E24, $0E25, $0E26, $0E27, $0E28, $0E29, $0E2A, $0E2B, $0E2C, $0E2D, $0E2E, $0E2F,
    $0E30, $0E31, $0E32, $0E33, $0E34, $0E35, $0E36, $0E37, $0E38, $0E39, $0E3A, $F8C1, $F8C2, $F8C3, $F8C4, $0E3F,
    $0E40, $0E41, $0E42, $0E43, $0E44, $0E45, $0E46, $0E47, $0E48, $0E49, $0E4A, $0E4B, $0E4C, $0E4D, $0E4E, $0E4F,
    $0E50, $0E51, $0E52, $0E53, $0E54, $0E55, $0E56, $0E57, $0E58, $0E59, $0E5A, $0E5B, $F8C5, $F8C6, $F8C7, $F8C8);
  CP875ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $009C, $0009, $0086, $007F, $0097, $008D, $008E, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $009D, $0085, $0008, $0087, $0018, $0019, $0092, $008F, $001C, $001D, $001E, $001F,
    $0080, $0081, $0082, $0083, $0084, $000A, $0017, $001B, $0088, $0089, $008A, $008B, $008C, $0005, $0006, $0007,
    $0090, $0091, $0016, $0093, $0094, $0095, $0096, $0004, $0098, $0099, $009A, $009B, $0014, $0015, $009E, $001A,
    $0020, $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398, $0399, $005B, $002E, $003C, $0028, $002B, $0021,
    $0026, $039A, $039B, $039C, $039D, $039E, $039F, $03A0, $03A1, $03A3, $005D, $0024, $002A, $0029, $003B, $005E,
    $002D, $002F, $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03AA, $03AB, $007C, $002C, $0025, $005F, $003E, $003F,
    $00A8, $0386, $0388, $0389, $00A0, $038A, $038C, $038E, $038F, $0060, $003A, $0023, $0040, $0027, $003D, $0022,
    $0385, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6,
    $00B0, $006A, $006B, $006C, $006D, $006E, $006F, $0070, $0071, $0072, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC,
    $00B4, $007E, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $03BD, $03BE, $03BF, $03C0, $03C1, $03C3,
    $00A3, $03AC, $03AD, $03AE, $03CA, $03AF, $03CC, $03CD, $03CB, $03CE, $03C2, $03C4, $03C5, $03C6, $03C7, $03C8,
    $007B, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $00AD, $03C9, $0390, $03B0, $2018, $2015,
    $007D, $004A, $004B, $004C, $004D, $004E, $004F, $0050, $0051, $0052, $00B1, $00BD, $001A, $0387, $2019, $00A6,
    $005C, $001A, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $00B2, $00A7, $001A, $001A, $00AB, $00AC,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $00B3, $00A9, $001A, $001A, $00BB, $009F);
  CP1250ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0083, $201E, $2026, $2020, $2021, $0088, $2030, $0160, $2039, $015A, $0164, $017D, $0179,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0098, $2122, $0161, $203A, $015B, $0165, $017E, $017A,
    $00A0, $02C7, $02D8, $0141, $00A4, $0104, $00A6, $00A7, $00A8, $00A9, $015E, $00AB, $00AC, $00AD, $00AE, $017B,
    $00B0, $00B1, $02DB, $0142, $00B4, $00B5, $00B6, $00B7, $00B8, $0105, $015F, $00BB, $013D, $02DD, $013E, $017C,
    $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7, $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
    $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7, $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
    $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7, $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
    $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7, $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9);
  CP1251ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0402, $0403, $201A, $0453, $201E, $2026, $2020, $2021, $20AC, $2030, $0409, $2039, $040A, $040C, $040B, $040F,
    $0452, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0098, $2122, $0459, $203A, $045A, $045C, $045B, $045F,
    $00A0, $040E, $045E, $0408, $00A4, $0490, $00A6, $00A7, $0401, $00A9, $0404, $00AB, $00AC, $00AD, $00AE, $0407,
    $00B0, $00B1, $0406, $0456, $0491, $00B5, $00B6, $00B7, $0451, $2116, $0454, $00BB, $0458, $0405, $0455, $0457,
    $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417, $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
    $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427, $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
    $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437, $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
    $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447, $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F);
  CP1252ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0160, $2039, $0152, $008D, $017D, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $0161, $203A, $0153, $009D, $017E, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF);
  CP1253ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021, $0088, $2030, $008A, $2039, $008C, $008D, $008E, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0098, $2122, $009A, $203A, $009C, $009D, $009E, $009F,
    $00A0, $0385, $0386, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $F8F9, $00AB, $00AC, $00AD, $00AE, $2015,
    $00B0, $00B1, $00B2, $00B3, $0384, $00B5, $00B6, $00B7, $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
    $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
    $03A0, $03A1, $F8FA, $03A3, $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
    $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
    $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7, $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, $F8FB);
  CP1254ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0160, $2039, $0152, $008D, $008E, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $0161, $203A, $0153, $009D, $009E, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF);
  CP1255ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $008A, $2039, $008C, $008D, $008E, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $009A, $203A, $009C, $009D, $009E, $009F,
    $00A0, $00A1, $00A2, $00A3, $20AA, $00A5, $00A6, $00A7, $00A8, $00A9, $00D7, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, $00BF,
    $05B0, $05B1, $05B2, $05B3, $05B4, $05B5, $05B6, $05B7, $05B8, $05B9, $05BA, $05BB, $05BC, $05BD, $05BE, $05BF,
    $05C0, $05C1, $05C2, $05C3, $05F0, $05F1, $05F2, $05F3, $05F4, $F88D, $F88E, $F88F, $F890, $F891, $F892, $F893,
    $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7, $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
    $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7, $05E8, $05E9, $05EA, $F894, $F895, $200E, $200F, $F896);
  CP1256ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $067E, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0679, $2039, $0152, $0686, $0698, $0688,
    $06AF, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $06A9, $2122, $0691, $203A, $0153, $200C, $200D, $06BA,
    $00A0, $060C, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $06BE, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $061B, $00BB, $00BC, $00BD, $00BE, $061F,
    $06C1, $0621, $0622, $0623, $0624, $0625, $0626, $0627, $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
    $0630, $0631, $0632, $0633, $0634, $0635, $0636, $00D7, $0637, $0638, $0639, $063A, $0640, $0641, $0642, $0643,
    $00E0, $0644, $00E2, $0645, $0646, $0647, $0648, $00E7, $00E8, $00E9, $00EA, $00EB, $0649, $064A, $00EE, $00EF,
    $064B, $064C, $064D, $064E, $00F4, $064F, $0650, $00F7, $0651, $00F9, $0652, $00FB, $00FC, $200E, $200F, $06D2);
  CP1257ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0083, $201E, $2026, $2020, $2021, $0088, $2030, $008A, $2039, $008C, $00A8, $02C7, $00B8,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0098, $2122, $009A, $203A, $009C, $00AF, $02DB, $009F,
    $00A0, $F8FC, $00A2, $00A3, $00A4, $F8FD, $00A6, $00A7, $00D8, $00A9, $0156, $00AB, $00AC, $00AD, $00AE, $00C6,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00F8, $00B9, $0157, $00BB, $00BC, $00BD, $00BE, $00E6,
    $0104, $012E, $0100, $0106, $00C4, $00C5, $0118, $0112, $010C, $00C9, $0179, $0116, $0122, $0136, $012A, $013B,
    $0160, $0143, $0145, $00D3, $014C, $00D5, $00D6, $00D7, $0172, $0141, $015A, $016A, $00DC, $017B, $017D, $00DF,
    $0105, $012F, $0101, $0107, $00E4, $00E5, $0119, $0113, $010D, $00E9, $017A, $0117, $0123, $0137, $012B, $013C,
    $0161, $0144, $0146, $00F3, $014D, $00F5, $00F6, $00F7, $0173, $0142, $015B, $016B, $00FC, $017C, $017E, $02D9);
  CP1258ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $20AC, $0081, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $008A, $2039, $0152, $008D, $008E, $008F,
    $0090, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $009A, $203A, $0153, $009D, $009E, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $0102, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $0300, $00CD, $00CE, $00CF,
    $0110, $00D1, $0309, $00D3, $00D4, $01A0, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $01AF, $0303, $00DF,
    $00E0, $00E1, $00E2, $0103, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $0301, $00ED, $00EE, $00EF,
    $0111, $00F1, $0323, $00F3, $00F4, $01A1, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $01B0, $20AB, $00FF);
  CP10000ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C4, $00C5, $00C7, $00C9, $00D1, $00D6, $00DC, $00E1, $00E0, $00E2, $00E4, $00E3, $00E5, $00E7, $00E9, $00E8,
    $00EA, $00EB, $00ED, $00EC, $00EE, $00EF, $00F1, $00F3, $00F2, $00F4, $00F6, $00F5, $00FA, $00F9, $00FB, $00FC,
    $2020, $00B0, $00A2, $00A3, $00A7, $2022, $00B6, $00DF, $00AE, $00A9, $2122, $00B4, $00A8, $2260, $00C6, $00D8,
    $221E, $00B1, $2264, $2265, $00A5, $00B5, $2202, $2211, $220F, $03C0, $222B, $00AA, $00BA, $2126, $00E6, $00F8,
    $00BF, $00A1, $00AC, $221A, $0192, $2248, $2206, $00AB, $00BB, $2026, $00A0, $00C0, $00C3, $00D5, $0152, $0153,
    $2013, $2014, $201C, $201D, $2018, $2019, $00F7, $25CA, $00FF, $0178, $2044, $20AC, $2039, $203A, $FB01, $FB02,
    $2021, $00B7, $201A, $201E, $2030, $00C2, $00CA, $00C1, $00CB, $00C8, $00CD, $00CE, $00CF, $00CC, $00D3, $00D4,
    $F8FF, $00D2, $00DA, $00DB, $00D9, $0131, $02C6, $02DC, $00AF, $02D8, $02D9, $02DA, $00B8, $02DD, $02DB, $02C7);
  CP10029ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $00C4, $0100, $0101, $00C9, $0104, $00D6, $00DC, $00E1, $0105, $010C, $00E4, $010D, $0106, $0107, $00E9, $0179,
    $017A, $010E, $00ED, $010F, $0112, $0113, $0116, $00F3, $0117, $00F4, $00F6, $00F5, $00FA, $011A, $011B, $00FC,
    $2020, $00B0, $0118, $00A3, $00A7, $2022, $00B6, $00DF, $00AE, $00A9, $2122, $0119, $00A8, $2260, $0123, $012E,
    $012F, $012A, $2264, $2265, $012B, $0136, $2202, $2211, $0142, $013B, $013C, $013D, $013E, $0139, $013A, $0145,
    $0146, $0143, $00AC, $221A, $0144, $0147, $2206, $00AB, $00BB, $2026, $00A0, $0148, $0150, $00D5, $0151, $014C,
    $2013, $2014, $201C, $201D, $2018, $2019, $00F7, $25CA, $014D, $0154, $0155, $0158, $2039, $203A, $0159, $0156,
    $0157, $0160, $201A, $201E, $0161, $015A, $015B, $00C1, $0164, $0165, $00CD, $017D, $017E, $016A, $00D3, $00D4,
    $016B, $016E, $00DA, $016F, $0170, $0171, $0172, $0173, $00DD, $00FD, $0137, $017B, $0141, $017C, $0122, $02C7);
  CP20107ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $00A4, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $00C9, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $00C4, $00D6, $00C5, $00DC, $005F,
    $00E9, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $00E4, $00F6, $00E5, $00FC, $007F,
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $00A4, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $00C9, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $00C4, $00D6, $00C5, $00DC, $005F,
    $00E9, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $00E4, $00F6, $00E5, $00FC, $007F);
  CP20866ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524, $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
    $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248, $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
    $2550, $2551, $2552, $0451, $2553, $2554, $2555, $2556, $2557, $2558, $2559, $255A, $255B, $255C, $255D, $255E,
    $255F, $2560, $2561, $0401, $2562, $2563, $2564, $2565, $2566, $2567, $2568, $2569, $256A, $256B, $256C, $00A9,
    $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433, $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
    $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432, $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
    $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413, $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
    $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412, $042C, $042B, $0417, $0428, $042D, $0429, $0427, $042A);
  CP20127ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F);
  CP21866ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524, $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
    $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248, $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
    $2550, $2551, $2552, $0451, $0454, $2554, $0456, $0457, $2557, $2558, $2559, $255A, $255B, $0491, $045E, $255E,
    $255F, $2560, $2561, $0401, $0404, $2563, $0406, $0407, $2566, $2567, $2568, $2569, $256A, $0490, $040E, $00A9,
    $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433, $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
    $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432, $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
    $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413, $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
    $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412, $042C, $042B, $0417, $0428, $042D, $0429, $0427, $042A);
  CP28592ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $02D8, $0141, $00A4, $013D, $015A, $00A7, $00A8, $0160, $015E, $0164, $0179, $00AD, $017D, $017B,
    $00B0, $0105, $02DB, $0142, $00B4, $013E, $015B, $02C7, $00B8, $0161, $015F, $0165, $017A, $02DD, $017E, $017C,
    $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7, $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
    $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7, $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
    $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7, $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
    $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7, $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9);
  CP28593ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0126, $02D8, $00A3, $00A4, $F7F5, $0124, $00A7, $00A8, $0130, $015E, $011E, $0134, $00AD, $F7F6, $017B,
    $00B0, $0127, $00B2, $00B3, $00B4, $00B5, $0125, $00B7, $00B8, $0131, $015F, $011F, $0135, $00BD, $F7F7, $017C,
    $00C0, $00C1, $00C2, $F7F8, $00C4, $010A, $0108, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $F7F9, $00D1, $00D2, $00D3, $00D4, $0120, $00D6, $00D7, $011C, $00D9, $00DA, $00DB, $00DC, $016C, $015C, $00DF,
    $00E0, $00E1, $00E2, $F7FA, $00E4, $010B, $0109, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $F7FB, $00F1, $00F2, $00F3, $00F4, $0121, $00F6, $00F7, $011D, $00F9, $00FA, $00FB, $00FC, $016D, $015D, $02D9);
  CP28594ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $0138, $0156, $00A4, $0128, $013B, $00A7, $00A8, $0160, $0112, $0122, $0166, $00AD, $017D, $00AF,
    $00B0, $0105, $02DB, $0157, $00B4, $0129, $013C, $02C7, $00B8, $0161, $0113, $0123, $0167, $014A, $017E, $014B,
    $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E, $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $012A,
    $0110, $0145, $014C, $0136, $00D4, $00D5, $00D6, $00D7, $00D8, $0172, $00DA, $00DB, $00DC, $0168, $016A, $00DF,
    $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F, $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $012B,
    $0111, $0146, $014D, $0137, $00F4, $00F5, $00F6, $00F7, $00F8, $0173, $00FA, $00FB, $00FC, $0169, $016B, $02D9);
  CP28595ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0401, $0402, $0403, $0404, $0405, $0406, $0407, $0408, $0409, $040A, $040B, $040C, $00AD, $040E, $040F,
    $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417, $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
    $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427, $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
    $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437, $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
    $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447, $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
    $2116, $0451, $0452, $0453, $0454, $0455, $0456, $0457, $0458, $0459, $045A, $045B, $045C, $00A7, $045E, $045F);
  CP28596ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $F7C8, $F7C9, $F7CA, $00A4, $F7CB, $F7CC, $F7CD, $F7CE, $F7CF, $F7D0, $F7D1, $060C, $00AD, $F7D2, $F7D3,
    $F7D4, $F7D5, $F7D6, $F7D7, $F7D8, $F7D9, $F7DA, $F7DB, $F7DC, $F7DD, $F7DE, $061B, $F7DF, $F7E0, $F7E1, $061F,
    $F7E2, $0621, $0622, $0623, $0624, $0625, $0626, $0627, $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
    $0630, $0631, $0632, $0633, $0634, $0635, $0636, $0637, $0638, $0639, $063A, $F7E3, $F7E4, $F7E5, $F7E6, $F7E7,
    $0640, $0641, $0642, $0643, $0644, $0645, $0646, $0647, $0648, $0649, $064A, $064B, $064C, $064D, $064E, $064F,
    $0650, $0651, $0652, $F7E8, $F7E9, $F7EA, $F7EB, $F7EC, $F7ED, $F7EE, $F7EF, $F7F0, $F7F1, $F7F2, $F7F3, $F7F4);
  CP28597ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $02BD, $02BC, $00A3, $F7C2, $F7C3, $00A6, $00A7, $00A8, $00A9, $F7C4, $00AB, $00AC, $00AD, $F7C5, $2015,
    $00B0, $00B1, $00B2, $00B3, $0384, $0385, $0386, $00B7, $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
    $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
    $03A0, $03A1, $F7C6, $03A3, $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
    $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
    $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7, $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, $F7C7);
  CP28598ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $F79C, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00D7, $00AB, $00AC, $00AD, $00AE, $203E,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, $F79D,
    $F79E, $F79F, $F7A0, $F7A1, $F7A2, $F7A3, $F7A4, $F7A5, $F7A6, $F7A7, $F7A8, $F7A9, $F7AA, $F7AB, $F7AC, $F7AD,
    $F7AE, $F7AF, $F7B0, $F7B1, $F7B2, $F7B3, $F7B4, $F7B5, $F7B6, $F7B7, $F7B8, $F7B9, $F7BA, $F7BB, $F7BC, $2017,
    $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7, $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
    $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7, $05E8, $05E9, $05EA, $F7BD, $F7BE, $F7BF, $F7C0, $F7C1);
  CP28599ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF);
  CP28603ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $201D, $00A2, $00A3, $00A4, $201E, $00A6, $00A7, $00D8, $00A9, $0156, $00AB, $00AC, $00AD, $00AE, $00C6,
    $00B0, $00B1, $00B2, $00B3, $201C, $00B5, $00B6, $00B7, $00F8, $00B9, $0157, $00BB, $00BC, $00BD, $00BE, $00E6,
    $0104, $012E, $0100, $0106, $00C4, $00C5, $0118, $0112, $010C, $00C9, $0179, $0116, $0122, $0136, $012A, $013B,
    $0160, $0143, $0145, $00D3, $014C, $00D5, $00D6, $00D7, $0172, $0141, $015A, $016A, $00DC, $017B, $017D, $00DF,
    $0105, $012F, $0101, $0107, $00E4, $00E5, $0119, $0113, $010D, $00E9, $017A, $0117, $0123, $0137, $012B, $013C,
    $0161, $0144, $0146, $00F3, $014D, $00F5, $00F6, $00F7, $0173, $0142, $015B, $016B, $00FC, $017C, $017E, $2019);
  CP28605ToUnicodeMap: TSBCS_MAP = ( {generated with MultiByteToWideChar}
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $00A1, $00A2, $00A3, $20AC, $00A5, $0160, $00A7, $0161, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $017D, $00B5, $00B6, $00B7, $017E, $00B9, $00BA, $00BB, $0152, $0153, $0178, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF);
  {windows unsupported CP's}
  CP28600ToUnicodeMap: TSBCS_MAP = ( {not supported by MultiByteToWideChar -> www.unicode.org }
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $0112, $0122, $012A, $0128, $0136, $00A7, $013B, $0110, $0160, $0166, $017D, $00AD, $016A, $014A,
    $00B0, $0105, $0113, $0123, $012B, $0129, $0137, $00B7, $013C, $0111, $0161, $0167, $017E, $2015, $016B, $014B,
    $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E, $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $00CF,
    $00D0, $0145, $014C, $00D3, $00D4, $00D5, $00D6, $0168, $00D8, $0172, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F, $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $00EF,
    $00F0, $0146, $014D, $00F3, $00F4, $00F5, $00F6, $0169, $00F8, $0173, $00FA, $00FB, $00FC, $00FD, $00FE, $0138);
  CP28604ToUnicodeMap: TSBCS_MAP = ( {not supported by MultiByteToWideChar -> www.unicode.org }
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $1E02, $1E03, $00A3, $010A, $010B, $1E0A, $00A7, $1E80, $00A9, $1E82, $1E0B, $1EF2, $00AD, $00AE, $0178,
    $1E1E, $1E1F, $0120, $0121, $1E40, $1E41, $00B6, $1E56, $1E81, $1E57, $1E83, $1E60, $1EF3, $1E84, $1E85, $1E61,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $0174, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $1E6A, $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $0176, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $0175, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $1E6B, $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $0177, $00FF);
  CP28606ToUnicodeMap: TSBCS_MAP = ( {not supported by MultiByteToWideChar -> www.unicode.org }
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $0105, $0141, $20AC, $201E, $0160, $00A7, $0161, $00A9, $0218, $00AB, $0179, $00AD, $017A, $017B,
    $00B0, $00B1, $010C, $0142, $017D, $201D, $00B6, $00B7, $017E, $010D, $0219, $00BB, $0152, $0153, $0178, $017C,
    $00C0, $00C1, $00C2, $0102, $00C4, $0106, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $0110, $0143, $00D2, $00D3, $00D4, $0150, $00D6, $015A, $0170, $00D9, $00DA, $00DB, $00DC, $0118, $021A, $00DF,
    $00E0, $00E1, $00E2, $0103, $00E4, $0107, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $0111, $0144, $00F2, $00F3, $00F4, $0151, $00F6, $015B, $0171, $00F9, $00FA, $00FB, $00FC, $0119, $021B, $00FF);

implementation

{$IF defined(FAST_MOVE) and not defined(PatchSystemMove)}uses ZFastCode;{$IFEND}

const
  dsMaxRStringSize = 8192; { Maximum string field size declared in DB.pas }
  dsMaxWStringSize = dsMaxRStringSize shr 1;

{$IFDEF FPC} {$PUSH}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}
function ZRawToUnicode(const S: RawByteString; const CP: Word): UnicodeString;
begin
  if Pointer(S) = nil
  then Result := ''
  else Result := PRawToUnicode(Pointer(S), PLengthInt(NativeUInt(S) - StringLenOffSet)^{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, CP);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  EgonHugeist:
  my fast Byte to Word shift without a lookup table
  eg. USACII7/LATIN 1 cp's
}
procedure MapByteToUTF16(Source: PByteArray; SourceBytes: LengthInt; Dest: PWordArray);
var
  PEnd: PAnsiChar;
begin
  PEnd := PAnsiChar(Source)+SourceBytes-8;
  while PAnsiChar(Source) < PEnd do //making a octed processing loop
  begin
    Dest[0] := Source[0];
    Dest[1] := Source[1];
    Dest[2] := Source[2];
    Dest[3] := Source[3];
    Dest[4] := Source[4];
    Dest[5] := Source[5];
    Dest[6] := Source[6];
    Dest[7] := Source[7];
    Inc(PWideChar(Dest), 8);
    Inc(PAnsiChar(Source), 8);
  end;
  Inc(PEnd, 8);
  while PAnsiChar(Source) < PEnd do //processing final bytes
  begin
    Dest[0] := Source[0];
    inc(PAnsiChar(Source));
    inc(PWideChar(Dest));
  end;
//  Dest[0] := Ord(#0);
end;

{**
  EgonHugeist:
  my fast Byte to Word shift with a lookup table
  eg. all single byte encodings
}
{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "AnsiSBCSToUTF16" marked as inline is not inlined}{$ENDIF}
procedure AnsiSBCSToUTF16(Source: PAnsichar; SourceBytes: LengthInt;
  var Dest: UnicodeString; SBCS_MAP: PSBCS_MAP);
begin
  {$IFDEF PWIDECHAR_IS_PUNICODECHAR}
  if (Pointer(Dest) = nil) or//empty
     ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ <> 1) or { unique string ? }
     (SourceBytes <> {%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^) then { length as expected ? }
  {$ELSE}
  if Length(Dest) <> LengthInt(SourceBytes) then //WideString isn't ref counted
  {$ENDIF}
  begin
    Dest := '';
    System.SetLength(Dest, SourceBytes);
  end;
  AnsiSBCSToUTF16(Pointer(Source), Pointer(Dest), SBCS_MAP, SourceBytes);
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

{**
  EgonHugeist:
  my fast Byte to Word move with a lookup table
  eg. all single byte encodings
}
procedure AnsiSBCSToUTF16(Source: PByteArray; Dest: PWordArray;
  SBCS_MAP: PSBCS_MAP; SourceBytes: LengthInt);
var
  PEnd: PAnsiChar;
begin
  PEnd := PAnsiChar(Source)+SourceBytes-8;
  while PAnsiChar(Source) < PEnd do begin//making a octed processing loop
    Dest[0] := SBCS_MAP[Source[0]];
    Dest[1] := SBCS_MAP[Source[1]];
    Dest[2] := SBCS_MAP[Source[2]];
    Dest[3] := SBCS_MAP[Source[3]];
    Dest[4] := SBCS_MAP[Source[4]];
    Dest[5] := SBCS_MAP[Source[5]];
    Dest[6] := SBCS_MAP[Source[6]];
    Dest[7] := SBCS_MAP[Source[7]];
    inc(PAnsiChar(Source),8);
    inc(PWideChar(Dest),8);
  end;
  Inc(PEnd, 8);
  while PAnsiChar(Source) < PEnd do //processing final bytes
  begin
    Dest[0] := SBCS_MAP[Source[0]];
    inc(PWideChar(Dest));
    inc(PAnsiChar(Source));
  end;
  //Dest[0] := Ord(#0);
end;

procedure AnsiSBCSToUTF16(Source: PAnsichar; SourceBytes: LengthInt;
  const MapProc: TSBCSMapProc; var Dest: UnicodeString);
begin
  {$IFDEF PWIDECHAR_IS_PUNICODECHAR}
  if (Pointer(Dest) = nil) or//empty
     ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ <> 1) or { unique string ? }
     (SourceBytes <> {%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^) then { length as expected ? }
  {$ELSE}
  if Length(Dest) <> SourceBytes then //WideString isn't ref counted
  {$ENDIF}
  begin
    Dest := '';
    System.SetLength(Dest, SourceBytes);
  end;
  MapProc(Pointer(Source), SourceBytes, Pointer(Dest));
end;

procedure AnsiMBCSToUTF16(Source: PAnsichar; SourceBytes: LengthInt;
  const MapProc: TMBCSMapProc; var Dest: UnicodeString);
var
  Buf: array[0..dsMaxWStringSize] of WideChar; //static buf to avoid mem allocs
  NewLen: LengthInt;
begin
  if SourceBytes > dsMaxWStringSize then begin
    {$IFDEF PWIDECHAR_IS_PUNICODECHAR}
    if (Pointer(Dest) = nil) or//empty
       ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ <> 1) or { unique string ? }
       (SourceBytes <> {%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^) then { length as expected ? }
    {$ELSE}
    if Length(Dest) <> SourceBytes then //WideString isn't ref counted
    {$ENDIF}
    begin
      Dest := '';
      System.SetLength(Dest, SourceBytes);
    end;
    NewLen := MapProc(Source, SourceBytes, Pointer(Dest));
    if NewLen <> Length(Dest) then
      SetLength(Dest, NewLen);
  end else begin
    NewLen := MapProc(Source, SourceBytes, @Buf[0]);
    System.SetString(Dest, PWideChar(@Buf[0]), NewLen);
  end;
end;

{ UTF8ToWideChar and its's used constant original written by Arnaud Bouchez
  see: syncommons.pas in mORMot framework www.synopse.info
  Changes:
    - replace PUTF8Char to PAnsichar
    - replace Returning bytes by WideChars and a LengthInt-Type
    - replace Dest^ := WideChar(c) casts by using PWord(Dest)^ := C; expresions
      which is imbelievable faster with FPC (they are spooling all such casts
      through the WideStringManager -> horrable performance drop )
    - omit StrLen determination we don't need here
    - add 4Byte ASCII quads with SHA optimization again if 00..7F was found in iteration loop
    - omit nil check of Value.p and dest since we use it only if we've real data
    - change arrays to packed arrays which are faster handled by ide
  Performance: faster than UTF8Decode but not faster than MultiByteToWideChar
  with D7 $ FPCso we exclude this function here
}
// some constants used for UTF-8 conversion, including surrogates
const
  UTF16_HISURROGATE_MIN = $d800;
  UTF16_HISURROGATE_MAX = $dbff;
  UTF16_LOSURROGATE_MIN = $dc00;
  UTF16_LOSURROGATE_MAX = $dfff;
  UTF8_EXTRABYTES: packed array[$80..$ff] of byte = (
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,0,0);

  UTF8_EXTRA: packed array[0..6] of record
    offset, minimum: cardinal;
  end = ( // http://floodyberry.wordpress.com/2007/04/14/utf-8-conversion-tricks
    (offset: $00000000;  minimum: $00010000),
    (offset: $00003080;  minimum: $00000080),
    (offset: $000e2080;  minimum: $00000800),
    (offset: $03c82080;  minimum: $00010000),
    (offset: $fa082080;  minimum: $00200000),
    (offset: $82082080;  minimum: $04000000),
    (offset: $00000000;  minimum: $04000000));
  //UTF8_EXTRA_SURROGATE = 3;
  UTF8_FIRSTBYTE: packed array[2..6] of byte = ($c0,$e0,$f0,$f8,$fc);

function UTF8ToWideChar(Source: PAnsichar; SourceBytes: LengthInt; Dest: PWideChar): NativeUInt;
// faster than System.UTF8Decode()
var c: cardinal;
    begd: pWideChar;
    endSource, endSourceBy4: PAnsiChar;
    i,extra: integer;
label Quit, By1, By4;
begin
  begd := dest;
  endSource := Source+SourceBytes;
  endSourceBy4 := endSource-4;
  if SourceBytes < 4 then
    goto By1;
  repeat
    // first handle 7 bit ASCII chars, by quad (Sha optimization)
By4:  c := PCardinal(Source)^;
      if c and $80808080<>0 then
        goto By1; // break on first non ASCII quad
      inc(Source,4);
      PCardinal(dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
      c := c shr 16;
      PCardinal(dest+2)^ := (c shl 8 or c) and $00ff00ff;
      inc(dest,4);
    until Source>EndSourceBy4;
  if Source<endSource then
    repeat
By1:  c := byte(Source^);
      inc(Source);
      if c and $80=0 then begin
        PWord(dest)^ := c; // much faster than dest^ := WideChar(c) for FPC
        inc(dest);
        if ({%H-}NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
        if Source<endSource then continue else break;
      end;
      extra := UTF8_EXTRABYTES[c];
      if (extra=0) or (Source+extra>endSource) then break;
      for i := 1 to extra do begin
        if byte(Source^) and $c0<>$80 then
          goto Quit; // invalid input content
        c := c shl 6+byte(Source^);
        inc(Source);
      end;
      with UTF8_EXTRA[extra] do begin
        dec(c,offset);
        if c<minimum then
          break; // invalid input content
      end;
      if c<=$ffff then begin
        PWord(dest)^ := c;
        inc(dest);
        if ({%H-}NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
        if Source<endSource then continue else break;
      end;
      dec(c,$10000); // store as UTF-16 surrogates
      PWordArray(dest)[0] := c shr 10  +UTF16_HISURROGATE_MIN;
      PWordArray(dest)[1] := c and $3FF+UTF16_LOSURROGATE_MIN;
      inc(dest,2);
      if ({%H-}NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
      if Source>=endSource then break;
    until false;
Quit:
  result := ({%H-}NativeUInt(dest)-{%H-}NativeUInt(begd)) shr 1; // dest-begd return codepoint length
  //PWord(dest)^ := Ord(#0); // always append a WideChar(0) to the end of the buffer
end;

function UTF8AsUTF16Words(Source: PAnsichar; SourceBytes: LengthInt): NativeUInt;
// faster than System.UTF8Decode()
var c: cardinal;
    endSource, endSourceBy4: PAnsiChar;
    i,extra: integer;
label Quit, By1, By4;
begin
  Result := 0;
  if (Source = nil) or (SourceBytes = 0) then
    Exit;
  endSource := Source+SourceBytes;
  endSourceBy4 := endSource-4;
  if SourceBytes < 4 then
    goto By1;
  repeat
    // first handle 7 bit ASCII chars, by quad (Sha optimization)
By4:  c := PCardinal(Source)^;
      if c and $80808080<>0 then
        goto By1; // break on first non ASCII quad
      inc(Source,4);
      inc(Result,4);
    until Source>EndSourceBy4;
  if Source<endSource then
    repeat
By1:  c := byte(Source^);
      inc(Source);
      if c and $80=0 then begin
        inc(Result);
        if ({%H-}NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
        if Source<endSource then continue else break;
      end;
      extra := UTF8_EXTRABYTES[c];
      if (extra=0) or (Source+extra>endSource) then break;
      for i := 1 to extra do begin
        if byte(Source^) and $c0<>$80 then
          goto Quit; // invalid input content
        c := c shl 6+byte(Source^);
        inc(Source);
      end;
      with UTF8_EXTRA[extra] do begin
        dec(c,offset);
        if c<minimum then
          break; // invalid input content
      end;
      if c<=$ffff then begin
        inc(Result);
        if ({%H-}NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
        if Source<endSource then continue else break;
      end;
      inc(Result,2);
      if ({%H-}NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
      if Source>=endSource then break;
    until false;
Quit:
end;
//(*
function CountOfUtf8Chars(Source: PAnsichar; SourceBytes: NativeUint): NativeUint;
// faster than System.UTF8Decode()
var c: cardinal;
    endSource, endSourceBy4: PAnsiChar;
    i,extra: integer;
label By1, By4;
begin
  Result := 0;
  if (Source = nil) or (SourceBytes = 0) then
    Exit;
  endSource := Source+SourceBytes;
  endSourceBy4 := endSource-4;
  if SourceBytes < 4 then
    goto By1;
  repeat
    // first handle 7 bit ASCII chars, by quad (Sha optimization)
By4:  c := PCardinal(Source)^;
      if c and $80808080<>0 then
        goto By1; // break on first non ASCII quad
      inc(Source,4);
      inc(Result,4);
    until Source>EndSourceBy4;
  if Source<endSource then
    repeat
By1:  c := byte(Source^);
      inc(Source);
      if c and $80=0 then begin
        inc(Result);
        if ({%H-}NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
        if Source<endSource then continue else break;
      end;
      extra := UTF8_EXTRABYTES[c];
      if (extra=0) or (Source+extra>endSource) then break;
      for i := 1 to extra do begin
        if byte(Source^) and $c0<>$80 then
          Exit; // invalid input content
        c := c shl 6+byte(Source^);
        inc(Source);
      end;
      with UTF8_EXTRA[extra] do begin
        dec(c,offset);
        if c<minimum then
          break; // invalid input content
      end;
      if c<=$ffff then begin
        inc(Result);
        if ({%H-}NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
        if Source<endSource then continue else break;
      end;
      inc(Result);
      if ({%H-}NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
      if Source>=endSource then break;
    until false;
end;

{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF} // uses pointer maths
function UTF8ToWideChar(source: PAnsiChar; dest: PWideChar; sourceBytes, DestWords: LengthInt): NativeUInt;
// faster than System.UTF8Decode()
var c: cardinal;
    begd, endDest: pWideChar;
    endSource, endSourceBy4: PAnsiChar;
    i,extra: integer;
label Quit, By1, By4;
begin
  begd := dest;
  endSource := Source+SourceBytes;
  endSourceBy4 := endSource-4;
  endDest := Dest+DestWords;
  if SourceBytes < 4 then
    goto By1;
  repeat
    // first handle 7 bit ASCII chars, by quad (Sha optimization)
By4:  c := PCardinal(Source)^;
      if (c and $80808080<>0) or (endDest-4 < Dest) then
        goto By1; // break on first non ASCII quad
      inc(Source,4);
      PCardinal(dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
      c := c shr 16;
      PCardinal(dest+2)^ := (c shl 8 or c) and $00ff00ff;
      inc(dest,4);
    until Source>EndSourceBy4;
  if Source<endSource then
    repeat
By1:  c := byte(Source^);
      inc(Source);
      if c and $80=0 then begin
        PWord(dest)^ := c; // much faster than dest^ := WideChar(c) for FPC
        inc(dest);
        if (NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
        if (Source<endSource) and (dest<endDest) then continue else break;
      end;
      extra := UTF8_EXTRABYTES[c];
      if (extra=0) or (Source+extra>endSource) then break;
      for i := 1 to extra do begin
        if byte(Source^) and $c0<>$80 then
          goto Quit; // invalid input content
        c := c shl 6+byte(Source^);
        inc(Source);
      end;
      with UTF8_EXTRA[extra] do begin
        dec(c,offset);
        if c<minimum then
          break; // invalid input content
      end;
      if c<=$ffff then begin
        PWord(dest)^ := c;
        inc(dest);
        if (NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
        if (Source<endSource) and (dest<endDest) then continue else break;
      end;
      dec(c,$10000); // store as UTF-16 surrogates
      PWordArray(dest)[0] := c shr 10  +UTF16_HISURROGATE_MIN;
      PWordArray(dest)[1] := c and $3FF+UTF16_LOSURROGATE_MIN;
      inc(dest,2);
      if (NativeUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4;
      if (source>=endsource) or (dest>=endDest) then break;
    until false;
Quit:
  result := (NativeUInt(dest)-NativeUInt(begd)) shr 1; // dest-begd return codepoint length
  if Result < NativeUint(DestWords) then
    PWord(Dest)^ := 0;

end;
{$IFDEF FPC} {$POP} {$ENDIF} // uses pointer maths

{**
  RawUnicodeToUtf8 and its's used constant original written by Arnaud Bouchez
  see: syncommons.pas in mORMot framework www.synopse.info
  convert a RawUnicode UTF-16 PWideChar into a UTF-8 buffer
  orgiginal named as RawUnicodeToUtf8()
   - replace system.UnicodeToUtf8 implementation, which is rather slow
   since Delphi 2009+
   - will append a trailing #0 to the ending PUTF8Char, unless
   ccfNoTrailingZero is set
   - if ccfReplacementCharacterForUnmatchedSurrogate is set, this function will identify
   unmatched surrogate pairs and replace them with EF BF BD / FFFD  Unicode
   Replacement character - see https://en.wikipedia.org/wiki/Specials_(Unicode_block)
Changes by EgonHugeist:
    - replace PUTF8Char to PAnsichar
    - replace PtrInt to NativeUInt
    - add hard word cast in the ascii-pair loop -> range-checks did make noise here
    - replace the ansichar casts with Byte/word  values -> nextgen
    - added three labels ( loop_ascii_pairs & done & next ) to loop in code and test ascii-pairs
      after each convertion again. so i commented the main repeat loop and all continue/break tests
}
//function RawUnicodeToUtf8(Dest: PAnsiChar; DestLen: NativeUint; Source: PWideChar;
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF} // uses pointer maths
function PUnicodeToUtf8Buf(Dest: PAnsiChar; DestLen: NativeUint; Source: PWideChar;
  SourceLen: NativeUint; Flags: TCharConversionFlags): NativeUint;
var c, i: Cardinal;
    Tail: PWideChar;
    j: integer;
label unmatch, loop_ascii_pairs, next, Done;
begin
  result := NativeUint(Dest);
  inc(DestLen,NativeUint(Dest));
  if (Source<>nil) and (Dest<>nil) then begin
    // first handle 7 bit ASCII WideChars, by pairs (Sha optimization)
    SourceLen := SourceLen*2+NativeUint(Source);
    Tail := PWideChar(SourceLen)-2;
loop_ascii_pairs:
    if (NativeUint(Dest)<DestLen) and (Source<=Tail) then
      repeat
        c := PCardinal(Source)^;
        if c and $ff80ff80<>0 then
          break; // break on first non ASCII pair
        c := c shr 8 or c;
        PWord(Dest)^ := Word(c);//EH: added this hard cast because of RangeCheck errors
        inc(Source,2);
        inc(Dest,2);
      until (Source>Tail) or (NativeUint(Dest)>=DestLen);
    // generic loop, handling one UCS4 char per iteration
    if (NativeUint(Dest)<DestLen) and (NativeUint(Source)<SourceLen) then
    begin//repeat
      // inlined UTF16CharToUtf8() with bufferoverlow check and $FFFD on unmatch
next: c := cardinal(Source^);
      inc(Source);
      case c of
        0..$7f: begin
            PByte(Dest)^ := Byte(c);
            inc(Dest);
            //if (NativeUint(Dest)<DestLen) and (NativeUint(Source)<SourceLen) then continue else break;
            if (NativeUint(Dest)<DestLen) and (NativeUint(Source)<SourceLen) //test if end is reached
            then if PByte(Source+1)^ > $7f //next no ascii?
              then goto next
              else goto loop_ascii_pairs
            else goto done;
          end;
        UTF16_HISURROGATE_MIN..UTF16_HISURROGATE_MAX:
          if (NativeUint(Source)>=SourceLen) or
             ((cardinal(Source^)<UTF16_LOSURROGATE_MIN) or (cardinal(Source^)>UTF16_LOSURROGATE_MAX)) then begin
unmatch:    if (NativeUint(Dest+3)>DestLen) or not (ccfReplacementCharacterForUnmatchedSurrogate in Flags)
            then goto Done;//break;
            PWord(Dest)^ := $BFEF;
            PByte(Dest+2)^ := $BD;
            inc(Dest,3);
            goto loop_ascii_pairs;//if (NativeUint(Dest)<DestLen) and (NativeUint(Source)<SourceLen) then continue else break;
          end else begin
            c := ((c-$D7C0)shl 10)+(cardinal(Source^) xor UTF16_LOSURROGATE_MIN);
            inc(Source);
          end;
        UTF16_LOSURROGATE_MIN..UTF16_LOSURROGATE_MAX:
          if (NativeUint(Source)>=SourceLen) or
             ((cardinal(Source^)<UTF16_HISURROGATE_MIN) or (cardinal(Source^)>UTF16_HISURROGATE_MAX)) then
            goto unmatch
          else begin
            c := ((cardinal(Source^)-$D7C0)shl 10)+(c xor UTF16_LOSURROGATE_MIN);
            inc(Source);
          end;
      end; // now c is the UTF-32/UCS4 code point
      case c of
        0..$7ff: i := 2;
        $800..$ffff: i := 3;
        $10000..$1FFFFF: i := 4;
        $200000..$3FFFFFF: i := 5;
        else i := 6;
      end;
      if NativeUint(Dest)+i>DestLen then
        goto Done;//break;
      for j := i-1 downto 1 do begin
        PByte(Dest+j)^ := Byte((c and $3f)+$80);
        c := c shr 6;
      end;
      PByte(Dest)^ := Byte(Byte(c) or UTF8_FIRSTBYTE[i]);
      inc(Dest,i);
      goto loop_ascii_pairs;//if (NativeUint(Dest)<DestLen) and (NativeUint(Source)<SourceLen) then continue else break;
    end; //until false;
Done:
    if not (ccfNoTrailingZero in Flags) then
      Dest^ := #0;
  end;
  result := NativeUint(Dest)-result;
end;
{$IFDEF FPC} {$POP} {$ENDIF} // uses pointer maths

function PRawToPRawBuf(Source, Dest: PAnsiChar; SourceBytes, MaxDestBytes: LengthInt; SrcCP, DestCP: Word): LengthInt;
var
  wBuf: array[0..dsMaxWStringSize] of WideChar;
  P: Pointer;
begin
  Result := 0;
  if (SourceBytes <> 0) and (Source <> nil) and (Dest <> nil ) then
    if (SrcCP = DestCP) then begin
      Result := Min(SourceBytes, MaxDestBytes);
      if Source <> Dest then
        {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Source^, Dest^, Result);
    end else begin
      while (SourceBytes >= 4) and (MaxDestBytes >= 4) and (PCardinal(Source)^ and $80808080 = 0) do begin//ascii quads <= #127
        PCardinal(Dest)^ := PCardinal(Source)^;
        Inc(Source, 4); Inc(Dest, 4); Inc(Result, 4);
        Dec(SourceBytes, 4); Dec(MaxDestBytes, 4);
      end;
      while (SourceBytes > 0) and (MaxDestBytes > 0) and (PByte(Source)^ and $80 = 0) do begin//ascii <= #127
        PByte(Dest)^ := PByte(Source)^;
        Inc(Source); Inc(Dest); Inc(Result);
        Dec(SourceBytes); Dec(MaxDestBytes);
      end;
      if (SourceBytes <= MaxDestBytes) and (SourceBytes > 0) then begin
        if SourceBytes <= dsMaxWStringSize
        then P := @wBuf[0]
        else GetMem(P, (SourceBytes+1) shl 1);
        SourceBytes := PRaw2PUnicodeBuf(Source, P, sourceBytes, SrcCP);
        Result := Result + PUnicode2PRawBuf(PWideChar(P), Dest, SourceBytes, MaxDestBytes, DestCP);
        if P <> @wBuf[0] then
          FreeMem(P);
      end;
    end;
end;

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "IsMBCSCodePage" marked as inline is not inlined}{$ENDIF}
procedure PRawToRawConvert(Source: PAnsiChar; SourceBytes: LengthInt;
  SrcCP, DestCP: Word; var Result: RawByteString);
var L, NL: LengthInt;
  Dest: PAnsiChar;
  sIsMBCSCodePage, dIsMBCSCodePage: Boolean;
label Jmp;
begin
  if (Source = nil) or (SourceBytes = 0) then
    Result := EmptyRaw
  else if SrcCP = DestCP then
    ZSetString(Source, SourceBytes, Result {$IFDEF WITH_RAWBYTESTRING},DestCP{$ENDIF})
  else begin
    sIsMBCSCodePage := IsMBCSCodePage(SrcCP);
    dIsMBCSCodePage := IsMBCSCodePage(DestCP);
    if sIsMBCSCodePage and not dIsMBCSCodePage then
      NL := SourceBytes
    else if not sIsMBCSCodePage and dIsMBCSCodePage then
      NL := SourceBytes shl 2
    else if not sIsMBCSCodePage and not dIsMBCSCodePage then
      NL := SourceBytes
    else
      NL := SourceBytes shl 1;
Jmp:ZSetString(nil, NL, Result {$IFDEF WITH_RAWBYTESTRING},DestCP{$ENDIF});
    Dest := Pointer(Result);
    L := PRawToPRawBuf(Source, Dest, SourceBytes, NL, SrcCP, DestCP);
    if L < NL then
      SetLength(Result, L{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF})
    else if L > NL then begin
      NL := L;
      goto Jmp;
    end;
  end;
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "IsMBCSCodePage" marked as inline is not inlined}{$ENDIF}
function PRawToUnicode(Source: PAnsiChar; SourceBytes: LengthInt;
  CP: Word): UnicodeString;
var
  wlen: LengthInt;
  wBuf: array[0..dsMaxWStringSize] of WideChar;
begin
  Result := '';
  if (SourceBytes > 0) and (Source <> nil) then begin
    //test multibyte encodings:
    if IsMBCSCodePage(cp) then begin
      if SourceBytes <= dsMaxWStringSize then begin //can we use a static buf? -> avoid memrealloc for the Result String
        wlen := PRaw2PUnicodeBuf(Source, @wBuf[0], sourceBytes, CP);
        System.SetString(Result, PWideChar(@wBuf[0]), wLen);
      end else if CP = zCP_UTF8 then begin
        wlen := UTF8AsUTF16Words(Source, sourceBytes); //return exactlen
        System.SetString(Result, nil, wLen);
        UTF8ToWideChar(Source, SourceBytes, Pointer(Result));
      end else begin //nope Buf to small
        System.SetString(Result, nil, SourceBytes);
        wlen := PRaw2PUnicodeBuf(Source, Pointer(Result), sourceBytes, CP);
        if wlen <> Length(Result) then
          SetLength(Result, wlen);
      end;
    end else begin //single byte encoding -> encode into result directly
      System.SetString(Result, nil, SourceBytes);
      PRaw2PUnicodeBuf(Source, Pointer(Result), sourceBytes, CP);
    end;
  end;
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "IsMBCSCodePage" marked as inline is not inlined}{$ENDIF}
procedure PRawToUnicode(Source: PAnsiChar; SourceBytes: LengthInt; CP: Word; var Result: UnicodeString);
var
  wlen: LengthInt;
  wBuf: array[0..dsMaxWStringSize] of WideChar;
begin
  if (SourceBytes = 0) or (Source = nil) then
    Result := ''
  else if IsMBCSCodePage(cp) then begin
    if SourceBytes <= dsMaxWStringSize then begin //can we use a static buf? -> avoid memrealloc for the Result String
      wlen := PRaw2PUnicodeBuf(Source, @wBuf[0], sourceBytes, CP);
      ZSetString(nil, wlen, Result);
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(wBuf[0], Pointer(Result)^, wlen shl 1);
    end else if CP = zCP_UTF8 then begin
      wlen := UTF8AsUTF16Words(Source, sourceBytes); //return exactlen
      ZSetString(nil, wlen, Result);
      UTF8ToWideChar(Source, SourceBytes, Pointer(Result));
    end else begin //nope Buf to small
      ZSetString(nil, SourceBytes, Result);
      wlen := PRaw2PUnicodeBuf(Source, Pointer(Result), sourceBytes, CP);
      if wlen <> Length(Result) then
        SetLength(Result, wlen);
    end;
  end else begin //single byte encoding -> encode into result directly
    ZSetString(nil, SourceBytes, Result);
    PRaw2PUnicodeBuf(Source, Pointer(Result), sourceBytes, CP);
  end;
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

{**
  convert a raw encoded string into a uniocde buffer
  Dest reserved space must be minimum SourceBytes + trailing #0 in codepoints
}
function PRaw2PUnicodeBuf(Source: PAnsiChar; Dest: Pointer;
  SourceBytes: LengthInt; CP: Word): LengthInt;
var
  C: Cardinal;
  PEnd: PAnsiChar;
  wlen, DestWords: LengthInt;
  {$IF not defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}
    {$IFNDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER}
    S: RawByteString;
    {$ENDIF}
  W: UnicodeString;
  {$IFEND}
  SBCS_MAP: PSBCS_MAP;
label A2U;
begin
  if Dest = nil then begin
    Result := 0;
    exit;
  end;
  Result := SourceBytes;
  if (SourceBytes = 0) or (Source = nil) then
    //PWord(Dest)^ := Ord(#0)
  else begin
    SBCS_MAP := nil;
    case CP of
      zCP_DOS437:         SBCS_MAP := @CP437ToUnicodeMap;
      zCP_DOS708:         SBCS_MAP := @CP708ToUnicodeMap;
      zCP_DOS720:         SBCS_MAP := @CP720ToUnicodeMap;
      zCP_DOS737:         SBCS_MAP := @CP737ToUnicodeMap;
      zCP_DOS775:         SBCS_MAP := @CP775ToUnicodeMap;
      zCP_DOS850:         SBCS_MAP := @CP850ToUnicodeMap;
      zCP_DOS852:         SBCS_MAP := @CP852ToUnicodeMap;
      zCP_DOS855:         SBCS_MAP := @CP855ToUnicodeMap;
      zCP_DOS857:         SBCS_MAP := @CP857ToUnicodeMap;
      zCP_DOS858:         SBCS_MAP := @CP858ToUnicodeMap;
      zCP_DOS860:         SBCS_MAP := @CP860ToUnicodeMap;
      zCP_DOS861:         SBCS_MAP := @CP861ToUnicodeMap;
      zCP_DOS862:         SBCS_MAP := @CP862ToUnicodeMap;
      zCP_DOS863:         SBCS_MAP := @CP863ToUnicodeMap;
      zCP_DOS864:         SBCS_MAP := @CP864ToUnicodeMap;
      zCP_DOS865:         SBCS_MAP := @CP865ToUnicodeMap;
      zCP_DOS866:         SBCS_MAP := @CP866ToUnicodeMap;
      zCP_DOS869:         SBCS_MAP := @CP869ToUnicodeMap;
      zCP_WIN874:         SBCS_MAP := @CP874ToUnicodeMap;
      zCP_WIN1250:        SBCS_MAP := @CP1250ToUnicodeMap;
      zCP_WIN1251:        SBCS_MAP := @CP1251ToUnicodeMap;
      zCP_WIN1252:        SBCS_MAP := @CP1252ToUnicodeMap;
      zCP_WIN1253:        SBCS_MAP := @CP1253ToUnicodeMap;
      zCP_WIN1254:        SBCS_MAP := @CP1254ToUnicodeMap;
      zCP_WIN1255:        SBCS_MAP := @CP1255ToUnicodeMap;
      zCP_WIN1256:        SBCS_MAP := @CP1256ToUnicodeMap;
      zCP_WIN1257:        SBCS_MAP := @CP1257ToUnicodeMap;
      zCP_WIN1258:        SBCS_MAP := @CP1258ToUnicodeMap;
      zCP_macintosh:      SBCS_MAP := @CP10000ToUnicodeMap;
      zCP_x_mac_ce:       SBCS_MAP := @CP10029ToUnicodeMap;
      zCP_x_IA5_Swedish:  SBCS_MAP := @CP20107ToUnicodeMap;
      zCP_KOI8R:          SBCS_MAP := @CP20866ToUnicodeMap;
      zCP_us_ascii:       SBCS_MAP := @CP20127ToUnicodeMap;
      zCP_KOI8U:          SBCS_MAP := @CP21866ToUnicodeMap;
      zCP_L1_ISO_8859_1:  begin
              MapByteToUTF16(Pointer(Source), SourceBytes, Dest);
              Exit;
            end;
      zCP_L2_ISO_8859_2:  SBCS_MAP := @CP28592ToUnicodeMap;
      zCP_L3_ISO_8859_3:  SBCS_MAP := @CP28593ToUnicodeMap;
      zCP_L4_ISO_8859_4:  SBCS_MAP := @CP28594ToUnicodeMap;
      zCP_L5_ISO_8859_5:  SBCS_MAP := @CP28595ToUnicodeMap;
      zCP_L6_ISO_8859_6:  SBCS_MAP := @CP28596ToUnicodeMap;
      zCP_L7_ISO_8859_7:  SBCS_MAP := @CP28597ToUnicodeMap;
      zCP_L8_ISO_8859_8:  SBCS_MAP := @CP28598ToUnicodeMap;
      zCP_L5_ISO_8859_9:  SBCS_MAP := @CP28599ToUnicodeMap;
      zCP_L7_ISO_8859_13: SBCS_MAP := @CP28603ToUnicodeMap;
      zCP_L9_ISO_8859_15: SBCS_MAP := @CP28605ToUnicodeMap;
      {not supported codepages by Windows MultiByteToWideChar}
      zCP_L6_ISO_8859_10: SBCS_MAP := @CP28600ToUnicodeMap;
      zCP_L8_ISO_8859_14: SBCS_MAP := @CP28604ToUnicodeMap;
      zCP_L10_ISO_8859_16:SBCS_MAP := @CP28606ToUnicodeMap;
      zCP_UTF8: begin
          Result := UTF8ToWideChar(Source, SourceBytes, Dest);
          Exit
        end;
      (* remaing fast conversion for MBCS encodings
      zCP_MSWIN921 = 921;
      zCP_MSWIN923 = 923;
      zCP_SHIFTJS = 932; {ANSI/OEM Japanese; Japanese (Shift-JIS)}
      zCP_GB2312 = 936; {ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312)}
      zCP_EUCKR = 949; {ANSI/OEM Korean (Unified Hangul Code)}
      zCP_Big5 = 950; {ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5)}
      ZCP_JOHAB = 1361; {Korean (Johab)}
      zCP_EUC_JP = 20932; {Japanese (JIS 0208-1990 and 0121-1990)}

      zCP_csISO2022JP = 50221;	{ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana)}
      zCP_euc_JP_win = 51932; {EUC Japanese}
      zCP_EUC_CN = 51936; {EUC Simplified Chinese; Chinese Simplified (EUC)}
      zCP_euc_kr = 51949; {EUC Korean}
      zCP_GB18030 = 54936;	{Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030)}
      zCP_UTF7 = 65000;
      *)
    end;
    if SBCS_MAP <> nil
    then AnsiSBCSToUTF16(Pointer(Source), Dest, SBCS_MAP, SourceBytes)
    else begin//for these where we do not have a conversion routine...
      PEnd := Source+SourceBytes-4;
      {first handle leading ASCII if possible }
      while (Source < PEnd ) and (PCardinal(Source)^ and $80808080 = 0) do
      begin
        C := PCardinal(Source)^;
        PCardinal(Dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
        c := c shr 16;
        PCardinal(PWideChar(Dest)+2)^ := (c shl 8 or c) and $00ff00ff;
        inc(Source,4);
        inc(PWideChar(Dest),4);
      end;
      inc(PEnd, 4);
      while (Source < PEnd) and (PByte(Source)^ and $80 = 0) do
      begin
        PWord(Dest)^ := PByte(Source)^; //Shift Byte to Word
        inc(Source);
        inc(PWideChar(Dest));
      end;
      if (Source < PEnd) then begin//convert remaining characters with codepage agnostic
        wlen := PEnd-Source;
        if CP = zCP_NONE then
          case ZDetectUTF8Encoding(Source, PEnd-Source) of
            etUTF8: begin
                      DestWords := UTF8ToWideChar(Source, wlen, Dest);
                      goto A2U;
                    end;
            else
              if (ZOSCodePage = zCP_UTF8) then begin //random success, we don't know ANY proper CP here
                MapByteToUTF16(Pointer(Source), wlen, Dest);
                Exit;
              end else
                CP := ZOSCodePage; //still a random success here!
          end;
        {$IFDEF WITH_UNICODEFROMLOCALECHARS}
        DestWords := UnicodeFromLocaleChars(CP, 0, Pointer(Source), wlen, Dest, wlen);
        {$ELSE}
          {$IFDEF MSWINDOWS}
          DestWords := MultiByteToWideChar(CP, 0, Source, wlen, Dest, wlen); //Convert Ansi to Wide with supported Chars
          {$ELSE}
            {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
            W := '';
            WidestringManager.Ansi2UnicodeMoveProc(Source, CP, W, wlen);
            {$ELSE}
            ZSetString(Source, wlen, S);
            W := UnicodeString(S); //random success
            {$ENDIF}
            DestWords := Min(Length(W), wlen);
            if DestWords > 0 then
              {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(W)^, Dest^, DestWords shl 1);
          {$ENDIF}
        {$ENDIF}
A2U:      Result := SourceBytes - wlen + DestWords;
        //Inc(PWideChar(Dest), DestWords);
      end;
     // PWord(Dest)^ := Ord(#0); //allways append the term
    end;
  end;
end;

function PRaw2PUnicode(Source: PAnsiChar; Dest: PWideChar; CP: Word; SourceBytes, DestWords: LengthInt): LengthInt;
var Buf: Pointer;
  sBuf: Array[0..dsMaxWStringSize] of WideChar; //avoid mem allocs -> stack
begin
  if DestWords <= SourceBytes then //no buffer overrun possible
    Result := PRaw2PUnicodeBuf(Source, Dest, SourceBytes, CP)
  else begin
    if SourceBytes <= dsMaxWStringSize
    then Buf := @sBuf[0]
    else Buf := AllocMem((SourceBytes+1) shl 1);
    Result := PRaw2PUnicodeBuf(Source, Buf, SourceBytes, CP);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf^, Dest^, (Min(Result, DestWords)+1) shl 1);
    if Buf <> @sBuf[0] then
      FreeMem(Buf, (SourceBytes+1) shl 1);
  end;
end;

function ZUnicodeToRaw(const US: UnicodeString; CP: Word): RawByteString;
{$IFDEF WITH_LCONVENCODING}
begin
  case CP of
    28591: //ISO_8859_1
      Result := UTF8ToISO_8859_1(UTF8Encode(US));
    28592:  //ISO_8859_2
      Result := UTF8ToISO_8859_2(UTF8Encode(US));
    1250: //WIN1250
      Result := UTF8ToCP1250(UTF8Encode(US));
    1251: //WIN1251
      Result := UTF8ToCP1251(UTF8Encode(US));
    1252: //WIN1252
      Result := UTF8ToCP1252(UTF8Encode(US));
    1253: //WIN1253
      Result := UTF8ToCP1253(UTF8Encode(US));
    1254: //WIN1254
      Result := UTF8ToCP1254(UTF8Encode(US));
    1255: //WIN1255
      Result := UTF8ToCP1255(UTF8Encode(US));
    1256: //WIN1256
      Result := UTF8ToCP1256(UTF8Encode(US));
    1257: //WIN1257
      Result := UTF8ToCP1257(UTF8Encode(US));
    1258: //WIN1258
      Result := UTF8ToCP1258(UTF8Encode(US));
    437: //CP437
      Result := UTF8ToCP437(UTF8Encode(US));
    850: //CP850
      Result := UTF8ToCP850(UTF8Encode(US));
    {$IFDEF LCONVENCODING_HAS_CP852_FUNCTIONS}
    852: //CP852
      Result := UTF8ToCP852(UTF8Encode(US));
    {$ENDIF}
    866: //CP866
      Result := UTF8ToCP866(UTF8Encode(US));
    874: //CP874
      Result := UTF8ToCP874(UTF8Encode(US));
    20866: //KOI8 (Russian)
      Result := UTF8ToKOI8(UTF8Encode(US));
    65001: //UTF8
      Result := UTF8Encode(US);
    else
      Result := RawByteString(US); //random success!
  end;
end;
{$ELSE}
begin
  {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS) or defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER)}
  if CP <> zCP_NONE
  then Result := PUnicodeToRaw(Pointer(US), Length(US), CP)
  else Result := RawByteString(US); //random success
  {$ELSE}
  if (CP = zCP_UTF8)
  then Result := UTF8Encode(US)
  else Result := RawByteString(US); //random success
  {$IFEND}
end;
{$ENDIF}

function PUnicodeToRaw(Source: PWideChar; SrcWords: LengthInt; CP: Word): RawByteString;
var
  ulen: Integer;
  Buf: Array[0..dsMaxRStringSize] of AnsiChar;
{$IF defined(FPC) and not defined(MSWINDOWS) and not defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER)}
  US: UnicodeString;
{$IFEND}
begin
  Result := EmptyRaw;
  if (SrcWords > 0) and (Source <> nil) then begin
    if (CP = zCP_NONE) or (CP = zCP_UTF16) then
      CP := ZOSCodePage; //random success
    ULen := Min(SrcWords shl 2, High(Integer)-1);
    if (CP = zCP_UTF8) then begin
      if Ulen <= dsMaxRStringSize then
        ZSetString(@Buf[0], PUnicodeToUtf8Buf(@Buf[0], ULen, Source, SrcWords, [ccfNoTrailingZero]), Result{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF})
      else begin
        ZSetString(nil, ULen, Result{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF}); //oversized
        SetLength(Result, PUnicodeToUtf8Buf(Pointer(Result), ULen, Source, SrcWords, [ccfNoTrailingZero]));
      end
    end else
    {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
    if Ulen <= dsMaxRStringSize then
      {$IFDEF WITH_UNICODEFROMLOCALECHARS}
      ZSetString(@Buf[0], LocaleCharsFromUnicode(CP, 0, Source, SrcWords, @Buf[0], ulen, NIL, NIL), Result{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF})
      {$ELSE}
      ZSetString(@Buf[0], WideCharToMultiByte(CP, 0, Source, SrcWords, @Buf[0], ulen, NIL, NIL), Result{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF})
      {$ENDIF}
    else begin
      ZSetString(nil, ULen, Result{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF}); //oversized
      {$IFDEF WITH_UNICODEFROMLOCALECHARS}
      SetLength(Result, LocaleCharsFromUnicode(CP, 0, Source, SrcWords, Pointer(Result), ulen, NIL, NIL)); // Convert Unicode down to Ansi
      {$ELSE}
      SetLength(Result, WideCharToMultiByte(CP,0, Source, SrcWords, Pointer(Result), ulen, nil, nil)); // Convert Wide down to Ansi
      {$ENDIF}
    end;
    {$ELSE}
      {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
      WidestringManager.Unicode2AnsiMoveProc(Source, Result, CP, SrcWords);
      {$ELSE}
      begin
        SetString(US, Source, SrcWords);
        {$IFDEF WITH_LCONVENCODING}
        Result := ZUnicodeToRaw(US, CP);
        {$ELSE}
        Result := RawByteString(Source); //random success
        {$ENDIF}
      end;
      {$ENDIF}
    {$IFEND}
  end;
end;

procedure PUnicodeToRaw(Source: PWideChar; SrcWords: LengthInt; CP: Word; var Result: RawByteString); overload;
var
  ulen: Integer;
  Buf: Array[0..dsMaxRStringSize] of AnsiChar;
{$IF defined(FPC) and not defined(MSWINDOWS) and not defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER)}
  US: UnicodeString;
{$IFEND}
begin
  if SrcWords = 0 then
    Result := EmptyRaw
  else begin
    if (CP = zCP_NONE) or (CP = zCP_UTF16) then
      CP := ZOSCodePage; //random success
    ULen := Min(SrcWords shl 2, High(Integer)-1);
    if (CP = zCP_UTF8) then begin
      if Ulen <= dsMaxRStringSize then
        ZSetString(@Buf[0], PUnicodeToUtf8Buf(@Buf[0], ULen, Source, SrcWords, [ccfNoTrailingZero]), Result{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF})
      else begin
        ZSetString(nil, ULen, Result{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF}); //oversized
        SetLength(Result, PUnicodeToUtf8Buf(Pointer(Result), ULen, Source, SrcWords, [ccfNoTrailingZero]));
      end
    end else
    {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
    if Ulen <= dsMaxRStringSize then
      {$IFDEF WITH_UNICODEFROMLOCALECHARS}
      ZSetString(@Buf[0], LocaleCharsFromUnicode(CP, 0, Source, SrcWords, @Buf[0], ulen, NIL, NIL), Result{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF})
      {$ELSE}
      ZSetString(@Buf[0], WideCharToMultiByte(CP, 0, Source, SrcWords, @Buf[0], ulen, NIL, NIL), Result{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF})
      {$ENDIF}
    else begin
      ZSetString(nil, ULen, Result{$IFDEF WITH_RAWBYTESTRING}, CP{$ENDIF}); //oversized
      {$IFDEF WITH_UNICODEFROMLOCALECHARS}
      SetLength(Result, LocaleCharsFromUnicode(CP, 0, Source, SrcWords, Pointer(Result), ulen, NIL, NIL)); // Convert Unicode down to Ansi
      {$ELSE}
      SetLength(Result, WideCharToMultiByte(CP,0, Source, SrcWords, Pointer(Result), ulen, nil, nil)); // Convert Wide down to Ansi
      {$ENDIF}
    end;
    {$ELSE}
      {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
      WidestringManager.Unicode2AnsiMoveProc(Source, Result, CP, SrcWords);
      {$ELSE}
      begin
        SetString(US, Source, SrcWords);
        {$IFDEF WITH_LCONVENCODING}
        Result := ZUnicodeToRaw(US, CP);
        {$ELSE}
        Result := RawByteString(Source); //random success
        {$ENDIF}
      end;
      {$ENDIF}
    {$IFEND}
  end;
end;

function PUnicode2PRawBuf(Source: PWideChar; Dest: PAnsiChar; SrcWords, MaxDestBytes: LengthInt; CP: Word): LengthInt;
{$IF not defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}
var
  {$IFNDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER}
  W: UnicodeString;
  {$ENDIF}
  s: RawByteString;
{$IFEND}
begin
  if CP = zCP_UTF8 then
    Result := PUnicodeToUtf8Buf(Dest, MaxDestBytes, Source, SrcWords, [ccfNoTrailingZero])
  else if (Dest = nil) or (SrcWords = 0) then
    Result := 0
  else begin
    if CP = zCP_NONE then
      CP := ZOSCodePage; //random success
    {$IF defined(MSWINDOWS) or defined(WITH_UNICODEFROMLOCALECHARS)}
      {$IFDEF MSWINDOWS}
      Result := WideCharToMultiByte(CP, 0, Source, SrcWords, Dest, MaxDestBytes, NIL, NIL);
      {$ELSE}
      Result := LocaleCharsFromUnicode(CP, 0, Source, SrcWords, Pointer(Dest), MaxDestBytes, NIL, NIL);
      {$ENDIF}
    {$ELSE} //FPC non Windows
      {if (CP = zCP_UTF8) then //FPC has a build in function here just for UTF16 to UTF8
        Result := UnicodeToUtf8(Dest, MaxDestBytes, Source, SrcWords)
      else }begin //no other build in function to encode into a buffer available yet ): i'm forced to localize the values
        {$IFDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //FPC2.7+
        S := '';
        WidestringManager.Unicode2AnsiMoveProc(Source, S, CP, SrcWords);
        {$ELSE}
          SetString(W, Source, SrcWords);
          {$IFDEF WITH_LCONVENCODING}
          S := ZUnicodeToRaw(W, CP);
          {$ELSE}
          S := RawByteString(W); //random success
          {$ENDIF}
        {$ENDIF}
        Result := Min(Length(S), MaxDestBytes);
        {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(S), Dest^, Result);
      end;
    {$IFEND}
  end;
end;

{$IFDEF WITH_LCONVENCODING}
function IsLConvEncodingCodePage(const CP: Word): Boolean;
var
  I: Integer;
begin
  for i := 0 to High(ZLConvCodepages) do
  begin
    Result := CP = ZLConvCodepages[i];
    if Result then Break;
  end;
end;
{$ENDIF}

{$IFDEF FPC}
  {$PUSH} {$WARN 5057 off : Local variable "lpcCPInfo" does not seem to be initialized}
{$ENDIF}
procedure SetZOSCodePage;
{$IFDEF MSWINDOWS}
var lpcCPInfo: _cpinfo;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  ZOSCodePage := GetACP;
  If GetCPInfo(ZOSCodePage, lpcCPInfo)
  then ZOSCodePageMaxCharSize := lpcCPInfo.MaxCharSize
  else ZOSCodePageMaxCharSize := 1;
  {$ELSE !MSWINDOWS}
    {$IFDEF FPC}
      {$ifdef UNIX}
  ZOSCodePage := GetSystemCodepage;
  if (ZOSCodePage = CP_NONE) then
    ZOSCodePage := CP_UTF8;
      {$ELSE UNIX}
  if Assigned (WideStringManager.GetStandardCodePageProc)
  then ZOSCodePage := WideStringManager.GetStandardCodePageProc(scpAnsi)
  else ZOSCodePage := zCP_UTF8;
      {$ENDIF UNIX}
    {$ELSE FPC}
  ZOSCodePage := zCP_UTF8;
    {$ENDIF FPC}
  if ZOSCodePage = zCP_UTF8
  then ZOSCodePageMaxCharSize := 4
  else ZOSCodePageMaxCharSize := 1
  {$ENDIF MSWINDOWS}
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function IsMBCSCodePage(CP: Word): Boolean;
begin
  Result := (CP >= zCP_csISO2022JP) or ((CP >=zCP_MSWIN921) and (CP <=zCP_Big5)) or (CP = ZCP_JOHAB) or (CP=zCP_EUC_JP)
end;

function ZDetectUTF8Encoding(Source: PAnsiChar; Len: NativeUInt): TEncodeType;
var
  c : Byte;
  EndPtr: PAnsichar;
begin
  Result := etUSASCII;
  if (Source = nil) or (Len = 0) then Exit;

  EndPtr := Source + Len -SizeOf(Cardinal);

  // skip leading US-ASCII part.
  while Source <= EndPtr do begin//Check next quad
    if PCardinal(Source)^ and $80808080<>0 then Break; //break on first non USASCII sequence
    inc(Source, SizeOf(Cardinal));
  end;
  Inc(EndPtr, SizeOf(Cardinal));

  while Source < EndPtr do begin//Check bytes
    if Byte(Source^) >= $80 then break; //break on first non USASCII sequence
    inc(Source);
  end;

  // If all character is US-ASCII, done.
  if Source = EndPtr then exit;

  while Source < EndPtr do begin
    c := Byte(Source^);
    case c of
      $00..$7F:  //Ascii7
        if (EndPtr - Source > SizeOf(PCardinal)) and (PCardinal(Source)^ and $80808080 = 0) //Check quad block ASCII again
        then inc(Source, SizeOf(PCardinal))
        else Inc(Source);
      $C2..$DF:  // non-overlong 2-byte
        if (Source+1 < EndPtr) and (Byte((Source+1)^) in [$80..$BF])
        then Inc(Source, 2)
        else break;
      $E0: // excluding overlongs
        if (Source+2 < EndPtr)
            and (Byte((Source+1)^) in [$A0..$BF])
            and (Byte((Source+2)^) in [$80..$BF])
        then Inc(Source, 3)
        else break;
      $E1..$EF: // straight 3-byte & excluding surrogates
        if (Source+2 < EndPtr)
            and (Byte((Source+1)^) in [$80..$BF])
            and (Byte((Source+2)^) in [$80..$BF])
        then Inc(Source, 3)
        else break;
      $F0: // planes 1-3
        if (Source+3 < EndPtr)
            and (Byte((Source+1)^) in [$90..$BF])
            and (Byte((Source+2)^) in [$80..$BF])
            and (Byte((Source+3)^) in [$80..$BF])
        then Inc(Source, 4)
        else break;
      $F1..$F3: if (Source+3 < EndPtr)
            and (Byte((Source+1)^) in [$80..$BF])
            and (Byte((Source+2)^) in [$80..$BF])
            and (Byte((Source+3)^) in [$80..$BF])
        then Inc(Source, 4)
        else break;
      $F4: if (Source+3 < EndPtr)
            and (Byte((Source+1)^) in [$80..$8F])
            and (Byte((Source+2)^) in [$80..$BF])
            and (Byte((Source+3)^) in [$80..$BF])
        then Inc(Source, 4)
        else break;
      else break;
    end;
  end;

  if Source = EndPtr then Result := etUTF8
  else Result := etANSI;
end;

function USASCII7ToUnicodeString(Source: PAnsiChar; Len: NativeUInt): UnicodeString; overload;
var C: Cardinal;
  Dest: PWideChar;
begin
  SetString(Result, nil, Len);
  Dest := Pointer(Result);
  {fast quad conversion from SHA}
  while Len >= 4 do begin
    C := PCardinal(Source)^;
    dec(Len,4);
    inc(Source,4);
    PCardinal(Dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
    c := c shr 16;
    PCardinal(Dest+2)^ := (c shl 8 or c) and $00ff00ff;
    inc(Dest,4);
  end;
  while Len > 0 do begin
    dec(Len);
    PWord(Dest)^ := Byte(Source^); //Shift Byte to Word
    inc(Source);
    inc(Dest);
  end;
end;

function USASCII7ToUnicodeString(const Source: RawByteString): UnicodeString; overload;
begin
  Result := USASCII7ToUnicodeString(Pointer(Source), Length(Source){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
end;

initialization
  SetZOSCodePage;
end.
