{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Variables classes and interfaces            }
{                                                         }
{           Originally written by Sergey Seroukhov        }
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

unit ZFunctionsDateTime;

interface

{$I ZCore.inc}

uses
  SysUtils, ZFunctions, ZExpression, ZVariant;

{** Date & time functions}

type
  /// <summary>Implements a DATE function.</summary>
  TZDateFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a TIME function.</summary>
  TZTimeFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a NOW function.</summary>
  TZNowFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a ENCODEDATE function.</summary>
  TZEncodeDateFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a ENCODETIME function.</summary>
  TZEncodeTimeFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a COMPOSEDATETIME function.</summary>
  TZComposeDateTimeFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a INCDATE function.</summary>
  TZIncDateFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a INCTIME function.</summary>
  TZIncTimeFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a ISLEAPYEAR function.</summary>
  TZIsLeapYearFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

{-------------------- Extracting functions ----------------------------}

  /// <summary>Implements a DATEOF function.</summary>
  TZDateOfFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a TIMEOF function.</summary>
  TZTimeOfFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a YEAROF function.</summary>
  TZYearOfFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MONTHOF function.</summary>
  TZMonthOfFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a DAYOF function.</summary>
  TZDayOfFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a HOUROF function.</summary>
  TZHourOfFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MINUTEOF function.</summary>
  TZMinuteOfFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a SECONDOF function.</summary>
  TZSecondOfFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MILLISECONDOF function.</summary>
  TZMilliSecondOfFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

{-------------------- *OFTHEYEAR Extracting functions ----------------------------}

  /// <summary>Implements a WEEKOFTHEYEAR function.</summary>
  TZWeekOfTheYearFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a DAYOFTHEYEAR function.</summary>
  TZDayOfTheYearFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a HOUROFTHEYEAR function.</summary>
  TZHourOfTheYearFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MINUTEOFTHEYEAR function.</summary>
  TZMinuteOfTheYearFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a SECONDOFTHEYEAR function.</summary>
  TZSecondOfTheYearFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MILLISECONDOFTHEYEAR function.</summary>
  TZMilliSecondOfTheYearFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

{-------------------- *OFTHEMONTH Extracting functions ----------------------------}

  /// <summary>Implements a WEEKOFTHEMONTH function.</summary>
  TZWeekOfTheMonthFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a HOUROFTHEMONTH function.</summary>
  TZHourOfTheMonthFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MINUTEOFTHEMONTH function.</summary>
  TZMinuteOfTheMonthFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a SECONDOFTHEMONTH function.</summary>
  TZSecondOfTheMonthFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MILLISECONDOFTHEMONTH function.</summary>
  TZMilliSecondOfTheMonthFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

{-------------------- *OFTHEWEEK Extracting functions ----------------------------}

  /// <summary>Implements a DAYOfTheWeek function.</summary>
  TZDayOfTheWeekFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a HOUROfTheWeek function.</summary>
  TZHourOfTheWeekFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MINUTEOfTheWeek function.</summary>
  TZMinuteOfTheWeekFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a SECONDOfTheWeek function.</summary>
  TZSecondOfTheWeekFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MILLISECONDOfTheWeek function.</summary>
  TZMilliSecondOfTheWeekFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

{---------------- *OFTHEDAY Extracting functions --------------------}

  /// <summary>Implements a MINUTEOFTHEDAY function.</summary>
  TZMinuteOfTheDayFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a SECONDOFTHEDAY function.</summary>
  TZSecondOfTheDayFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MILLISECONDOFTHEDAY function.</summary>
  TZMilliSecondOfTheDayFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

{---------------- *OfTheHour Extracting functions --------------------}

  /// <summary>Implements a SECONDOFTHEHOUR function.</summary>
  TZSecondOfTheHourFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MILLISECONDOFTHEHOUR function.</summary>
  TZMilliSecondOfTheHourFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

{---------------- *OFTHEMINUTE Extracting functions --------------------}

  /// <summary>Implements a MILLISECONDOfTheHour function.</summary>
  TZMilliSecondOfTheMinuteFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

{---------------- *BETWEEN functions --------------------}

  /// <summary>Implements a YEARSBETWEEN function.</summary>
  TZYearsBetweenFunction = class (TZAbstractFunction, IZFunction)
  public
     /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
   function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MONTHSBETWEEN function.</summary>
  TZMonthsBetweenFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a WEEKSBETWEEN function.</summary>
  TZWeeksBetweenFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a DAYSBETWEEN function.</summary>
  TZDaysBetweenFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a HOURSBETWEEN function.</summary>
  TZHoursBetweenFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MINUTESBETWEEN function.</summary>
  TZMinutesBetweenFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a SECONDSBETWEEN function.</summary>
  TZSecondsBetweenFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

  /// <summary>Implements a MILLISECONDSBETWEEN function.</summary>
  TZMillisecondsBetweenFunction = class (TZAbstractFunction, IZFunction)
  public
    /// <summary>Executes this function.</summary>
    /// <param>"Stack" the TZExecutionStack object.</param>
    /// <param>"VariantManager" an interface of a variant processor object.</param>
    /// <returns>a function result variable.</returns>
    function Execute(Stack: TZExecutionStack;
      const VariantManager: IZVariantManager): TZVariant;
  end;

procedure AddDateTimeFunctions(Functions : TZFunctionsList);

implementation

uses
  ZMessages, DateUtils;

Function IncDate(const aDate : TDateTime; const aYear, aMonth, aWeek, aDay : LongInt) : TDateTime;
begin
  Result := aDate;
  if aYear  <> 0 then Result := IncYear(Result, aYear);
  if aMonth <> 0 then Result := IncMonth(Result, aMonth);
  if aWeek  <> 0 then Result := IncWeek(Result, aWeek);
  if aDay   <> 0 then Result := IncDay(Result, aDay);
end;

Function IncTime(const aDate : TDateTime; const aHour, aMinute, aSecond, aMillisec : LongInt) : TDateTime;
begin
  Result := IncHour(IncMinute(IncSecond(IncMillisecond(aDate, aMilliSec),aSecond),aMinute),aHour);
end;

{ TZDateFunction }

function TZDateFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsDateTime(Result, Date);
end;

{ TZTimeFunction }

function TZTimeFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsDateTime(Result, Time);
end;

{ TZNowFunction }

function TZNowFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsDateTime(Result, Now);
end;

{ TZEncodeDateFunction }

function TZEncodeDateFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
  Year , Month, Day : LongInt;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));

  Year  := 0;
  Month := 1;
  Day   := 1;

  if ParamsCount > 0 then
    Year := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount));
  if ParamsCount > 1 then
    Month := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-1));
  if ParamsCount > 2 then
    Day := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-2));

  VariantManager.SetAsDateTime(Result, EncodeDate(Year,Month,Day));
end;

{ TZEncodeDateFunction }

function TZEncodeTimeFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
  Hour , Minute, Second, MilliSecond : LongInt;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));

  Hour        := 0;
  Minute      := 0;
  Second      := 0;
  MilliSecond := 0;

  if ParamsCount > 0 then
    Hour := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount));
  if ParamsCount > 1 then
    Minute := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-1));
  if ParamsCount > 2 then
    Second := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-2));
  if ParamsCount > 3 then
    MilliSecond := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-3));

  VariantManager.SetAsDateTime(Result, EncodeTime(Hour,Minute,Second,MilliSecond));
end;

{ TZComposeDateTimeFunction }

function TZComposeDateTimeFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsDateTime(Result, VariantManager.GetAsDateTime(Stack.GetParameter(2))+
    VariantManager.GetAsDateTime(Stack.GetParameter(1)));
end;

{ TZIncDateFunction }

function TZIncDateFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
  Date : TDateTime;
  Year , Month, Week, Day : LongInt;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));

  if (ParamsCount <= 2) then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Date  := VariantManager.GetAsDateTime(Stack.GetParameter(ParamsCount));
  Year  := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-1));
  Month := 0;
  Week  := 0;
  Day   := 0;
  if ParamsCount > 2 then
     Month := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-2));
  if ParamsCount > 3 then
     Week := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-3));
  if ParamsCount > 4 then
     Day := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-4));

  VariantManager.SetAsDateTime(Result, IncDate(Date,Year,Month,Week,Day));
end;

{ TZIncTimeFunction }

function TZIncTimeFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
  Date : TDateTime;
  Hour , Minute, Second, MilliSecond : LongInt;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));

  if (ParamsCount <= 2) then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Date := VariantManager.GetAsDateTime(Stack.GetParameter(ParamsCount));
  Hour := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-1));

  Minute      := 0;
  Second      := 0;
  MilliSecond := 0;

  if ParamsCount > 2 then
    Minute := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-2));
  if ParamsCount > 3 then
    Second := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-3));
  if ParamsCount > 4 then
    MilliSecond := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-4));

  VariantManager.SetAsDateTime(Result, IncTime(Date, Hour,Minute,Second,MilliSecond));
end;

{ TZIsLeapYearFunction }

function TZIsLeapYearFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsBoolean(Result, IsLeapYear(
    VariantManager.GetAsInteger(Stack.GetParameter(1))));
end;

{ TZDateOfFunction }

function TZDateOfFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsDateTime(Result, DateOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZTimeOfFunction }

function TZTimeOfFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsDateTime(Result, TimeOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZYearOfFunction }

function TZYearOfFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, YearOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMonthOfFunction }

function TZMonthOfFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MonthOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZDayOfFunction }

function TZDayOfFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, DayOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHourOfFunction }

function TZHourOfFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, HourOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinuteOfFunction }

function TZMinuteOfFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MinuteOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfFunction }

function TZSecondOfFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfFunction }

function TZMilliSecondOfFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZWeekOfTheYearFunction }

function TZWeekOfTheYearFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, WeekOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZDayOfTheYearFunction }

function TZDayOfTheYearFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, DayOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHourOfTheYearFunction }

function TZHourOfTheYearFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, HourOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinuteOfTheYearFunction }

function TZMinuteOfTheYearFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MinuteOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfTheYearFunction }

function TZSecondOfTheYearFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheYearFunction }

function TZMilliSecondOfTheYearFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
  {$IFDEF WITH_MILLISECONDOFTHEYEAR_BUG}
  function MilliSecondOfTheYear(const AValue: TDateTime): Int64;
  begin
    Result := MilliSecondOf(AValue) + Int64(SecondOfTheYear(AValue)) * MSecsPerSec;
  end;
  {$ENDIF}
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZWeekOfTheMonthFunction }

function TZWeekOfTheMonthFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, WeekOfTheMonth(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHourOfTheMonthFunction }

function TZHourOfTheMonthFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, HourOfTheMonth(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinuteOfTheMonthFunction }

function TZMinuteOfTheMonthFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MinuteOfTheMonth(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfTheMonthFunction }

function TZSecondOfTheMonthFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOfTheMonth(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheMonthFunction }

function TZMilliSecondOfTheMonthFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheMonth(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZDayOfTheWeekFunction }

function TZDayOfTheWeekFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, DayOfTheWeek(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHourOfTheWeekFunction }

function TZHourOfTheWeekFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, HourOfTheWeek(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinuteOfTheWeekFunction }

function TZMinuteOfTheWeekFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MinuteOfTheWeek(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfTheWeekFunction }

function TZSecondOfTheWeekFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOfTheWeek(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheWeekFunction }

function TZMilliSecondOfTheWeekFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheWeek(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinuteOfTheDayFunction }

function TZMinuteOfTheDayFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MinuteOfTheDay(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfTheDayFunction }

function TZSecondOfTheDayFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOfTheDay(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheDayFunction }

function TZMilliSecondOfTheDayFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheDay(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfTheHourFunction }

function TZSecondOfTheHourFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOfTheHour(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheHourFunction }

function TZMilliSecondOfTheHourFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheHour(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheMinuteFunction }

function TZMilliSecondOfTheMinuteFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheMinute(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZYearsBetweenFunction }

function TZYearsBetweenFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, YearsBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMonthsBetweenFunction }

function TZMonthsBetweenFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, MonthsBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZWeeksBetweenFunction }

function TZWeeksBetweenFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, WeeksBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZDaysBetweenFunction }

function TZDaysBetweenFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, DaysBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHoursBetweenFunction }

function TZHoursBetweenFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, HoursBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinutesBetweenFunction }

function TZMinutesBetweenFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, MinutesBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondsBetweenFunction }

function TZSecondsBetweenFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, SecondsBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHoursBetweenFunction }

function TZMillisecondsBetweenFunction.Execute(Stack: TZExecutionStack;
  const VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, MillisecondsBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

procedure AddDateTimeFunctions(Functions : TZFunctionsList);
begin
  Functions.Add(TZDateFunction.Create('DATE'));
  Functions.Add(TZTimeFunction.Create('TIME'));
  Functions.Add(TZNowFunction.Create('NOW'));

// First the Aliases

  Functions.Add(TZEncodeDateFunction.Create('ENCD'));
  Functions.Add(TZEncodeTimeFunction.Create('ENCT'));
//  Functions.Add(TZComposeDateTimeFunction.Create('COMPDT'));

  Functions.Add(TZIncDateFunction.Create('INCD'));
  Functions.Add(TZIncTimeFunction.Create('INCT'));

  Functions.Add(TZIsLeapYearFunction.Create('LEAPY'));

//  Functions.Add(TZDateOfFunction.Create('DATEOF'));
//  Functions.Add(TZTimeOfFunction.Create('TIMEOF'));

//  Functions.Add(TZYearOfFunction.Create('YEAROF'));
//  Functions.Add(TZMonthOfFunction.Create('MONTHOF'));
//  Functions.Add(TZDayOfFunction.Create('DAYOF'));
//  Functions.Add(TZHourOfFunction.Create('HOUROF'));

  Functions.Add(TZMinuteOfFunction.Create('MINOF'));
  Functions.Add(TZSecondOfFunction.Create('SECOF'));
  Functions.Add(TZMilliSecondOfFunction.Create('MSECOF'));

  Functions.Add(TZWeekOfTheYearFunction.Create('WofY'));
  Functions.Add(TZDayOfTheYearFunction.Create('DofY'));
  Functions.Add(TZHourOfTheYearFunction.Create('HofY'));
  Functions.Add(TZMinuteOfTheYearFunction.Create('MINofY'));
  Functions.Add(TZSecondOfTheYearFunction.Create('SECofY'));
  Functions.Add(TZMilliSecondOfTheYearFunction.Create('MSECofY'));

  Functions.Add(TZWeekOfTheMonthFunction.Create('WofM'));
  Functions.Add(TZHourOfTheMonthFunction.Create('HofM'));
  Functions.Add(TZMinuteOfTheMonthFunction.Create('MINofM'));
  Functions.Add(TZSecondOfTheMonthFunction.Create('SECofM'));
  Functions.Add(TZMilliSecondOfTheMonthFunction.Create('MSECofM'));

  Functions.Add(TZDayOfTheWeekFunction.Create('DofW'));
  Functions.Add(TZHourOfTheWeekFunction.Create('HofW'));
  Functions.Add(TZMinuteOfTheWeekFunction.Create('MINofW'));
  Functions.Add(TZSecondOfTheWeekFunction.Create('SECofW'));
  Functions.Add(TZMilliSecondOfTheWeekFunction.Create('MSECofW'));

  Functions.Add(TZMinuteOfTheDayFunction.Create('MINofD'));
  Functions.Add(TZSecondOfTheDayFunction.Create('SECofD'));
  Functions.Add(TZMilliSecondOfTheDayFunction.Create('MSECofD'));

  Functions.Add(TZSecondOfTheHourFunction.Create('SECofH'));
  Functions.Add(TZMilliSecondOfTheHourFunction.Create('MSECofH'));

  Functions.Add(TZMilliSecondOfTheMinuteFunction.Create('MSECofMIN'));

  Functions.Add(TZYearsBetweenFunction.Create('YBTW'));
  Functions.Add(TZMonthsBetweenFunction.Create('MBTW'));
  Functions.Add(TZWeeksBetweenFunction.Create('WBTW'));
  Functions.Add(TZDaysBetweenFunction.Create('DBTW'));
  Functions.Add(TZHoursBetweenFunction.Create('HBTW'));
  Functions.Add(TZMinutesBetweenFunction.Create('MINBTW'));
  Functions.Add(TZSecondsBetweenFunction.Create('SECBTW'));
  Functions.Add(TZMilliSecondsBetweenFunction.Create('MSECBTW'));

// End of Aliases

  Functions.Add(TZEncodeDateFunction.Create('ENCODEDATE'));
  Functions.Add(TZEncodeTimeFunction.Create('ENCODETIME'));
  Functions.Add(TZComposeDateTimeFunction.Create('COMPOSEDATETIME'));

  Functions.Add(TZIncDateFunction.Create('INCDATE'));
  Functions.Add(TZIncTimeFunction.Create('INCTIME'));

  Functions.Add(TZIsLeapYearFunction.Create('ISLEAPYEAR'));

  Functions.Add(TZDateOfFunction.Create('DATEOF'));
  Functions.Add(TZTimeOfFunction.Create('TIMEOF'));

  Functions.Add(TZYearOfFunction.Create('YEAROF'));
  Functions.Add(TZMonthOfFunction.Create('MONTHOF'));
  Functions.Add(TZDayOfFunction.Create('DAYOF'));
  Functions.Add(TZHourOfFunction.Create('HOUROF'));

  Functions.Add(TZMinuteOfFunction.Create('MINUTEOF'));
  Functions.Add(TZSecondOfFunction.Create('SECONDOF'));
  Functions.Add(TZMilliSecondOfFunction.Create('MILLISECONDOF'));

  Functions.Add(TZWeekOfTheYearFunction.Create('WEEKOFTHEYEAR'));
  Functions.Add(TZDayOfTheYearFunction.Create('DAYOFTHEYEAR'));
  Functions.Add(TZHourOfTheYearFunction.Create('HOUROFTHEYEAR'));
  Functions.Add(TZMinuteOfTheYearFunction.Create('MINUTEOFTHEYEAR'));
  Functions.Add(TZSecondOfTheYearFunction.Create('SECONDOFTHEYEAR'));
  Functions.Add(TZMilliSecondOfTheYearFunction.Create('MILLISECONDOFTHEYEAR'));

  Functions.Add(TZWeekOfTheMonthFunction.Create('WEEKOFTHEMONTH'));
  Functions.Add(TZHourOfTheMonthFunction.Create('HOUROFTHEMONTH'));
  Functions.Add(TZMinuteOfTheMonthFunction.Create('MINUTEOFTHEMONTH'));
  Functions.Add(TZSecondOfTheMonthFunction.Create('SECONDOFTHEMONTH'));
  Functions.Add(TZMilliSecondOfTheMonthFunction.Create('MILLISECONDOFTHEMONTH'));

  Functions.Add(TZDayOfTheWeekFunction.Create('DAYOFTHEWEEK'));
  Functions.Add(TZHourOfTheWeekFunction.Create('HOUROFTHEWEEK'));
  Functions.Add(TZMinuteOfTheWeekFunction.Create('MINUTEOFTHEWEEK'));
  Functions.Add(TZSecondOfTheWeekFunction.Create('SECONDOFTHEWEEK'));
  Functions.Add(TZMilliSecondOfTheWeekFunction.Create('MILLISECONDOFTHEWEEK'));

  Functions.Add(TZMinuteOfTheDayFunction.Create('MINUTEOFTHEDAY'));
  Functions.Add(TZSecondOfTheDayFunction.Create('SECONDOFTHEDAY'));
  Functions.Add(TZMilliSecondOfTheDayFunction.Create('MILLISECONDOFTHEDAY'));

  Functions.Add(TZSecondOfTheHourFunction.Create('SECONDOFTHEHOUR'));
  Functions.Add(TZMilliSecondOfTheHourFunction.Create('MILLISECONDOFTHEHOUR'));

  Functions.Add(TZMilliSecondOfTheMinuteFunction.Create('MILLISECONDOFTHEMINUTE'));

  Functions.Add(TZYearsBetweenFunction.Create('YEARSBETWEEN'));
  Functions.Add(TZMonthsBetweenFunction.Create('MONTHSBETWEEN'));
  Functions.Add(TZWeeksBetweenFunction.Create('WEEKSBETWEEN'));
  Functions.Add(TZDaysBetweenFunction.Create('DAYSBETWEEN'));
  Functions.Add(TZHoursBetweenFunction.Create('HOURSBETWEEN'));
  Functions.Add(TZMinutesBetweenFunction.Create('MINUTESBETWEEN'));
  Functions.Add(TZSecondsBetweenFunction.Create('SECONDSBETWEEN'));
  Functions.Add(TZMilliSecondsBetweenFunction.Create('MILLISECONDSBETWEEN'));

end;

end.

