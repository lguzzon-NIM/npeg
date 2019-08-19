
import strutils
import tables

# Some constants with "sane" values - these will have to be made configurable one day

const
  npegPattMaxLen* {.intdefine.} = 4096
  npegInlineMaxLen* {.intdefine.} = 50
  npegRetStackSize* {.intdefine.} = 1024
  npegBackStackSize* {.intdefine.} = 1024
  npegDebug* = defined(npegDebug)
  npegTrace* = defined(npegTrace)
  npegExpand* = defined(npegExpand)

type

  NPegException* = object of Exception
    matchLen*: int
    matchMax*: int
  
  CapFrameType* = enum cftOpen, cftClose
  
  CapKind* = enum
    ckStr,          # Plain string capture
    ckJString,      # JSON string capture
    ckJBool,        # JSON Bool capture
    ckJInt,         # JSON Int capture
    ckJFloat,       # JSON Float capture
    ckJArray,       # JSON Array
    ckJObject,      # JSON Object
    ckJFieldFixed,  # JSON Object field with fixed tag
    ckJFieldDynamic,# JSON Object field with dynamic tag
    ckAction,       # Action capture, executes Nim code at match time
    ckRef           # Reference
    ckAST,          # Abstract syntax tree capture
    ckClose,        # Closes capture

  CapFrame* = tuple
    cft: CapFrameType
    si: int
    ck: CapKind
    name: string

  Ref* = object
    key*: string
    val*: string

  Subject* = openArray[char]

  Opcode* = enum
    opStr,          # Matching: Literal string or character
    opIStr,         # Matching: Literal string or character, case insensitive
    opSet,          # Matching: Character set and/or range
    opAny,          # Matching: Any character
    opNop,          # Matching: Always matches, consumes nothing
    opSpan          # Matching: Match a sequence of 0 or more character sets
    opChoice,       # Flow control: stores current position
    opCommit,       # Flow control: commit previous choice
    opPartCommit,   # Flow control: optimized commit/choice pair
    opCall,         # Flow control: call another rule
    opJump,         # Flow control: jump to target
    opReturn,       # Flow control: return from earlier call
    opFail,         # Fail: unwind stack until last frame
    opCapOpen,      # Capture open
    opCapClose,     # Capture close
    opBackref       # Back reference
    opErr,          # Error handler

  CharSet* = set[char]

  Inst* = object
    case op*: Opcode
      of opChoice, opCommit, opPartCommit:
        offset*: int
      of opStr, opIStr:
        str*: string
      of opCall, opJump:
        callLabel*: string
        callOffset*: int
      of opSet, opSpan:
        cs*: CharSet
      of opCapOpen, opCapClose:
        capKind*: CapKind
        capAction*: NimNode
        capName*: string
        capId*: BiggestInt
      of opErr:
        msg*: string
      of opFail, opReturn, opAny, opNop:
        discard
      of opBackref:
        refName*: string
    when npegTrace:
      name*: string
      pegRepr*: string

  Patt* = seq[Inst]

  Grammar* = TableRef[string, Patt]


#
# Misc helper functions
#

proc subStrCmp*(s: Subject, slen: int, si: int, s2: string): bool =
  if si > slen - s2.len:
    return false
  for i in 0..<s2.len:
    if s[si+i] != s2[i]:
      return false
  return true


proc subIStrCmp*(s: Subject, slen: int, si: int, s2: string): bool =
  if si > slen - s2.len:
    return false
  for i in 0..<s2.len:
    if s[si+i].toLowerAscii != s2[i].toLowerAscii:
      return false
  return true


proc slice*(s: Subject, iFrom, iTo: int): string =
  let len = iTo - iFrom
  result.setLen(len)
  when false:
    copyMem(result[0].addr, s[iFrom].unsafeAddr, len)
  else:
    for i in 0..<len:
      result[i] = s[i+iFrom]


type

  TwoWayTable*[X,Y] = ref object
    x2y: Table[X, Y]
    y2x: Table[Y, X]

  Symtab* = TwoWayTable[string, int]

proc newTwoWayTable*[X,Y](): owned TwoWayTable[X,Y] =
  new result
  result.x2y = initTable[X, Y]()
  result.y2x = initTable[Y, X]()

proc add*[X,Y](s: owned TwoWayTable[X,Y], x: X, y: Y) =
  s.x2y[x] = y
  s.y2x[y] = x

proc contains*[X,Y](s: owned TwoWayTable[X,Y], y: Y): bool =
  return y in s.y2x

proc contains*[X,Y](s: owned TwoWayTable[X,Y], x: X): bool =
  return x in s.x2y

proc get*[X,Y](s: owned TwoWayTable[X,Y], y: Y): X =
  return s.y2x[y]

proc get*[X,Y](s: owned TwoWayTable[X,Y], x: X): Y =
  return s.x2y[x]

