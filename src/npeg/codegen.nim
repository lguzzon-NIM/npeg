
import macros
import strutils
import tables
import npeg/[common,patt,stack,capture]

type

  RetFrame = int

  BackFrame = tuple
    ip: int
    si: int
    rp: int
    cp: int

  MatchResult* = object
    ok*: bool
    matchLen*: int
    matchMax*: int
    cs*: Captures


  Parser*[T] = object
    fn*: proc(s: Subject, userdata: var T): MatchResult
    charSets: Table[CharSet, int]


# This macro translates `$1`.. into `capture[0]`.. for use in code block captures

proc mkDollarCaptures(n: NimNode): NimNode =
  if n.kind == nnkPrefix and
       n[0].kind == nnkIdent and n[0].eqIdent("$") and
       n[1].kind == nnkIntLit:
    let i = int(n[1].intVal-1)
    result = quote do:
      capture[`i`].s
  elif n.kind == nnkNilLit:
    result = quote do:
      discard
  else:
    result = copyNimNode(n)
    for nc in n:
      result.add mkDollarCaptures(nc)


# Template for generating the parsing match proc.
#
# Note: Dummy 'ip' and 'capture' nodes are passed into this template to prevent these
# names from getting mangled so that the code in the `peg` macro can access it.
# I'd love to hear if there are better solutions for this.

template skel(T: untyped, cases: untyped, ip: NimNode, userdata: NimNode, capture: NimNode) =

  {.push hint[XDeclaredButNotUsed]: off.}

  let match = proc(s: Subject, userdata: var T): MatchResult =

    # The parser state

    var
      ip: int
      si: int
      simax: int
      refs = initTable[string, string]()
      retStack = initStack[RetFrame]("return", 8, npegRetStackSize)
      capStack = initStack[CapFrame]("capture", 8)
      backStack = initStack[BackFrame]("backtrace", 8, npegBackStackSize)

    # Debug trace. Slow and expensive

    proc doTrace(iname: string, s: Subject, msg: string) =
      var l: string
      l.add if ip >= 0: align($ip, 3) else: "   "
      l.add "|" & align($si, 3)
      l.add "|" & alignLeft(dumpString(s, si, 24), 24)
      l.add "|" & alignLeft(iname, 15)
      l.add "|" & alignLeft(msg, 30)
      l.add "|" & alignLeft(repeat("*", backStack.top), 20)
      echo l

    template trace(iname: string, s: Subject, msg: string) =
      when npegTrace:
        doTrace(iname, s, msg)

    # State machine instruction handlers

    template opStrFn(s2: string, iname="") =
      trace iname, s, "str \"" & dumpString(s2) & "\""
      if subStrCmp(s, s.len, si, s2):
        inc ip
        inc si, s2.len
      else:
        ip = -1

    template opIStrFn(s2: string, iname="") =
      trace iname, s, "istr \"" & dumpString(s2) & "\""
      if subIStrCmp(s, s.len, si, s2):
        inc ip
        inc si, s2.len
      else:
        ip = -1

    template opSetFn(cs: CharSet, iname="") =
      trace iname, s, "set " & dumpSet(cs)
      if si < s.len and s[si] in cs:
        inc ip
        inc si
      else:
        ip = -1

    template opSpanFn(cs: CharSet, iname="") =
      trace iname, s, "span " & dumpSet(cs)
      while si < s.len and s[si] in cs:
        inc si
      inc ip

    template opNopFn(iname="") =
      trace iname, s, "nop"
      inc ip

    template opAnyFn(iname="") =
      trace iname, s, "any"
      if si < s.len:
        inc ip
        inc si
      else:
        ip = -1

    template opChoiceFn(n: int, iname="") =
      trace iname, s, "choice -> " & $n
      push(backstack, (ip:n, si:si, rp:retStack.top, cp:capStack.top))
      inc ip

    template opCommitFn(n: int, iname="") =
      trace iname, s, "commit -> " & $n
      discard pop(backStack)
      ip = n

    template opPartCommitFn(n: int, iname="") =
      trace iname, s, "pcommit -> " & $n
      update(backStack, si, si)
      update(backStack, cp, capStack.top)
      ip = n

    template opCallFn(label: string, offset: int, iname="") =
      trace iname, s, "call -> " & label & ":" & $(ip+offset)
      push(retStack, ip+1)
      ip += offset

    template opJumpFn(label: string, offset: int, iname="") =
      trace iname, s, "jump -> " & label & ":" & $(ip+offset)
      ip += offset

    template opCapOpenFn(n: int, capname: string, iname="") =
      let ck = CapKind(n)
      trace iname, s, "capopen " & $ck & " -> " & $si
      push(capStack, (cft: cftOpen, si: si, ck: ck, name: capname))
      inc ip

    template opCapCloseFn(n: int, actionCode: untyped, iname="") {.dirty.} =
      # This template is dirty to avoid name mangling
      let ck = CapKind(n)
      trace iname, s, "capclose " & $ck & " -> " & $si
      push(capStack, (cft: cftClose, si: si, ck: ck, name: ""))
      if ck == ckAction:
        proc fn(capture: Captures, userdata: var T): bool =
          result = true
          actionCode
        let capture = collectCaptures(fixCaptures(s, capStack, FixOpen))
        let ok = fn(capture, userdata)
        if ok:
          inc ip
        else:
          ip = -1
      elif ck == ckRef:
        let cs = collectCapturesRef(fixCaptures(s, capStack, FixOpen))
        refs[cs.key] = cs.val
        inc ip
      else:
        inc ip

    template opBackrefFn(refName: string, iname="") =
      # This is a proc because we do not want to export 'contains'
      if refName in refs:
        let s2 = refs[refName]
        trace iname, s, "backref " & refName & ":\"" & s2 & "\""
        if subStrCmp(s, s.len, si, s2):
          inc ip
          inc si, s2.len
        else:
          ip = -1
      else:
        raise newException(NPegException, "Unknown back reference '" & refName & "'")

    template opReturnFn(iname="") =
      trace iname, s, "return"
      if retStack.top == 0:
        trace iname, s, "done"
        result.ok = true
        break
      ip = pop(retStack)

    template opFailFn(iname="") =
      trace iname, s, "fail"
      if backStack.top == 0:
        trace iname, s, "error"
        break
      (ip, si, retStack.top, capStack.top) = pop(backStack)

    template opErrFn(msg: string, iname="") =
      trace iname, s, "err " & msg
      var e = newException(NPegException, "Parsing error at #" & $si & ": expected \"" & msg & "\"")
      e.matchLen = si
      e.matchMax = simax
      raise e

    while true:

      # These cases will be filled in by genCode() which uses this template
      # as the match lambda boilerplate:

      cases

      # Keep track of the highest string index we ever reached, this is a good
      # indication of the location of errors when parsing fails

      if si > simax:
        simax = si

    # When the parsing machine is done, close the capture stack and collect all
    # the captures in the match result

    result.matchLen = si
    result.matchMax = simax
    if result.ok and capStack.top > 0:
      result.cs = fixCaptures(s, capStack, FixAll)

  {.pop.}

  Parser[T](fn: match)


# Convert the list of parser instructions into a Nim finite state machine

proc genCode*(patt: Patt, T: NimNode): NimNode =

  let ipNode = ident("ip")
  var cases = nnkCaseStmt.newTree(ipNode)

  for n, i in patt.pairs:

    let call = nnkCall.newTree(ident($i.op & "Fn"))

    case i.op:
      of opStr, opIStr:
        call.add newLit(i.str)

      of opSet, opSpan:
        let setNode = nnkCurly.newTree()
        for c in i.cs: setNode.add newLit(c)
        call.add setNode

      of opChoice, opCommit, opPartCommit:
        call.add newLit(n + i.offset)

      of opCall, opJump:
        call.add newLit(i.callLabel)
        call.add newLit(i.callOffset)

      of opCapOpen:
        call.add newLit(i.capKind.int)
        call.add newLit(i.capName)

      of opCapClose:
        call.add newLit(i.capKind.int)
        call.add mkDollarCaptures(i.capAction)

      of opBackref:
        call.add newLit(i.refName)

      of opErr:
        call.add newLit(i.msg)

      of opReturn, opAny, opNop, opFail:
        discard

    when npegTrace:
      call.add newLit(i.name)

    cases.add nnkOfBranch.newTree(newLit(n), call)

  cases.add nnkElse.newTree(parseStmt("opFailFn()"))
  result = getAst skel(T, cases, ipNode, ident "userdata", ident "capture")

  when npegExpand:
    echo result.repr


