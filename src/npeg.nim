
#
# Copyright (c) 2019 Ico Doornekamp
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#
# This parser implementation is based on the following papers:
#
# - A Text Pattern-Matching Tool based on Parsing Expression Grammars
#   (Roberto Ierusalimschy)
#
# - An efficient parsing machine for PEGs
#   (Jos Craaijo)
#


import tables
import macros
import strutils
import npeg/[common,codegen,capture,parsepatt,grammar,dot,lib]

export NPegException, Parser, MatchResult, contains

# Create a parser for a PEG grammar

macro peg*(name: string, n: untyped): untyped =
  var dot = newDot(name.strVal)
  var grammar = parseGrammar(n, dot)
  let code = grammar.link(name.strVal(), dot).genCode(bindSym"bool")
  dot.dump()
  code

macro peg*(T: typedesc, name: string, n: untyped): untyped =
  var dot = newDot(name.strVal)
  var grammar = parseGrammar(n, dot)
  let code = grammar.link(name.strVal(), dot).genCode(T)
  dot.dump()
  code


# Create a parser for a single PEG pattern

macro patt*(n: untyped): untyped =
  var grammar = newGrammar()
  var patt = parsePatt("anonymous", n, grammar)
  grammar.add("anonymous", patt)
  grammar.link("anonymous").genCode(bindsym"bool")


# Define a grammar for storage in the global library.

macro grammar*(libNameNode: string, n: untyped) =
  let grammar = parseGrammar(n)
  let libName = libNameNode.strval
  libStore(libName, grammar)


# Match a subject string

proc match*[T](p: Parser, s: Subject, userdata: var T): MatchResult =
  p.fn(s, userdata)

proc match*(p: Parser, s: Subject): MatchResult =
  var userdata: bool # dummy if user does not provide a type
  p.match(s, userdata)


# Match a file

when defined(windows) or defined(posix) and not defined(nimV2):
  import memfiles
  proc matchFile*[T](p: Parser, fname: string, userdata: var T): MatchResult =
    var m = memfiles.open(fname)
    var a: ptr UncheckedArray[char] = cast[ptr UncheckedArray[char]](m.mem)
    result = p.fn(toOpenArray(a, 0, m.size-1), userdata)
    m.close()
  
  proc matchFile*(p: Parser, fname: string): MatchResult =
    var userdata: bool # dummy if user does not provide a type
    matchFile(p, fname, userdata)

# Return all plain string captures from the match result

proc captures*(mr: MatchResult): seq[string] =
  for cap in collectCaptures(mr.cs):
    result.add cap.s


# Return a tree with Json captures from the match result

when not defined(nimV2):
  import json
  proc capturesJson*(mr: MatchResult): JsonNode =
    collectCapturesJson(mr.cs)


# Return a tree with AST captures from the match result

proc capturesAST*(mr: MatchResult): owned ASTNode =
  collectCapturesAST(mr.cs)

proc `$`*(a: ASTNode): string =
  proc aux(a: ASTNode, s: var string, d: int=0) =
    s &= indent(a.id & " " & a.val, d) & "\n"
    for k in a.kids:
      aux(k, s, d+1)
  aux(a, result)


import npeg/builtins

